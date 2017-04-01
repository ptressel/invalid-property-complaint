# The purpose of this set of functions is to provide a way to efficiently
# match points (e.g. representing incidents or other location-based reports)
# with a collection of polygons (e.g. representing property parcels).  The
# expected case has non-overlapping polygons (property boundaries don't
# overlap), and fairly compact polygons, so comparing each point against every
# polygon can be avoided.  Here, we do that by dividing the area into a grid,
# finding which polygons overlap each grid cell.  The grid cell that a point
# falls into can be computed directly, and then the point can be intersected
# with just the (hopefully few) polygons overlapping that cell.

# The grid is specific to a collection of polygons, as its overall size and
# stride depend on the span of the polygons.  A functions is provided to build
# the grid given a collection of polygons.  Functions to look up points in the
# grid are also provided.  There are also utility functions that were found
# useful.

# Since the polygon collection may not cover the entire space where points are
# expected to fall (for instance, property boundaries may not extend into
# streets, but incidents may be reported at locations in streets adjacent to
# properties), it will also be useful to associate points with nearby polygons,
# if the initial match with the actual polygons fails.  A simple way to do this
# is to extend all the polygons outward by an appropriate width (e.g. the width
# of 1-2 street lanes), then construct a grid using the collection of extended
# polygons.

# Grid construction and matching can be performed in either a geodetic
# coordinate reference system (longitude, latitude), or a projected
# coordinate reference system, but extending the polygons requires a projected
# CRS, so the functions relating to extending polygons will return errors if
# given un-projected polygons.  Projection always involves some distortion,
# which can be minimized by using a local CRS, just for the area of interest.
# This will be a planar approximation, and distances won't be exact.  In
# general, it will not work well to convert to a projected CRS, compute
# locations, then transform those back to longitude and latitude.  However,
# picking one projection, converting everything, and doing all matching in that
# projection, should be fine -- just use the original values of longitude and
# latitude to identify the locations, rather than re-projecting locations back.

# Given a collection of polygons, construct a grid covering the bounding box of
# the polygon collection, and assign polygons to the grid cells they overlap.
# This can then be used to quickly look up which polygon(s) a point is in:
# The grid cell the point is in can be computed directly from the point's
# coordintates.  Then the point can be intersected with just the polygons that
# overlap that cell.  Choosing good cell dimensions is important:  Don't want
# them to be too large, as too many polygons may overlap each cell.  Don't want
# them to be too small, else the grid will be large.

require(sp, quietly=TRUE)
require(rgeos, quietly=TRUE)
require(hash, quietly=TRUE)
require(maptools, quietly=TRUE)

# A note on the implementation:  R matrices are immutable.  Every assignment to
# an R matrix causes the matrix to be copied.  The same is true for lists and
# vectors.
#
# <rant>
# Yes, I'm aware that some references claim that not all objects are copied
# on assignment.  However, every matrix I've created has had the named field
# in its header set to 2, which means a copy must be done regardless of whether
# there are any other references to the object.  And inspect() shows that the
# matrix *is* replaced, without exception, in all cases I've tried, including
# simple numeric matrices.  The excuse offered for immutable objects is safety
# in the case of multiple accessors to an object.  However, this is a delusion.
# If multiple consumers of an object get their own private copies that are not
# updated, then they may be using *stale information*.  The cure for multiple
# accessors is not private copies, it's proper synchronization.  We've known
# how to do synchronization since the time of punched cards.  The Java language
# designers spent several years studying what caused bugs in programs, and,
# guess what -- Java has mutable objects!  Java also supports synchronization
# as part of the core language.  That doesn't exhaust the arguments against
# "immutable is safer", but let me just add one thing:
# If your program >>>> crashes <<<< because of immutability, that's not safe!
# </rant>
#
# The matrix for a large grid would not only be copied on every row of a large
# dataset, but for every grid cell that row's polygon covers.  And if we use
# lists to hold the row numbers of polygons assigned to a grid cell, then a new
# list will get created each time a number is added.  The second
# re-implementation of this program was designed to be space efficient, and
# would have been...in a different language.  After discovering that the
# matrices were being replaced, it became clear that the R garbage collector
# was not freeing up memory until it got right up to the memory.limit() value,
# which was set to all of physical memory, and the program would eventually
# crash.  Explicit gc() calls would just be a band-aid on a flawed process.
# So...this version uses environments to hold the grid.  It still uses lists to
# hold the row numbers for each grid cell, and does call gc() to get rid of the
# abandoned lists.  If this affects performance too much, environments could be
# used in place of lists, but this would affect memory usage -- the size of an
# empty list is 40 bytes, versus 640 bytes for an empty environment.

# Assign polygons to grid cells in two steps.  First, determine which cells a
# polygon might belong to based on its bounding box.  This does not require
# intersection -- it can be computed directly.  Second, for each grid cell that
# overlaps the polygon's bounding box, check if there is an overlap between the
# grid cell polygon and the actual data polygon.  For each actual overlap,
# assign the polygon row number to that grid cell.

# First, need a function to compute the grid cell a point is in.  Can we assume
# it won't fall on a boundary?  No, we should not, especially if this function
# will also be used to assign look up the polygon(s) for a point.  Compute the
# row and column that the point falls in, and if on a boundary, return a tuple
# with indices on both sides, but exclude any ouside the bounds of the grid.
# For instance, if we pass in the (xmin, ymin) of the entire grid, we want to
# get only cell [1, 1].  Remember that indices start from 1.
#
# Start with a function to handle a single axis -- the same computation
# applies to both axes.
#
# Arguments:
# z -- coordinate
# grid_z_min -- coordinate of lower edge of the grid
# grid_z_cellsize -- size of once cell along this direction
# grid_z_num_cells -- number of cells along this direction
#
# Returns:
# A list of grid indices, which may include one or two indices.  If this
# coordinate falls in the interior of a grid cell, then there will be only
# one index, which will be named "interior".  If this coordinate falls on
# a grid division, then one or both of the indices on either side will be
# included, so long as they are within the grid span.  The index of the cell
# below the coordinate will be labeled "below", and that of the cell above
# the coordinate will be labeled "above".  If the coordinate is on the
# lower end of the grid, the "below" index would be out of range, so will not
# be included.  Likewise, if the coordinate is on the upper end of the grid,
# the "above" index will not be included.
#
coordinate_to_grid_indices <- function(z,
                                       grid_z_min,
                                       grid_z_cellsize,
                                       grid_z_num_cells) {
    z_cell_raw <- (z - grid_z_min) / grid_z_cellsize + 1
    # This is what we would want to do next to get the index for the cell that
    # value falls in...
    # z_cell <- floor(z_cell_raw)
    # However...due to floating point precision issues, floor() is unreliable.
    # A value that R displays as an integer x may become x-1 when floor()
    # is applied, so floor is not designed to take floating point issues into
    # account.  Which means, we'll need to do that by hand.  So...use a
    # function, all.equal(), that does do a floating point aware "good enough"
    # equality check.  See if the raw cell index value is equal (enough) to
    # that value rounded to the nearest integer.  If it is, use that.
    # Otherwise, it's not already "on" a grid division, and we want to round
    # down to the nearest division, so take the floor of it.
    # Is this "close enough" to an integer already?  If so, that's what floor
    # "should" return.
    z_cell <- round(z_cell_raw)
    is_on_cell_edge <- isTRUE(all.equal(z_cell_raw, z_cell))
    if (!is_on_cell_edge) {
        # This point is in the interior of the cell, not on the
        # upper edge.
        z_cell <- floor(z_cell_raw)
    }
    # Assume we are out of range and will have no return values.
    z_cells <- NULL
    # z_cell is either interior or on the upper side of the given coordinate.
    # We want to include it if it's in range.
    if ((z_cell >= 1) & (z_cell <= grid_z_num_cells)) {
        # Yes, it's in range.  Use the appropriate label.
        if (!is_on_cell_edge) {
            z_cells <- list(interior=z_cell)
        } else {
            z_cells <- list(above=z_cell)
        }
    }
    # If this is on a grid division, we may have a cell below at z_cell-1.
    if ((z_cell-1 >= 1) & (z_cell-1 <= grid_z_num_cells) & is_on_cell_edge) {
        # Yes, include the lower cell.
        if (is.null(z_cells)) {
            z_cells <- list(below=z_cell-1)
        } else {
            z_cells <- c(below=z_cell-1, z_cells)
        }
    }
    z_cells
}

# Get the grid cells for a pair of coordinates.
# Arguments:
# x - longitude
# y - latitude
# grid_data - the structure containing the grid and its metadata
#
point_to_grid_cells <- function(x, y,
                                grid_min,
                                grid_cellsize,
                                grid_ncell) {
    x_cells <- coordinate_to_grid_indices(x,
                                          grid_min["x"],
                                          grid_cellsize["x"],
                                          grid_ncell["x"])
    y_cells <- coordinate_to_grid_indices(y,
                                          grid_min["y"],
                                          grid_cellsize["y"],
                                          grid_ncell["y"])
    list(x=x_cells, y=y_cells)
}

# Given an x, y grid index pair, return a string that can serve as a hash key.
#
grid_index_to_key <- function(x, y) {
    paste("c", x, y, sep=".")
}

# Given a grid key, return the x, y index pair.
#
grid_key_to_index <- function(grid_key) {
    parts <- unlist(strsplit(grid_key, ".", fixed=TRUE))
    as.numeric(parts[2:3])
}

# Next, construct the grid and populate it.

# polygon_data is a SpatialPolygonsDataFrame.  Only the polygons are used.
# (There should not be any NA polygons.  This would be simple to fix if needed.)
#
# grid_scale is the name of a summary statistic function, likely one of "mean"
# or "median".  This is applied to the heights and widths of the polygon
# bounding boxes to get an initial scale for the grid cell height and width.
# If there are extreme outliers in polygon size, then median is likely a better
# choice.  It's the default for this reason.
#
# grid_scale_multiplier allows adjusting the grid spacing.  The cell width
# and height are grid_scale_multiplier times the value selected by grid_scale.
#
# save_sp_polygons controls whether the SpatialPolygon made for each row is
# saved for reuse by the function that finds the polygons a point lies in.
# (The polygon intersect function from the rgeos package requires a
# SpatialPolygons object.  If there is enough memory, then retaining these
# avoids the need to recreate them on each point check.)
#
# show_time can be set to TRUE to print the CPU and elapsed time this uses.
# verbose can be set to TRUE to print which cells each polygon is assigned to.
# The latter is for testing, and should not be set to TRUE for a large run.
#
# show_progress controls whether a progress message is printed every
# progress_interval rows.
#
# gc_interval is the number of rows after which to do a garbage collection call.
# R does not schedule gc well -- it allows the process to run out of memory, and
# fails to return memory to the OS.  Doing explicit calls to gc will allow
# larger datasets to be processed, but the gc call itself is very time
# consuming -- a single call can take up to half a minute.
#
# quit_row is for testing or doing a preliminary check using part of the data.
# if specified, the grid setup will stop after polygons from quit_row rows
# have been processed.
#
# Returns a list containing the populated grid and related information.
#
construct_polygon_grid <- function(polygon_data,
                                   grid_scale="median",
                                   grid_scale_multiplier=3,
                                   save_sp_polygons=FALSE,
                                   show_time=FALSE,
                                   show_progress=FALSE,
                                   progress_interval=100,
                                   verbose=FALSE,
                                   gc_interval=100,
                                   quit_row=NULL) {

    if (show_time) {
        start_time <- proc.time()
    }

    # Get the overall size and cell size for the grid.
    polygon_data_bbox <- bbox(polygon_data)
    grid_total_height <- polygon_data_bbox["y", "max"] - polygon_data_bbox["y", "min"]
    grid_total_width <- polygon_data_bbox["x", "max"] - polygon_data_bbox["x", "min"]

    if (verbose) {
        cat("grid_total_height, _width: ",
            grid_total_height, " ", grid_total_width, "\n")
    }

    # Get statistics on the individual polygon bounding boxes, in order to set
    # the grid cell size.
    polygon_bboxes <- lapply(polygon_data@polygons, bbox)
    bbox_height <- sapply(polygon_bboxes, function(b) abs(b["y", "max"] - b["y", "min"]))
    bbox_width <- sapply(polygon_bboxes, function(b) abs(b["x", "max"] - b["x", "min"]))
    grid_cell_base_height <- get(grid_scale)(bbox_height)
    grid_cell_base_width <- get(grid_scale)(bbox_width)
    grid_cell_height <- grid_scale_multiplier * grid_cell_base_height
    grid_cell_width <- grid_scale_multiplier * grid_cell_base_width
    grid_height_num_cells <- ceiling(grid_total_height / grid_cell_height)
    grid_width_num_cells <- ceiling(grid_total_width / grid_cell_width)

    if (verbose) {
        cat("grid_cell_base_height, _width: ",
            grid_cell_base_height, " ", grid_cell_base_width, "\n")
        cat("grid_cell_height, _width: ",
            grid_cell_height, " ", grid_cell_width, "\n")
        cat("grid_height_num_cells, _width_: ",
            grid_height_num_cells, " ", grid_width_num_cells, "\n")
    }

    # Store the cells in a hash table (environment).  Each element represents a
    # grid cell, and is indexed by a key containing the x and y grid indicies.
    # Each cell contains a list holding an arbitrary number of row indices in
    # polygon_data.
    grid_polygons <- hash()

    # To assign polygons to the grid cells they overlap, we'll first narrow
    # down the candidate cells by computing which cells the polygon's bounding
    # box overlaps.  This is simple arithmetic, so is fast.  It reduces the
    # number of times we need to check intersection with the actual polygon,
    # a much more expensive operation.
    # In order to use the rgeos gIntersects function, we need an sp
    # SpatialPolygons object for each grid cell.  We'll let sp and rgeos do
    # the work (mostly to try out these functions...).  The documentation for
    # GridTopology and SpatialGrid is somewhat obscure.  After some reading
    # of the code, experimentation, and web searching, it appears that
    # GridTopology wants to be told the center of the "south-west" cell.
    grid_cellsize <- c(x=grid_cell_width, y=grid_cell_height)
    grid_min <- c(x=polygon_data_bbox["x","min"], y=polygon_data_bbox["y","min"])
    grid_offset <- grid_min + 0.5 * grid_cellsize
    grid_ncell <- c(x=grid_width_num_cells, y=grid_height_num_cells)
    grid_gt <- GridTopology(grid_offset, grid_cellsize, grid_ncell)
    grid_sg <- SpatialGrid(grid_gt)
    grid_spol <- as(grid_sg, "SpatialPolygons")
    # Those need the coordinate reference system (CRS) set to the same as the
    # polygon data.
    grid_spol_list <- lapply(grid_spol@polygons,
                             function(p) SpatialPolygons(list(p),
                                                         proj4string=polygon_data@proj4string))

    # Convert to a matrix.  To stick with common spatial point convention, want
    # the x direction (longitude) as the first matrix dimension, y (latitude) as
    # the second.  This is also the usual plotting and map display convention:
    # The horizontal, x axis is east-west, and the vertical, y axis is
    # north-south.  Want to be able to compute the lower-left longitude,
    # latitude corner of each grid cell as:
    # x_min + (x_index - 1) * x_cellsize, y_min + (y_index - 1) * y_cellsize
    # (Note the inconsistency between layout of a physical x, y grid, where the
    # first dimension is horizontal, and the second is vertical, with the layout
    # of a spreadsheet or data frame or mathematical matrix.  Yes, this is
    # weird, and yes, we shouldn't have borked the convention like this for
    # spreadsheets, etc., vs. spatial layout.)
    # Because the sp "grid" is in raster (image) order, starting at the "top" of
    # the "image", this won't quite give us the layout we want...see the next
    # step...
    grid_bboxes_backwards <- matrix(data=grid_spol_list,
                                    nrow=grid_width_num_cells,
                                    ncol=grid_height_num_cells,
                                    byrow=FALSE)
    # SpatialGrid lists its elements in raster row order, i.e. it starts with
    # the top left, numbers along the top row, then goes to the left side of the
    # row below that,...  We want x-y plot order.  So, we need to reverse the
    # order of the y axis.
    grid_bboxes <- grid_bboxes_backwards[ , grid_height_num_cells:1]

    if (verbose) {
        cat("Dimensions of grid_bboxes are: ", dim(grid_bboxes), "\n")
    }

    # Optionally save the SpatialPolygons object we make for each row of the
    # data.  gIntersects requires sp "spatial" objects, not just Polygon or
    # Polygons.  If there is room in memory, saving these will speed up
    # checking which polygons a point lies in.
    if (save_sp_polygons) {
        sp_polygons <- hash()
    } else {
        sp_polygons <- NULL
    }

    # Attempt to start with any abandoned objects cleaned up.
    gc()
    next_gc <- gc_interval

    if (show_progress) {
        cat("About to start inserting polygons into grid.\n")
        # Show progress every progress_interval rows.
        next_progress_msg <- progress_interval
    }

    # Next insert each polygon into the grid.
    #
    # This is not currently parellelized, but it could be if needed.  It is not
    # amenable as-is to use of foreach, as the assignment of two polygons to
    # grid cells are not independent -- they may both need to act on the same
    # cell.  A mutex guarding updates to a cell would allow use of foreach.
    # But this really cries out for a mapreduce framework.  Each mapper would
    # act on some subset of polygons, one at a time, and emit the indices of
    # each cell the polygon belongs to as the key, with the polygon's name as
    # the value.  The reducer would just emit the list it got for each cell.

    if (is.null(quit_row)) {
        end_row <- nrow(polygon_data)
    } else {
        end_row <- quit_row
    }

    for (row in 1:end_row) {
        if (show_progress && (next_progress_msg == row)) {
            cat("Starting row ", row, "\n")
            next_progress_msg <- next_progress_msg + progress_interval
        }
        # Compute minimum grid indices along x and y that this polygon overlaps.
        min_cell <- point_to_grid_cells(polygon_bboxes[[row]]["x", "min"],
                                        polygon_bboxes[[row]]["y", "min"],
                                        grid_min,
                                        grid_cellsize,
                                        grid_ncell)
        # If this point falls on a grid division in either the x or y direction,
        # then we may have two indices for that direction.  If we were checking
        # an incident location, we would want to test the point against polygons
        # on either adjacent cell.  But here, we have a polygon bounding box,
        # and want to see which cells it overlaps.  We know that the interior of
        # the bounding box lies above and to the right of the min x, min y
        # bounding box corner, so we only want the "interior" or "above" index.
        # Because the grid was sized to cover all the polygons, this polygon's
        # bounding box will not extend beyond the grid region.
        min_cell_x <- min_cell$x$interior
        if (is.null(min_cell_x)) {
            min_cell_x <- min_cell$x$above
        }
        min_cell_y <- min_cell$y$interior
        if (is.null(min_cell_y)) {
            min_cell_y <- min_cell$y$above
        }
        # Now find the the grid indices that the polygon ends in.  Get the
        # indices of the upper right corner of the bounding box.  If this lands
        # exactly on a grid cell division, we only want to include the cells
        # "below" the division.
        max_cell <- point_to_grid_cells(polygon_bboxes[[row]]["x", "max"],
                                        polygon_bboxes[[row]]["y", "max"],
                                        grid_min,
                                        grid_cellsize,
                                        grid_ncell)
        # Yes, this is ok if max_cell is NULL.
        max_cell_x <- max_cell$x$interior
        if (is.null(max_cell_x)) {
            max_cell_x <- max_cell$x$below
        }
        max_cell_y <- max_cell$y$interior
        if (is.null(max_cell_y)) {
            max_cell_y <- max_cell$y$below
        }
        # Cleanup
        rm(min_cell, max_cell)
        # Iterate over all grid cells in that span.  This is an inclusive range.
        if (verbose) {
            cat("Row", row.names(polygon_data[row,]),
                "spans grid cells x:", min_cell_x, "-", max_cell_x,
                "y:", min_cell_y, "-", max_cell_y, "\n")
        }
        # gIntersects needs SpatialPolygons objects.  The grid cells are
        # already that class, but the individual rows in the
        # polygon_data@polygons are ordinary Polygons objects -- the entire
        # polygon_data@polygons is a SpatialPolygons object.  So make a
        # SpatialPolygons out of the single row.
        sp <- SpatialPolygons(list(polygon_data@polygons[[row]]),
                              proj4string=polygon_data@proj4string)
        if (save_sp_polygons) {
            sp_polygons[[as.character(row)]] <- sp
        }
        for (x_idx in min_cell_x:max_cell_x) {
            for (y_idx in min_cell_y:max_cell_y) {
                # Does the polygon intersect this cell?
                # R seems to have an odd behavior when dereferencing an element
                # from a list if that list is in a matrix element.  All R
                # non-primitives get packaged in a list if they are put in a
                # matrix, but R just keeps returning a list, no matter how many
                # times one applies [[1]] to that matrix element.  However, if
                # one assigns the matrix element to another variable, R seems
                # to forget that it came from a matrix, and [[1]] returns the
                # list element successfully.  This behavior may depend on some
                # quirk of how this particular matrix was populated.
                # Regardless, don't allow R to be weird here -- explicitly take
                # the grid cell list out of the matrix before attempting to
                # take the grid cell polygon out of that list.
                cell_list <- grid_bboxes[x_idx, y_idx]
                cell <- cell_list[[1]]
                if (gIntersects(cell, sp)) {
                    # Found an overlap between a polygon and this grid cell.
                    # Add this polygon to the list for this cell.
                    x_y_key <- grid_index_to_key(x_idx, y_idx)
                    cell_polygons <- grid_polygons[[x_y_key]]
                    if (is.null(cell_polygons)) {
                        new_cell_polygons <- list(row)
                    } else {
                        new_cell_polygons <- c(cell_polygons, row)
                        # Cleanup
                        rm(cell_polygons)
                    }
                    grid_polygons[[x_y_key]] <- new_cell_polygons
                    if (verbose) {
                        cat("Assigning", row.names(polygon_data[row,]),
                            "to grid cell [", x_idx, ",", y_idx, "]", "\n")
                    }
                }
            }
        }
        # Cleanup
        if (row == next_gc) {
            gc()
            next_gc <- next_gc + gc_interval
        }
    }

    # No longer need the grid cell bounding boxes.
    rm(grid_bboxes)
    gc()

    # Package up all those grid-related bits of info.  Use the x, y naming
    # convention.  Include the caller's scale choices in case they are trying
    # multiple values.
    grid_data <- list(
        grid_polygons=grid_polygons,
        sp_polygons=sp_polygons,
        grid_min=grid_min,
        grid_cellsize=grid_cellsize,
        grid_ncell=grid_ncell,
        grid_scale=grid_scale,
        grid_scale_multiplier=grid_scale_multiplier
    )

    if (show_time) {
        diff_time <- proc.time() - start_time
        print(diff_time)
    }
    if (verbose) {
        print_grid(grid_data, polygon_data)
    }

    grid_data
}

# Test helper that prints what's in a completed grid. Do not run this on a
# large grid.
#
print_grid <- function(grid_data, polygon_data) {
    for (x_idx in 1:grid_data$grid_ncell["x"]) {
        for (y_idx in 1:grid_data$grid_ncell["y"]) {
            x_y_key <- grid_index_to_key(x_idx, y_idx)
            cell_list <- grid_data$grid_polygons[[x_y_key]]
            if (!is.null(cell_list)) {
                cat("[", x_idx, ",", y_idx, "]: ", sep="")
                for (row in cell_list) {
                    # Creating an SPDF appears to make the ID column factor.
                    row_id <- as.character(polygon_data@data[row, "ID"])
                    cat(row_id, ", ")
                }
                cat("\n")
            }
        }
    }
}

# rgeos functions like gIntersects and gBuffer need SpatialPolygons objects.
# Individual rows in a SpatialPolygonsDataFrame @polygons slot are ordinary
# Polygons objects -- the entire @polygons slot is a SpatialPolygons object.
#
# Make a SpatialPolygons object out of one row of a SpatialPolygonsDataFrame.
#
make_sp_polygon <- function(polygon_data, row) {
    SpatialPolygons(list(polygon_data@polygons[[row]]),
                    proj4string=polygon_data@proj4string)
}

# Make a set of SpatialPolygons for each row of a SpatialPolygonsDataFrame.
# These will be stored in a hash with the row number (as character) as the key.
#
make_sp_polygons <- function(polygon_data) {
    sp_polygons <- hash()
    for (row in 1:nrow(polygon_data)) {
        # Make a SpatialPolygons out of the single row.
        spoly <- make_sp_polygon(polygon_data, row)
        sp_polygons[[as.character(row)]] <- spoly
    }
    sp_polygons
}

# Look up the polygons that the given x, y point lies in.
# x - vector of x coordinates of the points. For instance, these may be
# longitudes or projected longitudes.
# y - vector of y coordinates of the points. For instance, these may be
# latitudes or projected latitudes.
# do_match - logical telling whether to check this point.
# do_gc - logical telling whether to run gc on this call.
# grid_data - a grid_data object, as returned by construct_polygon_grid.
# polygon_data - the original data on which the grid is based. This is where the
# actual polygons reside.
#
# Returns a list containing, for each point:
# NA if do_match is FALSE, i.e. no match should be done for this point.
# NULL if no polygons contain the point.
# a list of row numbers in polygon_data, of polygons containing the point.
#
# Use this with mapply, like this:
# Say that:
# x_coords is a vector of longitudes or projected longitudes, likewise
# y_coords for (projected) latitudes,
# do_match_vec is a logical telling which points to test.
# some_grid_data is a structure returned by construct_polygon_grid(), and
# some_polygon_data is the SpatialPolygonsDataFrame that was used to construct
# some_grid_data, and gc_interval is how frequently gc should be done.
# polygons_for_points <- mapply(polygons_containing_point,
#     x_coords, y_coords, do_match_vec,
#     (1:length(x_coords) %% gc_interval) == 0,
#     MoreArgs=list(grid_data=some_grid_data, polygon_data=some_polygon_data))
#
polygons_containing_point <- function(x, y, do_match, do_gc,
                                      grid_data, polygon_data) {
    # Exclude unwanted points.
    if (!do_match) {
        return(NA)
    }
    # Collect row ids here, for polygons containing the point.
    matching_rows <- NULL
    # Make an sp object for the point, to use with gIntersects.
    point <- SpatialPoints(cbind(x, y),
                           proj4string=polygon_data@proj4string)
    # Which grid cell(s) is this point in?  It may be in more than one if it is
    # on a grid cell boundary.  We'll need to check all the returned cells.
    cells <- point_to_grid_cells(x, y,
                                 grid_data$grid_min,
                                 grid_data$grid_cellsize,
                                 grid_data$grid_ncell)
    for (x_idx in cells$x) {
        for (y_idx in cells$y) {
            # Does this grid cell contain any polygons?
            x_y_key <- grid_index_to_key(x_idx, y_idx)
            cell_list <- grid_data$grid_polygons[[x_y_key]]
            if (!is.null(cell_list)) {
                # Check if the point is in each polygon that overlaps the cell.
                for (row in cell_list) {
                    # Did we already match this polygon?
                    if (!(row %in% matching_rows)) {
                        # Intersect the point with this row's polygon.
                        if (!is.null(grid_data$sp_polygons)) {
                            spoly <- grid_data$sp_polygons[[row]]
                        } else {
                            spoly <- make_sp_polygon(polygon_data, row)
                        }
                        if (gIntersects(point, spoly)) {
                            # Add to the list of polygons.
                            matching_rows <- c(matching_rows, row)
                        }
                        # If we created the sp polygon, remove it.
                        if (is.null(grid_data$sp_polygons)) {
                            rm(spoly)
                        }
                    }
                }
            }
        }
    }
    if (do_gc) {
        gc()
    }
    # Return the matching polygon rows.
    return(matching_rows)
}

# This executes the mapply over vectors of x and y coordinates (e.g. longitude
# and latitude, or projected coordinates).  It also
# calls the garbage collector at the specified interval, because the sp
# polygons are large.
#
# x_vec - vector of x coordinates of the points. For instance, these may be
# longitudes or projected longitudes.
# y_vec - vector of y coordinates of the points. For instance, these may be
# latitudes or projected latitudes.
# do_match_vec - vector of logicals telling which coordinates to check. (This
# can be used in place of subsetting the point data. Subsetting by extracting
# the relevant rows is awkward when it comes time to merge the results back
# into other results for the entire dataset.  Also, if the data comes from a
# SpatialPointsDataFrame, that may use row names as IDs to match up
# corresponding points, and the ID values are usually row numbers.
# Using a logical vector avoids the need to transform results from subsetted
# coordinates back to the original row positions.  Complete lists of polygon
# or address match results from different runs can be combined element by
# element.)
# grid_data - a grid_data object, as returned by construct_polygon_grid.
# polygon_data - the original data on which the grid is based. This is where the
# actual polygons reside.
#
polygons_containing_points <- function(x_vec, y_vec, do_match_vec,
                                       grid_data, polygon_data,
                                       gc_interval=1000) {
    # Start with any abandoned objects cleaned up.
    gc()
    # Make a vector that indicates when to do gc().
    do_gc_vec <- (1:length(x_vec) %% gc_interval) == 0
    results <- mapply(polygons_containing_point, x_vec, y_vec,
                      do_match_vec, do_gc_vec,
                      MoreArgs=list(grid_data=grid_data,
                                    polygon_data=polygon_data))
    gc()
    return(results)
}

# Some points may fall outside polygons.  It's suspected that these points lie
# in the street facing the property.  One option for associating these with the
# adjacent polygon is to extend the polygon outward by an appropriate amount
# based on the size of streets and where the incident reporters are likely to
# be getting the location, e.g. using their own location while on the sidewalk
# or street nearby.  Non-street boundaries are moot, as they have little to no
# gap between them, so incidents there would have been assigned to a polygon.
#
# Could we extend only polygons near the unassigned points?  E.g. find the grid
# cell for the point, then extend polygons in adjacent grid cells, no more than
# the chosen width away from the original grid cell.  To avoid re-doing the
# extension, save the extended polygons in a hash (mutable!) with their row
# number as key.  Then for each point, identify the relevant grid cells, check
# for already-extended polygons and use those, and extend the others.  Record
# those in the hash.
#
# But that doesn't reuse the above functions.  As a first go, just extend all
# the polygons, put them in a SpatialPolygonsDataFrame object, and create a new
# grid.  This will be expensive, but we have the "do gc at intervals" mechanism.
#
# This version uses the polygon buffering function, gBuffer, provided by
# GEOS / rgeos.

# This extends a single row's polygons.  Each row in the SPDF slot Polygons is
# a list of Polygon objects.  gBuffer needs a SpatialPolygons object, so the
# main purpose of this function is to either use a pre-made object, or make one.
# The returned object is also a SpatialPolygons.
#
extend_row_polygons <- function(grid_data, polygon_data, row, width) {
    if (!is.null(grid_data$sp_polygons)) {
        spoly <- grid_data$sp_polygons[[row]]
    } else {
        spoly <- make_sp_polygon(polygon_data, row)
    }
    extended_spoly <- gBuffer(spoly, width=width, joinStyle="BEVEL")
    # If we created the sp polygon, remove it.
    if (is.null(grid_data$sp_polygons)) {
        rm(spoly)
    }
}

# This takes the original polygon_data SpatialPolygonsDataFrame object, produces
# another SpatialPolygonsDataFrame, with extended polygons.  It does not keep
# the full data frame -- it puts only the original ID number in as data (just
# to have a non-empty data).
#
# The function used to extend the polygons, gBuffer in rgeos, needs a
# SpatialPolygons object.  If these are available in grid_data, they are used.
#
extend_spdf_polygons <- function(grid_data, polygon_data, width,
                                 show_time=FALSE,
                                 show_progress=FALSE,
                                 progress_interval=100,
                                 gc_interval=100,
                                 quit_row=NULL) {

    if (show_time) {
        start_time <- proc.time()
    }

    if (show_progress) {
        cat("About to start extending polygons.\n")
        # Show progress every progress_interval rows.
        next_progress_msg <- progress_interval
        }

    # Start with any abandoned objects cleaned up.
    gc()
    next_gc <- gc_interval

    # Aggregate new rows here.
    extended_polygon_data <- NULL

    if (is.null(quit_row)) {
        end_row <- nrow(polygon_data)
    } else {
        end_row <- quit_row
    }

    for (row in 1:end_row) {

        if (show_progress && (next_progress_msg == row)) {
            cat("Starting row ", row, "\n")
            next_progress_msg <- next_progress_msg + progress_interval
        }

        # Extend this row's polygon(s).
        extended_polygons <- extend_row_polygons(grid_data, polygon_data, row,
                                                 width)
        # Provide minimal data.  Note row name matches the ID from the row's
        # SP object.
        id <- parcel_data@polygons[[row]]@ID
        minimal_row_data <- data.frame(ID=c(id), row.names=c(id))
        # Make an SPDF for this one row.
        extended_row <- SpatialPolygonsDataFrame(Sr=extended_polygons,
                                                 data=minimal_row_data)
        # Combine with the collection so far.
        if (row == 1) {
            extended_polygon_data <- extended_row
        } else {
            extended_polygon_data <- spRbind(extended_polygon_data,
                                             extended_row)
        }

        # Cleanup
        if (row == next_gc) {
            gc()
            next_gc <- next_gc + gc_interval
        }
    }

    gc()

    if (show_time) {
        diff_time <- proc.time() - start_time
        print(diff_time)
    }

    extended_polygon_data
}
