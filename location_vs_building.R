# Initial exploration of datasets, focusing on location.
# These are incidents with latitude and longitude -- some are missing, but
# may have addresses.  Goal is to group these incidents according to what
# building they apply to.  We do not have polygons for building footprints,
# nor for property boundaries.  Q: Does Detroit have parcel boundary data?
# Yes!!!

setwd("~/coursera/u_washington/datasci_capstone/work")
# Note the supplied data files are in a data subdirectory that is linked
# from the course repository.  Other data and intermediate files are not
# in subdirectories.

library(lubridate)
library(sp)
library(rgdal)
library(rgeos)
library(hash)
library(maptools)
library(ggplot2)
library(ggmap)
library(plyr)

# 311 calls
calls_311 <- read.csv("data/detroit-311.csv", stringsAsFactors=FALSE)
# Several fields are times w/ form 01/01/2015 09:14:35 AM -- will need to
# convert those...later.
#
# The 311 calls include some that are not necessarily indicative of a problem
# with a specific property, as they are reports of problems with city
# infrastructure, such as clogged street drains, potholes.  Locations of such
# reports may be in streets, and do not fall within property parcel boundaries.
# Note, however, that issues of this sort may indicate city neglect of an area.
# For now, need to separate city issues from property issues.  It may become
# useful later to identify nearby properties that may be affected by city
# maintenance issues.

calls_311_issue_types <- unique(calls_311$issue_type)
# > length(calls_311_issue_types)
# [1] 23
# Convert this column to factor.
calls_311$issue_type <- factor(calls_311$issue_type, levels=calls_311_issue_types)
# Other columns have only a few values, so are better as factor.
calls_311$ticket_status <- factor(calls_311$ticket_status)
# The city column is only "City of Detroit" so can be removed.
calls_311$city <- NULL
# The images are stored on the SeeClickFix website -- they won't be used for now
# so can be removed.
calls_311$image <- NULL
# The text location field is not more precise than the lng and lat fields.
calls_311$location <- NULL
# It is not clear that any date field other than the report creation data will
# be useful, unless we want to look at city response times.  For now, keep the
# created and last updated columns, and convert those to date-times.
calls_311$acknowledged_at <- NULL
calls_311$ticket_closed_date_time <- NULL
calls_311$ticket_created_date_time <- mdy_hms(calls_311$ticket_created_date_time, tz="EST")
calls_311$ticket_last_updated_date_time <- mdy_hms(calls_311$ticket_last_updated_date_time, tz="EST")
# Write out the file containing all issue types.
saveRDS(calls_311, "calls_311.rds")

# The issue types come from the SeeClickFix app, which is used to submit 311
# issues.
# http://www.detroitmi.gov/How-Do-I/Mobile-Apps/ImproveDetroit
# https://seeclickfix.com/detroit
# Queries for these issues on the SeeClickFix site allow splitting them info
# those that relate to city maintenance vs. property issues.  A few issue types
# are used for both.
city_issues <- c(1, 3, 4, 6, 7, 8, 9, 12, 13)
city_issues_names <- calls_311_issue_types[city_issues]
# > city_issues_names
# [1] "Clogged Drain"                          "Manhole Cover Issue"
# [3] "Water Main Break"                       "Fire Hydrant Issue"
# [5] "Traffic Signal Issue"                   "Potholes"
# [7] "Traffic Sign Issue"                     "Street Light Pole Down"
# [9] "Test (internal use only, public issue)"
definite_property_issues <- c(11, 14, 16, 17, 18, 19, 20, 22, 23)
definite_property_issues_names <- calls_311_issue_types[definite_property_issues]
# > definite_property_issues_names
# [1] "Running Water in a Home or Building"
# [2] "Graffiti"
# [3] "Residential Snow Removal Issue"
# [4] "Graffiti Abatement (internal use only, public issue)"
# [5] "DPW - Debris Removal"
# [6] "Trash Issue - Improper placement of refuse container between collections/left at curbside"
# [7] "Trash Issue - Bulk waste deposited more than 24 hours before designated time"
# [8] "Detroit Land Bank Referral"
# [9] "DPW - Other environmental"
city_or_property_issues <- c(2, 5, 10, 15, 21)
city_or_property_issues_names <- calls_311_issue_types[city_or_property_issues]
# > city_or_property_issues_names
# [1] "Tree Issue"
# [2] "Abandoned Vehicle"
# [3] "Illegal Dumping / Illegal Dump Sites"
# [4] "Customer Service (internal use only, private issue)"
# [5] "Curbside Solid Waste Issue"
property_issues <- c(definite_property_issues, city_or_property_issues)
property_issues_names <- c(definite_property_issues_names, city_or_property_issues_names)
# Now separate out the property issue data.
calls_311_property <- calls_311[as.numeric(calls_311$issue_type) %in% property_issues, ]
# > nrow(calls_311_property)
# [1] 11184
# > nrow(calls_311)
# [1] 19680
# And abandon the full data.
rm(calls_311)

blight_violations <- read.csv("data/detroit-blight-violations.csv")
crime <- read.csv("data/detroit-crime.csv")
demolition_permits <- read.csv("data/detroit-demolition-permits.tsv", sep="\t")

# Get the parcel boundary polygons.
# https://data.detroitmi.gov/Property-Parcels/Parcel-Map/fxkw-udwf
# This is a Socrata site.  API doc says SODA rather than SODA 2 so may not be
# using the latest API version.
# File was exported as GeoJSON and downloaded rather than read via the API
# as it's not yet known which other fields may be useful.  API is better for
# doing queries that limit either rows or columns.

# This load took a long time -- at least an hour.  Turns out readOGR has a
# stringsAsFactors argument that should have been set to FALSE as all other
# data...even numbers...were read as factor.  Given the long load time, will
# not re-do the load, but just convert the factors of the columns deemed useful.
# But anyone else doing this may want to include stringsAsFactors=FALSE.
parcel_data <- readOGR(dsn="parcel_map.geojson",
                       layer="OGRGeoJSON",
                       stringsAsFactors=FALSE)

# rgdal doc says that readOGR returns a Spatial vector object.  This appears to
# to mean the Spatial class of the sp package.  Class of the returned object
# SpatialPolygonsDataFrame.  The non-polygon data is in a @data member which is
# a data.frame, and the polygons are in a @polygons member, which is a list of
# Polygons objects, each of which is a list of Polygon objects.  The output of
# these commands is omitted for brevity...  There are lots of potentially
# useful fields in the @data slot.  These were all read in as factor, even when
# inappropriate, so they'll have to be converted if needed.
# > str(parcel_data, list.len=5)
# > str(parcel_data@data)

# Look inside the @polygons information.
# > class(parcel_data@polygons[[1]])
# [1] "Polygons"
# attr(,"package")
# [1] "sp
# > class(parcel_data@polygons[[1]]@Polygons)
# [1] "list"
# > class(parcel_data@polygons[[1]]@ID)
# [1] "character"
# > class(parcel_data@polygons[[1]]@Polygons[[1]])
# [1] "Polygon"
# attr(,"package")
# [1] "sp"

# How many elements are there per each row in the @polygons slot?  That is, are
# all of them single Polygons object?  Yes.
num_polygons <- sapply(parcel_data@polygons, length)
num_polygons_values <- unique(num_polygons)
# > num_polygons_values
# [1] 1
# It's entirely legitimate for a property to be a multipolygon (i.e. contain
# several disjoint polygons) or have holes.  sp represents both holes and
# multiple polygons with a list of Polygon objects -- it does not (need to)
# specify any nesting in the syntax (like WKT) or have an object hierarchy.
# Just out of interest, see if there are any parcels that use more than one
# Polygon object.  Yep, there are.
num_polygon <- sapply(parcel_data@polygons, function(polygons_row) {
    length(polygons_row@Polygons)
})
num_polygon_values <- sort(unique(num_polygon))
# > num_polygon_values
# [1] 1 2 4 5 6 7
# Also just for interest, see if any have more than one non-hole polygon, and
# if any have holes.
num_non_hole <- sapply(parcel_data@polygons, function(polygons_row) {
    sum(!sapply(polygons_row@Polygons, "slot", "hole"))
})
num_non_hole_values <- sort(unique(num_non_hole))
# > num_non_hole_values
# [1] 1 2 4 6 7
# How many holes does each row have?
num_hole <- num_polygon - num_non_hole
num_hole_values <- sort(unique(num_hole))
# > num_hole_values
# [1] 0 1 3 4
# And, how many rows are multipolygons?  Only a few.
is_multipolygon <- num_non_hole > 1
# > sum(is_multipolygon)
# [1] 50

# Given a single element from the polygons list, here's how to get its bounding
# box:
p1 <- parcel_data@polygons[[1]]
p1.p1 <- p1@Polygons[[1]]
# Can get the bounding box from either of the above:
# > bbox(p1)
#         min       max
# x -83.14729 -83.14575
# y  42.36880  42.37193
# > bbox(p1.p1)
#         min       max
# x -83.14729 -83.14575
# y  42.36880  42.37193

# Will want to use the bounding boxes.  Do they need to be materialized to
# to speed up the work?  They will be repeatedly reused.  I don't see the
# bounding box stored with the Polygon objects.  The Spatial constructor can
# take a bbox argument...does that set a bbox slot, and will bbox() reuse that?
# The bbox() code for Polygon and Polygons classes is at:
# https://github.com/edzer/sp/blob/7b5454c3edfeac7e5a055d52fc340a13db69a751/R/SpatialPolygons-methods.R#L65
# That calls range on the coordinates in both directions.  Notice that is is
# *not* valid for spherical geometry.  In the current application, where the
# polygons are small and not near the poles, that's moot, but it's incorrect in
# general.  For now, extract the bbox of each row and save that outside of the
# Spatial data frame.
parcel_bboxes <- lapply(parcel_data@polygons, bbox)

# Next want to find out how big to make a grid for indexing.  Want polygons to
# only extend across two adjacent grid cells.  To figure out what spacing to
# use for the grid, get statistics on the bounding boxes, in particular, maximum
# width or height.

# We may be interested mainly in residential properties, or may want to split
# into categories for prediction.  This document shows the numerical codes for
# different property types:
# https://data.detroitmi.gov/api/views/fxkw-udwf/files/3bf805fe-c70c-4dda-9090-f2849b3d8dae?download=true&filename=CityPropertyClassCodes.pdf
# There are abbreviations not defined there, but found them here:
# http://grcity.us/city-clerk/CityCommission/061615epsec.pdf
# It appears there are four main categories for real property: commercial,
# industrial, residential, exempt.  There are also codes for personal property,
# which can be ignored (and which likely had no polygons, so would have been
# dropped by readOGR).  So, split up the real property codes into those four
# categories.
commercial_codes <- c(201, 202, 203, 204, 205, 208, 261, 265)
industrial_codes <- c(301, 303, 305, 308, 309, 361, 365, 901, 902, 903, 904, 951, 952, 953, 954)
residential_codes <- c(401, 402, 403, 446, 447, 448, 461, 465)
exempt_codes <- c(700, 761, 765)

commercial_parcel_rows <- parcel_data@data$propclass %in% commercial_codes
industrial_parcel_rows <- parcel_data@data$propclass %in% industrial_codes
residential_parcel_rows <- parcel_data@data$propclass %in% residential_codes
exempt_parcel_rows <- parcel_data@data$propclass %in% exempt_codes

commercial_parcel_count <- sum(commercial_parcel_rows)
industrial_parcel_count <- sum(industrial_parcel_rows)
residential_parcel_count <- sum(residential_parcel_rows)
exempt_parcel_count <- sum(exempt_parcel_rows)
# > commercial_parcel_count
# [1] 21769
# > industrial_parcel_count
# [1] 6203
# > residential_parcel_count
# [1] 312307
# > exempt_parcel_count
# [1] 36850

# Not all rows fall in those categories.
# > nrow(parcel_data@data)
# [1] 384674
# > nrow(parcel_data@data) - commercial_parcel_count - industrial_parcel_count - residential_parcel_count - exempt_parcel_count
# [1] 7545
# Get the codes for the remaining rows and check that they are all personal
# property or "obsolete" or other that we're not interested in.
other_parcel_rows <- mapply(function(a, b, c, d) !(a | b | c | d),
                            commercial_parcel_rows,
                            industrial_parcel_rows,
                            residential_parcel_rows,
                            exempt_parcel_rows)
# > sum(other_parcel_rows)
# [1] 7545
other_codes <- unique(parcel_data@data$propclass[other_parcel_rows])
# > length(other_codes)
# [1] 11
# > sort(other_codes)
# [1] 207 281 282 302 381 382 404 701 703 710

# Notice that the initial digits are mutually exclusive over the main
# categories.  The same digits are used for real and personal property, but,
# one presumes, personal property doesn't have polygons, so readOGR would not
# have loaded personal property rows.
commercial_codes <- c(201, 202, 203, 204, 205, 207, 208, 261, 265, 281, 282)
industrial_codes <- c(301, 302, 303, 305, 308, 309, 361, 365, 381, 382, 901, 902, 903, 904, 951, 952, 953, 954)
residential_codes <- c(401, 402, 403, 404, 446, 447, 448, 461, 465)
exempt_codes <- c(700, 761, 765)

# Now redo the above extraction of rows by category.
commercial_parcel_rows <- parcel_data@data$propclass %in% commercial_codes
industrial_parcel_rows <- parcel_data@data$propclass %in% industrial_codes
residential_parcel_rows <- parcel_data@data$propclass %in% residential_codes
exempt_parcel_rows <- parcel_data@data$propclass %in% exempt_codes

commercial_parcel_count <- sum(commercial_parcel_rows)
industrial_parcel_count <- sum(industrial_parcel_rows)
residential_parcel_count <- sum(residential_parcel_rows)
exempt_parcel_count <- sum(exempt_parcel_rows)
# > commercial_parcel_count
# [1] 21986
# > industrial_parcel_count
# [1] 6491
# > residential_parcel_count
# [1] 315409
# > exempt_parcel_count
# [1] 36885

# What codes are left over now?  There are some 7xx codes unaccounted for.
# Other 7xx are exempt -- does that imply these are as well?

other_parcel_rows <- mapply(function(a, b, c, d) !(a | b | c | d),
                            commercial_parcel_rows,
                            industrial_parcel_rows,
                            residential_parcel_rows,
                            exempt_parcel_rows)

# One of the leftovers is blank -- is that an NA?
# > sum(other_parcel_rows)
# [1] 3903
other_codes <- unique(parcel_data@data$propclass[other_parcel_rows])
# > length(other_codes)
# [1] 4
# > sort(other_codes)
# [1] 701 703 710

# commercial_parcels <- parcel_data[commercial_parcel_rows, ]
# industrial_parcels <- parcel_data[industrial_parcel_rows, ]
residential_parcels <- parcel_data[residential_parcel_rows, ]
# exempt_parcels <- parcel_data[exempt_parcel_rows, ]

# There's no reason to make the grid non-square, so we could lump the vertical
# and horizontal bbox dimensions together.  At the least, we just need need
# something bigger than twice the max of either.  However, we would like to
# know if there are outliers, which could indicate a code grouped with the
# wrong main category.  As a first check, just get the summary stats.  These
# are the overall ranges.
#
# Note that x is the east-west / longitude axis, and y is the north-south /
# latitude axis.  Later, we'll switch to a projected coordinate system, but x
# and y will retain their orientations.
bbox_width <- sapply(parcel_bboxes, function(b) abs(b["x", "max"] - b["x", "min"]))
# > summary(bbox_width)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0000070 0.0003400 0.0004110 0.0004121 0.0004670 0.0519100
bbox_height <- sapply(parcel_bboxes, function(b) abs(b["y", "max"] - b["y", "min"]))
# > summary(bbox_height)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0000060 0.0001180 0.0002040 0.0002265 0.0002880 0.0186500
# The actual distance corresponding to one degree of longitude and one degree of
# latitude differs, so can't compare them, but for purposes of forming a grid,
# that doesn't matter, so long as the scale is roughly the same.
bbox_extent <- c(bbox_width, bbox_height)
# > summary(bbox_extent)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0000060 0.0001630 0.0003060 0.0003193 0.0004200 0.0519100

# There are orders of magnitude difference between the extremes.  We'll likely
# need to get histograms to look for outliers or long tails.  First, what is
# the extent in an understandable distance measure of the two extremes?  The
# size of a degree of latitude or longitude at a given latitude is given here:
# http://msi.nga.mil/MSISiteContent/StaticFiles/Calculators/degree.html
# Approximate centroid of Detroit is found here:
# http://www.openstreetmap.org/relation/134591#map=14/42.3466/-83.0574&layers=HD
detroit_lon <- -83.0574
detroit_lat <- 42.3466
detroit_lon_degree_feet <- 270340
detroit_lat_degree_feet <- 364436
detroit_lon_degree_meters <- 82400
detroit_lat_degree_meters <- 111080

bbox_height_summary <- summary(bbox_height)
bbox_width_summary <- summary(bbox_width)

# The mean bbox width and height are reasonable for residential lot sizes.
bbox_height_mean_feet <- bbox_height_summary["Mean"] * detroit_lat_degree_feet
bbox_width_mean_feet <- bbox_width_summary["Mean"] * detroit_lon_degree_feet
# > bbox_width_mean_feet
#     Mean
# 111.4071
# > bbox_height_mean_feet
#     Mean
# 82.54475

# The extremes are...extreme.
bbox_width_min_feet <- bbox_width_summary["Min."] * detroit_lon_degree_feet
bbox_height_min_feet <- bbox_height_summary["Min."] * detroit_lat_degree_feet
bbox_width_max_feet <- bbox_width_summary["Max."] * detroit_lon_degree_feet
bbox_height_max_feet <- bbox_height_summary["Max."] * detroit_lat_degree_feet
# > bbox_width_min_feet
#    Min.
# 1.89265
# > bbox_height_min_feet
#     Min.
# 2.186616
# > bbox_width_max_feet
#     Max.
# 14033.35
# > bbox_height_max_feet
#     Max.
# 6796.731

# Get histograms of the parcel bounding box dimensions in feet.
bbox_width_feet <- bbox_width * detroit_lon_degree_feet
bbox_height_feet <- bbox_height * detroit_lat_degree_feet
# > summary(bbox_width_feet)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
#     1.893    91.920   111.100   111.400   126.200 14030.000
# > summary(bbox_height_feet)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#    2.187   43.000   74.340   82.550  105.000 6795.000
# Everything is at the low end if the few enormous parcels are included.
bbox_width_feet_hist <- hist(bbox_width_feet, breaks=100)
bbox_height_feet_hist <- hist(bbox_height_feet, breaks=100)
# How many parcels are over 1000 feet in width or height?
bbox_gt_1000_either_dim <- sum((bbox_width_feet > 1000) | (bbox_height_feet > 1000))
# > bbox_gt_1000_either_dim
# [1] 466
# What's the biggest?
# > which.max(bbox_width_feet)
# [1] 101581
# > which.max(bbox_height_feet)
# [1] 101581
biggest_parcel <- which.max(bbox_width_feet)
parcel_data@data[biggest_parcel, "taxpayer1"]
# > parcel_data@data[biggest_parcel, "taxpayer1"]
# [1] CITY OF DETROIT-RECREATION DEPT 15
# > bbox_width_feet[biggest_parcel]
# [1] 14033.08
# Not the airport?  Which one is the airport?  Turns out there are multiple
# parcels owned by the airport.
airport_parcels <- which(parcel_data@data[, "taxpayer1"] == "DETROIT CITY AIRPORT")
# > length(airport_parcels)
# [1] 39
# Only one is really big, though.
# > airport_parcels[which.max(bbox_width_feet[airport_parcels])]
# [1] 166729
# > airport_parcels[which.max(bbox_height_feet[airport_parcels])]
# [1] 166729
airport_parcel <- airport_parcels[which.max(bbox_width_feet[airport_parcels])]
# > bbox_width_feet[airport_parcel]
# [1] 5694.981
# > bbox_height_feet[airport_parcel]
# [1] 6176.096

# How many are under 10 feet in both width and height?
bbox_lt_10_both_dim <- sum((bbox_width_feet < 10) & (bbox_height_feet < 10))
# > bbox_lt_10_both_dim
# [1] 286
# There are little ones less than 10 ft on a side.  What are they?  There is a
# sharp break in the number of parcels at just under 9.5 ft square -- pick a
# size that selects out just a few.  In any case, they seem legitmate.
# > sum((bbox_width_feet < 9.48) & (bbox_height_feet < 9.48))
# [1] 186
# > sum((bbox_width_feet < 9.47) & (bbox_height_feet < 9.47))
# [1] 47
# > sum((bbox_width_feet < 9.46) & (bbox_height_feet < 9.46))
# [1] 11
# > parcel_data@data[(bbox_width_feet < 9.46) & (bbox_height_feet < 9.46), "taxpayer1"]
#  [1] "BERG, IRVING & HARRIET"              "KIRBY PARKING STRUCTURE LLC"
#  [3] "EL-HOSNI, MICHELLE & DANIEL MAROUNE" "FAMULARO, RICHARD R & PHYLLIS J"
#  [5] "LAPOINTE, DENISE A"                  "KIRBY PARKING STRUCTURE LLC"
#  [7] "CITY OF DETROIT-P&DD"                "CITY OF DETROIT"
#  [9] "MCDANIEL, EDWIN"                     "WABASH R R CO"
# [11] "EASTLAKE BAPTIST CHURCH"

# One concern to watch out for is that small parcels may be embedded in larger
# ones.  Will need to find out how often a point lands in more than one parcel,
# and select a way to disambiguate these cases -- perhaps always assign to the
# smaller parcel...or perhaps it would be more proper to assign to all.

# The biggest ones are several miles!  The Detroit airport is here, as are
# malls, corporate facilities, parks, cemetaries, golf courses...
parcel_width_gt_0.01 <- parcel_data[bbox_width > 0.01,]
# > nrow(parcel_width_gt_0.01)
# [1] 24
# > parcel_width_gt_0.01@data[,"taxpayer1"]
#  [1] DETROIT GATEWAY PARK MALL           MIDWEST MEMORIAL GROUP, LLC
#  [3] SIENA GROUP LLC                     CITY OF DETROIT RECREATION DEPT
#  [5] DETROIT GOLF CLUB                   CONRAIL CORP
#  [7] GENERAL MOTORS CORP                 D T RR CO
#  [9] MT ELLIOTT CEMETERY ASSOCIATION     STATE OF MICH
# [11] CITY OF DETROIT-RECREATION DEPT 15  LYNCH ROAD LAND LLC
# [13] DETROIT CITY AIRPORT                PENN CENTRAL
# [15] CITY OF DETROIT SEWAGE PLANT        CITY OF DETROIT WATER BOARD
# [17] MIDWEST MEMORIAL GROUP LLC          CITY OF DETROIT-RECREATION DEPT 234
# [19] FCA US LLC                          CITY OF DETROIT-RECREATION DEPT 037
# [21] CITY OF DETROIT-RECREATION DEPT 149 CITY OF DETROIT-RECREATION DEPT 149
# [23] CITY OF DETROIT-RECREATION DEPT 149 CITY OF DETROIT-RECREATION DEPT 56

# So, the extremes are legitimate.  But they are not residential properties.
# We might split up the properties by main category, in case the resulting
# groups are more uniform.  Get the stats on just the codes tentatively
# identified as residential.  The upper extremes are an order of magnitude
# smaller, but there are still some few-foot bboxes.
residential_bbox_width_summary <- summary(bbox_width[residential_parcel_rows])
residential_bbox_height_summary <- summary(bbox_height[residential_parcel_rows])
# > residential_bbox_height_summary
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0000090 0.0001170 0.0001900 0.0002031 0.0002820 0.0035440
# > residential_bbox_width_summary
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 0.0000140 0.0003600 0.0004160 0.0004008 0.0004680 0.0037880

# The sp polygons object appears to have an issue with a logical expression for
# row indexing -- it spits out nonsensical warning messages.  See, this is the
# problem with permitting the called function to peek into its un-processed
# argument list -- something no respectable language should ever do.  So to
# work around this quirk, construct a single logical vector first.
idx <- (bbox_width < 0.00002) & (residential_parcel_rows)
residential_width_lt_0.00002 <- parcel_data[idx,]
# > nrow(residential_width_lt_0.00002)
# [1] 14
# These are similar to the ones for all rows, except the RR entry is missing.
# So maybe these are equipment that is placed in residential areas.
# > residential_width_lt_0.00002@data[, "taxpayer1"]
#  [1] CITY OF DETROIT             CITY OF DETROIT
#  [3] DETROIT LAND BANK AUTHORITY TYLER, LLOYD
#  [5] ADAMS, VICTOR               ADAMS, VICTOR
#  [7] ADAMS, VICTOR               CITY OF DETROIT
#  [9] CITY OF DETROIT             CITY OF DETROIT
# [11] CITY OF DETROIT             ASKER, RANDY
# [13] HUD                         CARNES, TYNESIA
idx <- (bbox_width > 0.002) & (residential_parcel_rows)
residential_width_gt_0.002 <- parcel_data[idx,]
# > nrow(residential_width_gt_0.002)
# [1] 22
# So...are these McMansions?  Some appear to be housing projects or care
# facilities, but...I would not call a church or school "residential".  And
# what's that "Bank of America" row?  Do those have distinctive codes?  No.
# On the other hand, these are properties about which one might want to predict
# future blight.
# > residential_width_gt_0.002@data[, "taxpayer1"]
#  [1] RADINSKI, ROLAND A                  GREATER NEW MT MORIAH BAPT CHURCH
#  [3] LINK, NOAH & BRYAN, ALEX            GRAND RIVER PROPERTY HOLDINGS LLC
#  [5] DETROIT LAND BANK AUTHORITY         EASTWOOD SENIOR LEASING LLC
#  [7] BRADBY VOA AFFORDALE HOUSING LLC    DETROIT HOUSING COMMISSION
#  [9] CORNERSTONE SCHOOLS ASSOCIATION     RUFFIN, BRODERICK
# [11] DETROIT LAND BANK AUTHORITY         UNIVERSAL ACADEMY
# [13] CITY OF DETROIT                     BANK OF AMERICA
# [15] HERMAN GARDENS HSG                  HERMAN GARDENS HSG
# [17] HERMAN GARDENS HSG                  MILLS, LINDA K
# [19] BRADSHAW, CASSANDRA F               SNEAD, JOAN W
# [21] DETROIT LAND BANK AUTHORITY         CITY OF DETROIT-JOHN SMITH HOMES R5

# For now, don't omit any types of polygons.  The main question next is whether
# to represent the grid as a sparse matrix, e.g. hash table with index tuples
# as hash keys, or to instantiate the matrix as an array.  The latter is better
# if the matrix is actually not sparse.  Here, we expect that the polygons
# cover the land regions, so all positions inside the Detroit boundary that are
# not water.  If Detroit were really awkwardly shaped for a north-south
# east-west grid, e.g. if it were diamond shaped, then half the cells would be
# empty.  Should probably use an array to start with, and histogram the # of
# polygons (or rather their bounding boxes) that overlap each cell.  Then
# adjust the grid size to balance the # cells with # polygons per cell.  Treat
# the horizontal (east-west) and vertical (north-south) grid sizes as tunable
# parameters.

# What is the span of the grid, i.e. how big is the bounding box for all the
# polygons?
parcel_data_bbox <- bbox(parcel_data)
# > print(parcel_data_bbox, digits=12)
#              min            max
# x -83.2876458699 -82.9106529877
# y  42.2554018867  42.4501028429
grid_total_height <- parcel_data_bbox["y", "max"] - parcel_data_bbox["y", "min"]
grid_total_width <- parcel_data_bbox["x", "max"] - parcel_data_bbox["x", "min"]
# Notice that the sp bbox function uses x and y not longitude and latitude.
# For consistency, stick with that naming convention for the grid.  Over this
# small of an extent, spherical geometry has very little effect on the results.

# Initial guess for grid sizes -- use a small multiple of the mean bbox height
# and width.  Us a common multiplier.  We'll adjust this so that the grid
# matrix isn't too outrageously big, and we don't have too many polygons per
# grid cell.  R weirdness note:  bbox_height_summary["Mean"] is a *table* with
# one value.  If we don't get the number out of the table, it will propagate
# that tableness into the result of any arithmetic expression it participates
# in...*except* for comparisons against numbers.  Those will fail, because the
# comparison will check if the type is the same.  Fortunately, as.numeric will
# strip away the tableness...and the name "Mean" as well.  We need to get rid
# of the name, as otherwise that will infect future names derived from these
# values.
grid_mean_multiplier <- 4
grid_cell_height <- as.numeric(bbox_height_summary["Mean"]) * grid_mean_multiplier
grid_cell_width <- as.numeric(bbox_width_summary["Mean"]) * grid_mean_multiplier
# grid_cell_height <- bbox_height_summary["Mean"] * grid_mean_multiplier
# grid_cell_width <- bbox_width_summary["Mean"] * grid_mean_multiplier
# Those are both named numbers, called "Mean".  Make the names go away else
# they will infect all future names derived from these numbers.
#names(grid_cell_height) <- NULL
#names(grid_cell_width) <- NULL

grid_height_num_cells <- ceiling(grid_total_height / grid_cell_height)
grid_width_num_cells <- ceiling(grid_total_width / grid_cell_width)
# With a multiplier of 4, we get on the order of 220 cells across and high.
# That's not too many, so stick with that for now.
# > grid_height_num_cells
#  215
# > grid_width_num_cells
#  229
# > grid_height_num_cells * grid_width_num_cells
# 49235

# Instantiate the grid.  Hmm...what sort of array do we want?  The contents are
# not primitives, but rather each holds a list, i.e. a reference.  In Matlab,
# this would be a cell array.  R doesn't have such a thing, but does permit
# one to have lists as matrix elements.
# http://stackoverflow.com/questions/25272183/r-equivalent-of-a-matlab-cell-matrix
# If we expect this to be sparse, we wouldn't want to create a list in a cell
# unless it's needed.  But we don't expect it to be sparse since the parcel
# polygons cover the Detroit area -- it's just the parts outside the Detroit
# bounds that will be empty.  Putting in the lists now wastes a bit of space
# but will be simpler later, and won't confuse R as to the type of the
# resulting matrix (it thinks the type is logical if one puts in NA for all
# cells, for instance).  (Don't call the matrix "grid" as that's a plotting
# function.)  (Note rep needs a list of the items to repeat, hence the empty
# list inside a list.)
grid_parcels <- matrix(data=rep(list(list()),
                                grid_width_num_cells*grid_height_num_cells),
                       nrow=grid_width_num_cells,
                       ncol=grid_height_num_cells)

# If / when we filter parcel assignment to grid cells by their actual polygons
# rather than just their bounding boxes, we'll need a polygon for each grid
# cell, to intersect with the parcel polygon.  Instead of instantiating this
# each time it's needed, make these once.  This is reasonable because the
# number of grid cells is not that large so the storage use is not excessive,
# but the number of times we do this check will be some multiple of the number
# of parcels.  Use official sp tools to do this.  The parameters for
# GridTopology are in the order x,y, i.e. longitude, latitude, or horizontal,
# vertical ... NOT row, column.  This...
# http://geostat-course.org/system/files/monday_slides.pdf
# ...says the first argument of GridTopology is the "south-west" corner.
# This is confusing because that's not where SpatialGrid starts its numbering.
# It starts in the *north*-west cornder -- this is shown by a plot in the very
# same document:  The element order goes left to right along the top cell row,
# then the row below that,...  Why would they be inconsistent like this?  Did
# they switch to *image* layout because they're thinking of rasters, instead
# of real-world longitude, latitude layout?  If so, this needs to be spelled
# out excruciatingly clearly.  Yah, clarity is not the R Way, but please, let's
# have an R revolution, and start writing real, usable documentation, not
# insider hints for people who *already know how the tool works*.
grid_cellsize <- c(grid_cell_width, grid_cell_height)
# Construct the center of the north-west cell.  That cell has x min, y max as
# its upper left corner.  The center of that cell is *below* y max by half the
# cell height.
#grid_offset <- c(parcel_data_bbox["x","min"] + 0.5 * grid_cell_width,
#              parcel_data_bbox["y","max"] - 0.5 * grid_cell_height)
# No, construct the center of the south-west cell.
grid_min <- c(parcel_data_bbox["x","min"], parcel_data_bbox["y","min"])
grid_offset <- grid_min + 0.5 * grid_cellsize
grid_ncell <- c(grid_width_num_cells, grid_height_num_cells)
grid_gt <- GridTopology(grid_offset, grid_cellsize, grid_ncell)
grid_sg <- SpatialGrid(grid_gt)
grid_spol <- as(grid_sg, "SpatialPolygons")
# In order to use the rgeom gIntersects function on each individual grid cell
# polygon, they have to each be a SpatialPolygons object -- gIntersects needs
# to have the coordinate reference system (CRS).  Use the parcel_data CRS.
grid_spol_list <- lapply(grid_spol@polygons,
                         function(p) SpatialPolygons(list(p),
                                                     proj4string=parcel_data@proj4string))

# Convert to a matrix.  Want the x direction (longitude) as the first
# dimension, y (latitude) as the second.  Want to be able to compute the lower
# left lon, lat corner of each grid cell as:
# x_min + (x_index - 1) * x_cellsize, y_min + (y_index - 1) * y_cellsize
# (Note the inconsistency between layout of a physical x, y grid, where the
# first dimesion is horizontal, and the second is vertical, with the layout
# of a spreadsheet or data frame.  Yes, this is weird, and yes, we shouldn't
# have borked the convention like this for spreadsheets vs. spatial layout.)
# Because the sp "grid" is in raster (image) order, starting at the "top" of
# the "image", this won't quite give us the layout we want...see the next
# step...
grid_bboxes_backwards <- matrix(data=grid_spol_list,
                                nrow=grid_width_num_cells,
                                ncol=grid_height_num_cells,
                                byrow=FALSE)

# SpatialGrid lists its elements in raster row order, i.e. it starts with the
# top left, numbers along the top row, then goes to the left side of the row
# below that,...  We want x-y plot order.  So, we need to reverse the order of
# the y axis.
grid_bboxes <- grid_bboxes_backwards[ , grid_height_num_cells:1]

# Spot check.
# parcel_data_bbox
#         min       max
# x -83.28765 -82.91065
# y  42.25540  42.45010
# grid_bboxes[1,1][[1]]@coords[1,]
# -83.28765  42.25540
# grid_bboxes[1,2][[1]]@coords[1,]
# -83.28765  42.25631
# grid_bboxes[1,grid_height_num_cells][[1]]@coords[1,]
# -83.28765  42.44929
# grid_bboxes[2,1][[1]]@coords[1,]
# -83.2860  42.2554
# grid_bboxes[grid_width_num_cells,1][[1]]@coords[1,]
# -82.91181  42.25540

# Package up all those grid-related bits of info so they can be passed to a
# function easily.
grid_data <- list(
    grid_parcels=grid_parcels,
    grid_bboxes=grid_bboxes,
    grid_x_min=parcel_data_bbox["x","min"],
    grid_y_min=parcel_data_bbox["y","min"],
    grid_x_cellsize=grid_cell_width,
    grid_y_cellsize=grid_cell_height,
    grid_x_num_cells=grid_width_num_cells,
    grid_y_num_cells=grid_height_num_cells
    )

# All those bounding box polygon structures except the last are intermediate
# values.  Remove the large ones.  Eventually...not just yet.
rm(grid_min,
   grid_offset,
   grid_ncell,
   grid_gt,
   grid_sg,
   grid_spol,
   grid_spol_list,
   grid_bboxes_backwards)

# Assign parcels to grid cells in two steps.  First, determine which cells a
# parcel might belong to based on its bounding box.  This does not require
# intersection -- it can be computed directly.  (This could work well by itself
# for the small and medium properties, but the bounding boxes of enormous
# properties are more likely to cover cells that their actual polygons do not.)
# Second, for each grid cell that overlaps the parcel's bounding box, check if
# there is an overlap between the grid cell polygon and the actual parcel
# polygon.  For each actual overlap, assign the parcel row number to that grid
# cell.

# First, need a function to compute the grid cell a point is in.  Can we assume
# it won't fall on a boundary?  No, we should not, especially if this will also
# be used to assign incidents to parcels.  Compute the row and column that
# falls in, and if on a boundary, return a tuple with indices on both sides,
# but exclude any ouside the bounds of the grid.  For instance, if we pass in
# the (xmin, ymin) of the entire grid, we want to get only cell [1, 1].
# Remember that indices start from 1.
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
    z_cells <- list()
    # z_cell is either interior or the upper side -- do we want to include it?
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
        z_cells <- c(below=z_cell-1, z_cells)
    }
    z_cells
}

# Get the grid cells for a pair of coordinates.
# Arguments:
# x - longitude
# y - latitude
# grid_data - the structure containing the grid and its metadata
#
point_to_grid_cells <- function(x, y, grid_data) {
    x_cells <- coordinate_to_grid_indices(x,
                                          grid_data$grid_x_min,
                                          grid_data$grid_x_cellsize,
                                          grid_data$grid_x_num_cells)
    y_cells <- coordinate_to_grid_indices(y,
                                          grid_data$grid_y_min,
                                          grid_data$grid_y_cellsize,
                                          grid_data$grid_y_num_cells)
    list(x=x_cells, y=y_cells)
}

# Next assign a parcel to each grid cell its polygon overlaps.
#
# Q: Is it worth checking the actual polygon, or just its bounding box, which
# is much much much faster? How much extra work will be done when we use the
# grid? We'll be comparing points, not polygons. We would be doing point-in-
# polygon for some polygons that don't actually overlap the grid cell they're
# assigned to...but how common will that be? For normal parcels, this will be
# very rare, as the parcels are smaller than the grid size. It will happen for
# the very large parcels (airport, mall,...) if they have irregular shapes
# that are not grid-aligned. Many will be on city-block layout, one expects,
# and if so, they'll be fairly-well grid-aligned. On the other hand, now that
# I have the grid polygons, which are *only* useful for intersecting with the
# parcel polygons, that shifts the balance toward filtering by the actual
# parcel polygons.
#
# Start by restricting the cells to check to those overlapped by the polygon's
# bounding box. (It does not overlap a cell if its edge is on a cell boundary
# but none of the interior protrudes into the cell interior.) Get the cell that
# the bounding box's (xmin, ymin) point belongs to. Then compute how many cells
# the width and height span, to determine the last row and column.
#
# Check for a non-empty intersection of the polygon with all grid cells in that
# span. If there's an overlap, insert the polygon's row number into the grid
# cell.
#
# Arguments:
# parcel_row - row number in parcel_data and parcel_bbox
# parcel_data - Detroit parcels
# parcel_bboxes - bounding boxes for the parcel polygons
# grid_data - the structure containing the grid and its metadata
#
# Return value:
# The entire modified grid_parcels matrix, which must be re-assigned back into
# parcel_data by the caller.
#
# <rant>  This is a consequence of the misguided decision not to support pass
# by reference in R. No, sorry, R fanatics, it is not a matter of "safety" or
# "avoiding side effects".  If we know the semantics of the language, then it's
# just as safe to mutate a passed object as it is to *overwrite* that object
# on function exit.  And guess what!  The operating system on which you are
# running R is, with very high probability, written in a language that has pass
# by reference, and makes heavy use of that feature.  I don't care which
# operating system you're using -- it *is* doing pass by reference, unless
# you're running on some obsure Erlang-based system.  The reason your OS is
# doing pass by reference is ***efficiency***.  Oh, and, did you know there's
# a language *designed* for safety?  And it uses pass by reference for objects?
# That's Java.  Designed by actual computer scientists and language developers,
# with the goal of reducing the incidence of bugs that are the biggest waste of
# developer time, and the biggest cause of security holes.  And they allowed
# pass by reference, because it was *not* a big cause of bugs or security
# issues.  </rant>
#
# Ok...this can be tolerated because it will be run only once.  If I were going
# to have to update the parcel grid repeatedly, I'd either use one of the
# (awkward, worrisome) methods for faking pass by reference, or I'd forego
# modularity, and make one giant function that included the guts of this
# function in a loop over rows of the parcel data.
#
assign_polygon_to_grid <- function(parcel_row,
                                   parcel_data,
                                   parcel_bboxes,
                                   grid_data,
                                   verbose=FALSE) {
    # Compute minimum grid indices along x and y that this parcel overlaps.
    min_cell <- point_to_grid_cells(parcel_bboxes[[parcel_row]]["x", "min"],
                                    parcel_bboxes[[parcel_row]]["y", "min"],
                                    grid_data)
    # If this point falls on a grid division in either the x or y direction,
    # then we may have two indices for that direction.  If we were checking
    # an incident location, we would want to test the point against parcels
    # on either adjacent cell.  But here, we have a parcel bounding box, and
    # want to see which cells it overlaps.  We know that the interior of the
    # bounding box lies above and to the right of the min x, min y bounding
    # box corner, so we only want the "interior" or "above" index.  Because
    # the grid was sized to cover all the parcel polygons, this polygon's
    # bounding box will not extend beyond the grid region.  Note if we get
    # a new version of the parcel boundary dataset, we need to re-run the
    # entire processing script.
    min_cell_x <- min_cell$x$interior
    if (is.null(min_cell_x)) {
        min_cell_x <- min_cell$x$above
    }
    min_cell_y <- min_cell$y$interior
    if (is.null(min_cell_y)) {
        min_cell_y <- min_cell$y$above
    }
    # Now find the the grid indices that the parcel ends in.  Get the indices
    # of the upper right corner of the bounding box.  If this lands exactly on
    # a grid cell division, we only want to include the cells "below" the
    # division.
    max_cell <- point_to_grid_cells(parcel_bboxes[[parcel_row]]["x", "max"],
                                    parcel_bboxes[[parcel_row]]["y", "max"],
                                    grid_data)
    max_cell_x <- max_cell$x$interior
    if (is.null(max_cell_x)) {
        max_cell_x <- max_cell$x$below
    }
    max_cell_y <- max_cell$y$interior
    if (is.null(max_cell_y)) {
        max_cell_y <- max_cell$y$below
    }
    # Iterate over all grid cells in that span.  This is an inclusive range.
    if (verbose) {
        cat("Row", row.names(parcel_data[parcel_row,]),
            "spans grid cells x:", min_cell_x, "-", max_cell_x,
            "y:", min_cell_y, "-", max_cell_y, "\n")
    }
    for (x_idx in min_cell_x:max_cell_x) {
        for (y_idx in min_cell_y:max_cell_y) {
            # Does the parcel polygon intersect this cell?  gIntersects needs
            # SpatialPolygons objects.  The grid cells are already that class,
            # but the individual rows in the parcel_data@polygons are ordinary
            # Polygons objects -- the entire parcel_data@polygons is a
            # SpatialPolygons object.  So make a SpatialPolygons out of the
            # single row.
            sp <- SpatialPolygons(list(parcel_data@polygons[[parcel_row]]),
                                  proj4string=parcel_data@proj4string)
            # R seems incapable of dereferencing an element from a list if that
            # list is in a matrix element.  All R non-primitives get packaged
            # in a list if they are put in a matrix, but R just keeps returning
            # a list, no matter how many times one applies [[1]] to that matrix
            # element.  However, if one assigns the matrix element to another
            # variable, R seems to forget that it came from a matrix, and [[1]]
            # returns the list element successfully.  Don't allow R to be weird
            # here -- explicitly take the grid cell list out of the matrix
            # before attempting to take the grid cell polygon out of that list.
            cell_list <- grid_data$grid_bboxes[x_idx, y_idx]
            cell <- cell_list[[1]]
            if (gIntersects(cell, sp)) {
                # Found an overlap between a parcel and this grid cell.
                # Add this parcel to the list for this cell.  Note these lists
                # are not expected to have more than a few elements, so no need
                # to worry about extending them one element at a time.
                # Please do not blame me for the bizarre behavior of R
                # matrices.  When they hold non-primitive contents, those
                # contents are packaged in a list.  (This is true even though
                # there are no bare primitives -- all primitives are in vectors
                # and vectors are lists.  So even primitives, it seems, should
                # be packed in lists.  But that would only hold were R a
                # *consistent* language...)  So our lists that hold parcel
                # row numbers are packaged inside lists, hence the [[1]] to
                # get them back out.
                grid_data$grid_parcels[x_idx, y_idx][[1]][[length(grid_data$grid_parcels[x_idx, y_idx][[1]])+1]] <- parcel_row
                if (verbose) {
                    cat("Assigning", row.names(parcel_data[parcel_row,]),
                        "to grid cell [", x_idx, ",", y_idx, "]", "\n")
                }
            }
        }
    }
    # Ugh.  Ugh ugh ugh ugh.  Return the new copy of the entire grid_parcels
    # matrix.
    grid_data$grid_parcels
}

# Need to test that on a small dataset where the correct results are known.
# Make a tiny grid with 3 x 4 cells. Offset the grid from integers just a
# little so it will be affected by floating-point precision issues.
fp_spoiler_x <- 0.001/3
fp_spoiler_y <- 0.002/3
test_grid_min <- c(1+fp_spoiler_x, 2+fp_spoiler_y)
test_grid_cellsize <- c(1,2)
test_grid_offset <- test_grid_min + 0.5 * test_grid_cellsize
test_grid_ncell <- c(3,4)
test_grid_gt <- GridTopology(test_grid_offset, test_grid_cellsize, test_grid_ncell)
test_grid_sg <- SpatialGrid(test_grid_gt)
test_grid_spol <- as(test_grid_sg, "SpatialPolygons")
test_grid_spol_list <- lapply(test_grid_spol@polygons,
                              function(p) SpatialPolygons(list(p),
                                                          proj4string=parcel_data@proj4string))

test_grid_bboxes_backwards <- matrix(data=test_grid_spol_list,
                                     nrow=test_grid_ncell[1],
                                     ncol=test_grid_ncell[2],
                                     byrow=FALSE)
test_grid_bboxes <- test_grid_bboxes_backwards[ , test_grid_ncell[2]:1]
test_grid_parcels <- matrix(data=rep(list(list()),
                                     test_grid_ncell[1]*test_grid_ncell[2]),
                            nrow=test_grid_ncell[1],
                            ncol=test_grid_ncell[2])
test_grid_data <- list(
    grid_parcels=test_grid_parcels,
    grid_bboxes=test_grid_bboxes,
    grid_x_min=test_grid_min[1],
    grid_y_min=test_grid_min[2],
    grid_x_cellsize=test_grid_cellsize[1],
    grid_y_cellsize=test_grid_cellsize[2],
    grid_x_num_cells=test_grid_ncell[1],
    grid_y_num_cells=test_grid_ncell[2]
    )

# Make a SpatialPolygonsDataFrame with a few simple rectangular polygons that
# cover the test cases.  Don't bother with a fancy @data slot.  (Set hole
# explicitly for clarity, but also obey the Polygon convention that an "island"
# polygon lists its points in clockwise order, and a "hole" counter-clockwise.
# Use the same coordinate reference system (CRS) as the actual parcel data,
# which is WGS84.  That means these small "latitude" and "longitude" values
# are near the infamous Zero-Zero Island, where lost map markers lie.)

# This rectangle lies entirely within one grid cell (the one at grid indices
# [1,1], which has corners (1,2), (1,4), (2,4), (2,2)).
p1 <- Polygon(coords=matrix(c(1.1, 2.1,
                              1.1, 3.9,
                              1.9, 3.9,
                              1.9, 2.1,
                              1.1, 2.1), ncol=2, byrow=TRUE),
              hole=FALSE)
ps1 <- Polygons(srl=list(p1), ID="p1")
# This overlaps several cells ([1,1], [2,1], [1,2], [2,2]).
p2 <- Polygon(coords=matrix(c(1.5, 2.5,
                              1.5, 5.5,
                              2.5, 5.5,
                              2.5, 2.5,
                              1.5, 2.5), ncol=2, byrow=TRUE),
              hole=FALSE)
ps2 <- Polygons(srl=list(p2), ID="p2")
# This is the same as a grid cell ([2,2]), to test that it is assigned to only
# that cell. Recall we added a little offset to the grid to spoil the integers,
# so have to do that here too.
p3 <- Polygon(coords=matrix(c(2.0+fp_spoiler_x, 4.0+fp_spoiler_y,
                              2.0+fp_spoiler_x, 6.0+fp_spoiler_y,
                              3.0+fp_spoiler_x, 6.0+fp_spoiler_y,
                              3.0+fp_spoiler_x, 4.0+fp_spoiler_y,
                              2.0+fp_spoiler_x, 4.0+fp_spoiler_y),
                            ncol=2, byrow=TRUE),
              hole=FALSE)
ps3 <- Polygons(srl=list(p3), ID="p3")
# This has a hole that excludes a grid cell but where the outer rectangle
# overlaps other cells.  This requires two Polygon objects, one for the outer
# boundary and one for the hole.  (The excluded cell is [2,3], and the polygon
# overlaps the cells around that, [1,2], [1,3], [1,4], [2,4], [3,4], [3,3],
# [3,2], [2,2].)
p4isle <- Polygon(coords=matrix(c(1.5, 4.5,
                                  1.5, 9.5,
                                  3.5, 9.5,
                                  3.5, 4.5,
                                  1.5, 4.5), ncol=2, byrow=TRUE),
                  hole=FALSE)
p4hole <- Polygon(coords=matrix(c(1.9, 5.9,
                                  3.1, 5.9,
                                  3.1, 8.1,
                                  1.9, 8.1,
                                  1.9, 5.9), ncol=2, byrow=TRUE),
                  hole=TRUE)
ps4 <- Polygons(srl=list(p4isle, p4hole), ID="p4")

# At this point, we have some cells with zero polygons, some with one, some
# with more than one.  Combine them in a SpatialPolygons.
sps <- SpatialPolygons(Srl=list(ps1, ps2, ps3, ps4),
                       proj4string=parcel_data@proj4string)
# Make a trivial data frame with just one column.  Add row names that match the
# ID values in the SpatialPolygons.
df <- data.frame(ID=c("p1", "p2", "p3", "p4"),
                 row.names=c("p1", "p2", "p3", "p4"))
# And finally, the SpatialPolygonsDataFrame.
test_spdf <- SpatialPolygonsDataFrame(Sr=sps, data=df)
# Get the bounding boxes.
test_bboxes <- lapply(test_spdf@polygons, bbox)

# Here's what we expect in the test parcel grid:
# [1,1]:  p1, p2
# [1,2]:  p2, p4
# [1,3]:  p4
# [1,4]:  p4
# [2,1]:  p2
# [2,2]:  p2, p3, p4
# [2,3]:
# [2,4]:  p4
# [3,1]:
# [3,2]:  p4
# [3,3]:  p4
# [3,4]:  p4

# Run the test.  Note to self:  Remember to clear the grid parcels matrix
# before re-running the test...
test_grid_data$grid_parcels <- assign_polygon_to_grid(1, test_spdf, test_bboxes, test_grid_data, TRUE)
test_grid_data$grid_parcels <- assign_polygon_to_grid(2, test_spdf, test_bboxes, test_grid_data, TRUE)
test_grid_data$grid_parcels <- assign_polygon_to_grid(3, test_spdf, test_bboxes, test_grid_data, TRUE)
test_grid_data$grid_parcels <- assign_polygon_to_grid(4, test_spdf, test_bboxes, test_grid_data, TRUE)

# And?
for (x_idx in 1:test_grid_data$grid_x_num_cells) {
    for (y_idx in 1:test_grid_data$grid_y_num_cells) {
        cat("[", x_idx, ",", y_idx, "]: ", sep="")
        cell_list_list <- test_grid_data$grid_parcels[x_idx,y_idx]
        cell_list <- cell_list_list[[1]]
        if (length(cell_list) > 0) {
            for (x in 1:length(cell_list)) {
                row <- cell_list[[x]]
                # Creating the SPDF appears to have made the ID column factor.
                row_id <- as.character(test_spdf@data[row, "ID"])
                cat(row_id, ", ")
            }
        }
        cat("\n")
    }
}

# Works:
# [1,1]: p1 , p2 ,
# [1,2]: p2 , p4 ,
# [1,3]: p4 ,
# [1,4]: p4 ,
# [2,1]: p2 ,
# [2,2]: p2 , p3 , p4 ,
# [2,3]:
# [2,4]: p4 ,
# [3,1]:
# [3,2]: p4 ,
# [3,3]: p4 ,
# [3,4]: p4 ,

# Also need to check that without pre-constructing the grid and tweaking it to
# illustrate floating point issues, so we can use this same test case to check
# a more efficient version of the process (see later).  This code is the same as
# was used to create the grid for parcel_data, but with the variable names
# changed.

test_spdf_bboxes <- lapply(test_spdf@polygons, bbox)
test_spdf_bbox_width <- sapply(test_spdf_bboxes, function(b) abs(b["x", "max"] - b["x", "min"]))
test_spdf_bbox_height <- sapply(test_spdf_bboxes, function(b) abs(b["y", "max"] - b["y", "min"]))
test_spdf_bbox_height_summary <- summary(test_spdf_bbox_height)
test_spdf_bbox_width_summary <- summary(test_spdf_bbox_width)
test_spdf_bbox <- bbox(test_spdf)
test_spdf_grid_total_height <- test_spdf_bbox["y", "max"] - test_spdf_bbox["y", "min"]
test_spdf_grid_total_width <- test_spdf_bbox["x", "max"] - test_spdf_bbox["x", "min"]
test_spdf_grid_mean_multiplier <- 1
# We're using median now.
test_spdf_grid_cell_height <- as.numeric(test_spdf_bbox_height_summary["Median"]) * test_spdf_grid_mean_multiplier
test_spdf_grid_cell_width <- as.numeric(test_spdf_bbox_width_summary["Median"]) * test_spdf_grid_mean_multiplier
test_spdf_grid_height_num_cells <- ceiling(test_spdf_grid_total_height / test_spdf_grid_cell_height)
test_spdf_grid_width_num_cells <- ceiling(test_spdf_grid_total_width / test_spdf_grid_cell_width)
test_spdf_grid_parcels <- matrix(data=rep(list(list()),
    test_spdf_grid_width_num_cells * test_spdf_grid_height_num_cells),
    nrow=test_spdf_grid_width_num_cells,
    ncol=test_spdf_grid_height_num_cells)
test_spdf_grid_cellsize <- c(test_spdf_grid_cell_width, test_spdf_grid_cell_height)
test_spdf_grid_min <- c(test_spdf_bbox["x","min"], test_spdf_bbox["y","min"])
test_spdf_grid_offset <- test_spdf_grid_min + 0.5 * test_spdf_grid_cellsize
test_spdf_grid_ncell <- c(test_spdf_grid_width_num_cells, test_spdf_grid_height_num_cells)
test_spdf_grid_gt <- GridTopology(test_spdf_grid_offset, test_spdf_grid_cellsize, test_spdf_grid_ncell)
test_spdf_grid_sg <- SpatialGrid(test_spdf_grid_gt)
test_spdf_grid_spol <- as(test_spdf_grid_sg, "SpatialPolygons")
test_spdf_grid_spol_list <- lapply(test_spdf_grid_spol@polygons,
    function(p) SpatialPolygons(list(p),
    proj4string=test_spdf@proj4string))
test_spdf_grid_bboxes_backwards <- matrix(data=test_spdf_grid_spol_list,
    nrow=test_spdf_grid_width_num_cells,
    ncol=test_spdf_grid_height_num_cells,
    byrow=FALSE)
test_spdf_grid_bboxes <- test_spdf_grid_bboxes_backwards[ , test_spdf_grid_height_num_cells:1]
test_spdf_grid_data <- list(
    grid_parcels=test_spdf_grid_parcels,
    grid_bboxes=test_spdf_grid_bboxes,
    grid_x_min=test_spdf_grid_min[1],
    grid_y_min=test_spdf_grid_min[2],
    grid_x_cellsize=test_spdf_grid_cellsize[1],
    grid_y_cellsize=test_spdf_grid_cellsize[2],
    grid_x_num_cells=test_spdf_grid_ncell[1],
    grid_y_num_cells=test_spdf_grid_ncell[2]
    )
test_spdf_grid_data$grid_parcels <- assign_polygon_to_grid(1, test_spdf, test_bboxes, test_spdf_grid_data, TRUE)
test_spdf_grid_data$grid_parcels <- assign_polygon_to_grid(2, test_spdf, test_bboxes, test_spdf_grid_data, TRUE)
test_spdf_grid_data$grid_parcels <- assign_polygon_to_grid(3, test_spdf, test_bboxes, test_spdf_grid_data, TRUE)
test_spdf_grid_data$grid_parcels <- assign_polygon_to_grid(4, test_spdf, test_bboxes, test_spdf_grid_data, TRUE)
for (x_idx in 1:test_spdf_grid_data$grid_x_num_cells) {
    for (y_idx in 1:test_spdf_grid_data$grid_y_num_cells) {
        cat("[", x_idx, ",", y_idx, "]: ", sep="")
        cell_list_list <- test_spdf_grid_data$grid_parcels[x_idx,y_idx]
        cell_list <- cell_list_list[[1]]
        if (length(cell_list) > 0) {
            for (x in 1:length(cell_list)) {
                row <- cell_list[[x]]
                row_id <- as.character(test_spdf@data[row, "ID"])
                cat(row_id, ", ")
            }
        }
        cat("\n")
    }
}

# This is what happens without the floating point tweaking, and using median
# instead of mean.  The computed cell sizes are not the same as the
# hand-selected sizes.

# [1,1]: p1 , p2 , p3 , p4 ,
# [1,2]: p2 , p3 , p4 ,
# [1,3]: p4 ,
# [2,1]: p2 , p3 , p4 ,
# [2,2]: p2 , p3 , p4 ,
# [2,3]: p4 ,
# [3,1]: p4 ,
# [3,2]: p4 ,
# [3,3]: p4 ,

# Now run that on the real data. Let's time this... I'm running this on a
# six year old laptop, Intel Core I3, two processors, four cores, 8GB memory,
# SSD (though that's not relevant for this operation except if there's paging).
# Don't turn on parallelism -- this isn't designed to take advantage of it.
# Also, I have other things running, that are using at least one core.
start_time <- proc.time()
for (row in 1:nrow(parcel_data)) {
    grid_data$grid_parcels <- assign_polygon_to_grid(row,
                                                     parcel_data,
                                                     parcel_bboxes,
                                                     grid_data,
                                                     FALSE)
}
diff_time <- proc.time() - start_time
# > diff_time
#     user   system  elapsed
# 10129.92   171.71 10832.23
# > diff_time["elapsed"] / 3600
#  elapsed
# 3.008953
# It doesn't look quite so bad per parcel:
# > diff_time["elapsed"] / nrow(parcel_data)
#    elapsed
# 0.02815951

# So, that took three hours of wall-clock time.  No good, it should be much
# faster.  From watching Windows Task Manager during the run, this used one
# core completely (as confirmed by user time being close to elapsed time),
# but more importantly, the memory use didn't grow -- it ranged from 1 to 1.4
# GB, and when it got to 1.4, would drop back to 1, so the R garbage collector
# was doing its job.  However, the GC should not even have a job to do --
# that's an artifact of copying and throwing away the grid parcel matrix on
# each loop pass.

# There are faster and / or more efficient ways to do this -- the above is
# nearly pessimal for R (though it would not be for a language with pass by
# reference).  First, some alternatives that still use R:
# 1) Put the entire grid construction inside a function, and do not use nested
# functions.
# 2) Invert the index:  Have the function that's called on each loop pass return
# a list of grid index pairs for the given row.  Then have the caller insert
# those in the matrix.  This avoids the copy of the matrix without giving R
# style critics fits.
# 3) Execute the matrix modifications in the scope of the enclosing environment.
# To heck with the R style critics.
# But maybe it's time to consider alternatives...
# 4) Use a language that supports pass by reference *and* synchronization.
# Like Java.  Start a pool of worker threads that pick their row to work on
# off of a counter guarded by a mutex -- just define the counter's increment
# method as synchronized.  Likewise, have the update method for each grid
# cell be synchronized.
# 5) Use a mapreduce framework, like Hadoop or AWS EMR.

# Before we try some of those other methods, let's get some statistics on how
# densely the grid is occupied -- histogram the length of the lists in the grid
# cells.  This gives an idea of whether the current "multiplier" is correct.
# If the grid construction is packaged up, would be nice to have it adjust the
# multiplier to get near some specific target for average number of polygons per
# cell.

# I'd like to use a vectorized operation.  Ah, apply will do it -- specify both
# margins.  Now...will apply take the contained list out of the matrix first
# before attempting to apply the function?  Because, as we've seen, each element
# is packed in a list, one cannot extract the contents unless one assigns that
# container list to a different variable.  Try apply'ing length() and see if we
# get all 1s.
test_grid_num_polys_per_cell <- apply(test_grid_data$grid_parcels, c(1,2),
                                      length)
# Yep, we do.  Okfine.
test_grid_num_polys_per_cell <- apply(test_grid_data$grid_parcels, c(1,2),
                                      function(cell_list_list) {
    # Get the contents out of the matrix's container list.
    cell_list <- cell_list_list[[1]]
    # Number of polygons:
    length(cell_list)
})
# > test_grid_num_polys_per_cell
#      [,1] [,2] [,3] [,4]
# [1,]    2    2    1    1
# [2,]    1    3    0    1
# [3,]    0    1    1    1

# Run it on the real data.
grid_num_polys_per_cell <- apply(grid_data$grid_parcels, c(1,2),
                                      function(cell_list_list) {
    # Get the contents out of the matrix's container list.
    cell_list <- cell_list_list[[1]]
    # Number of polygons:
    length(cell_list)
})
# Get the histogram.
grid_num_polys_hist <- table(as.vector(grid_num_polys_per_cell))
# > length(grid_num_polys_hist)
# [1] 82
# Ugh.  That implies there are too many polygons per cell.  And, it's sparse.
# > grid_num_polys_hist
#     0     1     2     3     4     5     6     7     8     9    10    11    12    13
# 22364  2398  1072   849   617   560   443   420   442   364   420   333   357   336
#    14    15    16    17    18    19    20    21    22    23    24    25    26    27
#   331   316   358   387   426   478   503   548   535   590   662   603   661   762
#    28    29    30    31    32    33    34    35    36    37    38    39    40    41
#   729   733   729   774   818   676   705   717   758   604   588   513   454   399
#    42    43    44    45    46    47    48    49    50    51    52    53    54    55
#   338   315   231   195   184   136   121    91    80    45    47    28    24     8
#    56    57    58    59    60    61    63    64    65    66    70    71    72    73
#     9     6     6     1     2     4     4     1     2     3     1     1     1     1
#    74    77    84    85    87    89    96   124   127   150   155   468
#     3     3     2     1     2     1     1     1     1     1     1     1
# Does that make any sense?  Can I plot the grid with the counts shown as color
# and see if the non-zero area looks like the shape of Detroit?  The range
# 0 to 468 may compress most of the low values down to the same color and make
# zero not look much different from one.  So may need to compress the 1 to 48
# range and shift it away from zero.  Or, just tell levelplot not to show zero.
# Split the colors at something like a log scale.
library(lattice)
levelplot(grid_num_polys_per_cell,
    xlab="west-east", ylab="south-north",
    at=c(0.5, 3.5, 10.5, 30.5, 100.5, 300.5, 1000.5),
    col.regions=rainbow(7))
# Yes, this looks like Detroit.  The empty space is due to Detroit's odd shape.
# It has a long narrow projection to the south,  so there is a lot of wasted
# space.  Not sparse enough to be worth switching to a hash table, though.
# We aren't going to look at the empty cells.  Real question is, are there
# enough overpopulated cells that we need to make the grid larger?  Get the
# summary stats over the non-zero cells.
# > grid_num_polys_nonzero <- as.vector(grid_num_polys_per_cell)
# > grid_num_polys_nonzero <- grid_num_polys_nonzero[grid_num_polys_nonzero > 0]
# > length(grid_num_polys_nonzero)
# [1] 26871
# > summary(grid_num_polys_nonzero)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    1.00    8.00   24.00   22.16   33.00  468.00
# Where is the cell with 468 parcels?  Unfortunately, relational operators don't
# vectorize over matrices.
# > which_468 <- which(apply(grid_num_polys_per_cell, c(1,2), function(num) {
# +     num == 468
# + }), arr.ind=TRUE)
# > which_468
#      row col
# [1,] 135 117
# > point_468 <- c(grid_data$grid_x_min + which_468[1] * grid_data$grid_x_cellsize,
# + grid_data$grid_y_min + which_468[2] * grid_data$grid_y_cellsize)
# > point_468
# [1] -83.06511  42.36140

# Are all the parcels in there?  One parcel can be in more than one cell, so
# can't just multiply each histogram count by its corresponding column name.
# > grid_num_polys_hist_elements <- as.numeric(names(grid_num_polys_hist))
# > sum(grid_num_polys_hist_elements * grid_num_polys_hist)
# [1] 595330
# > nrow(parcel_data)
# [1] 384674
# Would have to (say) put all the parcel ids from the grid into a hash table,
# and then check if all are there.

# The grid construction and assignment of polygons to the grid has been
# repackaged into functions, in construct_polygon_grid.R.  The assignment of
# polygons has been flattened -- it no longer calls a function so the grid
# object is not repeatedly copied and garbage collected.  Try that version on
# the test grid.
source("construct_polygon_grid.R")

test_grid_data_too <- construct_polygon_grid(test_spdf, "median", 1, TRUE, TRUE)

#    user  system elapsed
#    0.12    0.02    0.14
# [1,1]: p1 , p2 , p3 , p4 ,
# [1,2]: p2 , p3 , p4 ,
# [1,3]: p4 ,
# [2,1]: p2 , p3 , p4 ,
# [2,2]: p2 , p3 , p4 ,
# [2,3]: p4 ,
# [3,1]: p4 ,
# [3,2]: p4 ,
# [3,3]: p4 ,

# And run the supposedly more efficient version on the real data.  Run this
# with mean so it can be compared with the earlier result.  Note this uses a
# lot of memory.  It's recommended at this point to save the environment, and
# write out a copy of parcel_data with all or most of the $data columns
# removed, then restart, read in just the reduced data, construct the grid,
# then write it back out.

# Save the entire environment in the current directory.
save.image(file="detroit.RData")
# Save parcel_data separately for paranoia.
saveRDS(parcel_data, "parcel_data.rds")
# Clear the environment except for functions, parcel_data, and test_spdf.
rm(list=setdiff(ls(), c(lsf.str(), "parcel_data", "test_spdf")))
gc()
# Remove columns from parcel_data$data.  Retain only parcelno, to avoid any
# potential odd behavior of SpatialPolygonsDataFrame if its @data slot is empty.
parcel_data@data[, setdiff(colnames(parcel_data@data), "parcelno")] <- list(NULL)
# Now would be a good time to shut down applications that use a lot of memory...
# source("construct_polygon_grid.R")
# grid_data_too <- construct_polygon_grid(parcel_data, "mean", 1, TRUE, FALSE)

# That ran out of memory.  Rewrote the whole thing using an environment (hash)
# to hold the grid.  Still have gc() calls in there.  This version turns out to
# be slower if gc() is run too frequently, as gc() is slow.  But it doesn't need
# the data stripped out of parcel_data, and other apps can run at the same time.
source("construct_polygon_grid.R")
grid_data_mean_hash_no_sp <- construct_polygon_grid(
    parcel_data,
    grid_scale="mean",
    grid_scale_multiplier=3,
    save_sp_polygons=FALSE,
    show_time=TRUE,
    show_progress=TRUE,
    progress_interval=100,
    verbose=FALSE,
    gc_interval=100,
    quit_row=NULL)
saveRDS(grid_data_mean_hash_no_sp, "grid_data_mean_hash_no_sp.rds")
#rm(grid_data_mean_hash_no_sp)
gc()

# Plot that.  Since the grid is a hash, extracting the count per cell is
# different.
grid_num_polys_per_cell_hash_list <- eapply(grid_data_mean_hash_no_sp$grid_polygons@.xData, length)
# That's a list with the grid keys as names.  Convert to a matrix so we can use
# levelplot.
grid_num_polys_per_cell_hash <- matrix(list(0),
                                       nrow=grid_data_mean_hash_no_sp$grid_ncell["x"],
                                       ncol=grid_data_mean_hash_no_sp$grid_ncell["y"])
for (i in seq_along(grid_num_polys_per_cell_hash_list)) {
    grid_key <- names(grid_num_polys_per_cell_hash_list[i])
    grid_idx <- grid_key_to_index(grid_key)
    num_polys <- grid_num_polys_per_cell_hash_list[[i]]
    grid_num_polys_per_cell_hash[grid_idx[1], grid_idx[2]] <- num_polys
}

# Get the histogram.
grid_num_polys_hist_hash <- table(as.vector(grid_num_polys_per_cell_hash))
# Plot the number of polygons per cell at the cell indices.
library(lattice)
levelplot(grid_num_polys_per_cell_hash,
    xlab="west-east", ylab="south-north",
    at=c(0.5, 3.5, 10.5, 30.5, 100.5, 300.5, 1000.5),
    col.regions=rainbow(7))
# rm(grid_num_polys_per_cell_hash)

# Because the grid scale multiplier was different for this run -- finer than
# for the matrix version, we don't have as many polygons per cell.  What is the
# maximum # polygons now, and where is it?
grid_max_polygons_per_cell_key_hash <- which.max(unlist(grid_num_polys_per_cell_hash_list))
# > grid_max_polygons_per_cell_key_hash
# c.179.156
#     37137
grid_max_polygons_per_cell_hash <- grid_num_polys_per_cell_hash_list[[grid_max_polygons_per_cell_key_hash]]
# > grid_max_polygons_per_cell_hash
# [1] 284
# The max # polygons per cell was 486 previously, so this is a fair drop.
# The number of elements in the hash is:
# > length(grid_data_mean_hash_no_sp$grid_polygons)
# [1] 47337
# Compare with the size of the matrix in the previous version -- the hash has
# slightly fewer elements in spite of being finer-grained.
# > grid_data$grid_ncell[1] * grid_data$grid_ncell[2]
# 49235

# At this point, we don't really need the original grid_data in memory, so
# write it out and remove it.  R won't give the space back to the OS without
# a restart, but it should be usable for other objects.
saveRDS(grid_data, "grid_data_matrix.rds")
rm(grid_data)
gc()

# That might still be too coarse a grid, since we had some cells with a lot of
# polygons.  With a hash table, we don't instantiate any cells we don't need,
# but there will still be more with a finer grid.  So memory use would be
# larger.  It's a tradeoff between memory and speed (as usual) because parcel
# membership lookups would be faster with fewer polygons per grid cell.

# Also, that run did not save the SpatialPolygons object that was made for each
# polygon, due to concern over memory.  Otherwise, the parcel lookup could be
# faster.  But we can make a set of SpatialPolygons at any time, so no need to
# re-run the grid setup.  Just run make_sp_polygons on parcel_data and
# save the result in grid_data$sp_polygons.  It is not clear that a set of
# SpatialPolygons would take up excessive space -- the memory used during runs
# of construct_polygon_grid may be high due to memory management issues, not
# the size of the finished objects.  Try making a set...
#
# start_time <- proc.time()
# grid_data$sp_polygons <- make_sp_polygons(parcel_data)
# diff_time <- proc.time() - start_time
# > gc()
#             used   (Mb) gc trigger   (Mb)  max used   (Mb)
# Ncells  62753870 3351.5   90464440 4831.4  90464440 4831.4
# Vcells 122973959  938.3  177602709 1355.1 147816630 1127.8
#
# Didn't take very long but...Task Manager shows the R process now using 5.27GB.
# Nevermind...

# Next get the parcels that each incident point lies in, for each dataset.
# start_time <- proc.time()
# calls_311_parcels <- mapply(polygons_containing_point,
#     calls_311$lng, calls_311$lat,
#     MoreArgs=list(grid_data=grid_data, polygon_data=parcel_data))
# diff_time <- proc.time() - start_time

# That's been replaced by a packaged function that does gc calls at intervals.
start_time <- proc.time()
calls_311_parcels <- polygons_containing_points(calls_311_property$lng,
                                                calls_311_property$lat,
                                                grid_data, parcel_data)
diff_time <- proc.time() - start_time
diff_time
saveRDS(calls_311_parcels, "calls_311_parcels.rds")

# calls_311_parcels <- readRDS("calls_311_parcels.rds")

# How many did we find?
calls_311_null_parcel <- sapply(calls_311_parcels, is.null)
saveRDS(calls_311_null_parcel, "calls_311_null_parcel.rds")
# > head(calls_311_null_parcel)
# [1] FALSE FALSE  TRUE FALSE  TRUE FALSE
# > sum(calls_311_null_parcel)
# [1] 3024
# > length(calls_311_null_parcel)
# [1] 11184
calls_311_fract_null_parcels <- sum(calls_311_null_parcel) / length(calls_311_null_parcel)
# > calls_311_fract_null_parcels
# [1] 0.2703863

# Check for more than one polygon enclosing an incident point -- none in this
# data -- yay!
# > calls_311_length_parcel <- sapply(calls_311_parcels, length)
# > head(calls_311_length_parcel)
# [1] 1 1 0 1 0 1
# > calls_311_multiple_parcels <- calls_311_length_parcel > 1
# > sum(calls_311_multiple_parcels)
# [1] 0

# Would like a fallback method or methods to assign incidents to parcels, to use
# only if the intersection above fails.  Options for dealing with the unassigned
# incidents:

# 1) It may be that some incident locations are actually in the street in front
# of where the incident took place.  And streets are not part of the property,
# or at least, not past any easement.  The incident might be reported by police
# fro a car stopped at the curb, or a concerned citizen standing on the
# sidewalk, capturing their own location via GPS.  If they are on the same side
# of the street as the property, then extending the front boundary outward by
# 1-2 traffic lane widths might cover these points.  If the non-street
# parcel boundaries are also extended, this should be moot (one hopes) as those
# boundaries have close to no gap between them, so incidents there will have
# been assigned to a parcel -- it should be only the street side that is outside
# of parcel polygons.  To extend the polygons...maybe move lat, lon vertex
# coordinates to the north / south or east / west, depending on whether they
# are already north / south or east / west of the polygon centroid.  This is
# likely ok, as the polygons don't have holes and aren't broken up in pieces --
# each property has a single polygon.  The polygons have a labpt field, which
# may be the centroid, or may be usable in place of the centroid, given that
# there are no holes or split polygons.

# 2) A simpler option would be to run a second matching pass using the parcel
# bounding boxes.  This has less justification, and would capture an arbitrary
# area around the property, maybe not aligned with the boundary, given that
# Detroit isn't north-south, east-west aligned.

# 3) Parse the address information in the incident and parcel files, put it in
# a standard form, do matching.

# Try 1) first.  Street lanes may range from 9-12 feet in width.  The standard
# for interstates is 12, but urban streets are more likely 10.  Start with about
# 1.5 lanes, i.e. 15 feet, to account for a parking / bicycle lane.  The vertex
# values are lat and lon degrees, not feet.  This uses the gBuffer polygon
# buffering function from GEOS / rgeos to extend the polygons, as this is a
# non-trivial operation.  gBuffer allows a single width by which to extend --
# it assumes the coordinate axes have uniform scale.  For now, rather than
# convert all polygons to a locally linear coordinate system (such as feet from
# the city center), we'll leave them in lat, lon degrees, and extend by the
# larger of the number of degrees for 15 feet in either lat or lon.  For more
# general use, especially at higher latitudes where the lat / lon coordinates
# can't reasonably be treated as linear, converting to local coordinates would
# be appropriate.  For a proof of concept, degrees are fine.

# detroit_lat_degree_feet <- 364436
# detroit_lon_degree_feet <- 270340
extend_boundary_ft <- 15
extend_boundary_lat <- extend_boundary_ft / detroit_lat_degree_feet
extend_boundary_lon <- extend_boundary_ft / detroit_lon_degree_feet
extend_width <- max(extend_boundary_lat, extend_boundary_lon)
# > extend_width
# [1] 5.548568e-05

meters_per_foot <- 0.3048
extend_boundary_meters <- extend_boundary_ft * meters_per_foot

# Altering the vertices will make new polygons.  Each one inserted in a
# SpatialPolygons object will likely make a new SpatialPolygons, or will remake
# some part of it.  I'm tempted to put them in an environment, using the
# incident file row numbers as keys.  There is an eapply for environments, if
# needed.
