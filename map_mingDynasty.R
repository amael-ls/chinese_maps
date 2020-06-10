
#### Aim of prog: Map the Ming dynasty (1368-1644)
## Remarks:
# 1/ All along history, the boundaries have been changing. In this program, we plot
# the boundaries of 1389 and 1433.
#
# 2/ Shapefile sources:
# 1389 (downloaded on 9th June 2020): https://worldmap.harvard.edu/data/geonode:_ming_boundaries_2hs
# 1433 (downloaded on 9th June 2020): https://worldmap.harvard.edu/data/geonode:_ming_crossed_kjs
#

#### Load packages and clear memory
library(sf)

rm(list = ls())
graphics.off()

options(max.print = 100)

#### Tool function
check_crs = function(...)
{
	shp_ls = list(...)
	n = length(shp_ls)

	for (i in 1:n)
		if (class(shp_ls) != "list")
			stop("Input argument must be a list")
	
	counter = 2

	ref_crs = st_crs(shp_ls[[1]])
	boolean = TRUE
	while ((counter <= n) & (boolean))
	{
		boolean = st_crs(shp_ls[[counter]]) == ref_crs
		counter = counter + 1
	}
	return (boolean)
}

#### Load data and keep geometry
## Main shapefiles
# Read shapefiles
ming_1389 = st_read(dsn = "~/owncloud/database/shapefiles/china/ming_boundaries_1389")
ming_1433 = st_read(dsn = "~/owncloud/database/shapefiles/china/ming_crossed_1433_kjs")
currentChina = st_read(dsn = "~/owncloud/database/shapefiles/china/admin_boundary")

# Bounding box
bbox_1389 = st_bbox(ming_1389)
bbox_1433 = st_bbox(ming_1433)
bbox_current = st_bbox(currentChina)

lonMin = min(bbox_1389["xmin"], bbox_1433["xmin"], bbox_current["xmin"])
lonMax = max(bbox_1389["xmax"], bbox_1433["xmax"], bbox_current["xmax"])
latMin = min(bbox_1389["ymin"], bbox_1433["ymin"], bbox_current["ymin"])
latMax = max(bbox_1389["ymax"], bbox_1433["ymax"], bbox_current["ymax"])

ext = st_bbox(raster::extent(lonMin - 3, lonMax + 3, latMin - 3, latMax + 3))

# Simplify
currentChina = st_simplify(x = currentChina, dTolerance = 0.05)

## Neighbourhood
# Read shapefiles
korea_north = st_read(dsn = "~/owncloud/database/shapefiles/korea_north/", layer = "PRK_adm0")
korea_south = st_read(dsn = "~/owncloud/database/shapefiles/korea_south/", layer = "KOR_adm0")
mongolia = st_read(dsn = "~/owncloud/database/shapefiles/mongolia/", layer = "MNG_adm0")
russia = st_read(dsn = "~/owncloud/database/shapefiles/russia/", layer = "gadm36_RUS_0")

# Check crs shapefiles
if (!check_crs(ming_1389, ming_1433, currentChina, korea_north, korea_south, mongolia, russia))
	stop("crs mismatch")

# Crop
korea_north = st_crop(korea_north, ext)
korea_south = st_crop(korea_south, ext)
mongolia = st_crop(mongolia, ext)
russia = st_crop(russia, ext)

# Simplify
korea_north = st_simplify(x = korea_north, dTolerance = 0.05)
korea_south = st_simplify(x = korea_south, dTolerance = 0.05)
mongolia = st_simplify(x = mongolia, dTolerance = 0.05)
russia = st_simplify(x = russia, dTolerance = 0.05)

#### Polygons and centroids
ming_1389 = st_union(st_geometry(ming_1389))
ming_1433 = st_union(st_geometry(ming_1433))
currentChina = st_union(st_geometry(currentChina))

korea_north = st_union(st_geometry(korea_north))
korea_south = st_union(st_geometry(korea_south))
mongolia = st_union(st_geometry(mongolia))
russia = st_union(st_geometry(russia))

currentChina_centro = st_coordinates(st_centroid(currentChina))
korea_north_centro = st_coordinates(st_centroid(korea_north))
korea_south_centro = st_coordinates(st_centroid(korea_south))
mongolia_centro = st_coordinates(st_centroid(mongolia))
russia_centro = st_coordinates(st_centroid(russia))

#### Plots
jpeg("./MingDynasty.jpg", quality = 100, width = 1080, height = 1080)
plot(0, pch = "", xlim = c(lonMin, lonMax), ylim = c(latMin, latMax), axes = FALSE,
	xlab = "", ylab = "", bg = "transparent")
## Borderlines
plot(ming_1433, col = "#FF990044", border = 0, add = TRUE)
plot(currentChina, col = NA, lwd = 4, add = TRUE)
plot(korea_north, col = NA, lwd = 1, add = TRUE)
plot(korea_south, col = NA, lwd = 1, add = TRUE)
plot(mongolia, col = NA, lwd = 1, add = TRUE)
plot(russia, col = NA, lwd = 1, add = TRUE)

## Text
points(116.383331, y = 39.916668, pch = 15)
points(118.766670, y = 32.049999, pch = 15)
text(116.383331, y = 39.916668, labels = "Beijing", pos = 2, cex = 2)
text(118.766670, y = 32.049999, labels = "Nanjing", pos = 2, cex = 2)
text(currentChina_centro[, "X"], currentChina_centro[, "Y"], labels = "China", cex = 2)
text(korea_north_centro[, "X"], korea_north_centro[, "Y"], labels = "N. Korea", cex = 2, pos = 4, offset = 2)
text(korea_south_centro[, "X"], korea_south_centro[, "Y"], labels = "S. Korea", cex = 2, pos = 4, offset = 2)
text(mongolia_centro[, "X"], mongolia_centro[, "Y"], labels = "Mongolia", cex = 2)
text(russia_centro[, "X"], russia_centro[, "Y"], labels = "Russia", cex = 2)
legend("topleft", legend = c("Ming 1433", "Current borders"), col = c("#FF990044", "#000000"),
	pch = c(15, NA), lwd = c(NA, 4), lty = c(NA, "solid"), pt.cex = 6, cex = 2, box.lty = 0)
dev.off()

pdf("./MingDynasty2.pdf", width = 12, height = 12)
plot(0, pch = "", xlim = c(lonMin, lonMax), ylim = c(latMin, latMax), axes = FALSE,
	xlab = "", ylab = "", bg = "transparent")
## Borderlines
plot(ming_1433, col = "#FF990044", border = 0, add = TRUE)
plot(currentChina, col = NA, lwd = 4, add = TRUE)
plot(korea_north, col = NA, lwd = 1, add = TRUE)
plot(korea_south, col = NA, lwd = 1, add = TRUE)
plot(mongolia, col = NA, lwd = 1, add = TRUE)
plot(russia, col = NA, lwd = 1, add = TRUE)

## Text
points(116.383331, y = 39.916668, pch = 15)
points(118.766670, y = 32.049999, pch = 15)
text(116.383331, y = 39.916668, labels = "Beijing", pos = 2, cex = 2)
text(118.766670, y = 32.049999, labels = "Nanjing", pos = 2, cex = 2)
text(currentChina_centro[, "X"], currentChina_centro[, "Y"], labels = "China", cex = 2)
text(korea_north_centro[, "X"], korea_north_centro[, "Y"], labels = "N. Korea", cex = 2, pos = 4, offset = 2)
text(korea_south_centro[, "X"], korea_south_centro[, "Y"], labels = "S. Korea", cex = 2, pos = 4, offset = 2)
text(mongolia_centro[, "X"], mongolia_centro[, "Y"], labels = "Mongolia", cex = 2)
text(russia_centro[, "X"], russia_centro[, "Y"], labels = "Russia", cex = 2)
legend("topleft", legend = c("Ming 1433", "Current borders"), col = c("#FF990044", "#000000"),
	pch = c(15, NA), lwd = c(NA, 4), lty = c(NA, "solid"), pt.cex = 6, cex = 2, box.lty = 0)
dev.off()
