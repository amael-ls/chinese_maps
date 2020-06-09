
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
## Read shapefiles
# Main shapefiles
ming_1389 = st_read(dsn = "~/owncloud/database/shapefiles/china/ming_boundaries_1389")
ming_1433 = st_read(dsn = "~/owncloud/database/shapefiles/china/ming_crossed_1433_kjs")
currentChina = st_read(dsn = "~/owncloud/database/shapefiles/china/admin_boundary")

## Check crs shapefiles
if (!check_crs(ming_1389, ming_1433, currentChina))
	stop("crs mismatch")

## Keep polygons
ming_1389 = st_union(st_geometry(ming_1389))
ming_1433 = st_union(st_geometry(ming_1433))
currentChina = st_union(st_geometry(currentChina))

#### Plots
## Bounding box
bbox_1389 = st_bbox(ming_1389)
bbox_1433 = st_bbox(ming_1433)
bbox_current = st_bbox(currentChina)

lonMin = min(bbox_1389["xmin"], bbox_1433["xmin"], bbox_current["xmin"])
lonMax = max(bbox_1389["xmax"], bbox_1433["xmax"], bbox_current["xmax"])
latMin = min(bbox_1389["ymin"], bbox_1433["ymin"], bbox_current["ymin"])
latMax = max(bbox_1389["ymax"], bbox_1433["ymax"], bbox_current["ymax"])

## Plot
jpeg("./MingDynasty.jpg", quality = 100, width = 1080, height = 1080)
plot(0, pch = "", xlim = c(lonMin, lonMax), ylim = c(latMin, latMax), axes = FALSE,
	xlab = "", ylab = "", bg = "transparent")
plot(ming_1433, col = "#FF9900AA", add = TRUE)
plot(currentChina, col = NA, lwd = 4, lty = "dotted", add = TRUE)
points(116.383331, y = 39.916668, pch = 15)
points(118.766670, y = 32.049999, pch = 15)
text(116.383331, y = 39.916668, labels = "Beijing", pos = 2, cex = 2)
text(118.766670, y = 32.049999, labels = "Nanjing", pos = 2, cex = 2)
legend("topleft", legend = c("Ming 1433", "Currently"), col = c("#FF9900AA", "#000000"),
	pch = c(15, NA), lwd = c(NA, 4), lty = c(NA, "dotted"), pt.cex = 6, cex = 2, box.lty = 0)
dev.off()
