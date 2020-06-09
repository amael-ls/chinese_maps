
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

## Neighbourhood
# bhutan = st_read(dsn = "~/owncloud/database/shapefiles/bhutan", layer = "BTN_adm0")
# india = st_read(dsn = "~/owncloud/database/shapefiles/india", layer = "IND_adm0")
korea_north = st_read(dsn = "~/owncloud/database/shapefiles/korea_north", layer = "PRK_adm0")
korea_south = st_read(dsn = "~/owncloud/database/shapefiles/korea_south", layer = "KOR_adm0")
# laos = st_read(dsn = "~/owncloud/database/shapefiles/laos", layer = "LAO_adm0")
mongolia = st_read(dsn = "~/owncloud/database/shapefiles/mongolia", layer = "MNG_adm0")
# myanmar = st_read(dsn = "~/owncloud/database/shapefiles/myanmar", layer = "MMR_adm0")
# nepal = st_read(dsn = "~/owncloud/database/shapefiles/nepal", layer = "NPL_adm0")
# vietnam = st_read(dsn = "~/owncloud/database/shapefiles/vietnam", layer = "VNM_adm0")

## Check crs shapefiles
# if (!check_crs(ming_1389, ming_1433, currentChina, bhutan, india, korea_north, korea_south, laos, mongolia, myanmar, nepal, vietnam))
if (!check_crs(ming_1389, ming_1433, currentChina, korea_north, korea_south, mongolia))
	stop("crs mismatch")

## Keep polygons
ming_1389 = st_union(st_geometry(ming_1389))
ming_1433 = st_union(st_geometry(ming_1433))
currentChina = st_union(st_geometry(currentChina))

# bhutan = st_union(st_geometry(bhutan))
# india = st_union(st_geometry(india))
korea_north = st_union(st_geometry(korea_north))
korea_south = st_union(st_geometry(korea_south))
# laos = st_union(st_geometry(laos))
mongolia = st_union(st_geometry(mongolia))
# myanmar = st_union(st_geometry(myanmar))
# nepal = st_union(st_geometry(nepal))
# vietnam = st_union(st_geometry(vietnam))

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
jpeg("./MingDynasty.jpg", quality = 100, width = 1080, height = 768)
plot(0, pch = "", xlim = c(lonMin, lonMax), ylim = c(latMin, latMax), axes = FALSE,
	xlab = "", ylab = "", bg = "transparent")
plot(ming_1389, col = "#FF990022", add = TRUE)
plot(currentChina, col = NA, lwd = 2, lty = "dotted", add = TRUE)
plot(korea_north, col = NA, add = TRUE)
plot(korea_south, col = NA, add = TRUE)
points(116.383331, y = 39.916668, pch = 15)
points(118.766670, y = 32.049999, pch = 15)
text(116.383331, y = 39.916668, labels = "Beijing", pos = 2)
text(118.766670, y = 32.049999, labels = "Nanjing", pos = 2)
legend("topleft", legend = c("Ming 1389", "Currently"), col = c("#FF990022", "#000000"),
	pch = c(15, NA), lty = c(NA, "dotted"), pt.cex = 2, box.lty = 0)
dev.off()

jpeg("./MingDynasty2.jpg", quality = 100, width = 1080, height = 768)
plot(0, pch = "", xlim = c(lonMin, lonMax), ylim = c(latMin, latMax), axes = FALSE,
	xlab = "", ylab = "", bg = "transparent")
plot(ming_1433, col = "#3366ff22", add = TRUE)
plot(ming_1389, col = "#FF990022", add = TRUE)
plot(currentChina, col = NA, lwd = 2, lty = "dotted", add = TRUE)
plot(korea_north, col = NA, add = TRUE)
plot(korea_south, col = NA, add = TRUE)
points(116.383331, y = 39.916668, pch = 15)
points(118.766670, y = 32.049999, pch = 15)
text(116.383331, y = 39.916668, labels = "Beijing", pos = 2)
text(118.766670, y = 32.049999, labels = "Nanjing", pos = 2)
legend("topleft", legend = c("Ming 1389", "Ming 1433", "Currently"), col = c("#FF9900", "#3366FF", "#000000"),
	pch = c(15, 15, NA), lty = c(NA, NA, "dotted"), pt.cex = 2, box.lty = 0)
dev.off()
