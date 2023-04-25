# GGIS489 Final Project
# Alec Thompson and Emily Ho

# Choose the corn yield file
wd <- ("maize_HarvAreaYield_Geotiff/")

corn_yield_per_hectare <- raster("maize_HarvAreaYield_Geotiff/maize_YieldPerHectare.tif")

corn_N_rate <- raster("Fertilizer_maize/maize_NitrogenApplication_Rate.tif")

acres_per_hectare <- 2.47105
corn_bushels_per_ton <- 39.3679
kilo_per_lbs <- .453592
N_kg_per_ton <- 16.07127
corn_N_removal_rate_per_bushel <- .9
corn_N_removal_rate_per_hectare <- corn_N_removal_rate_per_bushel * corn_bushels_per_ton
corn_points <- rasterToPoints(corn_yield_per_hectare)
corn_removal <- N_kg_per_ton*corn_yield_per_hectare
corn_dif <- corn_N_rate - corn_removal
library(maps)
map("world", fill=TRUE, col="white", bg="lightblue", mar=c(0,0,0,0))
plot(corn_dif,zlim=c(0,500),add=TRUE)
