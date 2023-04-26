# import needed libraries
install.packages("sp")
library(sp)
install.packages("raster")
library(raster)

library(raster)
library(RColorBrewer)
library(sf)
library(rnaturalearth)



# list of all path files for the data files
"./Fertilizer_maize/maize_PotassiumDataQuality.tif"
"./Fertilizer_maize/maize_PotassiumApplication_Total.tif"
"./Fertilizer_maize/maize_PotassiumApplication_Rate.tif"
"./Fertilizer_maize/maize_PhosphorusDataQuality.tif"
"./Fertilizer_maize/maize_PhosphorusApplication_Total.tif"
"./Fertilizer_maize/maize_PhosphorusApplication_Rate.tif"
"./Fertilizer_maize/maize_NitrogenDataQuality.tif"
"./Fertilizer_maize/maize_NitrogenApplication_Total.tif"
"./Fertilizer_maize/maize_NitrogenApplication_Rate.tif"


"./Fertilizer_soybean/soybean_PotassiumDataQuality.tif"
"./Fertilizer_soybean/soybean_PotassiumApplication_Total.tif"
"./Fertilizer_soybean/soybean_PotassiumApplication_Rate.tif"
"./Fertilizer_soybean/soybean_PhosphorusDataQuality.tif"
"./Fertilizer_soybean/soybean_PhosphorusApplication_Total.tif"
"./Fertilizer_soybean/soybean_PhosphorusApplication_Rate.tif"
"./Fertilizer_soybean/soybean_NitrogenDataQuality.tif"
"./Fertilizer_soybean/soybean_NitrogenApplication_Total.tif"
"./Fertilizer_soybean/soybean_NitrogenApplication_Rate.tif"


"./FertilizerBalance_Geotiff/PhosphorusBalanceOnLandscape_140Crops.tif"
"./FertilizerBalance_Geotiff/NitrogenBalanceOnLandscape_140Crops.tif"


"./maize_HarvAreaYield_Geotiff/maize_HarvAreaYield_Geotiff/maize_YieldPerHectare.tif"
"./maize_HarvAreaYield_Geotiff/maize_HarvAreaYield_Geotiff/maize_Production.tif"
"./maize_HarvAreaYield_Geotiff/maize_HarvAreaYield_Geotiff/maize_HarvestedAreaHectares.tif"
"./maize_HarvAreaYield_Geotiff/maize_HarvAreaYield_Geotiff/maize_HarvestedAreaFraction.tif"
"./maize_HarvAreaYield_Geotiff/maize_HarvAreaYield_Geotiff/maize_DataQuality_Yield.tif"
"./maize_HarvAreaYield_Geotiff/maize_HarvAreaYield_Geotiff/maize_DataQuality_HarvestedArea.tif"

"./soybean_HarvAreaYield_Geotiff/soybean_HarvAreaYield_Geotiff/soybean_YieldPerHectare.tif"
"./soybean_HarvAreaYield_Geotiff/soybean_HarvAreaYield_Geotiff/soybean_Production.tif"
"./soybean_HarvAreaYield_Geotiff/soybean_HarvAreaYield_Geotiff/soybean_HarvestedAreaHectares.tif"
"./soybean_HarvAreaYield_Geotiff/soybean_HarvAreaYield_Geotiff/soybeane_HarvestedAreaFraction.tif"
"./soybean_HarvAreaYield_Geotiff/soybean_HarvAreaYield_Geotiff/soybean_DataQuality_Yield.tif"
"./soybean_HarvAreaYield_Geotiff/soybean_HarvAreaYield_Geotiff/soybean_DataQuality_HarvestedArea.tif"


# Load the raster package if not already loaded
library(raster)

get_information <- function(path) {
  raster_layer <- raster(path)

  # Get the resolution, extent, and projection of the raster
  res_info <- res(raster_layer)
  extent_info <- extent(raster_layer)
  proj_info <- projection(raster_layer)

  # Get the unit of measurement from the projection information
  # unit_info <- attr(proj_info, "unit")
  unit_info <- attr(res_info, "unit")

  # Print the resolution, extent, projection, and unit of measurement
  cat("Resolution:", res_info, "\n")
  cat("Extent:\n")
  cat("xmin:", xmin(extent_info), "\n")
  cat("ymin:", ymin(extent_info), "\n")
  cat("xmax:", xmax(extent_info), "\n")
  cat("ymax:", ymax(extent_info), "\n")
  cat("Projection: ", proj_info, "\n")
  cat("Unit of measurement:", unit_info, "\n")

  data_values <- values(raster_layer)
  # Print the first 10 values
  print(data_values)

  plot(raster_layer)
}

get_information("./Fertilizer_maize/maize_PhosphorusApplication_Rate.tif")

add_function <- function(x, y) {
  sum <- x + y
  return(sum)
}

add_rasters <- function(raster_directories, name_for_plot) {

  # Read in the first raster
  sum_raster_layer <- raster(raster_directories[[1]])

  # Iterate through the remaining rasters and add them to the sum_raster_layer
  for (i in 2:length(raster_directories)) {
    current_raster <- raster(raster_directories[[i]])
    sum_raster_layer <- overlay(sum_raster_layer, current_raster, fun = add_function)
  }

  # Plot the final sum raster layer
  # plot(sum_raster_layer)

  # Set custom breaks and colors for the plot
  breaks <- seq(cellStats(sum_raster_layer, "min"), cellStats(sum_raster_layer, "max"), length.out = 10)
  colors <- colorRampPalette(c("yellow", "red"))(length(breaks)-1)

  # Create a plot with a blue background color and the sum raster layer using custom breaks and colors
  plot(sum_raster_layer, breaks = breaks, col = colors, bg = "lightblue", main = name_for_plot)

  # Add country borders as a shapefile layer
  countries <- ne_countries(scale = "medium", returnclass = "sf")
  # Extract the geometry attribute from the countries object
  countries_geom <- st_geometry(countries)

  # Plot the countries with black borders
  plot(countries_geom, border = "black", add = TRUE)

}


maize_total_fertilizer_list <- list("./Fertilizer_maize/maize_PotassiumApplication_Total.tif",
                                    "./Fertilizer_maize/maize_PhosphorusApplication_Total.tif",
                                    "./Fertilizer_maize/maize_NitrogenApplication_Total.tif")
# add_rasters(maize_total_fertilizer_list)

soybean_total_fertilizer_list <- list("./Fertilizer_soybean/soybean_PotassiumApplication_Total.tif",
                                      "./Fertilizer_soybean/soybean_PhosphorusApplication_Total.tif",
                                      "./Fertilizer_soybean/soybean_NitrogenApplication_Total.tif")
# add_rasters(soybean_total_fertilizer_list)

maize_and_soybean_total_application_list <- c(maize_total_fertilizer_list, soybean_total_fertilizer_list)
add_rasters(maize_and_soybean_total_application_list, "Total Maize and Soybean Fertilizer Application")
