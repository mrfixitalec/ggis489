# GGIS489 Final Project
# Alec Thompson and Emily Ho


crops <- c("wheat", "maize", "rice", "barley", "millet", "sorghum", "soybean", "sunflower", "potato", "cassava", "sugarcane", "sugarbeet", "oilpalm", "rapeseed", "groundnut", "cotton", "rye")


removalRate <- function(crop){
  # This is output in kg/ton for c(N,P2O5,K2O)
  # Source: http://www.ipni.net/article/IPNI-3296
  # For cassava: https://www.sciencedirect.com/science/article/abs/pii/S0378429015300642
  # Used Canola numbers for rapeseed
  # Oil Palm numbers: https://theicct.org/wp-content/uploads/2021/06/Teh_palm-residues_final.pdf

  output <- switch(
    crop,
    "wheat"=c(25,9.5,5.5),
    "maize"=c(12,6.3,4.5),
    "rice"=c(13,6.7,3.6),
    "barley"=c(21,8.3,6.7),
    "millet"=c(28,8.0,8.0),
    "sorghum"=c(13,7.8,5.4),
    "soybean"=c(55,12,20),
    "sunflower"=c(27,9.7,9.0),
    "potato"=c(3,1.5,6.5),
    "cassava"=c(4.5,.83,6.6),
    "sugarcane"=c(1,.65,1.8),
    "sugarbeet"=c(1.9,1.1,3.7),
    "oilpalm"=c(2.94,.44,3.71),
    "rapeseed"=c(32,16.0,8.0),
    "groundnut"=c(35,5.5,8.5),
    "cotton"=c(64,28,38),
    "rye"=c(25,8.2,5.5),
    -1
  )
  return (output)
}

loadLibraries <- function(){
  library(sp)
  library(raster)
  library(RColorBrewer)
  library(sf)
  library(rnaturalearth)
}

estimateLeaching <- function(yieldFile,nutrientFile,crop,NPK_choice,plotName){
  # yieldFile is the filePath for the desired crop yield file
  # nutrientFile is path for desired crop nutrient application data
  # crop is the string that needs to be passed in that matches one of the strings from the removalRate function
  # NPK choice needs to be a character, "N" for Nitrogen, "P" for Phosphorus, and "K" for Potassium
  # plotName should be a string for the plot name
  yield_hectare <- raster(yieldFile)
  nutrient_hectare <- raster(nutrientFile)
  removal_vec <- removalRate(crop)
  if (removal_vec[1] == -1){
    stop("String for 'crop' is not valid. Please choose a valid crop string.")
  }
  NPK_val <- switch (NPK_choice,
    "N" = removal_vec[1],
    "P" = removal_vec[2],
    "K" = removal_vec[3],
    -1
  )
  if (NPK_val == -1){
    stop("NPK_choice is not a valid character (N,P,or K). Please choose a valid character.")
  }
  removal_hectare <- yield_hectare * NPK_val
  nutrient_dif <- nutrient_hectare - removal_hectare

  # Set custom breaks and colors for the plot
  colors <- colorRampPalette(c("red","pink","blue","orange","yellow"))(10)

  # Create a plot with a blue background color and the sum raster layer using custom breaks and colors
  plot(nutrient_dif, col = colors, bg = "lightblue", main = plotName, zlim=c(0,(1/8)*cellStats(nutrient_dif, "max")))
  # Add country borders as a shapefile layer
  countries <- ne_countries(scale = "medium", returnclass = "sf")
  # Extract the geometry attribute from the countries object
  countries_geom <- st_geometry(countries)

  # Plot the countries with black borders
  plot(countries_geom, border = "black", add = TRUE)

}
