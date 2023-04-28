# GGIS489 Final Project
# Alec Thompson and Emily Ho

loadLibraries <- function(){
  install.packages(c("sp","raster","RColorBrewer","sf","rnaturalearth","fields"))
  library(sp)
  library(raster)
  library(RColorBrewer)
  library(sf)
  library(rnaturalearth)
  library(fields)
}

crops <- c("wheat", "maize", "rice", "barley", "millet", "sorghum", "soybean", "sunflower", "potato", "cassava","sugarcane", "sugarbeet", "oilpalm", "rapeseed", "groundnut", "cotton", "rye")


# removalRate -------------------------------------------------------------
# Gets the NPK removal rate data for the specific crop input in format c(N,P,K).
# Returns -1 if crop string is invalid
# This is output in kg/ton for c(N,P2O5,K2O)
# Source: http://www.ipni.net/article/IPNI-3296
# For cassava: https://www.sciencedirect.com/science/article/abs/pii/S0378429015300642
# Used Canola numbers for rapeseed
# Oil Palm numbers: https://theicct.org/wp-content/uploads/2021/06/Teh_palm-residues_final.pdf

removalRate <- function(crop){
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

# yield_string ------------------------------------------------------------
# Function that takes in the specified crop and gets the file path for the
# yield/hectare .tif file for that crop.
# Returns: string for the file path
yield_string <- function(crop){
  yield_base_path <- "_HarvAreaYield_Geotiff"
  yield_file_end <- "_YieldPerHectare.tif"
  toRet <- paste(crop,yield_base_path,"/",crop,yield_file_end,sep="")
  return (toRet)
}


# NPK_string --------------------------------------------------------------
# Function that takes in the specified crop and nutrient and gets the file path for the
# application rate .tif file for that crop.
# Returns: string for the file path
NPK_string <- function(crop,NPK){
  nutrient_base_path <- "Fertilizer_"
  nutrient_file_end <- paste("_",NPK,"Application_Rate.tif",sep="")
  toRet <- paste(nutrient_base_path,crop,"/",crop,nutrient_file_end,sep="")
  return (toRet)
}


# plot_graph --------------------------------------------------------------
# function that takes a raster and a plot name as arguments and generates
# a plot of the raster with appropriate coloring and legend.
plot_graph <- function(ras_to_plot,plot_name){
  #plot(ras_to_plot)
  # Set custom colors for the plot
  colors <- colorRampPalette(c(brewer.pal(7,"Greens")))(7)

  # Get the mean plus three sd
  mean_3sd <- 3*cellStats(ras_to_plot,"sd")+cellStats(ras_to_plot, "mean")
  # Make the breaks according to the min, max, and mean+3sd
  breaks <- c(cellStats(ras_to_plot,"min"),seq(0, mean_3sd, length.out = 6),cellStats(ras_to_plot,"max"))
  # Plot the image with a legend containing all possible data values
  imagePlot(ras_to_plot,breaks=breaks,col=colors,useRaster=TRUE,main=plot_name)
  # Add country borders as a shapefile layer
  countries <- ne_countries(scale = "medium", returnclass = "sf")
  # Extract the geometry attribute from the countries object
  countries_geom <- st_geometry(countries)
  # Plot the countries with black borders
  plot(countries_geom, border = "black", add = TRUE)
}


# estimateLeaching --------------------------------------------------------
# Arguments: yieldFile->string where the yield/hectare file is located
#           nutrientFile->string where the nutrient application rate file is located
#           crop->string for the crop name. must match a crop in the "crops" vector
#           NPK_choice->character that denotes nutrient choice, either "N", "P", or "K"
#           plotName->string that will be the name of the outputted plot
# Use: computes the total estimated leaching for a given crop with a given nutrient
#       and outputs this plot
# Does not return anything, just plots
estimateLeaching <- function(yieldFile,nutrientFile,crop,NPK_choice,plotName){
  # Load the files in
  yield_hectare <- raster(yieldFile)
  nutrient_hectare <- raster(nutrientFile)
  # Get the removal rates for the crop and check validity
  removal_vec <- removalRate(crop)
  if (removal_vec[1] == -1){
    stop("String for 'crop' is not valid. Please choose a valid crop string.")
  }
  # Choose nutrient based on argument
  NPK_val <- switch (NPK_choice,
    "N" = removal_vec[1],
    "P" = removal_vec[2],
    "K" = removal_vec[3],
    -1
  )
  # Make sure nutrient choice was valid
  if (NPK_val == -1){
    stop("NPK_choice is not a valid character (N,P,or K). Please choose a valid character.")
  }
  # Get removal data (yield/hectare*nutrient/yield)
  removal_hectare <- yield_hectare * NPK_val
  # Get difference in applied and removed
  nutrient_dif <- nutrient_hectare - removal_hectare

  plot_graph(nutrient_dif,plotName)

}

# totalLeaching -----------------------------------------------------------
# Arguments: NPK_choice->one of c("N","P","K") to compute total estimated leaching
#           of a given nutrient for all of the possible crops.
# Use: similar to estimateLeaching, but for all crops
# Returns: nothing. Just plots the output plot

totalLeaching <- function(NPK_choice){
  # Check NPK_choice valid
  NPK_full <- switch (NPK_choice,
   "N" = "Nitrogen",
   "P" = "Phosphorus",
   "K" = "Potassium",
   -1
  )
  if (NPK_full == -1){
    stop("NPK_choice is not a valid character (N,P,or K). Please choose a valid character.")
  }
  # Base is final raster that will be plotted
  base <- NA
  for (i in 1:length(crops)){
    # Get nutrient removal rates for current crop
    removal_vec <- removalRate(crops[i])
    # Get file paths and load rasters
    crop_yield_path <- yield_string(crops[i])
    crop_nutrient_path <- NPK_string(crops[i],NPK_full)
    yield_hectare <- raster(crop_yield_path)
    nutrient_hectare <- raster(crop_nutrient_path)
    NPK_val <- switch (NPK_choice,
     "N" = removal_vec[1],
     "P" = removal_vec[2],
     "K" = removal_vec[3],
     -1
    )
    # Compute total removed nutrients
    removal_hectare <- yield_hectare * NPK_val
    # Get difference in applied and removed
    nutrient_dif <- nutrient_hectare - removal_hectare
    if(i == 1){
      base <- nutrient_dif
    } else{
      #IMPORTANT: need cover() fxn to not lose data to NAs
      temp_base <- base + nutrient_dif
      base <- cover(temp_base,base,nutrient_dif)
    }
  }
  plot_graph(base,paste("Total",NPK_choice,"leached across 17 crops"))
}

