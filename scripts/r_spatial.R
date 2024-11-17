# Spatial analysis in R # Han Olff nov 2021
rm(list = ls())
# set the working directory where your GIS data are located
setwd("~/Downloads/APCE2024/apce2024gis") 

#### Restoring packages etc. #### # restore the libraries of the project renv::restore()


#### Extra step for Mac users #### # overige stappen voor mac
Sys.setenv(PROJ_LIB = "/usr/local/Cellar/proj/9.5.0/share/proj")
Sys.getenv("PROJ_LIB")
# --- andere ideeen voor het oplossen van het probleem in mac --- #Sys.setenv(PATH = paste("/opt/homebrew/bin", Sys.getenv("PATH"), sep=":"))
#system("which pkg-config")
#Sys.setenv(PKG_CONFIG_PATH = "/opt/homebrew/lib/pkgconfig")
#system("pkg-config --cflags --libs gdal proj")
#install.packages("terra", configure.args="--with-gdal-config=/opt/homebrew/opt/gdal/bin/gdal-config --with-proj-include=/opt/homebrew/opt/proj/include --with-proj-lib=/opt/homebrew/opt/proj/lib") #install.packages("tidyterra")
#### Loading libraries and colourpalettes ####
# load the different libraries
library(terra) # for working with raster data
library(tidyterra) # for adding terra objects to ggplot library(ggspatial) # for scale bars
library(sf) # for vector data objects
library(tidyverse) # ggplot, dplyr etc
library(scales)
library(viridis) 
library(nlme)
library(patchwork)
library(ggnewscale)
# for oob (out of bounds) scale library(ggnewscale) # for using multiple color fill scales in ggplot library(patchwork) # for combining multiple ggplots in one panel plot
# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/ # Base R palettes
barplot(rep(1,10), col = grey.colors(10))
mycolors <- c("red", "white", "blue")
mycolors
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround barplot(rep(1,10), col = rev(terrain.colors(10))) # rev means reverse the colour palette order library(RColorBrewer)
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG")) library(viridis)
barplot(rep(1,10), col = viridis::viridis(10))
barplot(rep(1,10), col = viridis::plasma(10))
#barplot(rep(1,10), col = viridis::heat(10)) --> heat is not a package from viridis

viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))) 
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")) 
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou2


# load the vector data for the whole ecosystem
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
                             layer="protected_areas_2022") # read protected area boundaries)
sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  

# read your study area !! check if this matches indeed the name of your area
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/studyarea.gpkg",
                       layer="my_study_area")


# load the raster data for the whole ecosystem
woodybiom<-terra::rast("/Users/semmeijer/Downloads/APCE2024/apce2024gis/Vegetation/2016_WoodyVegetation/TBA_gam_utm36s.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)
class(elevation)
plot(protected_areas)
plot(elevation)
plot(protected_areas,add=T)

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

# plot the woody biomass map that you want to predict
woody_map<-ggplot() +
  tidyterra::geom_spatraster(data=woodybiom) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77,6.55),
                       oob=squish,
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="woody biomass") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
woody_map
# first graph that is needed in your document


# make an elevation map 
elevation_map <- ggplot() + 
  tidyterra::geom_spatraster(data=elevation) + 
  scale_fill_gradientn(colours=terrain.colors(10),
                       limits=c(500, 2100), # can be found in QGIS
                       oob=squish, # everything that is outside the scale will not be commited 
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=0.5, col="red") +
  tidyterra::geom_spatvector(data=lakes, fill="royalblue3", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers, col="deepskyblue2", linewidth=0.5) +
  labs(title = "Elevation") +
  coord_sf(xlimits, ylimits, datum = sf::st_crs(32736)) + 
  theme(axis.text = element_blank(),
        axis.ticks= element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.2) 
# first graph that is needed in your document elevation_map
elevation_map
                                                                                                                 # plot the rainfall map but make the colours from light blue to dark blue 
rainfall_map <- ggplot() + tidyterra::geom_spatraster(data=rainfall) + scale_fill_gradientn(colours=rev(viridis::viridis(10)), limits=c(1000, 3000), # can be found in QGIS
  oob=squish, # everything that is outside the scale will not be commited 
  name="mm") + 
  tidyterra::geom_spatvector(data=protected_areas, fill=NA, linewidth=0.5) + 
  tidyterra::geom_spatvector(data=studyarea, fill=NA, linewidth=0.5, col="red") +
  tidyterra::geom_spatvector(data=lakes, fill="royalblue3", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers, col="deepskyblue2", linewidth=0.5) +
  labs(title = "Rainfall") +coord_sf(xlimits, ylimits, datum = sf::st_crs(32736)) + 
  theme(axis.text = element_blank(),axis.ticks= element_blank()) + 
  ggspatial::annotation_scale(location="bl", width_hint = 0.2) # first graph that is needed in your document rainfall_map combine the different maps into one composite map using the patchwork library # and save it to a high resolution png
rainfall_map

woody_map + elevation_map + rainfall_map
all_maps<-woody_map +elevation_map + rainfall_map
patchwork::plot_layout(ncol=1)
all_maps 
 ggsave("/Users/semmeijer/Documents/Ecology&Conservation/Github/spatial-r-SMeijer09/figures/all_maps.png", width = 18, height = 18, units = "cm",dpi=300)
                                                                                                                                                                                                     ############################ ### explore your study area
# For the study area, I have used a CRS of EPSG:4326, so I will reproject the woodybiom raster to match the studyarea's CRS
# Reproject woodybiom to match studyarea's CRS (EPSG:4326)
woodybiom_tf <- terra::project(woodybiom, "EPSG:4326")
elevation_tf <- terra::project(elevation, "EPSG:4326")

# Define x and y limits based on the extent of studyarea
xlimits_sa <- c(sf::st_bbox(studyarea)$xmin, sf::st_bbox(studyarea)$xmax) 
ylimits_sa <- c(sf::st_bbox(studyarea)$ymin, sf::st_bbox(studyarea)$ymax)

# set the limits of your study area 
xlimits<-sf::st_bbox(studyarea)[c(1,3)] 
ylimits<-sf::st_bbox(studyarea)[c(2,4)] 
saExt<-terra::ext(studyarea)

# crop the woody biomass to the extent of the studyarea
woodybiom_sa <- terra::crop(woodybiom_tf, studyarea)
                                                                                                              # plot the woody biomass 
woody_map_sa<-ggplot() + tidyterra::geom_spatraster(data=woodybiom_sa) + 
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77,6.55), oob=squish, name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas, fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea, fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes, fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers, col="blue",linewidth=0.5) +
  labs(title="Woody biomass in the study area") + 
  coord_sf(xlimits,ylimits,expand=F, datum = sf::st_crs(32736)) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank()) + 
  ggspatial::annotation_scale(location="bl",width_hint=0.2) 
woody_map_sa

# make maps also for the other layers that you found 
# make an elevation map for the study area 
elevation_sa<-terra::crop(elevation_tf,studyarea)
 elevation_map_sa<-ggplot() + tidyterra::geom_spatraster(data=elevation_sa) + 
   scale_fill_gradientn(colours=terrain.colors(10),limits=c(500,2100), oob=squish, name="meters") +
   tidyterra::geom_spatvector(data=protected_areas, fill=NA,linewidth=0.5) +
   tidyterra::geom_spatvector(data=studyarea, fill=NA,linewidth=0.5,col="red") +
   tidyterra::geom_spatvector(data=lakes, fill="lightblue",linewidth=0.5) +
   tidyterra::geom_spatvector(data=rivers,col="blue",linewidth=0.5) + 
   labs(title="Elevation in the study area") +
   coord_sf(xlimits,ylimits,expand=F, datum = sf::st_crs(32736)) +
   theme(axis.text = element_blank(), axis.ticks = element_blank()) +
   ggspatial::annotation_scale(location="bl",width_hint=0.2) 
 elevation_map_sa

 #check extend
# create 500 random points in our study area 
# and add them to the previous map
 
# make distance to river map
# find dist2 river in files 
dist2river_sa<-terra::rast("/Users/semmeijer/Downloads/APCE2024/apce2024gis/2022_rivers/DistanceToRiver.tif") 
dist2river_tf <- terra::project(dist2river_sa, "EPSG:4326")
                                                                                                              # crop the distance to river to the extent of the studyarea 
dist2river_sa<-terra::crop(dist2river_tf,studyarea)
# Check the extents of both the study area and the raster 
print(terra::ext(studyarea)) ####### incorrect extend
print(terra::ext(dist2river_sa))

dist2river_map_sa <-ggplot() + tidyterra::geom_spatraster(data=dist2river_sa/1000) + 
  scale_fill_gradientn(colours = pal_zissou2,limits=c(0,10), oob=squish, name="kilometers") +
  tidyterra::geom_spatvector(data = protected_areas,fill=NA, linewidth=0.7) + 
  tidyterra::geom_spatvector(data=rivers,linewidth=0.3,col="blue") +
  labs(title = "Distance to rivers") +
  coord_sf(xlim=xlimits,ylim=ylimits, # set bounding box
           datum=sf::st_crs(32736)) + # keep in original projected coordinates 
  theme(axis.text = element_blank(),axis.ticks = element_blank()) + # Remove axis coordinate labels 
  ggspatial::annotation_scale(location = "bl", # Position: bottom left
                              width_hint = 0.2) # Adjust width of the scale bar
dist2river_map_sa

# extract your the values of the different raster layers to the points
# make long format
# plot how woody cover is predicted by different variables

#now add rainfall
rainfall <- terra::rast("/Users/semmeijer/Downloads/APCE2024/apce2024gis/rainfall/CHIRPS_MeanAnnualRainfall.tif")
rainfall_30m <- rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rainfall_30m <- terra::resample(rainfall, rainfall_30m, method = "bilinear")  
rainfall_30m <- terra::project(rainfall_30m, "EPSG:4326")

rainfall_sa<-terra::crop(rainfall_30m,studyarea)

#check extent
print(terra::ext(rainfall_30m)) ######### incorrect extend
print(terra::ext(studyarea))

#plot
rainfall_map_sa <- ggplot() + tidyterra::geom_spatraster(data = rainfall_sa) + 
  scale_fill_gradientn(colours = pal_zissou1, limits = c(500, 1500), oob = squish, name = "mm") +
  tidyterra::geom_spatvector(data = protected_areas, fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, fill = NA, linewidth = 0.5, col = "red") +
  tidyterra::geom_spatvector(data = lakes, fill = "royalblue3", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "deepskyblue2", linewidth = 0.5) +
  labs(title = "Rainfall in the study area") +
  coord_sf(xlimits, ylimits, expand = F, datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)
rainfall_map_sa

#add soil CEC
soil_CEC <- terra::rast("/Users/semmeijer/Downloads/APCE2024/apce2024gis/Soil/Soil_fertility_CEC_5_15cm.tif")
soil_CEC_tf <- terra::project(soil_CEC, crs(studyarea))
#crop
soil_CEC_sa <- terra::crop(soil_CEC_tf, studyarea)
#check extend
print(terra::ext(soil_CEC))
print(terra::ext(soil_CEC_tf))
print(terra::ext(soil_CEC_sa)) ########## incorrect extend
print(terra::ext(studyarea))

#plot
soil_CEC_map_sa <- ggplot() + tidyterra::geom_spatraster(data = soil_CEC_sa) + 
  scale_fill_gradientn(colours = pal_zissou2, limits = c(100, 350), oob = squish, name = "cmol/kg") +
  tidyterra::geom_spatvector(data = protected_areas, fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, fill = NA, linewidth = 0.5, col = "red") +
  tidyterra::geom_spatvector(data = lakes, fill = "royalblue3", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "deepskyblue2", linewidth = 0.5) +
  labs(title = "Soil CEC in the study area") +
  coord_sf(xlimits, ylimits, expand = F, datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)
soil_CEC_map_sa

#add number of years burned
years_burned <- terra::rast("/Users/semmeijer/Downloads/APCE2024/apce2024gis/Fire/BurnFreq.tif")
years_burned_tf <- terra::project(years_burned, "EPSG:4326")
#crop
years_burned_sa <- terra::crop(years_burned_tf, studyarea)
#check extend
print(terra::ext(years_burned_sa)) ########## incorrect extend
print(terra::ext(studyarea))

#plot
years_burned_map_sa <- ggplot() + tidyterra::geom_spatraster(data = years_burned_sa) + 
  scale_fill_gradientn(colours = pal_zissou2, limits = c(0, 17), oob = squish, name = "years burned") +
  tidyterra::geom_spatvector(data = protected_areas, fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, fill = NA, linewidth = 0.5, col = "red") +
  tidyterra::geom_spatvector(data = lakes, fill = "royalblue3", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "deepskyblue2", linewidth = 0.5) +
  labs(title = "Number of years burned in the study area") +
  coord_sf(xlimits, ylimits, expand = F, datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)
years_burned_map_sa

#add core protected areas
r<-terra::rast("/Users/semmeijer/Downloads/APCE2024/apce2024gis/2022_protected_areas/CoreProtectedAreas.tif") 
CoreProtectedAreas <- r |> #  replace NA by 0
  is.na() |>
  terra::ifel(0,r) 
#project then crop
CoreProtectedAreas_tf <- terra::project(CoreProtectedAreas, "EPSG:4326")
CoreProtectedAreas_sa <- terra::crop(CoreProtectedAreas_tf, studyarea)
#check extent
print(terra::ext(CoreProtectedAreas_sa)) ########## incorrect extend
print(terra::ext(studyarea))

#plot
CoreProtectedAreas_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(CoreProtectedAreas_sa)) +
  scale_fill_manual(values=c("grey","lightgreen"),
                    labels=c("no","yes")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Core protected areas") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
CoreProtectedAreas_map_sa

#ADD landform hills
hills <- terra::rast("/Users/semmeijer/Downloads/APCE2024/apce2024gis/landforms/hills.tif")
hills_tf <- terra::project(hills, "EPSG:4326")
#crop
hills_sa <- terra::crop(hills_tf, studyarea)
#check extend
print(terra::ext(hills_sa)) ########## incorrect extend
print(terra::ext(studyarea))

#plot
hills_map_sa <- ggplot() +
  tidyterra::geom_spatraster(data=as.factor(hills_sa)) +
  scale_fill_manual(values=c("black","orange"),
                    labels=c("valleys\nand\nplains","hills")) +
  tidyterra::geom_spatvector(data = protected_areas, fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, fill = NA, linewidth = 0.5, col = "red") +
  tidyterra::geom_spatvector(data = lakes, fill = "royalblue3", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "deepskyblue2", linewidth = 0.5) +
  labs(title = "Hills in the study area") +
  coord_sf(xlimits, ylimits, expand = F, datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)
hills_map_sa

#Now add 250 random points to the study area
set.seed(123)
rpoints <- terra::spatSample(studyarea, size = 250, 
                             method = "random")
# plot the points
rpoints_map_sa<-ggplot() +
  tidyterra::geom_spatvector(data=rpoints, size=0.5) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="250 random points") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rpoints_map_sa

##### save all maps in 1 figure
all_maps_sa<-woody_map_sa +dist2river_map_sa + elevation_map_sa + CoreProtectedAreas_map_sa + rainfall_map_sa + 
  soil_CEC_map_sa + years_burned_map_sa + hills_map_sa +rpoints_map_sa + 
  patchwork::plot_layout(ncol=3)
all_maps_sa
ggsave("/Users/semmeijer/Documents/Ecology&Conservation/Github/spatial-r-SMeijer09/figures/all_maps_sa.png", width = 297, height = 210, units = "mm",dpi=300)
