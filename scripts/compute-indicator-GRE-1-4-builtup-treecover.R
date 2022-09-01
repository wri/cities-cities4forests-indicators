
library(plotly)
library(leaflet)
library(leaflet.extras)
library(plyr)
library(dplyr)
library(rgdal)
library(shinyWidgets)
library(rnaturalearth)
library(tidyverse)
library(sf)
library(rgeos)
library(httr)
library(jsonlite)
library(raster)
library(data.table)
library(DT)
library(leafem)
library(RColorBrewer)


# define aws s3 path

aws_s3_path = "https://cities-cities4forests.s3.eu-west-3.amazonaws.com/"



# read boundaries georef -----


boundary_georef = read.csv(paste(aws_s3_path,
                                 "data/boundaries/v_0/boundary_georef.csv",
                                 sep = ""),
                           fileEncoding="UTF-8-BOM")

cities = unique(boundary_georef$geo_name)

# make empty df ----

GRE_1_4_builtup_tree_cover = data.frame(geo_id = as.character(),
                                        GRE_1_4_percent_tree_cover_builtup_areas = as.numeric())


# loop on cities ----


# remove cities without tml data
cities_tml = cities[!(cities%in%c ("MEX-Monterrey","BRA-Salvador"))]

for(i in 1:length(cities_tml)){ 
  
  geo_name = cities_tml[i]
  print(geo_name)
  
  aoi_boundary_name = boundary_georef[boundary_georef$geo_name == geo_name, "aoi_boundary_name"]
  units_boundary_name = boundary_georef[boundary_georef$geo_name == geo_name, "units_boundary_name"]
  
  # Process area of interest ----
  
  # read boundaries -----
  
  print("read aoi boundaris")
  boundary_aoi = st_read(paste(aws_s3_path,
                               "data/boundaries/v_0/boundary-",
                               geo_name,
                               "-",
                               aoi_boundary_name,
                               ".geojson",
                               sep = "")
  )
  
  # read world cover ----
  
  print("read esa world cover")
  esa_worldcover_data_path = paste("https://cities-cities4forests.s3.eu-west-3.amazonaws.com/",
                                   "data/land_use/esa_world_cover/v_0/",
                                   geo_name,
                                   "-",
                                   aoi_boundary_name,
                                   "-ESA-world_cover-2020.tif",
                                   sep = "")
  
  
  # collect raster data
  esa_worldcover_data = raster(esa_worldcover_data_path)
  
  # get built-up ----
  esa_worldcover_data_builtup = esa_worldcover_data
  esa_worldcover_data_builtup[esa_worldcover_data_builtup!=50] = 0
  esa_worldcover_data_builtup[esa_worldcover_data_builtup==50] = 1
  
  esa_worldcover_data_builtup_city = raster::mask(esa_worldcover_data_builtup,boundary_aoi)
  
  # read tml ----
  
  print("read tml for")
  tml_data_path = paste("https://cities-cities4forests.s3.eu-west-3.amazonaws.com/data/tree_cover/tree_mosaic_land/v_0/",
                        geo_name,
                        "-",
                        aoi_boundary_name,
                        "-TML-tree_cover-2000.tif",
                        sep = "")
  
  # collect raster data
  tml_data = raster(tml_data_path,
                    crs = "+proj=longlat +datum=WGS84")
  
  
  tml_data_city = raster::mask(tml_data, boundary_aoi)
  
  # built up with tree cover ----
  
  print("compute indicator")
  tml_data_city_resampled = resample(tml_data_city,
                                     esa_worldcover_data_builtup_city,
                                     method= "bilinear")
  city_tml_builtup = esa_worldcover_data_builtup_city * tml_data_city_resampled
  
  avg_tree_cover_built = mean(values(city_tml_builtup), na.rm=TRUE)
  
  GRE_1_4_builtup_tree_cover_i = data.frame(geo_id= boundary_aoi$geo_id,
                                            GRE_1_4_percent_tree_cover_builtup_areas = avg_tree_cover_built)
  
  # fill indicator table ----
  
  GRE_1_4_builtup_tree_cover = GRE_1_4_builtup_tree_cover %>% 
    bind_rows(GRE_1_4_builtup_tree_cover_i)
  
  
  # Process unit of analysis ----
  
  
  print("read boundary unit")
  boundary_unit = st_read(paste(aws_s3_path,
                                "data/boundaries/v_0/boundary-",
                                geo_name,
                                "-",
                                units_boundary_name,
                                ".geojson",
                                sep = "")
  )
  
  
  for(j in 1:nrow(boundary_unit)){ 
    print(j)
    
    # j = 1
    boundary_unit_j =boundary_unit[j, ]
    print(boundary_unit_j$geo_id)
    
    print("compute indicator")
    esa_worldcover_data_builtup_city_unit = raster::mask(esa_worldcover_data_builtup,boundary_unit_j)
    tml_data_city_unit = raster::mask(tml_data, boundary_unit_j)
    
    # built up with tree cover
    tml_data_city_unit_resampled = resample(tml_data_city_unit,
                                            esa_worldcover_data_builtup_city_unit,
                                            method= "bilinear")
    city_tml_builtup_unit_j = esa_worldcover_data_builtup_city_unit * tml_data_city_unit_resampled
    
    avg_tree_cover_built = mean(values(city_tml_builtup_unit_j), na.rm=TRUE)
    
    GRE_1_4_builtup_tree_cover_i = data.frame(geo_id= boundary_unit_j$geo_id,
                                              GRE_1_4_percent_tree_cover_builtup_areas = avg_tree_cover_built)
    
    # fill indicator table ----
    
    GRE_1_4_builtup_tree_cover = GRE_1_4_builtup_tree_cover %>% 
      bind_rows(GRE_1_4_builtup_tree_cover_i)
    
  }
  
  
}

# store output ----
write.csv(GRE_1_4_builtup_tree_cover,
          "./data/indicators/GRE_1_4_builtup_tree_cover.csv")


# upload to s3 ----

file_path = paste(getwd(), "/data/indicators/GRE_1_4_builtup_tree_cover.csv", sep = "")

object_path = "data/indicators/dev/cities_indicators_GRE_1_4.csv"


put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)






