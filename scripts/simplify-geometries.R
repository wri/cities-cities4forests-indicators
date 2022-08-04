################################################################################################
# This script is to simplify boundaries geometries in order to reduce the file size
################################################################################################

library(sp)
library(rgeos)
library(sf)
library(aws.s3)


# boundary-ETH-Addis_Ababa-ADM4 ---------


# read original file
geo_boundary = st_read("https://cities-cities4forests.s3.eu-west-3.amazonaws.com/data/boundaries/v_0/boundary-ETH-Addis_Ababa-ADM4.geojson")

# compute file size
object.size(geo_boundary)/1000000

# get types of polygons to verify that we don't modify polygons types because of the simplification
unique(st_geometry_type(geo_boundary, by_geometry = TRUE))

# simplify by selecting a tolerance factor (test different factors the higher the tolerance more the geometry is simplified)
geo_boundary_simplified <- st_simplify(geo_boundary, 
                                           preserveTopology = TRUE, 
                                           dTolerance = 50)

# get types of polygons to verify that we don't modify polygons types because of the simplification
unique(st_geometry_type(geo_boundary_simplified, by_geometry = TRUE))

# compute output file size
object.size(geo_boundary_simplified)/1000000

# store simplified output
st_write(geo_boundary_simplified,
         "./data/boundaries/v_0/boundary-ETH-Addis_Ababa-ADM4.geojson",
         delete_dsn = TRUE) 

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/boundary-ETH-Addis_Ababa-ADM4_simplified.geojson",
                  sep = "")

object_path = "data/boundaries/v_0/boundary-ETH-Addis_Ababa-ADM4.geojson"

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)


# boundary-ETH-Dire_Dawa-ADM3 ---------

boundary_name = "boundary-ETH-Dire_Dawa-ADM3"
# read original file
geo_boundary = st_read(paste("https://cities-cities4forests.s3.eu-west-3.amazonaws.com/data/boundaries/v_0/", 
                             boundary_name,
                             ".geojson",
                             sep = ""))

# compute file size
object.size(geo_boundary)/1000000

# get types of polygons to verify that we don't modify polygons types because of the simplification
unique(st_geometry_type(geo_boundary, by_geometry = TRUE))

# simplify by selecting a tolerance factor (test different factors the higher the tolerance more the geometry is simplified)
geo_boundary_simplified <- st_simplify(geo_boundary, 
                                       preserveTopology = TRUE, 
                                       dTolerance = 50)

# get types of polygons to verify that we don't modify polygons types because of the simplification
unique(st_geometry_type(geo_boundary_simplified, by_geometry = TRUE))

# compute output file size
object.size(geo_boundary_simplified)/1000000

# store simplified output
st_write(geo_boundary_simplified,
         paste("./data/boundaries/v_0/", 
               boundary_name,
               "_simplified.geojson",
               sep = ""),
         delete_dsn = TRUE) 

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  boundary_name,
                  "_simplified.geojson",
                  sep = "")

object_path = paste("/data/boundaries/v_0/", 
                    boundary_name,
                    ".geojson",
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-ETH-Dire_Dawa-ADM3union ---------

boundary_name = "boundary-ETH-Dire_Dawa-ADM3union"
# read original file
geo_boundary = st_read(paste("https://cities-cities4forests.s3.eu-west-3.amazonaws.com/data/boundaries/v_0/", 
                             boundary_name,
                             ".geojson",
                             sep = ""))

# compute file size
object.size(geo_boundary)/1000000

# get types of polygons to verify that we don't modify polygons types because of the simplification
unique(st_geometry_type(geo_boundary, by_geometry = TRUE))

# simplify by selecting a tolerance factor (test different factors the higher the tolerance more the geometry is simplified)
geo_boundary_simplified <- st_simplify(geo_boundary, 
                                       preserveTopology = TRUE, 
                                       dTolerance = 50)

# get types of polygons to verify that we don't modify polygons types because of the simplification
unique(st_geometry_type(geo_boundary_simplified, by_geometry = TRUE))

# compute output file size
object.size(geo_boundary_simplified)/1000000

# store simplified output
st_write(geo_boundary_simplified,
         paste("./data/boundaries/v_0/", 
               boundary_name,
               "_simplified.geojson",
               sep = ""),
         delete_dsn = TRUE) 

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  boundary_name,
                  "_simplified.geojson",
                  sep = "")

object_path = paste("/data/boundaries/v_0/", 
                    boundary_name,
                    ".geojson",
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)