library(dplyr)
library(tidyr)
library(uuid)
library(tidyverse)
library(sf)
library(aws.s3)




# boundary-BRA-Salvador-ADM4.geojson ----

file_name = "boundary-BRA-Salvador-ADM4.geojson"

geo_parent_name = "BRA-Salvador"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM4",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-BRA-Salvador-ADM4.geojson ----

file_name = "boundary-BRA-Salvador-ADM4union.geojson"

geo_parent_name = "BRA-Salvador"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM4-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-COD-Bukavu-ADM3.geojson ----

file_name = "boundary-COD-Bukavu-ADM3.geojson"

geo_parent_name = "COD-Bukavu"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = name) %>%
  add_column(geo_level = "ADM3",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-COD-Bukavu-ADM3union3.geojson ----

file_name = "boundary-COD-Bukavu-ADM3union.geojson"

geo_parent_name = "COD-Bukavu"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = name) %>%
  add_column(geo_level = "ADM3-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-COD-Uvira-ADM3.geojson ----

file_name = "boundary-COD-Uvira-ADM3.geojson"

geo_parent_name = "COD-Uvira"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM3",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-COD-Uvira-ADM3union.geojson ----

file_name = "boundary-COD-Uvira-ADM3union.geojson"

geo_parent_name = "COD-Uvira"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM3-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-COG-Brazzaville-ADM4.geojson ----

file_name = "boundary-COG-Brazzaville-ADM4.geojson"

geo_parent_name = "COG-Brazzaville"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM4",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-COG-Brazzaville-ADM4union.geojson ----

file_name = "boundary-COG-Brazzaville-ADM4union.geojson"

geo_parent_name = "COG-Brazzaville"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM4-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-COL-Barranquilla-ADM4 ----

file_name = "boundary-COL-Barranquilla-ADM4.geojson"

geo_parent_name = "COL-Barranquilla"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM4",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-COL-Barranquilla-ADM4union ----

file_name = "boundary-COL-Barranquilla-ADM4union.geojson"

geo_parent_name = "COL-Barranquilla"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM4-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-ETH-Addis_Ababa-ADM4 ----

file_name = "boundary-ETH-Addis_Ababa-ADM4.geojson"

geo_parent_name = "ETH-Addis_Ababa"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM4",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)


# boundary-ETH-Addis_Ababa-ADM4union ----

file_name = "boundary-ETH-Addis_Ababa-ADM4union.geojson"

geo_parent_name = "ETH-Addis_Ababa"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM4-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-ETH-Dire_Dawa-ADM3 ----

file_name = "boundary-ETH-Dire_Dawa-ADM3.geojson"

geo_parent_name = "ETH-Dire_Dawa"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM3",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-ETH-Dire_Dawa-ADM3union ----

file_name = "boundary-ETH-Dire_Dawa-ADM3union.geojson"

geo_parent_name = "ETH-Dire_Dawa"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM3-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)
# boundary-KEN-Nairobi-ADM3 ----

file_name = "boundary-KEN-Nairobi-ADM3.geojson"

geo_parent_name = "KEN-Nairobi"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM3",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-KEN-Nairobi-ADM3union ----

file_name = "boundary-KEN-Nairobi-ADM3union.geojson"

geo_parent_name = "KEN-Nairobi"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM3-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-MDG-Antananarivo-ADM4 ----

file_name = "boundary-MDG-Antananarivo-ADM4.geojson"

geo_parent_name = "MDG-Antananarivo"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM4",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-MDG-Antananarivo-ADM4union ----

file_name = "boundary-MDG-Antananarivo-ADM4union.geojson"

geo_parent_name = "MDG-Antananarivo"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM4-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-MEX-Mexico_City-ADM2 ----

file_name = "boundary-MEX-Mexico_City-ADM2.geojson"

geo_parent_name = "MEX-Mexico_City"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM2",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-MEX-Mexico_City-ADM2union ----

file_name = "boundary-MEX-Mexico_City-ADM2union.geojson"

geo_parent_name = "MEX-Mexico_City"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM2-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object
# boundary-MEX-Monterrey-ADM2 ----

file_name = "boundary-MEX-Monterrey-ADM2.geojson"

geo_parent_name = "MEX-Monterrey"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM2",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-MEX-Monterrey-ADM2union ----

file_name = "boundary-MEX-Monterrey-ADM2union.geojson"

geo_parent_name = "MEX-Monterrey"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM2-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-RWA-Musanze-ADM5union ----

file_name = "boundary-RWA-Musanze-ADM5union.geojson"

geo_parent_name = "RWA-Musanze"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  # rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM5-union",
             creation_date = Sys.Date(),
             geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)

# boundary-RWA-Musanze-ADM5 ----

file_name = "boundary-RWA-Musanze-ADM5.geojson"

geo_parent_name = "RWA-Musanze"

# read boundary
city_boundary <- st_read(paste("./data/boundaries/corrected/",
                               file_name,
                               sep = ""),
                         quiet = TRUE)
city_boundary

# update schemas
city_boundary_v0 = city_boundary %>% 
  rename(geo_name = shapeName) %>%
  add_column(geo_level = "ADM5",
             creation_date = Sys.Date(),
             # geo_name = geo_parent_name,
             geo_parent_name = geo_parent_name) %>% 
  mutate(geo_number = row_number()) %>%
  mutate_at("geo_name", as.character) %>%
  unite(geo_id, c("geo_parent_name","geo_level","geo_number"), sep = "_", remove = FALSE) %>%
  dplyr::select(geo_id,geo_level,geo_name,geo_parent_name,creation_date)

city_boundary_v0
plot(city_boundary_v0)

# write new boubdary
st_write(city_boundary_v0,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)



#####################################
############# merge shapes ----------


# get files names
boundaries_files_names = list.files("./data/boundaries/v_0/")
boundaries_files_names = boundaries_files_names[-length(boundaries_files_names)]

# initiate the geojson file
i = 1

file_name = boundaries_files_names[i]
boundary_city = st_read(paste("./data/boundaries/v_0/", file_name, sep = ""),
                        quiet = TRUE)
boundaries_cities = boundary_city

# loop on all files

nb_files = length(boundaries_files_names) - 1
for(i in 2:nb_files){
  
  print(i)
  
  file_name = boundaries_files_names[i]
  print(file_name)
  boundary_city = st_read(paste("./data/boundaries/v_0/", file_name, sep = ""),
                          quiet = TRUE)
  
  boundaries_cities = boundaries_cities %>% 
    bind_rows(boundary_city)
}



boundaries_cities_df = boundaries_cities %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

View(boundaries_cities_df)

file_name = "boundary-cities-cities4forests.geojson"

st_write(boundaries_cities,
         paste("./data/boundaries/v_0/", file_name, sep = ""),
         delete_dsn = TRUE)

# upload to s3
file_path = paste(getwd(),
                  "/data/boundaries/v_0/",
                  file_name,
                  sep = "")
object_path = paste("data/boundaries/v_0/",
                    file_name,
                    sep = "")

put_object(file = file_path, 
           object = object_path, 
           bucket = "cities-cities4forests",
           acl = "public-read",
           multipart = TRUE)