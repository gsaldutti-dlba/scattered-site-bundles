#global loading

##########Load Data############

vars<- c("Parcel ID"="ParcelID")
#read in main structure data
main <- readRDS(here::here('appData/main.RDS')) %>%
  st_transform(4326) %>%
  st_cast("POLYGON")

#read in potential site data
sites <- readRDS(here::here('appData/sites.RDS')) %>%
  st_transform(4326) %>%
  st_cast("POLYGON") 

#format distance col
sites$nearest_dist <- as.numeric(sites$nearest_dist)

#read in distance matrix
mat_distances <- readRDS(here::here('appData/distance-matrix.RDS'))

#set color palette
dist_palette <- "magma"

districts <- readRDS(here::here('appData/districts.RDS')) %>% st_cast("POLYGON") %>%st_cast("LINESTRING")

lra<- readRDS(here::here('appData/LRA.RDS'))