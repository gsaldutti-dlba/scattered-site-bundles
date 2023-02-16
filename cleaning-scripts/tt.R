library(sf)
library(dplyr)
source(here::here('R/sfimport.R'))


####Read data####
geo_p <- readRDS(here::here('data/parcels.RDS')) #parcels
sf_main <- sfimport(here::here('data/main.csv')) #main structures
sf_sites <- sfimport(here::here('data/sites.csv'))

#properties with > 1 attempts to sell
#sf_attempts_1 <- sfimport(here::here('data/sf_attempts-to-sell-1.csv'))
#properties with 0 attempts
#sf_attempts_0 <- sfimport(here::here('data/sf_attempts-to-sell-0.csv'))
#properties with 0 inqs
#sf_inqueries <- sfimport(here::here('data/sf_prop-inqs.csv'))

# #grop and create inqury count column for inquery df
# sf_inqueries <- sf_inqueries %>% 
#   group_by(RelatedPropertyParcelID) %>%
#   mutate(total_inqueries= n()) %>%
#   ungroup() %>%
#   distinct(RelatedPropertyParcelID, .keep_all=T)
# 
# #### Clean SF data ####
# 
sf_main$Neighborhood <- NA
sf_main$District <- NA
sf_sites$District <- NA
#create list of site dfs
sf_list <- list(sf_main, sf_sites)


names(sf_list) <- c("sf_main","sf_sites")

#function for selecting cols and joining parcel geoms 
get_cols <- function(x, geo_p) {
  
  #select columns based on strings- done to standardize data
  x <- x[,grepl("ParcelID|AccountN|Address|Program|District|Neighborhood|PropertyClass", 
                colnames(x))]
  
  #create list of columns to search for and rename
  y <- c("ParcelID", "AccountN", "Address", "Program","District", "Neighborhood", "PropertyClass")
  
  ci <- c()
  #get index of colname that matches
  for(i in 1:length(y)) {
  j <- grep(y[i],
             colnames(x))#grep colnames based on y list 
  ci <- c(ci, j) #return vector of indices
  }
  
  x<-x[, ci] #reorder columns based on index 
  
 #rename cols for later binding
  colnames(x) <- y[-2]
  
  #rename parcel id cols
  colnames(x)[1] <- "ParcelID" #rename parcel id col for joining
  x$index <- 1:nrow(x) #create index row for rejoining data 
  
  #join data to parcel for geoms
  x<- merge(x, geo_p[,'parcel_number'], by.x="ParcelID", by.y="parcel_number")
  x <- st_as_sf(x) %>% st_transform(2898)#transform crs
  
} 
#apply function to return list of dfs
sf_list <- lapply(sf_list, get_cols, geo_p) #return list of dfs with parcel geoms


###create single df for sites

#bind rows from list of dfs 
#sf_df <- bind_rows(sf_list, .id = "column_label")


# sf_sites <- sf_sites[,grepl("ParcelID|AccountN|Address|Program|District|Neighborhood|PropertyClass", 
#                           colnames(sf_sites))]
# 
# 
# sf_main <- sf_main[,grepl("ParcelID|AccountN|Address|Program|District|Neighborhood|PropertyClass", 
#                    colnames(sf_main))]

### join main structures parcel geoms and transform
sf_main <- sf_list[[1]]
#sf_main <- sf_main %>% left_join(geo_p, by =c("ParcelID"="parcel_number"))
sf_main <- sf_main %>% st_as_sf() %>% st_transform(2898) %>% st_centroid() #transform


#sf_sites <- sf_sites %>% left_join(geo_p, by = c('PropertyParcelID'='parcel_number'))
sf_sites <- sf_list[[2]]
sf_sites <- sf_sites %>% st_as_sf() %>% st_transform(2898) %>% st_centroid()

#remove duplicate sites from those in main
sf_sites <-  sf_sites %>% filter(!sf_sites$ParcelID %in% sf_main$ParcelID)

#### Spatial Operations ####
nearest_feature <- st_nearest_feature(sf_sites, sf_main) #get nearest feature for each site

#get distance of all sites to structures
#creates matrix for filtering within app
dist2 <- st_distance(sf_sites, sf_main) 
dist2 <- as.matrix(dist2)

#get distance to nearest feature for each site in relation to main structure
nearest_dist <- st_distance(sf_sites, sf_main[nearest_feature,],by_element=T)

#bind distance to nearest feature to dataframe
sf_df <- cbind(sf_sites, nearest_dist)

#get index of nearest feature for each site
sf_df <- cbind(sf_df, nearest_feature)

#generate index for structures to match 
sf_main$index <- 1:nrow(sf_main)

#join dfs by index 
sf_df <- sf_df %>% 
  rename(ParcelID_site = "ParcelID") %>%
  left_join(st_drop_geometry(sf_main[,c("index","ParcelID")]), 
                             by = c("nearest_feature"='index')) 

sf_df <- rename(sf_df, ParcelID_structure = "ParcelID")



#set up quantiles for adding quant col 
# classes <- 5
# quantiles <- sf_df %>%
#   pull(nearest_dist) %>%
#   quantile() %>%
#   as.vector()
  

# here create custom labels
# labels <- purrr::imap_chr(quantiles, function(., idx){
#   return(paste0(round(quantiles[idx], 0),
#                 "ft",
#                 " – ",
#                 round(quantiles[idx + 1], 0),
#          "ft"))
# })
# 
# labels <- labels[1:length(labels) - 1]


# sf_df <- sf_df %>%  mutate(quantiles = cut(nearest_dist,
#                               breaks = quantiles,
#                               labels = labels,
#                               include.lowest = T))


sf_df <- st_drop_geometry(sf_df)
sf_df <- sf_df %>% left_join(geo_p[,"parcel_number"], by = c("ParcelID_site"="parcel_number"))

sf_df <- sf_df %>% st_as_sf()

sf_main <- st_drop_geometry(sf_main)
sf_main <- sf_main %>%
  left_join(geo_p[,"parcel_number"], by=c("ParcelID"="parcel_number"))

sf_main <- sf_main %>% st_as_sf()

saveRDS(dist2, here::here('appData/distance-matrix.RDS'))
saveRDS(sf_df,here::here('appData/sites.RDS'))
saveRDS(sf_main, here::here("appData/main.RDS"))


