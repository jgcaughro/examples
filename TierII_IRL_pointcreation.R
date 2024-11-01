#Sript will accomplish generating random points for Tier II Sampling in the IRL.
#-JA

#load libraries
library(sp)
library(sf)
library(tibble)


#### load shapefiles ####
#Segment 22
seg22_poly <- read_sf(dsn = "./2023_IRL_Seg_Shapefiles", layer = "Seg22_final")
plot(seg22_poly)

#adjusting projection
st_crs(seg22_poly)
seg22_poly <- st_transform(seg22_poly, crs = 4326)
#checking
st_crs(seg22_poly)


#Segment 23
seg23_poly <- read_sf(dsn = "./2023_IRL_Seg_Shapefiles", layer = "Seg23_final")
plot(seg23_poly)

#adjusting projection
st_crs(seg23_poly)
seg22_poly <- st_transform(seg23_poly, crs = 4326)
#checking
st_crs(seg23_poly)


#Segment 24
seg24_poly <- read_sf(dsn = "./2023_IRL_Seg_Shapefiles", layer = "Seg24_final")
plot(seg24_poly)

#adjusting projection
st_crs(seg24_poly)
seg24_poly <- st_transform(seg24_poly, crs = 4326)
#checking
st_crs(seg24_poly)


#Segment 25
seg25_poly <- read_sf(dsn = "./2023_IRL_Seg_Shapefiles", layer = "Seg25_final")
plot(seg25_poly)

#adjusting projection
st_crs(seg25_poly)
seg25_poly <- st_transform(seg25_poly, crs = 4326)
#checking
st_crs(seg25_poly)


#Segment 26
seg26_poly <- read_sf(dsn = "./2023_IRL_Seg_Shapefiles", layer = "Seg26_final")
plot(seg26_poly)

#adjusting projection
st_crs(seg26_poly)
seg26_poly <- st_transform(seg26_poly, crs = 4326)
#checking
st_crs(seg26_poly)


#Segment 27
seg27_poly <- read_sf(dsn = "./2023_IRL_Seg_Shapefiles", layer = "Seg27_final")
plot(seg27_poly)

#adjusting projection
st_crs(seg27_poly)
seg27_poly <- st_transform(seg27_poly, crs = 4326)
#checking
st_crs(seg27_poly)



#plot of shapefiles 
plot(seg22_poly)
plot(seg23_poly)
plot(seg24_poly)
plot(seg25_poly)
plot(seg26_poly)
plot(seg27_poly)




#### random point generation ####

#Segment 22
#creating random points
point22 <- st_sample(seg22_poly, size = c(10,10), type = "random")
plot(point22)
plot(seg22_poly, col = "NA", add = T)

#converting to exportable data frame
point22 <- do.call(rbind, st_geometry(point22)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

#adding columns for Id and Order
Id <- seg22_poly$Id
point22$Id <- rep(Id, each = 10)
point22$order <- rep(c(1:10), times = length(Id))

#export
write.csv(point22, "./points/point22.csv", header = T)



#Segment 23
#creating random points
point23 <- st_sample(seg23_poly, size = c(10,10), type = "random")
plot(point23)
plot(seg23_poly, col = "NA", add = T)

#converting to exportable data frame
point23 <- do.call(rbind, st_geometry(point23)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

#adding columns for Id and Order
Id <- seg23_poly$Id
point23$Id <- rep(Id, each = 10)
point23$order <- rep(c(1:10), times = length(Id))

#export
write.csv(point23, "./points/point23.csv", header = T)



#Segment 24
#creating random points
point24 <- st_sample(seg24_poly, size = c(10,10), type = "random")
plot(point24)
plot(seg24_poly, col = "NA", add = T)

#converting to exportable data frame
point24 <- do.call(rbind, st_geometry(point24)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

#adding columns for Id and Order
Id <- seg24_poly$Id
point24$Id <- rep(Id, each = 10)
point24$order <- rep(c(1:10), times = length(Id))

#export
write.csv(point24, "./points/point24.csv", header = T)



#Segment 25
#creating random points
point25 <- st_sample(seg25_poly, size = c(10,10), type = "random")
plot(point25)
plot(seg25_poly, col = "NA", add = T)

#converting to exportable data frame
point25 <- do.call(rbind, st_geometry(point25)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

#adding columns for Id and Order
Id <- seg25_poly$Id
point25$Id <- rep(Id, each = 10)
point25$order <- rep(c(1:10), times = length(Id))

#export
write.csv(point25, "./points/point25.csv", header = T)




#Segment 26
#creating random points
point26 <- st_sample(seg26_poly, size = c(10,10), type = "random")
plot(point26)
plot(seg26_poly, col = "NA", add = T)

#converting to exportable data frame
point26 <- do.call(rbind, st_geometry(point26)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

#adding columns for Id and Order
Id <- seg26_poly$Id
point26$Id <- rep(Id, each = 10)
point26$order <- rep(c(1:10), times = length(Id))

#export
write.csv(point26, "./points/point26.csv", header = T)



#Segment 27
#creating random points
point27 <- st_sample(seg27_poly, size = c(10,10), type = "random")
plot(point27)
plot(seg27_poly, col = "NA", add = T)

#converting to exportable data frame
point27 <- do.call(rbind, st_geometry(point27)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

#adding columns for Id and Order
Id <- seg27_poly$Id
point27$Id <- rep(Id, each = 10)
point27$order <- rep(c(1:10), times = length(Id))

#export
write.csv(point27, "./points/point27.csv", header = T)


