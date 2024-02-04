library(sf)
library(terra)
library(spData)
library(tidyverse)

# fichier de points espacés de 10° en 10° en longitude et latitude
# longitude variant de -175° à 175°
# latitude variant de -85° à 85°

sf_points <- expand.grid(
  seq(-175, 175, by = 10),
  seq(-85, 85, by = 10)
  ) |>
  set_names("lon", "lat") |> 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")
  
world_moll <- st_transform(world, crs = "+proj=moll")
sf_points_moll <- st_transform(sf_points, crs = "+proj=moll")

ggplot(data = world_moll) + 
  geom_sf(fill = "blue", alpha = 0.1) + 
  geom_sf(data = sf_points_moll, size = 0.1)

# fonction génératrice de "carré géographique" de n degrés de côté
# le point de base (lon, lat) est le point inférieur gauche. 

polygone_geo <- function(lon, lat, n) {
  polygon_list <- list(rbind(
    c(lon, lat),
    c(lon + n, lat),
    c(lon + n, lat + n),
    c(lon, lat + n),
    c(lon, lat)
  ))
  st_polygon(polygon_list)
}

polygone_sfc <- seq(-90, 89.75, by = 0.25) |>
  map(\(x) polygone_geo(lon = 0, lat = x, n = 0.25)) |> 
  st_sfc(crs = "EPSG:4326")

ggplot(data = world_moll) + 
  geom_sf(fill = "blue", alpha = 0.1) + 
  geom_sf(data = sf_points_moll, size = 0.1) +
  geom_sf(data = st_as_sf(polygone_sfc), color = "red", alpha = 0)
