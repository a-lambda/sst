mr <- st_read("DATA/mediterranean_region.gpkg")
lonlat <- expand.grid(lon, lat)
df_sst <- cbind(lonlat, as.vector(sst))
names(df_sst) <- c("lon", "lat", "sst")
sst_points <- st_as_sf(df_sst, coords = c("lon", "lat"), crs = 4326)
sst_points_only <- st_geometry(sst_points)

mr_points <- st_within(sst_points_only, mr, sparse = FALSE)
mr_points_index <- which(mr_points == TRUE)
plot(sst_points_only[mr_points_index])

length(mr_points_index)
df_sst_mediterranean_region <- df_sst[mr_points_index,]
plot(mr)
st_crs(mr)
st_crs(sst_points_only)
