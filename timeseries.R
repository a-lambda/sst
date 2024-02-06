### chargement librairies utiles

libs <- c(
  "ncdf4",
  "tidyverse",
  "terra",
  "sf"
)
#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}
#load libraries
invisible(lapply(libs, library, character.only = TRUE))

### get sst data from all NOAA NetCDF files

message(length(list.files(path = "DATA")))
files <- list.files(path = "DATA", pattern = "*.nc", full.names = TRUE)

get_sst_weighted_mean <- function(sst) {
  
  
  df_area_weights <- expand.grid(lon, area_weights)
  names(df_area_weights) <- c("lon", "w")
  w <- df_area_weights$w
  df_sst_weights <- cbind(df_sst, w)
  df_sst_60S_60N <- df_sst_weights |> 
    filter(lat >= -60 & lat <= 60) |> 
    summarize(wm_sst = weighted.mean(sst, w, na.rm = TRUE)) -> 
    sst_weighted_mean
  sst_weighted_mean
}

area_weights <- read_delim(
  file = "area_weights.csv",
  col_types = c("i", "d")
) |> 
  pull( w )

tib_sst <- tibble(
  n = numeric(0),
  sst_wmean = numeric(0)
)

for (file in files[1:10]) {
  nc <- nc_open(file)
  time <- ncvar_get(nc, varid = "time")
  lon <- ncvar_get(nc, varid = "lon")
  lat <- ncvar_get(nc, varid = "lat")
  sst <- ncvar_get(nc, varid = "sst")
  expand.grid(lon, lat) |> 
    cbind(as.vector(sst)) |> 
    cbind()
  
  message(dim(sst))
  nc_close(nc)
}