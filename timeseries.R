### chargement librairies utiles

libs <- c(
  "ncdf4",
  "tidyverse",
  "data.table",
  "jsonlite"
)
#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}
#load libraries
invisible(lapply(libs, library, character.only = TRUE))

#weigths to apply to square of 0.25°x0.25° along the latitude
area_weights <- fread(
  file = "area_weights.csv",
  dec = ","
) |> 
  pull( w )

# get weighted mean of one variable 
# between lat -60 and lat +60 from one NetCDF file
get_var_weighted_mean <- function(file, var) {
  # open Netcdf file
  nc <- nc_open(file)
  # get time and var
  time <- ncvar_get(nc, varid = "time")
  var <- ncvar_get(nc, varid = var)
  nc_close(nc)
  w <- matrix(rep(area_weights, 1440), ncol = 720, byrow = TRUE)
  wm_var_60S_60N <- weighted.mean(var[,121:600], w[,121:600], na.rm = TRUE)
  c(time, wm_var_60S_60N)
}

# get all weighted mean for one variable for all NetCDF files available
get_var_all_weighted_mean <- function(var) {
  files <- list.files(path = "DATA", pattern = "*.nc", full.names = TRUE)
  origin_date <- ymd("1978-01-01")
  wm_matrix <- matrix(numeric(0), nrow = length(files), ncol = 2)
  colnames(wm_matrix) <- c("time", "wm")
  for (i in seq_along(files)) { 
    result <- get_var_weighted_mean(files[i], "sst")
    wm_matrix[i, 1] <- result[1]
    wm_matrix[i, 2] <- result[2]
  }
  df_wm_var <- as_tibble(wm_matrix) |>
    set_names("time", paste0("wm_", var)) |> 
    mutate(date = lubridate::as_date(time, origin = origin_date))
}
  
df_wm_sst <- get_var_all_weighted_mean("sst")
saveRDS(df_wm_sst, "weighted_mean_sst.RDS")

## load official json data to compare mine with

link <- "https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_world2_sst_day.json"
result <- jsonlite::fromJSON(link)



