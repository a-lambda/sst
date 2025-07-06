# This script compares NA value for all nc files
# At day 16011 exactly the same points are concerned
# So it seems interesting to store only points that have no NA values

library(ncdf4)
library(purrr)

nc_get_var_data_and_assign_to_symbol_named_like_var <- function(var, nc_file) {
  
  nc_var_data <- ncvar_get(nc = nc_file, varid = var )
  envir <- globalenv()
  assign(var, nc_var_data, envir = envir)
  
}

create_df_sst <- function(nc_file) {
  nc <- nc_open(nc_file)
  walk(c("lon", "lat", "sst"), 
       nc_get_var_data_and_assign_to_symbol_named_like_var, nc_file = nc)
  lonlat <- expand.grid(lon, lat)
  df_sst <- cbind(lonlat, as.vector(sst))
  names(df_sst) <- c("lon", "lat", "sst")
  return(df_sst)
}

get_sst_na_indices <- function(nc_file) {
  df_sst <- create_df_sst(nc_file)
  sst_na_indices <- which(is.na(df_sst$sst))
}

nc_files <- list.files(path = "DATA/RAW/", 
                       pattern = "*.nc",
                       full.names = TRUE)

identique          <- logical(0)
sst_na_indices_ref <- get_sst_na_indices(nc_files[1])

for (i in 2:(length(nc_files))) {
  
  message(paste("comparaison avec le ", i, "ème fichier"))
  sst_na_indices <- get_sst_na_indices(nc_files[i])
  identique[i] <- identical(sst_na_indices_ref, sst_na_indices)

}

