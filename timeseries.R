### chargement librairies utiles

libs <- c(
  "ncdf4",
  "tidyverse",
  "data.table",
  "jsonlite",
  "tictoc",
  "fpp3"
)
#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}
#load libraries
invisible(lapply(libs, library, character.only = TRUE))

#base functions

yyyymm <- function(date) { # transform Date '1981-06-25' to string '198106' 
  annee <- year(date)
  mois <- month(date)
  paste0(annee, if_else(mois > 9, as.character(mois), paste0("0", mois)))
}

yyyymmdd <- function(date) { # transform Date '1981-06-25' to string '19810625'
  annee <- year(date)
  mois <- month(date)
  jour <- day(date)
  paste0(
    annee,
    if_else(mois > 9, as.character(mois), paste0("0", mois)),
    if_else(jour > 9, as.character(jour), paste0("0", jour))
  )
}

# weights to apply to square of 0.25°x0.25° along the latitude
area_weights <- fread(
  file = "area_weights.csv",
  dec = ","
) |> pull( w )

# get weighted mean of one variable 
# between lat -60 and lat +60 from one NetCDF file
get_var_weighted_mean <- function(file, var, lat_min, lat_max) {
  # are we consider preliminary file
  status_ok <- if_else(str_detect(file, '_preliminary'), FALSE, TRUE)
  #--- get data from NetCDF file
  nc <- nc_open(file)
  lat <- ncvar_get(nc, varid = "lat")
  time <- ncvar_get(nc, varid = "time")
  var <- ncvar_get(nc, varid = var)
  nc_close(nc)
  #--- create the area_weights matrix
  w <- matrix(rep(area_weights, 1440), ncol = 720, byrow = TRUE)
  i_min <- min(which(lat >= lat_min))
  i_max <- max(which(lat <= lat_max))
  #--- calculate weighted mean
  wm_var_60S_60N <- weighted.mean(
    var[,i_min:i_max], 
    w[,i_min:i_max], 
    na.rm = TRUE
  )
  #--- 
  c(time, wm_var_60S_60N, status_ok)
}

# get all weighted mean for one variable for all NetCDF files available
get_var_all_weighted_mean <- function(var, lat_min, lat_max) {
  files <- list.files(path = "DATA", pattern = "*.nc", full.names = TRUE)
  if (length(files) > 0 ) {
    #--- RDS file format : weighted_mean_<var>_XXXS_XXXN.RDS
    RDS_file <- paste0(
      'weighted_mean_', var, '_', abs(lat_min), 'S_', abs(lat_max), 'N.RDS'
    )
    if (file.exists(RDS_file)) {
      #--- return valid files which mean have already been treated
      RDS_data <- readRDS(RDS_file)
      valid_files_already_treated <- RDS_data |>
        filter(status_ok) |> 
        pull(date) |> 
        yyyymmdd() |> 
        vapply(\(x) paste0('DATA/oisst-avhrr-v02r01.', x, '.nc'), character(1))
      #--- return tempo files which mean have already been treated
      tempo_files_already_treated <- RDS_data |>
        filter(!status_ok) |> 
        pull(date) |> 
        yyyymmdd() |> 
        vapply(\(x) paste0('DATA/oisst-avhrr-v02r01.', x, '_preliminary.nc'), character(1))
      files_treated <- c(valid_files_already_treated, tempo_files_already_treated)
      files_to_treat <- files[!(files %in% files_treated)]
    } else {
      files_to_treat <- files
    }
    if (length(files_to_treat) > 0) {
      #--- be aware not to consider old tempo files for later version !!!
      origin_date <- ymd("1978-01-01")
      wm_matrix <- matrix(numeric(0), nrow = length(files_to_treat), ncol = 3)
      colnames(wm_matrix) <- c('time', 'wm', 'status_ok')
      for (i in seq_along(files_to_treat)) { 
        result <- get_var_weighted_mean(files_to_treat[i], var, lat_min, lat_max)
        wm_matrix[i, 1] <- result[1] # time in days from origin date
        wm_matrix[i, 2] <- result[2] # weighted mean
        wm_matrix[i, 3] <- result[3] # status
      }
      df_wm_var <- as_tibble(wm_matrix) |>
        set_names('time', paste0('wm_', var), 'status_ok') |> 
        mutate(
          date = lubridate::as_date(time, origin = origin_date),
          status_ok = as.logical(status_ok)
        )
      if (file.exists(RDS_file)) {
        df_wm_var <- rbind(RDS_data, df_wm_var)
      }
      df_wm_var
    } else {
      if (file.exists(RDS_file)) {
        RDS_data
      }
    }
  } else { 
    message("aucun fichier de données à traiter") 
  }
}

# get weighted mean var with -60 < latitude < 60

df_wm_sst_60S_60N <- get_var_all_weighted_mean("sst", -60, 60)
saveRDS(df_wm_sst_60S_60N, 'weighted_mean_sst_60S_60N.RDS')

df_wm_anom_60S_60N <- get_var_all_weighted_mean("anom", -60, 60)
saveRDS(df_wm_anom_60S_60N, 'weighted_mean_anom_60S_60N.RDS')

df_wm_err_60S_60N <- get_var_all_weighted_mean("err", -60, 60)
saveRDS(df_wm_err_60S_60N, 'weighted_mean_err_60S_60N.RDS')

df_wm_ice_60S_60N <- get_var_all_weighted_mean("ice", -60, 60)
saveRDS(df_wm_ice_60S_60N, 'weighted_mean_ice_60S_60N.RDS')

# get weighted mean var with -60 < latitude < 60

df_wm_sst_90S_90N <- get_var_all_weighted_mean("sst", -90, 90)
saveRDS(df_wm_sst_90S_90N, 'weighted_mean_sst_90S_90N.RDS')

df_wm_anom_90S_90N <- get_var_all_weighted_mean("anom", -90, 90)
saveRDS(df_wm_anom_90S_90N, 'weighted_mean_anom_90S_90N.RDS')

df_wm_err_90S_90N <- get_var_all_weighted_mean("err", -90, 90)
saveRDS(df_wm_err_90S_90N, 'weighted_mean_err_90S_90N.RDS')

df_wm_ice_90S_90N <- get_var_all_weighted_mean("ice", -90, 90)
saveRDS(df_wm_ice_90S_90N, 'weighted_mean_ice_90S_90N.RDS')

## load official json data to compare mine with

# link <- "https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_world2_sst_day.json"
# df_wm_sst_60S_60N_json <- jsonlite::fromJSON(link)
# saveRDS(df_wm_sst_60S_60N_json, "df_wm_sst_60S_60N_json.json")
# sst_60S_60N_json <- readRDS("df_wm_sst_60S_60N_json.json") |>
#   unnest_longer(col = "data", values_to = "wm_sst", indices_to = "yday") |>
#   mutate(year = name) |> 
#   select(year, yday, wm_sst) |> 
#   filter(year %in% 1981:2024) |> # eliminate sigma and general mean data
#   mutate(year = as.integer(year)) |>
#   drop_na()
# 
# sst_60S_60N_json |> 
#   summarize(.by = year, totday = n()) -> result

sst_60S_60N <- readRDS('weighted_mean_sst_60S_60N.RDS') |> 
  mutate(
    year = year(date),
    yday = yday(date),
    month = month(date)
    ) 

# sst_join <- inner_join(
#   sst_60S_60N_json,
#   sst_60S_60N,
#   by = c("year", "yday") 
# ) |> 
#   mutate(
#     wm_sst_round = round(wm_sst.y, 2),
#     diff = wm_sst.x - wm_sst_round
#   ) 
# 
# sst_join |> 
#   filter(abs(diff) > 0) |> 
#   summarize(
#     sum_diff_neg = sum(if_else(diff < 0, diff, 0)),
#     sum_diff_pos = sum(if_else(diff > 0, diff, 0))
#   )


ggplot(data = sst_60S_60N,
       aes(x = yday, y = wm_sst, group = year)) +
  geom_line(aes(color = factor(year))) +
  scale_color_grey(start = 0.8, end = 0.2) +
  geom_line(
    data = sst_60S_60N |> filter(year == 2023),
    aes(x = yday, y = wm_sst),
    color = "orange",
    linewidth = 1) +
  geom_line(
    data = sst_60S_60N |> filter(year == 2024),
    aes(x = yday, y = wm_sst, group = year),
    color = "black", 
    linewidth = 1) +
  labs(
    title = "Daily Sea Surface Temperature, World (60°S-60°N, 0-360°E)",
    subtitle = "Dataset: NOAA OISST V2.1",
    caption = 'caption test',
    color = NULL,
    x = NULL,
    y = NULL
  ) -> p

p + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(margin = margin(t = 0, unit = 'cm')),
    legend.spacing.x = unit(0.5, 'cm'),
    legend.key = element_blank(),
    legend.text = element_text(colour = 'black'),
  ) +
  guides(
    color = guide_legend(
      ncol = 7, 
      byrow = TRUE, 
      reverse = FALSE,
      label = TRUE,
      #label.hjust = 1,
      #keywidth = unit(0.8, 'cm'),
      label.position = "right",
      x.intersp = 0.2,
      text.width = 0.045
    )
  ) 
  
ggplot(data = sst_60S_60N,
       aes(x = date, y = wm_sst)) +
  geom_line() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) + 
  geom_smooth(
    data = sst_60S_60N |> 
      filter(year > 2000),
    method = "lm",
    se = FALSE, colour = "#FFA050"
  ) +
  geom_smooth(
    data = sst_60S_60N |> 
      filter(year > 2010),
    method = "lm",
    se = FALSE, colour = "#FF0000"
  )
  
sst_90S_90N <- readRDS("weighted_mean_sst_90S_90N.RDS") |> 
  mutate(year = year(date))
ggplot(data = sst_90S_90N) +
  geom_line(aes(x = date, y = wm_sst, group = year, color = year))

# ts_sst_60S_60N <- as_tsibble(sst_60S_60N)
# autoplot(ts_sst_60S_60N, wm_sst)
# gg_season(ts_sst_60S_60N, wm_sst, period = "year")

# ts_sst_90S_90N <- as_tsibble(sst_90S_90N)
# autoplot(ts_sst_90S_90N, wm_sst)
# gg_season(ts_sst_90S_90N, wm_sst)

