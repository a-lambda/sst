library(lubridate)


yyyymm <- function(date) {
  annee <- year(date)
  mois <- month(date)
  paste0(annee, if_else(mois > 9, as.character(mois), paste0("0", mois)))
}


yyyymmdd <- function(date) {
  annee <- year(date)
  mois <- month(date)
  jour <- day(date)
  paste0(
    annee,
    if_else(mois > 9, as.character(mois), paste0("0", mois)),
    if_else(jour > 9, as.character(jour), paste0("0", jour))
  )
}

start_date <- ymd("1981-09-01")

seq_days <- seq(
  start_date,
  Sys.Date() - days(15),
  by = "days"
)

root_access <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr"
file_prefixe <- "oisst-avhrr-v02r01"
file_suffixe <- "nc"

tic()
seq_oisst_files <- paste(root_access, yyyymm(seq_days), file_prefixe, yyyymmdd(seq_days), file_suffixe)
toc()


oisst_file_concat <- function(date) {
  root_access <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr"
  dir <- yyyymm(date)
  file_prefixe <- "oisst-avhrr-v02r01"
  file_suffixe <- "nc"
  file <- paste(file_prefixe, yyyymmdd(date), file_suffixe, sep = ".")
  paste(root_access, dir, file, sep = "/")
}

tic()
seq_oisst_files_v1 <- vapply(seq_days, oisst_file_concat, character(1))
toc()



