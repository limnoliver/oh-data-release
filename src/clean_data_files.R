munge_annual <- function(in_file, out_file) {
  dat <- readr::read_csv(in_file)

  
  dat_out <- select(dat, -total_benthic_area, -lake_name, -print_name, -print_name_unique)
  
  readr::write_csv(dat_out, out_file)
}

munge_metrics <- function(in_file, out_file) {
  dat <- readr::read_csv(in_file)
  names(dat) <- gsub('current', 'y2018', names(dat))
  names(dat) <- gsub('perc_oh', 'perc_benthic_oha', names(dat))
  dat_out <- select(dat, -starts_with('total_benthic')) %>%
    select(-y2018_secchi_min, -y2018_secchi_max) %>%
    rename(y2018_secchi = y2018_secchi_mean) %>%
    select(-lake_area_km2, -print_name, -print_name_unique)
    
  
  readr::write_csv(dat_out, out_file)
  
}