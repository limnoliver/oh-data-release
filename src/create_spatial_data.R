create_spatial_data <- function(in_dat) {
  
  out_dat <- sf::st_as_sf(in_dat, 
                          coords = c('centroid_lon', 'centroid_lat'), crs = 4326,
                          remove = FALSE) %>%
    select(-group_id, -meteo_filename) %>%
    mutate(print_name = ifelse(is.na(lake_name), site_id, lake_name),
           print_name_unique = ifelse(is.na(lake_name), site_id, paste0(lake_name, ' (', site_id, ')')))
  
  return(out_dat)
}