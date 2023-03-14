library(tidyverse)
# status figures
metrics <- readr::read_csv('out_data/oha_metrics_by_lake.csv')

# % of Benthic as OHA
poh_status <- ggplot(metrics, aes(x = y2018_annual_perc_benthic_oha_mean)) +
  geom_histogram() +
  labs(x = "'Current' Optical Habitat Area (% of benthic area)",
       y = 'Number of Lakes') +
  #geom_vline(xintercept = 5, color = 'red') +
  geom_vline(xintercept = median(metrics$y2018_annual_perc_benthic_oha_mean), 
             color = 'red', linetype = 'dashed')

# how many lakes had >5% BA as OH in 2018? Answer: 305
sum(metrics$y2018_annual_perc_benthic_oha_mean > 5)

ggsave('figures/pohba_2018_histogram.png', poh_status, height = 3, width = 5)

poh_potential <- ggplot(metrics, aes(x = max_annual_perc_benthic_oha_mean)) +
  geom_histogram() +
  labs(x = "Maximum OHA (% of benthic area)",
       y = 'Number of Lakes') +
  geom_vline(xintercept = 5, color = 'red') +
  geom_vline(xintercept = median(metrics$max_annual_perc_benthic_oha_mean), 
             color = 'red', linetype = 'dashed')

poh_pofm <- ggplot(metrics, aes(x = y2018_annual_percofmax_oh_mean)) +
  geom_histogram() +
  labs(x = "Current OHA (% of maximum OHA)",
       y = 'Number of Lakes') +
  #geom_vline(xintercept = 5, color = 'red') +
  geom_vline(xintercept = median(metrics$y2018_annual_percofmax_oh_mean), 
             color = 'red', linetype = 'dashed')
ggsave('figures/pmaxoh_2018_histogram.png', poh_pofm, height = 3, width = 5)

# how many lakes >75%, 50%, 25%
sum(metrics$y2018_annual_percofmax_oh_mean > 75)
# how does current OHA compare to maximum OHA
current_v_max <- ggplot(metrics, aes(x = max_annual_perc_benthic_oha_mean, 
                                     y = y2018_annual_perc_benthic_oha_mean)) +
  geom_point() +
  labs(x = "Maximum OHA (% of benthic area)",
       y = "'Current' OHA (% of benthic area)") +
  geom_abline(slope = 1, intercept = 0, color = 'red4') + 
  geom_abline(slope = 0.75, intercept = 0, color = 'red3') +
  geom_abline(slope = 0.50, intercept = 0, color = 'darkorange') +
  geom_abline(slope = 0.25, intercept = 0, color = 'orange') 

# potential, in terms of how far the lake can go to maximum OHA and needs to go OHA
oha_dist_vs_pot <- ggplot(metrics, aes(x = y2018_annual_percofmax_oh_mean,
                    y = y2018_dist_optimal_mean)) +
  geom_point(aes(color = y2018_annual_perc_benthic_oha_mean > 5), alpha = 0.7) +
  scale_color_manual(values = c('red', 'black')) +
  labs(x = 'Current OHA (% of max OHA)', 
       y = 'Distance to optimal clarity (Secchi, m)',
       color = 'Current OHA > 5%\nof Benthic Area')

# flow chart of lakes
# lakes w/<5% OHA
ggplot(metrics, aes(x = max_annual_perc_benthic_oha_mean, y = clarity_window_width_5PBOH)) +
  geom_point(aes(color = y2018_annual_perc_benthic_oha_mean))+
  scale_color_viridis_c(option = 'A', direction = -1) +
  labs(x = 'Maximum OHA (% benthic area)', 
       y = 'Width of Secchi values (m)\nthat achieves >5% OHA',
       color = 'Current OHA\n(% benthic area)')

# distance to max OHA and distance to 5PBAOH
dist_max <- ggplot(metrics, aes(x = y2018_dist_optimal_mean)) +
  geom_histogram() +
  labs(x = 'Clarity distance to max OH (Secchi, m)', y = 'Number of Lakes') +
  geom_vline(xintercept = median(metrics$y2018_dist_optimal_mean), 
             color = 'red', linetype = 'dashed')
sum(metrics$y2018_dist_optimal_mean >= 2)
ggsave('figures/distmaxoh_2018_histogram.png', dist_max, height = 3, width = 5)

# distance to the minimum clarity value that achieves 5PBAOH
metrics <- metrics %>%
  mutate(y2018_dist_min_clarity_5PBOH = min_clarity_5PBOH - y2018_secchi,
         y2018_dist_max_clarity_5PBOH = max_clarity_5PBOH - y2018_secchi,
         # find the closest distance to the window
         y2018_mindist_window_clarity_5PBOH = 
           ifelse(abs(y2018_dist_min_clarity_5PBOH) < abs(y2018_dist_max_clarity_5PBOH), 
                  y2018_dist_min_clarity_5PBOH, 
                  y2018_dist_max_clarity_5PBOH),
         y2018_maxdist_window_clarity_5PBOH = 
           ifelse(abs(y2018_dist_min_clarity_5PBOH) > abs(y2018_dist_max_clarity_5PBOH), 
                  y2018_dist_min_clarity_5PBOH, 
                  y2018_dist_max_clarity_5PBOH))

# for lakes with <5%, how far to get into window?
dist_5PBAOH <- ggplot(filter(metrics, y2018_annual_perc_benthic_oha_mean < 5), aes(x = y2018_dist_window_clarity_5PBOH)) +
  geom_histogram() +
  labs(x = 'Clarity distance to 5PBAOH (Secchi, m)\nfor lakes with <5% PBAOH', y = 'Number of Lakes') +
  geom_vline(xintercept = median(metrics$y2018_dist_window_clarity_5PBOH, na.rm = TRUE), 
             color = 'red', linetype = 'dashed')
ggsave('figures/dist5PBAOH_2018_histogram.png', dist_5PBAOH, height = 3, width = 5)

# for lakes with >5%, how less clear could they become before moving out of window?
dist_to_bottom_window <- ggplot(filter(metrics, y2018_annual_perc_benthic_oha_mean >=5),
                               aes(x = y2018_dist_min_clarity_5PBOH)) +
  geom_histogram() +
  labs(x = 'Distance to bottom of 5PBAOH window (Secchi, m)\nfor lakes with >5% PBAOH', y = 'Number of Lakes') +
  geom_vline(xintercept = median(metrics$y2018_dist_min_clarity_5PBOH[metrics$y2018_annual_perc_benthic_oha_mean >=5], na.rm = TRUE), 
             color = 'red', linetype = 'dashed')

dist_to_top_window <- ggplot(filter(metrics, y2018_annual_perc_benthic_oha_mean >=5),
                               aes(x = y2018_dist_max_clarity_5PBOH)) +
  geom_histogram() +
  labs(x = 'Distance to top of 5PBAOH window (Secchi, m)\nfor lakes with >5% PBAOH', y = 'Number of Lakes') +
  geom_vline(xintercept = median(metrics$y2018_dist_max_clarity_5PBOH[metrics$y2018_annual_perc_benthic_oha_mean >=5], na.rm = TRUE), 
             color = 'red', linetype = 'dashed')

ggsave('figures/distbottomwindow_2018_histogram.png', dist_to_bottom_window, height = 3, width = 5)
ggsave('figures/disttopwindow_2018_histogram.png', dist_to_top_window, height = 3, width = 5)

# optimal clarity, how that relates to lake properties
meta <- read.csv(I('../lake-temperature-out/1_fetch/out/lake_metadata.csv'))
metrics2 <- left_join(metrics, meta)
optimal_v_depth <- ggplot(metrics2, aes(y = optimal_secchi_mean, x = depth)) +
  geom_point(alpha = 0.5) +
  #geom_smooth(method = 'lm') +
  labs(x = "Lake Depth (m)", y = 'Optimal Secchi depth (m)')
ggsave('figures/opt_clarity_lakedepth.png', optimal_v_depth, height = 4, width = 4, scale = 1.1)

ggplot(metrics2, aes(y = optimal_secchi_mean, x = area)) +
  geom_point() +
  scale_x_log10() +
  labs(x = "Lake Area (m^2)", y = 'Optimal Secchi depth (m)')

# histogram of optimal water clarity
optimal <- ggplot(metrics, aes(x = optimal_secchi_mean)) +
  geom_histogram() +
  labs(x = 'Optimal water clarity (Secchi, m)', y = 'Number of Lakes') +
  geom_vline(xintercept = median(metrics$optimal_secchi_mean), 
             color = 'red', linetype = 'dashed')
ggsave('figures/optimal_secchi.png', optimal, height = 3, width = 5)

# histogram of window widths
window_width <- ggplot(metrics, aes(x = clarity_window_width_5PBOH)) +
  geom_histogram() +
  labs(x = 'Width of Secchi depths\nthat produce >5% BAOH (m)', y = 'Number of Lakes') +
  geom_vline(xintercept = median(metrics$clarity_window_width_5PBOH), 
             color = 'red', linetype = 'dashed')
ggsave('figures/window_secchi_5PBAOH.png', window_width, height = 3, width = 5)

# max PBAOH
max_PBAOH <- ggplot(metrics, aes(x = max_annual_perc_benthic_oha_mean)) +
  geom_histogram() +
  labs(x = 'Maximum PBAOH', y = 'Number of Lakes') +
  geom_vline(xintercept = median(metrics$max_annual_perc_benthic_oha_mean), 
             color = 'red', linetype = 'dashed')
sum(metrics$max_annual_perc_benthic_oha_mean < 5)
ggsave('figures/max_PBAOH.png', max_PBAOH, height = 3, width = 5)

# how much gain is possible (in BA percentage points?)
metrics$possible_pp_gain <- metrics$max_annual_perc_benthic_oha_mean - metrics$y2018_annual_perc_benthic_oha_mean
metrics2$possible_pp_gain <- metrics$max_annual_perc_benthic_oha_mean - metrics$y2018_annual_perc_benthic_oha_mean

ggplot(metrics, aes(y = possible_pp_gain, x = y2018_dist_optimal_mean)) +
  geom_point(aes(color = y2018_annual_perc_benthic_oha_mean >5 )) +
  scale_color_manual(values = c('orange', 'lightgray'))

ggplot(metrics, aes(y = possible_pp_gain, x = y2018_annual_perc_benthic_oha_mean)) +
  geom_point() +
  geom_point(data = filter(metrics2, grepl('Mille Lacs', lake_name)), aes(y = possible_pp_gain, x = y2018_annual_perc_benthic_oha_mean), color = 'red')

# how sensitive is OH in lakes to small changes in clarity?
metrics <- metrics %>%
  mutate(small_i_change = case_when(
    y2018_annual_perc_benthic_oha_mean < 5 & (y2018_annual_perc_benthic_oha_mean + y2018_slope_pp_per_interval_i_mean >5) ~ '>5% BAOH',
    y2018_annual_perc_benthic_oha_mean > 5 & (y2018_annual_perc_benthic_oha_mean + y2018_slope_pp_per_interval_i_mean <5) ~ '<5% BAOH',
    TRUE ~ 'no change'))
change_small_i <- ggplot(metrics, aes(x = y2018_annual_perc_benthic_oha_mean, y = y2018_slope_pp_per_interval_i_mean)) +
  geom_point(aes(color = small_i_change), alpha =0.7) +
  labs(x = 'Current PBAOH', y = 'Percentage point change in PBAOH given a\n0.25m increase in Secchi depth',
       color = 'Change category') 
ggsave('figures/change_small_secchi_i.png',change_small_i, height = 3, width = 5)

metrics <- metrics %>%
  mutate(small_d_change = case_when(
    y2018_annual_perc_benthic_oha_mean < 5 & (y2018_annual_perc_benthic_oha_mean + y2018_slope_pp_per_interval_d_mean >5) ~ '>5% BAOH',
    y2018_annual_perc_benthic_oha_mean > 5 & (y2018_annual_perc_benthic_oha_mean + y2018_slope_pp_per_interval_d_mean <5) ~ '<5% BAOH',
    TRUE ~ 'no change'))
change_small_i <- ggplot(metrics, aes(x = y2018_annual_perc_benthic_oha_mean, y = y2018_slope_pp_per_interval_d_mean)) +
  geom_point(aes(color = small_d_change), alpha =0.7) +
  labs(x = 'Current PBAOH', y = 'Percentage point change in PBAOH given a\n0.25m decrease in Secchi depth',
       color = 'Change category') 
ggsave('figures/change_small_secchi_d.png',change_small_i, height = 3, width = 5)

clarity_v_change <- ggplot(metrics, aes(x = y2018_secchi, y = y2018_slope_pp_per_interval_d_mean)) +
  geom_point(aes(color = small_d_change), alpha =0.7) +
  labs(x = 'Current Secchi', y = 'Percentage point change in PBAOH given a\n0.25m decrease in Secchi depth',
       color = 'Change category') 
ggsave('figures/change_v_clarity_d.png',clarity_v_change, height = 3, width = 5)

clarity_v_change_i <- ggplot(metrics, aes(x = y2018_secchi, y = y2018_slope_pp_per_interval_i_mean)) +
  geom_point(aes(color = small_i_change), alpha =0.7) +
  labs(x = 'Current Secchi', y = 'Percentage point change in PBAOH given a\n0.25m increase in Secchi depth',
       color = 'Change category') 
ggsave('figures/change_v_clarity_i.png',clarity_v_change_i, height = 3, width = 5)
