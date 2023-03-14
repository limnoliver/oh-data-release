library(tidyverse)
library(ggridges)
# read in annual scenarios
scenarios <- readr::read_csv('out_data/oha_annual_scenarios.csv')
head(scenarios)

# find the average opto_hab by scenario and lake, then by scenario

byscenario <- scenarios %>%
  group_by(site_id, secchi_scenario) %>%
  summarize(annual_opti_hab = mean(annual_opti_hab),
            perc_benthic_oha = mean(perc_benthic_oha))
summary <- byscenario %>%
  group_by(secchi_scenario) %>%
  summarize(median_pbaoh = median(perc_benthic_oha, na.rm = TRUE),
            pbaoh90 = quantile(perc_benthic_oha, c(0.9), na.rm = TRUE),
            pbaoh10 = quantile(perc_benthic_oha, c(0.1), na.rm = TRUE))

pbaoh_by_scenario <- ggplot(byscenario, aes(x = secchi_scenario, y = perc_benthic_oha, group = secchi_scenario)) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 17)) +
  geom_hline(yintercept = 5, color = 'red', linetype = 'dashed') +
  labs(x = 'Secchi depth scenario (m)', y = '% Benthic Area as Optical Habitat')

ggsave('figures/pbaoh_by_scenario.png', height = 3, width = 5)


test <- filter(byscenario, secchi_scenario %in% seq(from = 0.25, to = 10, by = 0.5))
ggplot(test, aes(x = perc_benthic_oha, y =secchi_scenario, group = secchi_scenario)) +
  geom_density_ridges() +
  scale_x_continuous(limits = c(0, 7))

# merge scenarios with 2018 secchi values to calculate scenarios in terms of "change"
# need to check why some of these are NAs? one lake missing. FOllow up at some point.
y2018secchi <- readr::read_csv('out_data/oha_metrics_by_lake.csv') %>%
  select(site_id, y2018_secchi)

head(byscenario)

bychange <- left_join(ungroup(byscenario), y2018secchi) %>%
  filter(!is.na(y2018_secchi)) %>%
  mutate(change_secchi = round(secchi_scenario - y2018_secchi, 2),
         change_per = round(100*((secchi_scenario - y2018_secchi)/y2018_secchi), 1)) %>%
  mutate(change_m_bin = cut(change_secchi, breaks = seq(-9.25, 10, 0.25)),
         change_per_bin = cut(change_per, breaks = c(seq(-100, 700, 5), 4470)))
# change by m
bychange$change_m_bin_lower <- as.numeric(sub("\\((.+),.*", "\\1",bychange$change_m_bin))
bychange <- group_by(bychange, change_m_bin) %>%
  mutate(prop_change_m_bin = n()/637) %>% ungroup()
ggplot(bychange, aes(x = change_m_bin_lower, y = perc_benthic_oha, group = change_m_bin_lower)) +
  geom_boxplot(aes(fill = prop_change_m_bin), outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 15)) +
  scale_fill_viridis_c() +
  geom_vline(xintercept = 0) +
  labs(fill = 'Proportion of\nLakes Included', x = 'Secchi change from 2018 value (m)',
       y = '% Benthic Area as Optical Habitat')

# change by %
bychange$change_per_bin_lower <- as.numeric(sub("\\((.+),.*", "\\1",bychange$change_per_bin))
bychange$change_per_bin_upper <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1",bychange$change_per_bin))
bychange$change_per_bin_mid <- bychange$change_per_bin_lower + bychange$change_per_bin_upper)/2

bychange <- group_by(bychange, change_per_bin) %>%
  mutate(prop_change_per_bin = n()/637) %>% ungroup()

bychange_summ <- group_by(bychange, change_per_bin) %>%
  summarize(median = median(perc_benthic_oha))
perc_change <- ggplot(filter(bychange, change_per < 200), aes(x = change_per_bin_mid, y = perc_benthic_oha, group = change_per_bin_mid)) +
  geom_boxplot(aes(fill = prop_change_per_bin), outlier.shape = NA, width = 5) +
  #geom_vline(xintercept = 0, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = c(-28,41), linetype = 'dashed', color = 'red') +
  geom_hline(yintercept = 4.8, linetype = 'dashed') +
  scale_y_continuous(limits = c(0, 15)) +
  
  scale_fill_viridis_c() +
  labs(fill = 'Proportion of\nLakes Included', x = 'Secchi change from 2018 value (%)',
       y = '% Benthic Area as Optical Habitat')

ggsave('figures/PBAOH_by_perc_change.png', perc_change, height = 5, width = 7)  

zebra <- bychange %>%
  group_by(site_id) %>%
  slice(which.min(abs(change_per - 41))) %>%
  mutate(scenario = 'Zebra Mussel Invasion')

eutr <- bychange %>%
  group_by(site_id) %>%
  slice(which.min(abs(change_per - (-28)))) %>%
  mutate(scenario = 'Eutrophication')

current <- y2018secchi <- readr::read_csv('out_data/oha_metrics_by_lake.csv') %>%
  select(site_id, y2018_secchi, y2018_annual_perc_benthic_oha_mean) %>%
  mutate(scenario = 'Current')

scenarios <- bind_rows(select(zebra, site_id, secchi_scenario, scenario, perc_benthic_oha),
                       select(eutr, site_id, secchi_scenario, scenario, perc_benthic_oha))

scenarios <- left_join(scenarios, select(current, site_id, y2018_secchi,
                              y2018_annual_perc_benthic_oha_mean))

scenarios <- scenarios %>%
  mutate(x = ifelse(scenario %in% 'Eutrophication', 1, 2),
         xend = ifelse(scenario %in% 'Eutrophication', 2, 3),
         y = ifelse(scenario %in% 'Eutrophication', perc_benthic_oha, y2018_annual_perc_benthic_oha_mean),
         yend = ifelse(scenario %in% 'Eutrophication', y2018_annual_perc_benthic_oha_mean, perc_benthic_oha),
         trend_pp = perc_benthic_oha - y2018_annual_perc_benthic_oha_mean)
  
scenarios$x <- factor(scenarios$x, levels = c('Eutrophication', 'Current', 'Zebra Mussel Invasion'))
scenarios$xend <- factor(scenarios$xend, levels = c('Eutrophication', 'Current', 'Zebra Mussel Invasion'))

scenarios_wide <- pivot_wider(scenarios, names_from = scenario, values_from = perc_benthic_oha, id_cols = c(site_id)) %>%
  mutate(eutrophication_pp = Eutrophication - Current, 
         zebra_pp = `Zebra Mussel Invasion` - Current) 

summary(scenarios_wide$zebra_pp)
summary(scenarios_wide$eutrophication_pp)
summary(scenarios_wide$`Zebra Mussel Invasion`)
sum(scenarios_wide$Eutrophication > 5 & scenarios_wide$Current < 5)
sum(scenarios_wide$`Zebra Mussel Invasion` > 5 & scenarios_wide$Current < 5)

head(scenarios_wide)
current_v_scenarios <- ggplot(scenarios) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = trend_pp)) +
  geom_point(data = filter(scenarios, scenario %in% 'Eutrophication'), aes(x = 1, y = perc_benthic_oha), size = 0.5) +
  geom_point(data = filter(scenarios, scenario %in% 'Zebra Mussel Invasion'), aes(x = 3, y = perc_benthic_oha), size = 0.5) +
  geom_point(data = filter(scenarios, scenario %in% 'Zebra Mussel Invasion'), aes(x = 2, y = y2018_annual_perc_benthic_oha_mean), size = 0.5) +
  scale_color_distiller(type = 'div', palette = 'PuOr', guide = 'colourbar', direction = 1) +
  theme_bw() +
  labs(x = '', y= '% Benthic Area as Optical Habitat', color = 'Percentage point\nchange in PBAOH') +
  scale_x_continuous(breaks = 1:3, labels = c('Eutrophication', 'Current', 'ZM Invasion'))

ggsave('figures/lines_current_scenarios_pbaoh.png', current_v_scenarios, height = 7, width = 4)  

scenarios_vio <- bind_rows(select(scenarios, site_id, scenario, perc_benthic_oha),
                           select(scenarios, site_id, perc_benthic_oha = y2018_annual_perc_benthic_oha_mean))

scenarios_vio <- mutate(scenarios_vio, scenario = ifelse(is.na(scenario), 'Current', scenario))
     
# violin plots of scenarios   
median.quartile <- function(x){
  out <- quantile(x, probs = c(0.25,0.5,0.75))
  names(out) <- c("ymin","y","ymax")
  return(out) 
}
scenarios_vio$scenario <- factor(scenarios_vio$scenario, levels = c('Eutrophication', 'Current', 'Zebra Mussel Invasion'))
scenario_vio <- ggplot(scenarios_vio, aes(x = scenario, y = perc_benthic_oha, group = scenario)) +
  geom_violin() +
  #geom_violin(fill = 'transparent', draw_quantiles = 0.5) +
  
  stat_summary(fun=median.quartile,geom='point', shape = 95, size = 8, color = rep(c('gray', 'black', 'gray'), 3)) +
  theme_bw() +
  labs(x = '', y = '% BAOH')

ggsave('figures/scenario_violin.png', scenario_vio, height = 3, width = 5)
# drivers of response to scenarios
meta <- read.csv(I('../lake-temperature-out/1_fetch/out/lake_metadata.csv'))
drivers <- left_join(scenarios_wide, meta) %>%
  left_join(current)

drive_zm <- ggplot(drivers, aes(x = depth, y = y2018_secchi)) +
  geom_point(aes(color = zebra_pp), size = 0.5, alpha = 0.7) +
  scale_color_distiller(type = 'div', palette = 'PuOr', guide = 'colourbar', direction = 1) +
  theme_bw() +
  labs(x = 'Depth (m)', y = 'Current Secchi depth (m)', color = "PP change in\nPBAOH given\nZM invasion")
 

drive_eutr <- ggplot(drivers, aes(x = depth, y = y2018_secchi)) +
  geom_point(aes(color = eutrophication_pp), size = 0.5, alpha = 0.7) +
  scale_color_distiller(type = 'div', palette = 'PuOr', guide = 'colourbar', direction = 1) +
  theme_bw() +
  labs(x = 'Depth (m)', y = 'Current Secchi depth (m)', color = "PP change in\nPBAOH given\neutrophication")

ggsave('figures/depth_secchi_zmchange.png', drive_zm, height = 3, width = 5)
ggsave('figures/depth_secchi_eutrchange.png', drive_eutr, height = 3, width = 5)

# DEPTH vs Size and how optimal secchi depth

drivers <- left_join(drivers, select(metrics, site_id, optimal_secchi_mean, clarity_window_width_5PBOH))
drivers$secchi_to_depth <- drivers$optimal_secchi_mean/drivers$depth
depth_area_optimal <- ggplot(drivers, aes(x = area, y=depth)) +
  geom_point(aes(color = secchi_to_depth))+
  scale_x_log10() +
  scale_color_viridis_c(option = 'C', direction = 1)

ggsave('figures/depth_v_area_w_optimal.png', depth_area_optimal, height = 3, width = 5)

# relationship between window width and depth
drive_window <- ggplot(drivers, aes(x = depth, y = clarity_window_width_5PBOH)) +
  geom_point(size = 0.5, alpha = 0.7) +
  theme_bw() +
  labs(x = 'Depth (m)', y = 'Width (m) of Secchi depths that produce >5% BAOH')

ggsave('figures/depth_v_window_5PBAOH.png', drive_window, height = 4, width = 4)

ggplot(drivers, aes(x = optimal_secchi_mean, y = clarity_window_width_5PBOH)) +
  geom_point(size = 0.5, alpha = 0.7) +
  theme_bw() +
  labs(x = 'Depth (m)', y = 'Width (m) of Secchi depths that produce >5% BAOH')

ggsave('figures/depth_v_window_5PBAOH.png', drive_window, height = 4, width = 4)

###########################
# tile plot of habitat availability
tile_dat <- left_join(byscenario, select(drivers, depth, site_id, y2018_annual_perc_benthic_oha_mean))
tile_dat_summ <- tile_dat %>%
  group_by(site_id) %>%
  summarize(min_5PBAOH = min(secchi_scenario[perc_benthic_oha > 5], na.rm = TRUE),
            max_5PBAOH = max(secchi_scenario[perc_benthic_oha > 5], na.rm = TRUE),
            current_secchi = unique(y2018_secchi),
            current_PBAOH = unique(y2018_annual_perc_benthic_oha_mean))

gtop <- 'nhdhr_59746189'
g30 <- 'nhdhr_47726570'
g20 <- 'nhdhr_120018316'
g10 <- 'nhdhr_120018106'
g5 <- 'nhdhr_120018392'

text_dat <- data.frame(x = 10, y = c(g5, g10, g20, g30, gtop),
                       label = c('Lakes <5m', 'Lakes 5-10m', 'Lakes 10-20m', 'Lakes 20-30m', 'Lakes >30m'))

p <- ggplot(tile_dat, aes(x = secchi_scenario, y = reorder(site_id, depth))) +
  geom_tile(aes(fill = perc_benthic_oha), color = NA) +
  geom_hline(yintercept = c(g5, g10, g20, g30), color = 'lightgray') +
  geom_text(data = text_dat, aes(x = x, y = y, label = label), color = 'lightgray',
            hjust = 'right', vjust = 1.5) +
  scale_fill_viridis_c(option = 'B') +
  scale_x_continuous(breaks = 1:10) +
  #geom_point(data = tile_dat_summ, aes(x = current_secchi, y = site_id, color = current_PBAOH >= 5), size = 0.1) +
  #geom_segment(data = tile_dat_summ, aes(x = min_5PBAOH, xend = max_5PBAOH, y = site_id, yend = site_id), color = 'red') +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),panel.grid = element_blank()) +
  coord_cartesian(xlim = c(0.125, 10.125), expand = FALSE) +
  labs(x = "Water Clarity Scenario\n(Secchi depth, m)",
       y = 'Lakes arranged by depth\n(deep lakes on top)',
       fill = '% BAOH',
       #subtitle = 'Optical habitat availability given water clarity scenarios. Color represents the mean\npercent benthic area availabable as Walleye optical habitat across 5 model years.')
)
ggsave('figures/clarity_scenarios_tile.png', p, height = 10, width = 7)  



