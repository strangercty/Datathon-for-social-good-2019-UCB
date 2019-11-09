library(dbplyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggfortify)
library(ggthemes)
library(maps)
library(mapdata)


fires=read.csv("wildfire.csv")
fires_CA=subset(fires,fires$STATE=="CA")

fires %>% 
  group_by(FIRE_YEAR) %>%
  summarize(n_fires = n()) %>%
  ggplot(aes(x = FIRE_YEAR, y = n_fires/1000)) + 
  geom_bar(stat = 'identity', fill = 'croal') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', size = 0.4, color = 'red') + 
  labs(x = '', y = 'Number of wildfires (thousands)', title = 'US Wildfires by Year')

size_classes <- c('A' = '0-0.25', 'B' = '0.26-9.9', 'C' = '10.0-99.9', 'D' = '100-299', 'E' = '300-999',
                  'F' = '1000-4999', 'G' = '5000+')
fires %>% 
  group_by(FIRE_SIZE_CLASS) %>%
  summarize(n = n()) %>%
  mutate(FIRE_SIZE_CLASS = size_classes[FIRE_SIZE_CLASS]) %>%
  ggplot(aes(x = FIRE_SIZE_CLASS, y= n)) +
  geom_bar(stat = 'identity', fill = 'coral') +
  labs(x = 'Fire size (acres)', y = 'Number of fires', title = 'Number of Wildfires by Size Class')

fires$BURN_TIME <- fires$CONT_DATE - fires$DISCOVERY_DATE
fires %>% 
  group_by(STAT_CAUSE_DESCR) %>%
  summarize(mean_burn_time = mean(BURN_TIME, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(STAT_CAUSE_DESCR, mean_burn_time), y = mean_burn_time)) +
  geom_bar(stat = 'identity', fill = 'orange') + 
  coord_flip() + 
  labs(x = '', y = 'Days', title = 'Average Wildfire Burn Time by Cause')

fires_CA$BURN_TIME <- fires_CA$CONT_DATE - fires_CA$DISCOVERY_DATE
fires_CA %>% 
  group_by(STAT_CAUSE_DESCR) %>%
  summarize(mean_burn_time = mean(BURN_TIME, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(STAT_CAUSE_DESCR, mean_burn_time), y = mean_burn_time)) +
  geom_bar(stat = 'identity', fill = 'firebrick2') + 
  coord_flip() + 
  labs(x = '', y = 'Days', title = 'Average Wildfire Burn Time by Cause')

