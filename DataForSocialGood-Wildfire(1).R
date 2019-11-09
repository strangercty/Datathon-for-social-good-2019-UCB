library(RSQLite)
library(dbplyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(xts)
library(ggfortify)
library(ggthemes)
library(maps)
library(mapdata)
library(leaflet)

# input the data
conn <- dbConnect(SQLite(), 'data/FPA_FOD_20170508.sqlite')
fires <- tbl(conn, "Fires") %>% collect()
print(object.size(fires), units = 'Gb')
dbDisconnect(conn)

fires$BURN_TIME <- fires$CONT_DATE - fires$DISCOVERY_DATE

# Add codes for DC and Puerto Rico to the default state lists
state.abb <- append(state.abb, c("DC", "PR"))
state.name <- append(state.name, c("District of Columbia", "Puerto Rico"))

# Map the state abbreviations to state names so we can join with the map data
fires$region <- map_chr(fires$STATE, function(x) { tolower(state.name[grep(x, state.abb)]) })

# Get the us state map data
state_map <- map_data('state')
counties <- map_data('county')
county_map <- map_data('county', 'california')

# Number of fires nationlly
fires %>% 
    select(region) %>%
    group_by(region) %>%
    summarize(n = n()) %>%
    right_join(state_map, by = 'region') %>%
    ggplot(aes(x = long, y = lat, group = group, fill = n)) + 
    geom_polygon() + 
    geom_path(color = 'white') + 
    scale_fill_continuous(low="burlywood1", high="red", guide = "colourbar",
                          name = 'Number of Fires') + 
    theme_map() + 
    coord_map('albers', lat0=30, lat1=40) + 
    ggtitle("Number of US Wildfires in Each State, 1992-2015") + 
    theme(plot.title = element_text(hjust = 0.5))

# mean burn time nationally
fires %>%
  select(region, BURN_TIME) %>%
  group_by(region) %>%
  summarize(mean_burn_time = mean(BURN_TIME, na.rm = TRUE)) %>%
  right_join(state_map, by = 'region') %>%
  ggplot(aes(x = long, y = lat, group = group, fill = mean_burn_time)) + 
  geom_polygon() + 
  geom_path(color = 'white') + 
  scale_fill_continuous(low="navajowhite1", high="firebrick2", guide = "colourbar",
                        name = 'Mean \n Burn \n Time \n (days)') + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle("Average Burn Time of US Wildfires from 1992 to 2015") + 
  theme(plot.title = element_text(hjust = 0.5))

# mean fire size nationally
fires %>%
  select(region, FIRE_SIZE) %>%
  group_by(region) %>%
  summarize(mean_fire_size = mean(FIRE_SIZE, na.rm = TRUE)) %>%
  right_join(state_map, by = 'region') %>%
  ggplot(aes(x = long, y = lat, group = group, fill = mean_fire_size)) + 
  geom_polygon() + 
  geom_path(color = 'white') + 
  scale_fill_continuous(low="lightblue2", high="deeppink2", guide = "colourbar",
                        name = 'Mean Fire Size \n (acres)') + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle("Average Fire Size of US Wildfires 1992-2015") + 
  theme(plot.title = element_text(hjust = 0.5))

# number of fires in CA
fires %>%
  filter(region == 'california') %>%
  group_by(region, subregion = tolower(FIPS_NAME)) %>%
  summarize(n = n()) %>%
  right_join(county_map, by = c('region', 'subregion')) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) + 
  geom_polygon() + 
  geom_path(color = 'white', size = 0.1) + 
  scale_fill_continuous(low="burlywood1", high="red", guide = "colourbar",
                        name = 'Number of Fires') + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle("Number of US Wildfires in California, 1992-2015") + 
  theme(plot.title = element_text(hjust = 0.5))

# mean burn time in CA
fires %>%
  filter(region == 'california') %>%
  group_by(region, subregion = tolower(FIPS_NAME)) %>%
  summarize(mean_burn_time = mean(BURN_TIME, na.rm = TRUE)) %>%
  right_join(county_map, by = c('region', 'subregion')) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = mean_burn_time)) + 
  geom_polygon() + 
  geom_path(color = 'white', size = 0.1) + 
  scale_fill_continuous(low="navajowhite1", high="firebrick2", guide = "colourbar",
                        name = 'Mean Burn Time \n (days)') + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle("Average Burn Time of CA Wildfires by County 1992-2015") + 
  theme(plot.title = element_text(hjust = 0.5))

# mean fire size in CA
fires %>%
  filter(region == 'california') %>%
  group_by(region, subregion = tolower(FIPS_NAME)) %>%
  summarize(mean_fire_size = mean(FIRE_SIZE, na.rm = TRUE)) %>%
  right_join(county_map, by = c('region', 'subregion')) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = mean_fire_size)) + 
  geom_polygon() + 
  geom_path(color = 'white', size = 0.1) + 
  scale_fill_continuous(low="lightblue2", high="deeppink2", guide = "colourbar",
                        name = 'Mean Fire Size \n (acres)') + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle("Average Fire Size of CA Wildfires by County 1992-2015") + 
  theme(plot.title = element_text(hjust = 0.5))

