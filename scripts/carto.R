library(sf)
library(tigris)
library(tidyverse)
library(ggmap)
library(terra)

setwd('~/Documents/Analytical_Toolkit_SDM/scripts')
source('functions.R')

co <- c(left = -111, bottom = 34, right = -102, top = 44)
co_map <- get_stamenmap(co, zoom = 8, maptype = "terrain-background")
co_map <- ggmap_bbox(co_map)

states <- tigris::states()
places <- tigris::places(state = c('CO', 'NM', 'WY')) %>% 
  filter(NAME %in% c('Denver', 'Albuquerque', 'Rawlins', 'Grand Junction',
                     'Durango', 'Laramie', 'Lander'))

ecoregions <- sf::read_sf("../data/us_eco_l4/us_eco_l4_no_st.shp") 

ecoregions <- st_transform(ecoregions, 3857)
states <- st_transform(states, 3857)


buffer_distance <- units::as_units(50, "kilometers") # want to buffer the edges...
ecoregion_bound <- st_convex_hull(ecoregions)
ecoregion_bound <- st_buffer(ecoregion_bound, dist = buffer_distance) %>% 
  st_transform(5070) %>% 
  st_as_sf() 
ecoregion_bound <- st_transform(ecoregion_bound, 3857)

# dependent variables - prediction

b.alp <- st_read('../data/B.alpina_occ.shp')
b.alp <- st_transform(b.alp, 3857)

png(filename = '../graphics/besseya_occ.png', bg = 'transparent', width = 720, height = 720)
ggmap(co_map) +
  geom_sf(data = states, fill = NA, inherit.aes = FALSE, color = 'grey50') +
  geom_sf(data = ecoregion_bound, alpha = 0.2, color = 'black', fill = NA, lwd = 2, inherit.aes = FALSE) +
  geom_sf(data = ecoregions, fill = "darkslategray4", alpha = 0.5, inherit.aes = FALSE, color = 'darkslategray4') +
  geom_sf_label(data = places, aes(label = NAME), inherit.aes = F,
                alpha = 0.5, label.size  = NA) +
  geom_sf(data = b.alp, inherit.aes = FALSE, color = '#4E2A84', size = 2) +
  theme_void() +
  labs(title = 'Occurrence records of Besseya alpina') + 
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(size=24),)
dev.off()


# dependent variables - including absences

absence <- st_sample(ecoregion_bound, size = nrow(b.alp)) %>% 
  st_as_sf() %>% 
  rename(geometry = 1) %>% 
  mutate(Record = 'Absence')

pa <- b.alp %>% 
  mutate(Record  = 'Presence') %>% 
  select(Record) %>% 
  bind_rows(absence, .)

png(filename = '../graphics/pseudo_abs.png', bg = 'transparent', width = 720, height = 720)
ggmap(co_map) +
  geom_sf(data = states, fill = NA, inherit.aes = FALSE, color = 'grey50') +
  geom_sf(data = ecoregion_bound, alpha = 0.2, color = 'black', fill = NA, lwd = 2, inherit.aes = FALSE) +
  geom_sf(data = ecoregions, fill = "darkslategray4", alpha = 0.5, inherit.aes = FALSE, color = 'darkslategray4') +
  geom_sf_label(data = places, aes(label = NAME), inherit.aes = F,
                alpha = 0.5, label.size  = NA) +
  geom_sf(data = pa, inherit.aes = FALSE, aes(color = Record)) +
  scale_color_manual(values = c("#7f1105", '#4E2A84')) +
  theme_void() +
  labs(title = 'records of Besseya alpina') + 
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=24),
        legend.position = 'bottom')
dev.off()


# prediction surface
p2d <- '../data'

prediction <- rast(file.path(p2d, 
  list.files(path = p2d, pattern = 'tif')))

erb_spat <- vect(ecoregion_bound)
b.alp_spat <- vect(b.alp)

png(filename = '../graphics/besseya_predicted.png', bg = 'transparent', width = 720, height = 720)
plot(prediction, axes = F, main = 'modelled habitat suitability of B. alpina')
dev.off()
