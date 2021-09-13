library(tidyverse)
library(osmdata)
library(sf)

q <- opq("Milwaukee, WI") %>%
  add_osm_feature(key = "highway", value = "motorway") %>%
  add_osm_feature(key = "highway", value = "motorway_link") %>%
  osmdata_sf()

t <- opq("Milwaukee, WI") %>%
  add_osm_feature(key = "highway", value = "trunk") %>%
  add_osm_feature(key = "highway", value = "trunk_link") %>%
  osmdata_sf()

p <- opq("Milwaukee, WI") %>%
  add_osm_feature(key = "highway", value = "primary") %>%
  add_osm_feature(key = "highway", value = "primary_link") %>%
  osmdata_sf()

s <- opq("Milwaukee, WI") %>%
  add_osm_feature(key = "highway", value = "secondary") %>%
  add_osm_feature(key = "highway", value = "secondary_link") %>%
  osmdata_sf()

t <- opq("Milwaukee, WI") %>%
  add_osm_feature(key = "highway", value = "tertiary") %>%
  add_osm_feature(key = "highway", value = "tertiary_link") %>%
  osmdata_sf()

r <- opq("Milwaukee, WI") %>%
  add_osm_feature(key = "highway", value = "residential") %>%
  add_osm_feature(key = "highway", value = "residential_link") %>%
  osmdata_sf()

v <- opq("Milwaukee, WI") %>%
  add_osm_feature(key = "highway", value = "service") %>%
  add_osm_feature(key = "highway", value = "service_link") %>%
  osmdata_sf()

citylimits <- st_read("../Shapefiles/Milwaukee/City Limits/citylimit.shp") %>%
  st_transform(., crs = st_crs(4326))

hoods <- st_read("../ShapeFiles/Milwaukee/Neighborhoods/neighborhood.shp") %>%
  st_transform(., crs = st_crs(4326))

west <- st_sfc(st_point(x = c(-88.07097, 42.92082)), crs = st_crs(4326))
east <- st_sfc(st_point(x = c(-87.86376, 42.92082)), crs = st_crs(4326))
north <- st_sfc(st_point(x = c(-87.86376, 42.92082)), crs = st_crs(4326))
south <- st_sfc(st_point(x = c(-87.86376, 43.19477)), crs = st_crs(4326))

ew <- st_distance(west, east)
ns <- st_distance(north, south)

all <- list("q" = q, 
            "t" = t, 
            "p" = p, 
            "s" = s, 
            "t" = t, 
            "r" = r,
            "v" = v) %>%
  map(., function(x) {
    st_transform(x$osm_lines, crs = st_crs(hoods)) %>%
      st_intersection(., hoods %>% filter(NEIGHBORHD == "BAY VIEW"))
  })

size <- 0.01

pdf("test.pdf", width = 7, height = 7, bg = "black")


all$q %>%
  ggplot() +
  geom_sf(size = size, color = "white") +
  geom_sf(data = all$t, size = size, color = "white") +
  geom_sf(data = all$p, size = size, color = "white") +
  geom_sf(data = all$s, size = size, color = "white") +
  geom_sf(data = all$t, size = size, color = "white") +
  geom_sf(data = all$r, size = size, color = "white") +
  geom_sf(data = all$v, size = size, color = "white") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"),
        plot.title = element_text(hjust = 0.5, 
                                  color = "white",
                                  family = "serif", 
                                  face = "bold",
                                  size = 16)) +
  labs(title = "BAY VIEW")

dev.off()

t$osm_lines %>%
  ggplot() +
  geom_sf()
