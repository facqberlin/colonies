library(needs)
needs(tidyverse,
      sf,
      ggplot2,
      rnaturalearth,
      osmdata,
      # geojsonrewind,
      jsonlite)

## to do: use this tool to rewind everything: https://observablehq.com/@bumbeishvili/rewind-geojson

## to do: add arguin to background countries
## brandenburg gold coast and tianjin are too small; think of solution

## to do: remove antarctica from background

data <- read_csv("input/colonies_details.csv")

countries <- ne_countries(returnclass = "sf", scale = "medium") %>%
  dplyr::select(name_long)#%>%
  # filter(name_long!="Antarctica")

fn <- "current_borders.geojson"
if(file.exists(fn)){
  file.remove(fn)
}
write_sf(countries, fn)

# borders_1900 <- st_read("https://raw.githubusercontent.com/aourednik/historical-basemaps/master/geojson/world_1900.geojson")
borders_1914 <- st_read("https://raw.githubusercontent.com/aourednik/historical-basemaps/master/geojson/world_1914.geojson")
# borders_1700 <- st_read("https://raw.githubusercontent.com/aourednik/historical-basemaps/master/geojson/world_1700.geojson")

## 1914
found_1914 <- borders_1914 %>%
  filter(grepl("Togoland|German.*Africa|Kamerun", NAME))%>%
  st_transform(4326)

colonies <- found_1914 %>%
              dplyr::select(name = NAME)

found_today <- ne_countries(returnclass = "sf", scale = "medium") %>%
  filter(grepl("Mariana|Palau|Marshall|Nauru|Samoa|Solomon|Micronesia", admin))%>%
  filter(admin != "American Samoa")%>%
  st_transform(4326)
## Samoa is what used to be Western Samoa (German colony), American Samoa was colonized by US
## Mariana Islands are Northern Mariana Islands (German colony), Southern islands are Guam (US colony)

colonies <- colonies %>%
  bind_rows(found_today %>%
              dplyr::select(name = admin))

## manually drawn shapes with geojson.io

# ## German concession in Tianjin
# https://en.wikipedia.org/wiki/Foreign_concessions_in_Tianjin
# and
## Jiaozhou Bay
# https://en.wikipedia.org/wiki/Kiautschou_Bay_Leased_Territory

shape <- st_read("input/tianjin.geojson") %>%
  mutate(name = c("Jiaozhou Bay", "Tianjin"))

shapes <- shape %>%
  filter(name == "Tianjin")

shape <- shape %>%
  filter(name == "Jiaozhou Bay")%>%
  st_intersection(countries %>% st_make_valid())

shapes <- shapes %>%
  bind_rows(shape %>%
              dplyr::select(-name_long))

## Wituland
# https://en.wikipedia.org/wiki/Wituland

shape <- st_read("input/wituland_raw.geojson") %>%
  mutate(name = "Wituland")%>%
  st_intersection(countries %>% st_make_valid())

shapes <- shapes %>%
  bind_rows(shape %>%
              dplyr::select(-name_long))

## Kaiser Wilhelmsland
# https://de.wikipedia.org/wiki/Kaiser-Wilhelms-Land

shape <- st_read("input/wilhelmsland_raw.geojson") %>%
  mutate(name = "Kaiser-Wilhelmsland")%>%
  st_intersection(countries %>% st_make_valid())

shapes <- shapes %>%
  bind_rows(shape %>%
              dplyr::select(-name_long))

## welserland
# https://de.wikipedia.org/wiki/Klein-Venedig_(Venezuela)

shape <- st_read("input/welserland_raw.geojson") %>%
  mutate(name = "Welserland")%>%
  st_intersection(countries %>% filter(name_long == "Venezuela")%>% st_make_valid())%>%
  st_cast("POLYGON")%>%
  mutate(area = st_area(geometry))%>%
  ## biggest area is main island, not part of archipelago
  filter(area == max(area))

shapes <- shapes %>%
  bind_rows(shape %>%
              dplyr::select(-name_long, -area))

## open street map
## --------------

## brandenburg gold coast

osmquery <- opq_osm_id(c(240909315,216464672), type = "way")%>%
  osmdata_sf()

shapes <- shapes %>%
  bind_rows(osmquery$osm_polygons %>%
              mutate(name = "Brandenburg Gold Coast")%>%
              group_by(name)%>%
              summarize(geometry = st_union(geometry)))

## st thomas

osmquery <- opq_osm_id(2754159, type = "relation")%>%
  osmdata_sf()

shapes <- shapes %>%
  bind_rows(osmquery$osm_multipolygons %>%
              mutate(name = "St. Thomas")%>%
              dplyr::select(name))

## arguin

osmquery <- opq_osm_id(4109731, type = "way")%>%
  osmdata_sf()

shapes <- shapes %>%
  bind_rows(osmquery$osm_polygons %>%
              mutate(name = "Arguin")%>%
              dplyr::select(name))

## bismarck archipelago

osmquery <- opq_osm_id(c(3777382, 3777383, 3777381, 3777384, 311780, 311779, 311778), type = "relation")%>%
  osmdata_sf()

provinces <- osmquery$osm_multipolygons %>%
  summarize(geometry = st_union(geometry))

result <- st_intersection(provinces, countries %>% filter(name_long == "Papua New Guinea")) %>%
  st_cast("POLYGON")%>%
  mutate(area = st_area(geometry))%>%
  ## biggest area is main island, not part of archipelago
  filter(area != max(area))%>%
  summarize(geometry = st_union(geometry))%>%
  mutate(name = "Bismarck Archipelago")

shapes <- shapes %>%
  bind_rows(result)

colonies <- colonies %>%
  bind_rows(shapes)

## finalize / add info

final <- colonies %>%
  mutate(name = str_replace_all(name,
                                c("German E\\. Africa \\(Tanganyika\\)" = "German East Africa",
                                  "Northern Mariana Islands" = "Mariana Islands",
                                  "Federated States of Micronesia" = "Caroline Islands",
                                  "Solomon Islands" = "North Solomon Islands")))%>%
  left_join(data)%>%
  st_make_valid()

fn <-  "test_colonies_pre_rewind.geojson"
if(file.exists(fn)){
  file.remove(fn)
}
write_sf(final, fn)
