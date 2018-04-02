
install.packages("devtools")
library(devtools)

install_github("tidyverse/ggplot2")
install_github("hrbrmstr/albersusa")


library(ggplot2)
library(sf)
library(dplyr)
library(ggmap)
library(viridis)# this one is for color
library(albersusa)
library(magrittr)


# **** US county map - Albers USA Composite projection ****(comes from the albersusa package)
counties <- counties_sf()
counties <- mutate(counties,fips=as.character(fips))
counties <- mutate(counties,fips=ifelse(state=="Alaska","02000",fips))
####% is for connecting the codes together

counties <- counties_sf()%>%
 mutate(counties,fips=as.character(fips))%>%
 mutate(counties,fips=ifelse(state=="Alaska","02000",fips))
####% The same as the previous codes

# **** 2016 Election Results ****(county level election results)
election <- read.csv("preselect16results.csv")
election <- filter(election,!is.na(county))  # remove all but the county-level data
election <- mutate(election,fips=as.character(fips)) # make "fips" a string
election <- mutate(election,fips = ifelse(nchar(fips)==4,paste0("0",fips),fips)) # add a leading "0" if it has a length of 4
head(election)
#We cannot join the numbers together in R, we need to paste a 0. 


elecjoin <- left_join(counties,election,c("fips","fips")) %>%
  filter(!is.na(lead)) %>%
  st_as_sf()
head(elecjoin)

ggplot() +
  geom_sf(data = elecjoin, aes(fill = lead), color = NA) +
  scale_fill_manual(values = c('#cc3333', '#3333cc'))


# **** Design your theme ****
# Theme documentation: http://ggplot2.tidyverse.org/reference/theme.html
myTheme <- function() {
  theme_void() + 
    theme(
      text = element_text(size = 7),
      plot.title = element_text(size = 14, color = "#111111", hjust = 0, vjust = 0, face = "bold"), 
      plot.subtitle = element_text(size = 12, color = "#333333", hjust = 0, vjust = 0),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(colour = "white"),
      legend.direction = "vertical", 
      legend.position = "right",
      plot.margin = margin(0, 0, 0, 0, 'cm'),
      legend.key.height = unit(1, "cm"), legend.key.width = unit(0.4, "cm"),
      legend.title = element_text(size = 9, color = "#111111", hjust = 0, vjust = 0, face = "bold"),
      legend.text = element_text(size = 8, color = "#111111", hjust = 0, vjust = 0)
    ) 
}

###assignment
#Set a continuous variable for displaying the 
marginofvictory <- mutate(elecjoin, percentage = Hillary.Clinton - Donald.Trump)
head(marginofvictory)

# Plot it using geom_sf()
ggplot() +
  geom_sf(data = marginofvictory, aes(fill = percentage),
          color = alpha("white",0.4), size=0.1) +
  scale_fill_gradient(low="tomato3", high="royalblue", name="Percentage of \nHillary over trump") +
  coord_sf(crs = st_crs(102003)) +
  labs(
    title = 'Results of the 2016 Presidential Election',
    subtitle = "Percentage of Hillary over trump",
    caption = "Source: Mark Kearney"
  )+
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    panel.background = element_rect(fill = "ghostwhite"),
    panel.border = element_rect(colour = "lightcyan4", fill=NA, size=0.5),
    panel.grid.major = element_line(color = "white")
  )

# to create a carto map
st_transform(st_as_sf(marginofvictory),crs = 4326) %>%
  st_write("shapefile/election-map.shp", driver = "ESRI Shapefile")

#create a cartogram
#import the cartogram
cartogram <- st_read('shapefile/election-cartogram1.shp', stringsAsFactors = FALSE)
st_crs(cartogram) = 4326
#alpha #is to set the transparency of the boundary lines

# Plot cartogram using geom_sf()
ggplot() +
  geom_sf(data = cartogram, aes(fill = percntg),
          color = alpha("white",0.4), size=0.1) +
  scale_fill_gradient(low="tomato3", high="royalblue", name="Percentage of \nHillary over trump") +
  coord_sf(crs = st_crs(102003)) +
  labs(
    title = 'Results of the 2016 Presidential Election',
    subtitle = "Percentage of Hillary over trump",
    caption = "Source: Mark Kearney"
  )+
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    panel.background = element_rect(fill = "ghostwhite"),
    panel.border = element_rect(colour = "lightcyan4", fill=NA, size=0.5),
    panel.grid.major = element_line(color = "white")
  )


################################################
########write electionmap into geojson##########
#About how to convert shapefile to JSON: 
#https://blog.exploratory.io/creating-geojson-out-of-shapefile-in-r-40bc0005857d

library(rgdal)
library(geojsonio)
library(spdplyr)
library(rmapshaper)

map1 <- dplyr::select(marginofvictory,fips,percentage,geometry) %>% 
  rename(FIPS = fips)

# Convert SP Data Frame to GeoJSON.
map1_json <- geojson_json(map1)

# Simplify the geometry information of GeoJSON.
map1_sim <- ms_simplify(map1_json)

# Keep only the polygons inside the bbox (boundary box).
map1_clipped <- ms_clip(map1_sim, bbox = c(-170, 15, -55, 72))

# Save it to a local file system.
geojson_write(map1_clipped, file = "JSON/map1.geojson")

#########################################
########write cartogram into geojson##########
map2 <- dplyr::select(cartogram,fips,percntg,geometry) %>% 
  rename(FIPS = fips, percentage = percntg)

# Convert SP Data Frame to GeoJSON.
map2_json <- geojson_json(map2)

# Simplify the geometry information of GeoJSON.
map2_sim <- ms_simplify(map2_json)

# Keep only the polygons inside the bbox (boundary box).
map2_clipped <- ms_clip(map2_sim, bbox = c(-170, 15, -55, 72))

# Save it to a local file system.
geojson_write(map2_clipped, file = "JSON/map2.geojson")



