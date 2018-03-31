
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
election <- read.csv("D:/620/MUSA-620-Week-3-master/MUSA-620-Week-3-master/preselect16results.csv")
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

ggplot() +
  geom_sf(data = elecjoin, aes(fill = lead), color = NA) +
  scale_fill_manual(values = c('#cc3333', '#3333cc')) +
  myTheme()



# **** Set the projection (EPSG code) ****
# Complete listing here: http://spatialreference.org/
# 4326 - WGS 84 (standard lat,lng coordinates)
# 3395 - Mercator
# 102003 - Albers

ggplot() +
  geom_sf(data = elecjoin, aes(fill = lead), color = NA) +
  scale_fill_manual(values = c('#cc3333', '#3333cc')) +
  coord_sf(crs = st_crs(102003)) +
  myTheme()



# Display the county borders. Give the appearance of thinness using alpha.
# Set labels
# Set legend title

ggplot() +
  geom_sf(data = elecjoin, aes(fill = lead),
          #color="white", size=0.000001) +           # you can only get so thin to illustrate
          color = alpha("white",0.2), size=0.1) +    # appearance of thinness
  scale_fill_manual(values = c('#cc3333', '#3333cc'),name = "Winner") +
  coord_sf(crs = st_crs(102003)) +
  labs(
    title = 'Results of the 2016 Presidential Election',
    subtitle = "Winning candidate in each U.S. county",
    caption = "Source: Mark Kearney"
  ) +
  myTheme()

#create a cartogram
#export into shapefile
st_transform(st_as_sf(elecjoin),crs = 4326) %>%
  st_write("election-map1.shp", driver = "ESRI Shapefile")
#import the cartogram
cartogram <- st_read('C:/Users/dell/Documents/election-map1.shp', stringsAsFactors = FALSE)
st_crs(cartogram) = 4326
#display the cartogram
ggplot() +
  geom_sf(data = cartogram, aes(fill = lead),
          #color="white", size=0.000001) +           # you can only get so thin to illustrate
          color = alpha("white",0.2), size=0.1) +    # appearance of thinness
  scale_fill_manual(values = c('#cc3333', '#3333cc'),name = "Winner") +
  #coord_sf(crs = st_crs(102003)) +
  labs(
    title = 'Results of the 2016 Presidential Election',
    subtitle = "Winning candidate in each U.S. county",
    caption = "Source: Mark Kearney"
  ) +
  myTheme()

#Set to continuous map
head(elecjoin)
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
  st_write("election-map3.shp", driver = "ESRI Shapefile")
#create a cartogram

#import the cartogram
cartogram <- st_read('C:/Users/dell/Documents/620Class3/election-cartogram1.shp', stringsAsFactors = FALSE)
st_crs(cartogram) = 4326

cartogram <- st_read('election-cartogram.shp', stringsAsFactors = FALSE)
st_crs(cartogram) = 4326

#alpha("white",0.8) #is to set the transparency of the boundary lines

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




