location <- c("lunch", "view")
addr <- c( "10 Main Street, Middletown, CT",
           "20 W 34th St., New York, NY, 10001")
df <- data.frame(location, addr)


knitr::kable(df, caption = "Address data")


library(tidygeocoder)
df <- tidygeocoder::geocode(df, address = addr, method = "osm")


knitr::kable(df, caption = "Address data with latitude and longitude")


library(ggmap)

# subset the data
library(dplyr)
homicide <- filter(crime, offense == "murder") %>%
  select(date, offense, address, lon, lat)

# view data
head(homicide, 3)


library(mapview)
library(sf)
mymap <- st_as_sf(homicide, coords = c("lon", "lat"), crs = 4326)
mapview(mymap)

library(sf)
library(mapview)
mymap <- st_as_sf(homicide, coords = c("lon", "lat"), crs = 4326)
mapview(mymap, color="black", col.regions="red",
        alpha.regions=0.5, legend = FALSE,
        homebutton = FALSE, map.types = "OpenStreetMap" )

# create leaflet graph
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=-72.6560002,
             lat=41.5541829,
             popup="The birthplace of quantitative wisdom.</br>
             No, Waldo is not here.")

ggmap::register_stadiamaps("your API key")

# find a bounding box for Houston, Texas
library(osmdata)
bb <- getbb("houston, tx")
bb

library(ggmap)
houston <- get_stadiamap(bbox = c(bb[1,1], bb[2,1], 
                                  bb[1,2], bb[2,2]),
                         maptype="stamen_toner_lite")
ggmap(houston)

# add incident locations
ggmap(houston) +
  geom_point(aes(x=lon,y=lat),data=homicide,
             color = "red", size = 2, alpha = 0.5)


# remove long and lat numbers and add titles
ggmap(houston) +
  geom_point(aes(x=lon,y=lat),data=homicide,
             color = "red", size = 2, alpha = 0.5) +
  theme_void() +
  labs(title = "Location of reported homocides",
       subtitle = "Houston Jan - Aug 2010",
       caption = "source: http://www.houstontx.gov/police/cs/")


library(ggmap)
# using geocode function to obtain the center coordinates
register_google(key="PutYourGoogleAPIKeyHere")
houston_center <- geocode("Houston, TX")

houston_center

# get Houston map
houston_map <- get_map(houston_center, 
                       zoom = 13, 
                       maptype = "roadmap")
ggmap(houston_map)

# add incident locations
ggmap(houston_map) +
  geom_point(aes(x=lon,y=lat),data=homicide,
             color = "red", size = 2, alpha = 0.5)

# add incident locations
ggmap(houston_map) +
  geom_point(aes(x=lon,y=lat),data=homicide,
             color = "red", size = 2, alpha = 0.5) +
  theme_void() +
  labs(title = "Location of reported homocides",
       subtitle = "Houston Jan - Aug 2010",
       caption = "source: http://www.houstontx.gov/police/cs/")


# view the first 12 region names in country.map
data(country.map, package = "choroplethrMaps")
head(unique(country.map$region), 12)

## ----choropleth2, message=FALSE, warning=FALSE---------------------------
# prepare dataset
data(gapminder, package = "gapminder")
plotdata <- gapminder %>%
  filter(year == 2007) %>%
  rename(region = country,
         value = lifeExp) %>%
  mutate(region = tolower(region)) %>%
  mutate(region = 
    recode(region,
          "united states"    = "united states of america",
          "congo, dem. rep." = "democratic republic of the congo",
          "congo, rep."      = "republic of congo",
          "korea, dem. rep." = "south korea",
          "korea. rep."      = "north korea",
          "tanzania"         = "united republic of tanzania",
          "serbia"           = "republic of serbia",
          "slovak republic"  = "slovakia",
          "yemen, rep."      = "yemen"))


library(choroplethr)
country_choropleth(plotdata)

country_choropleth(plotdata,
                   num_colors=9) +
  scale_fill_brewer(palette="YlOrRd") +
  labs(title = "Life expectancy by country",
       subtitle = "Gapminder 2007 data",
       caption = "source: https://www.gapminder.org",
       fill = "Years")


library(ggplot2)
library(choroplethr)
data(continental_us_states)

library(readr)
hisplat <- read_tsv("hisplat.csv")

hisplat$region <- tolower(hisplat$state)
hisplat$value <- hisplat$percent

state_choropleth(hisplat, 
                 num_colors=9,
                 zoom = continental_us_states) +
  scale_fill_brewer(palette="YlGnBu") +
  labs(title = "Hispanic and Latino Population",
       subtitle = "2010 US Census",
       caption = "source: https://tinyurl.com/2fp7c5bw",
       fill = "Percent") 


library(ggplot2)
library(choroplethr)
library(dplyr)

# enter violent crime rates by county
crimes_ct <- data.frame(
  county = c("fairfield", "hartford", 
             "litchfield", "middlesex", 
             "new haven", "new london", 
             "tolland", "windham"),
  value = c(3.00, 3.32, 
            1.02, 1.24, 
            4.13, 4.61, 
            0.16, 1.60)
)

crimes_ct


# obtain region codes for connecticut
data(county.regions, 
     package = "choroplethrMaps")
region <- county.regions %>%
  filter(state.name == "connecticut")

region


# join crime data to region code data
plotdata <- inner_join(crimes_ct, 
                       region, 
                       by=c("county" = "county.name"))
plotdata

# create choropleth map
county_choropleth(plotdata, 
                  state_zoom = "connecticut",
                  reference_map = TRUE,
                  num_colors = 8) +
  scale_fill_brewer(palette="YlOrRd") +
  labs(title = "Connecticut Violent Crime Rates",
       subtitle = "FBI 2012 data",
       caption = "source: https://ucr.fbi.gov",
       fill = "Violent Crime\n Rate Per 1000")


library(sf)
# unzip shape file
shapefile <- "cb_2022_us_state_20m.zip"
shapedir  <- tools::file_path_sans_ext(shapefile)
if(!dir.exists(shapedir)){
  unzip(shapefile, exdir=shapedir)
}

# convert the shapefile into a data frame 
# of class sf (simple features)
USMap <- st_read("cb_2022_us_state_20m/cb_2022_us_state_20m.shp")

head(USMap, 3)

litRates <- read.csv("USLitRates.csv")
head(litRates, 3)

# states in litRates not in USMap
setdiff(litRates$State, USMap$NAME)

continentalUS <- USMap %>% 
  left_join(litRates, by=c("NAME"="State")) %>%
  filter(NAME != "Hawaii" & NAME != "Alaska" & 
           NAME != "Puerto Rico")
head(continentalUS, 3)

library(ggplot2)
ggplot(continentalUS, aes(geometry=geometry, fill=Rate)) +
  geom_sf()

library(dplyr)
ggplot(continentalUS, aes(geometry=geometry, fill=Rate)) +
  geom_sf() +
  theme_void() +
  geom_sf_text(aes(label=STUSPS), size=2) +
  scale_fill_steps(low="yellow", high="royalblue", 
                   n.breaks = 10) +
  labs(title="Literacy Rates by State",
       fill = "% literate",
       x = "", y = "",
       subtitle="Updated May 2023",
       caption="source: https://worldpopulationreview.com")

