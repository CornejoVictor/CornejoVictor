# Install necessary packages
install.packages(c("rnaturalearth", "sf", "tidyverse", "wbstats", "paletteer", "leaflet", "rmapshaper", "htmlwidgets"))

# Load libraries
library(rnaturalearth)
library(sf)
library(tidyverse)
library(wbstats)
library(paletteer)
library(leaflet)
library(rmapshaper)
library(htmlwidgets)

# Download simplified country data
countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name, iso_a3, pop_est, geometry) %>%  
  rmapshaper::ms_simplify(keep = 0.1)

# Download population data from the World Bank (2023)
population_data <- wb_data(
  indicator = "SP.POP.TOTL",
  start_date = 2023,
  end_date = 2023
) %>%
  select(iso3c, SP.POP.TOTL)  

# Merge population data with the map
countries_updated <- countries %>%
  left_join(population_data, by = c("iso_a3" = "iso3c")) %>%
  mutate(pop_est = ifelse(!is.na(SP.POP.TOTL), SP.POP.TOTL, pop_est)) %>%
  select(-SP.POP.TOTL)  

# Ensure there are no negative or zero population values
countries_updated <- countries_updated %>%
  filter(!is.na(pop_est) & pop_est > 0) %>%
  mutate(log_pop_est = log10(pop_est))  

# Load airports and ports with scale = 10
airports <- ne_download(scale = 10, type = "airports", category = "cultural", returnclass = "sf")
ports <- ne_download(scale = 10, type = "ports", category = "cultural", returnclass = "sf")

# Define the color palette based on population
colors <- as.character(paletteer_c("ggthemes::Blue-Teal", 30))  

# Create a color function for population
pal <- colorNumeric(
  palette = colors, 
  domain = countries_updated$log_pop_est,  
  na.color = "gray"
)

# Airplane icon
airport_icon <- makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/9239/9239335.png",  
  iconWidth = 20, iconHeight = 20
)

# Port icon
port_icon <- makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/10785/10785638.png",  
  iconWidth = 20, iconHeight = 20
)

# Create the interactive map (WITHOUT LATERAL SCROLLING)
map <- leaflet(countries_updated, options = leafletOptions(
  worldCopyJump = FALSE,  
  dragging = TRUE,
  minZoom = 2,  
  maxZoom = 8   
)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  
  setView(lng = 0, lat = 20, zoom = 2) %>%  
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%  
  addPolygons(
    fillColor = ~pal(log_pop_est),  
    fillOpacity = 0.7,  
    color = "black",  
    weight = 1,  
    smoothFactor = 0,  
    highlightOptions = highlightOptions(
      weight = 2,
      color = "black",
      bringToFront = TRUE
    ),
    label = ~paste(name, ": ", format(pop_est, big.mark = ","), " inhabitants"),  
    popup = ~paste("<b>", name, "</b><br>", "Population: ", format(pop_est, big.mark = ","), " inhabitants")  
  ) %>%
  addMarkers(
    data = airports,
    icon = airport_icon,  
    label = ~name,        
    popup = ~paste("<b>Airport:</b> ", name),  
    group = "Airports"
  ) %>%
  addMarkers(
    data = ports,
    icon = port_icon,     
    label = ~name,        
    popup = ~paste("<b>Port:</b> ", name),  
    group = "Ports"
  ) %>%
  addLegend(
    pal = pal,                      
    values = ~log_pop_est,  
    title = "Population (2023)",    
    position = "bottomright",       
    labFormat = labelFormat(transform = function(x) round(10^x))  
  ) %>%
  addLayersControl(
    overlayGroups = c("Airports", "Ports"),  
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # 🔹 Optimization with JavaScript
  htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      setTimeout(function() { map.invalidateSize(); }, 100);
    }
  ")

# 🔹 Save the optimized HTML
saveWidget(map, "map.html", selfcontained = TRUE)
