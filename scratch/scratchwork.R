library(dataRetrieval)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

sites <- whatNWISsites(stateCd = "WA")

sites_filtered <- sites %>% 
  filter(nchar(site_no) == 8,
         site_tp_cd == "ST" | site_tp_cd == "SP", ## keep only streams and springs
         str_detect(tolower(station_nm), "yakima")
  )

siteNo <- "12510500"
pCode <- "00060"
start.date <- Sys.Date() - days(3)
end.date <- Sys.Date() + days(3)
sCode <- "00003"

rivers <- readNWISuv(
  siteNumbers = siteNo,
  parameterCd = pCode,
  startDate = start.date,
  endDate = end.date
)

river_stat <- readNWISdv(
  siteNumbers = siteNo,
  parameterCd = pCode,
  startDate = start.date,
  endDate = end.date,
  statCd = sCode
)

site_info <- readNWISsite(siteNumbers = siteNo)

# Extract coordinates
lat <- site_info$dec_lat_va
lon <- site_info$dec_long_va

# Create leaflet map
map <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  setView(lng = lon, lat = lat, zoom = 12) %>%  # Center on the site
  addMarkers(lng = lon, lat = lat)

# Display the map
map

rivers <- renameNWISColumns(rivers)
names(attributes(river))

variableInfo <- attr(river, "variableInfo")
siteInfo <- attr(river, "siteInfo")

discharge_plot <- ggplot(
  data = river,
  aes(dateTime, Flow_Inst)
) + 
geom_line(color = "blue") + 
  labs(
    x = "Date",
    y = variableInfo$variableDescription,
    title = siteInfo$station_nm
  ) + 
  theme_minimal()

discharge_plot

