### Function to create a map of the currently selected site to display in 
### StreamNotes app

createSiteMap <- function(site) {
  ## Get site info
  site_info <- readNWISsite(siteNumbers = site)
  
  ## Extract coordinates
  lat <- site_info$dec_lat_va
  lon <- site_info$dec_long_va
  
  ## Create leaflet map
  map <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap tiles
    setView(lng = lon, lat = lat, zoom = 12) %>%  # Center on the site
    addMarkers(lng = lon, lat = lat)
  
  ## Return the map
  map
}