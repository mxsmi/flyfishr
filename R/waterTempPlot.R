### Function to generate the water temperature (F) plot

waterTempPlot <- function(site_no) {
 
  ## Parameters for readNWISuv (the function that gets the data for a specific site)
  siteNo <- site_no
  pCode <- "00010"
  start.date <- Sys.Date() - days(5)
  end.date <- Sys.Date()
  sCode <- "00003"
  
  ## Get site data
  site <- readNWISuv(
    siteNumbers = siteNo,
    parameterCd = pCode,
    startDate = start.date,
    endDate = end.date
  )
  
  ## Get mean daily value for site
  site_stat <- readNWISdv(
    siteNumbers = siteNo,
    parameterCd = pCode,
    startDate = start.date,
    endDate = end.date,
    statCd = sCode
  )
   
  ## dataRetreival's built in clean names function
  site <- renameNWISColumns(site)
  site_stat <- renameNWISColumns(site_stat)
  ## Convert Date column to DateTime for mean daily value data
  site_stat$Date <- as_datetime(site_stat$Date)
  
  ## Store variable info and site info attributes to label the plot with
  variableInfo <- attr(site, "variableInfo")
  siteInfo <- attr(site, "siteInfo")
  
  ## Build the plot of water discharge
  # Validate data first
  validate(
    need(nrow(site) > 0, "No water temp data available for this river"),
    need("Wtemp_Inst" %in% names(site), "Temp data not found"),
  )
  
  ## Convert from Celsius to Fahrenheit
  site_stat$Wtemp <- (site_stat$Wtemp * 9/5) + 32
  site$Wtemp_Inst <- (site$Wtemp_Inst * 9/5) + 32
  ## Build plot
  water_temp_plot <- ggplot(
    data = site,
    aes(dateTime, Wtemp_Inst)
  ) + 
    geom_line(aes(color = "Temp")) + 
    geom_point(data = site_stat,
               aes(x = Date, y = Wtemp, color = "Mean daily value"),
               # color = "red",
               shape = 15,
               size = 3
    ) +
    scale_color_manual(
      name = "Legend",
      values = c("Temp" = "forestgreen", "Mean daily value" = "magenta")
    ) +
    labs(
      x = "Date",
      y = str_replace(variableInfo$variableDescription, "Celsius", "Fahrenheit"),
      title = siteInfo$station_nm
    ) + 
    theme_minimal() + 
    theme(axis.title = element_text(size = 14))
  
  ## Return plot
  water_temp_plot
  
}