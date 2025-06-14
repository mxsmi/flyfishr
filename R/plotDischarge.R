### Function to create the discharge plot in StreamNotes app

plotDischarge <- function(site_no) {

  ## Parameters for readNWISuv (the function that gets the data for a specific site)
  siteNo <- site_no
  pCode <- "00060"
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
    need(nrow(site) > 0, "No discharge data available for this river"),
    need("Flow_Inst" %in% names(site), "Flow data not found"),
  )

  ## Build plot
  discharge_plot <- ggplot(
    data = site,
    aes(dateTime, Flow_Inst)
  ) +
    geom_line(aes(color = "Flow")) +
    geom_point(data = site_stat,
               aes(x = Date, y = Flow, color = "Mean daily value"),
               # color = "red",
               shape = 17,
               size = 3
    ) +
    scale_color_manual(
      name = "Legend",
      values = c("Flow" = "blue", "Mean daily value" = "red")
    ) +
    labs(
      x = "Date",
      y = variableInfo$variableDescription,
      title = siteInfo$station_nm
    ) +
    theme_minimal() +
    theme(axis.title = element_text(size = 14))

  ## Return plot
  discharge_plot

}
