waterDataServer <- function(id, sites, selected_site) {
  moduleServer(id, function(input, output, session) {

    # Site number for current selection
    siteNo <- reactive({
      req(sites())
      req(selected_site())
      site_no <- sites() %>%
        filter(station_nm == selected_site()) %>%
        pull(site_no)
      return(site_no)
    })

    # Discharge data
    discharge <- reactive({
      req(selected_site())
      req(siteNo())
      temp_data <- readNWISuv(
        siteNumbers = siteNo(),
        parameterCd = "00060",
        startDate = Sys.Date() - days(5),
        endDate = Sys.Date()
      ) %>%
        renameNWISColumns() %>%
        arrange(desc(dateTime))
      return(temp_data)
    })

    # Water temperature data
    water_temp <- reactive({
      req(selected_site())
      req(siteNo())
      temp_data <- readNWISuv(
        siteNumbers = siteNo(),
        parameterCd = "00010",
        startDate = Sys.Date() - days(5),
        endDate = Sys.Date()
      ) %>%
        renameNWISColumns()
      if ("Wtemp_Inst" %in% names(temp_data)) {
        temp_data <- temp_data %>% mutate(
          Wtemp_Inst = (Wtemp_Inst * 9/5) + 32
        ) %>%
          arrange(desc(dateTime))
      }
      return(temp_data)
    })

    # Current readings
    current_temp <- reactive({
      req(water_temp())
      if ("Wtemp_Inst" %in% names(water_temp())) {
        return(water_temp()$Wtemp_Inst[1])
      } else {
        return("Not available")
      }
    })

    current_discharge <- reactive({
      req(discharge())
      return(discharge()$Flow_Inst[1])
    })

    return(list(
      siteNo = siteNo,
      discharge = discharge,
      water_temp = water_temp,
      current_temp = current_temp,
      current_discharge = current_discharge
    ))
  })
}
