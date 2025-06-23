### This module defines the UI and associated server function for getting the
### current water data (discharge and water temp) for the currently selected site
### Returns a list of reactive values for the currently selected site:
###   - siteNo: Site number
###   - discharge: Discharge data for the last 5 days
###   - water_temp: Water Temperature data for the last 5 days
###   - current_temp: Most recent water temperature reading (F)
###   - current_discharge: Most recent discharge reading (cubic feet per second)

waterDataServer <- function(id, sites, selected_site) {
  moduleServer(id, function(input, output, session) {

    ## Site number for current selection
    siteNo <- reactive({
      req(sites())
      req(selected_site())
      site_no <- sites() %>%
        filter(station_nm == selected_site()) %>%
        pull(site_no)
      return(site_no)
    })

    ## Discharge data
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

    ## Water temperature data
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

      ## If instantaneous water temp values are available, convert them from
      ## Celsius to Fahrenheit
      if ("Wtemp_Inst" %in% names(temp_data)) {
        temp_data <- temp_data %>% mutate(
          Wtemp_Inst = (Wtemp_Inst * 9/5) + 32
        ) %>%
          arrange(desc(dateTime))
      }
      return(temp_data)
    })

    ## Most recent water temp reading
    current_temp <- reactive({
      req(water_temp())
      if ("Wtemp_Inst" %in% names(water_temp())) {
        return(water_temp()$Wtemp_Inst[1])
      } else {
        return("Not available")
      }
    })

    ## Most recent discharge reading
    current_discharge <- reactive({
      req(discharge())
      return(discharge()$Flow_Inst[1])
    })

    ## Return list of results to be stored as reactive values in flyfishrApp.R
    return(list(
      siteNo = siteNo,
      discharge = discharge,
      water_temp = water_temp,
      current_temp = current_temp,
      current_discharge = current_discharge
    ))
  })
}
