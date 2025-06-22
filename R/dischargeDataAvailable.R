### Function for processing data for a given state. Retrieves data for that state,
### then filters it to only streams/springs (ST/SP) and only sites that have
### instantaneous flow data (a Flow_Inst column after applying renameNWISColumns)

library(dataRetrieval)

dischargeDataAvailable <- function(state) {

  ## Fetch sites that have Instantaneous discharge data for the selected state
  data <- whatNWISdata(
    stateCd = state,
    service = "uv",
    parameterCd = "00060"
  )

  ## Filter data to only names sites that are streams or springs
  data <- data %>%
    filter(nchar(site_no) == 8,
           site_tp_cd == "ST" | site_tp_cd == "SP"
    )

  ## If now data is found, return NULL
  if (!nrow(data) > 0) {
    return(NULL)
  }

  return(data)
}
