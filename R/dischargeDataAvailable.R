### Function for processing data for a given state. Retrieves data for that state,
### then filters it to only streams/springs (ST/SP) and only sites that have
### instantaneous flow data (a Flow_Inst column after applying renameNWISColumns)

library(dataRetrieval)

dischargeDataAvailable <- function(state, site) {

  validate(
    need(state %in% stateCd$STUSAB, "Invalid state selection")
  )

  data <- whatNWISsites(stateCd = state) ## load data for selected state
  data <- data %>% ## keep only streams and springs that match riverinput
    filter(nchar(site_no) == 8,
           site_tp_cd == "ST" | site_tp_cd == "SP",
           str_detect(tolower(station_nm), tolower(site)),
    )
  ## Returns a list of dataframes with the results of whatNWISdata
  ## (service = uv - "unit"/instantaneous values) for each site.
  sites_with_dis <- lapply(unique(data$site_no),
                           function(s) {
                             whatNWISdata(siteNumber = s, service = "uv")
                             })

  ## Subset the list to remove any sites that don't have any unit values
  sites_with_dis <- sites_with_dis[sapply(sites_with_dis, nrow) > 0]
  ## Subset the list to only keep dataframes that contain discharge data
  ## ("00060" parameter code).
  sites_with_dis <- sites_with_dis[sapply(sites_with_dis,
                              function(df) {
                                "00060" %in% df$parm_cd
                                })]

  ##
  sites_with_dis_df <- data.frame(
    site_no = vapply(sites_with_dis, function(df) df$site_no[1],
                     FUN.VALUE = character(1)),
    station_nm = vapply(sites_with_dis, function(df) df$station_nm[1],
                        FUN.VALUE = character(1)),
    stringsAsFactors = FALSE
  )

  return(sites_with_dis_df)
}
