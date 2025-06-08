### Function for processing data for a given state. Retrieves data for that state,
### then filters it to only streams/springs (ST/SP) and only sites that have 
### instantaneous flow data (a Flow_Inst column after applying renameNWISColumns)

dischargeDataAvailable <- function(data) {
  sites_with_dis <- lapply(unique(data$site_no), 
                           function(site) {
                             whatNWISdata(siteNumber = site, service = "uv")
                             })
  
  sites_with_dis <- sites_with_dis[sapply(sites_with_dis, nrow) > 0]
  sites_with_dis <- sites_with_dis[sapply(sites_with_dis, 
                              function(df) {
                                "00060" %in% df$parm_cd
                                })]
  sites_with_dis_vec <- vapply(sites_with_dis, 
                              function(df) {
                                df$station_nm[1]
                                }, 
                              FUN.VALUE = character(1)) 
  return(sites_with_dis_vec)
}