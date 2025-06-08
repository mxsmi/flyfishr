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

pCode <- "00060"
start.date <- Sys.Date() - days(3)
end.date <- Sys.Date() + days(3)
sCode <- "00003"

s <- lapply(unique(sites_filtered$site_no), 
       function(site) {whatNWISdata(siteNumber = site, service = "uv")})

s_filtered <- s[sapply(s, nrow) > 0]

s_filtered <- s_filtered[sapply(s_filtered, function(df) {"00060" %in% df$parm_cd})]

len <- length(s_filtered)
s_f_vec <- vapply(s_filtered, 
                  function(df) {df$site_no[1]}, 
                  FUN.VALUE = character(1))

available_data <- whatNWISdata(
  siteNumber = siteNo,
  service = "dv"  # unit values (instantaneous)
)

rivers <- readNWISuv(
  siteNumbers = unique(sites$site_no),
  parameterCd = pCode,
  startDate = start.date,
  endDate = end.date
)

river_sites <- sites %>% filter(grepl("frying", sites$station_nm, ignore.case = TRUE))

siteNo <- "09080400"
pCode <- "00060"
start.date <- Sys.Date() - days(3)
end.date <- Sys.Date() + days(3)
sCode <- "00003"

river <- readNWISuv(
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

river <- renameNWISColumns(river)
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

