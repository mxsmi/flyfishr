library(dataRetrieval)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

sites <- whatNWISsites(stateCd = "CO", )

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

