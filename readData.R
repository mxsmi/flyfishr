library(dataRetrieval)
library(dplyr)
library(stringr)
library(ggplot2)

sites_wa <- whatNWISsites(stateCd = "WA")

yakima_sites <- sites_wa %>% filter(grepl("yakima", sites_wa$station_nm, ignore.case = TRUE))

siteNo <- "12484500"
pCode <- "00060"
start.date <- "2025-05-30"
end.date <- "2025-06-05"

yakima <- readNWISuv(
  siteNumbers = siteNo,
  parameterCd = pCode,
  startDate = start.date,
  endDate = end.date
)

yakima <- renameNWISColumns(yakima)
names(attributes(yakima))

variableInfo <- attr(yakima, "variableInfo")
siteInfo <- attr(yakima, "siteInfo")

discharge_plot <- ggplot(
  data = yakima,
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

