### R function that returns a prompt to give to the AI model to generate a fly-
### fishing report for the currently selected river

## Load libraries
library(glue)
library(readr)

fishingReportPrompt <- function(site, date = Sys.Date(), temp, flow) {
  path <- paste0(getwd(), "/R/prompt.txt")
  promptText <- read_file(path)
  prompt <- glue(promptText)
  return(prompt)
}
