### R function that returns a prompt to give to the AI model to generate a fly-
### fishing report for the currently selected river

library(glue)
library(readr)

fishingReportPrompt <- function(river, date = Sys.Date(), temp) {
  path <- paste0(getwd(), "/R/prompt.txt")
  promptText <- read_file(path)
  prompt <- glue(promptText)
  return(prompt)
}