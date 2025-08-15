## R function that returns a prompt to give to the AI model to generate a fly-
## fishing report for the currently selected river

### Load libraries
library(glue)
library(readr)

fishingReportPrompt <- function(site, date = Sys.Date(), temp, flow) {
  ### Read prompt text from the prompt.txt
  promptText <- read_file("data/prompt.txt")
  ### use glue::glue to insert the site-specific variables into the prompt
  prompt <- glue(promptText)
  return(prompt)
}
