### R function that returns a prompt to give to the AI model to generate a fly-
### fishing report for the currently selected river

library(glue)

fishingReportPrompt <- function(river) {
  prompt <- glue("Generate a fly-fishing report for {river} for today based on 
                 today's weather conditions, water flows for today's date, and 
                 the time of year it is now. Include recommendations for specific 
                 flies and techniques. Write it using markdown")
  return(prompt)
}