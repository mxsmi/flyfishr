## Shiny app primary document

library(devtools)

# devtools::load_all()
# More concise one-liner
sapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

flyfishrApp()

