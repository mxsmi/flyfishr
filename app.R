## Shiny app primary document

library(devtools)

## Set API key
# if (file.exists("github_models_api_key.env")) {
#   readRenviron("github_models_api_key.env")
# }

devtools::load_all()
flyfishrApp()

