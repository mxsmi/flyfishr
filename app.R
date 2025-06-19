## Shiny app primary document

library(devtools)

GH_MODELS_TOKEN = Sys.getenv("GH_MODELS_TOKEN")

devtools::load_all()
flyfishrApp(GH_MODELS_TOKEN)

