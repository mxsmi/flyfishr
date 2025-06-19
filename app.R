## Shiny app primary document

library(devtools)

GH_MODELS_TOKEN = Sys.getenv("GH_MODELS_TOKEN")

print(GH_MODELS_TOKEN)

# devtools::load_all()
# More concise one-liner
sapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

flyfishrApp(GH_MODELS_TOKEN)

