# flyfishr R Package

## Install instructions:

This repository contains code and associated files for the flyfishr R package. 
To install the package into your R environment, run these commands:

1) `install.packages("devtools")`
2) `devtools::install_github("mxsmi/flyfishr")`
3) `library(flyfishr)`

## flyfishr app

The package contains a function to run the flyfishr app locally, which is called 
`flyfishrApp()`. A live version of the app hosted on Posit Connect Cloud can also 
be found here:

<https://01977bba-60c2-6207-1749-121b40cadb43.share.connect.posit.cloud/>

The flyfishr app is a data dashboard that allows you to select a state and a USGS
monitoring site for that state, then shows you information that is relevant to 
fly-fishing. Here is the information currently available on the app:

- Map: shows a map of the area surrounding the selected USGS monitoring site
- Flows & Temperature: shows graphs of current discharge levels and water temperature 
for the currently selected site. As well as mean daily values for both
- Fly-fishing Report: An AI generated fly-fishing report for the selected site based on current
water levels, water temperatures, and weather conditions

Future plans for the app include to add the following:

- Lake data
- Make it so that users can create an account and log in
- A "Fish Log" that allows users to record data about fish that they catch, including
uploading a photo. 
- A database of fly-fishing data collected from the app

