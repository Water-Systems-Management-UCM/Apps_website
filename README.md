# Apps for website DWR and USDA NASS 

This repository contains the code and file for the Shiny web apps in the website 

Each folder contains an app: one .R script file and the .csv files needed to run them

Each .R can be edited to update the app (update the year for inflation adjustment, or edit the year that needss to be removed from the Ag comissioners data)

The AW.DWRdata.csv file contains the data from the original source DWR's land and water use estimates: https://water.ca.gov/Programs/Water-Use-And-Efficiency/Land-And-Water-Use/Agricultural-Land-And-Water-Use-Estimates

**To update the app only is necessary to put in the AW.DWRdata.csv file the latest data posted by DWR. Remember that this data set is at a county scale. Sometimes the data comes with tabs or spaces before and after the county names and crop names delete them before updating the file.**
