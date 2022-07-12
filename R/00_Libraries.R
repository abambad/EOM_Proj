##############
# Libraries
##############
# CRAN packages
CRAN_package_list <- c("lubridate",
                       "maps",
                       "mapdata",
                       "dplyr",
                       "rgdal",
                       "wesanderson",
                       "viridis",
                       "ggplot2",
                       "dendextend",
                       "rpart")

not_in_list <- CRAN_package_list[!(CRAN_package_list %in% installed.packages())]
lapply(not_in_list,install.packages,dependencies=TRUE)

lapply(CRAN_package_list, require, character.only = TRUE)
       
# NON-CRAN packages
if("RGeostats" %in% installed.packages()) library(RGeostats)
if(!("RGeostats" %in% installed.packages())) cat("RGeostats must be loaded and installed from http://rgeostats.free.fr/ \n")


my.palette = wes_palette("Zissou1", 16, type = "continuous")

rm(CRAN_package_list,not_in_list)