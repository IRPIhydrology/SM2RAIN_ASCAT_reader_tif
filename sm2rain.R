#SM2Rain ASCAT Extractor
#Dataset .tif format Avaiable at: https://zenodo.org/record/2671575#.XO_iSYhKhPY

#cleaning the environment, console and plots
rm(list=ls())
graphics.off()
dev.off()
cat("\f")


#in case tidyr package is activated, you must deactivate it
#detach("package:tidyr")


# load packages
library(raster)
library(rgdal)


# setting working directory
setwd("C:/Users/walte/Documents")


#Create list of SM2RAIN file paths
sm2rain_path <- "mnt" # name of the Folder containing all the .tif files
all_sm2rain <- list.files(sm2rain_path,
                            full.names = TRUE,
                            pattern = ".tif$")
sm2rain_stack <- stack(all_sm2rain)


#in case you wanna see details about the dataset

#crs(sm2rain_stack)
#extent(sm2rain_stack)
#res(sm2rain_stack)
#yres(sm2rain_stack)
#xres(sm2rain_stack)


# Coordinates of the station you want to extract: longitude, latitude
point <- cbind(-40.15, -6.31)


# Extraction of SM2RAIN-ASCAT rainfall from 2007 to 2018
sm2_extract <- extract(sm2rain_stack, point)
sm2_extract <- t(sm2_extract)
dates <- rownames(sm2_extract)
sm2 <- as.data.frame(sm2_extract)
sm2 <- sm2$V1
sm2 <- data.frame(dates, sm2)


# load packages: avoid conflicts
library(dplyr)
library(lubridate)
library(tidyr)


#Extraction of the dates from the files name
sm2 <- sm2 %>% separate(dates, c('string', 'date', 'scale'), sep = "_", remove = TRUE, convert = FALSE)
sm2 <- data.frame(date=sm2$date, rainfall = sm2$sm2)


#cleaning envronment variable
toKeep <- c("sm2", "point")
rm(list=setdiff(ls(), toKeep))


#formatting the dates
sm2$date <- ymd(sm2$date)


#Populating missing dates with NA
sm2 <- sm2 %>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(as.Date('2007-01-01'), as.Date('2018-12-31'), by="day"))

#Converting to Time Series
sm2_ts <- ts(sm2$rainfall, start=c(2007, 01, 01), frequency=365.25)

#Plotting

library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots

ggplot(sm2, aes(date, rainfall)) +
  geom_line(stat="identity", na.rm = TRUE, color="blue") +
  ggtitle(paste0("SM2RAIN-ASCAT lon;lat=", point[1], ";", point[2])) +
  xlab("") + ylab("rainfall [mm/day]") +
  scale_x_date(labels=date_format ("%Y"), breaks=date_breaks("1 year")) +
  theme(plot.title = element_text(lineheight=1, face="bold", size = 16)) +
  theme(text = element_text(size=12))





