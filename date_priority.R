

## This script was developed by Carlos Linares #
# We want to organize the data and prioritize the weeks that need to be ID.           

# Data: Collected during the Pioneer light project in Idaho 2020-2023
#                                                                        #
# Predictors: eventually we will have light treatment, weather, elevation, 
##############################################################################



#####clean workspace to improve efficiency: ###
rm(list = ls()) 


# libraries ----------------

library(tidyverse)
library(magrittr) #sometimes I use pipes 
library(lubridate)

# load data. 
# this is the data that the undergrads have produce while they habe been identifying insects.

raw_insects<-read.csv("data/Moth_abundance_pioneers - Sheet1.csv")

# lets make sure all sites are there. 
# PROBLEM: all the sites have different spellings, there are empty sites and sites that are not of interest like uv and IBO 

unique(raw_insects$Site.of.Sample)



# select columns and rows ------------- 
# here we select the columns and rows that we want to filter out.

prti <-
  raw_insects %>% select(Site.of.Sample, Date.of.Sample, IDer, Total.Lepidoptera)

prti <-
  prti %>% filter(
    !Site.of.Sample %in% c(
      "",
      "UV1",
      "UV1 ",
      "IBO-UV3",
      "ID-UV1",
      "ID-UV2",
      "UV",
      "IBO",
      "UV3",
      "UV5",
      "ID-UV4",
      "UVTest01",
      "UV2",
      "IBO- UV2",
      "Top of Iron mine UV6",
      "UV 2"
    )
  )

unique(prti$Site.of.Sample)# none of the tags above should be in the new filter data

# add week column-------------

# check dates are more or less the same format.

unique(prti$Date.of.Sample) # seems they do

# date as date

prti$Date.of.Sample<-mdy(prti$Date.of.Sample)

# add week

prti$wk<-Date.of.Sample<-week(prti$Date.of.Sample)

#add year

prti$yrs<-Date.of.Sample<-year(prti$Date.of.Sample)


# site column clean up --------------

unique(prti$Site.of.Sample)


# ad a col that indicates if it has been processed. 
# first we make total leps a number

prti$<-as.numeric(prti$Total.Lepidoptera)

# add a col to indicate they have been processed

if_else(condition = is.na(prti$Total.Lepidoptera),true =1, false = 0 ) # 

prti <- prti %>% mutate(prss = if_else(is.na(Total.Lepidoptera), 1, 0))

t <-
  prti %>% group_by(Site.of.Sample, wk, yrs) %>% summarize(unique_weel = unique(wk))



















#Junk

# t1<-filter(prti, Date.of.Sample == "Aug 10 2021") # seems we don't need this anymore
# t2<-filter(prti, Date.of.Sample == "")
# t3<-filter(prti, Date.of.Sample == "8/5")
