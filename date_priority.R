

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
  raw_insects %>% select(Site.of.Sample, Date.of.Sample, IDer, Total.Lepidoptera, Plecoptera,Trichoptera,Ephemeroptera)

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
      "UV 2",
      "e"
    )
  )

unique(prti$Site.of.Sample)# none of the tags above should be in the new filter data

# fix problems with the name of the IDer spelling


prti$IDer = ifelse(prti$IDer %in% c("JESSE HOMZA", "Jesse HOMZA"),
                   "Jesse Homza",
                   prti$IDer)
prti$IDer = ifelse(prti$IDer %in% "Abigial Bower", "Abigail Bower", prti$IDer)
prti$IDer = ifelse(prti$IDer %in% "Greta Holliday ", "Greta Holliday", prti$IDer)

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

prti$Total.Lepidoptera<-as.numeric(prti$Total.Lepidoptera)
prti$Trichoptera<-as.numeric(prti$Trichoptera)
prti$Ephemeroptera<-as.numeric(prti$Ephemeroptera)
prti$Plecoptera<-as.numeric(prti$Plecoptera)


# add a col to indicate they have been processed


prti <- prti %>% mutate(prss = if_else(is.na(Total.Lepidoptera), 0, 1))


# sort the data by site date and week. 

prti<-prti %>% arrange(Site.of.Sample, Date.of.Sample, yrs)


write.csv(x = prti,"data/prioritysites.csv")




# Graphs

hist(prti$Total.Lepidoptera,breaks = 20)

# this didn't work


ggplot(prti,aes(x=IDer, y=Total.Lepidoptera))+
  geom_boxplot()


ggplot(prti,aes(IDer,Trichoptera))+
  geom_boxplot()+
  geom_point()

ggplot(prti,aes(IDer,Ephemeroptera))+
  geom_boxplot()+
  geom_point()

ggplot(prti,aes(IDer,Plecoptera))+
  geom_boxplot()+
  geom_point()






#Junk

# t1<-filter(prti, Date.of.Sample == "Aug 10 2021") # seems we don't need this anymore
# t2<-filter(prti, Date.of.Sample == "")
# t3<-filter(prti, Date.of.Sample == "8/5")


# this might be junk

# Identify rows with NA in the count column
na_rows <- prti %>% filter(is.na(Total.Lepidoptera))

# Extract the site and week columns from the NA rows
na_sites <- na_rows$Site.of.Sample
na_weeks <- na_rows$wk
na_date<-na_rows$Date.of.Sample

# Create a unique table of the NA sites and weeks
na_sites_weeks <- data.frame(site = na_sites, week = na_weeks, date=na_date) %>%
  distinct() # this removes duplicates

# Print the unique table of NA sites and weeks


na_sites_weeks <- na_sites_weeks %>% arrange(site, week)

