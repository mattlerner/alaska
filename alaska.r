library(tidyverse)
library(gdata)

setwd("/users/mattathias/desktop/qmss/alaska")

# Mortality data:
# https://wonder.cdc.gov/controller/datarequest?stage=search&S__search1=mortality&S__logic1=or&S__search2=&S__logic2=or&S__search3=&action-Search=Search

# Education data:
# mean years of schooling
# https://www.census.gov/data/tables/time-series/demo/educational-attainment/cps-historical-time-series.html

# GNI (personal income)
# https://apps.bea.gov/regional/downloadzip.cfm

# inequality
# http://www.shsu.edu/eco_mwf/inequality.html

mortality_1 <- read.delim("mortality/Compressed Mortality, 1968-1978.txt") %>% select(Notes, State, Race, Age.Group, Deaths, Gender, Population, Crude.Rate.Lower.95..Confidence.Interval, Crude.Rate.Standard.Error, X..of.Total.Deaths, Crude.Rate.Upper.95..Confidence.Interval, Crude.Rate)
mortality_2 <- read.delim("mortality/Compressed Mortality, 1979-1998.txt") %>% select(Notes, State, Race, Age.Group, Deaths, Gender, Population, Crude.Rate.Lower.95..Confidence.Interval, Crude.Rate.Standard.Error, X..of.Total.Deaths, Crude.Rate.Upper.95..Confidence.Interval, Crude.Rate)
mortality_3 <- read.delim("mortality/Compressed Mortality, 1999-2016.txt") %>% select(Notes, State, Race, Age.Group, Deaths, Gender, Population, Crude.Rate.Lower.95..Confidence.Interval, Crude.Rate.Standard.Error, X..of.Total.Deaths, Crude.Rate.Upper.95..Confidence.Interval, Crude.Rate)
mortality <- rbind(mortality_1, mortality_2, mortality_3)

gni <- read.csv("gni/CA1/CA1_1969_2016__ALL_AREAS.csv")

inequality <- read.xls('inequality/Frank_Gini_2015.xls')



