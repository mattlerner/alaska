########## Setup ##########
library(dplyr)
library(foreign)

setwd("/users/matt/desktop/qmss/masters thesis/data")

########## Functions ##########

# read and print first p lines of file
readlines <- function(filename, p) {
  connection <- file(filename, "r")
  first_line <- readLines(connection, n = p)
  close(connection)
  return(first_line)
}

########## Raw data filepaths ##########

filepath_population <- "population/us.1969_2017.19ages.adjusted.txt"


########## Raw datafiles ##########


##### Population #####

# State populations
population_lines <- readLines(filepath_population)

# Create population dataframe
population_raw <- data.frame(row.names=1:length(population_lines))

# Create variables one-by-one
population_raw$year <- as.numeric(substr(population_lines,1,4))
population_raw$state_code <- substr(population_lines,5,6)
population_raw$age <- as.factor(substr(population_lines,17,18)) # this is a 01-18 factor!
population_raw$population <- as.numeric(substr(population_lines,19,26))

# Create population annual totals
population_totals <- population_raw %>% group_by(state_code, year) %>% summarise(total_population = sum(population))

##### Mortality #####

# There are three compressed mortality files for three time periods. First we read them all in, raw.
mortality_raw_1968_1978 <- read.delim("mortality/Compressed Mortality, 1968-1978.txt") %>% select(Notes, State, Age.Group, Deaths, Population)
mortality_raw_1979_1998 <- read.delim("mortality/Compressed Mortality, 1979-1998.txt") %>% select(Notes, State, Age.Group, Deaths, Population)
mortality_raw_1999_2016 <- read.delim("mortality/Compressed Mortality, 1999-2016.txt") %>% select(Notes, State, Age.Group, Deaths, Population)

# raw full file
# this has age groups -- should we get raw ages?
mortality_raw <- rbind(mortality_raw_1968_1978, mortality_raw_1979_1998, mortality_raw_1999_2016)

##### ICPSR Education #####

load("icpsr/ds0001/34373-0001-Data.rda")
icpsr_raw <- da34373.0001
rm(da34373.0001)

icpsr <- icpsr_raw
icpsr$year <- as.numeric(substr(icpsr$YEAR,6,9))
icpsr$state <- sub("^.*\\) (?=[A-Za-z])", "", icpsr$STATE, perl=TRUE)

########## Clean: ##########