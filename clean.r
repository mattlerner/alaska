########## Setup ##########
library(dplyr)
library(foreign)
library(stats) # weighted.mean
library(spatstat) # weighted.median
library(tidyr)

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

########## State Crosswalk ##########

state_crosswalk <- read.csv('state_crosswalk.csv')

########## Raw datafiles ##########

##### Population #####

# State populations
population_lines <- readLines(filepath_population)

# Create population dataframe
population_raw <- data.frame(row.names=1:length(population_lines))

# Create variables one-by-one
population_raw$year <- as.numeric(substr(population_lines,1,4))
population_raw$state_code <- substr(population_lines,5,6)
population_raw$age <- as.numeric(substr(population_lines,17,18)) # this is a 01-18 factor!
population_raw$population <- as.numeric(substr(population_lines,19,26))

# collapase into states and years
population_states_years <- population_raw %>% group_by(state_code, year, age) %>% summarise(population = sum(population)) 

# age groups
population_states_years$age.group <- ""
population_states_years[population_states_years$age <= 04, "age.group"] <- "age.under.19"
population_states_years[population_states_years$age >= 05 & population_states_years$age <= 09, "age.group"] <- "age.20.to.44"
population_states_years[population_states_years$age >= 10 & population_states_years$age <= 13, "age.group"] <- "age.45.to.64"
population_states_years[population_states_years$age >= 14, "age.group"] <- "age.over.65"

# get total
population_states_years <- population_states_years %>% group_by(state_code, year) %>% mutate(total_population_year = sum(population))

# Population age/year shares
population_share_by_age <- population_states_years %>% group_by(state_code, year, age.group) %>% summarise(proportion = sum(population)/max(total_population_year))
state_age_distributions <- merge(spread(population_share_by_age, age.group, proportion), state_crosswalk, by.x="state_code", by.y="USPS") %>% mutate(state = State) %>% select(state, year, age.under.19, age.20.to.44, age.45.to.64, age.over.65)

# Create population annual totals
population_totals <- population_raw %>% group_by(state_code, year) %>% summarise(total_population = sum(population))

##### Mortality #####

# There are three compressed mortality files for three time periods. First we read them all in, raw.
mortality_raw_1968_1978 <- read.delim("mortality/Compressed Mortality, 1968-1978.txt", stringsAsFactors = FALSE) %>% select(Notes, State, Age.Group, Deaths, Population, Year)
mortality_raw_1979_1998 <- read.delim("mortality/Compressed Mortality, 1979-1998.txt", stringsAsFactors = FALSE) %>% select(Notes, State, Age.Group, Deaths, Population, Year)
mortality_raw_1999_2016 <- read.delim("mortality/Compressed Mortality, 1999-2016.txt", stringsAsFactors = FALSE) %>% select(Notes, State, Age.Group, Deaths, Population, Year)

# raw full file
# this has age groups -- should we get raw ages?
mortality_raw <- rbind(mortality_raw_1968_1978, mortality_raw_1979_1998, mortality_raw_1999_2016) %>% mutate(Population = as.numeric(Population))

# creating infant mortality
infant_mortality <- mortality_raw %>% subset(Age.Group == "< 1 year") %>% mutate(mortality_rate = Deaths / Population)
write.csv(infant_mortality, file="temp/infant_mortality.csv", row.names=FALSE)


########## Correlates of State Policy ##########

# aafdp: Alaska Dividend size
# atkin_index: Atkinson inequality index
# gini_coef: state income inequality
# cash_assist_percap: cash assistance per capita
# gsppcap: gross state product per capita
# 

correlates_raw <- read.csv("correlates/correlatesofstatepolicyprojectv2_1.csv")

# pare down
correlates <- correlates_raw[,c('Aedpi','gsptotal','gini_coef','real2_pc_inc_quar','unemploy_comp_percap','aapfdp','state','year','hsdiploma','welfare_spending_percap','pro_welfare','atkin_index','z_education_expenditures_per_pup','unemployment','apwpi')]

# write to csv
write.csv(correlates, file="temp/correlates.csv", row.names=FALSE)

########## ACS data ##########


########## CPS Person data ##########
# https://www.bls.gov/lau/notescps.htm
# https://cps.ipums.org/cps/sample_weights.shtml
# https://cps.ipums.org/cps-action/variables/EDUC#codes_section
# https://cps.ipums.org/cps-action/variables/EDUC#codes_section
# ASECWTH === household weight
# ASECWT === supplement weight (person)
# STATEFIP === State FIPS code. Alaska is 02.

# notes on interpreting codes
# 111 = bachelor's degree
# 110 = four years of college

# this is not the actual file!!!
cps_raw <- read.csv("cps/cps_00018.csv")

# NOTE: There is no Alaska-specific data between 1968 and 1977 (I think)

cps_temp <- cps_raw %>% group_by(STATEFIP) %>% mutate(min_year_data = min(YEAR))
cps_temp <- cps_temp %>% subset(YEAR >= 1977)# & YEAR <= 1992)

# get state names and rename variables
cps_statenames <- merge(cps_temp, state_crosswalk, by.x="STATEFIP", by.y="FIPS", suffixes=c("",""))
write.csv(cps_statenames, "temp/cps_statenames.csv")

# recoding
education_recode <- cps_statenames
names(education_recode)[names(education_recode) == 'YEAR'] <- 'year'
names(education_recode)[names(education_recode) == 'State'] <- 'state'
names(education_recode)[names(education_recode) == 'MONTH'] <- 'month'

# REMOVING NIU/blank
education_recode <- education_recode %>% subset(EDUC != 001)

# actual recode
# maybe recode according to cps instructions...?
education_recode$EDUC_RECODE <- plyr::mapvalues(education_recode$EDUC,from=c(0,1,2,10,11,12,13,14,20,21,22,30,31,32,40,50,60,70,71,72,73,80,81,90,91,92,100,110,111,120,121,122,123,124,125),to=c(0,0,0,1,1,1,1,1,2,2,2,3,3,3,4,5,6,7,7,7,7,8,8,8,9,9,10,11,11,12,12,13,14,15,16))
education_recode <- education_recode %>% mutate(under18 = (AGE < 18)) %>% group_by(state,year) %>% mutate(percentage_under_18 = weighted.mean(under18, ASECWT))
education_recode$ind_status_completion <- (education_recode$AGE >= 18 & education_recode$AGE <= 24 & education_recode$EDUC >= 7)
education_recode$any_college <- as.numeric(education_recode$EDUC_RECODE >= 8)
education_recode$hs_dropout <- as.numeric(education_recode$AGE >= 18 & education_recode$AGE <= 24 & education_recode$EDUC <= 72)
education_recode$hs_graduate <- as.numeric(education_recode$EDUC >= 73)
education_recode$migrator <- as.numeric(education_recode$MIGRATE1 == 5 | education_recode$MIGRATE1 == 6 | (education_recode$year < 1980 & education_recode$year > 1976 & education_recode$MIGRAT75 >= 50 & education_recode$MIGRAT75 < 90))
education_recode <- education_recode %>% group_by(state,year) %>% mutate(low_household_income = quantile(HHINCOME, 0.1, na.rm=TRUE))
education_recode <- education_recode %>% mutate(low_income = as.numeric(HHINCOME < low_household_income))

# education, period
education_timeseries_all <- education_recode %>% group_by(state, year) %>% summarise(mean_education_level = weighted.mean(EDUC_RECODE, ASECWT), mean_highest_grade = weighted.mean(HIGRADE, ASECWT), percentage_under_18 = max(percentage_under_18))
write.csv(education_timeseries_all, file="temp/education_timeseries_all.csv", row.names=FALSE)

# education, period (among the poor)
education_timeseries_poor <- education_recode %>% subset(POVERTY == 22 | POVERTY == 21 | POVERTY == 10) %>% group_by(state, year) %>% summarise(mean_education_level = weighted.mean(EDUC_RECODE, ASECWT), mean_highest_grade = weighted.mean(HIGRADE, ASECWT), percentage_under_18 = max(percentage_under_18))
write.csv(education_timeseries_poor, file="temp/education_timeseries_poor.csv", row.names=FALSE)

# education by state and year, youth
youth_education_timeseries <- education_recode %>% subset(AGE >= 18 & AGE <= 24) %>% group_by(state, year) %>% summarise(mean_education_level = weighted.mean(EDUC, ASECWT))
write.csv(youth_education_timeseries, file="temp/youth_education_timeseries.csv", row.names=FALSE)

# adult education by state and year
adult_education_timeseries <- education_recode %>% subset(AGE >= 20 & AGE <= 44) %>% group_by(state, year) %>% summarise(mean_education_level = weighted.mean(EDUC_RECODE, ASECWT), percentage_under_18 = max(percentage_under_18))
write.csv(adult_education_timeseries, file="temp/adult_education_timeseries.csv", row.names=FALSE)

# any college?
any_college_timeseries <- education_recode %>% subset(AGE >= 18 & AGE <= 24) %>% group_by(state, year) %>% summarise(mean_education_level = weighted.mean(any_college, ASECWT), percentage_under_18 = max(percentage_under_18))
write.csv(any_college_timeseries, file="temp/any_college_timeseries", row.names=FALSE)

# any college among the poor?
any_college_timeseries_poor <- education_recode %>% subset(POVERTY == 22 | POVERTY == 21 | POVERTY == 10) %>% group_by(state, year) %>% summarise(mean_education_level = weighted.mean(any_college, ASECWT), percentage_under_18 = max(percentage_under_18))
write.csv(any_college_timeseries_poor, file="temp/any_college_timeseries_poor", row.names=FALSE)

# total status dropout rate substitute
dropout_timeseries <- education_recode %>% group_by(state, year) %>% summarise(mean_education_level = weighted.mean(hs_dropout, ASECWT), percentage_under_18 = max(percentage_under_18))
write.csv(dropout_timeseries, file="temp/dropout_timeseries.csv", row.names=FALSE)

# status completion rate
status_completion_timeseries <- education_recode %>% group_by(state, year) %>% summarise(mean_education_level = sum((AGE >= 18 & AGE <= 24 & HIGRADE >= 150 & migrator == 0)*ASECWT) / sum((AGE >= 18 & AGE <= 24 & migrator == 0)*ASECWT))
write.csv(status_completion_timeseries, file="temp/status_completion_timeseries.csv", row.names=FALSE)

# status completion rate for people who aren't in the petroleum pipeline business
status_completion_timeseries_nopetro <- education_recode %>% group_by(state, year) %>% summarise(mean_education_level = sum((AGE >= 18 & AGE <= 24 & HIGRADE >= 150 & IND1950 != 567)*ASECWT) / sum((AGE >= 18 & AGE <= 24 & IND1950 != 567)*ASECWT))
write.csv(status_completion_timeseries_nopetro, file="temp/status_completion_timeseries_nopetro.csv", row.names=FALSE)


########## Other CPS-derived data ##########

# total in-migration by year and state
in_migration_totals <- education_recode %>% group_by(state, year) %>% summarise(percent_migrators = sum(migrator*ASECWT) / sum(ASECWT))
ggplot() + geom_line(aes(x = in_migration_totals[in_migration_totals$state != "Alaska","year"], y = in_migration_totals[in_migration_totals$state != "Alaska","percent_migrators"], group="state"), color="#d3d3d3") + geom_line(aes(x = in_migration_totals[in_migration_totals$state == "Alaska","year"], y = in_migration_totals[in_migration_totals$state == "Alaska","percent_migrators"]), color="#000000")

# total in-migration by year and state and industry
in_migration_totals_industry <- education_recode %>% group_by(state, year, IND1950) %>% summarise(percent_migrators = sum(migrator*ASECWT) / sum(ASECWT))
ggplot() + geom_line(aes(x = in_migration_totals[in_migration_totals$state != "Alaska","year"], y = in_migration_totals[in_migration_totals$state != "Alaska","percent_migrators"], group="state"), color="#d3d3d3") + geom_line(aes(x = in_migration_totals[in_migration_totals$state == "Alaska","year"], y = in_migration_totals[in_migration_totals$state == "Alaska","percent_migrators"]), color="#000000")


a <- in_migration_1981 %>% subset(year == 1981) %>% arrange(desc(percent_migrators))

########## Data for analysis - Full Education ##########

data_temp_1 <- merge(correlates, status_completion_timeseries_nopetro, by=c('state','year'))
data <- merge(data_temp_1, state_age_distributions, by=c('state', 'year'))

# remove non applicable regions and times
data <- data %>% subset(state != "District of Columbia")
data <- data %>% subset(year >= 1977)# & year <= 1992)

########## New variables ##########

data$gsp_total = data$gsptotal * 1000000
data$log_gsp = log(data$gsp_total)
data <- data %>% mutate(diff_log_gsp_1 = log_gsp - lag(log_gsp, 1), diff_log_gsp_2 = log_gsp - lag(log_gsp, 2))
data <- data %>% mutate(lag_under18_1 = lag(percentage_under_18, 1), lag_under18_2 = lag(percentage_under_18, 2))
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(unemp_lag1 = lag(unemployment, 1), unemp_lag2 = lag(unemployment, 2), unemp_lag5 = lag(unemployment, 5))
data$state <- as.character(data$state)
data$state_factor <- as.numeric(as.factor(data$state))

# lagged level of education
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(higrade_lag1 = lag(mean_highest_grade, 1), higrade_lag2 = lag(mean_highest_grade, 2))
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(educ_lag1 = lag(mean_education_level, 1), educ_lag2 = lag(mean_education_level, 2))
# data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(hs_lag1 = lag(mean_hs_graduation, 1), hs_lag2 = lag(mean_hs_graduation, 2))

# lagged welfare spending per capita
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(welfare_lag1 = lag(welfare_spending_percap, 1), welfare_lag2 = lag(welfare_spending_percap, 2))

# lagged spending per pupil
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(perpupil_lag1 = lag(z_education_expenditures_per_pup, 1), perpupil_lag2 = lag(z_education_expenditures_per_pup, 2), perpupil_lag3 = lag(z_education_expenditures_per_pup, 3))

write.csv(data, "temp/data_youth.csv")


########## Data for analysis - Adult Education ##########

data_temp_2 <- merge(correlates, adult_education_timeseries, by=c('state','year'))
data <- merge(data_temp_2, state_age_distributions, by=c('state', 'year'))

# remove non applicable regions and times
data <- data %>% subset(state != "District of Columbia")
data <- data %>% subset(year >= 1977)# & year <= 1992)

########## New variables ##########

data$gsp_total = data$gsptotal * 1000000
data$log_gsp = log(data$gsp_total)
data$log_unemp = log(data$unemployment)
data <- data %>% mutate(diff_log_gsp_1 = log_gsp - lag(log_gsp, 1), diff_log_gsp_2 = log_gsp - lag(log_gsp, 2))
data <- data %>% mutate(lag_under18_1 = lag(percentage_under_18, 1), lag_under18_2 = lag(percentage_under_18, 2))
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(unemp_lag1 = lag(unemployment, 1), unemp_lag2 = lag(unemployment, 2), unemp_lag3 = lag(unemployment, 3))
data$state <- as.character(data$state)
data$state_factor <- as.numeric(as.factor(data$state))

# lagged level of education
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(educ_lag1 = lag(mean_education_level, 1), educ_lag2 = lag(mean_education_level, 2))
# data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(hs_lag1 = lag(mean_hs_graduation, 1), hs_lag2 = lag(mean_hs_graduation, 2))

# lagged welfare spending per capita
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(welfare_lag1 = lag(welfare_spending_percap, 1), welfare_lag2 = lag(welfare_spending_percap, 2))

# lagged spending per pupil
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(perpupil_lag1 = lag(z_education_expenditures_per_pup, 1), perpupil_lag2 = lag(z_education_expenditures_per_pup, 2), perpupil_lag3 = lag(z_education_expenditures_per_pup, 3))

write.csv(data, "temp/data_adult.csv")

