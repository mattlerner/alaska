########## Setup ##########
library(dplyr)
library(foreign)
library(stats) # weighted.mean
library(spatstat) # weighted.median
library(tidyr)
library(ggplot2)

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

data_temp_1 <- merge(correlates, status_completion_timeseries, by=c('state','year'))
data <- merge(data_temp_1, state_age_distributions, by=c('state', 'year'))

# remove non applicable regions and times
data <- data %>% subset(state != "District of Columbia")
data <- data %>% subset(year >= 1977)

########## New variables ##########

data$gsp_total = data$gsptotal * 1000000
data$log_gsp = log(data$gsp_total)
data <- data %>% mutate(diff_log_gsp_1 = log_gsp - lag(log_gsp, 1), diff_log_gsp_2 = log_gsp - lag(log_gsp, 2))
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(unemp_lag1 = lag(unemployment, 1), unemp_lag2 = lag(unemployment, 2), unemp_lag5 = lag(unemployment, 5))
data$state <- as.character(data$state)
data$state_factor <- as.numeric(as.factor(data$state))

# lagged level of education
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(educ_lag1 = lag(mean_education_level, 1), educ_lag2 = lag(mean_education_level, 2))

# lagged welfare spending per capita
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(welfare_lag1 = lag(welfare_spending_percap, 1), welfare_lag2 = lag(welfare_spending_percap, 2))

# lagged spending per pupil
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(perpupil_lag1 = lag(z_education_expenditures_per_pup, 1), perpupil_lag2 = lag(z_education_expenditures_per_pup, 2), perpupil_lag3 = lag(z_education_expenditures_per_pup, 3))

write.csv(data, "temp/data_youth.csv")


# get total
population_states_years <- population_states_years %>% group_by(state_code, year) %>% mutate(total_population_year = sum(population))
write.csv(population_states_years,'temp/population_states_years.csv')

# Population age/year shares
population_share_by_age <- population_states_years %>% group_by(state_code, year, age.group) %>% summarise(proportion = sum(population)/max(total_population_year))
state_age_distributions <- merge(spread(population_share_by_age, age.group, proportion), state_crosswalk, by.x="state_code", by.y="USPS") %>% mutate(state = State) %>% select(state, year, age.under.19, age.20.to.44, age.45.to.64, age.over.65)

# Create population annual totals
population_totals <- population_raw %>% group_by(state_code, year) %>% summarise(total_population = sum(population))

##### Mortality #####

# There are three compressed mortality files for three time periods. First we read them all in, raw.
#mortality_raw_1968_1978 <- read.delim("mortality/Compressed Mortality, 1968-1978.txt", stringsAsFactors = FALSE) %>% select(Notes, State, Age.Group, Deaths, Population, Year)
#mortality_raw_1979_1998 <- read.delim("mortality/Compressed Mortality, 1979-1998.txt", stringsAsFactors = FALSE) %>% select(Notes, State, Age.Group, Deaths, Population, Year)
#mortality_raw_1999_2016 <- read.delim("mortality/Compressed Mortality, 1999-2016.txt", stringsAsFactors = FALSE) %>% select(Notes, State, Age.Group, Deaths, Population, Year)

# raw full file
# this has age groups -- should we get raw ages?
#mortality_raw <- rbind(mortality_raw_1968_1978, mortality_raw_1979_1998, mortality_raw_1999_2016) %>% mutate(Population = as.numeric(Population))

# creating infant mortality
#infant_mortality <- mortality_raw %>% subset(Age.Group == "< 1 year") %>% mutate(mortality_rate = Deaths / Population)
#write.csv(infant_mortality, file="temp/infant_mortality.csv", row.names=FALSE)

########## Correlates of State Policy ##########

# aafdp: Alaska Dividend size
# atkin_index: Atkinson inequality index
# gini_coef: state income inequality
# cash_assist_percap: cash assistance per capita
# gsppcap: gross state product per capita
# 

correlates_raw <- read.csv("correlates/correlatesofstatepolicyprojectv2_1.csv")

# pare down
correlates <- correlates_raw[,c('Aedpi','gsptotal','gini_coef','real2_pc_inc_quar','unemploy_comp_percap','aapfdp','state','year','hsdiploma','welfare_spending_percap','pro_welfare','atkin_index','z_education_expenditures_per_pup','unemployment','apwpi','pc_inc_ann')]

# write to csv
write.csv(correlates, file="temp/correlates.csv", row.names=FALSE)

########## ACS data ##########

acs_raw <- read.csv("acs/usa_00002.csv")

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
cps_raw <- read.csv("cps/cps_00020.csv")

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
#education_recode <- education_recode %>% subset(EDUC != 001)

# actual recode
# maybe recode according to cps instructions...?
education_recode$EDUC_RECODE <- plyr::mapvalues(education_recode$EDUC,from=c(0,1,2,10,11,12,13,14,20,21,22,30,31,32,40,50,60,70,71,72,73,80,81,90,91,92,100,110,111,120,121,122,123,124,125),to=c(0,0,0,1,1,1,1,1,2,2,2,3,3,3,4,5,6,7,7,7,7,8,8,8,9,9,10,11,11,12,12,13,14,15,16))
education_recode <- education_recode %>% mutate(under18 = (AGE < 18)) %>% group_by(state,year) %>% mutate(percentage_under_18 = weighted.mean(under18, ASECWT))
education_recode$ind_status_completion <- (education_recode$HIGRADE >= 150 & education_recode$HIGRADE != 999 & !is.na(education_recode$HIGRADE))
education_recode$ind_eleventh_grade <- (education_recode$HIGRADE >= 131 & education_recode$HIGRADE != 999 & !is.na(education_recode$HIGRADE))
education_recode$HHINCOME.10k <- education_recode$HHINCOME/10000
education_recode$any_college <- as.numeric(education_recode$HIGRADE >= 151)
education_recode$age.18.24 <- as.numeric(education_recode$AGE >= 18 & education_recode$AGE <= 24 & !is.na(education_recode$AGE))
education_recode$hs_dropout <- as.numeric(education_recode$AGE >= 18 & education_recode$AGE <= 24 & education_recode$EDUC <= 72)
education_recode$hs_graduate <- as.numeric(education_recode$EDUC >= 73)
education_recode$migrator_75 <- as.numeric(education_recode$MIGRAT75 >= 50 & education_recode$MIGRAT75 <= 90 & !is.na(education_recode$MIGRAT75))
education_recode$migrator_1year <- as.numeric(education_recode$MIGRATE1 == 5 | education_recode$MIGRATE1 == 6)

education_recode$migrator <- plyr::mapvalues(as.numeric(education_recode$MIGRATE1 == 5 | education_recode$MIGRATE1 == 6 | (education_recode$year < 1980 & education_recode$year > 1976 & education_recode$MIGRAT75 >= 50 & education_recode$MIGRAT75 < 90)),NA,0)
education_recode <- education_recode %>% group_by(state,year) %>% mutate(low_household_income = quantile(HHINCOME, 0.1, na.rm=TRUE))
education_recode <- education_recode %>% mutate(low_income = as.numeric(HHINCOME < low_household_income))
write.csv(education_recode, file="temp/education_recode.csv", row.names=FALSE)

#####
# Get out-migrants by state -- this is the percent of all 18-24-year-old migrants nationwide
# that hail from a given state in that year
pop.estimates.18.24 <- education_recode %>% group_by(state, year) %>% summarise(pop.18.24 = sum(age.18.24*ASECWT), MIGSTA1=max(STATEFIP))
outmigration_18.24 <- education_recode %>% subset(AGE >= 18 & AGE <= 24) %>% group_by(MIGSTA1, year) %>% summarise(migrators.18.24 = sum(migrator*ASECWT))
outmigration_percentage <- merge(outmigration_18.24, pop.estimates.18.24, by=c('MIGSTA1','year')) %>% mutate(outmigrators.pct.18.24 = migrators.18.24/pop.18.24) %>% select(state, year, outmigrators.pct.18.24)

#####

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

# eleventh grade?
eleventh_grade_timeseries <- education_recode %>% subset(AGE >= 16 & AGE <= 18) %>% group_by(state, year) %>% summarise(mean_education_level = weighted.mean(ind_eleventh_grade,ASECWT))
write.csv(eleventh_grade_timeseries, file="temp/eleventh_grade_timeseries.csv", row.names=FALSE)

# status completion rate only for 18-19 year olds
status_completion_timeseries_18.19 <- education_recode %>% group_by(state, year) %>% summarise(mean_education_level = sum((AGE >= 18 & AGE <= 19 & HIGRADE >= 150 & !is.na(AGE) & !is.na(HIGRADE))*ASECWT) / sum((AGE >= 18 & AGE <= 19 & !is.na(AGE) & !is.na(HIGRADE))*ASECWT))
write.csv(status_completion_timeseries_18.19, file="temp/status_completion_timeseries_1819.csv", row.names=FALSE)

# status completion rate for those without any college (btwn 18 and 24)
status_completion_timeseries_nocollege <- education_recode %>% group_by(state, year) %>% summarise(mean_education_level = sum((AGE >= 18 & AGE <= 24 & any_college == 0 & HIGRADE >= 150)*ASECWT) / sum((AGE >= 18 & AGE <= 24 & any_college == 0)*ASECWT))
write.csv(status_completion_timeseries_nocollege, file="temp/status_completion_timeseries_nocollege.csv", row.names=FALSE)

########## Main CPS-derived dataset! ##########

# status completion rate and a bunch of other junk
status_completion_timeseries <- education_recode %>% group_by(state, year) %>% mutate(
  population = sum(ASECWT),
  population.above.24 = sum((AGE >= 24 & !is.na(AGE)) * ASECWT),
  percent_college = (sum((AGE >= 24 & !is.na(AGE) & HIGRADE >= 190)*ASECWT)/population.above.24)) %>% subset(AGE >= 18 & AGE <= 24) %>% group_by(state, year) %>% summarise(
    mean_education_level = weighted.mean(ind_status_completion,ASECWT),
    migrants.18.to.24 = weighted.mean(migrator_1year, ASECWT),
    migrants.since75 = weighted.mean(migrator_75, ASECWT),
    hh.income.18.to.24 = weighted.mean(HHINCOME, ASECWT),
    population = max(population),
    population.above.24 = max(population.above.24),
    percent_college = max(percent_college))
write.csv(status_completion_timeseries, file="temp/status_completion_timeseries.csv", row.names=FALSE)

########## Data for analysis - Full Education ##########

data_temp_1 <- merge(correlates, status_completion_timeseries, by=c('state','year'))
data_temp_2 <- merge(data_temp_1, outmigration_percentage, by=c('state','year'), all.x=TRUE)
data <- merge(data_temp_2, state_age_distributions, by=c('state', 'year'), all.x=TRUE)

# remove non applicable regions and times
data <- data %>% subset(state != "District of Columbia")
data <- data %>% subset(year >= 1977)

########## New variables ##########

data$gsp_total = data$gsptotal * 1000000
data$log_gsp = log(data$gsp_total)
data$gdp_percap = data$gsp_total/data$population
data$log_gdp_percap = log(data$gdp_percap)


data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(unemp_lag1 = lag(unemployment, 1), unemp_lag2 = lag(unemployment, 2), unemp_lag5 = lag(unemployment, 5))

data$state <- as.character(data$state)
data$state_factor <- as.numeric(as.factor(data$state))

# pct change gdp
data <- data%>% arrange(state, year) %>% group_by(state) %>% mutate(pct_change_gdp = (gsp_total - lag(gsp_total, 1))/lag(gsp_total, 1))

# lagged level of education
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(educ_lag1 = lag(mean_education_level, 1), educ_lag2 = lag(mean_education_level, 2))

# lagged welfare spending per capita
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(welfare_lag1 = lag(welfare_spending_percap, 1), welfare_lag2 = lag(welfare_spending_percap, 2))

# lagged spending per pupil
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(perpupil_lag1 = lag(z_education_expenditures_per_pup, 1), perpupil_lag2 = lag(z_education_expenditures_per_pup, 2), perpupil_lag3 = lag(z_education_expenditures_per_pup, 3))

# lagged migrants 18-24
data <- data %>% arrange(state, year) %>% group_by(state) %>% mutate(delta.migrants.18.to.24 = (migrants.18.to.24 - lag(migrants.18.to.24, 1))/lag(migrants.18.to.24, 1))


write.csv(data, "temp/data_youth.csv")




########## Other data for thesis ##########

###### Welders education
#education of welders in 1977 -- OCC1950 = 685
welders_completers_1977 <- cps_raw %>% subset(YEAR == 1977 & AGE >= 18 & OCC1950 == 685 & !is.na(HIGRADE)) %>% mutate(status_completer = (HIGRADE >= 150 & !is.na(HIGRADE)))
weighted.mean(welders_completers_1977$status_completer, welders_completers_1977$ASECWT)

# welders ages in 1977
hist(welders_completers_1977$AGE)

all_people_1977 <- cps_raw %>% subset(YEAR == 1977 & AGE >= 18 & !is.na(HIGRADE)) %>% mutate(status_completer = (HIGRADE >= 150 & !is.na(HIGRADE)))
weighted.mean(all_people_1977$status_completer, all_people_1977$ASECWT)

alaskans_1977 <- cps_raw %>% subset(STATEFIP == 2 & AGE >= 18 & !is.na(HIGRADE)) %>% mutate(status_completer = (HIGRADE >= 150 & !is.na(HIGRADE)))
weighted.mean(alaskans_1977$status_completer, alaskans_1977$ASECWT)


########## Other CPS-derived data ##########

# total in-migration by year and state
cps_raw$migrator <- (cps_raw$MIGRATE1 == 5 | cps_raw$MIGRATE1 == 6)
cps_raw$status_completer <- (cps_raw$AGE >= 18 & cps_raw$AGE <= 24 & cps_raw$HIGRADE >= 150 & cps_raw$HIGRADE != 999 & !is.na(cps_raw$HIGRADE))
cps_raw$age.18.24 <- (cps_raw$AGE >= 18 & cps_raw$AGE <= 24)
in_migration_totals <- cps_raw %>% group_by(STATEFIP, YEAR) %>% summarise(total_in_migrators = sum(migrator * ASECWT))

# out-migration totals
out_migration_totals <- cps_raw %>% group_by(MIGSTA1, YEAR) %>% summarise(total_out_migrators = sum(migrator * ASECWT))

# all migration
all_migration <- merge(in_migration_totals, out_migration_totals, by=c('STATEFIP','YEAR')) %>% select(-MIGSTA1)

# total in-migration by year and state and industry -- top 10
in_migration_totals_industry <- education_recode %>% group_by(state, year) %>% mutate(n_migrators = sum((AGE >= 18 & AGE <= 24 & migrator)*ASECWT)) %>% group_by(state, year, IND1950) %>% summarise(percent_migrators = sum((AGE >= 18 & AGE <= 24 & migrator)*ASECWT) / max(n_migrators)) %>% subset(percent_migrators != 0) %>% group_by(state, year) %>% arrange(desc(percent_migrators)) %>% top_n(10) %>% arrange(state, year, desc(percent_migrators))

# total in-migration in 1981 in descending order
in_migration_1981 <- in_migration_totals %>% subset(year == 1981) %>% arrange(desc(percent_migrators))

# educational attainment migrants vs nonmigrants by state and year
education_by_migration <- education_recode %>% group_by(state, year, migrator) %>% summarise(mean_education_level = weighted.mean(HIGRADE, ASECWT), number=n())

# average household income of non completers vs completers
status_completion_income <-  education_recode %>% subset(AGE >= 18 & AGE <= 24) %>% group_by(state, year, ind_status_completion) %>% summarise(avg.completer.income = weighted.mean(HHINCOME, ASECWT)) %>% arrange(state, year)


###### ACS: Alaskans educational attainment
# ACS, 2001-2005
mavg <- function(x,n){stats::filter(x,rep(1/n,n), sides=2)}
acs_raw$status_completers <- as.numeric(acs_raw$EDUC >= 6 & !is.na(acs_raw$EDUC))
acs_raw$attended_college <- as.numeric(acs_raw$EDUC >= 7 & !is.na(acs_raw$EDUC))
acs_raw$birth_year <- acs_raw$YEAR - acs_raw$AGE
acs_raw$year_at_age_18 <- acs_raw$birth_year + 18

status_completion_by_age_18 <- acs_raw %>% subset(year_at_age_18 >= 1960 & year_at_age_18 <= 1990) %>% dplyr::group_by(STATEFIP, year_at_age_18) %>% summarise(status_completion_rate = weighted.mean(status_completers,PERWT))
college_by_age_18 <- acs_raw %>% subset(year_at_age_18 >= 1960 & year_at_age_18 <= 1990) %>% dplyr::group_by(STATEFIP, year_at_age_18) %>% summarise(college_rate = weighted.mean(attended_college,PERWT))

hs_alaska <- status_completion_by_age_18[status_completion_by_age_18$STATEFIP == 2,]
hs_nonalaska <- status_completion_by_age_18[status_completion_by_age_18$STATEFIP != 2,]

college_alaska <- college_by_age_18[college_by_age_18$STATEFIP == 2,]
college_nonalaska <- college_by_age_18[college_by_age_18$STATEFIP != 2,]

alaska_college_1940.to.1980$mavg_3yr <- mavg(alaska_college_1940.to.1980$college_rate, 3)
alaska_college_1940.to.1980$mavg_5yr <- mavg(alaska_college_1940.to.1980$college_rate, 5)

# high school
ggplot() + geom_line(data=hs_nonalaska, aes(x=year_at_age_18, y=status_completion_rate, group=factor(STATEFIP)), color="#d3d3d3", show.legend=FALSE) + geom_line(data=hs_alaska, aes(x=year_at_age_18, y=status_completion_rate), color="black") + geom_vline(xintercept = 1982, linetype="dotted") + geom_hline(aes(yintercept=0), linetype="dashed") + ylab("% with some college education") + ylim(0.7, 1) + xlab("Year at age 18") + ggtitle("Figure Z: blah")  + theme_bw() + theme(panel.border = element_blank()) + theme(plot.title = element_text(hjust = 0.5), text=element_text(family="CMU Serif", color="#3c3c3c"))

# college
ggplot() + geom_line(data=college_nonalaska, aes(x=year_at_age_18, y=college_rate, group=factor(STATEFIP)), color="#d3d3d3", show.legend=FALSE) + geom_line(data=college_alaska, aes(x=year_at_age_18, y=college_rate), color="black") + geom_vline(xintercept = 1982, linetype="dotted") + geom_hline(aes(yintercept=0), linetype="dashed") + ylab("% with some college education") + xlab("Year at age 18") + ggtitle("Figure Z: blah")  + theme_bw() + theme(panel.border = element_blank()) + theme(plot.title = element_text(hjust = 0.5), text=element_text(family="CMU Serif", color="#3c3c3c"))

###### CPS: In-migration and out-migration by state

