########## Setup ##########
library(dplyr)
library(foreign)
library(ggplot2)
library(Synth)
library(tidyr)

setwd("/users/matt/desktop/qmss/masters thesis/data")

########## Data insheet and merge ##########

data <- read.csv("temp/data_youth.csv")
data$state <- as.character(data$state) # sometimes reads in as a factor

########## Synth ##########

synth_data <- as.data.frame(data)

# create matrices from panel data that provide inputs for synth()

# Matt note: this function (dataprep()) does most of the work. Among other things, it yields
# a matrix with four columns. X1 and X0 are predictor values for the treated and control (respectively).
# Z1 and Z0 contain the ACTUAL values of the outcome variable during the pretreatment period ONLY
# Y1plot is a year*1 vector of outcome values for treatment unit. Y0plot is a year*numstates matrix
# of outcome values for the donor states.

all_predictors <- c("educ_lag1","educ_lag2","apwpi","log_gsp","unemployment")

dataprep.out <-
  dataprep(
    foo = synth_data,
    predictors = all_predictors,
    predictors.op = "mean",
    dependent = "mean_education_level",
    unit.variable = "state_factor",
    time.variable = "year",
    special.predictors = list(
      list("mean_education_level", 1981, "mean"),
      list("hsdiploma", 1980, "mean")
    ),
    treatment.identifier = 2,
    controls.identifier = c(1,3:50),#c(45, 51, 27, 44, 13), # Utah, Wyoming, Montana, Hawaii, and Idaho
    time.predictors.prior = c(1977:1981),
    time.optimize.ssr = c(1977:1981),
    unit.names.variable = "state",
    time.plot = 1977:1991
  )

## run the synth command to identify the weights
## that create the best possible synthetic 
## control unit for the treated.
synth.out <- synth(dataprep.out)

## there are two ways to summarize the results
## we can either access the output from synth.out directly
round(synth.out$solution.w,2)
# contains the unit weights or
synth.out$solution.v 
## contains the predictor weights. 

# Matt note: This yields gaps by comparing the outcome variable for the treated unit (across ALL years)
# with the value of the outcome variable for the synthetic control, which is calculated by multiplying
# the matrix dataprep.out$Y0plot by the vector of state weights, getting the weighted sums

# in order to get the correct weights on the correct donor units, we need to sort
dataprep.out.y0.cols.sorted <- dataprep.out$Y0plot[,order(colnames(dataprep.out$Y0plot))]
synth.out.solution.w.rows.sorted <- synth.out$solution.w[order(rownames(synth.out$solution.w))]
synthetic_control_values <- dataprep.out.y0.cols.sorted %*% synth.out.solution.w.rows.sorted
gaps <- dataprep.out$Y1plot - synthetic_control_values;
actual_synth_gaps <- data.frame(gap=c(gaps), year=row.names(gaps))
write.csv(actual_synth_gaps, 'temp/actual_synth_gaps.csv')

## also there are three convenience functions to summarize results.
## to get summary tables for all information 
## (V and W weights plus balance btw. 
## treated and synthetic control) use the 
## synth.tab() command
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)

## to get summary plots for outcome trajectories 
## of the treated and the synthetic control unit use the 
## path.plot() and the gaps.plot() commands

## plot in levels (treated and synthetic)
# Matt note: this plots the paths of synthetic and treatment across the whole timeframe
path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

## ggplot synthetic vs actual
ggplot() + geom_line(aes(x = as.numeric(row.names(dataprep.out$Y1plot)), y = dataprep.out$Y1plot)) + geom_line(aes(x = as.numeric(row.names(synthetic_control_values)), y = synthetic_control_values), linetype="dashed") + theme_bw() + theme(panel.border = element_blank()) + scale_x_continuous(breaks=seq(1977,1991,1)) + geom_vline(aes(xintercept=1982), linetype="dotted") + xlab("Year") + ylab("Status completion rate") + ylim(0.6,1)

## plot the gaps (treated - synthetic)
# Matt note: this plots gaps between treatment and synthetic (as before, solid line is treatment)
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)

# nonzero state weights
nonzero_state_weights <- data.frame(synth.tables$tab.w[synth.tables$tab.w$w.weights != 0,])

########## Estimated treatment effect ##########

# This is the average difference between the treatment unit and the synthetic control during
# the treatment period.


########## Placebo ##########

# Here we loop across all states, creating a synthetic control for each one and piling them
# all into a matrix

all_states <- unique(data$state_factor)
states_to_loop_through <- setdiff(all_states, c(2)) # all states except for Alaska
all_years <- (min(data$year)+2):max(data$year)
all_trials <- rep(NA, length(all_states) * length(all_years))
actual_synth_gaps <- read.csv('temp/actual_synth_gaps.csv')
treatment_gap = actual_synth_gaps$gap

synth_data <- data

# prep a vector for adding in gaps
all_trials = c()
for (i in 1:length(all_states)) {
  # for (year in all_years) {
  tryCatch({
    state <- all_states[i]
    control_states <- setdiff(all_states, c(state))
    dataprep.out<-
      dataprep(
        foo = synth_data,
        predictors = all_predictors,
        predictors.op = "mean",
        dependent = "mean_education_level",
        unit.variable = "state_factor",
        time.variable = "year",
        special.predictors = list(
          list("mean_education_level", 1977:1981, "mean"),
          list("hsdiploma", 1977, "mean")
        ),
        treatment.identifier = state,
        controls.identifier = control_states,
        time.predictors.prior = c(1977:1981),#(year-1)),
        time.optimize.ssr = c(1977:1981),#(year-1)),
        unit.names.variable = "state",
        time.plot = 1977:1991
      )
    
    synth.out <- synth(dataprep.out)
    
    dataprep.out.y0.cols.sorted <- dataprep.out$Y0plot[,order(colnames(dataprep.out$Y0plot))]
    synth.out.solution.w.rows.sorted <- synth.out$solution.w[order(rownames(synth.out$solution.w))]
    synthetic_control_values <- dataprep.out.y0.cols.sorted %*% synth.out.solution.w.rows.sorted
    gaps <- dataprep.out$Y1plot - synthetic_control_values;
    all_trials <- cbind(all_trials, gaps)
  }, error=function(e) { print(paste("ERROR:",state))})
  # }
}

########## Plot treatment gaps alongside placebos ##########

all_trials_frame <- data.frame(all_trials, year=row.names(all_trials))
all_trials_frame_long <- gather(all_trials_frame, region, gap, X1:X50)
all_trials_frame_long$year <- as.numeric(as.character(all_trials_frame_long$year))
write.csv(all_trials_frame_long, "temp/all_trials_frame_long.csv", row.names=FALSE)

ggplot() + geom_line(data=all_trials_frame_long, aes(x=year, y=gap, group=factor(region)), color="#d3d3d3", show.legend=FALSE) + geom_line(data=actual_synth_gaps, aes(x=year, y=gap), color="black") + geom_vline(xintercept = 1982, linetype="dotted") + ylab("Gap between synthetic and treated unit") + xlab("Year") + ggtitle("Placebo: Synthetic Alaska vs. Synthetic Other States")  + theme_bw() + theme(panel.border = element_blank())

########## RMSE ##########

# along full length of treatment
rmse_placebos <- sqrt(colMeans(all_trials*all_trials))
rmse_alaska <- sqrt(mean(actual_synth_gaps$gap*actual_synth_gaps$gap))
p_value <- sum(as.numeric(rmse_placebos >= rmse_alaska)) / length(rmse_placebos)

# in pre-treatment period
all_trials_pretreatment <- all_trials[row.names(all_trials) < 1982,]
alaska_gaps_pretreatment <- actual_synth_gaps %>% subset(year < 1982)
rmse_pretreatment_alaska <- sqrt(mean(alaska_gaps_pretreatment$gap*alaska_gaps_pretreatment$gap))

rmse_pretreatment <- sqrt(colMeans(all_trials_pretreatment*all_trials_pretreatment))
rmse_percentile <-sum(as.numeric(rmse_pretreatment <= rmse_pretreatment_alaska)) / length(rmse_pretreatment)

# Histogram of RMSE pretreatment here

########## Fit during pretreatment ##########

alaska_actual_pretreatment_values <- dataprep.out$Y1plot[row.names(dataprep.out$Y1plot) < 1982,]

# s.d. of Alaka's synthetic/treatment gap in pretreatment period
sd_alaska_gap_pretreatment <- sd(alaska_gaps_pretreatment$gap)
sd_alaska_actual_pretreatment <- sd(alaska_actual_pretreatment_values)

# doing the same thing for every state and giving the percentage in which year-on-year actual variation exceeds 
gap_less_than_variation <- c()
for (state in states_to_loop_through) {
  sd_state_gap_pretreatment <- sd(all_trials[row.names(all_trials)<1982,state])
  sd_state_actual_pretreatment <- sd(dataprep.out$Y0plot[row.names(dataprep.out$Y0plot) < 1982, state])
  gap_less_than_variation <- c(gap_less_than_variation,sd_state_gap_pretreatment<sd_state_actual_pretreatment)
}

# MEAN ABSOLUTE ERROR
mae_pretreatment <- colMeans(abs(all_trials[row.names(all_trials) < 1982,]))

# Histogram of MAE pretreatment here

########## Tables ##########

########## Graphs ##########

# Alaska vs other states on status completion
ggplot() + geom_line(data=synth_data[synth_data$state !="Alaska",], aes(x=year, y=mean_education_level, group=state), color="#d3d3d3", show.legend=FALSE) + geom_line(data=synth_data[synth_data$state=="Alaska",], aes(x=year, y=mean_education_level), color="black") + geom_vline(xintercept = 1982, linetype="dotted") + ylab("Status completion rate") + xlab("Year") + ggtitle("Status Completion: Alaska vs. Other States")  + theme_bw() + theme(panel.border = element_blank()) + xlim(1977,1991)



