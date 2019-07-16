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
state_crosswalk <- read.csv('state_crosswalk.csv')

########## Synth ##########

synth_data <- as.data.frame(data)

# create matrices from panel data that provide inputs for synth()

# Matt note: this function (dataprep()) does most of the work. Among other things, it yields
# a matrix with four columns. X1 and X0 are predictor values for the treated and control (respectively).
# Z1 and Z0 contain the ACTUAL values of the outcome variable during the pretreatment period ONLY
# Y1plot is a year*1 vector of outcome values for treatment unit. Y0plot is a year*numstates matrix
# of outcome values for the donor states.

all_predictors <- c("log_gsp","unemployment","percent_college","gdp_percap")

dataprep.out.original <-
  dataprep(
    foo = synth_data,
    predictors = all_predictors,
    predictors.op = "mean",
    dependent = "mean_education_level",
    unit.variable = "state_factor",
    time.variable = "year",
    special.predictors = list(
      list("mean_education_level", 1977, "mean"),
      list("mean_education_level", 1981, "mean")
      ),
    treatment.identifier = 2,
    controls.identifier = c(1,3:50),
    time.predictors.prior = c(1977:1981),
    time.optimize.ssr = c(1977:1981),
    unit.names.variable = "state",
    time.plot = 1977:1991
  )

## run the synth command to identify the weights
## that create the best possible synthetic 
## control unit for the treated.
synth.out.original <- synth(dataprep.out.original)

## there are two ways to summarize the results
## we can either access the output from synth.out directly
round(synth.out.original$solution.w,2)
# contains the unit weights or
synth.out.original$solution.v 
## contains the predictor weights. 

# Matt note: This yields gaps by comparing the outcome variable for the treated unit (across ALL years)
# with the value of the outcome variable for the synthetic control, which is calculated by multiplying
# the matrix dataprep.out$Y0plot by the vector of state weights, getting the weighted sums

# in order to get the correct weights on the correct donor units, we need to sort
dataprep.out.y0.cols.sorted.original <- dataprep.out.original$Y0plot[,order(colnames(dataprep.out.original$Y0plot))]
synth.out.solution.w.rows.sorted.original <- synth.out.original$solution.w[order(rownames(synth.out.original$solution.w))]
synthetic_control_values <- dataprep.out.y0.cols.sorted.original %*% synth.out.solution.w.rows.sorted.original
gaps <- dataprep.out.original$Y1plot - synthetic_control_values;
actual_synth_gaps <- data.frame(gap=c(gaps), year=as.numeric(row.names(gaps)))
write.csv(actual_synth_gaps, 'temp/actual_synth_gaps.csv')

## also there are three convenience functions to summarize results.
## to get summary tables for all information 
## (V and W weights plus balance btw. 
## treated and synthetic control) use the 
## synth.tab() command
synth.tables.original <- synth.tab(
  dataprep.res = dataprep.out.original,
  synth.res = synth.out.original)
print(synth.tables.original)

## to get summary plots for outcome trajectories 
## of the treated and the synthetic control unit use the 
## path.plot() and the gaps.plot() commands

## plot in levels (treated and synthetic)
# Matt note: this plots the paths of synthetic and treatment across the whole timeframe
path.plot(dataprep.res = dataprep.out.original,synth.res = synth.out.original)

## ggplot synthetic vs actual
ggplot() + geom_line(aes(x = as.numeric(row.names(dataprep.out.original$Y1plot)), y = dataprep.out.original$Y1plot)) + geom_line(aes(x = as.numeric(row.names(synthetic_control_values)), y = synthetic_control_values), linetype="dashed") + theme_bw() + theme(panel.border = element_blank()) + scale_x_continuous(breaks=seq(1977,1991,1)) + geom_vline(aes(xintercept=1982), linetype="dotted") + xlab("Year") + ylab("Status completion rate") + ylim(0.6,1)


######


## plot the gaps (treated - synthetic)
# Matt note: this plots gaps between treatment and synthetic (as before, solid line is treatment)
gaps.plot(dataprep.res = dataprep.out.original,synth.res = synth.out.original)

# ggplot gaps
ggplot() + geom_line(aes(x = year, y = gap), data=actual_synth_gaps) + theme_bw() + theme(panel.border = element_blank()) + scale_x_continuous(breaks=seq(1977,1991,1)) + geom_vline(aes(xintercept=1982), linetype="dotted") + geom_hline(aes(yintercept=0), linetype="dashed") + xlab("Year") + ylab("Synthetic-Treatment gap") + ylim(-0.05,0.05)

# nonzero state weights
nonzero_state_weights <- data.frame(synth.tables.original$tab.w[synth.tables.original$tab.w$w.weights != 0,])

# VARIABLE WEIGHTS HERE

# ggplot of actual donor states vs actual alaska
donor_factors <- as.numeric(row.names(nonzero_state_weights))
synth_donors <- synth_data %>% subset(state_factor %in% donor_factors & year < 1992)
alaska_only <- synth_data %>% subset(state_factor == 2  & year < 1992)
ggplot() + geom_line(aes(y=mean_education_level, x=year, group=state), color="#d3d3d3", data=synth_donors) + geom_line(aes(y=mean_education_level, x=year), data=alaska_only) + theme_bw() + theme(panel.border = element_blank()) + scale_x_continuous(breaks=seq(1977,1991,1))+ xlab("Year") + ylab("Status Completion Rate") + ylim(0.6,1)

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
#actual_synth_gaps <- read.csv('temp/actual_synth_gaps.csv')
treatment_gap = actual_synth_gaps$gap

synth_data <- data

# prep a vector for adding in gaps
all_trials = c()
for (i in 1:length(states_to_loop_through)) {
  # for (year in all_years) {
  tryCatch({
    state <- states_to_loop_through[i]
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
          list("mean_education_level", 1977, "mean"),
          list("mean_education_level", 1981, "mean")
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
    synthetic_control_values_placebo <- dataprep.out.y0.cols.sorted %*% synth.out.solution.w.rows.sorted
    gaps <- dataprep.out$Y1plot - synthetic_control_values_placebo;
    all_trials <- cbind(all_trials, gaps)
    colnames(all_trials)[ncol(all_trials)] <- state # set column name
  }, error=function(e) { print(paste("ERROR:",state))})
  # }
}

save(all_trials, file="all_trials.rdata")


########## Plot treatment gaps alongside placebos ##########

all_trials_frame <- data.frame(all_trials, year=row.names(all_trials))
all_trials_frame_long <- gather(all_trials_frame, region, gap, X1:X49)
all_trials_frame_long$year <- as.numeric(as.character(all_trials_frame_long$year))
write.csv(all_trials_frame_long, "temp/all_trials_frame_long.csv", row.names=FALSE)

ggplot() + geom_line(data=all_trials_frame_long, aes(x=year, y=gap, group=factor(region)), color="#d3d3d3", show.legend=FALSE) + geom_line(data=actual_synth_gaps, aes(x=year, y=gap), color="black") + geom_vline(xintercept = 1982, linetype="dotted") + geom_hline(aes(yintercept=0), linetype="dashed") + ylab("Gap between synthetic and treated unit") + xlab("Year") + ggtitle("Placebo: Synthetic Alaska vs. Synthetic Other States")  + theme_bw() + theme(panel.border = element_blank())

########## RMSE ##########

# in post-treatment period
all_trials_posttreatment <- all_trials[row.names(all_trials) >= 1982,]
alaska_gaps_posttreatment <- actual_synth_gaps %>% subset(as.numeric(year) >= 1982)
rmse_placebos <- sqrt(colMeans(all_trials_posttreatment*all_trials_posttreatment))
rmse_alaska <- sqrt(mean(alaska_gaps_posttreatment$gap*alaska_gaps_posttreatment$gap))
p_value <- sum(as.numeric(rmse_placebos >= rmse_alaska)) / length(rmse_placebos)

# in pre-treatment period
all_trials_pretreatment <- all_trials[row.names(all_trials) < 1982,]
alaska_gaps_pretreatment <- actual_synth_gaps %>% subset(year < 1982)
rmse_pretreatment_alaska <- sqrt(mean(alaska_gaps_pretreatment$gap*alaska_gaps_pretreatment$gap))

rmse_pretreatment <- sqrt(colMeans(all_trials_pretreatment*all_trials_pretreatment))
rmse_percentile <-sum(as.numeric(rmse_pretreatment <= rmse_pretreatment_alaska)) / length(rmse_pretreatment)

# Histogram of RMSE pretreatment here
ggplot() + geom_histogram(aes(x=rmse_pretreatment), fill="#d3d3d3", bins=10) + geom_vline(xintercept = rmse_pretreatment_alaska, linetype="dotted") + theme_bw() + theme(panel.border = element_blank()) + xlab("Pretreatment RMSE") + ylab("Number of Controls") + ggtitle("Figure Blah")  + theme_bw() + theme(panel.border = element_blank()) + theme(plot.title = element_text(hjust = 0.5), text=element_text(family="CMU Serif", color="#3c3c3c"))

########## Fit during pretreatment ##########

alaska_actual_pretreatment_values <- dataprep.out$Y1plot[row.names(dataprep.out$Y1plot) < 1982,]

# s.d. of Alaka's synthetic/treatment gap in pretreatment period
sd_alaska_gap_pretreatment <- sd(alaska_gaps_pretreatment$gap)
sd_alaska_actual_pretreatment <- sd(alaska_actual_pretreatment_values)

# doing the same thing for every state and giving the percentage in which year-on-year actual variation exceeds 
sd_ratio <- c()
for (state in states_to_loop_through) {
  print(as.character(state))
  sd_state_gap_pretreatment <- sd(all_trials[row.names(all_trials) < 1982,as.character(state)])
  sd_state_actual_pretreatment <- sd(dataprep.out.original$Y0plot[row.names(dataprep.out.original$Y0plot) < 1982, as.character(state)])
  sd_ratio <- c(sd_ratio,sd_state_gap_pretreatment/sd_state_actual_pretreatment)
}

mae_pretreatment_alaska <- abs(mean(alaska_gaps_pretreatment$gap))

# MEAN ABSOLUTE ERROR
mae_pretreatment <- colMeans(abs(all_trials[row.names(all_trials) < 1982,]))

# Histogram of MAE pretreatment here
ggplot() + geom_histogram(aes(x=mae_pretreatment), fill="#d3d3d3") + geom_vline(xintercept = rmse_pretreatment_alaska, linetype="dotted") + theme_bw() + theme(panel.border = element_blank()) + xlab("Pretreatment MAE") + ylab("# of placebos")

# Histogram of s.d.(gap) / s.d. (actual)
ggplot() + geom_histogram(aes(x=sd_ratio), fill="#d3d3d3") + geom_vline(xintercept = sd_alaska_gap_pretreatment/sd_alaska_actual_pretreatment, linetype="dotted") + theme_bw() + theme(panel.border = element_blank()) + xlab("Ratio") + ylab("# of placebos")

########## S.D. Ratios: Pretreatment RMSE vs pretreatment SD ##########

# pretreatment sd for all units
pretreatment_placebo_sd <- apply(dataprep.out.original$Z0, 2, sd)
placebo_sd_ratio <- rmse_pretreatment / pretreatment_placebo_sd

# for alaska
pretreatment_alaska_sd <- sd(alaska_actual_pretreatment_values)
alaska_sd_ratio <- rmse_pretreatment_alaska/pretreatment_alaska_sd

# sd histogram
sd_plot_1 <- ggplot() + geom_histogram(aes(x=placebo_sd_ratio), fill="#d3d3d3", bins=10) + geom_vline(xintercept = alaska_sd_ratio, linetype="dotted") + theme_bw() + theme(panel.border = element_blank()) + xlab("Ratio of pretreatment RMSE to pretreatment standard deviation") + ylab("# of placebos") + ggtitle("Figure Blah")  + theme_bw() + theme(panel.border = element_blank()) + theme(plot.title = element_text(hjust = 0.5), text=element_text(family="CMU Serif", color="#3c3c3c"))
plot(sd_plot_1)

########## Pre/post RMSE ratios by region ##########

# combining
alaska_gap_frame <- data.frame(rmse_ratio = rmse_alaska / rmse_pretreatment_alaska, State="Alaska")
rmse_placebo_ratios_frame <- data.frame(rmse_ratio = rmse_placebos / rmse_pretreatment, FIPS = names(rmse_pretreatment))
rmse_ratios_w_states <- merge(rmse_placebo_ratios_frame, state_crosswalk, by="FIPS") %>% select(State, rmse_ratio)

# plot!
ggplot() + geom_dotplot(data = rmse_ratios_w_states, aes(x=rmse_ratio, y=reorder(State,desc(State))), binaxis='y', dotsize=0.5, fill="#ffffff") + geom_dotplot(data = alaska_gap_frame, aes(x=rmse_ratio, y=State), binaxis='y', dotsize=0.5, fill="#000000") + theme_bw() + theme(panel.border = element_blank()) + xlab("Ratio: Post- / Pre-treatment RMSE") + ylab("") + ggtitle("Figure Blah")  + theme_bw() + theme(panel.border = element_blank()) + theme(plot.title = element_text(hjust = 0.5), text=element_text(family="CMU Serif", color="#3c3c3c"))

########## Data for full version ##########

# Put all the tables we need for visualization and graphics into one big .rdata file

########## Tables -- Out ##########

# nonzero state weights
nonzero_state_weights <- data.frame(synth.tables.original$tab.w[synth.tables.original$tab.w$w.weights != 0,])

# variable weights here
variable_weights <- synth.tables.original$tab.w
save(variable_weights, file="variable_weights.rdata")

# values of predictor variables in pretreatment period
pretreatment_predictor_values <- synth.tables.original$tab.pred

# in-migration in 1981 with education level of migrators and non-migrators (according to CPS)

########## Graphs -- Out ##########

## ggplot synthetic vs actual
ggplot() + geom_line(aes(x = as.numeric(row.names(dataprep.out.original$Y1plot)), y = dataprep.out.original$Y1plot)) + geom_line(aes(x = as.numeric(row.names(synthetic_control_values)), y = synthetic_control_values), linetype="dashed") + theme_bw() + theme(panel.border = element_blank()) + scale_x_continuous(breaks=seq(1977,1991,1)) + geom_vline(aes(xintercept=1982), linetype="dotted") + xlab("Year") + ylab("Status completion rate") + ylim(0.6,1)

# treated-synthetic gaps
ggplot() + geom_line(aes(x = year, y = gap), data=actual_synth_gaps) + theme_bw() + theme(panel.border = element_blank()) + scale_x_continuous(breaks=seq(1977,1991,1)) + geom_vline(aes(xintercept=1982), linetype="dotted") + geom_hline(aes(yintercept=0), linetype="dashed") + xlab("Year") + ylab("Synthetic-Treatment gap") + ylim(-0.05,0.05)

# donor states vs actual alaska
ggplot() + geom_line(aes(y=mean_education_level, x=year, group=state), color="#d3d3d3", data=synth_donors) + geom_line(aes(y=mean_education_level, x=year), data=alaska_only) + theme_bw() + theme(panel.border = element_blank()) + scale_x_continuous(breaks=seq(1977,1991,1))+ xlab("Year") + ylab("Status Completion Rate") + ylim(0.6,1)

# treatment gaps alongside placebos
ggplot() + geom_line(data=all_trials_frame_long, aes(x=year, y=gap, group=factor(region)), color="#d3d3d3", show.legend=FALSE) + geom_line(data=actual_synth_gaps, aes(x=year, y=gap), color="black") + geom_vline(xintercept = 1982, linetype="dotted") + ylab("Gap between synthetic and treated unit") + xlab("Year") + ggtitle("Placebo: Synthetic Alaska vs. Synthetic Other States")  + theme_bw() + theme(panel.border = element_blank())

# Alaska vs other states on status completion
ggplot() + geom_line(data=synth_data[synth_data$state !="Alaska",], aes(x=year, y=mean_education_level, group=state), color="#d3d3d3", show.legend=FALSE) + geom_line(data=synth_data[synth_data$state=="Alaska",], aes(x=year, y=mean_education_level), color="black") + geom_vline(xintercept = 1982, linetype="dotted") + ylab("Status completion rate") + xlab("Year") + ggtitle("Status Completion: Alaska vs. Other States") + theme(panel.border = element_blank()) + xlim(1977,1991)

save(
  dataprep.out.original,
  synthetic_control_values,
  nonzero_state_weights,
  pretreatment_predictor_values,
  all_trials_frame_long,
  actual_synth_gaps,
  synth_donors,
  donor_factors,
  alaska_only,
  rmse_pretreatment,
  rmse_pretreatment_alaska,
  mae_pretreatment,
  mae_pretreatment_alaska,
  placebo_sd_ratio,
  alaska_sd_ratio,
  rmse_ratios_w_states,
  alaska_gap_frame,
  synth_data,
  file="../final/data_for_figures.rdata")


