
library(foreign)
library(haven)
library(ggplot2)
library(broom)
library(plyr)
library(dplyr)
library(DataCombine)
library(plm)
library(estimatr)
library(gsynth)
library(reshape2)
library(zoo)
library(stargazer)
library(did)

gc()

getwd()
setwd("./output")

data <- read.csv("democracy_measures_paper.csv")


data$unified_r[data$partycontrol==0] <- 1
data$unified_r[data$partycontrol!=0] <- 0
data$unified_d[data$partycontrol==2] <- 1
data$unified_d[data$partycontrol!=2] <- 0
data$divided_govt[data$partycontrol==1] <- 1
data$divided_govt[data$partycontrol!=1] <- 0

#broader democracy index (appendix)
data <- join(data, read.csv("democracy_full_factanal.csv")[2:4])

setwd("..")

#racial demographics

demographics <- read.csv("state_demographic_data.csv")
demographics$state <- gsub(" \\(", "", demographics$state)
demographics$state <- gsub("\\)", "", demographics$state)
demographics$state <- gsub('[[:digit:]]+', '', demographics$state)
totals <- aggregate(pop ~ state + year, demographics, sum, na.rm=T)
names(totals)[3] <- "total_pop"
demographics <- join(demographics, totals)
demographics$pop_pct <- 100*(demographics$pop/demographics$total_pop)
#demographics$state_year <- paste(demographics$state, demographics$year)

demographics <- reshape(demographics[,c("pop_pct","state","year","race")], 
                        idvar = c("state","year"), timevar = "race", direction = "wide")
demographics <- demographics[,c(1,2,5,6)]
names(demographics) <- c("state","year","pct_black","pct_white")

latino <- read.csv("state_hispanic_pop_data.csv")
latino$state <- gsub(" \\(", "", latino$state)
latino$state <- gsub("\\)", "", latino$state)
latino$state <- gsub('[[:digit:]]+', '', latino$state)
totals <- aggregate(pop ~ state + year, latino, sum, na.rm=T)
names(totals)[3] <- "total_pop"
latino <- join(latino, totals)
latino$pop_pct <- 100*(latino$pop/latino$total_pop)

latino <- reshape(latino[,c("pop_pct","state","year","race")], 
                        idvar = c("state","year"), timevar = "race", direction = "wide")
latino <- latino[,c(1,2,3)]
names(latino) <- c("state","year","pct_latino")

demographics <- join(demographics, latino)


demographics <- slide(demographics, "pct_black", "year", "state", "pct_black_lag", 
                      slideBy = -4, keepInvalid = F, reminder = TRUE)
demographics <- slide(demographics, "pct_latino", "year", "state", "pct_latino_lag", 
                      slideBy = -4, keepInvalid = F, reminder = TRUE)

demographics$pct_black_change_ratio <- (demographics$pct_black/demographics$pct_black_lag - 1)
demographics$pct_latino_change_ratio <- (demographics$pct_latino/demographics$pct_latino_lag - 1)

demographics$pct_black_change <- (demographics$pct_black - demographics$pct_black_lag)
demographics$pct_latino_change <- (demographics$pct_latino - demographics$pct_latino_lag)



data <- join(data, demographics)



#Shor and McCarty scores

shor_mccarty <- data.frame(read_dta("shor mccarty 1993-2016 state aggregate data May 2018 release (Updated July 2018).dta"))
shor_mccarty$st <- as.character(shor_mccarty$st)

shor_plot <- rbind.fill(shor_mccarty[,c("st", "h_diffs", "year")], shor_mccarty[,c("st", "s_diffs", "year")])
shor_plot$chamber[!is.na(shor_plot$h_diffs)] <- "Lower"
shor_plot$chamber[!is.na(shor_plot$s_diffs)] <- "Upper"
shor_plot$h_diffs[is.na(shor_plot$h_diffs)] <- shor_plot$s_diffs[is.na(shor_plot$h_diffs)]


data <- join(data, shor_mccarty[,names(shor_mccarty)[grepl("_error", names(shor_mccarty))==F & (names(shor_mccarty) %in% c("alpha","fips")==F)]])
rm(shor_mccarty); gc()

data$polarization_sen <- abs(data$sen_dem-data$sen_rep)
data$polarization_hou <- abs(data$hou_dem-data$hou_rep)

data$polarization_hou <- as.numeric(scale(data$polarization_hou))
data$polarization_sen <- as.numeric(scale(data$polarization_sen))
data$polarization_avg <- rowSums(data[,c("polarization_hou","polarization_sen")], na.rm=T)/2

#Competition variables
obrian <- read.dta("data_onepartystates.dta", convert.factors = "string")
names(obrian) <- tolower(names(obrian))

data <- join(data, obrian[,c("state","year","rolldemleg","rolldemvs")], match="first")

data$competition_allleg <- -1*abs(50-data$rolldemleg)/100
data$competition_votes <- -1*abs(50-data$rolldemvs)/100

data$competition_index <- rowSums(data[,names(data)[grepl("competition_", names(data))]], na.rm=F)


competition_vars <- names(data)[grepl("competition_", names(data))]



###Cleaning

data <- data[order(data$state, data$year),]

data[,c("state","competition_allleg")] <- data %>% group_by(state) %>% transmute(competition_allleg=na.locf(competition_allleg, na.rm=F))
data[,c("state","competition_votes")] <- data %>% group_by(state) %>% transmute(competition_votes=na.locf(competition_votes, na.rm=F))
data[,c("state","competition_index")] <- data %>% group_by(state) %>% transmute(competition_index=na.locf(competition_index, na.rm=F))


data[data$year>2014, competition_vars] <- NA

for(i in competition_vars){
  
  data[,i] <- as.numeric(scale(data[,i]))
  
  #create lag competition variable
  data$tempvar <- data[,i]
  data <- data[order(data$state, data$year),]
  data$competition_lag <- NULL
  data <- slide(data, "tempvar", "year", "state", "competition_lag", slideBy = -2, keepInvalid = F, reminder = TRUE)
  data$competition_lag[is.na(data$competition_lag) & data$year<2002] <- data$tempvar[is.na(data$competition_lag) & data$year<2002]
  
  names(data)[names(data)=="competition_lag"] <- paste(i, "_lag", sep="")
  data$tempvar <- NULL
}


#Cost of Voting Index

data <- join(data, read.csv("cost_of_voting_index.csv"))

#turnout

epi <- read.csv("epi_indicators-all_years.csv", na.strings = "")
names(epi)[1:2] <- c("st","state")

data <- join(data, epi[,c("st","year","vep_turnout")], by=c("st","year"))



setwd("./output")

saveRDS(data, file="clean_data.RDS")





####################Descriptive Stats########################


########Plots########


pdf("correlation_with_covi.pdf", h=4, w=6)
ggplot(data[!is.na(data$cost_of_voting_index),], aes(x=democracy_mcmc, y=cost_of_voting_index, label=st)) +
  #geom_point() +
  #geom_line(aes(group=st), color="grey80") +
  geom_text(size=2) +
  geom_smooth(se=F, method="lm", color="black", size=.5) +
  xlab("State Democracy Index") +
  ylab("Cost of Voting Index") +
  facet_wrap(~year) +
  theme_classic()
dev.off()

cor(data$democracy_mcmc[data$year==2000], data$cost_of_voting_index[data$year==2000])
cor(data$democracy_mcmc[data$year==2004], data$cost_of_voting_index[data$year==2004])
cor(data$democracy_mcmc[data$year==2008], data$cost_of_voting_index[data$year==2008])
cor(data$democracy_mcmc[data$year==2012], data$cost_of_voting_index[data$year==2012])
cor(data$democracy_mcmc[data$year==2016], data$cost_of_voting_index[data$year==2016])


pdf("correlation_with_turnout.pdf", h=4, w=6)
ggplot(data[!is.na(data$vep_turnout),], aes(x=democracy_mcmc, y=vep_turnout*100, label=st)) +
  geom_text(size=2) +
  geom_smooth(se=F, method="lm", color="black", size=.5) +
  xlab("State Democracy Index") +
  ylab("Voter Turnout") +
  facet_wrap(~year) +
  theme_classic()
dev.off()


pdf("democracy_timeseries.pdf", h=6, w=6)
ggplot(data, aes(x=year, y=democracy_mcmc)) +
  geom_line(aes(group=st), color="grey80") +
  geom_smooth(se=F, color="black") +
  xlab("Year") +
  ylab("Democracy Score") +
  theme_classic()
dev.off()


pdf("democracy_timeseries_party.pdf", h=4, w=6)
ggplot(data[!is.na(data$partycontrol),], aes(x=year, y=democracy_mcmc)) +
  geom_line(aes(group=st), color="grey80") +
  geom_smooth(aes(group=factor(partycontrol), linetype=factor(partycontrol)), color="black") +
  scale_linetype_manual(values=c(1,2,3), name="", labels=c("Republican", "Divided", "Democratic")) +
  xlab("Year") +
  ylab("Democracy Score") +
  scale_x_continuous(breaks=c(2000,2005,2010,2015)) +
  #facet_grid(~additive_mcmc) +
  theme_classic()
dev.off()



pdf("democracy_timeseries_party_color.pdf", h=4, w=6)
ggplot(data[!is.na(data$partycontrol),], aes(x=year, y=democracy_mcmc, color=factor(partycontrol))) +
  geom_line(aes(group=st), color="grey80") +
  geom_smooth(aes(group=factor(partycontrol))) +
  scale_color_manual(values=c("red","dark green","dodger blue"), name="", labels=c("Republican", "Divided", "Democratic")) +
  xlab("Year") +
  ylab("Democracy Score") +
  scale_x_continuous(breaks=c(2000,2005,2010,2015)) +
  theme_classic()
dev.off()


plotdata <- data
plotdata$State <- NA
plotdata$State <- "Other"
plotdata$State[plotdata$state=="North Carolina"] <- "NC"
plotdata$State[plotdata$state=="Washington"] <- "WA"
plotdata$State[plotdata$state=="Texas"] <- "TX"

plotdata$democracy_mcmc_sd[plotdata$State=="Other"] <- NA

plotdata$State <- factor(plotdata$State,levels(factor(plotdata$State))[c(1,3,4,2)])

pdf("NC_highlight.pdf", h=4, w=8)
ggplot(plotdata[!is.na(plotdata$State),], aes(x=year, y=democracy_mcmc, size=State, color=State, 
                                              linetype=State)) +
  geom_ribbon(aes(ymin=democracy_mcmc-democracy_mcmc_sd,
                  ymax=democracy_mcmc+democracy_mcmc_sd), alpha = .3, color=NA) +
  geom_line(aes(group=state)) +
  scale_linetype_manual(values=c(1,2,3,1)) +
  scale_color_manual(values=c("black","black", "black","grey80")) +
  scale_size_manual(values=c(1.2, 1.2, 1.2, 0.2), guide=F) +
  #geom_smooth(aes(group=component), se=F) +
  xlab("Year") +
  ylab("Democracy Score") +
  scale_x_continuous(breaks=c(2000,2005,2010,2015)) +
  theme_classic()
dev.off()


#####Map#####

plot_means <- aggregate(democracy_mcmc ~ state + year, data[data$year==2000|data$year==2018,], mean, na.rm=T)
plot_means <- join(plot_means, data[,c("st","state")], match="first")
plot_means$region <- tolower(as.character(plot_means$state))

states <- map_data("state")
states <- join(states, plot_means)

pdf("democracy_map_2000_2018.pdf", h=3.5, w=8)
ggplot(data = states[!is.na(states$year),]) + 
  geom_polygon(aes(x = long, y = lat, fill = democracy_mcmc, group = group), color = "black") + 
  scale_fill_gradient(low = "white", high = "black", name="Democratic\nPerformance") +
  coord_quickmap() +
  facet_wrap(~year) +
  xlab("") +
  ylab("") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "bottom",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
dev.off()


###Racial Demographics

plotdata <- reshape(data[,c("st","year","pct_black","pct_latino","democracy_mcmc","partycontrol")], 
                         varying = c("pct_black", "pct_latino"), 
                         v.names = "pct_race",
                         timevar = "race", 
                         times = c("Black","Latino"), 
                         #new.row.names = 1:1000,
                         direction = "long")

out <- list()

for(i in levels(factor(data$state))){
  
  temp <- data.frame(data$democracy_mcmc[data$year==2018 & data$state==i]-
                       data$democracy_mcmc[data$year==2008 & data$state==i])
  temp$state <- i
  names(temp) <- c("change","state")
  out[[i]] <- temp
  
}
change <- do.call(rbind, out)

plotdata$State <- "Other"
plotdata$State[plotdata$st=="NC"] <- "NC"
plotdata$State[plotdata$st=="WI"] <- "WI"
plotdata$State[plotdata$st=="TN"] <- "TN"
plotdata$State[plotdata$st=="AL"] <- "AL"
plotdata$State[plotdata$st=="OH"] <- "OH"

plotdata$State <- factor(plotdata$State,levels(factor(plotdata$State))[c(1,2,3,5,6,4)])


pdf("democracy_timeseries_race.pdf", h=3, w=5)
ggplot(plotdata[!is.na(plotdata$partycontrol),], aes(x=year, y=pct_race, color=State, size=State, group=st)) +
  geom_line(show.legend=F) +
  scale_color_manual(values=c("black","black", "black","black","black","grey80")) +
  scale_size_manual(values=c(1.2, 1.2, 1.2, 1.2, 1.2, 0.3), guide=F) +  
  geom_text(data=plotdata[plotdata$State!="Other" & plotdata$year==2018,], 
            aes(x=2019, y=pct_race+1, label=State), size=3, show.legend=F) +
  xlab("Year") +
  ylab("Percent of State Population") +
  scale_x_continuous(breaks=c(2000,2005,2010,2015)) +
  facet_wrap(~race) +
  coord_cartesian(ylim=c(0,40)) +
  theme_classic()
dev.off()





##########################################################################################
####################################Hypothesis Tests#####################################
##########################################################################################
data$Democratic[data$partycontrol!=2] <- 0
data$Democratic[data$partycontrol==2] <- 1
data$Republican[data$partycontrol!=0] <- 0
data$Republican[data$partycontrol==0] <- 1


data <- data[order(data$state, data$year),]
data$competition_lag <- NULL

####################################DiD#####################################

lm_data <- model.frame(democracy_mcmc ~ Republican + Democratic + competition_allleg_lag + competition_votes_lag +
                         polarization_avg + state + year + democracy_additive + 
                         pct_black_change + pct_latino_change, data)
table(lm_data$year)

lm_1 <- lm(democracy_mcmc ~ competition_allleg_lag + factor(state) + factor(year), lm_data)
lm_2 <- lm(democracy_mcmc ~ polarization_avg + factor(state) + factor(year), lm_data)
lm_3 <- lm(democracy_mcmc ~ Republican + factor(state) + factor(year), lm_data)
lm_4 <- lm(democracy_mcmc ~ competition_allleg_lag + polarization_avg + Republican + factor(state) + factor(year), lm_data)
lm_5 <- lm(democracy_mcmc ~ competition_allleg_lag*polarization_avg + Republican + factor(state) + factor(year), lm_data)
lm_6 <- lm(democracy_mcmc ~ competition_allleg_lag + polarization_avg*Republican + factor(state) + factor(year), lm_data)
lm_7 <- lm(democracy_mcmc ~ polarization_avg + competition_allleg_lag*Republican + factor(state) + factor(year), lm_data)

lm_robust(democracy_mcmc ~ polarization_avg + competition_allleg_lag*Republican, 
          lm_data, se_type="stata", clusters=state)


stargazer(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7, 
          se = starprep(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7,
                        se_type="stata", clusters=lm_data$state),
          style="ajps",
          omit.stat=c("ser","f"),
          omit=c("year","state"),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out="main_results.tex")


###with racial demographics race interaction


lm_1 <- lm(democracy_mcmc ~ pct_black_change + pct_latino_change + factor(state) + factor(year), lm_data)
lm_2 <- lm(democracy_mcmc ~ pct_black_change*competition_allleg_lag + pct_latino_change*competition_allleg_lag + factor(state) + factor(year), lm_data)
lm_3 <- lm(democracy_mcmc ~ pct_black_change*polarization_avg + pct_latino_change*polarization_avg + factor(state) + factor(year), lm_data)
lm_4 <- lm(democracy_mcmc ~ pct_black_change*Republican + pct_latino_change*Republican + factor(state) + factor(year), lm_data)

lm_robust(democracy_mcmc ~ pct_black_change*Republican + pct_latino_change*Republican +factor(state) +factor(year), 
          lm_data, se_type="stata", clusters=state)


stargazer(lm_1, lm_2, lm_3, lm_4,
          se = starprep(lm_1, lm_2, lm_3, lm_4, 
                        se_type="stata", clusters=lm_data$state),
          style="ajps",
          omit.stat=c("ser","f"),
          omit=c("year","state"),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out="main_results_race.tex")




###with additive index

lm_1 <- lm(democracy_additive ~ competition_allleg_lag + factor(state) + factor(year), lm_data)
lm_2 <- lm(democracy_additive ~ polarization_avg + factor(state) + factor(year), lm_data)
lm_3 <- lm(democracy_additive ~ Republican + factor(state) + factor(year), lm_data)
lm_4 <- lm(democracy_additive ~ competition_allleg_lag + polarization_avg + Republican + factor(state) + factor(year), lm_data)
lm_5 <- lm(democracy_additive ~ competition_allleg_lag*polarization_avg + Republican + factor(state) + factor(year), lm_data)
lm_6 <- lm(democracy_additive ~ competition_allleg_lag + polarization_avg*Republican + factor(state) + factor(year), lm_data)
lm_7 <- lm(democracy_additive ~ polarization_avg + competition_allleg_lag*Republican + factor(state) + factor(year), lm_data)

lm_robust(democracy_additive ~ polarization_avg + competition_allleg_lag*Republican, 
          lm_data, se_type="stata", clusters=state)


stargazer(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7, 
          se = starprep(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7,
                        se_type="stata", clusters=lm_data$state),
          style="ajps",
          omit.stat=c("ser","f"),
          omit=c("year","state"),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out="appendix_additive_index_results.tex")



###with electoral competitiveness

lm_1 <- lm(democracy_mcmc ~ competition_votes_lag + factor(state) + factor(year), lm_data)
lm_2 <- lm(democracy_mcmc ~ polarization_avg + factor(state) + factor(year), lm_data)
lm_3 <- lm(democracy_mcmc ~ Republican + factor(state) + factor(year), lm_data)
lm_4 <- lm(democracy_mcmc ~ competition_votes_lag + polarization_avg + Republican + factor(state) + factor(year), lm_data)
lm_5 <- lm(democracy_mcmc ~ competition_votes_lag*polarization_avg + Republican + factor(state) + factor(year), lm_data)
lm_6 <- lm(democracy_mcmc ~ competition_votes_lag + polarization_avg*Republican + factor(state) + factor(year), lm_data)
lm_7 <- lm(democracy_mcmc ~ polarization_avg + competition_votes_lag*Republican + factor(state) + factor(year), lm_data)

lm_robust(democracy_mcmc ~ polarization_avg + competition_allleg_lag*Republican, 
          lm_data, se_type="stata", clusters=state)


stargazer(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7, 
          se = starprep(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7,
                        se_type="stata", clusters=lm_data$state),
          style="ajps",
          omit.stat=c("ser","f"),
          omit=c("year","state"),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out="appendix_electoralcomp_results.tex")



#with broader democracy measure (appendix)

lm_data <- model.frame(democracy_full_factanal ~ Republican + Democratic + competition_allleg_lag + competition_votes_lag +
                         polarization_avg + state + year + democracy_additive + 
                         pct_black_change + pct_latino_change, data)
table(lm_data$year)

lm_1 <- lm(democracy_full_factanal ~ competition_allleg_lag + factor(state) + factor(year), lm_data)
lm_2 <- lm(democracy_full_factanal ~ polarization_avg + factor(state) + factor(year), lm_data)
lm_3 <- lm(democracy_full_factanal ~ Republican + factor(state) + factor(year), lm_data)
lm_4 <- lm(democracy_full_factanal ~ competition_allleg_lag + polarization_avg + Republican + factor(state) + factor(year), lm_data)
lm_5 <- lm(democracy_full_factanal ~ competition_allleg_lag*polarization_avg + Republican + factor(state) + factor(year), lm_data)
lm_6 <- lm(democracy_full_factanal ~ competition_allleg_lag + polarization_avg*Republican + factor(state) + factor(year), lm_data)
lm_7 <- lm(democracy_full_factanal ~ polarization_avg + competition_allleg_lag*Republican + factor(state) + factor(year), lm_data)

lm_robust(democracy_full_factanal ~ polarization_avg + competition_allleg_lag*Republican, 
          lm_data, se_type="stata", clusters=state)


stargazer(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7, 
          se = starprep(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7,
                        se_type="stata", clusters=lm_data$state),
          style="ajps",
          omit.stat=c("ser","f"),
          omit=c("year","state"),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out="main_results_broad_democracy.tex")



###############gsynth###############

data$period <- data$year-1999
data$id <- as.integer(factor(data$state))

synth_1 <- gsynth(democracy_mcmc ~ unified_r, data = data[!is.na(data$unified_r),c("period","id","unified_r","democracy_mcmc")], 
                  parallel = FALSE,
                  index = c("id","period"), force = "two-way",
                  CV = TRUE, r = c(0, 5), 
                  se = T, min.T0 = 7)

pdf("gsynth_partycontrol.pdf", h=4, w=6)
plot(synth_1, main="", ylab="Democratic Performance", xlab="Years Relative to GOP Control", theme.bw = T)
dev.off()



synth_2 <- gsynth(democracy_mcmc ~ unified_r, data = data[!is.na(data$unified_r),c("period","id","unified_r","democracy_mcmc")], 
                  parallel = FALSE,
                  index = c("id","period"), force = "two-way",
                  CV = TRUE, r = c(0, 4), 
                  se = T, min.T0 = 6)

pdf("gsynth_partycontrol_2.pdf", h=4, w=6)
plot(synth_2, ylim=c(-6.5,0.5), main="", ylab="Democratic Performance", xlab="Years Relative to GOP Control", theme.bw = T)
dev.off()


synth_3 <- gsynth(democracy_mcmc ~ unified_r, data = data[!is.na(data$unified_r),c("period","id","unified_r","democracy_mcmc")], 
                  parallel = FALSE,
                  index = c("id","period"), force = "two-way",
                  CV = TRUE, r = c(0, 3), 
                  se = T, min.T0 = 5)

pdf("gsynth_partycontrol_3.pdf", h=4, w=6)
plot(synth_3, ylim=c(-6.5,0.5), main="", ylab="Democratic Performance", xlab="Years Relative to GOP Control", theme.bw = T)
dev.off()



synth_4 <- gsynth(democracy_mcmc ~ unified_r, data = data[!is.na(data$unified_r),c("period","id","unified_r","democracy_mcmc")], 
                  parallel = FALSE,
                  index = c("id","period"), force = "two-way",
                  CV = TRUE, r = c(0, 2), 
                  se = T, min.T0 = 4)

pdf("gsynth_partycontrol_4.pdf", h=4, w=6)
plot(synth_4, ylim=c(-6.5,0.5), main="", ylab="Democratic Performance", xlab="Years Relative to GOP Control", theme.bw = T)
dev.off()


data$placebo <- 0

for(i in levels(factor(data$state))){
  
  data$placebo[data$state==i & data$year>=sample(2000:2018, 1)] <- sample(c(0,1), 1)
  
}

synth_placebo <- gsynth(democracy_mcmc ~ placebo, data = data[!is.na(data$unified_r),c("period","id","unified_r","placebo","democracy_mcmc")], 
                  parallel = FALSE,
                  index = c("id","period"), force = "two-way",
                  CV = TRUE, r = c(0, 5), 
                  se = T, min.T0 = 7)

pdf("gsynth_partycontrol_placebo.pdf", h=4, w=6)
plot(synth_placebo, main="", ylab="Effect of Placebo Treatment", xlab="Years Relative to GOP Control", theme.bw = T)
dev.off()


########################################################################
########################DID Package####################################
########################################################################


####################################GOP Control

did_data <- data

did_data$period <- did_data$year-1999


for(i in levels(factor(did_data$state))){
  
  did_data$first_r_year[did_data$state==i] <- min(did_data$period[did_data$state==i & did_data$unified_r==1])
  
}

did_data$first_r_year[is.infinite(did_data$first_r_year)] <- 0
did_data$first_r_year[did_data$unified_r!=1 & did_data$first_r_year!=0 & did_data$period>did_data$first_r_year] <- NA

did_data$id <- as.numeric(as.factor(did_data$state))

att <- att_gt(yname = "democracy_mcmc",
              tname = "period",
              idname = "id",
              gname = "first_r_year",
              #xformla = ~X,
              allow_unbalanced_panel = T,
              data = did_data[!is.na(did_data$first_r_year),]
)


att_simple <- aggte(att, type = "simple")

att_dynamic <- aggte(att, type = "dynamic")

att_group <- aggte(att, type = "group")

temp <- data.frame(rbind(att_simple$overall.att, att_dynamic$overall.att, att_group$overall.att))
names(temp) <- "estimate"
temp$DID <- c("Group-Time", "Dynamic", "Group")
temp$se <- rbind(att_simple$overall.se, att_dynamic$overall.se, att_group$overall.se)

plotdata <- temp
rm(temp); gc()
plotdata$conf.low <- plotdata$estimate-(1.96*plotdata$se)
plotdata$conf.high <- plotdata$estimate+(1.96*plotdata$se)

pdf("DiD_SantannaCallaway.pdf", h=3, w=3)
ggplot(plotdata, aes(x=DID, y=estimate)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0) +
  #facet_wrap(~category, nrow=1) +
  geom_hline(yintercept=0, linetype=2) +
  #coord_cartesian(ylim=c(-.5,.5)) +
  scale_shape_discrete(name="") +
  xlab("ATT Aggregation") +
  ylab("Effect of GOP Control on\nDemocratic Performance") +
  ylim(-2,1) +
  theme_classic()
dev.off()



####################################Competition

did_data <- data

did_data$period <- did_data$year-1999

did_data$competition_change <- as.numeric(scale(did_data$competition_allleg - did_data$competition_allleg_lag))
did_data$competition_change[did_data$competition_change>=1] <- 1
did_data$competition_change[did_data$competition_change!=1 | is.na(did_data$competition_change)] <- 0

for(i in levels(factor(did_data$state))){
  
  did_data$first_r_year[did_data$state==i] <- min(did_data$period[did_data$state==i & did_data$competition_change==1], na.rm=T)
  
}

did_data$first_r_year[is.infinite(did_data$first_r_year)] <- 0
#did_data$first_r_year[did_data$competition_change!=1 & did_data$first_r_year!=0 & did_data$period>did_data$first_r_year] <- NA

did_data$id <- as.numeric(as.factor(did_data$state))

att <- att_gt(yname = "democracy_mcmc",
              tname = "period",
              idname = "id",
              gname = "first_r_year",
              allow_unbalanced_panel = T,
              data = did_data[!is.na(did_data$first_r_year),]
)


att_simple <- aggte(att, type = "simple")

att_dynamic <- aggte(att, type = "dynamic")

att_group <- aggte(att, type = "group")

temp <- data.frame(rbind(att_simple$overall.att, att_dynamic$overall.att, att_group$overall.att))
names(temp) <- "estimate"
temp$DID <- c("Group-Time", "Dynamic", "Group")
temp$se <- rbind(att_simple$overall.se, att_dynamic$overall.se, att_group$overall.se)

plotdata <- temp
rm(temp); gc()
plotdata$conf.low <- plotdata$estimate-(1.96*plotdata$se)
plotdata$conf.high <- plotdata$estimate+(1.96*plotdata$se)

pdf("DiD_SantannaCallaway_competition.pdf", h=3, w=3)
ggplot(plotdata, aes(x=DID, y=estimate)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_discrete(name="") +
  xlab("ATT Aggregation") +
  ylab("Effect of Competition on\nDemocratic Performance") +
  ylim(-1,1) +
  theme_classic()
dev.off()






####################################Polarization

did_data <- data

did_data <- did_data[order(did_data$state, did_data$year),]
did_data$polarization_lag <- NULL
did_data <- slide(did_data, "polarization_avg", "year", "state", "polarization_lag", slideBy = -2, keepInvalid = F, reminder = TRUE)

did_data$period <- did_data$year-1999

did_data$polarization_change <- as.numeric(scale(did_data$polarization_avg - did_data$polarization_lag))
did_data$polarization_change[did_data$polarization_change>=1] <- 1
did_data$polarization_change[did_data$polarization_change!=1 | is.na(did_data$polarization_change)] <- 0

for(i in levels(factor(did_data$state))){
  
  did_data$first_r_year[did_data$state==i] <- min(did_data$period[did_data$state==i & did_data$polarization_change==1], na.rm=T)
  
}

did_data$first_r_year[is.infinite(did_data$first_r_year)] <- 0

did_data$id <- as.numeric(as.factor(did_data$state))

att <- att_gt(yname = "democracy_mcmc",
              tname = "period",
              idname = "id",
              gname = "first_r_year",
              #xformla = ~X,
              allow_unbalanced_panel = T,
              data = did_data[!is.na(did_data$first_r_year),]
)


att_simple <- aggte(att, type = "simple")

att_dynamic <- aggte(att, type = "dynamic")

att_group <- aggte(att, type = "group")

temp <- data.frame(rbind(att_simple$overall.att, att_dynamic$overall.att, att_group$overall.att))
names(temp) <- "estimate"
temp$DID <- c("Group-Time", "Dynamic", "Group")
temp$se <- rbind(att_simple$overall.se, att_dynamic$overall.se, att_group$overall.se)

plotdata <- temp
rm(temp); gc()
plotdata$conf.low <- plotdata$estimate-(1.96*plotdata$se)
plotdata$conf.high <- plotdata$estimate+(1.96*plotdata$se)

pdf("DiD_SantannaCallaway_polarization.pdf", h=3, w=3)
ggplot(plotdata, aes(x=DID, y=estimate)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_discrete(name="") +
  xlab("ATT Aggregation") +
  ylab("Effect of Polarization on\nDemocratic Performance") +
  ylim(-1,1) +
  theme_classic()
dev.off()










####################################Racial Demographics

#Latino
did_data <- data

did_data <- did_data[order(did_data$state, did_data$year),]

did_data$period <- did_data$year-1999

did_data$pct_latino_change <- as.numeric(scale(did_data$pct_latino_change/did_data$pct_latino))

did_data$pct_latino_change[did_data$pct_latino_change>=1] <- 1
did_data$pct_latino_change[did_data$pct_latino_change!=1 | is.na(did_data$pct_latino_change)] <- 0

for(i in levels(factor(did_data$state))){
  
  did_data$first_r_year[did_data$state==i] <- min(did_data$period[did_data$state==i & did_data$pct_latino_change==1], na.rm=T)
  
}

did_data$first_r_year[is.infinite(did_data$first_r_year)] <- 0

did_data$id <- as.numeric(as.factor(did_data$state))

att <- att_gt(yname = "democracy_mcmc",
              tname = "period",
              idname = "id",
              gname = "first_r_year",
              #xformla = ~X,
              allow_unbalanced_panel = T,
              data = did_data[!is.na(did_data$first_r_year),]
)


att_simple <- aggte(att, type = "simple")

att_dynamic <- aggte(att, type = "dynamic")

att_group <- aggte(att, type = "group")

temp <- data.frame(rbind(att_simple$overall.att, att_dynamic$overall.att, att_group$overall.att))
names(temp) <- "estimate"
temp$DID <- c("Group-Time", "Dynamic", "Group")
temp$se <- rbind(att_simple$overall.se, att_dynamic$overall.se, att_group$overall.se)

plotdata <- temp
rm(temp); gc()
plotdata$conf.low <- plotdata$estimate-(1.96*plotdata$se)
plotdata$conf.high <- plotdata$estimate+(1.96*plotdata$se)

pdf("DiD_SantannaCallaway_latino.pdf", h=3, w=3)
ggplot(plotdata, aes(x=DID, y=estimate)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0) +
  #facet_wrap(~category, nrow=1) +
  geom_hline(yintercept=0, linetype=2) +
  #coord_cartesian(ylim=c(-.5,.5)) +
  scale_shape_discrete(name="") +
  xlab("ATT Aggregation") +
  ylab("Effect of Polarization on\nDemocratic Performance") +
  ylim(-1,1) +
  theme_classic()
dev.off()



#Black
did_data <- data

did_data <- did_data[order(did_data$state, did_data$year),]

did_data$period <- did_data$year-1999

did_data$pct_black_change <- as.numeric(scale(did_data$pct_black_change/did_data$pct_black))

did_data$pct_black_change[did_data$pct_black_change>=1] <- 1
did_data$pct_black_change[did_data$pct_black_change!=1 | is.na(did_data$pct_black_change)] <- 0

for(i in levels(factor(did_data$state))){
  
  did_data$first_r_year[did_data$state==i] <- min(did_data$period[did_data$state==i & did_data$pct_black_change==1], na.rm=T)
  
}

did_data$first_r_year[is.infinite(did_data$first_r_year)] <- 0
#did_data$first_r_year[did_data$competition_change!=1 & did_data$first_r_year!=0 & did_data$period>did_data$first_r_year] <- NA

did_data$id <- as.numeric(as.factor(did_data$state))

att <- att_gt(yname = "democracy_mcmc",
              tname = "period",
              idname = "id",
              gname = "first_r_year",
              #xformla = ~X,
              allow_unbalanced_panel = T,
              data = did_data[!is.na(did_data$first_r_year),]
)


att_simple <- aggte(att, type = "simple")

att_dynamic <- aggte(att, type = "dynamic")

att_group <- aggte(att, type = "group")

temp <- data.frame(rbind(att_simple$overall.att, att_dynamic$overall.att, att_group$overall.att))
names(temp) <- "estimate"
temp$DID <- c("Group-Time", "Dynamic", "Group")
temp$se <- rbind(att_simple$overall.se, att_dynamic$overall.se, att_group$overall.se)

plotdata <- temp
rm(temp); gc()
plotdata$conf.low <- plotdata$estimate-(1.96*plotdata$se)
plotdata$conf.high <- plotdata$estimate+(1.96*plotdata$se)

pdf("DiD_SantannaCallaway_black.pdf", h=3, w=3)
ggplot(plotdata, aes(x=DID, y=estimate)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0) +
  #facet_wrap(~category, nrow=1) +
  geom_hline(yintercept=0, linetype=2) +
  #coord_cartesian(ylim=c(-.5,.5)) +
  scale_shape_discrete(name="") +
  xlab("ATT Aggregation") +
  ylab("Effect of Polarization on\nDemocratic Performance") +
  ylim(-1,1) +
  theme_classic()
dev.off()


######Looping through cutoffs

gc()

output <- list()

for(i in c(0.5,1,1.5,2)){
  
  
  did_data <- data
  
  did_data <- did_data[order(did_data$state, did_data$year),]
  
  did_data$period <- did_data$year-1999
  
  output2 <- list()
  
  for(t in seq(2, 10, by=2)){
    
    
    did_data <- slide(did_data, "polarization_avg", "year", "state", "polarization_avg_lag", 
                      slideBy = (t*-1), keepInvalid = F, reminder = TRUE)
    did_data <- slide(did_data, "competition_allleg", "year", "state", "competition_allleg_lag", 
                      slideBy = (t*-1), keepInvalid = F, reminder = TRUE)
    
    
    did_data$polarization_avg_change <- did_data$polarization_avg - did_data$polarization_avg_lag
    
    did_data$polarization_avg_change <- as.numeric(scale(did_data$polarization_avg_change))
    
    did_data$polarization_avg_change[did_data$polarization_avg_change>=i] <- 1
    did_data$polarization_avg_change[did_data$polarization_avg_change!=1 | is.na(did_data$polarization_avg_change)] <- 0
    
    
    did_data$competition_allleg_change <- did_data$competition_allleg - did_data$competition_allleg_lag
    
    did_data$competition_allleg_change <- as.numeric(scale(did_data$competition_allleg_change))
    
    did_data$competition_allleg_change[did_data$competition_allleg_change>=i] <- 1
    did_data$competition_allleg_change[did_data$competition_allleg_change!=1 | is.na(did_data$competition_allleg_change)] <- 0
    
    
    did_data <- slide(did_data, "pct_latino", "year", "state", "pct_latino_lag", 
                          slideBy = (t*-1), keepInvalid = F, reminder = TRUE)
    did_data <- slide(did_data, "pct_black", "year", "state", "pct_black_lag", 
                      slideBy = (t*-1), keepInvalid = F, reminder = TRUE)
    
    
    did_data$pct_black_change <- did_data$pct_black - did_data$pct_black_lag
    
    did_data$pct_black_change <- as.numeric(scale(did_data$pct_black_change/did_data$pct_black))
    
    did_data$pct_black_change[did_data$pct_black_change>=i] <- 1
    did_data$pct_black_change[did_data$pct_black_change!=1 | is.na(did_data$pct_black_change)] <- 0
    
    
    did_data$pct_latino_change <- did_data$pct_latino - did_data$pct_latino_lag
    
    did_data$pct_latino_change <- as.numeric(scale(did_data$pct_latino_change))
    
    did_data$pct_latino_change[did_data$pct_latino_change>=i] <- 1
    did_data$pct_latino_change[did_data$pct_latino_change!=1 | is.na(did_data$pct_latino_change)] <- 0
    
    for(j in levels(factor(did_data$state))){
      
      did_data$first_polarization_year[did_data$state==j] <- min(did_data$period[did_data$state==j & did_data$polarization_avg_change==1], na.rm=T)
      did_data$first_competition_year[did_data$state==j] <- min(did_data$period[did_data$state==j & did_data$competition_allleg_change==1], na.rm=T)
      
      did_data$first_latino_year[did_data$state==j] <- min(did_data$period[did_data$state==j & did_data$pct_latino_change==1], na.rm=T)
      did_data$first_black_year[did_data$state==j] <- min(did_data$period[did_data$state==j & did_data$pct_black_change==1], na.rm=T)
      
    }
    
    did_data$first_polarization_year[is.infinite(did_data$first_polarization_year)] <- 0
    did_data$first_competition_year[is.infinite(did_data$first_competition_year)] <- 0
    
    did_data$first_latino_year[is.infinite(did_data$first_latino_year)] <- 0
    did_data$first_black_year[is.infinite(did_data$first_black_year)] <- 0
    
    did_data$id <- as.numeric(as.factor(did_data$state))
    
    
    #Models
    
    
    att2 <- att_gt(yname = "democracy_mcmc",
                   tname = "period",
                   idname = "id",
                   gname = "first_polarization_year",
                   #xformla = ~partycontrol,
                   allow_unbalanced_panel = T,
                   data = did_data[!is.na(did_data$first_polarization_year) & !is.na(did_data$partycontrol),]
    )
    
    
    att_simple2 <- aggte(att2, type = "simple")
    
    att_dynamic2 <- aggte(att2, type = "dynamic")
    
    att_group2 <- aggte(att2, type = "group")
    
    
    temp <- data.frame(rbind(att_simple2$overall.att, att_dynamic2$overall.att, att_group2$overall.att))
    names(temp) <- "estimate"
    temp$DID <- c("Group-Time", "Dynamic", "Group")
    temp$se <- rbind(att_simple2$overall.se, att_dynamic2$overall.se, att_group2$overall.se)
    temp$race <- "Polarization"
    
    
    
    att2 <- att_gt(yname = "democracy_mcmc",
                   tname = "period",
                   idname = "id",
                   gname = "first_competition_year",
                   #xformla = ~partycontrol,
                   allow_unbalanced_panel = T,
                   data = did_data[!is.na(did_data$first_competition_year) & !is.na(did_data$partycontrol),]
    )
    
    
    att_simple2 <- aggte(att2, type = "simple")
    
    att_dynamic2 <- aggte(att2, type = "dynamic")
    
    att_group2 <- aggte(att2, type = "group")
    
    
    temp2 <- data.frame(rbind(att_simple2$overall.att, att_dynamic2$overall.att, att_group2$overall.att))
    names(temp2) <- "estimate"
    temp2$DID <- c("Group-Time", "Dynamic", "Group")
    temp2$se <- rbind(att_simple2$overall.se, att_dynamic2$overall.se, att_group2$overall.se)
    temp2$race <- "Competition"
    
    
    
    
    att <- att_gt(yname = "democracy_mcmc",
                  tname = "period",
                  idname = "id",
                  gname = "first_black_year",
                  #xformla = ~partycontrol,
                  allow_unbalanced_panel = T,
                  data = did_data[!is.na(did_data$first_black_year) & !is.na(did_data$partycontrol),]
    )
    
    
    att_simple <- aggte(att, type = "simple")
    
    att_dynamic <- aggte(att, type = "dynamic")
    
    att_group <- aggte(att, type = "group")
    
    
    temp3 <- data.frame(rbind(att_simple$overall.att, att_dynamic$overall.att, att_group$overall.att))
    names(temp3) <- "estimate"
    temp3$DID <- c("Group-Time", "Dynamic", "Group")
    temp3$se <- rbind(att_simple$overall.se, att_dynamic$overall.se, att_group$overall.se)
    temp3$race <- "Black"
    
    att2 <- att_gt(yname = "democracy_mcmc",
                   tname = "period",
                   idname = "id",
                   gname = "first_latino_year",
                   #xformla = ~partycontrol,
                   allow_unbalanced_panel = T,
                   data = did_data[!is.na(did_data$first_latino_year) & !is.na(did_data$partycontrol),]
    )
    
    
    att_simple2 <- aggte(att2, type = "simple")
    
    att_dynamic2 <- aggte(att2, type = "dynamic")
    
    att_group2 <- aggte(att2, type = "group")
    
    
    temp4 <- data.frame(rbind(att_simple2$overall.att, att_dynamic2$overall.att, att_group2$overall.att))
    names(temp4) <- "estimate"
    temp4$DID <- c("Group-Time", "Dynamic", "Group")
    temp4$se <- rbind(att_simple2$overall.se, att_dynamic2$overall.se, att_group2$overall.se)
    temp4$race <- "Latino"
    
    temp5 <- rbind(temp, temp2, temp3, temp4)
    temp5$i <- i
    temp5$t <- t
    
    output2[[t]] <- temp5
    
    gc()
  }
  
  index <- i*2
  output[[index]] <- do.call(rbind, output2)
  
}


plotdata <- do.call(rbind, output)

write.csv(plotdata, "DID_package_simulations.csv")



pdf("DID_race_simulations_black.pdf")
ggplot(plotdata[plotdata$race=="Black",], aes(x=t, y=estimate)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=estimate-1.96*se, ymax=estimate+1.96*se), width=0) +
  facet_wrap(DID~i) +
  geom_hline(yintercept=0, linetype=2) +
  #coord_cartesian(ylim=c(-.5,.5)) +
  scale_shape_discrete(name="") +
  xlab("Years of Demographic Change") +
  ylab("Democratic Performance") +
  coord_cartesian(ylim=c(-2,2)) +
  theme_classic()
dev.off()

pdf("DID_race_simulations_latino.pdf")
ggplot(plotdata[plotdata$race=="Latino",], aes(x=t, y=estimate)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=estimate-(1.96*se), ymax=estimate+(1.96*se)), width=0) +
  facet_wrap(DID~i) +
  geom_hline(yintercept=0, linetype=2) +
  #coord_cartesian(ylim=c(-.5,.5)) +
  scale_shape_discrete(name="") +
  xlab("Years of Demographic Change") +
  ylab("Democratic Performance") +
  coord_cartesian(ylim=c(-2,2)) +
  theme_classic()
dev.off()



pdf("DID_race_simulations_polarization.pdf")
ggplot(plotdata[plotdata$race=="Polarization",], aes(x=t, y=estimate)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=estimate-1.96*se, ymax=estimate+1.96*se), width=0) +
  facet_wrap(DID~i) +
  geom_hline(yintercept=0, linetype=2) +
  #coord_cartesian(ylim=c(-.5,.5)) +
  scale_shape_discrete(name="") +
  xlab("Years of Demographic Change") +
  ylab("Democratic Performance") +
  coord_cartesian(ylim=c(-2,2)) +
  theme_classic()
dev.off()

pdf("DID_race_simulations_competition.pdf")
ggplot(plotdata[plotdata$race=="Competition",], aes(x=t, y=estimate)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=estimate-(1.96*se), ymax=estimate+(1.96*se)), width=0) +
  facet_wrap(DID~i) +
  geom_hline(yintercept=0, linetype=2) +
  #coord_cartesian(ylim=c(-.5,.5)) +
  scale_shape_discrete(name="") +
  xlab("Years of Demographic Change") +
  ylab("Democratic Performance") +
  coord_cartesian(ylim=c(-2,2)) +
  theme_classic()
dev.off()



##################Robustness Checks WITHOUT VOTER ID#######################

data <- join(data, read.csv("measure_novoterid.csv"))




lm_data <- model.frame(democracy_novoterid ~ Republican + Democratic + competition_allleg_lag + competition_votes_lag +
                         polarization_avg + state + year + democracy_additive + 
                         pct_black_change + pct_latino_change, data)
table(lm_data$year)

lm_1 <- lm(democracy_novoterid ~ competition_allleg_lag + factor(state) + factor(year), lm_data)
lm_2 <- lm(democracy_novoterid ~ polarization_avg + factor(state) + factor(year), lm_data)
lm_3 <- lm(democracy_novoterid ~ Republican + factor(state) + factor(year), lm_data)
lm_4 <- lm(democracy_novoterid ~ competition_allleg_lag + polarization_avg + Republican + factor(state) + factor(year), lm_data)
lm_5 <- lm(democracy_novoterid ~ competition_allleg_lag*polarization_avg + Republican + factor(state) + factor(year), lm_data)
lm_6 <- lm(democracy_novoterid ~ competition_allleg_lag + polarization_avg*Republican + factor(state) + factor(year), lm_data)
lm_7 <- lm(democracy_novoterid ~ polarization_avg + competition_allleg_lag*Republican + factor(state) + factor(year), lm_data)

lm_robust(democracy_novoterid ~ polarization_avg + competition_allleg_lag*Republican, 
          lm_data, se_type="stata", clusters=state)


stargazer(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7, 
          se = starprep(lm_1, lm_2, lm_3, lm_4, lm_5, lm_6, lm_7,
                        se_type="stata", clusters=lm_data$state),
          style="ajps",
          omit.stat=c("ser","f"),
          omit=c("year","state"),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out="novoterid_results.tex")


###with racial demographics race interaction


lm_1 <- lm(democracy_novoterid ~ pct_black_change + pct_latino_change + factor(state) + factor(year), lm_data)
lm_2 <- lm(democracy_novoterid ~ pct_black_change*competition_allleg_lag + pct_latino_change*competition_allleg_lag + factor(state) + factor(year), lm_data)
lm_3 <- lm(democracy_novoterid ~ pct_black_change*polarization_avg + pct_latino_change*polarization_avg + factor(state) + factor(year), lm_data)
lm_4 <- lm(democracy_novoterid ~ pct_black_change*Republican + pct_latino_change*Republican + factor(state) + factor(year), lm_data)

lm_robust(democracy_novoterid ~ pct_black_change*Republican + pct_latino_change*Republican +factor(state) +factor(year), 
          lm_data, se_type="stata", clusters=state)


stargazer(lm_1, lm_2, lm_3, lm_4,
          se = starprep(lm_1, lm_2, lm_3, lm_4, 
                        se_type="stata", clusters=lm_data$state),
          style="ajps",
          omit.stat=c("ser","f"),
          omit=c("year","state"),
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out="novoterid_results_race.tex")






###############gsynth###############

data$period <- data$year-1999
data$id <- as.integer(factor(data$state))

synth_1 <- gsynth(democracy_novoterid ~ unified_r, data = data[!is.na(data$unified_r),c("period","id","unified_r","democracy_novoterid")], 
                  parallel = FALSE,
                  index = c("id","period"), force = "two-way",
                  CV = TRUE, r = c(0, 5), se = T, min.T0 = 7)

pdf("gsynth_partycontrol_novoterid.pdf", h=4, w=6)
plot(synth_1, main="", ylab="Effect of GOP Control", xlab="Years Relative to GOP Control", theme.bw = T)
dev.off()



####################################GOP Control

did_data <- data

did_data$period <- did_data$year-1999


for(i in levels(factor(did_data$state))){
  
  did_data$first_r_year[did_data$state==i] <- min(did_data$period[did_data$state==i & did_data$unified_r==1])
  
}

did_data$first_r_year[is.infinite(did_data$first_r_year)] <- 0
did_data$first_r_year[did_data$unified_r!=1 & did_data$first_r_year!=0 & did_data$period>did_data$first_r_year] <- NA

did_data$id <- as.numeric(as.factor(did_data$state))

att <- att_gt(yname = "democracy_novoterid",
              tname = "period",
              idname = "id",
              gname = "first_r_year",
              #xformla = ~X,
              allow_unbalanced_panel = T,
              data = did_data[!is.na(did_data$first_r_year),]
)


att_simple <- aggte(att, type = "simple")

att_dynamic <- aggte(att, type = "dynamic")

att_group <- aggte(att, type = "group")

temp <- data.frame(rbind(att_simple$overall.att, att_dynamic$overall.att, att_group$overall.att))
names(temp) <- "estimate"
temp$DID <- c("Group-Time", "Dynamic", "Group")
temp$se <- rbind(att_simple$overall.se, att_dynamic$overall.se, att_group$overall.se)

plotdata <- temp
rm(temp); gc()
plotdata$conf.low <- plotdata$estimate-(1.96*plotdata$se)
plotdata$conf.high <- plotdata$estimate+(1.96*plotdata$se)

pdf("DiD_SantannaCallaway_novoterid.pdf", h=3, w=3)
ggplot(plotdata, aes(x=DID, y=estimate)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0) +
  #facet_wrap(~category, nrow=1) +
  geom_hline(yintercept=0, linetype=2) +
  #coord_cartesian(ylim=c(-.5,.5)) +
  scale_shape_discrete(name="") +
  xlab("ATT Aggregation") +
  ylab("Effect of GOP Control on\nDemocratic Performance") +
  ylim(-2,1) +
  theme_classic()
dev.off()


