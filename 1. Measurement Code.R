
library(ggplot2)
library(reshape2)
library(broom)
library(plyr)
library(foreign)
library(ltm)
library(psych)
library(MCMCpack)
library(wfe)
library(DataCombine)
library(lme4)
library(stringr)
library(dplyr)
library(zoo)
library(FactoMineR)
library(factoextra)
library(BayesFM)
library(stargazer)



join <- plyr::join

gc()



##List of All Variables
variables <- read.csv("variables_list.csv", na="", stringsAsFactors=F); gc()
n_var <- length(variables$description)

variables <- variables[order(variables$description),]



###########Load and Merge Data###########

##Grumbach 2018 data and Grumbach and Hill 2019 data
data <- join(read.dta("grumbach_2018.dta"), 
             read.csv("sdr_data.csv", stringsAsFactors=F), type="full")
data <- data[data$year>1970,]

#Female Governors
data$femgov <- NULL
data <- join(data, read.csv("female_governors_klarner_extended.csv", na="", stringsAsFactors = F))

#Ban on Sanctuary cities
data$sanctuary_ban <- 0
data$sanctuary_ban[(data$st=="AZ"|data$st=="GA") & data$year>=2010] <- 1
data$sanctuary_ban[(data$st=="TN") & data$year>=2017] <- 1

#party control

data <- join(data, read.csv("party_control_post2014.csv", stringsAsFactors = F))
data$partycontrol[data$year>=2015] <- data$partycontrolpost2014[data$year>=2015]

#AVR
data$avr_any <- 0
data$avr_any[data$st %in% c("OR") & data$year>=2016] <- 1
data$avr_any[data$st %in% c("AK","CA","CO","CT","GA","NJ","RI","VT") & data$year>=2018] <- 1
data$avr_any[data$st %in% c("MD","MA","MI","NM","WA") & data$year>=2020] <- 1

data$avr_backend <- 0
data$avr_backend[data$st %in% c("AK","CO","MA","OR")] <- data$avr_any[data$st %in% c("AK","CO","MA","OR")]


#SDR
data$sdr[data$st=="ND"] <- 1

#Voter ID
data$voterid_any[data$w_voterid==0] <- 0
data$voterid_any[data$w_voterid>0] <- 1
data$voterid_strict[data$w_voterid!=2] <- 0
data$voterid_strict[data$w_voterid==2] <- 1

#Prisoner Vote
data$prisoner_vote <- 0
data$prisoner_vote[data$st=="ME"|data$st=="VT"] <- 1

##Kelly and Witko 2012 data
data <- join(data, 
             read.csv("kelly_witko_2012_ajps_data.csv", 
                      na="", stringsAsFactors=F)[,c("state", "year", "marketgini", 
                                                    "postgini","uniondense")])

##Chetty mobility data
data <- join(data, read.dta("upward_mobility_data.dta", convert.factors = F))
names(data)[names(data)=="upward_mobility"] <- "x_upward_mobility"

data <- join(data, read.csv("health_ineq_online_table_5.csv", 
                            stringsAsFactors = F)[,c("st","year",
                                                     "le_agg_q1_F", "le_agg_q1_M",
                                                     "le_agg_q4_F", "le_agg_q4_M")])

data$x_life_expectancy_inequality <- ((data$le_agg_q4_F+data$le_agg_q4_M)/2) - 
  ((data$le_agg_q1_F+data$le_agg_q1_M)/2)


##Elections Performance Index data

epi <- read.csv("epi_indicators-all_years.csv", na.strings = "")
names(epi)[1:2] <- c("st","state")

data <- join(data, epi, by=c("st","year"))

rm(epi); gc()

data <- join(data, read.csv("1980-2014 November General Election - Turnout Rates.csv", stringsAsFactors = F))
data$vep_turnout[is.na(data$vep_turnout)] <- data$vep_turnout_pre08[is.na(data$vep_turnout)]


##Black/White Incarceration Ratio

data$BLACKM[data$BLACKM<0] <- NA
data$BLACKF[data$BLACKF<0] <- NA
data$WHITEM[data$BLACKM<0] <- NA
data$WHITEF[data$BLACKF<0] <- NA

data$blackincarcerate <- (data$BLACKM + data$BLACKF)/data$pop_black_i
data$whiteincarcerate <- (data$WHITEM + data$WHITEF)/data$pop_white_i

data$blackincarcerate[data$blackincarcerate<=0] <- NA
data$blackincarcerate[data$blackincarcerate>0.1] <- NA
data$whiteincarcerate[data$whiteincarcerate<=0] <- NA

data$bw_incarcerate_ratio <- data$blackincarcerate/data$whiteincarcerate
data$bw_incarcerate_ratio[data$st=="RI" & data$year==2001] <- NA

#Right to Work (correction)
data$labor_grtw[data$year<2010] <- data$labor_right_to_work[data$year<2010]

#Campaign Finance
data$x_ind_limits[data$x_ind_limits==999999] <- NA
data$x_pac_limits[data$x_pac_limits==999999] <- NA
data$x_ind_limits <- 1/data$x_ind_limits
data$x_pac_limits <- 1/data$x_pac_limits
data$pac_limit <- NULL
data$pac_limit[data$pac_unlimited==1] <- 0
data$pac_limit[data$pac_unlimited==0] <- 1

#District compactness (gerrymandering) from Kaufman, King, & Komisarchik
load("preds_6_14_18.RData")

finalpreds$state_fips <- as.numeric(as.character(gsub("_.*$", "", finalpreds$district)))
finalpreds$year <- as.numeric(as.character(str_sub(finalpreds$district, start= -4)))

compactness <- aggregate(compactness ~ state_fips + year, finalpreds, mean, na.rm=T)

data <- join(data, compactness[,c("state_fips","year","compactness")])

rm(finalpreds, compactness); gc()

##Asset forfeiture

data <- join(data, read.csv("IFJ_asset_forfeiture_ratings_cleaned.csv", stringsAsFactors = F))

##Caughey Warshaw Responsiveness
cw_data <- read.dta("caughey_warshaw_summary.dta")
cw_data$stpo <- as.character(cw_data$stpo)
cw_data$year <- as.numeric(as.character(cw_data$year))
names(cw_data)[1] <- "st"

for(i in unique(cw_data$year)){
  
  cw_data$masseconlib_est[cw_data$year==i] <- as.numeric(scale(cw_data$masseconlib_est[cw_data$year==i]))

}

cw_data$econ_opinion_residual <- (cw_data$policyeconlib_est - cw_data$masseconlib_est)^2
cw_data$social_opinion_residual <- (cw_data$policysociallib_est - cw_data$masssociallib_est)^2

data <- join(data, cw_data[,c("st","year","econ_opinion_residual","social_opinion_residual")])
rm(cw_data); gc()


##spatial segregation
data <- join(data, read.csv("black_white_segregation2000_2009.csv", na="", stringsAsFactors = F))
data <- join(data, read.csv("latino_white_segregation2000_2009.csv", na="", stringsAsFactors = F))

temp <- data[data$year>=2000, c("state","year","bw_segregation","lw_segregation")]

temp <- temp %>%
  group_by(state) %>%
  mutate(bw_segregation_i = na.approx(bw_segregation, na.rm=FALSE))

temp <- temp %>%
  group_by(state) %>%
  mutate(lw_segregation_i = na.approx(lw_segregation, na.rm=FALSE))

data <- join(data, temp[,c("state", "year", "bw_segregation_i","lw_segregation_i")])

rm(temp); gc()

#preregistration

prereg <- read.csv("preregistration_laws.csv", stringsAsFactors = F)

data$youth_preregistration <- 0

for(i in levels(factor(prereg$state))){
  if(!is.na(prereg$preregistration_year[prereg$state==i])){
    data$youth_preregistration[data$state==i & data$year>=prereg$preregistration_year[prereg$state==i]] <- 
      prereg$preregistration_law[prereg$state==i]
  }
  
  if(is.na(prereg$preregistration_year[prereg$state==i])){
    data$youth_preregistration[data$state==i] <- 
      prereg$preregistration_law[prereg$state==i]
  }
  
}

data$youth_preregistration[data$st=="NC" & data$year<2013] <- 4
data$youth_preregistration[data$st=="OR" & data$year<2016] <- 3

rm(prereg); gc()


#registration drive restrictions

regdrive <- read.csv("registration_drive_restrictions.csv", stringsAsFactors = F)

data$registration_drive_restriction <- 0

for(i in levels(factor(regdrive$state))){
  if(!is.na(regdrive$registration_drive_restriction_year[regdrive$state==i])){
    data$registration_drive_restriction[data$state==i & data$year>=regdrive$registration_drive_restriction_year[regdrive$state==i]] <- 
      regdrive$registration_drive_restriction[regdrive$state==i]
  }
  
  if(is.na(regdrive$registration_drive_restriction_year[regdrive$state==i])){
    data$registration_drive_restriction[data$state==i] <- 
      regdrive$registration_drive_restriction[regdrive$state==i]
  }
}

data$registration_drive_restriction[data$st=="OH" & data$year>=2011] <- 2

rm(regdrive); gc()


#Criminalization of protest

data$protest_crime <- 0
data$protest_crime[data$st %in% c("LA","MO","SD","TN", "WV") & data$year>=2016] <- 1
data$protest_crime[data$st %in% c("OK") & data$year>=2016] <- 2
data$protest_crime[data$st %in% c("ND") & data$year>=2016] <- 3


#Warshaw Stephanopoulos Gerrymandering Data
load("state_house_analysis.RData")
load("congress_analysis.RData")

state_elections_statehouse$st <- as.character(state_elections_statehouse$stateabrev)
state_elections_congress$st <- as.character(state_elections_congress$STATEAB)
state_elections_statehouse$year <- state_elections_statehouse$cycle
state_elections_congress$year <- state_elections_congress$election

names(state_elections_congress)[(names(state_elections_congress) %in% c("st", "year"))==F] <- paste(names(state_elections_congress)[(names(state_elections_congress) %in% c("st", "year"))==F],
                                                                                                    "_c", sep="")
names(state_elections_statehouse)[(names(state_elections_statehouse) %in% c("st", "year"))==F] <- paste(names(state_elections_statehouse)[(names(state_elections_statehouse) %in% c("st", "year"))==F],
                                                                                                    "_s", sep="")
numvars_c <- unlist(lapply(state_elections_congress, is.numeric))  
numvars_s <- unlist(lapply(state_elections_statehouse, is.numeric))  

state_elections_congress[,numvars_c] <- abs(state_elections_congress[,numvars_c])
state_elections_statehouse[,numvars_s] <- abs(state_elections_statehouse[,numvars_s])

data <- join(data, state_elections_statehouse[,c("st", "year", "EG_s", "mean_median_s", "symmetry_s", "declination2_s",
                                         "EG_pres_s", "mean_median_pres_s", "declination2_pres_s", "symmetry_pres_s")])
data <- join(data, state_elections_congress[,c("st", "year", "EG_c", "mean_median_c", "symmetry_c", "declination2_c",
                                                 "EG_pres_c", "mean_median_pres_c", "declination2_pres_c", "symmetry_pres_c")])


#Felons

data$nofelons <- data$nofelons/data$pop_annual


#poverty rate by race

pov_race_wide <- read.csv("state_poverty_by_race.csv", na.strings = c("", "N/A"))
names(pov_race_wide)[1] <- "state"
pov_race_wide$Footnotes <- NULL

pov_race_long <- reshape(pov_race_wide, 
                    varying = names(pov_race_wide)[2:length(pov_race_wide)], 
                    v.names = "pov_rate",
                    timevar = "race_year", 
                    times = names(pov_race_wide)[2:length(pov_race_wide)], 
                    direction = "long")
pov_race_long$year <- gsub("\\_\\_Black", "", pov_race_long$race_year)
pov_race_long$year <- gsub("\\_\\_Hispanic", "", pov_race_long$year)
pov_race_long$year <- gsub("\\_\\_American.Indian.Alaska.Native", "", pov_race_long$year)
pov_race_long$year <- as.numeric(gsub("X", "", pov_race_long$year))

pov_race_long$povrate_black[grepl("Black", pov_race_long$race_year)] <- pov_race_long$pov_rate[grepl("Black", pov_race_long$race_year)]
pov_race_long$povrate_latino[grepl("Hispanic", pov_race_long$race_year)] <- pov_race_long$pov_rate[grepl("Hispanic", pov_race_long$race_year)]
pov_race_long$povrate_native[grepl("American", pov_race_long$race_year)] <- pov_race_long$pov_rate[grepl("American", pov_race_long$race_year)]

data <- join(data, pov_race_long[!is.na(pov_race_long$povrate_black),c("state","year","povrate_black")])
data <- join(data, pov_race_long[!is.na(pov_race_long$povrate_latino),c("state","year","povrate_latino")])
data <- join(data, pov_race_long[!is.na(pov_race_long$povrate_native),c("state","year","povrate_native")])

rm(list=ls(pattern="pov_race")); gc()


#####Subsetting years

data <- data[data$year>=2000 & data$year<=2018,]

#eliminating DC
data <- data[data$st!="" & data$st!="DC",]


#########Imputing NAs##########

data <- data[order(data$state, data$year),]
data[, c(which(names(data) %in% variables$variable | names(data)%in% c("state", "pop_annual")))] <- ddply(data[, 
                                                                                                               c(which(names(data) %in% variables$variable | names(data)%in% c("state", "pop_annual")))], .(state), na.locf, na.rm=F)

data <- data[order(data$state, rev(data$year)),]
data[, c(which(names(data) %in% variables$variable | names(data)%in% c("state", "pop_annual")))] <- ddply(data[, 
                                                                                                               c(which(names(data) %in% variables$variable | names(data)%in% c("state", "pop_annual")))], .(state), na.locf, na.rm=F)


for(i in variables$variable){
  cat(i, "\n")
  
  data$temp <- as.numeric(data[,i])
  data$temp[is.na(data$temp)] <- mean(data$temp, na.rm=T)
  data[,i] <- data$temp
  
}
data$temp <- NULL



##########JUST ELECTORAL#############


###Coding to numeric
numeric_data <- data[,names(data) %in% c("state", "year", variables$variable[variables$paper_area=="Electoral"])]

numeric_data[,names(numeric_data) %in% variables$variable] <-
  apply(numeric_data[,names(numeric_data) %in% variables$variable], 2, function(x) as.numeric(as.character(x)))

write.csv(numeric_data[,names(numeric_data) %in% c("state", "year", variables$variable)], "check.csv")

mcmcmodel <- MCMCfactanal(as.matrix(numeric_data[,names(numeric_data) %in% variables$variable]),
                          factors=1, 
                          lambda.constraints = list(sdr=list(1,"+"),
                            EG_c=list(1,"-"),
                            absvot=list(1, "+"),
                            prov_rej_all=list(1,"-"),
                            residual=list(1,"-"),
                            uocava_nonret=list(1,"-"),
                            registration_drive_restriction=list(1,"-"),
                            website_provisional_status=list(1,"+")
                          ),     
                          burnin = 1000, mcmc = 1000,
                          thin=1,
                          store.scores=T, verbose=1
)


mcmc <- data.frame(summary(mcmcmodel)$statistics)
mcmc_scores <- mcmc[grepl("phi.", row.names(mcmc)), ]

#Loadings
mcmc_summary <- data.frame(summary(mcmcmodel)[[1]])
mcmc_summary$variable <- rownames(mcmc_summary)
mcmc_summary <- mcmc_summary[grepl("phi_",mcmc_summary$variable)==F,]
mcmc_summary$parameter[grepl("Lambda",mcmc_summary$variable)] <- "discrimination"
mcmc_summary$parameter[grepl("Lambda",mcmc_summary$variable)==F] <- "difficulty"
mcmc_summary$variable <- gsub("Lambda","",mcmc_summary$variable)
mcmc_summary$variable <- gsub("Psi","",mcmc_summary$variable)
mcmc_summary$variable <- gsub("_1","",mcmc_summary$variable)
mcmc_summary$cilow <- mcmc_summary$Mean - 1.96*mcmc_summary$Naive.SE
mcmc_summary$cihigh <- mcmc_summary$Mean + 1.96*mcmc_summary$Naive.SE

mcmc_summary$variable_full <- factor(mcmc_summary$variable, 
                                     levels=unique(mcmc_summary$variable[order(mcmc_summary$Mean)]))

mcmc_summary <- join(mcmc_summary, variables[,c("variable","description")])

mcmc_summary$description_full <- factor(mcmc_summary$description, 
                                        levels=unique(mcmc_summary$description[order(mcmc_summary$Mean)]))

data$democracy_mcmc <- mcmc_scores$Mean
data$democracy_mcmc_sd <- mcmc_scores$SD
data$democracy_mcmc_se <- mcmc_scores$Naive.SE



setwd("./output")

pdf("discrimination_loadings.pdf", h=7, w=7)
ggplot(mcmc_summary[mcmc_summary$parameter=="discrimination",], 
       aes(x=description_full, y=Mean)) +
  geom_point()  +
  geom_errorbar(aes(ymin=(Mean - 1.96*SD), 
                    ymax=(Mean + 1.96*SD)), width=0) +
  xlab("Variable") +
  ylab("Discrimination Parameter") +
  coord_flip() +
  geom_hline(yintercept=0, linetype=2) +
  theme_bw()
dev.off()


write.csv(mcmc_summary, "mcmc_paper_loadings.csv")



##########Additive index

additive_data <- numeric_data

sapply(additive_data, function(x)all(is.na(x)))

#scaling variables to range [0,1]
for(i in names(additive_data)[names(additive_data) %in% variables$variable]){
  
  cat(class(additive_data[,i]), "\n")
  
  
  additive_data[,i] <- (additive_data[,i] - min(additive_data[,i], na.rm=T)) / 
    (max(additive_data[,i], na.rm=T) - min(additive_data[,i], na.rm=T))
  
  
  
}

additive_data$democracy_additive <- rowSums(additive_data[,names(additive_data) %in% variables$variable[variables$direction=="+"]], 
                                            na.rm=T) -
  rowSums(additive_data[,names(additive_data) %in% variables$variable[variables$direction=="-"]], 
          na.rm=T)

additive_data$democracy_additive <- (additive_data$democracy_additive - 
                                       mean(additive_data$democracy_additive, na.rm=T)) / 
  sd(additive_data$democracy_additive, na.rm=T)

data$democracy_additive <- additive_data$democracy_additive








#############################ELECTORAL + LIBERAL#############################
###Coding to numeric
numeric_data <- data

numeric_data[,names(numeric_data) %in% variables$variable] <-
  apply(numeric_data[,names(numeric_data) %in% variables$variable], 2, function(x) as.numeric(as.character(x)))

write.csv(numeric_data[,names(numeric_data) %in% c("state", "year", variables$variable)], "check.csv")

mcmcmodel <- MCMCfactanal(numeric_data[,names(numeric_data) %in% variables$variable],
                             factors=1, 
                             lambda.constraints = list(sdr=list(1,"+"),
                                                       EG_c=list(1,"-"),
                                                       blackincarcerate=list(1, "-"),
                                                       prov_rej_all=list(1,"-"),
                                                       residual=list(1,"-"),
                                                       det_sentencing=list(1,"-")
                             ),     
                             data=numeric_data[,names(numeric_data) %in% variables$variable], 
                             burnin = 1000, mcmc = 1000,
                             thin=1,
                             store.scores=T, verbose=1
)


mcmc <- data.frame(summary(mcmcmodel)$statistics)
mcmc_scores <- mcmc[grepl("phi.", row.names(mcmc)), ]

#Loadings
mcmc_summary <- data.frame(summary(mcmcmodel)[[1]])
mcmc_summary$variable <- rownames(mcmc_summary)
mcmc_summary <- mcmc_summary[grepl("phi_",mcmc_summary$variable)==F,]
mcmc_summary$parameter[grepl("Lambda",mcmc_summary$variable)] <- "discrimination"
mcmc_summary$parameter[grepl("Lambda",mcmc_summary$variable)==F] <- "difficulty"
mcmc_summary$variable <- gsub("Lambda","",mcmc_summary$variable)
mcmc_summary$variable <- gsub("Psi","",mcmc_summary$variable)
mcmc_summary$variable <- gsub("_1","",mcmc_summary$variable)
mcmc_summary$cilow <- mcmc_summary$Mean - 1.96*mcmc_summary$Naive.SE
mcmc_summary$cihigh <- mcmc_summary$Mean + 1.96*mcmc_summary$Naive.SE

mcmc_summary$variable_full <- factor(mcmc_summary$variable, 
                                    levels=unique(mcmc_summary$variable[order(mcmc_summary$Mean)]))

mcmc_summary <- join(mcmc_summary, variables[,c("variable","description")])

mcmc_summary$description_full <- factor(mcmc_summary$description, 
                                     levels=unique(mcmc_summary$description[order(mcmc_summary$Mean)]))


pdf("discrimination_loadings_lib.pdf", h=7, w=7)
ggplot(mcmc_summary[mcmc_summary$parameter=="discrimination",], 
       aes(x=description_full, y=Mean)) +
  geom_point()  +
  geom_errorbar(aes(ymin=(Mean - 1.96*SD), 
                    ymax=(Mean + 1.96*SD)), width=0) +
  xlab("Variable") +
  ylab("Discrimination Parameter") +
  coord_flip() +
  geom_hline(yintercept=0, linetype=2) +
  theme_bw()
dev.off()


write.csv(mcmc_summary, "mcmc_paper_loadings_lib.csv")




data$democracy_mcmc_lib <- mcmc_scores$Mean
data$democracy_mcmc_lib_sd <- mcmc_scores$SD
data$democracy_mcmc_lib_se <- mcmc_scores$Naive.SE


ggplot(data[!is.na(data$partycontrol),
            c("st", "partycontrol","democracy_mcmc","year")], 
       aes(x=year, y=democracy_mcmc, color=factor(partycontrol))) +
  geom_line(aes(group=st), color="grey80") +
  geom_smooth(aes(group=factor(partycontrol)), se=F) +
  scale_color_manual(values=c("red","dark green","dodger blue"), name="", labels=c("Republican", "Divided", "Democratic")) +
  xlab("Year") +
  ylab("Democracy Score") +
  scale_x_continuous(breaks=c(2000,2005,2010,2015)) +
  theme_classic()



##########Additive index

additive_data <- numeric_data

sapply(additive_data, function(x)all(is.na(x)))

#scaling variables to range [0,1]
for(i in names(additive_data)[names(additive_data) %in% variables$variable]){
  
  cat(class(additive_data[,i]), "\n")
  

  additive_data[,i] <- (additive_data[,i] - min(additive_data[,i], na.rm=T)) / 
    (max(additive_data[,i], na.rm=T) - min(additive_data[,i], na.rm=T))
  

  
}

additive_data$democracy_additive_lib <- rowSums(additive_data[,names(additive_data) %in% variables$variable[variables$direction=="+"]], 
                                                 na.rm=T) -
  rowSums(additive_data[,names(additive_data) %in% variables$variable[variables$direction=="-"]], 
          na.rm=T)

additive_data$democracy_additive_lib <- (additive_data$democracy_additive - 
                                            mean(additive_data$democracy_additive, na.rm=T)) / 
  sd(additive_data$democracy_additive_lib, na.rm=T)

data$democracy_additive_lib <- additive_data$democracy_additive_lib

write.csv(data[,c("st","state","year","partycontrol",
                  "democracy_mcmc","democracy_mcmc_sd","democracy_mcmc_se","democracy_additive",
                  "democracy_mcmc_lib","democracy_mcmc_lib_sd","democracy_mcmc_lib_se","democracy_additive_lib")], 
          "democracy_measures_paper.csv")








##########Electoral WITHOUT VOTER ID#############


###Coding to numeric
numeric_data <- data[,names(data) %in% c("state", "year", variables$variable[variables$paper_area=="Electoral" & grepl("voterid",variables$variable)==F])]

numeric_data[,names(numeric_data) %in% variables$variable] <-
  apply(numeric_data[,names(numeric_data) %in% variables$variable], 2, function(x) as.numeric(as.character(x)))

write.csv(numeric_data[,names(numeric_data) %in% c("state", "year", variables$variable)], "check.csv")


mcmcmodel <- MCMCfactanal(as.matrix(numeric_data[,names(numeric_data) %in% variables$variable]),
                          factors=1, 
                          lambda.constraints = list(sdr=list(1,"+"),
                                                    EG_c=list(1,"-"),
                                                    absvot=list(1, "+"),
                                                    prov_rej_all=list(1,"-"),
                                                    #det_sentencing=list(1,"-"),
                                                    #postdna=list(1,"+"),
                                                    residual=list(1,"-"),
                                                    uocava_nonret=list(1,"-"),
                                                    registration_drive_restriction=list(1,"-"),
                                                    website_provisional_status=list(1,"+")
                          ),     
                          burnin = 1000, mcmc = 1000,
                          thin=1,
                          store.scores=T, verbose=1
)


mcmc <- data.frame(summary(mcmcmodel)$statistics)
mcmc_scores <- mcmc[grepl("phi.", row.names(mcmc)), ]

#Loadings
mcmc_summary <- data.frame(summary(mcmcmodel)[[1]])
mcmc_summary$variable <- rownames(mcmc_summary)
mcmc_summary <- mcmc_summary[grepl("phi_",mcmc_summary$variable)==F,]
mcmc_summary$parameter[grepl("Lambda",mcmc_summary$variable)] <- "discrimination"
mcmc_summary$parameter[grepl("Lambda",mcmc_summary$variable)==F] <- "difficulty"
mcmc_summary$variable <- gsub("Lambda","",mcmc_summary$variable)
mcmc_summary$variable <- gsub("Psi","",mcmc_summary$variable)
mcmc_summary$variable <- gsub("_1","",mcmc_summary$variable)
mcmc_summary$cilow <- mcmc_summary$Mean - 1.96*mcmc_summary$Naive.SE
mcmc_summary$cihigh <- mcmc_summary$Mean + 1.96*mcmc_summary$Naive.SE

mcmc_summary$variable_full <- factor(mcmc_summary$variable, 
                                     levels=unique(mcmc_summary$variable[order(mcmc_summary$Mean)]))

mcmc_summary <- join(mcmc_summary, variables[,c("variable","description")])

mcmc_summary$description_full <- factor(mcmc_summary$description, 
                                        levels=unique(mcmc_summary$description[order(mcmc_summary$Mean)]))



pdf("discrimination_loadings_novoterid.pdf", h=7, w=7)
ggplot(mcmc_summary[mcmc_summary$parameter=="discrimination",], 
       aes(x=description_full, y=Mean)) +
  geom_point()  +
  geom_errorbar(aes(ymin=(Mean - 1.96*SD), 
                    ymax=(Mean + 1.96*SD)), width=0) +
  xlab("Variable") +
  ylab("Discrimination Parameter") +
  coord_flip() +
  geom_hline(yintercept=0, linetype=2) +
  theme_bw()
dev.off()


write.csv(mcmc_summary, "mcmc_paper_loadings_novoterid.csv")




data$democracy_novoterid <- mcmc_scores$Mean

write.csv(data[,c("st","state","year","partycontrol","democracy_novoterid")], "measure_novoterid.csv", row.names = F)


setwd("..")
getwd()

