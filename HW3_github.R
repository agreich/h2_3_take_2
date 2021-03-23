##HW 3 Alexandra Reich##

#problem 1
#read.csv
goa <- read.csv("/Users/alexandrareich/Desktop/Fish 622 QuanFish/HW/HW3/goa_race_specimen.csv", header=TRUE, skip=6)

#filter the data to only include northern Rockfish
#only include knwonw sex
# and known age and length
library(tidyverse)
library(dplyr)
library(manipulate)
library(bbmle)
library(visreg) 
library(ggthemes)
library(manipulate)

nr.dat <- goa %>% filter(Common.Name=="northern rockfish",
                         !is.na(Age..years.), !is.na(Length..mm.),
                         Sex!="Unknown")
nr.dat_m <- nr.dat %>% filter(Sex=="Male")
nr.dat_f <-nr.dat %>% filter(Sex=="Female")

#3
#create a von Bert function
#first, predict age, says the corresponding lab (5)
pred_LVB <- function(age, Linf, k, t0) {
  pred.length <- Linf*(1-exp(-k*(age-t0))) #just the Von Bert equation
  return(pred.length)
}

#4
NLL_LVB <- function(ln_Linf, ln_k, t0, ln_sigma, obs.age, obs.length){
 #exponentiate parameters
  Linf <- exp(ln_Linf)
  k <- exp(ln_k)
  sigma <- exp(ln_sigma)
  
  # Extract Observed lengths and weights ,- NOPE, these are given in formula 
#obs.age <- data$Age..years.
 #obs.length <- data$Length..mm.
  
  # Create model predictions
  pred.length <- pred_LVB(age=obs.age, Linf=Linf, k=k, t0=t0)
  
  # Calculate log-likelihood (from a lognormal distribution)
  #  NOTE: offset, because there are age-0 fish and log(0)= -Infinity
  
  logLike <- dnorm(x=log(obs.length +1e-6), mean=log(pred.length +1e-6), sd=sigma, log=TRUE)
  
  # Calculate total negative log likelihood
  NLL <- -1*sum(logLike, na.rm=TRUE) # We need to add na.rm as there are some NA's for weights in the dataset
  return(NLL)
}

#5fit data to male and female Northern Rockfish using mle2
#starting values
obs.age_m <- nr.dat_m$Age..years.
obs.length_m <- nr.dat_m$Length..mm.

obs.age_f <- nr.dat_f$Age..years.
obs.length_f <- nr.dat_f$Length..mm.


#?????????????????????????????????????????/
#male #SHOULD t0 NOT BE ESIMATED IN START???????
rockfish_male_model <- mle2( NLL_LVB,
                             start=list(ln_Linf=log(450), ln_k=log(0.2), t0=0, ln_sigma=log(0.5)),
                             data = list (obs.age=obs.age_m, obs.length=obs.length_m),
                             method = "Nelder-Mead",
                             optimizer= "nlminb",
                             control=list(maxit=1e6)
)

#female
rockfish_female_model <- mle2( NLL_LVB,
                               start=list(ln_Linf=log(500), ln_k=log(0.2), t0=0, ln_sigma=log(0.5)),
                               data = list (obs.age=obs.age_f, obs.length=obs.length_f),
                               method = "Nelder-Mead",
                               optimizer= "nlminb",
                               control=list(maxit=1e6)
)

summary(rockfish_male_model)
summary(rockfish_female_model)

#6: extract parameter values from your pair of fitted models
Linf_m <- exp(coef(rockfish_male_model)[1])
k_m <- exp(coef(rockfish_male_model)[2])
t0_m <- exp(coef(rockfish_male_model)[3])
sigma_m <- exp(coef(rockfish_male_model)[4])

Linf_f <- exp(coef(rockfish_female_model)[1])
k_f <- exp(coef(rockfish_female_model)[2])
t0_f <- exp(coef(rockfish_female_model)[3])
sigma_f <- exp(coef(rockfish_female_model)[4])

Linf_m 
k_m 
t0_m 
sigma_m

Linf_f 
k_f 
t0_f 
sigma_f 

#7 create a table of the parameter estimates and put this in the word doc.
##BUT, SHOULD t0 BE ESTIMATED? ASK DUIRNG OFFICE HOURS!!!

#8 plot for each sex as points, models as lines. Like last time


###PART 2##### Pollock Spawning Biomass calc
