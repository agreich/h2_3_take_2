##HW 3 Alexandra Reich##

#problem 1
#read.csv
goa <- read.csv("/Users/alexandrareich/Desktop/Fish 622 QuanFish/HW/HW3/goa_race_specimen.csv", header=TRUE, skip=6)

#filter the data to only include northern Rockfish
#only include knwonw sex
# and known age and length
library(tidyverse)
library(dplyr)

nr.dat <- goa %>% filter(Common.Name=="northern rockfish",
                         !is.na(Age..years.), !is.na(Length..mm.),
                         Sex!="Unknown")
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
#male
rockfish_male_model <- mle2(
  
  
)

#female
rockfish_female_model
