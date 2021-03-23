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
  
  
}