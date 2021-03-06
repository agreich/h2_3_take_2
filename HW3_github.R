##HW 3 Alexandra Reich##

#problem 1
#read.csv
goa <- read.csv("/Users/alexandrareich/Desktop/Fish 622 QuanFish/HW/HW3/goa_race_specimen.csv", header=TRUE, skip=6)

#filter the data to only include northern Rockfish
#only include knwonw sex
# and known age and length
library(tvthemes)
library(tidyverse)
library(dplyr)
library(manipulate)
library(bbmle)
library(visreg) 
library(ggthemes)
library(manipulate)
library(cowplot)

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
t0_m <-coef(rockfish_male_model)[3]
sigma_m <- exp(coef(rockfish_male_model)[4])

Linf_f <- exp(coef(rockfish_female_model)[1])
k_f <- exp(coef(rockfish_female_model)[2])
t0_f <- coef(rockfish_female_model)[3]
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
pred_length_M <- pred_LVB(age=obs.age_m, Linf=Linf_m, k=k_m, t0=t0_m)
pred_length_F <- pred_LVB(age=obs.age_f, Linf=Linf_f, k=k_f, t0=t0_f)

#male plot
ggplot()+geom_point(aes(x=obs.age_m, y=obs.length_m)) + geom_line(aes(x=obs.age_m, y=pred_length_M), color="red")+
  xlab("Age")+ylab("Length") +theme_cowplot()
#female plot
ggplot()+geom_point(aes(x=obs.age_f, y=obs.length_f)) +geom_line(aes(x=obs.age_f, y=pred_length_F), color="red") +
  xlab("Age") +ylab("Length")+theme_cowplot() #+
  #scale_color_avatar(pallete="FireNation")+
  #theme_avatar()









###PART 3##### Pollock Per-Recruit Analysis
#what age is biomass maxxed?
# first develop an approx (predicted?) for pollock weight at age W(t)
#load pollock data
pol.dat <- read.csv(file="/Users/alexandrareich/Desktop/Fish 622 QuanFish/HW/HW3/pollock_race_specimen.csv", header=TRUE, skip=6)

#subset for complete observations
pol.dat <- pol.dat %>% filter(!is.na(Age..years.),
                              !is.na(Length..mm.),
                              !is.na(Weight..gm.))

#create a function called pred_wl
pred_wl <- function (L, alpha, beta){
  weight <- alpha*(L^beta)
  return(weight)
}

test <- pred_wl(L=obs.length_3, alpha=0.00005, beta=3)
test
#create a NLL function
NLL_wl <- function (ln_alpha, ln_beta, ln_sigma_3, obs.length_3, obs.weight_3){
  #exponentiate the thing
  alpha <- exp(ln_alpha)
  beta <- exp(ln_beta)
  sigma_3 <- exp(ln_sigma_3)
  
  # Create model predictions
  pred.weight <- pred_wl(L=obs.length_3, alpha=alpha, beta=beta)
  
  # Calculate log-likelihood (from a lognormal distribution)
  #  NOTE: offset, because there are age-0 fish and log(0)= -Infinity
  logLike <- dnorm(x=log(obs.weight_3 +1e-6), mean=log(pred.weight +1e-6), sd=sigma_3, log=TRUE) #so why is observed and predicted where they are?
  
  # Calculate total negative log likelihood
  NLL <- -1*sum(logLike, na.rm=TRUE) # We need to add na.rm as there are some NA's for weights in the dataset
  return(NLL)
}

#3.4 fit the model using MLE2
#report the estimated parameter values in the word doc
#give data to the data
obs.weight_3 <- pol.dat$Weight..gm.
obs.length_3 <- pol.dat$Length..mm.

model_wl <- mle2(NLL_wl,
                 start=list(ln_alpha = log(0.00005), ln_beta=log(3), ln_sigma_3=log(2)), #maybe try log 2
                 data = list (obs.length_3=obs.length_3, obs.weight_3=obs.weight_3),
                 method = "Nelder-Mead",
                 optimizer= "nlminb",
                 control=list(maxit=1e6)
) #what to start alpha and beta? beta is typically around 3. But what for alpha?
summary(model_wl)
coef(model_wl)
# Explore with manipulate()
#manipulate(plot_wl(data=pollock, alpha, beta), 
#           alpha = slider(min=0, max=1e-5, initial=7e-6, step=1e-7),
 #          beta = slider(min=2, max=4, initial=3, step=0.001)
#)
#report your values to word doc!
#get the values
alpha <- exp(coef(model_wl)[1])
beta <- exp(coef(model_wl)[2])
sigma_3 <- exp(coef(model_wl)[3])
  
alpha
beta
sigma_3


#3.5 GRAPH
pred_model_weight <- pred_wl(L=obs.length_3, alpha=alpha, beta=beta)

ggplot()+geom_point(aes(x=obs.length_3, y=obs.weight_3)) + geom_line(aes(x=obs.length_3, y=pred_model_weight), color="red")+
  xlab("Weight")+ylab("Length")

######3.6 VON BERT
#predLVB function should still work here...
#pred_LVB(age,Linf, k, t0)
#will NLL function work?
#I think it will!
obs.age_3 <- pol.dat$Age..years.
pollock_vonBert <- mle2(NLL_LVB,
                        start=list(ln_Linf=log(700), ln_k=log(0.2), t0=0, ln_sigma=log(0.5)),
                        data = list (obs.age=obs.age_3, obs.length=obs.length_3),
                        method = "Nelder-Mead",
                        optimizer= "nlminb",
                        control=list(maxit=1e6)
                        )
summary(pollock_vonBert)
coef(pollock_vonBert)

#get the coefficients
Linf_p <- exp(coef(pollock_vonBert)[1])
k_p <- exp(coef(pollock_vonBert)[2])
t0_p <- coef(pollock_vonBert)[3]
sigma_p <- exp(coef(pollock_vonBert)[4])

Linf_p 
k_p 
t0_p 
sigma_p

#plot the von Bert
pred_pollock_length_vonBert <- pred_LVB(pol.dat$Age..years., Linf_p, k_p, t0_p)

ggplot()+geom_point(aes(x=pol.dat$Age..years., y=obs.length_3)) +geom_line(aes(x=pol.dat$Age..years., y=pred_pollock_length_vonBert), color="red") +
  xlab("Age") +ylab("Length")+theme_cowplot() #+




#STARTING THE PART WITHOUT NUMBERS
ages <- c(1:15)
ages
#predict the weight at age for this range of ages
length_at_age <- pred_LVB(ages, Linf_p, k_p, t0_p)
waa <- pred_wl(length_at_age, alpha, beta)
waa

ages2 <- c(0:14)
ages2
#the weird part in pink
#create a vector N the length of ages
#N <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#length(N)

#N1 <- 1000
M <- 0.3
#instantaneous natural mortality formula
#N0*exp(-Zt)
Nt <- N1 *exp(-M*ages2)
Nt

#the for loop, which I'm choosing not to use:



#calculate expected Biomass at age B(t)
B <- waa * Nt
max(B)
B  
  

#plot weight, age, biomass
#par(mrow=c(3,1))
#make three plots, and then combine into one with GGplot

#plot numbers(Nt) at age
p1 <- ggplot()+aes(x=ages, y=Nt)+geom_point()+theme_cowplot() +xlab("Age") +ylab("N(t)")

#plot weight at age
p2 <- ggplot()+aes(x=ages, y=waa)+geom_point()+theme_cowplot() +xlab("Age") +ylab("W(t)")

#plotbiomass at age
p3 <- ggplot()+aes(x=ages, y=B)+geom_point()+theme_cowplot() +xlab("Age") +ylab("B(t)")

#combine into a three panel figure
plot_grid(p1,p2,p3, nrow=3, ncol=1)


#repeat process but natural mort is 0.9 for age-1, 0.45 for age 2, 0.3 for ages 3+
M1 <- 0.9
M2 <- 0.45
M3 <- 0.3

#ugh, now I need a for loop
N_diff_morts <- vector(length=15)
N_diff_morts[1] <-1000
N_diff_morts[2] <-N_diff_morts[1]*exp(-M1)
N_diff_morts[3] <-N_diff_morts[2]*exp(-M2)
for (i in 3:(length(N_diff_morts)-1)){
  N_diff_morts[i+1] <- N_diff_morts[i]*exp(-M3)
  print(i)
}
N.2 <- N_diff_morts

#waa should be the same?
waa

#calc biomass
B.2 <- N.2 *waa
B.2

#plot the things
#plot numbers at age
p1 <- ggplot()+aes(x=ages, y=N.2)+geom_point()+theme_cowplot() +xlab("Age") +ylab("N(t)")

#plot weight at age
p2 <- ggplot()+aes(x=ages, y=waa)+geom_point()+theme_cowplot() +xlab("Age") +ylab("W(t)")

#plotbiomass at age
p3 <- ggplot()+aes(x=ages, y=B.2)+geom_point()+theme_cowplot() +xlab("Age") +ylab("B(t)")

#combine into a three panel figure
plot_grid(p1,p2,p3, nrow=3, ncol=1)