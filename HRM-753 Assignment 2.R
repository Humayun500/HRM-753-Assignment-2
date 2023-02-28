save.image("C:/Users/humay/Dropbox/HRM/HRM 753/Assignments/Assignment data/HRM753_InfluenzaData.RData")

install.packages("pacman")
install.packages("tidyverse")

#consecutive.seasonal.vaccinations variable creation 
#(when vac0809=1 & intervention=TIV, made it 1, and when when vac0809=0 & intervention=placebo made it 0)
HRM753_InfluenzaData$vac0809 
HRM753_InfluenzaData$consecutive.seasonal.vaccinations = 
  case_when(
    HRM753_InfluenzaData$vac0809 == "1" & HRM753_InfluenzaData$intervention == "TIV" ~ "1",
    HRM753_InfluenzaData$vac0809 == "0" | HRM753_InfluenzaData$intervention == "placebo" ~ "0"
  )

#Question 1: the relationship between consecutive.seasonal.vaccinations and post-vaccination antibody response to seasonal influenza A/H1N1 with linear regression
# The variable for post vacc of influenza A/H1N1 is postvax.sH1 

head (HRM753_InfluenzaData$postvax.sH1)


#log transformation of the outcome HRM753_InfluenzaData$postvax.sH1)

HRM753_InfluenzaData$log.postvax.sH1 = log (HRM753_InfluenzaData$postvax.sH1)
sum(is.na (HRM753_InfluenzaData$log.postvax.sH1))

#To meet the assumption of normality histogram is drown, Shapiro Wilkinson test was done.

shapiro.test (HRM753_InfluenzaData$log.postvax.sH1)

HRM753_InfluenzaData %>% 
  ggplot (aes(x=log.postvax.sH1))+
  geom_histogram (binwidth=0.7, fill="#800000")+
  labs(x="Antibody response ",
       y= NULL,
       title= "Histogram of log-transformed antibody response to H1N1")


# in histogram the log outcome seems to be normal although there are some outlier in the left.  
# However, in the conservative Shapiro Wilkinson test the log outcome is not significantly normal. 

#simple linear regression 
lm.log.postvax.sH1= lm (log.postvax.sH1~consecutive.seasonal.vaccinations, data=HRM753_InfluenzaData)
summary (lm.log.postvax.sH1)

lm.log.postvax.sH1.exp= exp (lm.log.postvax.sH1)

# Question 2: 
# the following models using simple linear regression: 
#	Seasonal influenza A/H1N1 antibody response (Y) on age (X1 – continuous)
#	Seasonal influenza A/H1N1 antibody response (Y) on age (X2 – categorical (5-8 (child), 9-11(tween) and 12-17(teenager)))
#	Seasonal influenza A/H1N1 antibody response (Y) on sex (X3)

#categorical (5-8 (child), 9-11(tween) and 12-17(teenager)))
library(dplyr)
library(freqtables)
sum(is.na (HRM753_InfluenzaData$age))
summary (HRM753_InfluenzaData$age)
HRM753_InfluenzaData$age.cat= as.factor (cut (HRM753_InfluenzaData$age, breaks = c(4,8,11,17),
                     labels = c("child", "tween", "teenager"))) 

HRM753_InfluenzaData$age.cat

HRM753_InfluenzaData %>% 
  freq_table(age.cat)


lm.log.postvax.sH1.age= lm (log.postvax.sH1~age, data=HRM753_InfluenzaData)
summary (lm.log.postvax.sH1.age)
confint(lm.log.postvax.sH1.age, level=0.95)

lm.log.postvax.sH1.age.cat= lm (log.postvax.sH1~age.cat, data=HRM753_InfluenzaData)
summary (lm.log.postvax.sH1.age.cat)
confint(lm.log.postvax.sH1.age.cat, level=0.95)

lm.log.postvax.sH1.sex= lm (log.postvax.sH1~male, data=HRM753_InfluenzaData)
summary (lm.log.postvax.sH1.sex)
confint(lm.log.postvax.sH1.sex, level=0.95)



#Question 4: Using your new natural log-transformed outcome, determine the slopes and intercept estimates for the following model using multiple linear regression
#Seasonal influenza A/H1N1 antibody response (Y) on 2 consecutive seasonal vaccinations (X1), age (X2 - continuous), and sex (X3)
lm.log.postvax.sH1.multiple= lm (log.postvax.sH1~consecutive.seasonal.vaccinations+age+male, data=HRM753_InfluenzaData)
summary (lm.log.postvax.sH1.multiple)
confint(lm.log.postvax.sH1.multiple, level=0.95)

t.test (age~consecutive.seasonal.vaccinations, data=HRM753_InfluenzaData)
t.test (age~male, data=HRM753_InfluenzaData)
chisq.test (HRM753_InfluenzaData$consecutive.seasonal.vaccinations, HRM753_InfluenzaData$male)

#You suspect that age is related to antibody response differently in males and females.
#Create a plot to visualize the relationship between age (continuous) and your transformed post-vaccination antibody response to seasonal influenza A/H1N1 data. 
#Use a different symbol for males and females. 
#How can you test your suspicion? Correctly specify the hypotheses. Interpret your findings. 


library(ggplot2)

ggplot(HRM753_InfluenzaData)+
  aes(age, log.postvax.sH1, color = male) +
  geom_point() +
  geom_smooth(method='lm',se=F)+
  labs (x= "Age in year",
        y= "Log of antibody response",
        title= "",
        fill = "Sex"
        )

data.male <- HRM753_InfluenzaData[ which(HRM753_InfluenzaData$male=='1'), ]
lm.log.postvax.sH1.age.male= lm (log.postvax.sH1~age, data=HRM753_InfluenzaData)

summary (lm.log.postvax.sH1.age.male)
view (data.male$male)
data.male$male

data.female <- HRM753_InfluenzaData[ which(HRM753_InfluenzaData$male=='0'), ]
view (data.female$male)

lm.log.postvax.sH1.age.female= lm (log.postvax.sH1~age, data=HRM753_InfluenzaData)
summary (lm.log.postvax.sH1.age.female)
data.female$male

