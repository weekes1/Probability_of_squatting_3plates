library(tidyverse)
library(mosaic)
library(Stat2Data)
library(readr)
athletes <- read_csv("Data Analysis/P_of_squatting_315/athletes.csv")


head(athletes)

#######################################clean the Data set####################################

athletes<-athletes%>%
  select(athlete_id,gender,age,height,weight,howlong,backsq,schedule)%>%
  filter(!is.na(schedule),!is.na(gender),age<80, !is.na(age),height>48,height<100,
         !is.na(height),weight<400,weight>100,!is.na(weight),!is.na(howlong),!is.na(backsq),backsq!=0,is.numeric(backsq))

head(athletes)
#58689 observations left

#creating binary for 315 bcksq
back_315<-c()

nrow(athletes)
for(i in 1:nrow(athletes)){
  if(athletes$backsq[i]>=315){
    back_315[i]=1
  }else{
    back_315[i]=0
  }
}

back_315

library(stringr)


athletes<-athletes%>%
  mutate(back_315=back_315,
         howlong =  str_extract(howlong, "[^|]+"),
         schedule =  str_extract(schedule, "[^|]+"))

head(athletes)
nrow(athletes)#58689 observations left

write_csv(athletes,"C:\\Users\\weeke\\OneDrive\\Documents\\Data Analysis\\P_of_squatting_315\\cleaned_athletes.csv")
#######################################EDA####################################

#summary statistics

favstats(back_315~age,data=athletes)
favstats(back_315~gender,data=athletes)
favstats(back_315~howlong,data=athletes)
favstats(back_315~schedule,data=athletes)

#quantitative predictors:
boxplot(age~back_315, data=athletes, main="Back Squatters O/U 315 vs Age ",
        xlab="315 Back Squat",
        ylab="Age",
        sub=substitute(paste(bold("Figure 1"))))

t.test(age~back_315, data=athletes)

boxplot(weight~back_315, data=athletes, main="Back Squatters O/U 315 vs Weight ",
        xlab="315 Back Squat",
        ylab="Weight",
        sub=substitute(paste(bold("Figure 2"))))
t.test(weight~back_315, data=athletes)

boxplot(height~back_315, data=athletes, main="Back Squatters O/U 315 vs Height ",
        xlab="315 Back Squat",
        ylab="Height",
        sub=substitute(paste(bold("Figure 3"))))
t.test(height~back_315, data=athletes)

#categorical
table(athletes$gender,athletes$back_315)
prop.table(tally(back_315~gender, data=athletes),2)

table(athletes$howlong,athletes$back_315)
prop.table(tally(back_315~howlong, data=athletes),2)

table(athletes$schedule,athletes$back_315)
prop.table(tally(back_315~schedule, data=athletes),2)

bargraph(~back_315, groups = gender, 
         type="proportion", data=athletes)

bargraph(~back_315, groups = howlong, 
         type="proportion", data=athletes)

bargraph(~back_315, groups = schedule, 
         type="proportion", data=athletes,main="Proportion O/U 315 for Training Frequency ",
         xlab="315 Back Squat",
         ylab="Proportion",
         sub=substitute(paste(bold("Figure 6"))))

head(athletes)

# interaction 
boxplot(age~factor(gender)*factor(back_315), data=athletes)
boxplot(age~factor(howlong)*factor(back_315), data=athletes)
boxplot(age~factor(schedule)*factor(back_315), data=athletes)

boxplot(height~factor(gender)*factor(back_315), data=athletes,main="Interaction Boxplot for Height and Gender",
        xlab="Gender.315er",
        ylab="Height",
        sub=substitute(paste(bold("Figure 7"))))
boxplot(height~factor(howlong)*factor(back_315), data=athletes)
boxplot(height~factor(schedule)*factor(back_315), data=athletes)

boxplot(weight~factor(gender)*factor(back_315), data=athletes)
boxplot(weight~factor(howlong)*factor(back_315), data=athletes)
boxplot(weight~factor(schedule)*factor(back_315), data=athletes)

emplogitplot2(back_315~weight+factor(howlong), data=athletes, 
              putlegend = "topleft", ngroups=2,main="Figure 8: Logit Plot for Weight and How long",
              xlab="Weight",
              ylab="Log(odds)")

emplogitplot2(back_315~weight+factor(schedule), data=athletes, 
              putlegend = "topleft", ngroups=2)

emplogitplot2(back_315~age+factor(howlong), data=athletes, 
              putlegend = "topleft", ngroups=2)

emplogitplot2(back_315~age+factor(schedule), data=athletes, 
              putlegend = "topleft", ngroups=2)

emplogitplot2(back_315~height+factor(howlong), data=athletes, 
              putlegend = "topleft", ngroups=2)

emplogitplot2(back_315~height+factor(schedule), data=athletes, 
              putlegend = "topleft", ngroups=2)
#tranformations

emplogitplot1(back_315~age, data=athletes)

emplogitplot1(back_315~age, data=athletes,ngroups = 6,main="Figure 9: Logit Plot for Age",
              xlab="Age",
              ylab="Log(odds) of 315er")

emplogitplot1(back_315~age, data=athletes,ngroups = 9)
#passes

emplogitplot1(back_315~weight, data=athletes)
emplogitplot1(back_315~weight, data=athletes,ngroups = 6,main="Figure 10: Logit Plot for Weight",
              xlab="Weight",
              ylab="Log(odds) of 315er")
#passes


emplogitplot1(back_315~height, data=athletes)
emplogitplot1(back_315~height, data=athletes,ngroups = 6)
emplogitplot1(back_315~height, data=athletes,ngroups = 9,main="Figure 11: Logit Plot for Height",
              xlab="Height",
              ylab="Log(odds) of 315er")
emplogitplot1(back_315~height, data=athletes,ngroups = 10)
#passes

######################################Model Selection####################################

 
fullmod = glm(back_315~age+height+gender+weight+howlong+schedule, family="binomial", data=athletes)

nullmod = glm(back_315~1, family="binomial", data=athletes)

step(fullmod, scope=list(lower=formula(nullmod)), direction="backward")
step(nullmod, scope=list(lower=formula(nullmod), 
                         upper=formula(fullmod)), direction="both")


install.packages("performance")
library(performance)
check_collinearity(fullmod)

mod<-coefficients((fullmod))
mod

######################################Use questions####################################

summary(fullmod)
OR_experience<-exp(1.2359263)/exp(-0.7815838)
OR_experience

OR_schedule<-exp(0.5789634)/exp(0.3462975)
OR_schedule

OR_gender<-exp(6.4310990)/exp(6.4310990-5.6902899)
OR_gender

OR_height<-exp(-0.1674201*72)/(exp(-0.1674201*80))
OR_height


OR_weight<-exp(0.0447357*220)/(exp(0.0447357*180))
OR_weight

OR_age<-exp(-0.0708355*23)/(exp(-0.0708355*55))
OR_age

OR_age<-exp(-0.0708355*13)/(exp(-0.0708355*45))
OR_age

######################################How well does the model work####################################

nrow(athletes)
set.seed(1)

test_id=sample(1:58689,5868, replace=FALSE) #Sample  unique observations from 1 to 13932
test=athletes[test_id,] #Takes rows corresponding to test_id and store it
train=athletes[-test_id,] #Takes row that do not correspond to test_id and store it

#train

train_mod<-glm(back_315~age+height+gender+weight+howlong+schedule, family="binomial", data=train)

preds<-predict.glm(train_mod,test)
pred_prob<-exp(preds)/(1+exp(preds))
pred_prob

for(i in 1:5868){
  if(pred_prob[i]>=0.5){
    pred_prob[i]=1
  }else{
    pred_prob[i]=0
  }
}

pred_prob_0s<-0
pred_prob_1s<-0

for(i in 1:5868){
  if(pred_prob[i]==1){
    pred_prob_1s= pred_prob_1s+1 
  }
  if(pred_prob[i]==0){
    pred_prob_0s= pred_prob_0s+1 
  }
  
}

pred_prob_0s
pred_prob_1s

actual<-athletes[test_id,]$back_315
actual

actual_prob_0s<-0
actual_prob_1s<-0

for(i in 1:5868){
  if(actual[i]==1){
    actual_prob_1s= actual_prob_1s+1 
  }
  if(actual[i]==0){
    actual_prob_0s= actual_prob_0s+1 
  }
  
}

actual_prob_0s
actual_prob_1s

predsol<-(pred_prob>0.5)
tally(~predsol+actual)

