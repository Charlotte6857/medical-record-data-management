#Course: Data Management and Statistic Computing
#Analysis: Final Assignment
#Created by: lc3297
#Created on: 12/13/2020
rm (list = ls())
#install.packages("readxl")
install.packages("readxl")
library("readxl")
#0)set working directory
setwd("~/Desktop/Data management final")
#1)Combine sample1, sample2, and sample3 datasets and drop duplicate observations.
sample1<-read.csv(file="sample1.csv")
sample2<-read.csv(file="sample2.csv")
sample3<-read.csv(file="sample3.csv")
sample4<-rbind(sample1, sample2,sample3)
sample4a <- unique(sample4)
#2)Merge in the combined sample with the medical record data. Only keep observations with both
#survey (the combined sample) and medical records.
med_rec <- read.csv(file="med_record.csv", header=TRUE, sep=",")
sample5 <- merge(sample4a, med_rec, by="id")
#3)Create sex variable and format the values (quest_61; 1=male, 0=female). How many females in the
#sample? *Note: include the answer as a comment after the corresponding statements
sample5$sex<-sample5$quest_61
install.packages("expss") 
library(expss)
val_lab(sample5$sex) = num_lab("1 male
                               0 female")
table(sample5$sex)
#There are 282 female
#4)Calculate the Rapid Estimate of Adult Literacy in Medicine – Revised (REALM-R) score. 
sample5$REALMR <- (sample5$quest_r1 + sample5$quest_r2 + sample5$quest_r3+ sample5$quest_r4+sample5$quest_r5
                    +sample5$quest_r6+ sample5$quest_r7+ sample5$quest_r8)
table(sample5$REALMR)
#5)Label the REALM-R score variable
install.packages("Hmisc")
library(Hmisc)
label(sample5$REALMR) <- "REALM-R score"

#6)Create a frequency histogram of the REALM-R score
hist(sample5$REALMR,main="")
#7)Create a categorical REALM Score variable. Health literacy is stratified into three categories: (0-3)
#High likelihood of limited literacy, (4-6) Possibility of limited literacy, (7-8) Adequate literacy.
sample5$REALMR_cat = cut(sample5$REALMR, 
                      c(0, 4, 7, 9),
                      right=FALSE)
table(sample5$REALMR_cat)
sample5$REALMR_cat = cut(sample5$REALMR, 
                         c(0, 4, 7, 9),
                         right=FALSE,labels=c(1:3))
table(sample5$REALMR_cat)
#8)Format the categorical REALM score variable
sample5$REALMR_cat <- ordered(sample5$REALMR_cat,
                           levels = c(1,2, 3),
                           labels = c("High likelihood of limited literacy", "Possibility of limited literacy", "Adequate literacy"))

table(sample5$REALMR_cat)
#9)Create a categorical body mass index (BMI) variable with 3 categories: under/normal weight (<25),
#overweight (25-30), and obese (≥30).
sample5$bmi_cat = cut(sample5$bmi_survey, 
                       c(0, 25, 30, 120), 
                       right=FALSE)

table(sample5$bmi_cat)
#10)Format the categorical BMI variable
sample5$bmi_cat = cut(sample5$bmi_survey, c(0, 25, 30, 120), right=FALSE, labels=c(1:3))
sample5$bmi_cat <- ordered(sample5$bmi_cat,
                            levels = c(1,2, 3),
                            labels = c("under/normal weight", "overweight", "obese"))

table(sample5$bmi_cat)

#11)Format the race_cat variable; 1=Non-Hispanic White, 2 =Black, including multi, 3=Other/Unknown 
library(expss)

val_lab(sample5$race_cat) = num_lab("1 non-hispanic white
                                    2 black
                                    3 other/unknown")

#12) How many Non-Hispanic White males are in the sample?
table(sample5$race_cat, sample5$sex)
# Answer: There are 40 Non-Hispanic White males.

#13)Create an index of the number of chronic diseases that includes diabetes (pers_diabetes), asthma (quest_36e), hypertension (pers_hypertension), and heart disease (quest_32). See note (*) in #4. 
sample5$cdi <- (sample5$pers_diabetes.x + sample5$quest_36e + sample5$pers_hypertension + sample5$quest_32)

#14)What is the mean BMI of black males? See note (*) in #3.
mean(sample5$bmi_survey[sample5$race_cat==2 & sample5$sex==1],na.rm = T)
# Answer: The mean BMI of black males is 30.41899.

#15)How many white males are obese? See note (*) in #3. 
table(sample5$bmi_cat, sample5$race_cat, sample5$sex)
# There are 48 White males are obese.

#16) Tabulate the REALM-R categorical variable by race_cat (r x c table). 
table(sample5$REALMR, sample5$race_cat)

#17)What percent of the other/unknown race group persons have a high likelihood of limited health literacy? See note (*) in #3. 
tab1<-table(sample5$REALMR_cat, sample5$race_cat, exclude=NULL)
prop.table(tab1,2)
# Answer: The percent of the other/unknown is 22.5%.

#18)Calculate the mean number of chronic diseases by gender. 
aggregate(cdi ~ sex, sample5, mean)
#answer:sex      cdi
#        0 1.709677
#        1 1.201681

#19)Calculate the median and interquartile range of the continuous REALM-R score by categorical BMI. 
aggregate(REALMR~ bmi_cat, sample5, median)
#answer:
#           bmi_cat REALMR
# under/normal weight      8
#          overweight      6
#               obese      7
aggregate(REALMR~ bmi_cat, sample5,  IQR)
#           bmi_cat REALMR
# under/normal weight    2.0
#          overweight    4.5
#               obese    4.0

#20)Limit the sample to women and create an indicator variable for overdue pap smear (last_pap>1). Tabulate the overdue paper variable. 
sample5$overdue_pap_smear = cut(sample5$last_pap, breaks=c(-Inf,1,Inf),labels=c("normal","overdue_pap_smear"))
sample5$overdue_pap_smear = ifelse(sample5$sex==1, NA, sample5$overdue_pap_smear)
table(sample5$overdue_pap_smear)

#21)Save the dataset. 
save(sample5, file = "sample5.Final")

#22)Create a variable that stores the leading digit of coph_visits and show the frequencies of the leading digits. 
sample5$leading_digit <- substr(sample5$coph_visits,1,1)
table(sample5$leading_digit)

#23)Limit the sample to records with even id numbers. 
sample5 <- sample5[sample5$'id' %% 2 ==0,]

#or

install.packages("tidyverse")
library(dplyr)
sample5 %>% filter(id %% 2 == 0)











