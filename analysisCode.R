##########  Author and Title ###########
### Phillips, J & Bloom, P.  ##
### Do children think immoral events are magical? ##
### ### ### ### ### ### ### ### ### ###       

#### load data and packages####
rm(list=ls())
setwd("C:/Users/Jonathan/Documents/currentProjects/Paul Bloom/Deontic Restriction/Materials_for_publication")

# packages
require(lsr)
require(ggplot2)
require(lme4)

## this is the last problem that needs to be fixed:
require(reshape2)
require(plyr)
require(dplyr)
require(tidyr)

#### Study 1: Judgments of possibility ####

## Developmental data
d1.1 <- read.csv("Data/study1_kids.csv")
d1.1$possibility.judgment <- d1.1$possibility.judgment-6 ## to  make response values easier to work with

## create the trial vector
d1.trial <- gather(d1.1[,c(1,10:17)],trial,scenario,-subj)
d1.trial <- d1.trial[seq(1,length(d1.trial$subj),8),]
d1.trial <- d1.trial[order(d1.trial$subj,d1.trial$scenario),]
d1.1 <- d1.1[order(d1.1$subj,d1.1$scenario),]
d1.1$trial <- d1.trial$trial
d1.1$trial <- substring(d1.1$trial,6,6)
d1.1 <- d1.1[d1.1$age<8,-c(7:17)] # delete redundant trial info, etc. and drops two 8 yr olds

## Adult data
d1.2 <- read.csv("Data/study1_adults.csv")
print(c(mean(d1.2$age[d1.2$age!=999]),sd(d1.2$age[d1.2$age!=999]))) ### the one participant removed failed to give an age and was coded 999

d1 <- rbind(d1.1,d1.2)
# set up factors
d1$sex <- factor(c("M","F")[d1$sex])
d1$scenario <- factor(c("Candy Bar","Ball","T-Shirt","School","Basketball","Best Friend","Movie","Chores")[d1$scenario])
d1$violation.type <- factor(c("Physical violation","Probability violation","Moral violation","Non-violation")[d1$violation.type])
d1$violation.type <- factor(d1$violation.type, levels=c("Physical violation","Probability violation","Non-violation","Moral violation"))

d1$age.group[d1$age > 17] <- "Adults"
d1$age.group[d1$age==4 | d1$age==5] <- "4-5 yrs"
d1$age.group[d1$age==6 | d1$age==7] <- "6-7 yrs"
table(d1$age.group)/8

##Overall analysis with random effects for subject and story

###Base Model
lm1.1 <- lmer(possibility.judgment ~ age.group + violation.type + (1|scenario) + (violation.type|subj), data=d1)
###Main effect of Age:
lm1.2 <- lmer(possibility.judgment ~ violation.type + (1|scenario) + (violation.type|subj), data=d1)
anova(lm1.1,lm1.2)
###Main effect of Violation Type: 
lm1.3 <- lmer(possibility.judgment ~ age.group + (1|scenario) + (violation.type|subj), data=d1)
anova(lm1.1,lm1.3)
###Age * Violation Type Interaction: 
lm1.4 <- lmer(possibility.judgment ~ age.group * violation.type + (1|scenario) + (violation.type|subj), data=d1)
anova(lm1.1,lm1.4)
summary(lm1.4)

## Chi-squared pairwise comparisons 
## 4- to 5-year-olds
### Physical vs. Moral
table(d1$possibility.judgment[d1$age.group=="4-5 yrs" & d1$violation.type=="Physical violation"])
table(d1$possibility.judgment[d1$age.group=="4-5 yrs" & d1$violation.type=="Moral violation"])
chisq1.1 <- matrix(c(25,64,30,57), ncol=2) 
chisq.test(chisq1.1) # X-squared = 0.5658, df = 1, p-value = 0.4519
cramersV(chisq1.1) # Cramer's V = 0.05669774
### Physical vs. Probability
table(d1$possibility.judgment[d1$age.group=="4-5 yrs" & d1$violation.type=="Probability violation"])
chisq1.2 <- matrix(c(25,64,26,42), ncol=2)
chisq.test(chisq1.2) # X-squared = 1.3761, df = 1, p-value = 0.2408
cramersV(chisq1.2) # Cramer's V = 0.09362072
### Moral vs. Probability 
chisq1.3 <- matrix(c(30,57,26,42), ncol=2)
chisq.test(chisq1.3) # X-squared = 0.0987, df = 1, p-value = 0.7534
cramersV(chisq1.3) # Cramer's V =  0.02523145
### Physical vs. Non-violation
table(d1$possibility.judgment[d1$age.group=="4-5 yrs" & d1$violation.type=="Non-violation"])
chisq1.4 <- matrix(c(25,64,50,26), ncol=2)
chisq.test(chisq1.4) # X-squared = 22.0035, df = 1, p-value = 2.722e-06
cramersV(chisq1.4) # Cramer's V = 0.365177
### Probability vs. Non-violation
chisq1.5 <- matrix(c(26,42,50,26), ncol=2)
chisq.test(chisq1.5) # X-squared = 9.8553, df = 1, p-value = 0.001693
cramersV(chisq1.5) # Cramer's V = 0.2616099
### Moral vs. Non-violation
chisq1.6 <- matrix(c(30,57,50,26), ncol=2)
chisq.test(chisq1.6) # X-squared = 14.6804, df = 1, p-value = 0.0001274
cramersV(chisq1.6) # Cramer's V = 0.3001063

## 6- to 7-year-olds
### Physical vs. Moral
table(d1$possibility.judgment[d1$age.group=="6-7 yrs" & d1$violation.type=="Physical violation"])
table(d1$possibility.judgment[d1$age.group=="6-7 yrs" & d1$violation.type=="Moral violation"])
chisq1.7 <- matrix(c(7,63,48,33), ncol=2) 
chisq.test(chisq1.7) # X-squared = 37.2477, df = 1, p-value = 1.04e-09
cramersV(chisq1.7) # Cramer's V = 0.4966621
### Physical vs. Probability
table(d1$possibility.judgment[d1$age.group=="6-7 yrs" & d1$violation.type=="Probability violation"])
chisq1.8 <- matrix(c(7,63,33,57), ncol=2)
chisq.test(chisq1.8) # X-squared = 13.545, df = 1, p-value = 0.0002329
cramersV(chisq1.8) # Cramer's V = 0.2909572
### Moral vs. Probability 
chisq1.9 <- matrix(c(48,33,33,57), ncol=2)
chisq.test(chisq1.9) # X-squared = 7.8456, df = 1, p-value = 0.005095
cramersV(chisq1.9) # Cramer's V = 0.2141975

### Physical vs. Non-violation
table(d1$possibility.judgment[d1$age.group=="6-7 yrs" & d1$violation.type=="Non-violation"])
chisq1.10 <- matrix(c(7,63,73,6), ncol=2)
chisq.test(chisq1.10) # X-squared = 98.076, df = 1, p-value < 2.2e-16
cramersV(chisq1.10) # Cramer's V = 0.8113125
### Probability vs. Non-violation
chisq1.11 <- matrix(c(33,57,73,6), ncol=2)
chisq.test(chisq1.11) # X-squared = 53.5425, df = 1, p-value = 2.531e-13
cramersV(chisq1.11) # Cramer's V = 0.562867
### Moral vs. Non-violation
chisq1.12 <- matrix(c(48,33,73,6), ncol=2)
chisq.test(chisq1.12) # X-squared = 22.0721, df = 1, p-value = 2.626e-06
cramersV(chisq1.12) # Cramer's V = 0.3714172

## Adults
### Physical vs. Moral
table(d1$possibility.judgment[d1$age.group=="Adults" & d1$violation.type=="Physical violation"])
table(d1$possibility.judgment[d1$age.group=="Adults" & d1$violation.type=="Moral violation"])
chisq1.13 <- matrix(c(12,158,148,7), ncol=2) 
chisq.test(chisq1.13) # X-squared = 250.1104, df = 1, p-value < 2.2e-16
cramersV(chisq1.13) # Cramer's V = 0.8772516
### Physical vs. Probability
table(d1$possibility.judgment[d1$age.group=="Adults" & d1$violation.type=="Probability violation"])
chisq1.14 <- matrix(c(12,158,98,51), ncol=2)
chisq.test(chisq1.14) # X-squared = 118.574, df = 1, p-value < 2.2e-16
cramersV(chisq1.14) # Cramer's V = 0.6096764
### Moral vs. Probability 
chisq1.15 <- matrix(c(148,7,98,51), ncol=2)
chisq.test(chisq1.15) # X-squared = 41.5372, df = 1, p-value = 1.156e-10
cramersV(chisq1.15) # Cramer's V = 0.3696426

### Physical vs. Non-violation
table(d1$possibility.judgment[d1$age.group=="Adults" & d1$violation.type=="Non-violation"])
chisq1.16 <- matrix(c(12,158,151,7), ncol=2)
chisq.test(chisq1.16) # X-squared = 253.0977, df = 1, p-value < 2.2e-16
cramersV(chisq1.16) # Cramer's V = 0.8784301
### Probability vs. Non-violation
chisq1.17 <- matrix(c(98,51,151,7), ncol=2)
chisq.test(chisq1.17) # X-squared = 42.5114, df = 1, p-value = 7.027e-11
cramersV(chisq1.17) # Cramer's V = 0.3721206
### Moral vs. Non-violation
chisq1.18 <- matrix(c(148,7,151,7), ncol=2)
chisq.test(chisq1.18) # X-squared = 0, df = 1, p-value = 1
cramersV(chisq1.18) # Cramer's V = 2.95e-16

#### Figure 1 ####

mss1 <- aggregate(possibility.judgment ~ subj + violation.type + age.group, d1, mean)
ms1 <- aggregate(possibility.judgment ~ violation.type + age.group, mss1, mean)
ms1$possibility.judgment <- ms1$possibility.judgment*100 

fig1 <- ggplot(ms1,aes(x=age.group, y=possibility.judgment, fill=violation.type))+
  ylab("% Events Judged to be Impossible") +
  xlab("") +
  coord_cartesian(ylim=c(0,100)) +
  geom_bar(position="dodge",stat="identity") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))    
    )
fig1
ggsave(file="Figures/Fig1.png",dpi=300)

#### Study 2a: Judgments of 'Magic' across development ####

## Developmental data
d2a.1 <- read.csv("Data/study2a_kids.csv")
## Adult data
d2a.2 <- read.csv("Data/study2a_adults.csv")
print(c(mean(d2a.2$age[seq(1,length(d2a.2$age),8)]),sd(d2a.2$age[seq(1,length(d2a.2$age),8)])))

d2a <- rbind(d2a.1[,-c(6,9:10)],d2a.2[,-c(8:11)])

# set up factors
d2a$sex <- factor(c("M","F")[d2a$sex])
d2a$scenario <- factor(c("Candy Bar","Ball","T-Shirt","School","Basketball","Best Friend","Movie","Chores")[d2a$scenario])
d2a$violation.type <- factor(c("Physical violation","Probability violation","Moral violation","Non-violation")[d2a$violation.type])
d2a$violation.type <- factor(d2a$violation.type, levels=c("Physical violation","Probability violation","Non-violation","Moral violation"))
d2a$possibility.judgment <- as.numeric(d2a$possibility.judgment)
d2a$possibility.judgment <- (d2a$possibility.judgment-7)*-1
d2a$age[which(d2a$age>18)] <- 7 ## this gives all adults a single value (individual ages are in d2a$adultAge)
d2a$age <- factor(c("3","4","5","6","7","8","Adult")[d2a$age])

d2a$age.group[d2a$age=="3"] <- "3 yrs"
d2a$age.group[d2a$age=="4" | d2a$age=="5"] <- "4-5 yrs"
d2a$age.group[d2a$age=="6" | d2a$age=="7"] <- "6-7 yrs"
d2a$age.group[d2a$age=="Adult"] <- "Adults"
table(d2a$age.group)/8

# Overall analysis with random effects for subject and story
## Base Model
lm2.1 <- lmer(possibility.judgment ~ age.group + violation.type + (1|scenario) + (violation.type|subj), data=d2a)
summary(lm2.1)
#effect of age
lm2.2 <- lmer(possibility.judgment ~ violation.type + (1|scenario) + (violation.type|subj), data=d2a)
anova(lm2.1, lm2.2)
## effect of violation type
lm2.3 <- lmer(possibility.judgment ~ age.group + (1|scenario) + (violation.type|subj), data=d2a)
anova(lm2.1, lm2.3)
## interaction effect
lm2.4 <- lmer(possibility.judgment ~ age.group * violation.type + (1|scenario) + (violation.type|subj), data=d2a)
anova(lm2.1,lm2.4)

# Chi-squared pairwise comparisons 
## 3-year-olds
### Physical vs. Moral
table(d2a$possibility.judgment[d2a$age.group=="3 yrs" & d2a$violation.type=="Physical violation"])
table(d2a$possibility.judgment[d2a$age.group=="3 yrs" & d2a$violation.type=="Moral violation"])
chisq2.1 <- matrix(c(10,32,12,27), ncol=2) 
chisq.test(chisq2.1) # X-squared = 0.2058, df = 1, p-value = 0.6501
cramersV(chisq2.1) # Cramer's V = 0.05040725
### Physical vs. Probability
table(d2a$possibility.judgment[d2a$age.group=="3 yrs" & d2a$violation.type=="Probability violation"])
chisq2.2 <- matrix(c(10,32,18,20), ncol=2)
chisq.test(chisq2.2) # X-squared = 3.8866, df = 1, p-value = 0.04867
cramersV(chisq2.2) # Cramer's V = 0.2204155
### Moral vs. Probability
chisq2.3 <- matrix(c(12,27,18,20), ncol=2)
chisq.test(chisq2.3) # X-squared = 1.5866, df = 1, p-value = 0.2078
cramersV(chisq2.3) # Cramer's V = 0.1435438

### Physical vs. Non-violation
table(d2a$possibility.judgment[d2a$age.group=="3 yrs" & d2a$violation.type=="Non-violation"])
chisq2.4 <- matrix(c(10,32,26,15), ncol=2)
chisq.test(chisq2.4) # X-squared = 11.6865, df = 1, p-value = 0.0006296
cramersV(chisq2.4) # Cramer's V = 0.3752342
### Probability vs. Non-violation
chisq2.5 <- matrix(c(18,20,26,15), ncol=2)
chisq.test(chisq2.5) # X-squared = 1.459, df = 1, p-value = 0.2271
cramersV(chisq2.5) # Cramer's V = 0.1358964
### Moral vs. Non-violation
chisq2.6 <- matrix(c(12,27,26,15), ncol=2)
chisq.test(chisq2.6) # X-squared = 7.2829, df = 1, p-value = 0.006962
cramersV(chisq2.6) # Cramer's V = 0.3017216

## 4- to 5-year-olds
### Physical vs. Moral
table(d2a$possibility.judgment[d2a$age.group=="4-5 yrs" & d2a$violation.type=="Physical violation"])
table(d2a$possibility.judgment[d2a$age.group=="4-5 yrs" & d2a$violation.type=="Moral violation"])
chisq2.7 <- matrix(c(15,69,62,28), ncol=2) 
chisq.test(chisq2.7) # X-squared = 43.8206, df = 1, p-value = 3.599e-11
cramersV(chisq2.7) # Cramer's V = 0.501839
### Physical vs. Probability
table(d2a$possibility.judgment[d2a$age.group=="4-5 yrs" & d2a$violation.type=="Probability violation"])
chisq2.8 <- matrix(c(15,69,65,18), ncol=2)
chisq.test(chisq2.8) # X-squared = 58.7443, df = 1, p-value = 1.796e-14
cramersV(chisq2.8) # Cramer's V = 0.5930953
### Moral vs. Probability 
chisq2.9 <- matrix(c(62,28,65,18), ncol=2)
chisq.test(chisq2.9) # X-squared = 1.5116, df = 1, p-value = 0.2189
cramersV(chisq2.9) # Cramer's V = 0.09347509

### Physical vs. Non-violation
table(d2a$possibility.judgment[d2a$age.group=="4-5 yrs" & d2a$violation.type=="Non-violation"])
chisq2.10 <- matrix(c(15,69,56,7), ncol=2)
chisq.test(chisq2.10) # X-squared = 69.9227, df = 1, p-value < 2.2e-16
cramersV(chisq2.10) # Cramer's V = 0.6896846
### Probability vs. Non-violation
chisq2.11 <- matrix(c(65,18,56,7), ncol=2)
chisq.test(chisq2.11) # X-squared = 2.1266, df = 1, p-value = 0.1448
cramersV(chisq2.11) # Cramer's V = 0.1206894
### Moral vs. Non-violation
chisq2.12 <- matrix(c(62,28,56,7), ncol=2)
chisq.test(chisq2.12) # X-squared = 7.3067, df = 1, p-value = 0.00687
cramersV(chisq2.12) # Cramer's V = 0.2185314

## 6- to 7-year-olds
### Physical vs. Moral
table(d2a$possibility.judgment[d2a$age.group=="6-7 yrs" & d2a$violation.type=="Physical violation"])
table(d2a$possibility.judgment[d2a$age.group=="6-7 yrs" & d2a$violation.type=="Moral violation"])
chisq2.13 <- matrix(c(3,98,64,6), ncol=2) 
chisq.test(chisq2.13) # X-squared = 132.0772, df = 1, p-value < 2.2e-16
cramersV(chisq2.13) # Cramer's V = 0.8788521
### Physical vs. Probability
table(d2a$possibility.judgment[d2a$age.group=="6-7 yrs" & d2a$violation.type=="Probability violation"])
chisq2.14 <- matrix(c(3,98,76,9), ncol=2)
chisq.test(chisq2.14) # X-squared = 137.6362, df = 1, p-value < 2.2e-16
cramersV(chisq2.14) # Cramer's V = 0.8602207
### Moral vs. Probability 
chisq2.15 <- matrix(c(64,6,76,9), ncol=2)
chisq.test(chisq2.15) # X-squared = 0.0224, df = 1, p-value = 0.881
cramersV(chisq2.15) # Cramer's V = 0.01202322

### Physical vs. Non-violation
table(d2a$possibility.judgment[d2a$age.group=="6-7 yrs" & d2a$violation.type=="Non-violation"])
chisq2.16 <- matrix(c(3,98,64,1), ncol=2)
chisq.test(chisq2.16) # X-squared = 145.8758, df = 1, p-value < 2.2e-16
cramersV(chisq2.16) # Cramer's V = 0.9374272
### Probability vs. Non-violation
chisq2.17 <- matrix(c(76,9,64,1), ncol=2)
chisq.test(chisq2.17) # X-squared = 3.5027, df = 1, p-value = 0.06127
cramersV(chisq2.17) # Cramer's V = 0.1528125
### Moral vs. Non-violation
chisq2.18 <- matrix(c(64,6,64,1), ncol=2)
chisq.test(chisq2.18) # X-squared = 2.1112, df = 1, p-value = 0.1462
cramersV(chisq2.18) # Cramer's V = 0.1250552

## Adults
### Physical vs. Moral
table(d2a$possibility.judgment[d2a$age.group=="Adults" & d2a$violation.type=="Physical violation"])
table(d2a$possibility.judgment[d2a$age.group=="Adults" & d2a$violation.type=="Moral violation"])
chisq2.19 <- matrix(c(13,145,154,6), ncol=2) 
chisq.test(chisq2.19) # X-squared = 243.4815, df = 1, p-value < 2.2e-16
cramersV(chisq2.19) # Cramer's V = 0.875023
### Physical vs. Probability
table(d2a$possibility.judgment[d2a$age.group=="Adults" & d2a$violation.type=="Probability violation"])
chisq2.20 <- matrix(c(13,154,154,11), ncol=2)
chisq.test(chisq2.20) # X-squared = 239.568, df = 1, p-value < 2.2e-16
cramersV(chisq2.20) # Cramer's V = 0.8494647
### Moral vs. Probability 
chisq2.21 <- matrix(c(154,6,154,11), ncol=2)
chisq.test(chisq2.21) # X-squared = 0.8677, df = 1, p-value = 0.3516
cramersV(chisq2.21) # Cramer's V = 0.05167072

### Physical vs. Non-violation
table(d2a$possibility.judgment[d2a$age.group=="Adults" & d2a$violation.type=="Non-violation"])
chisq2.22 <- matrix(c(13,154,141,0), ncol=2)
chisq.test(chisq2.22) # X-squared = 256.3724, df = 1, p-value < 2.2e-16
cramersV(chisq2.22) # Cramer's V = 0.9123474
### Probability vs. Non-violation
chisq2.23 <- matrix(c(154,11,141,0), ncol=2)
chisq.test(chisq2.23) # X-squared = 7.9217, df = 1, p-value = 0.004885
cramersV(chisq2.23) # Cramer's V = 0.1608971
### Moral vs. Non-violation
chisq2.24 <- matrix(c(154,6,141,0), ncol=2)
chisq.test(chisq2.24) # X-squared = 3.6463, df = 1, p-value = 0.0562
cramersV(chisq2.24) # Cramer's V = 0.1100629  

mss3 <- aggregate(possibility.judgment ~ violation.type + age.group + subj, d2a, mean)
ms3 <- aggregate(possibility.judgment ~ violation.type + age.group, mss3, mean)
ms3$possibility.judgment <- ms3$possibility.judgment*100

#### Figure 2 ####
fig2 <- ggplot(ms3,aes(x=age.group, y=possibility.judgment, fill=violation.type))+
  ylab("% Events Judged to Require Magic") +
  xlab("Age Group") +
  geom_bar(position="dodge",stat="identity") +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))    
  )
fig2
ggsave(file="Figures/Fig2.png",dpi=300)

#### Study 2b: Adult relative likelihood rankings ####

d2b <- read.csv("Data/study2b.csv")

d2b$subj <- rep(1:length(d2b$IP_Address))
d2b$Gender.1 <- factor(c("Male","Female")[d2b$Gender.1])
d2b$Education <- factor(c("Grammar School","Highschool or Equivalent","Vocational/Technical School",
                            "Some College","College Graduate (4 years)","Master's Degree",
                            "Doctoral Degree (PhD)","Professional Degree (JD,MD,etc.)","Other")[d2b$Education])
d2b.age <- c(mean(d2b$Age[d2b$Age>18], na.rm=T), 
              sd(d2b$Age[d2b$Age>18], na.rm=T))  #Anything under 18 is a typo as Amazon restricts mturk workers below 18
d2b.gender <- table(d2b$Gender.1)
d2b.education <- table(d2b$Education)
d2b$time <- rowMeans(d2b[,c(11,16,21,26,31,36,41,46)], na.rm=T)

# if you want to exclude those who seemed to being go too fast, here is the relevant distributions of time spent reading
hist(d2b$time, breaks=50,col="Red")
hist(d2b$time[d2b$time<75], breaks=50,col="Red")

## For item-wise analyses, assign each item its average rank
d2b.means <- melt(d2b[-1,-c(1,2:6,11,16,21,26,31,36,41,46:54)]) ##the column selection gets rid of anything that's not an item
d2b.means <- aggregate(value~variable, d2b.means, mean)
##assigns violation.type and scenario
violation.types <- c("Physical violation","Probability violation","Moral violation","Non-violation")
scenarios <- c("Candy Bar","Ball","T-Shirt","School","Basketball","Best Friend","Movie","Chores")
d2b.means$violation.type <- rep(violation.types,8)
d2b.means$scenario <- rep(scenarios, each=4)

## physical violations vs. probability violations
var.test(d2b.means$value[d2b.means$violation.type=="Physical violation"],d2b.means$value[d2b.means$violation.type=="Probability violation"])
t.test(d2b.means$value[d2b.means$violation.type=="Physical violation"],d2b.means$value[d2b.means$violation.type=="Probability violation"])
cohensD(d2b.means$value[d2b.means$violation.type=="Physical violation"],d2b.means$value[d2b.means$violation.type=="Probability violation"])

## probability violations vs. moral violations
var.test(d2b.means$value[d2b.means$violation.type=="Probability violation"],d2b.means$value[d2b.means$violation.type=="Moral violation"])
t.test(d2b.means$value[d2b.means$violation.type=="Probability violation"],d2b.means$value[d2b.means$violation.type=="Moral violation"],var.equal=T)
cohensD(d2b.means$value[d2b.means$violation.type=="Probability violation"],d2b.means$value[d2b.means$violation.type=="Moral violation"])

## moral violations vs. non-violations
var.test(d2b.means$value[d2b.means$violation.type=="Moral violation"],d2b.means$value[d2b.means$violation.type=="Non-violation"])
t.test(d2b.means$value[d2b.means$violation.type=="Moral violation"],d2b.means$value[d2b.means$violation.type=="Non-violation"],var.equal=T)
cohensD(d2b.means$value[d2b.means$violation.type=="Moral violation"],d2b.means$value[d2b.means$violation.type=="Non-violation"])

#### Figure 3 ####
d2b.graph <- melt(d2b, id.var=c("subj"), measure.vars=c("IMPOS_Total","IMPROB_Total","IMMOR_Total","POSSIB_Total"))
colnames(d2b.graph) <- c("subj","violation.type","possibility.judgment")
d2b.graph$violation.type <- factor(c("Physical violation","Probability violation","Moral violation","Non-violation")[d2b.graph$violation.type])
d2b.graph$violation.type <- factor(d2b.graph$violation.type, levels=c("Physical violation","Probability violation","Non-violation","Moral violation"))

mss2.1 <- aggregate(possibility.judgment ~ violation.type + subj, d2b.graph, mean)
ms2.1 <- aggregate(possibility.judgment ~ violation.type, mss2.1, mean)

fig3 <- ggplot(ms2.1,aes(x=violation.type, y=possibility.judgment, fill=violation.type))+
  ylab("Mean Relative Likelihood Ranking") +
  xlab("") +
  coord_cartesian(ylim=c(1,4)) +
  geom_bar(position="dodge",stat="identity") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position="none"
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))    
    )
fig3
ggsave(file="Figures/Fig3.png",dpi=300)
