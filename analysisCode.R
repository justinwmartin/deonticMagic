##########  Author and Title ###########
### Phillips, J & Bloom, P.  ##
### Do children think immoral events are magical? ##
### ### ### ### ### ### ### ### ### ###       


#### load data and packages####
rm(list=ls())
setwd("C:/Users/Jonathan/Documents/currentProjects/Paul Bloom/Deontic Restriction/Materials_for_publication")
setwd("C:/Users/Jphil/Documents/currentProjects/Paul Bloom/Deontic Restriction/Materials_for_publication")

# packages
require(lsr)
require(ggplot2)
require(lme4)
require(optimx)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

require(reshape2)
require(plyr)
require(dplyr)
require(tidyr)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
d1.1$sex <- factor(c("M","F")[d1.1$sex])

## Adult data
d1.2 <- read.csv("Data/study1_adults.csv")
print(c(mean(d1.2$age[d1.2$age!=999]),sd(d1.2$age[d1.2$age!=999]))) ### the one participant removed failed to give an age and was coded 999
# M = 30.192308  SD = 9.746788
d1.2$sex <- factor(d1.2$sex)
    
d1 <- rbind(d1.1,d1.2)
# set up factors
d1$scenario <- factor(c("Candy Bar","Ball","T-Shirt","School","Basketball","Best Friend","Movie","Chores")[d1$scenario])
d1$violation.type <- factor(c("Physical violation","Probability violation","Moral violation","Non-violation")[d1$violation.type])
d1$violation.type <- factor(d1$violation.type, levels=c("Physical violation","Probability violation","Non-violation","Moral violation"))

d1$age.group[d1$age > 17] <- "Adults"
d1$age.group[d1$age==4 | d1$age==5] <- "4-5 yrs"
d1$age.group[d1$age==6 | d1$age==7] <- "6-7 yrs"
table(d1$age.group)/8
aggregate(sex~age.group, FUN=function(x) table(x)/8, data=d1)


##Overall analysis with random incepts and slopes for subject and story
## NB: This is the most maximal model that I can get to converge across the models I'm comparing
d1$violation.typeNumeric<-as.numeric(d1$violation.type)
lm1.1 <- glmer(possibility.judgment ~ age.group * violation.type + (violation.type|scenario) + (violation.type|subj), data=d1,
               family="binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
#summary(lm1.1)

###Age * Violation Type Interaction:
lm1.2 <- glmer(possibility.judgment ~ age.group + violation.type +  (violation.type|scenario) + (violation.type|subj), data=d1,
               family="binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
anova(lm1.1,lm1.2)

###Main effect of Age:
lm1.3 <- glmer(possibility.judgment ~ violation.type + (violation.type|scenario) + (violation.type|subj), data=d1,
               family="binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
anova(lm1.2,lm1.3)

###Main effect of Violation Type: 
lm1.4 <- glmer(possibility.judgment ~ age.group + (violation.type|scenario) + (violation.type|subj), data=d1,
               family="binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
anova(lm1.2,lm1.4)

## Analysis at the level of items
d1.1 <-  ddply(d1, c("age.group","violation.type","scenario"), summarise,
               possibility.judgment = mean(possibility.judgment, na.rm=TRUE)*100,
               sd   = sd(possibility.judgment,na.rm=TRUE))

d1.2 <-  ddply(d1.1, c("age.group","violation.type"), summarise,
               mean = mean(possibility.judgment, na.rm=TRUE),
               sd   = sd(possibility.judgment,na.rm=TRUE))

print(d1.2) ## these are just the group means and SDs to report in the paper

## 4- to 5-year-olds
#Moral vs. ordinary
var.test(d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Moral violation"],
       d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Non-violation"])
t.test(d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Moral violation"],
       d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Non-violation"],var.equal = T)
cohensD(d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Moral violation"],
        d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Non-violation"])
## t(14) = 3.27, p = .006, d = 1.62 

# Moral vs. chance
t.test(d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Moral violation"],mu=50,alternative = "greater")
## t(7) = 1.90, p < .050

#Moral vs. impossible
var.test(d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Moral violation"],
       d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Physical violation"])
t.test(d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Moral violation"],
       d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Physical violation"],var.equal = T)
cohensD(d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Moral violation"],
        d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Physical violation"])
## t(14) = -1.17, p = .261, d = 0.59

#Improbable vs. ordinary
var.test(d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Probability violation"],
       d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Non-violation"])
t.test(d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Probability violation"],
       d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Non-violation"],var.equal = T)
cohensD(d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Probability violation"],
        d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Non-violation"])
## t(14) = 3.49, p = .004, d = 1.75

# Improbable vs. chance
t.test(d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Probability violation"],mu=50,alternative = "greater")
# t(7) = 2.28, p = .028


## 6- to 7-year-olds
# vs. 4- to 5-year-olds for immoral events
var.test(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"],
         d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Moral violation"])
t.test(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"],
       d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Moral violation"],var.equal = T)
cohensD(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"],
        d1.1$possibility.judgment[d1.1$age.group=="4-5 yrs" & d1.1$violation.type=="Moral violation"])
## t(14) = -2.42, p = .030, d = 1.21

#Moral vs. ordinary
var.test(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"],
       d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Non-violation"])
t.test(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"],
       d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Non-violation"],var.equal = T)
cohensD(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"],
        d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Non-violation"])
##  t(14) = 4.65, p < .001, d = 2.33

#Improbable vs. ordinary
var.test(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Probability violation"],
         d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Non-violation"])
t.test(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Probability violation"],
       d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Non-violation"],var.equal = T)
cohensD(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Probability violation"],
        d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Non-violation"])
## t(14) = 5.74, p < .001, d = 2.87

# Physical violation vs. improbable event
var.test(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Physical violation"],
         d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Probability violation"])
t.test(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Physical violation"],
       d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Probability violation"])
cohensD(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Physical violation"],
        d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Probability violation"])
##  t(9.08) = 3.16, p = .011, d = 1.58 

# Physical violation vs. moral violation
var.test(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Physical violation"],
         d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"])
t.test(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Physical violation"],
       d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"], var.equal = T)
cohensD(d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Physical violation"],
        d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"])
## t(14) = 7.18, p < .001, d = 3.59

## Adults
# vs. 6- to 7-year-olds for immoral events
var.test(d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Moral violation"],
         d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"])
t.test(d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Moral violation"],
       d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"])
cohensD(d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Moral violation"],
        d1.1$possibility.judgment[d1.1$age.group=="6-7 yrs" & d1.1$violation.type=="Moral violation"])
## t(7.79) = -5.78, p < .001, d = 2.89

# Moral violation vs. ordinary
var.test(d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Moral violation"],
       d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Non-violation"])
t.test(d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Moral violation"],
       d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Non-violation"],var.equal = T)
cohensD(d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Moral violation"],
        d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Non-violation"])
## t(14) = 0.05, p = .961, d = 0.02

# Improbable violation vs. ordinary
var.test(d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Probability violation"],
       d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Non-violation"])
t.test(d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Probability violation"],
       d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Non-violation"])
cohensD(d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Probability violation"],
        d1.1$possibility.judgment[d1.1$age.group=="Adults" & d1.1$violation.type=="Non-violation"])
## t(7.78) = 3.73, p = .006, d = 1.86


#### Figure 1 ####

d1.sums <- ddply(d1, c("age.group","violation.type","subj"), summarise,
                 possibility.judgment = mean(possibility.judgment, na.rm=TRUE)*100)

d1.sums <- ddply(d1.sums, c("age.group","violation.type"), summarise,
                 N    = length(possibility.judgment),
                 mean = mean(possibility.judgment, na.rm=TRUE),
                 sd   = sd(possibility.judgment,na.rm=TRUE),
                 se   = sd / sqrt(N) )

d1.sums$violation.type <- factor(c("Physical","Statistical","Non-violation","Moral")[d1.sums$violation.type])
d1.sums$violation.type <- factor(d1.sums$violation.type,levels=c("Non-violation","Statistical","Physical","Moral"))

fig1 <- ggplot(d1.sums,aes(x=age.group, y=mean, fill=violation.type))+
  ylab("% Events Judged to be Impossible") +
  xlab("") +
  coord_cartesian(ylim=c(0,105)) +
  scale_fill_manual(values=cbPalette) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  guides(fill = guide_legend(title = "Violation Type")) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.text=element_text(size=rel(1.33))
    ,legend.position=c(.1,.875)
    ,legend.title=element_text(size=rel(1.45))
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.75))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.65))    
    ) 

print(fig1)
#ggsave(file="Figures/Fig3.png",dpi=400)

# pdf("C:/Users/Jonathan/Dropbox/Apps/ShareLaTeX/Magic_PNAS/figure1.pdf", 7.5, 6.5)
# plot(fig1)
# dev.off()


#### Study 1b: Event ratings ####
d1b <- read.csv("Data/study1b.csv",stringsAsFactors = F)

print(c(mean(d1b$Age, na.rm=T),sd(d1b$Age, na.rm=T))) 
d1b$Gender <- factor(c("Male","Female")[d1b$Gender])
table(d1b$Gender)

timecol <- c((rep(0:16)*17)+16)
d1b$time <- rowSums(d1b[,timecol],  na.rm=T)
#hist(d1b$time[d1b$time<600],breaks=25,col="Red") # looks like removing everyone below 100 would be very reasonable. 
#d1b <- d1b[d1b$time>100,]

d1b <- d1b[,c(1,19:30,36:47,53:64,70:81,87:98,104:115,121:132,138:149,
              155:166,172:183,189:200,206:217,223:234,240:251,257:268,274:285,366)]

d1bl <- gather(d1b,question,response,-c(1,194),na.rm=T)

d1bl$scenario <- substr(d1bl$question,2,2)
d1bl$scenario <- factor(d1bl$scenario)
d1bl$scenario <- factor(c("Candy Bar","Ball","T-Shirt","School","Basketball","Best Friend","Movie","Chores")[d1bl$scenario])

d1bl$gender <- substr(d1bl$question,3,3)
d1bl$judgment <- substr(d1bl$question,5,10)
d1bl$judgment <- factor(d1bl$judgment)
d1bl$judgment <- factor(c("Morally Wrong","Morally Wrong","Physically Impossible","Physically Impossible","Statistically Improbable","Statistically Improbable")[d1bl$judgment])

d1bl$violation.type <- substrRight(d1bl$question,1)
d1bl$violation.type <- factor(d1bl$violation.type)
d1bl$violation.type <- factor(c("Physical violation","Probability violation","Moral violation","Non-violation")[d1bl$violation.type])

# table of responses, which allows you to see the NAs (response.3 = NA)
aggregate(response ~ violation.type * judgment, FUN=table, data=d1bl)

d1bl <- d1bl[d1bl$response!=3,] ## these are the NAs, which we are excluding
d1bl$response[d1bl$response==1] <- 1
d1bl$response[d1bl$response==2] <- -1

## Analyses: 

d1b.sums <- ddply(d1bl, c("judgment","violation.type","scenario"), summarise,
                  responses = mean(response, na.rm=TRUE))

d1b.sum$judgment <- factor(c("Wrong","Impossible","Improbable")[d1b.sum$judgment])
d1b.sum$judgment <- factor(d1b.sum$judgment, levels = c("Impossible","Improbable","Wrong"))
d1b.sum$violation.type <- factor(c("Moral","Non-violation","Physical","Statistical")[d1b.sum$violation.type])
d1b.sum$violation.type <- factor(d1b.sum$violation.type, levels=c("Physical","Statistical","Moral","Non-violation"))

## This is the highly significant overall interaction for judgment type and violation type (not reported)
lm1b.1 <- lmer(responses ~ judgment * violation.type + (1|scenario), data=d1b.sums)
## interaction effect
lm1b.2 <- lmer(responses ~ judgment + violation.type + (1|scenario), data=d1b.sums)
anova(lm1b.1,lm1b.2)

## Physical impossiiblity judgments
### vs. improbable
var.test(d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Physical violation"],
         d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Probability violation"])
t.test(d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Physical violation"],
       d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Probability violation"],var.equal = T)
cohensD(d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Physical violation"],
        d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Probability violation"])
## p<.001, d=8.81

### vs. impossible
var.test(d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Physical violation"],
         d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Moral violation"])
t.test(d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Physical violation"],
       d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Moral violation"],var.equal = T)
cohensD(d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Physical violation"],
        d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Moral violation"])
## p<.001, d=14.78

### vs. non-violations
var.test(d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Physical violation"],
       d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Non-violation"])
t.test(d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Physical violation"],
       d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Non-violation"],var.equal = T)
cohensD(d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Physical violation"],
        d1b.sums$responses[d1b.sums$judgment=="Physically Impossible" & d1b.sums$violation.type=="Non-violation"])
## p<.001, d=13.39

## Extremely improbable judgments
### vs. impossible
var.test(d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Probability violation"],
         d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Physical violation"])
t.test(d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Probability violation"],
       d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Physical violation"],var.equal = T)
cohensD(d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Probability violation"],
        d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Physical violation"])
## p=.107, d=0.86

### vs. immoral
var.test(d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Probability violation"],
         d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Moral violation"])
t.test(d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Probability violation"],
       d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Moral violation"],var.equal = T)
cohensD(d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Probability violation"],
        d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Moral violation"])
## p<.001, d=5.34

### vs. non-violations
var.test(d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Probability violation"],
         d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Non-violation"])
t.test(d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Probability violation"],
       d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Non-violation"])
cohensD(d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Probability violation"],
        d1b.sums$responses[d1b.sums$judgment=="Statistically Improbable" & d1b.sums$violation.type=="Non-violation"])
## p<.001, d=9.74

## Moral judgments
### vs. impossible
var.test(d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Moral violation"],
       d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Physical violation"])
t.test(d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Moral violation"],
       d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Physical violation"])
cohensD(d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Moral violation"],
       d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Physical violation"])
## p=.001, d=2.31

### vs. improbable
var.test(d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Moral violation"],
         d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Probability violation"])
t.test(d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Moral violation"],
       d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Probability violation"],var.equal=T)
cohensD(d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Moral violation"],
        d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Probability violation"])
## p<.001, d=2.67

### vs. non-violations
var.test(d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Moral violation"],
         d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Non-violation"])
t.test(d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Moral violation"],
       d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Non-violation"])
cohensD(d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Moral violation"],
        d1b.sums$responses[d1b.sums$judgment=="Morally Wrong" & d1b.sums$violation.type=="Non-violation"])
## p<.001, d=9.54

### Study 1 combined analyses ####

d1b.sums1 <- ddply(d1bl, c("judgment","violation.type","scenario"), summarise,
                   response = mean(response, na.rm=TRUE))

d1b.sumsW <- cbind(d1b.sums1[d1b.sums1$judgment=="Morally Wrong",2:4],d1b.sums1[d1b.sums1$judgment=="Statistically Improbable",4],d1b.sums1[d1b.sums1$judgment=="Physically Impossible",4])
colnames(d1b.sumsW) <- c("violation.type","scenario","moral","probable","possible")
d1b.sumsW$intersection <- d1b.sumsW$moral+d1b.sumsW$probable+d1b.sumsW$possible

aggregate(intersection ~ violation.type, FUN=mean, data=d1b.sumsW)

d1b.1 <- left_join(d1.1,d1b.sumsW,by=c("scenario","violation.type")) 
## you can ingore the warning message about the factors with different levels.
## if you get a type-mismatch error here for scenario, make sure you've recompiled d1.1 (line 85 or so)

#write.csv(d1b.1,file="table1.csv",row.names=F) # this is the csv we analyze in the supplmenatary materials

## these are not included in the manuscript, but it's worth noting that they come out as expected
## Interactions:
lm1.i <- lmer(possibility.judgment ~ (moral * age.group) + (probable*age.group) + (possible*age.group) + (1|scenario), data=d1b.1)

# interaction between age and morality
lm1.im <- lmer(possibility.judgment ~ (moral + age.group) + (probable*age.group) + (possible*age.group) + (1|scenario), data=d1b.1)
anova(lm1.i,lm1.im)
# interaction with age and probability
lm1.ip <- lmer(possibility.judgment ~ (moral * age.group) + (probable + age.group) + (possible * age.group) + (1|scenario), data=d1b.1)
anova(lm1.i,lm1.ip)
# interaction with age and possibility
lm1.ipr <- lmer(possibility.judgment ~ (moral * age.group) + (probable * age.group) + (possible + age.group) + (1|scenario), data=d1b.1)
anova(lm1.i,lm1.ipr)

# Here is how you would check for the yet higher-order interaction
lm1.a0 <- lmer(possibility.judgment ~ (moral * age.group) * (probable*age.group) * (possible*age.group) + (1|scenario), data=d1b.1)
anova(lm1.a0,lm1.i)
# NB: the lack of significance here is probably due to a lack of power as there are only 8 data points for each age group * violation type

## these are the simple tests using the ratings that are reported in the paper: 
## younger kids
lm1.y <- lmer(possibility.judgment~moral + probable + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="4-5 yrs",])
#effect of morality
lm1.ym <- lmer(possibility.judgment~ probable + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="4-5 yrs",])
anova(lm1.y,lm1.ym)
## X^2(1) = 6.897, p = .009

##effect of probability
lm1.ys <- lmer(possibility.judgment~ moral + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="4-5 yrs",])
anova(lm1.y,lm1.ys)
## X^2(1) = 5.79, p = .016,

## effect of physical impossibility
lm1.yp <- lmer(possibility.judgment~ moral + probable + (1|scenario), data=d1b.1[d1b.1$age.group=="4-5 yrs",])
anova(lm1.y,lm1.yp)
## X^2(1) = 0.43, p = .510

## older kids
lm1.o <- lmer(possibility.judgment~moral + probable + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="6-7 yrs",])
#effect of morality
lm1.om <- lmer(possibility.judgment~ probable + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="6-7 yrs",])
anova(lm1.o,lm1.om)
## X^2(1) = 5.01, p = .025

##effect of statistical probability
lm1.os <- lmer(possibility.judgment~ moral + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="6-7 yrs",])
anova(lm1.o,lm1.os)
## X^2(1) = 14.503, p < .001

## effect of physical impossibility
lm1.op <- lmer(possibility.judgment~ moral + probable + (1|scenario), data=d1b.1[d1b.1$age.group=="6-7 yrs",])
anova(lm1.o,lm1.op)
## X^2(1) = 6.388, p = .011

## adults
lm1.a <- lmer(possibility.judgment~moral + probable + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="Adults",])
#effect of morality
lm1.am <- lmer(possibility.judgment~ probable + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="Adults",])
anova(lm1.a,lm1.am)
## X^2(1) < 0.01, p = .986

##effect of statistical probability
lm1.as <- lmer(possibility.judgment~ moral + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="Adults",])
anova(lm1.a,lm1.as)
## X^2(1) = 16.289, p < .001

## effect of physical impossibility
lm1.ap <- lmer(possibility.judgment~ moral + probable + (1|scenario), data=d1b.1[d1b.1$age.group=="Adults",])
anova(lm1.a,lm1.ap)
## X^2(1) = 56.257, p < .001

## Study 1: differentatiation over development ####

## age interaction with intersection and physically possible (controlling for event's probability)
lm3.1 <- lmer(possibility.judgment ~ (intersection * age.group) + (possible * age.group) + (1|scenario), data=d1b.1)
## intersection * age interaction
lm3.2 <- lmer(possibility.judgment ~ (intersection + age.group) + (possible * age.group) + (1|scenario), data=d1b.1)
anova(lm3.1,lm3.2)
## X^2(2) = 6.88, p = .032

## possibility * age interaction
lm3.3 <- lmer(possibility.judgment ~ (intersection * age.group) + (possible + age.group) + (1|scenario), data=d1b.1)
anova(lm3.1,lm3.3)
## X^2(2) = 29.5, p < .001

### Figure 2 ####

d1b.2 <- gather(d1b.1[d1b.1$age.group=="4-5 yrs",c(2:4,8:9)], judgment, value, -c(1:3))

d1b.2$judgment<- factor(d1b.2$judgment)
d1b.2$judgment <- factor(c("Involved Violation","Physically Impossible")[d1b.2$judgment])

fig3a <- ggplot(d1b.2, aes(x=possibility.judgment, y=value, label=as.numeric(scenario))) +
  #geom_point(aes(color=violation.type),stat = "identity", position = "jitter") +
  geom_text(aes(color=violation.type),stat = "identity", position = "jitter") +
  facet_wrap(~judgment) +
  stat_smooth(formula = y ~ x, method=lm) +
  #stat_smooth(aes(y=force, x=reflectPoss), formula=y~x, method=lm, se=FALSE, color="black", linetype=2 ) +
  #scale_color_manual(values=c("black","black")) + 
  theme_bw() +
  xlab("Judgment that the event was impossible") +
  ylab("Mean score for the event") + 
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title = element_blank()
    ,legend.text = element_text(size=rel(1.25))
    ,legend.position=c(.87,.9)
    ,axis.text=element_text(size=rel(1))
    ,strip.text=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.95)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))
  )

print(fig3a)

# scaling 
d1b.1$possibility.judgment[d1b.1$age.group=="4-5 yrs"] <- scale(d1b.1$possibility.judgment[d1b.1$age.group=="4-5 yrs"])
d1b.1$possibility.judgment[d1b.1$age.group=="6-7 yrs"] <- scale(d1b.1$possibility.judgment[d1b.1$age.group=="6-7 yrs"])
d1b.1$possibility.judgment[d1b.1$age.group=="Adults"] <- scale(d1b.1$possibility.judgment[d1b.1$age.group=="Adults"])
d1b.1$possible <- scale(d1b.1$possible)
d1b.1$intersection <- scale(d1b.1$intersection)

d1.m <- data.frame(rep(c("Younger Children","Older Children","Adults"),2),rep(c("Involved Violation","Physically Impossibility"),each=3),beta=NA)
colnames(d1.m) <- c("AgeGroup","Predictor","Beta")

lm3.y <- lmer(possibility.judgment ~ intersection  + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="4-5 yrs",])
d1.m[4,3] <- as.data.frame(summary(lm3.y)[10])[3,1]
d1.m[1,3] <- as.data.frame(summary(lm3.y)[10])[2,1]

lm3.o <- lmer(possibility.judgment ~ intersection  + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="6-7 yrs",])
d1.m[5,3] <- as.data.frame(summary(lm3.o)[10])[3,1]
d1.m[2,3] <- as.data.frame(summary(lm3.o)[10])[2,1]

lm3.a <- lmer(possibility.judgment ~ intersection  + possible + (1|scenario), data=d1b.1[d1b.1$age.group=="Adults",])
d1.m[6,3] <- as.data.frame(summary(lm3.a)[10])[3,1]
d1.m[3,3] <- as.data.frame(summary(lm3.a)[10])[2,1]

d1.m$AgeGroup <- factor(d1.m$AgeGroup,levels=c("Younger Children","Older Children","Adults"))

fig2b <- ggplot(d1.m, aes(x=AgeGroup, y=Beta, group=Predictor)) +
  #geom_point(aes(color=Predictor,shape=Predictor),stat = "identity",size=3) +
  geom_line(aes(linetype=Predictor,color=Predictor),size=2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values=c("grey","black")) + 
  scale_linetype_manual(values=c("twodash","solid")) +
  guides(fill = guide_legend(title = "Predictor")) +
  theme_bw() +
  xlab("") +
  ylab("Standarized Model Estimates") + 
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position = c(.8,.1)
    ,legend.text = element_text(size=rel(1.5))
    ,legend.title = element_text(size=rel(1.5))
    ,axis.text.x = element_text(size=rel(1.75))
    ,axis.title=element_text(vjust=.95,size=rel(1.5))
    ,axis.ticks = element_blank()
  )

print(fig2b)



#### Study 2a: Judgments of magic ####

## Developmental data
d2a.1 <- read.csv("Data/study2a_kids.csv")
d2a.1$sex <- factor(c("M","F")[d2a.1$sex])
## Adult data
d2a.2 <- read.csv("Data/study2a_adults.csv")
print(c(mean(d2a.2$age[seq(1,length(d2a.2$age),8)]),sd(d2a.2$age[seq(1,length(d2a.2$age),8)])))

d2a <- rbind(d2a.1[,-c(6,9:10)],d2a.2[,-c(8:11)])

# set up variables
d2a$scenario <- factor(c("Candy Bar","Ball","T-Shirt","School","Basketball","Best Friend","Movie","Chores")[d2a$scenario])
d2a$violation.type <- factor(c("Physical violation","Probability violation","Moral violation","Non-violation")[d2a$violation.type])
d2a$violation.type <- factor(d2a$violation.type, levels=c("Physical violation","Probability violation","Non-violation","Moral violation"))
d2a$possibility.judgment <- as.numeric(d2a$possibility.judgment)
d2a$possibility.judgment <- (d2a$possibility.judgment-7)*-1
d2a$age[which(d2a$age>17)] <- 7 ## this gives all adults a single value (individual ages are in d2a$adultAge)
d2a$age <- factor(c("3","4","5","6","7","8","Adult")[d2a$age])

d2a$age.group[d2a$age=="3"] <- "3 yrs"
d2a$age.group[d2a$age=="4" | d2a$age=="5"] <- "4-5 yrs"
d2a$age.group[d2a$age=="6" | d2a$age=="7"] <- "6-7 yrs"
d2a$age.group[d2a$age=="Adult"] <- "Adults"
d2a$age.group <- factor(d2a$age.group, levels=c("3 yrs","4-5 yrs","6-7 yrs","Adults"))
table(d2a$age.group)/8

aggregate(sex~age.group, FUN=function(x) table(x)/8,data=d2a)

# Overall analysis with random intercepts and slopes for scenario and a random intercept for subjects 
### NB: I simply can't get a model with random slopes for subjects to converge, so it's been dropped.
### In extrapolating from these results, it is probably best to rely on the combined analyses that follow
### fwiw, both sets of analyses tell much the same story.

## Most maximal model that will converge
lm2.1 <- glmer(possibility.judgment ~ age.group * violation.type + (violation.type|scenario) + (1|subj), data=d2a,
               family="binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
#summary(lm2.1)

## interaction effect
lm2.2 <- glmer(possibility.judgment ~ age.group + violation.type + (violation.type|scenario) + (1|subj), data=d2a,
               family="binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
anova(lm2.1,lm2.2)

#effect of age
lm2.3 <- glmer(possibility.judgment ~ violation.type +  (violation.type|scenario) + (1|subj), data=d2a,
               family="binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
anova(lm2.2,lm2.3)

## effect of violation type
lm2.4 <- glmer(possibility.judgment ~ age.group +  (violation.type|scenario) + (1|subj), data=d2a,
               family="binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
anova(lm2.2,lm2.4)


## Analysis at the level of the different events
d2.1 <-  ddply(d2a, c("age.group","violation.type","scenario"), summarise,
               mean = mean(possibility.judgment, na.rm=TRUE)*100,
               sd   = sd(possibility.judgment,na.rm=TRUE))

## Print means and SDs for each group -- first taking an average for each scenario
print(ddply(d2a, c("age.group","violation.type","scenario"), summarise,
            possibility.judgment = mean(possibility.judgment, na.rm=TRUE)*100) %>% 
      ddply(c("age.group","violation.type"), summarise,
            mean = mean(possibility.judgment, na.rm=TRUE),
            sd   = sd(possibility.judgment,na.rm=TRUE)))

## 3 year olds 
# moral vs ordinary
var.test(d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"],
         d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Non-violation"])
t.test(d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"],
       d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Non-violation"],var.equal = T)
cohensD(d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"],
        d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Non-violation"])
## t(14) = 2.92, p = .011, d = 1.46

# moral vs chance
t.test(d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"],mu=50,alternative="greater")
## t(7) = 2.20, p = .032

# moral vs improbable
var.test(d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"],
         d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Probability violation"])
t.test(d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"],
       d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Probability violation"],var.equal = T)
cohensD(d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"],
        d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Probability violation"])
## t(14) = 1.76, p = .099, d = 0.88

# physical vs immoral
var.test(d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Physical violation"],
         d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"])
t.test(d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Physical violation"],
       d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"],var.equal = T)
cohensD(d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Physical violation"],
        d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"])
## t(14) = 0.91, p = .377, d = 0.46

## 4-5-year-olds 
#vs. 3 year olds
var.test(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"],
         d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"])
t.test(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"],
       d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"],var.equal = T)
cohensD(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"],
        d2.1$mean[d2.1$age.group=="3 yrs" & d2.1$violation.type=="Moral violation"])
## t(14) = -3.33, p = .005, d = 1.67

# moral vs physical violation
var.test(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"],
         d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Physical violation"])
t.test(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"],
       d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Physical violation"],var.equal = T)
cohensD(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"],
        d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Physical violation"])
## t(14) = -5.77, p < .001, d = 2.88

# moral vs ordinary
var.test(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"],
         d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Non-violation"])
t.test(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"],
       d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Non-violation"],var.equal = T)
cohensD(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"],
        d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Non-violation"])
## t(14) = 2.23, p = .043, d = 1.12

# improbable vs. moral
var.test(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Probability violation"],
         d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"])
t.test(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Probability violation"],
       d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"],var.equal = T)
cohensD(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Probability violation"],
        d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"])
## t(14) = -1.00, p = .333, d = 0.50

# improbable vs. ordinary
var.test(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Probability violation"],
         d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Non-violation"])
t.test(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Probability violation"],
       d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Non-violation"],var.equal = T)
cohensD(d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Probability violation"],
        d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Non-violation"])
## t(14) = 1.58, p = .136, d = 0.79

## 6-7-year-olds 
#vs. 4-5 year olds
var.test(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"],
         d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"])
t.test(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"],
       d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"],var.equal = T)
cohensD(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"],
        d2.1$mean[d2.1$age.group=="4-5 yrs" & d2.1$violation.type=="Moral violation"])
## t(14) = -2.29, p = .038, d = 1.15

# moral vs ordinary
var.test(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"],
         d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Non-violation"])
t.test(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"],
       d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Non-violation"])
cohensD(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"],
        d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Non-violation"])
## t(7) = 2.60, p = .035, d = 1.30

# improbable vs. ordinary
var.test(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Probability violation"],
         d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Non-violation"])
t.test(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Probability violation"],
       d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Non-violation"])
cohensD(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Probability violation"],
        d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Non-violation"])
## t(7) = 2.26, p = .058, d = 1.13

# moral vs physical violation (this is the one that is closest to physical violations)
var.test(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"],
         d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Physical violation"])
t.test(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"],
       d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Physical violation"])
cohensD(d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"],
        d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Physical violation"])
## t(8.79) = -22.11, p < .001, d = 11.06

## Adults
# vs. 6-7 year olds
var.test(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Moral violation"],
         d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"])
t.test(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Moral violation"],
       d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"],var.equal = T)
cohensD(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Moral violation"],
        d2.1$mean[d2.1$age.group=="6-7 yrs" & d2.1$violation.type=="Moral violation"])
## t(14) = -1.43, p = .174, d = 0.72

# moral vs. ordinary
var.test(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Moral violation"],
         d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Non-violation"])
t.test(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Moral violation"],
         d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Non-violation"])
cohensD(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Moral violation"],
        d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Non-violation"])
## t(7) = 1.96, p = .091, d = 0.98

# improbable vs. ordinary
var.test(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Probability violation"],
         d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Non-violation"])
t.test(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Probability violation"],
         d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Non-violation"])
cohensD(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Probability violation"],
        d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Non-violation"])
## t(7) = 4.99, p = .002, d = 2.49

# physical violations vs. improbable (NB: this the closest one to physical violations)
var.test(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Physical violation"],
         d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Probability violation"])
t.test(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Physical violation"],
         d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Probability violation"], var.equal = T)
cohensD(d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Physical violation"],
        d2.1$mean[d2.1$age.group=="Adults" & d2.1$violation.type=="Probability violation"])
## t(14) = 34.62, p < .001, d = 17.31

d2a.sums <- ddply(d2a, c("age.group","violation.type","subj"), summarise,
                  possibility.judgment = mean(possibility.judgment, na.rm=TRUE)*100)

d2a.sums <- ddply(d2a.sums, c("age.group","violation.type"), summarise,
                  N    = length(possibility.judgment),
                  mean = mean(possibility.judgment, na.rm=TRUE),
                  sd   = sd(possibility.judgment,na.rm=TRUE),
                  se   = sd / sqrt(N) )

d2a.sums$violation.type <- factor(c("Physical","Statistical","Non-violation","Moral")[d2a.sums$violation.type])
d2a.sums$violation.type <- factor(d2a.sums$violation.type,levels=c("Non-violation","Statistical","Physical","Moral"))

## This is just to make the graph somewhat more legible:
d2a.sums$mean[d2a.sums$mean==0] <- .25

#### Figure 3 ####
fig2 <- ggplot(d2a.sums,aes(x=age.group, y=mean, fill=violation.type))+
  ylab("% Events Judged to Require Magic") +
  xlab("") +
  coord_cartesian(ylim=c(0,105)) +
  scale_fill_manual(values=cbPalette) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  guides(fill = guide_legend(title = "Violation Type")) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.text=element_text(size=rel(1.33))
    ,legend.position=c(.1,.89)
    ,legend.title=element_text(size=rel(1.45))
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.75))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.65))    
  ) 

print(fig2)
# ggsave(file="Figures/Fig5.png",dpi=400)

# pdf("C:/Users/Jonathan/Dropbox/Apps/ShareLaTeX/Magic_PNAS/figure2.pdf", 8, 6.5)
# plot(fig2)
# dev.off()


### Study 2b: Event ratings ####
d2b <- read.csv("Data/study2b.csv",stringsAsFactors = F)

print(c(mean(d2b$Age, na.rm=T),sd(d2b$Age, na.rm=T))) 
d2b$Gender <- factor(c("Male","Female")[d2b$Gender])
table(d2b$Gender)

timecol <- c((rep(0:16)*17)+16)
d2b$time <- rowSums(d2b[,timecol],  na.rm=T)
#hist(d2b$time[d2b$time<600],breaks=25,col="Red") # looks like removing everyone below 100 would be very reasonable. 
#d2b <- d2b[d2b$time>100,]

d2b <- d2b[,c(1,19:30,36:47,53:64,70:81,87:98,104:115,121:132,138:149,
              155:166,172:183,189:200,206:217,223:234,240:251,257:268,274:285,366)]

d2bl <- gather(d2b,question,response,-c(1,194),na.rm=T)

d2bl$scenario <- substr(d2bl$question,2,2)
d2bl$scenario <- factor(d2bl$scenario)
d2bl$scenario <- factor(c("Candy Bar","Ball","T-Shirt","School","Basketball","Best Friend","Movie","Chores")[d2bl$scenario])

d2bl$gender <- substr(d2bl$question,3,3)
d2bl$judgment <- substr(d2bl$question,5,10)
d2bl$judgment <- factor(d2bl$judgment)
d2bl$judgment <- factor(c("Morally Wrong","Morally Wrong","Physically Impossible","Physically Impossible","Statistically Improbable","Statistically Improbable")[d2bl$judgment])

d2bl$violation.type <- substrRight(d2bl$question,1)
d2bl$violation.type <- factor(d2bl$violation.type)
d2bl$violation.type <- factor(c("Physical violation","Probability violation","Moral violation","Non-violation")[d2bl$violation.type])

# table of responses, which allows you to see the NAs (response.3 = NA)
aggregate(response ~ violation.type * judgment, FUN=table, data=d2bl)


d2bl <- d2bl[d2bl$response!=3,]
d2bl$response[d2bl$response==1] <- 1
d2bl$response[d2bl$response==2] <- -1

## Analyses

d2b.sums <- ddply(d2bl, c("judgment","violation.type","scenario"), summarise,
                  responses = mean(response, na.rm=TRUE))

d2b.sum$judgment <- factor(c("Wrong","Impossible","Improbable")[d2b.sum$judgment])
d2b.sum$judgment <- factor(d2b.sum$judgment, levels = c("Impossible","Improbable","Wrong"))
d2b.sum$violation.type <- factor(c("Moral","Non-violation","Physical","Statistical")[d2b.sum$violation.type])
d2b.sum$violation.type <- factor(d2b.sum$violation.type, levels=c("Physical","Statistical","Moral","Non-violation"))

## This is the highly significant overall interaction for judgment type and violation type (not reported)
lm2b.1 <- lmer(responses ~ judgment * violation.type + (1|scenario), data=d2b.sums)
## interaction effect
lm2b.2 <- lmer(responses ~ judgment + violation.type + (1|scenario), data=d2b.sums)
anova(lm2b.1,lm2b.2)

## Physical impossiiblity judgments
### vs. improbable
var.test(d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Physical violation"],
         d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Probability violation"])
t.test(d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Physical violation"],
       d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Probability violation"])
cohensD(d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Physical violation"],
        d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Probability violation"])
## p<.001, d=17.84

### vs. impossible
var.test(d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Physical violation"],
         d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Moral violation"])
t.test(d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Physical violation"],
       d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Moral violation"],var.equal = T)
cohensD(d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Physical violation"],
        d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Moral violation"])
## p<.001, d=19.22

### vs. non-violations
var.test(d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Physical violation"],
         d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Non-violation"])
t.test(d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Physical violation"],
       d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Non-violation"],var.equal = T)
cohensD(d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Physical violation"],
        d2b.sums$responses[d2b.sums$judgment=="Physically Impossible" & d2b.sums$violation.type=="Non-violation"])
## p<.001, d=26.74

## Extremely improbable judgments
### vs. impossible
var.test(d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Probability violation"],
         d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Physical violation"])
t.test(d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Probability violation"],
       d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Physical violation"])
cohensD(d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Probability violation"],
        d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Physical violation"])
## p<.001, d=5.35

### vs. immoral
var.test(d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Probability violation"],
         d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Moral violation"])
t.test(d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Probability violation"],
       d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Moral violation"],var.equal = T)
cohensD(d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Probability violation"],
        d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Moral violation"])
## p<.001, d=2.39

### vs. non-violations
var.test(d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Probability violation"],
         d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Non-violation"])
t.test(d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Probability violation"],
       d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Non-violation"], var.equal = T)
cohensD(d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Probability violation"],
        d2b.sums$responses[d2b.sums$judgment=="Statistically Improbable" & d2b.sums$violation.type=="Non-violation"])
## p<.001, d=3.98

## Moral judgments
### vs. impossible
var.test(d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Moral violation"],
         d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Physical violation"])
t.test(d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Moral violation"],
       d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Physical violation"])
cohensD(d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Moral violation"],
        d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Physical violation"])
## p<.001, d=2.77

### vs. improbable
var.test(d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Moral violation"],
         d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Probability violation"])
t.test(d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Moral violation"],
       d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Probability violation"],var.equal=T)
cohensD(d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Moral violation"],
        d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Probability violation"])
## p<.001, d=7.89

### vs. non-violations
var.test(d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Moral violation"],
         d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Non-violation"])
t.test(d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Moral violation"],
       d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Non-violation"])
cohensD(d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Moral violation"],
        d2b.sums$responses[d2b.sums$judgment=="Morally Wrong" & d2b.sums$violation.type=="Non-violation"])
## p<.001, d=13.24

### Study 2 combined analyses ####

d2b.sums1 <- ddply(d2bl, c("judgment","violation.type","scenario"), summarise,
                   response = mean(response, na.rm=TRUE))

d2b.sumsW <- cbind(d2b.sums1[d2b.sums1$judgment=="Morally Wrong",2:4],d2b.sums1[d2b.sums1$judgment=="Statistically Improbable",4],d2b.sums1[d2b.sums1$judgment=="Physically Impossible",4])
colnames(d2b.sumsW) <- c("violation.type","scenario","moral","probable","possible")
d2b.sumsW$intersection <- d2b.sumsW$moral+d2b.sumsW$probable+d2b.sumsW$possible

aggregate(intersection ~ violation.type, FUN=mean, data=d2b.sumsW)

d2b.1 <- left_join(d2.1,d2b.sumsW,by=c("scenario","violation.type"))
## you can ingore the warning message about the factors with different levels

#write.csv(d2b.1,file="table2.csv",row.names=F) # this is the csv analyzed in the supplmenatary materials

## these are not included in the manuscript, but it's worth noting that they come out as expected
## Interactions:
lm2.i <- lmer(mean ~ (moral * age.group) + (probable*age.group) + (possible*age.group) + (1|scenario), data=d2b.1)

# interaction between age and morality
lm2.im <- lmer(mean ~ (moral + age.group) + (probable*age.group) + (possible*age.group) + (1|scenario), data=d2b.1)
anova(lm2.i,lm2.im)
# interaction with age and probability
lm2.ip <- lmer(mean ~ (moral * age.group) + (probable + age.group) + (possible * age.group) + (1|scenario), data=d2b.1)
anova(lm2.i,lm2.ip)
# interaction with age and possibility
lm2.ipr <- lmer(mean ~ (moral * age.group) + (probable * age.group) + (possible + age.group) + (1|scenario), data=d2b.1)
anova(lm2.i,lm2.ipr)
# Here is how you would check for the yet higher-order interaction
lm2.a0 <- lmer(mean ~ (moral * age.group) * (probable*age.group) * (possible*age.group) + (1|scenario), data=d2b.1)
anova(lm2.a0,lm2.i)

## these are the simple tests using the ratings that are reported in the paper: 

## 3-yr-olds
lm2.t <- lmer(mean~moral + probable + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="3 yrs",])
#effect of morality
lm2.tm <- lmer(mean~ probable + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="3 yrs",])
anova(lm2.t,lm2.tm)
## X^2(1) = 9.170, p = .002

##effect of probability
lm2.ts <- lmer(mean~ moral + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="3 yrs",])
anova(lm2.t,lm2.ts)
## X^2(1) = 0.325, p = .569

## effect of physical impossibility
lm2.tp <- lmer(mean~ moral + probable + (1|scenario), data=d2b.1[d2b.1$age.group=="3 yrs",])
anova(lm2.t,lm2.tp)
## X^2(1) = 0.767, p = .381

## younger kids
lm2.y <- lmer(mean~moral + probable + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="4-5 yrs",])
#effect of morality
lm2.ym <- lmer(mean~ probable + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="4-5 yrs",])
anova(lm2.y,lm2.ym)
## X^2(1) = 4.856, p = .028

##effect of probability
lm2.ys <- lmer(mean~ moral + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="4-5 yrs",])
anova(lm2.y,lm2.ys)
## X^2(1) = 2.29, p = .130

## effect of physical impossibility
lm2.yp <- lmer(mean~ moral + probable + (1|scenario), data=d2b.1[d2b.1$age.group=="4-5 yrs",])
anova(lm2.y,lm2.yp)
## X^2(1) = 7.59, p = .006

## older kids
lm2.o <- lmer(mean~moral + probable + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="6-7 yrs",])
#effect of morality
lm2.om <- lmer(mean~ probable + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="6-7 yrs",])
anova(lm2.o,lm2.om)
## X^2(1) = 3.01, p = .083

##effect of statistical probability
lm2.os <- lmer(mean~ moral + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="6-7 yrs",])
anova(lm2.o,lm2.os)
## X^2(1) = 8.35, p = .004

## effect of physical impossibility
lm2.op <- lmer(mean~ moral + probable + (1|scenario), data=d2b.1[d2b.1$age.group=="6-7 yrs",])
anova(lm2.o,lm2.op)
## X^2(1) = 38.714, p < .001  

## adults
lm2.a <- lmer(mean~moral + probable + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="Adults",])
#effect of morality
lm2.am <- lmer(mean~ probable + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="Adults",])
anova(lm2.a,lm2.am)
## X^2(1) = 0.034, p = .855

##effect of statistical probability
lm2.as <- lmer(mean~ moral + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="Adults",])
anova(lm2.a,lm2.as)
## X^2(1) = 4.895, p = .027

## effect of physical impossibility
lm2.ap <- lmer(mean~ moral + probable + (1|scenario), data=d2b.1[d2b.1$age.group=="Adults",])
anova(lm2.a,lm2.ap)
## X^2(1) = 75.66, p < .001

## Study 2: Differentiating across development ####

## age interaction with intersection and physically possible
lm3.1 <- lmer(mean ~ (intersection * age.group) + (possible * age.group) + probable + (1|scenario), data=d2b.1)
## intersection * age interaction
lm3.2 <- lmer(mean ~ (intersection + age.group) + (possible * age.group) + probable + (1|scenario), data=d2b.1)
anova(lm3.1,lm3.2)
## X^2(1) = 10.74, p = .013

## possibility * age interaction
lm3.3 <- lmer(mean ~ (intersection * age.group) + (possible + age.group) + probable + (1|scenario), data=d2b.1)
anova(lm3.1,lm3.3)
## X^2(1) = 39.57, p < .001

## Figure 4 ####

d2a$age.split[d2a$age.group=="3 yrs"] <- "Younger"
d2a$age.split[d2a$age.group=="4-5 yrs"] <- "Younger"
d2a$age.split[d2a$age.group=="6-7 yrs"] <- "Older"
d2a$age.split[d2a$age.group=="Adults"] <- "Adults"
d2a$age.split <- factor(d2a$age.split)

d2.2 <-  ddply(d2a, c("age.split","violation.type","scenario"), summarise,
               mean = mean(possibility.judgment, na.rm=TRUE)*100,
               sd   = sd(possibility.judgment,na.rm=TRUE))

d2b.2 <- left_join(d2.2,d2b.sumsW,by=c("scenario","violation.type"))

d2b.2 <- gather(d2b.2[d2b.2$age.split=="Younger",c(1:4,8:9)], judgment, value, -c(1:4))

d2b.2$judgment<- factor(d2b.2$judgment)
d2b.2$judgment <- factor(c("Involved Violation","Physically Impossible")[d2b.2$judgment])

fig3a <- ggplot(d2b.2, aes(x=mean, y=value, label=as.numeric(scenario))) +
  geom_text(aes(color=violation.type),stat = "identity", position = "jitter") +
  facet_wrap(~judgment) +
  stat_smooth(formula = y ~ x, method=lm) +
  #stat_smooth(aes(y=force, x=reflectPoss), formula=y~x, method=lm, se=FALSE, color="black", linetype=2 ) +
  #scale_color_manual(values=c("black","black")) + 
  theme_bw() +
  xlab("Judgment that the event was impossible") +
  ylab("Mean score for the event") + 
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title = element_blank()
    ,legend.text = element_text(size=rel(1.25))
    ,legend.position=c(.87,.9)
    ,axis.text=element_text(size=rel(1))
    ,strip.text=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.95)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))
  )

print(fig3a)


d2b.1$mean[d2b.1$age.group=="3 yrs"] <- scale(d2b.1$mean[d2b.1$age.group=="3 yrs"])
d2b.1$mean[d2b.1$age.group=="4-5 yrs"] <- scale(d2b.1$mean[d2b.1$age.group=="4-5 yrs"])
d2b.1$mean[d2b.1$age.group=="6-7 yrs"] <- scale(d2b.1$mean[d2b.1$age.group=="6-7 yrs"])
d2b.1$mean[d2b.1$age.group=="Adults"] <- scale(d2b.1$mean[d2b.1$age.group=="Adults"])
d2b.1$possible <- scale(d2b.1$possible)
d2b.1$intersection <- scale(d2b.1$intersection)

d2.m <- data.frame(rep(c("3 yr olds","4-5 yr olds","6-7 yr olds","Adults"),2),rep(c("Physically Impossibility","Involved Violation"),each=4),beta=NA)
colnames(d2.m) <- c("AgeGroup","Predictor","Beta")

lm3.3 <- lmer(mean ~ intersection  + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="3 yrs",])
d2.m[1,3] <- as.data.frame(summary(lm3.3)[10])[3,1]
d2.m[5,3] <- as.data.frame(summary(lm3.3)[10])[2,1]

lm3.y <- lmer(mean ~ intersection  + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="4-5 yrs",])
d2.m[2,3] <- as.data.frame(summary(lm3.y)[10])[3,1]
d2.m[6,3] <- as.data.frame(summary(lm3.y)[10])[2,1]

lm3.o <- lmer(mean ~ intersection  + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="6-7 yrs",])
d2.m[3,3] <- as.data.frame(summary(lm3.o)[10])[3,1]
d2.m[7,3] <- as.data.frame(summary(lm3.o)[10])[2,1]

lm3.a <- lmer(mean ~ intersection  + possible + (1|scenario), data=d2b.1[d2b.1$age.group=="Adults",])
d2.m[4,3] <- as.data.frame(summary(lm3.a)[10])[3,1]
d2.m[8,3] <- as.data.frame(summary(lm3.a)[10])[2,1]

d2.m$AgeGroup <- factor(d2.m$AgeGroup,levels=c("3 yr olds","4-5 yr olds","6-7 yr olds","Adults"))

fig6b <- ggplot(d2.m, aes(x=AgeGroup, y=Beta, group=Predictor)) +
  #geom_point(aes(color=Predictor,shape=Predictor),stat = "identity",size=3) +
  geom_hline(yintercept = 0) +
  geom_line(aes(linetype=Predictor,color=Predictor),size=2) +
  scale_color_manual(values=c("grey","black")) + 
  scale_linetype_manual(values=c("twodash","solid")) +
  guides(fill = guide_legend(title = "Predictor")) +
  theme_bw() +
  xlab("") +
  ylab("Standarized Model Estimates") + 
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position = c(.8,.1)
    ,legend.text = element_text(size=rel(1.5))
    ,legend.title = element_text(size=rel(1.5))
    ,axis.text.x = element_text(size=rel(1.75))
    ,axis.title=element_text(vjust=.95,size=rel(1.5))
    ,axis.ticks = element_blank()
  )

print(fig6b)

