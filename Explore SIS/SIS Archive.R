## Default repository
local({r <- getOption("repos")
r["CRAN"] <- "http://cran.case.edu" 
options(repos=r)
})

## Packages
library(lubridate)
library(dplyr)
library(plyr)
library(psych)
library(plotrix)

## Read in SIS data
SISdata <- read.csv("scrub_sis.csv", header = TRUE, sep = ',', na.strings = "")
colnames(SISdata)
str(SISdata)

# Data Cleaning
# Removing Rows with NA Living Type
SISdata <- SISdata[which(SISdata$LivingType!="NA"),]
## levels(SISdata$LivingType)
SISdata$LivingType = factor(SISdata$LivingType)
## levels(SISdata$LivingType)

# Identifying Duplicate Assessments Per ID
dups <- data.frame(table(SISdata$fake_id))
dups[which(dups$Freq >1), ]

# Single Assessment Per ID
# colnames(SISdata)[12] 
SISout <- SISdata[rev(order(as.Date(SISdata[,12]))), ] # SIS assessment date in descending order
SISout$rownum <- ave(SISout$fake_id, SISout$fake_id, FUN = seq_along) # Creating row number by fake_id
SISsub <- SISout[which(SISout$rownum==1), ] # Subsetting to include most recent assessment per fake_id

# Descriptive Statistics & Univariate Analysis
# Living Type
LT_table <- table(SISsub$LivingType)
prop.table(LT_table)
#################################################
# Home Living
# Section: Standardized Scores
describe(SISsub$homeliving_std)
boxplot(SISsub$homeliving_std, main="Home Living Standardized Score")
boxplot(homeliving_std~LivingType,data=SISsub, main="Home Living Standardized Score \nby Living Type")
aggregate(SISsub$homeliving_std, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
kruskal.test(homeliving_std~LivingType, data=SISsub)

describe(SISsub$ABE_std)
boxplot(SISsub$ABE_std, main="ABE")
boxplot(ABE_std~LivingType,data=SISsub, main="ABE by Living Type")
aggregate(SISsub$ABE_std, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
kruskal.test(ABE_std~LivingType, data=SISsub)

# Section: Raw Score
describe(SISsub$s1a_Score_Raw)
boxplot(SISsub$s1a_Score_Raw, main="Home Living Raw Score")
boxplot(s1a_Score_Raw~LivingType,data=SISsub, main="Home Living Raw Score by Living Type")
aggregate(SISsub$s1a_Score_Raw, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
kruskal.test(s1a_Score_Raw~LivingType, data=SISsub)

# Item: Raw Score
SISsub$s1_Toilet <- rowSums(SISsub[,c("s1a_1_dst","s1a_1_fqy","s1a_1_tos")])
table(SISsub$s1_Toilet)
t_s1_Toilet <- table(SISsub$s1_Toilet,SISsub$LivingType)
fisher.test(t_s1_Toilet, simulate.p.value = TRUE)

SISsub$s1_Clothes <- rowSums(SISsub[,c("s1a_2_dst","s1a_2_fqy","s1a_2_tos")])
table(SISsub$s1_Clothes)
t_s1_Clothes <- table(SISsub$s1_Clothes,SISsub$LivingType)
fisher.test(t_s1_Clothes, simulate.p.value = TRUE)

SISsub$s1_PrepFood <- rowSums(SISsub[,c("s1a_3_dst","s1a_3_fqy","s1a_3_tos")])
table(SISsub$s1_PrepFood)
t_s1_PrepFood <- table(SISsub$s1_PrepFood,SISsub$LivingType)
fisher.test(t_s1_PrepFood, simulate.p.value = TRUE)

SISsub$s1_EatFood <- rowSums(SISsub[,c("s1a_4_dst","s1a_4_fqy","s1a_4_tos")])
table(SISsub$s1_EatFood)
t_s1_EatFood <- table(SISsub$s1_EatFood,SISsub$LivingType)
fisher.test(t_s1_EatFood, simulate.p.value = TRUE)

SISsub$s1_HouseKeep <- rowSums(SISsub[,c("s1a_5_dst","s1a_5_fqy","s1a_5_tos")])
table(SISsub$s1_HouseKeep)
t_s1_HouseKeep <- table(SISsub$s1_HouseKeep,SISsub$LivingType)
fisher.test(t_s1_HouseKeep, simulate.p.value = TRUE)

SISsub$s1_Dress <- rowSums(SISsub[,c("s1a_6_dst","s1a_6_fqy","s1a_6_tos")])
table(SISsub$s1_Dress)
t_s1_Dress <- table(SISsub$s1_Dress,SISsub$LivingType)
fisher.test(t_s1_Dress, simulate.p.value = TRUE)

SISsub$s1_Hygiene <- rowSums(SISsub[,c("s1a_7_dst","s1a_7_fqy","s1a_7_tos")])
table(SISsub$s1_Hygiene)
t_s1_Hygiene <- table(SISsub$s1_Hygiene,SISsub$LivingType)
fisher.test(t_s1_Hygiene, simulate.p.value = TRUE)

SISsub$s1_Appliance <- rowSums(SISsub[,c("s1a_8_dst","s1a_8_fqy","s1a_8_tos")])
table(SISsub$s1_Appliance)
t_s1_Appliance <- table(SISsub$s1_Appliance,SISsub$LivingType)
fisher.test(t_s1_Appliance, simulate.p.value = TRUE)

# Question: Raw Score
# Toilet
table(SISsub$s1a_1_dst) table(SISsub$s1a_1_dst,SISsub$LivingType) chisq.test(table(SISsub$s1a_1_dst,SISsub$LivingType))
table(SISsub$s1a_1_fqy) table(SISsub$s1a_1_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1a_1_fqy,SISsub$LivingType),simulate.p.value = TRUE)
table(SISsub$s1a_1_tos)	table(SISsub$s1a_1_tos,SISsub$LivingType) chisq.test(table(SISsub$s1a_1_tos,SISsub$LivingType))
# Clothes
table(SISsub$s1a_2_dst)	table(SISsub$s1a_2_dst,SISsub$LivingType) fisher.test(table(SISsub$s1a_2_dst,SISsub$LivingType),simulate.p.value = TRUE)
table(SISsub$s1a_2_fqy)	table(SISsub$s1a_2_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1a_2_fqy,SISsub$LivingType),simulate.p.value = TRUE)
table(SISsub$s1a_2_tos)	table(SISsub$s1a_2_tos,SISsub$LivingType) chisq.test(table(SISsub$s1a_2_tos,SISsub$LivingType))
# Preparing Food
table(SISsub$s1a_3_dst)	table(SISsub$s1a_3_dst,SISsub$LivingType) fisher.test(table(SISsub$s1a_3_dst,SISsub$LivingType),simulate.p.value = TRUE)
table(SISsub$s1a_3_fqy)	table(SISsub$s1a_3_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1a_3_fqy,SISsub$LivingType))
table(SISsub$s1a_3_tos)	table(SISsub$s1a_3_tos,SISsub$LivingType) chisq.test(table(SISsub$s1a_3_tos,SISsub$LivingType))
# Eating Food
table(SISsub$s1a_4_dst)	table(SISsub$s1a_4_dst,SISsub$LivingType) chisq.test(table(SISsub$s1a_4_dst,SISsub$LivingType))
table(SISsub$s1a_4_fqy)	table(SISsub$s1a_4_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1a_4_fqy,SISsub$LivingType),simulate.p.value = TRUE)
table(SISsub$s1a_4_tos)	table(SISsub$s1a_4_tos,SISsub$LivingType) chisq.test(table(SISsub$s1a_4_tos,SISsub$LivingType))
# Housekeeping
table(SISsub$s1a_5_dst)	table(SISsub$s1a_5_dst,SISsub$LivingType) chisq.test(table(SISsub$s1a_5_dst,SISsub$LivingType))
table(SISsub$s1a_5_fqy)	table(SISsub$s1a_5_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1a_5_fqy,SISsub$LivingType),simulate.p.value = TRUE)
table(SISsub$s1a_5_tos)	table(SISsub$s1a_5_tos,SISsub$LivingType) chisq.test(table(SISsub$s1a_5_tos,SISsub$LivingType))
# Dressing
table(SISsub$s1a_6_dst)	table(SISsub$s1a_6_dst,SISsub$LivingType) fisher.test(table(SISsub$s1a_6_dst,SISsub$LivingType),simulate.p.value = TRUE)
table(SISsub$s1a_6_fqy)	table(SISsub$s1a_6_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1a_6_fqy,SISsub$LivingType))
table(SISsub$s1a_6_tos)	table(SISsub$s1a_6_tos,SISsub$LivingType) chisq.test(table(SISsub$s1a_6_tos,SISsub$LivingType))
# Hygiene
table(SISsub$s1a_7_dst)	table(SISsub$s1a_7_dst,SISsub$LivingType) fisher.test(table(SISsub$s1a_7_dst,SISsub$LivingType),simulate.p.value = TRUE)
table(SISsub$s1a_7_fqy)	table(SISsub$s1a_7_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1a_7_fqy,SISsub$LivingType))
table(SISsub$s1a_7_tos)	table(SISsub$s1a_7_tos,SISsub$LivingType) chisq.test(table(SISsub$s1a_7_tos,SISsub$LivingType))
# Appliances
table(SISsub$s1a_8_dst)	table(SISsub$s1a_8_dst,SISsub$LivingType) fisher.test(table(SISsub$s1a_8_dst,SISsub$LivingType),simulate.p.value = TRUE)
table(SISsub$s1a_8_fqy)	table(SISsub$s1a_8_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1a_8_fqy,SISsub$LivingType),simulate.p.value = TRUE)
table(SISsub$s1a_8_tos)	table(SISsub$s1a_8_tos,SISsub$LivingType) chisq.test(table(SISsub$s1a_8_tos,SISsub$LivingType))
#################################################
# Community Living
# Section: Standardized Score
describe(SISsub$commliving_std)
boxplot(SISsub$commliving_std, main="Community Living")
boxplot(commliving_std~LivingType,data=SISsub, main="Community Living by Living Type")
aggregate(SISsub$commliving_std, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(commliving_std~LivingType, data=SISsub))

# Section: Raw Score
describe(SISsub$s1b_Score_Raw)
boxplot(SISsub$s1b_Score_Raw, main="Community Living Raw Score")
boxplot(s1b_Score_Raw~LivingType,data=SISsub, main="Community Living Raw Score \nby Living Type")
aggregate(SISsub$s1b_Score_Raw, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(s1b_Score_Raw~LivingType, data=SISsub))

# Item: Raw Score
SISsub$s1_GetAround <- rowSums(SISsub[,c("s1b_1_dst","s1b_1_fqy","s1b_1_tos")])
table(SISsub$s1_GetAround)
table(SISsub$s1_GetAround,SISsub$LivingType)
t_s1_GetAround <- table(SISsub$s1_GetAround,SISsub$LivingType)
fisher.test(t_s1_GetAround, simulate.p.value = TRUE)

SISsub$s1_Rec <- rowSums(SISsub[,c("s1b_2_dst","s1b_2_fqy","s1b_2_tos")])
table(SISsub$s1_Rec)
table(SISsub$s1_Rec,SISsub$LivingType)
t_s1_Rec <- table(SISsub$s1_Rec,SISsub$LivingType)
fisher.test(t_s1_Rec, simulate.p.value = TRUE)

SISsub$s1_PubService <- rowSums(SISsub[,c("s1b_3_dst","s1b_3_fqy","s1b_3_tos")])
table(SISsub$s1_PubService)
table(SISsub$s1_PubService,SISsub$LivingType)
t_s1_PubService <- table(SISsub$s1_PubService,SISsub$LivingType)
fisher.test(t_s1_PubService, simulate.p.value = TRUE)

SISsub$s1_Visits <- rowSums(SISsub[,c("s1b_4_dst","s1b_4_fqy","s1b_4_tos")])
table(SISsub$s1_Visits)
table(SISsub$s1_Visits,SISsub$LivingType)
t_s1_Visits <- table(SISsub$s1_Visits,SISsub$LivingType)
fisher.test(t_s1_Visits, simulate.p.value = TRUE)

SISsub$s1_PrefActiv <- rowSums(SISsub[,c("s1b_5_dst","s1b_5_fqy","s1b_5_tos")])
table(SISsub$s1_PrefActiv)
table(SISsub$s1_PrefActiv,SISsub$LivingType)
t_s1_PrefActiv <- table(SISsub$s1_PrefActiv,SISsub$LivingType)
fisher.test(s1_PrefActiv, simulate.p.value = TRUE)

SISsub$s1_Shop <- rowSums(SISsub[,c("s1b_6_dst","s1b_6_fqy","s1b_6_tos")])
table(SISsub$s1_Shop)
table(SISsub$s1_Shop,SISsub$LivingType)
t_s1_Shop <- table(SISsub$s1_Shop,SISsub$LivingType)
fisher.test(t_s1_Shop, simulate.p.value = TRUE)

SISsub$s1_CommInt <- rowSums(SISsub[,c("s1b_7_dst","s1b_7_fqy","s1b_7_tos")])
table(SISsub$s1_CommInt)
table(SISsub$s1_CommInt,SISsub$LivingType)
t_s1_CommInt <- table(SISsub$s1_CommInt,SISsub$LivingType)
fisher.test(t_s1_CommInt, simulate.p.value = TRUE)

SISsub$s1_AccessSet <- rowSums(SISsub[,c("s1b_8_dst","s1b_8_fqy","s1b_8_tos")])
table(SISsub$s1_AccessSet)
table(SISsub$s1_AccessSet,SISsub$LivingType)
t_s1_AccessSet <- table(SISsub$s1_AccessSet,SISsub$LivingType)
fisher.test(t_s1_AccessSet, simulate.p.value = TRUE)

# Question: Raw Score
# Getting Around
table(SISsub$s1b_1_dst)	table(SISsub$s1b_1_dst,SISsub$LivingType) fisher.test(table(SISsub$s1b_1_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1b_1_fqy)	table(SISsub$s1b_1_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1b_1_fqy,SISsub$LivingType))
table(SISsub$s1b_1_tos)	table(SISsub$s1b_1_tos,SISsub$LivingType) chisq.test(table(SISsub$s1b_1_tos,SISsub$LivingType))
# Recreation
table(SISsub$s1b_2_dst)	table(SISsub$s1b_2_dst,SISsub$LivingType) chisq.test(table(SISsub$s1b_2_dst,SISsub$LivingType))
table(SISsub$s1b_2_fqy)	table(SISsub$s1b_2_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1b_2_fqy,SISsub$LivingType))
table(SISsub$s1b_2_tos)	table(SISsub$s1b_2_tos,SISsub$LivingType) chisq.test(table(SISsub$s1b_2_tos,SISsub$LivingType))
# Public Services
table(SISsub$s1b_3_dst)	table(SISsub$s1b_3_dst,SISsub$LivingType) fisher.test(table(SISsub$s1b_3_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1b_3_fqy)	table(SISsub$s1b_3_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1b_3_fqy,SISsub$LivingType))
table(SISsub$s1b_3_tos)	table(SISsub$s1b_3_tos,SISsub$LivingType) chisq.test(table(SISsub$s1b_3_tos,SISsub$LivingType))
# Visit Friends/Family
table(SISsub$s1b_4_dst)	table(SISsub$s1b_4_dst,SISsub$LivingType) chisq.test(table(SISsub$s1b_4_dst,SISsub$LivingType))
table(SISsub$s1b_4_fqy)	table(SISsub$s1b_4_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1b_4_fqy,SISsub$LivingType))
table(SISsub$s1b_4_tos)	table(SISsub$s1b_4_tos,SISsub$LivingType) chisq.test(table(SISsub$s1b_4_tos,SISsub$LivingType))
# Preferred Activities
table(SISsub$s1b_5_dst)	table(SISsub$s1b_5_dst,SISsub$LivingType) chisq.test(table(SISsub$s1b_5_dst,SISsub$LivingType))
table(SISsub$s1b_5_fqy)	table(SISsub$s1b_5_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1b_5_fqy,SISsub$LivingType))
table(SISsub$s1b_5_tos)	table(SISsub$s1b_5_tos,SISsub$LivingType) chisq.test(table(SISsub$s1b_5_tos,SISsub$LivingType))
# Shopping
table(SISsub$s1b_6_dst)	table(SISsub$s1b_6_dst,SISsub$LivingType) fisher.test(table(SISsub$s1b_6_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1b_6_fqy)	table(SISsub$s1b_6_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1b_6_fqy,SISsub$LivingType))
table(SISsub$s1b_6_tos)	table(SISsub$s1b_6_tos,SISsub$LivingType) chisq.test(table(SISsub$s1b_6_tos,SISsub$LivingType))
# Community Interaaction
table(SISsub$s1b_7_dst)	table(SISsub$s1b_7_dst,SISsub$LivingType) chisq.test(table(SISsub$s1b_7_dst,SISsub$LivingType))
table(SISsub$s1b_7_fqy)	table(SISsub$s1b_7_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1b_7_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1b_7_tos)	table(SISsub$s1b_7_tos,SISsub$LivingType) chisq.test(table(SISsub$s1b_7_tos,SISsub$LivingType))
# Accessing Settings
table(SISsub$s1b_8_dst)	table(SISsub$s1b_8_dst,SISsub$LivingType) fisher.test(table(SISsub$s1b_8_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1b_8_fqy)	table(SISsub$s1b_8_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1b_8_fqy,SISsub$LivingType))
table(SISsub$s1b_8_tos)	table(SISsub$s1b_8_tos,SISsub$LivingType) chisq.test(table(SISsub$s1b_8_tos,SISsub$LivingType))
#################################################
# Lifelong Learning
# Section: Standardized Score
describe(SISsub$lifelearng_std)
boxplot(SISsub$lifelearng_std, main="Lifelong Learning")
boxplot(lifelearng_std~LivingType,data=SISsub, main="Lifelong Learning by Living Type")
aggregate(SISsub$lifelearng_std, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(lifelearng_std~LivingType, data=SISsub))

# Section: Raw Score
describe(SISsub$s1c_Score_Raw)
boxplot(SISsub$s1c_Score_Raw, main="Lifelong Learning Raw Score")
boxplot(s1c_Score_Raw~LivingType,data=SISsub, main="Lifelong Learning Raw Score by Living Type")
aggregate(SISsub$s1c_Score_Raw, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(s1c_Score_Raw~LivingType, data=SISsub))

# Item: Raw Score
SISsub$s1_LearnInteract <- rowSums(SISsub[,c("s1c_1_dst","s1c_1_fqy","s1c_1_tos")])
table(SISsub$s1_LearnInteract)
table(SISsub$s1_LearnInteract,SISsub$LivingType)
t_s1_LearnInteract <- table(SISsub$s1_LearnInteract,SISsub$LivingType)
fisher.test(t_s1_LearnInteract, simulate.p.value = TRUE)

SISsub$s1_LearnDecsn <- rowSums(SISsub[,c("s1c_2_dst","s1c_2_fqy","s1c_2_tos")])
table(SISsub$s1_LearnDecsn)
table(SISsub$s1_LearnDecsn,SISsub$LivingType)
t_s1_LearnDecsn <- table(SISsub$s1_LearnDecsn,SISsub$LivingType)
fisher.test(t_s1_LearnDecsn, simulate.p.value = TRUE)

SISsub$s1_ProbSolve <- rowSums(SISsub[,c("s1c_3_dst","s1c_3_fqy","s1c_3_tos")])
table(SISsub$s1_ProbSolve)
table(SISsub$s1_ProbSolve,SISsub$LivingType)
t_s1_ProbSolve <- table(SISsub$s1_ProbSolve,SISsub$LivingType)
fisher.test(t_s1_ProbSolve, simulate.p.value = TRUE)

SISsub$s1_UseTech <- rowSums(SISsub[,c("s1c_4_dst","s1c_4_fqy","s1c_4_tos")])
table(SISsub$s1_UseTech)
table(SISsub$s1_UseTech,SISsub$LivingType)
t_s1_UseTech <- table(SISsub$s1_UseTech,SISsub$LivingType)
fisher.test(t_s1_UseTech, simulate.p.value = TRUE)

SISsub$s1_AccessTrain <- rowSums(SISsub[,c("s1c_5_dst","s1c_5_fqy","s1c_5_tos")])
table(SISsub$s1_AccessTrain)
table(SISsub$s1_AccessTrain,SISsub$LivingType)
t_s1_AccessTrain <- table(SISsub$s1_AccessTrain,SISsub$LivingType)
fisher.test(t_s1_AccessTrain, simulate.p.value = TRUE)

SISsub$s1_Academic <- rowSums(SISsub[,c("s1c_6_dst","s1c_6_fqy","s1c_6_tos")])
table(SISsub$s1_Academic)
table(SISsub$s1_Academic,SISsub$LivingType)
t_s1_Academic <- table(SISsub$s1_Academic,SISsub$LivingType)
fisher.test(t_s1_Academic, simulate.p.value = TRUE)

SISsub$s1_HealthSkill <- rowSums(SISsub[,c("s1c_7_dst","s1c_7_fqy","s1c_7_tos")])
table(SISsub$s1_HealthSkill)
table(SISsub$s1_HealthSkill,SISsub$LivingType)
t_s1_HealthSkill <- table(SISsub$s1_HealthSkill,SISsub$LivingType)
fisher.test(t_s1_HealthSkill, simulate.p.value = TRUE)

SISsub$s1_SelfDeterm <- rowSums(SISsub[,c("s1c_8_dst","s1c_8_fqy","s1c_8_tos")])
table(SISsub$s1_SelfDeterm)
table(SISsub$s1_SelfDeterm,SISsub$LivingType)
t_s1_SelfDeterm <- table(SISsub$s1_SelfDeterm,SISsub$LivingType)
fisher.test(t_s1_SelfDeterm, simulate.p.value = TRUE)

SISsub$s1_SelfManage <- rowSums(SISsub[,c("s1c_9_dst","s1c_9_fqy","s1c_9_tos")])
table(SISsub$s1_SelfManage)
table(SISsub$s1_SelfManage,SISsub$LivingType)
t_s1_SelfManage <- table(SISsub$s1_SelfManage,SISsub$LivingType)
fisher.test(t_s1_SelfManage, simulate.p.value = TRUE)

# Question: Raw Score
# Learning Ineteraction
table(SISsub$s1c_1_dst)	table(SISsub$s1c_1_dst,SISsub$LivingType) fisher.test(table(SISsub$s1c_1_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1c_1_fqy)	table(SISsub$s1c_1_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1c_1_fqy,SISsub$LivingType))
table(SISsub$s1c_1_tos)	table(SISsub$s1c_1_tos,SISsub$LivingType) chisq.test(table(SISsub$s1c_1_tos,SISsub$LivingType))
# Learning Decisions
table(SISsub$s1c_2_dst)	table(SISsub$s1c_2_dst,SISsub$LivingType) chisq.test(table(SISsub$s1c_2_dst,SISsub$LivingType))
table(SISsub$s1c_2_fqy)	table(SISsub$s1c_2_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1c_2_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1c_2_tos)	table(SISsub$s1c_2_tos,SISsub$LivingType) chisq.test(table(SISsub$s1c_2_tos,SISsub$LivingType))
# Problem Solving
table(SISsub$s1c_3_dst)	table(SISsub$s1c_3_dst,SISsub$LivingType) chisq.test(table(SISsub$s1c_3_dst,SISsub$LivingType))
table(SISsub$s1c_3_fqy)	table(SISsub$s1c_3_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1c_3_fqy,SISsub$LivingType))
table(SISsub$s1c_3_tos)	table(SISsub$s1c_3_tos,SISsub$LivingType) chisq.test(table(SISsub$s1c_3_tos,SISsub$LivingType))
# Using Technology
table(SISsub$s1c_4_dst)	table(SISsub$s1c_4_dst,SISsub$LivingType) fisher.test(table(SISsub$s1c_4_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1c_4_fqy)	table(SISsub$s1c_4_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1c_4_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1c_4_tos)	table(SISsub$s1c_4_tos,SISsub$LivingType) chisq.test(table(SISsub$s1c_4_tos,SISsub$LivingType))
# Accessing Training
table(SISsub$s1c_5_dst)	table(SISsub$s1c_5_dst,SISsub$LivingType) fisher.test(table(SISsub$s1c_5_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1c_5_fqy)	table(SISsub$s1c_5_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1c_5_fqy,SISsub$LivingType))
table(SISsub$s1c_5_tos)	table(SISsub$s1c_5_tos,SISsub$LivingType) chisq.test(table(SISsub$s1c_5_tos,SISsub$LivingType))
# Academics
table(SISsub$s1c_6_dst)	table(SISsub$s1c_6_dst,SISsub$LivingType) chisq.test(table(SISsub$s1c_6_dst,SISsub$LivingType))
table(SISsub$s1c_6_fqy)	table(SISsub$s1c_6_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1c_6_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1c_6_tos)	table(SISsub$s1c_6_tos,SISsub$LivingType) chisq.test(table(SISsub$s1c_6_tos,SISsub$LivingType))
# Learning Health Skills
table(SISsub$s1c_7_dst)	table(SISsub$s1c_7_dst,SISsub$LivingType) chisq.test(table(SISsub$s1c_7_dst,SISsub$LivingType))
table(SISsub$s1c_7_fqy)	table(SISsub$s1c_7_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1c_7_fqy,SISsub$LivingType))
table(SISsub$s1c_7_tos)	table(SISsub$s1c_7_tos,SISsub$LivingType) chisq.test(table(SISsub$s1c_7_tos,SISsub$LivingType))
# Learning Self-Determiniation
table(SISsub$s1c_8_dst)	table(SISsub$s1c_8_dst,SISsub$LivingType) chisq.test(table(SISsub$s1c_8_dst,SISsub$LivingType))
table(SISsub$s1c_8_fqy)	table(SISsub$s1c_8_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1c_8_fqy,SISsub$LivingType))
table(SISsub$s1c_8_tos)	table(SISsub$s1c_8_tos,SISsub$LivingType) chisq.test(table(SISsub$s1c_8_tos,SISsub$LivingType))
# Learning Self-Management
table(SISsub$s1c_9_dst)	table(SISsub$s1c_9_dst,SISsub$LivingType) chisq.test(table(SISsub$s1c_9_dst,SISsub$LivingType))
table(SISsub$s1c_9_fqy)	table(SISsub$s1c_9_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1c_9_fqy,SISsub$LivingType))
table(SISsub$s1c_9_tos)	table(SISsub$s1c_9_tos,SISsub$LivingType) chisq.test(table(SISsub$s1c_9_tos,SISsub$LivingType))
#################################################
# Employment
# Section: Standardized Score
describe(SISsub$employment_std)
boxplot(SISsub$employment_std, main="Employment")
boxplot(employment_std~LivingType,data=SISsub, main="Employment by Living Type")
aggregate(SISsub$employment_std, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(employment_std~LivingType, data=SISsub))

# Section: Raw Score
describe(SISsub$s1d_Score_Raw)
boxplot(SISsub$s1d_Score_Raw, main="Employment Raw Score")
boxplot(s1d_Score_Raw~LivingType,data=SISsub, main="Employment Raw Score by Living Type")
aggregate(SISsub$s1d_Score_Raw, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(s1d_Score_Raw~LivingType, data=SISsub))

# Item: Raw Score
SISsub$s1_JobAccom <- rowSums(SISsub[,c("s1d_1_dst","s1d_1_fqy","s1d_1_tos")])
table(SISsub$s1_JobAccom)
table(SISsub$s1_JobAccom,SISsub$LivingType)
t_s1_JobAccom <- table(SISsub$s1_JobAccom,SISsub$LivingType)
fisher.test(t_s1_JobAccom, simulate.p.value = TRUE)

SISsub$s1_JobSkill <- rowSums(SISsub[,c("s1d_2_dst","s1d_2_fqy","s1d_2_tos")])
table(SISsub$s1_JobSkill)
table(SISsub$s1_JobSkill,SISsub$LivingType)
t_s1_JobSkill <- table(SISsub$s1_JobSkill,SISsub$LivingType)
fisher.test(t_s1_JobSkill, simulate.p.value = TRUE)

SISsub$s1_CoInteract <- rowSums(SISsub[,c("s1d_3_dst","s1d_3_fqy","s1d_3_tos")])
table(SISsub$s1_CoInteract)
table(SISsub$s1_CoInteract,SISsub$LivingType)
t_s1_CoInteract <- table(SISsub$s1_CoInteract,SISsub$LivingType)
fisher.test(t_s1_CoInteract, simulate.p.value = TRUE)

SISsub$s1_SupInteract <- rowSums(SISsub[,c("s1d_4_dst","s1d_4_fqy","s1d_4_tos")])
table(SISsub$s1_SupInteract)
table(SISsub$s1_SupInteract,SISsub$LivingType)
t_s1_SupInteract <- table(SISsub$s1_SupInteract,SISsub$LivingType)
fisher.test(t_s1_SupInteract, simulate.p.value = TRUE)

SISsub$s1_WrkSpd <- rowSums(SISsub[,c("s1d_5_dst","s1d_5_fqy","s1d_5_tos")])
table(SISsub$s1_WrkSpd)
table(SISsub$s1_WrkSpd,SISsub$LivingType)
t_s1_WrkSpd <- table(SISsub$s1_WrkSpd ,SISsub$LivingType)
fisher.test(t_s1_WrkSpd, simulate.p.value = TRUE)

SISsub$s1_WrkQual <- rowSums(SISsub[,c("s1d_6_dst","s1d_6_fqy","s1d_6_tos")])
table(SISsub$s1_WrkQual)
table(SISsub$s1_WrkQual,SISsub$LivingType)
t_s1_WrkQual <- table(SISsub$s1_WrkQual,SISsub$LivingType)
fisher.test(t_s1_WrkQual, simulate.p.value = TRUE)

SISsub$s1_ChgAssign <- rowSums(SISsub[,c("s1d_7_dst","s1d_7_fqy","s1d_7_tos")])
table(SISsub$s1_ChgAssign)
table(SISsub$s1_ChgAssign,SISsub$LivingType)
t_s1_ChgAssign <- table(SISsub$s1_ChgAssign,SISsub$LivingType)
fisher.test(t_s1_ChgAssign, simulate.p.value = TRUE)

SISsub$s1_SeekAssist <- rowSums(SISsub[,c("s1d_8_dst","s1d_8_fqy","s1d_8_tos")])
table(SISsub$s1_SeekAssist)
table(SISsub$s1_SeekAssist,SISsub$LivingType)
t_s1_SeekAssist <- table(SISsub$s1_SeekAssist,SISsub$LivingType)
fisher.test(t_s1_SeekAssist, simulate.p.value = TRUE)

# Question: Raw Score
# Job Accomodations
table(SISsub$s1d_1_dst)	table(SISsub$s1d_1_dst,SISsub$LivingType) chisq.test(table(SISsub$s1d_1_dst,SISsub$LivingType))
table(SISsub$s1d_1_fqy)	table(SISsub$s1d_1_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1d_1_fqy,SISsub$LivingType))
table(SISsub$s1d_1_tos)	table(SISsub$s1d_1_tos,SISsub$LivingType) chisq.test(table(SISsub$s1d_1_tos,SISsub$LivingType))
# Specific Job Skills
table(SISsub$s1d_2_dst)	table(SISsub$s1d_2_dst,SISsub$LivingType) chisq.test(table(SISsub$s1d_2_dst,SISsub$LivingType))
table(SISsub$s1d_2_fqy)	table(SISsub$s1d_2_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1d_2_fqy,SISsub$LivingType))
table(SISsub$s1d_2_tos)	table(SISsub$s1d_2_tos,SISsub$LivingType) chisq.test(table(SISsub$s1d_2_tos,SISsub$LivingType))
# Co-Worker Interaction
table(SISsub$s1d_3_dst)	table(SISsub$s1d_3_dst,SISsub$LivingType) chisq.test(table(SISsub$s1d_3_dst,SISsub$LivingType))
table(SISsub$s1d_3_fqy)	table(SISsub$s1d_3_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1d_3_fqy,SISsub$LivingType))
table(SISsub$s1d_3_tos)	table(SISsub$s1d_3_tos,SISsub$LivingType) chisq.test(table(SISsub$s1d_3_tos,SISsub$LivingType))
# Supervisor Interaction
table(SISsub$s1d_4_dst)	table(SISsub$s1d_4_dst,SISsub$LivingType) chisq.test(table(SISsub$s1d_4_dst,SISsub$LivingType))
table(SISsub$s1d_4_fqy)	table(SISsub$s1d_4_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1d_4_fqy,SISsub$LivingType))
table(SISsub$s1d_4_tos)	table(SISsub$s1d_4_tos,SISsub$LivingType) chisq.test(table(SISsub$s1d_4_tos,SISsub$LivingType))
# Work Speed
table(SISsub$s1d_5_dst)	table(SISsub$s1d_5_dst,SISsub$LivingType) chisq.test(table(SISsub$s1d_5_dst,SISsub$LivingType))
table(SISsub$s1d_5_fqy)	table(SISsub$s1d_5_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1d_5_fqy,SISsub$LivingType))
table(SISsub$s1d_5_tos)	table(SISsub$s1d_5_tos,SISsub$LivingType) chisq.test(table(SISsub$s1d_5_tos,SISsub$LivingType))
# Work Quality
table(SISsub$s1d_6_dst)	table(SISsub$s1d_6_dst,SISsub$LivingType) chisq.test(table(SISsub$s1d_6_dst,SISsub$LivingType))
table(SISsub$s1d_6_fqy)	table(SISsub$s1d_6_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1d_6_fqy,SISsub$LivingType))
table(SISsub$s1d_6_tos)	table(SISsub$s1d_6_tos,SISsub$LivingType) chisq.test(table(SISsub$s1d_6_tos,SISsub$LivingType))
# Changing Assignments
table(SISsub$s1d_7_dst)	table(SISsub$s1d_7_dst,SISsub$LivingType) chisq.test(table(SISsub$s1d_7_dst,SISsub$LivingType))
table(SISsub$s1d_7_fqy)	table(SISsub$s1d_7_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1d_7_fqy,SISsub$LivingType))
table(SISsub$s1d_7_tos)	table(SISsub$s1d_7_tos,SISsub$LivingType) chisq.test(table(SISsub$s1d_7_tos,SISsub$LivingType))
# Seeing Assistance
table(SISsub$s1d_8_dst)	table(SISsub$s1d_8_dst,SISsub$LivingType) fisher.test(table(SISsub$s1d_8_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1d_8_fqy)	table(SISsub$s1d_8_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1d_8_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1d_8_tos)	table(SISsub$s1d_8_tos,SISsub$LivingType) chisq.test(table(SISsub$s1d_8_tos,SISsub$LivingType))
#################################################
# Health & Safety
# Section: Standardized Score
describe(SISsub$hlthsafety_std)
boxplot(SISsub$hlthsafety_std, main="Health & Safety")
boxplot(hlthsafety_std~LivingType,data=SISsub, main="Health & Safety \nby Living Type")
aggregate(SISsub$hlthsafety_std, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(hlthsafety_std~LivingType, data=SISsub))

# Section: Raw Score
describe(SISsub$s1e_Score_Raw)
boxplot(SISsub$s1e_Score_Raw, main="Health & Safety Raw Score")
boxplot(s1e_Score_Raw~LivingType,data=SISsub, main="Health & Safety Raw Score by Living Type")
aggregate(SISsub$s1e_Score_Raw, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(s1e_Score_Raw~LivingType, data=SISsub))

# Item: Raw Score
SISsub$s1_TakeMeds <- rowSums(SISsub[,c("s1e_1_dst","s1e_1_fqy","s1e_1_tos")])
table(SISsub$s1_TakeMeds)
table(SISsub$s1_TakeMeds,SISsub$LivingType)
t_s1_TakeMeds <- table(SISsub$s1_TakeMeds,SISsub$LivingType)
fisher.test(t_s1_TakeMeds, simulate.p.value = TRUE)

SISsub$s1_AvoidHaz <- rowSums(SISsub[,c("s1e_2_dst","s1e_2_fqy","s1e_2_tos")])
table(SISsub$s1_AvoidHaz)
table(SISsub$s1_AvoidHaz,SISsub$LivingType)
t_s1_AvoidHaz <- table(SISsub$s1_AvoidHaz,SISsub$LivingType)
fisher.test(t_s1_AvoidHaz, simulate.p.value = TRUE)

SISsub$s1_ObtainHC <- rowSums(SISsub[,c("s1e_3_dst","s1e_3_fqy","s1e_3_tos")])
table(SISsub$s1_ObtainHC)
table(SISsub$s1_ObtainHC,SISsub$LivingType)
t_s1_ObtainHC <- table(SISsub$s1_ObtainHC,SISsub$LivingType)
fisher.test(t_s1_ObtainHC, simulate.p.value = TRUE)

SISsub$s1_Moving <- rowSums(SISsub[,c("s1e_4_dst","s1e_4_fqy","s1e_4_tos")])
table(SISsub$s1_Moving)
table(SISsub$s1_Moving,SISsub$LivingType)
t_s1_Moving <- table(SISsub$s1_Moving,SISsub$LivingType)
fisher.test(t_s1_Moving, simulate.p.value = TRUE)

SISsub$s1_EmergSvc <- rowSums(SISsub[,c("s1e_5_dst","s1e_5_fqy","s1e_5_tos")])
table(SISsub$s1_EmergSvc)
table(SISsub$s1_EmergSvc,SISsub$LivingType)
t_s1_EmergSvc <- table(SISsub$s1_EmergSvc,SISsub$LivingType)
fisher.test(t_s1_EmergSvc, simulate.p.value = TRUE)

SISsub$s1_Nutrition <- rowSums(SISsub[,c("s1e_6_dst","s1e_6_fqy","s1e_6_tos")])
table(SISsub$s1_Nutrition)
table(SISsub$s1_Nutrition,SISsub$LivingType)
t_s1_Nutrition <- table(SISsub$s1_Nutrition,SISsub$LivingType)
fisher.test(t_s1_Nutrition, simulate.p.value = TRUE)

SISsub$s1_Fitness <- rowSums(SISsub[,c("s1e_7_dst","s1e_7_fqy","s1e_7_tos")])
table(SISsub$s1_Fitness)
table(SISsub$s1_Fitness,SISsub$LivingType)
t_s1_s1_Fitness <- table(SISsub$s1_Fitness,SISsub$LivingType)
fisher.test(t_s1_Fitness, simulate.p.value = TRUE)

SISsub$s1_EmoWelBe <- rowSums(SISsub[,c("s1e_8_dst","s1e_8_fqy","s1e_8_tos")])
table(SISsub$s1_EmoWelBe)
table(SISsub$s1_EmoWelBe,SISsub$LivingType)
t_s1_EmoWelBe <- table(SISsub$s1_EmoWelBe,SISsub$LivingType)
fisher.test(t_s1_EmoWelBe, simulate.p.value = TRUE)

# Question: Raw Score
# Taking Medications
table(SISsub$s1e_1_dst)	table(SISsub$s1e_1_dst,SISsub$LivingType) fisher.test(table(SISsub$s1e_1_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1e_1_fqy)	table(SISsub$s1e_1_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1e_1_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1e_1_tos)	table(SISsub$s1e_1_tos,SISsub$LivingType) chisq.test(table(SISsub$s1e_1_tos,SISsub$LivingType))
# Avoiding Hazards
table(SISsub$s1e_2_dst)	table(SISsub$s1e_2_dst,SISsub$LivingType) chisq.test(table(SISsub$s1e_2_dst,SISsub$LivingType))
table(SISsub$s1e_2_fqy)	table(SISsub$s1e_2_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1e_2_fqy,SISsub$LivingType))
table(SISsub$s1e_2_tos)	table(SISsub$s1e_2_tos,SISsub$LivingType) chisq.test(table(SISsub$s1e_2_tos,SISsub$LivingType))
# Obtaining Health Care
table(SISsub$s1e_3_dst)	table(SISsub$s1e_3_dst,SISsub$LivingType) chisq.test(table(SISsub$s1e_3_dst,SISsub$LivingType))
table(SISsub$s1e_3_fqy)	table(SISsub$s1e_3_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1e_3_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1e_3_tos)	table(SISsub$s1e_3_tos,SISsub$LivingType) chisq.test(table(SISsub$s1e_3_tos,SISsub$LivingType))
# Moving About
table(SISsub$s1e_4_dst)	table(SISsub$s1e_4_dst,SISsub$LivingType) chisq.test(table(SISsub$s1e_4_dst,SISsub$LivingType))
table(SISsub$s1e_4_fqy)	table(SISsub$s1e_4_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1e_4_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1e_4_tos)	table(SISsub$s1e_4_tos,SISsub$LivingType) chisq.test(table(SISsub$s1e_4_tos,SISsub$LivingType))
# Accessing Emergency Services
table(SISsub$s1e_5_dst)	table(SISsub$s1e_5_dst,SISsub$LivingType) chisq.test(table(SISsub$s1e_5_dst,SISsub$LivingType))
table(SISsub$s1e_5_fqy)	table(SISsub$s1e_5_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1e_5_fqy,SISsub$LivingType))
table(SISsub$s1e_5_tos)	table(SISsub$s1e_5_tos,SISsub$LivingType) chisq.test(table(SISsub$s1e_5_tos,SISsub$LivingType))
# Nutritional Diet
table(SISsub$s1e_6_dst)	table(SISsub$s1e_6_dst,SISsub$LivingType) chisq.test(table(SISsub$s1e_6_dst,SISsub$LivingType))
table(SISsub$s1e_6_fqy)	table(SISsub$s1e_6_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1e_6_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1e_6_tos)	table(SISsub$s1e_6_tos,SISsub$LivingType) chisq.test(table(SISsub$s1e_6_tos,SISsub$LivingType))
# Physical Fitness
table(SISsub$s1e_7_dst)	table(SISsub$s1e_7_dst,SISsub$LivingType) chisq.test(table(SISsub$s1e_7_dst,SISsub$LivingType))
table(SISsub$s1e_7_fqy)	table(SISsub$s1e_7_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1e_7_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1e_7_tos)	table(SISsub$s1e_7_tos,SISsub$LivingType) chisq.test(table(SISsub$s1e_7_tos,SISsub$LivingType))
# Emotional Well-Being
table(SISsub$s1e_8_dst)	table(SISsub$s1e_8_dst,SISsub$LivingType) chisq.test(table(SISsub$s1e_8_dst,SISsub$LivingType))
table(SISsub$s1e_8_fqy)	table(SISsub$s1e_8_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1e_8_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1e_8_tos)	table(SISsub$s1e_8_tos,SISsub$LivingType) chisq.test(table(SISsub$s1e_8_tos,SISsub$LivingType))
#################################################
# Social Activities
# Section: Standardized Score
describe(SISsub$social_std)
boxplot(SISsub$social_std, main="Social Activities")
boxplot(social_std~LivingType,data=SISsub, main="Social Activities by Living Type")
aggregate(SISsub$social_std, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(social_std~LivingType, data=SISsub))

# Section: Raw Score
SISsub$s1f_Score_Raw <- rowSums(SISsub[,c("s1f_1_dst","s1f_1_fqy","s1f_1_tos","s1f_2_dst","s1f_2_fqy",
                                          "s1f_2_tos","s1f_3_dst","s1f_3_fqy","s1f_3_tos","s1f_4_dst",
                                          "s1f_4_fqy","s1f_4_tos","s1f_5_dst","s1f_5_fqy","s1f_5_tos",
                                          "s1f_6_dst","s1f_6_fqy","s1f_6_tos","s1f_7_dst","s1f_7_fqy",
                                          "s1f_7_tos","s1f_8_dst","s1f_8_fqy","s1f_8_tos")])
describe(SISsub$s1f_Score_Raw)
boxplot(SISsub$s1f_Score_Raw, main="Social Activities Raw Score")
boxplot(s1f_Score_Raw~LivingType,data=SISsub, main="Social Activities Raw Score \nby Living Type")
aggregate(SISsub$s1f_Score_Raw, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(s1f_Score_Raw~LivingType, data=SISsub))

# Item: Raw Score
SISsub$s1_SocHome <- rowSums(SISsub[,c("s1f_1_dst","s1f_1_fqy","s1f_1_tos")])
table(SISsub$s1_SocHome)
table(SISsub$s1_SocHome,SISsub$LivingType)
t_s1_SocHome <- table(SISsub$s1_SocHome,SISsub$LivingType)
fisher.test(t_s1_SocHome, simulate.p.value = TRUE)

SISsub$s1_RecOthers <- rowSums(SISsub[,c("s1f_2_dst","s1f_2_fqy","s1f_2_tos")])
table(SISsub$s1_RecOthers)
table(SISsub$s1_RecOthers,SISsub$LivingType)
t_s1_RecOthers <- table(SISsub$s1_RecOthers,SISsub$LivingType)
fisher.test(t_s1_RecOthers, simulate.p.value = TRUE)

SISsub$s1_SocOut <- rowSums(SISsub[,c("s1f_3_dst","s1f_3_fqy","s1f_3_tos")])
table(SISsub$s1_SocOut)
table(SISsub$s1_SocOut,SISsub$LivingType)
t_s1_SocOut <- table(SISsub$s1_SocOut,SISsub$LivingType)
fisher.test(t_s1_SocOut, simulate.p.value = TRUE)

SISsub$s1_Friends <- rowSums(SISsub[,c("s1f_4_dst","s1f_4_fqy","s1f_4_tos")])
table(SISsub$s1_Friends)
table(SISsub$s1_Friends,SISsub$LivingType)
t_s1_Friends <- table(SISsub$s1_Friends,SISsub$LivingType)
fisher.test(t_s1_Friends, simulate.p.value = TRUE)

SISsub$s1_CommHelp <- rowSums(SISsub[,c("s1f_5_dst","s1f_5_fqy","s1f_5_tos")])
table(SISsub$s1_CommHelp)
table(SISsub$s1_CommHelp,SISsub$LivingType)
t_s1_CommHelp <- table(SISsub$s1_CommHelp,SISsub$LivingType)
fisher.test(t_s1_CommHelp, simulate.p.value = TRUE)

SISsub$s1_AppropSS <- rowSums(SISsub[,c("s1f_6_dst","s1f_6_fqy","s1f_6_tos")])
table(SISsub$s1_AppropSS)
table(SISsub$s1_AppropSS,SISsub$LivingType)
t_s1_AppropSS <- table(SISsub$s1_AppropSS,SISsub$LivingType)
fisher.test(t_s1_AppropSS, simulate.p.value = TRUE)

SISsub$s1_IntRela <- rowSums(SISsub[,c("s1f_7_dst","s1f_7_fqy","s1f_7_tos")])
table(SISsub$s1_IntRela)
table(SISsub$s1_IntRela,SISsub$LivingType)
t_s1_IntRela <- table(SISsub$s1_IntRela,SISsub$LivingType)
fisher.test(t_s1_IntRela, simulate.p.value = TRUE)

SISsub$s1_Volunteer <- rowSums(SISsub[,c("s1f_8_dst","s1f_8_fqy","s1f_8_tos")])
table(SISsub$s1_Volunteer)
table(SISsub$s1_Volunteer,SISsub$LivingType)
t_s1_Volunteer <- table(SISsub$s1_Volunteer,SISsub$LivingType)
fisher.test(t_s1_Volunteer, simulate.p.value = TRUE)

# Question: Raw Score
# Socializing at Home
table(SISsub$s1f_1_dst)	table(SISsub$s1f_1_dst,SISsub$LivingType) chisq.test(table(SISsub$s1f_1_dst,SISsub$LivingType))
table(SISsub$s1f_1_fqy)	table(SISsub$s1f_1_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1f_1_fqy,SISsub$LivingType))
table(SISsub$s1f_1_tos)	table(SISsub$s1f_1_tos,SISsub$LivingType) chisq.test(table(SISsub$s1f_1_tos,SISsub$LivingType))
# Recreation with Others
table(SISsub$s1f_2_dst)	table(SISsub$s1f_2_dst,SISsub$LivingType) fisher.test(table(SISsub$s1f_2_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1f_2_fqy)	table(SISsub$s1f_2_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1f_2_fqy,SISsub$LivingType))
table(SISsub$s1f_2_tos)	table(SISsub$s1f_2_tos,SISsub$LivingType) chisq.test(table(SISsub$s1f_2_tos,SISsub$LivingType))
# Socializing out of Home
table(SISsub$s1f_3_dst)	table(SISsub$s1f_3_dst,SISsub$LivingType) chisq.test(table(SISsub$s1f_3_dst,SISsub$LivingType))
table(SISsub$s1f_3_fqy)	table(SISsub$s1f_3_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1f_3_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1f_3_tos)	table(SISsub$s1f_3_tos,SISsub$LivingType) chisq.test(table(SISsub$s1f_3_tos,SISsub$LivingType))
# Making Friends
table(SISsub$s1f_4_dst)	table(SISsub$s1f_4_dst,SISsub$LivingType) chisq.test(table(SISsub$s1f_4_dst,SISsub$LivingType))
table(SISsub$s1f_4_fqy)	table(SISsub$s1f_4_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1f_4_fqy,SISsub$LivingType))
table(SISsub$s1f_4_tos)	table(SISsub$s1f_4_tos,SISsub$LivingType) chisq.test(table(SISsub$s1f_4_tos,SISsub$LivingType))
# Communication with Helpers
table(SISsub$s1f_5_dst)	table(SISsub$s1f_5_dst,SISsub$LivingType) chisq.test(table(SISsub$s1f_5_dst,SISsub$LivingType))
table(SISsub$s1f_5_fqy)	table(SISsub$s1f_5_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1f_5_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1f_5_tos)	table(SISsub$s1f_5_tos,SISsub$LivingType) chisq.test(table(SISsub$s1f_5_tos,SISsub$LivingType))
# Appropriate Social Skills
table(SISsub$s1f_6_dst)	table(SISsub$s1f_6_dst,SISsub$LivingType) chisq.test(table(SISsub$s1f_6_dst,SISsub$LivingType))
table(SISsub$s1f_6_fqy)	table(SISsub$s1f_6_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1f_6_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1f_6_tos)	table(SISsub$s1f_6_tos,SISsub$LivingType) chisq.test(table(SISsub$s1f_6_tos,SISsub$LivingType))
# Intimate Relationships
table(SISsub$s1f_7_dst)	table(SISsub$s1f_7_dst,SISsub$LivingType) chisq.test(table(SISsub$s1f_7_dst,SISsub$LivingType))
table(SISsub$s1f_7_fqy)	table(SISsub$s1f_7_fqy,SISsub$LivingType) fisher.test(table(SISsub$s1f_7_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s1f_7_tos)	table(SISsub$s1f_7_tos,SISsub$LivingType) chisq.test(table(SISsub$s1f_7_tos,SISsub$LivingType))
# Volunteer Work
table(SISsub$s1f_8_dst)	table(SISsub$s1f_8_dst,SISsub$LivingType) chisq.test(table(SISsub$s1f_8_dst,SISsub$LivingType))
table(SISsub$s1f_8_fqy)	table(SISsub$s1f_8_fqy,SISsub$LivingType) chisq.test(table(SISsub$s1f_8_fqy,SISsub$LivingType))
table(SISsub$s1f_8_tos)	table(SISsub$s1f_8_tos,SISsub$LivingType) chisq.test(table(SISsub$s1f_8_tos,SISsub$LivingType))
#################################################
# Protection & Advocacy
# Section: Raw Score
# colnames(SISsub)[(440:447)] 
SISsub$s2_Score_Total <- rowSums(SISsub[,c(440:447)])
describe(SISsub$s2_Score_Total)
boxplot(SISsub$s2_Score_Total, main="Protection & Advocacy")
boxplot(s2_Score_Total~LivingType,data=SISsub, main="Protection & Advocacy by Living Type")
aggregate(SISsub$s2_Score_Total, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(s2_Score_Total~LivingType, data=SISsub))

# Item: Raw Score
table(SISsub$self_advoc)  table(SISsub$self_advoc,SISsub$LivingType)  fisher.test(table(SISsub$self_advoc,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$money_mgmt)  table(SISsub$money_mgmt,SISsub$LivingType)  fisher.test(table(SISsub$money_mgmt,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$no_exploit)  table(SISsub$no_exploit,SISsub$LivingType)  fisher.test(table(SISsub$no_exploit,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$legal_resp)  table(SISsub$legal_resp,SISsub$LivingType)  fisher.test(table(SISsub$legal_resp,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$participate) table(SISsub$participate,SISsub$LivingType) fisher.test(table(SISsub$participate,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$legal_srvs)  table(SISsub$legal_srvs,SISsub$LivingType)  fisher.test(table(SISsub$legal_srvs,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$decisions)   table(SISsub$decisions,SISsub$LivingType)   fisher.test(table(SISsub$decisions,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$other_advoc) table(SISsub$other_advoc,SISsub$LivingType) fisher.test(table(SISsub$other_advoc,SISsub$LivingType), simulate.p.value = TRUE)

# Question: Raw Score
# Self-Advocacy
table(SISsub$s2_1_dst)	table(SISsub$s2_1_dst,SISsub$LivingType)  chisq.test(table(SISsub$s2_1_dst,SISsub$LivingType))
table(SISsub$s2_1_fqy)	table(SISsub$s2_1_fqy,SISsub$LivingType)  chisq.test(table(SISsub$s2_1_fqy,SISsub$LivingType))
table(SISsub$s2_1_tos)	table(SISsub$s2_1_tos,SISsub$LivingType) chisq.test(table(SISsub$s2_1_tos,SISsub$LivingType))
# Money Management
table(SISsub$s2_2_dst)	table(SISsub$s2_2_dst,SISsub$LivingType) fisher.test(table(SISsub$s2_2_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s2_2_fqy)	table(SISsub$s2_2_fqy,SISsub$LivingType) fisher.test(table(SISsub$s2_2_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s2_2_tos)	table(SISsub$s2_2_tos,SISsub$LivingType) fisher.test(table(SISsub$s2_2_tos,SISsub$LivingType), simulate.p.value = TRUE)
# Expoited by Others
table(SISsub$s2_3_dst)	table(SISsub$s2_3_dst,SISsub$LivingType) chisq.test(table(SISsub$s2_3_dst,SISsub$LivingType))
table(SISsub$s2_3_fqy)	table(SISsub$s2_3_fqy,SISsub$LivingType) chisq.test(table(SISsub$s2_3_fqy,SISsub$LivingType))
table(SISsub$s2_3_tos)	table(SISsub$s2_3_tos,SISsub$LivingType) chisq.test(table(SISsub$s2_3_tos,SISsub$LivingType))
# Legal Responsibility
table(SISsub$s2_4_dst)	table(SISsub$s2_4_dst,SISsub$LivingType) chisq.test(table(SISsub$s2_4_dst,SISsub$LivingType))
table(SISsub$s2_4_fqy)	table(SISsub$s2_4_fqy,SISsub$LivingType) fisher.test(table(SISsub$s2_4_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s2_4_tos)	table(SISsub$s2_4_tos,SISsub$LivingType) chisq.test(table(SISsub$s2_4_tos,SISsub$LivingType))
# Participation
table(SISsub$s2_5_dst)	table(SISsub$s2_5_dst,SISsub$LivingType) fisher.test(table(SISsub$s2_5_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s2_5_fqy)	table(SISsub$s2_5_fqy,SISsub$LivingType) chisq.test(table(SISsub$s2_5_fqy,SISsub$LivingType))
table(SISsub$s2_5_tos)	table(SISsub$s2_5_tos,SISsub$LivingType) chisq.test(table(SISsub$s2_5_tos,SISsub$LivingType))
# Legal Services
table(SISsub$s2_6_dst)	table(SISsub$s2_6_dst,SISsub$LivingType) fisher.test(table(SISsub$s2_6_dst,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s2_6_fqy)	table(SISsub$s2_6_fqy,SISsub$LivingType) fisher.test(table(SISsub$s2_6_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s2_6_tos)	table(SISsub$s2_6_tos,SISsub$LivingType) fisher.test(table(SISsub$s2_6_tos,SISsub$LivingType), simulate.p.value = TRUE)
# Decision Making
table(SISsub$s2_7_dst)	table(SISsub$s2_7_dst,SISsub$LivingType) chisq.test(table(SISsub$s2_7_dst,SISsub$LivingType))
table(SISsub$s2_7_fqy)	table(SISsub$s2_7_fqy,SISsub$LivingType) fisher.test(table(SISsub$s2_7_fqy,SISsub$LivingType), simulate.p.value = TRUE)
table(SISsub$s2_7_tos)	table(SISsub$s2_7_tos,SISsub$LivingType) chisq.test(table(SISsub$s2_7_tos,SISsub$LivingType))
# Other Advocacy
table(SISsub$s2_8_dst)	table(SISsub$s2_8_dst,SISsub$LivingType) chisq.test(table(SISsub$s2_8_dst,SISsub$LivingType))
table(SISsub$s2_8_fqy)	table(SISsub$s2_8_fqy,SISsub$LivingType) chisq.test(table(SISsub$s2_8_fqy,SISsub$LivingType))
table(SISsub$s2_8_tos)	table(SISsub$s2_8_tos,SISsub$LivingType) chisq.test(table(SISsub$s2_8_tos,SISsub$LivingType))
#################################################
# Medical Supports
# Section: Raw Score
describe(SISsub$s3a_Score_Total)
boxplot(SISsub$s3a_Score_Total, main="Medical Supports")
boxplot(s3a_Score_Total~LivingType,data=SISsub, main="Medical Supports by Living Type")
aggregate(SISsub$s3a_Score_Total, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(s3a_Score_Total~LivingType, data=SISsub))

# Item/Question: Raw Score
table(SISsub$s3a_1_support)	  table(SISsub$s3a_1_support,SISsub$LivingType) # Oxygen Therapy
chisq.test(table(SISsub$s3a_1_support,SISsub$LivingType))
table(SISsub$s3a_2_support)	  table(SISsub$s3a_2_support,SISsub$LivingType) # Postural Drainage
fisher.test(table(SISsub$s3a_2_support,SISsub$LivingType))
table(SISsub$s3a_3_support)	  table(SISsub$s3a_3_support,SISsub$LivingType) # Chest PT
fisher.test(table(SISsub$s3a_3_support,SISsub$LivingType))
table(SISsub$s3a_4_support)	  table(SISsub$s3a_4_support,SISsub$LivingType) # Suctioning
fisher.test(table(SISsub$s3a_4_support,SISsub$LivingType))
table(SISsub$s3a_5_support)	  table(SISsub$s3a_5_support,SISsub$LivingType) # Oral Stimulation
fisher.test(table(SISsub$s3a_5_support,SISsub$LivingType))
table(SISsub$s3a_6_support)	  table(SISsub$s3a_6_support,SISsub$LivingType) # Tube Feeding
fisher.test(table(SISsub$s3a_6_support,SISsub$LivingType))
table(SISsub$s3a_7_support)	  table(SISsub$s3a_7_support,SISsub$LivingType) # Parental Feeding
# chisq.test(table(SISsub$s3a_7_support,SISsub$LivingType))
table(SISsub$s3a_8_support)	  table(SISsub$s3a_8_support,SISsub$LivingType) # Positioning
chisq.test(table(SISsub$s3a_8_support,SISsub$LivingType))
table(SISsub$s3a_9_support)	  table(SISsub$s3a_9_support,SISsub$LivingType) # Dressing Wounds
chisq.test(table(SISsub$s3a_9_support,SISsub$LivingType))
table(SISsub$s3a_10_support)	table(SISsub$s3a_10_support,SISsub$LivingType) # Prevent Infection
chisq.test(table(SISsub$s3a_10_support,SISsub$LivingType))
table(SISsub$s3a_11_support)	table(SISsub$s3a_11_support,SISsub$LivingType) # Seizure Management
chisq.test(table(SISsub$s3a_11_support,SISsub$LivingType))
table(SISsub$s3a_12_support)	table(SISsub$s3a_12_support,SISsub$LivingType) # Dialysis
# fisher.test(table(SISsub$s3a_12_support,SISsub$LivingType))
table(SISsub$s3a_13_support)	table(SISsub$s3a_13_support,SISsub$LivingType) # Ostomy Care
chisq.test(table(SISsub$s3a_13_support,SISsub$LivingType))
table(SISsub$s3a_14_support)	table(SISsub$s3a_14_support,SISsub$LivingType) # Transfers
chisq.test(table(SISsub$s3a_14_support,SISsub$LivingType))
table(SISsub$s3a_15_support)	table(SISsub$s3a_15_support,SISsub$LivingType) # Therapy Services
chisq.test(table(SISsub$s3a_15_support,SISsub$LivingType))
table(SISsub$s3a_16_support)	table(SISsub$s3a_16_support,SISsub$LivingType) # Other Medical
chisq.test(table(SISsub$s3a_16_support,SISsub$LivingType))
#################################################
# Behavioral Supports
# Section: Raw Score
describe(SISsub$s3b_Score_Total)
boxplot(SISsub$s3b_Score_Total, main="Behavioral Supports")
boxplot(s3b_Score_Total~LivingType,data=SISsub, main="Behavioral Supports by Living Type")
aggregate(SISsub$s3b_Score_Total, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(s3b_Score_Total~LivingType, data=SISsub))

# Item/Question: Raw Score
table(SISsub$s3b_1_support)	  table(SISsub$s3b_1_support,SISsub$LivingType) # Assault
chisq.test(table(SISsub$s3b_1_support,SISsub$LivingType))
table(SISsub$s3b_2_support)	  table(SISsub$s3b_2_support,SISsub$LivingType) # Propery Destruction
chisq.test(table(SISsub$s3b_2_support,SISsub$LivingType))
table(SISsub$s3b_3_support)	  table(SISsub$s3b_3_support,SISsub$LivingType) # Stealing
chisq.test(table(SISsub$s3b_3_support,SISsub$LivingType))
table(SISsub$s3b_4_support)	  table(SISsub$s3b_4_support,SISsub$LivingType) # Self Injury
chisq.test(table(SISsub$s3b_4_support,SISsub$LivingType))
table(SISsub$s3b_5_support)	  table(SISsub$s3b_5_support,SISsub$LivingType) # Pica
chisq.test(table(SISsub$s3b_5_support,SISsub$LivingType))
table(SISsub$s3b_6_support)	  table(SISsub$s3b_6_support,SISsub$LivingType) # Suicide Attempts
fisher.test(table(SISsub$s3b_6_support,SISsub$LivingType))
table(SISsub$s3b_7_support)	  table(SISsub$s3b_7_support,SISsub$LivingType) # Sexual Aggression
chisq.test(table(SISsub$s3b_7_support,SISsub$LivingType))
table(SISsub$s3b_8_support)	  table(SISsub$s3b_8_support,SISsub$LivingType) # Inappropriate
chisq.test(table(SISsub$s3b_8_support,SISsub$LivingType))
table(SISsub$s3b_9_support)	  table(SISsub$s3b_9_support,SISsub$LivingType) # Outbursts
chisq.test(table(SISsub$s3b_9_support,SISsub$LivingType))
table(SISsub$s3b_10_support)	table(SISsub$s3b_10_support,SISsub$LivingType) # Wandering
chisq.test(table(SISsub$s3b_10_support,SISsub$LivingType))
table(SISsub$s3b_11_support)	table(SISsub$s3b_11_support,SISsub$LivingType) # Substance Abuse
fisher.test(table(SISsub$s3b_11_support,SISsub$LivingType))
table(SISsub$s3b_12_support)	table(SISsub$s3b_12_support,SISsub$LivingType) # Mental Health Tx
chisq.test(table(SISsub$s3b_12_support,SISsub$LivingType))
table(SISsub$s3b_13_support)	table(SISsub$s3b_13_support,SISsub$LivingType) # Other Behavioral
chisq.test(table(SISsub$s3b_13_support,SISsub$LivingType))
#################################################
# Total Standard Scores
describe(SISsub$TotalStandard)
boxplot(SISsub$TotalStandard, main="Total Standardized Score")
boxplot(TotalStandard~LivingType,data=SISsub, main="Total Standardized Score \nby Living Type")
aggregate(SISsub$TotalStandard, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(TotalStandard~LivingType, data=SISsub))

describe(SISsub$SupportNeedsIndex)
boxplot(SISsub$SupportNeedsIndex, main="Support Needs Index")
boxplot(SupportNeedsIndex~LivingType,data=SISsub, main="Support Needs Index \nby Living Type")
aggregate(SISsub$SupportNeedsIndex, by=list(SISsub$LivingType), FUN = function(x) 
  c(mean=mean(x), se=std.error(x), cil=(mean(x)-2*std.error(x)), ciu=(mean(x)+2*std.error(x))) )
summary(aov(SupportNeedsIndex~LivingType, data=SISsub))