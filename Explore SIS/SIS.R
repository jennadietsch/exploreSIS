## Default repository
local({r <- getOption("repos")
  r["CRAN"] <- "http://cran.case.edu" 
  options(repos=r)
})
#################################################
## Packages
library(lubridate)
library(dplyr)
library(plyr)
library(psych)
library(plotrix)
library(car)
library(caret)
#################################################
## Read in SIS data
SISdata <- read.csv("scrub_sis.csv", header = TRUE, sep = ',', na.strings = "")
colnames(SISdata)
str(SISdata)
#################################################
# Data Cleaning
# Removing Rows with NA Living Type
SISdata <- SISdata[which(SISdata$LivingType!="NA"),]
## levels(SISdata$LivingType)
SISdata$LivingType = factor(SISdata$LivingType)
## levels(SISdata$LivingType)
#################################################
# Identifying Duplicate Assessments Per ID
dups <- data.frame(table(SISdata$fake_id))
dups[which(dups$Freq >1), ]
#################################################
# Single Assessment Per ID
# colnames(SISdata)[12] 
SISout <- SISdata[rev(order(as.Date(SISdata[,12]))), ] # SIS assessment date in descending order
SISout$rownum <- ave(SISout$fake_id, SISout$fake_id, FUN = seq_along) # Creating row number by fake_id
SISdf <- SISout[which(SISout$rownum==1), ] # Subsetting to include most recent assessment per fake_id
#################################################
# Section One & Section Two
# Rating: Frequency of Support
#   4= hourly or more frequently
#   3=at least once a day but not once an hour
#   2=at least once a week, but not once a day
#   1=at least once a month, but not once a week
#   0=none or less than monthly

# Rating: Daily Support Time
#   4=4 hours or more
#   3=2 hours to less than 4 hours
#   2=30 minutes to less than 2 hours
#   1=less than 30 minutes
#   0=none

# Rating: Type of Support
#   4=full physical assistance
#   3=partial physical assistance
#   2=verbal/gestural prompting
#   1=monitoring
#   0=none


# Section Three
#   0=no support needed
#   1=some support needed
#   2=extensive support needed
#################################################
# Recoding Section One & Two Variables: Condesning Categories Three & Four
SISdf[ ,39:323] <- lapply(SISdf[ ,39:323], FUN = function(x) recode(x, "4=3;3=3;2=2;1=1;0=0"))
##################################################
# Creating training and test set
trainIndex <- createDataPartition(SISdf$LivingType, p = .8, list = FALSE, times = 1)

SISsub <- SISdf[ trainIndex,]
SISsubTest  <- SISdf[-trainIndex,]
##################################################
# Dependent Variable: Living Type
LT_table <- table(SISsub$LivingType)
prop.table(LT_table)
#################################################
# Descriptive Statistics & Univariate Analysis
# Section One: Home Living
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
#################################################
# Section One: Community Living
# Item: Raw Score
SISsub$s1_GetAround <- rowSums(SISsub[,c("s1b_1_dst","s1b_1_fqy","s1b_1_tos")])
table(SISsub$s1_GetAround)
t_s1_GetAround <- table(SISsub$s1_GetAround,SISsub$LivingType)
fisher.test(t_s1_GetAround, simulate.p.value = TRUE)

SISsub$s1_Rec <- rowSums(SISsub[,c("s1b_2_dst","s1b_2_fqy","s1b_2_tos")])
table(SISsub$s1_Rec)
t_s1_Rec <- table(SISsub$s1_Rec,SISsub$LivingType)
fisher.test(t_s1_Rec, simulate.p.value = TRUE)

SISsub$s1_PubService <- rowSums(SISsub[,c("s1b_3_dst","s1b_3_fqy","s1b_3_tos")])
table(SISsub$s1_PubService)
t_s1_PubService <- table(SISsub$s1_PubService,SISsub$LivingType)
fisher.test(t_s1_PubService, simulate.p.value = TRUE)

SISsub$s1_Visits <- rowSums(SISsub[,c("s1b_4_dst","s1b_4_fqy","s1b_4_tos")])
table(SISsub$s1_Visits)
t_s1_Visits <- table(SISsub$s1_Visits,SISsub$LivingType)
fisher.test(t_s1_Visits, simulate.p.value = TRUE)

SISsub$s1_PrefActiv <- rowSums(SISsub[,c("s1b_5_dst","s1b_5_fqy","s1b_5_tos")])
table(SISsub$s1_PrefActiv)
t_s1_PrefActiv <- table(SISsub$s1_PrefActiv,SISsub$LivingType)
fisher.test(t_s1_PrefActiv, simulate.p.value = TRUE)

SISsub$s1_Shop <- rowSums(SISsub[,c("s1b_6_dst","s1b_6_fqy","s1b_6_tos")])
table(SISsub$s1_Shop)
t_s1_Shop <- table(SISsub$s1_Shop,SISsub$LivingType)
fisher.test(t_s1_Shop, simulate.p.value = TRUE)

SISsub$s1_CommInt <- rowSums(SISsub[,c("s1b_7_dst","s1b_7_fqy","s1b_7_tos")])
table(SISsub$s1_CommInt)
t_s1_CommInt <- table(SISsub$s1_CommInt,SISsub$LivingType)
fisher.test(t_s1_CommInt, simulate.p.value = TRUE)

SISsub$s1_AccessSet <- rowSums(SISsub[,c("s1b_8_dst","s1b_8_fqy","s1b_8_tos")])
table(SISsub$s1_AccessSet)
t_s1_AccessSet <- table(SISsub$s1_AccessSet,SISsub$LivingType)
fisher.test(t_s1_AccessSet, simulate.p.value = TRUE)
#################################################
# Section One: Lifelong Learning
# Item: Raw Score
SISsub$s1_LearnInteract <- rowSums(SISsub[,c("s1c_1_dst","s1c_1_fqy","s1c_1_tos")])
table(SISsub$s1_LearnInteract)
t_s1_LearnInteract <- table(SISsub$s1_LearnInteract,SISsub$LivingType)
fisher.test(t_s1_LearnInteract, simulate.p.value = TRUE)

SISsub$s1_LearnDecsn <- rowSums(SISsub[,c("s1c_2_dst","s1c_2_fqy","s1c_2_tos")])
table(SISsub$s1_LearnDecsn)
t_s1_LearnDecsn <- table(SISsub$s1_LearnDecsn,SISsub$LivingType)
fisher.test(t_s1_LearnDecsn, simulate.p.value = TRUE)

SISsub$s1_ProbSolve <- rowSums(SISsub[,c("s1c_3_dst","s1c_3_fqy","s1c_3_tos")])
table(SISsub$s1_ProbSolve)
t_s1_ProbSolve <- table(SISsub$s1_ProbSolve,SISsub$LivingType)
fisher.test(t_s1_ProbSolve, simulate.p.value = TRUE)

SISsub$s1_UseTech <- rowSums(SISsub[,c("s1c_4_dst","s1c_4_fqy","s1c_4_tos")])
table(SISsub$s1_UseTech)
t_s1_UseTech <- table(SISsub$s1_UseTech,SISsub$LivingType)
fisher.test(t_s1_UseTech, simulate.p.value = TRUE)

SISsub$s1_AccessTrain <- rowSums(SISsub[,c("s1c_5_dst","s1c_5_fqy","s1c_5_tos")])
table(SISsub$s1_AccessTrain)
t_s1_AccessTrain <- table(SISsub$s1_AccessTrain,SISsub$LivingType)
fisher.test(t_s1_AccessTrain, simulate.p.value = TRUE)

SISsub$s1_Academic <- rowSums(SISsub[,c("s1c_6_dst","s1c_6_fqy","s1c_6_tos")])
table(SISsub$s1_Academic)
t_s1_Academic <- table(SISsub$s1_Academic,SISsub$LivingType)
fisher.test(t_s1_Academic, simulate.p.value = TRUE)

SISsub$s1_HealthSkill <- rowSums(SISsub[,c("s1c_7_dst","s1c_7_fqy","s1c_7_tos")])
table(SISsub$s1_HealthSkill)
t_s1_HealthSkill <- table(SISsub$s1_HealthSkill,SISsub$LivingType)
fisher.test(t_s1_HealthSkill, simulate.p.value = TRUE)

SISsub$s1_SelfDeterm <- rowSums(SISsub[,c("s1c_8_dst","s1c_8_fqy","s1c_8_tos")])
table(SISsub$s1_SelfDeterm)
t_s1_SelfDeterm <- table(SISsub$s1_SelfDeterm,SISsub$LivingType)
fisher.test(t_s1_SelfDeterm, simulate.p.value = TRUE)

SISsub$s1_SelfManage <- rowSums(SISsub[,c("s1c_9_dst","s1c_9_fqy","s1c_9_tos")])
table(SISsub$s1_SelfManage)
t_s1_SelfManage <- table(SISsub$s1_SelfManage,SISsub$LivingType)
fisher.test(t_s1_SelfManage, simulate.p.value = TRUE)
#################################################
# Section One: Employment
# Item: Raw Score
SISsub$s1_JobAccom <- rowSums(SISsub[,c("s1d_1_dst","s1d_1_fqy","s1d_1_tos")])
table(SISsub$s1_JobAccom)
t_s1_JobAccom <- table(SISsub$s1_JobAccom,SISsub$LivingType)
fisher.test(t_s1_JobAccom, simulate.p.value = TRUE)

SISsub$s1_JobSkill <- rowSums(SISsub[,c("s1d_2_dst","s1d_2_fqy","s1d_2_tos")])
table(SISsub$s1_JobSkill)
t_s1_JobSkill <- table(SISsub$s1_JobSkill,SISsub$LivingType)
fisher.test(t_s1_JobSkill, simulate.p.value = TRUE)

SISsub$s1_CoInteract <- rowSums(SISsub[,c("s1d_3_dst","s1d_3_fqy","s1d_3_tos")])
table(SISsub$s1_CoInteract)
t_s1_CoInteract <- table(SISsub$s1_CoInteract,SISsub$LivingType)
fisher.test(t_s1_CoInteract, simulate.p.value = TRUE)

SISsub$s1_SupInteract <- rowSums(SISsub[,c("s1d_4_dst","s1d_4_fqy","s1d_4_tos")])
table(SISsub$s1_SupInteract)
t_s1_SupInteract <- table(SISsub$s1_SupInteract,SISsub$LivingType)
fisher.test(t_s1_SupInteract, simulate.p.value = TRUE)

SISsub$s1_WrkSpd <- rowSums(SISsub[,c("s1d_5_dst","s1d_5_fqy","s1d_5_tos")])
table(SISsub$s1_WrkSpd)
t_s1_WrkSpd <- table(SISsub$s1_WrkSpd ,SISsub$LivingType)
fisher.test(t_s1_WrkSpd, simulate.p.value = TRUE)

SISsub$s1_WrkQual <- rowSums(SISsub[,c("s1d_6_dst","s1d_6_fqy","s1d_6_tos")])
table(SISsub$s1_WrkQual)
t_s1_WrkQual <- table(SISsub$s1_WrkQual,SISsub$LivingType)
fisher.test(t_s1_WrkQual, simulate.p.value = TRUE)

SISsub$s1_ChgAssign <- rowSums(SISsub[,c("s1d_7_dst","s1d_7_fqy","s1d_7_tos")])
table(SISsub$s1_ChgAssign)
t_s1_ChgAssign <- table(SISsub$s1_ChgAssign,SISsub$LivingType)
fisher.test(t_s1_ChgAssign, simulate.p.value = TRUE)

SISsub$s1_SeekAssist <- rowSums(SISsub[,c("s1d_8_dst","s1d_8_fqy","s1d_8_tos")])
table(SISsub$s1_SeekAssist)
t_s1_SeekAssist <- table(SISsub$s1_SeekAssist,SISsub$LivingType)
fisher.test(t_s1_SeekAssist, simulate.p.value = TRUE)
#################################################
# Section One: Health & Safety
# Item: Raw Score
SISsub$s1_TakeMeds <- rowSums(SISsub[,c("s1e_1_dst","s1e_1_fqy","s1e_1_tos")])
table(SISsub$s1_TakeMeds)
t_s1_TakeMeds <- table(SISsub$s1_TakeMeds,SISsub$LivingType)
fisher.test(t_s1_TakeMeds, simulate.p.value = TRUE)

SISsub$s1_AvoidHaz <- rowSums(SISsub[,c("s1e_2_dst","s1e_2_fqy","s1e_2_tos")])
table(SISsub$s1_AvoidHaz)
t_s1_AvoidHaz <- table(SISsub$s1_AvoidHaz,SISsub$LivingType)
fisher.test(t_s1_AvoidHaz, simulate.p.value = TRUE)

SISsub$s1_ObtainHC <- rowSums(SISsub[,c("s1e_3_dst","s1e_3_fqy","s1e_3_tos")])
table(SISsub$s1_ObtainHC)
t_s1_ObtainHC <- table(SISsub$s1_ObtainHC,SISsub$LivingType)
fisher.test(t_s1_ObtainHC, simulate.p.value = TRUE)

SISsub$s1_Moving <- rowSums(SISsub[,c("s1e_4_dst","s1e_4_fqy","s1e_4_tos")])
table(SISsub$s1_Moving)
t_s1_Moving <- table(SISsub$s1_Moving,SISsub$LivingType)
fisher.test(t_s1_Moving, simulate.p.value = TRUE)

SISsub$s1_EmergSvc <- rowSums(SISsub[,c("s1e_5_dst","s1e_5_fqy","s1e_5_tos")])
table(SISsub$s1_EmergSvc)
t_s1_EmergSvc <- table(SISsub$s1_EmergSvc,SISsub$LivingType)
fisher.test(t_s1_EmergSvc, simulate.p.value = TRUE)

SISsub$s1_Nutrition <- rowSums(SISsub[,c("s1e_6_dst","s1e_6_fqy","s1e_6_tos")])
table(SISsub$s1_Nutrition)
t_s1_Nutrition <- table(SISsub$s1_Nutrition,SISsub$LivingType)
fisher.test(t_s1_Nutrition, simulate.p.value = TRUE)

SISsub$s1_Fitness <- rowSums(SISsub[,c("s1e_7_dst","s1e_7_fqy","s1e_7_tos")])
table(SISsub$s1_Fitness)
t_s1_Fitness <- table(SISsub$s1_Fitness,SISsub$LivingType)
fisher.test(t_s1_Fitness, simulate.p.value = TRUE)

SISsub$s1_EmoWelBe <- rowSums(SISsub[,c("s1e_8_dst","s1e_8_fqy","s1e_8_tos")])
table(SISsub$s1_EmoWelBe)
t_s1_EmoWelBe <- table(SISsub$s1_EmoWelBe,SISsub$LivingType)
fisher.test(t_s1_EmoWelBe, simulate.p.value = TRUE)
#################################################
# Section One: Social Activities
# Item: Raw Score
SISsub$s1_SocHome <- rowSums(SISsub[,c("s1f_1_dst","s1f_1_fqy","s1f_1_tos")])
table(SISsub$s1_SocHome)
t_s1_SocHome <- table(SISsub$s1_SocHome,SISsub$LivingType)
fisher.test(t_s1_SocHome, simulate.p.value = TRUE)

SISsub$s1_RecOthers <- rowSums(SISsub[,c("s1f_2_dst","s1f_2_fqy","s1f_2_tos")])
table(SISsub$s1_RecOthers)
t_s1_RecOthers <- table(SISsub$s1_RecOthers,SISsub$LivingType)
fisher.test(t_s1_RecOthers, simulate.p.value = TRUE)

SISsub$s1_SocOut <- rowSums(SISsub[,c("s1f_3_dst","s1f_3_fqy","s1f_3_tos")])
table(SISsub$s1_SocOut)
t_s1_SocOut <- table(SISsub$s1_SocOut,SISsub$LivingType)
fisher.test(t_s1_SocOut, simulate.p.value = TRUE)

SISsub$s1_Friends <- rowSums(SISsub[,c("s1f_4_dst","s1f_4_fqy","s1f_4_tos")])
table(SISsub$s1_Friends)
t_s1_Friends <- table(SISsub$s1_Friends,SISsub$LivingType)
fisher.test(t_s1_Friends, simulate.p.value = TRUE)

SISsub$s1_CommHelp <- rowSums(SISsub[,c("s1f_5_dst","s1f_5_fqy","s1f_5_tos")])
table(SISsub$s1_CommHelp)
t_s1_CommHelp <- table(SISsub$s1_CommHelp,SISsub$LivingType)
fisher.test(t_s1_CommHelp, simulate.p.value = TRUE)

SISsub$s1_AppropSS <- rowSums(SISsub[,c("s1f_6_dst","s1f_6_fqy","s1f_6_tos")])
table(SISsub$s1_AppropSS)
t_s1_AppropSS <- table(SISsub$s1_AppropSS,SISsub$LivingType)
fisher.test(t_s1_AppropSS, simulate.p.value = TRUE)

SISsub$s1_IntRela <- rowSums(SISsub[,c("s1f_7_dst","s1f_7_fqy","s1f_7_tos")])
table(SISsub$s1_IntRela)
t_s1_IntRela <- table(SISsub$s1_IntRela,SISsub$LivingType)
fisher.test(t_s1_IntRela, simulate.p.value = TRUE)

SISsub$s1_Volunteer <- rowSums(SISsub[,c("s1f_8_dst","s1f_8_fqy","s1f_8_tos")])
table(SISsub$s1_Volunteer)
t_s1_Volunteer <- table(SISsub$s1_Volunteer,SISsub$LivingType)
fisher.test(t_s1_Volunteer, simulate.p.value = TRUE)
#################################################
# Section Two: Protection & Advocacy
# Item: Raw Score
# Self Advocacy
table(SISsub$self_advoc)  
table(SISsub$self_advoc,SISsub$LivingType)  
fisher.test(table(SISsub$self_advoc,SISsub$LivingType), simulate.p.value = TRUE)

# Money Management
table(SISsub$money_mgmt)  
table(SISsub$money_mgmt,SISsub$LivingType)  
fisher.test(table(SISsub$money_mgmt,SISsub$LivingType), simulate.p.value = TRUE)

# Exploited by Others
table(SISsub$no_exploit)  
table(SISsub$no_exploit,SISsub$LivingType)  
fisher.test(table(SISsub$no_exploit,SISsub$LivingType), simulate.p.value = TRUE)

# Legal Responsibility
table(SISsub$legal_resp)  
table(SISsub$legal_resp,SISsub$LivingType)  
fisher.test(table(SISsub$legal_resp,SISsub$LivingType), simulate.p.value = TRUE)

# Participation
table(SISsub$participate) 
table(SISsub$participate,SISsub$LivingType) 
fisher.test(table(SISsub$participate,SISsub$LivingType), simulate.p.value = TRUE)

# Legal Services
table(SISsub$legal_srvs)
table(SISsub$legal_srvs,SISsub$LivingType)
fisher.test(table(SISsub$legal_srvs,SISsub$LivingType), simulate.p.value = TRUE)

# Decision Making
table(SISsub$decisions) 
table(SISsub$decisions,SISsub$LivingType)
fisher.test(table(SISsub$decisions,SISsub$LivingType), simulate.p.value = TRUE)

# Other Advocacy
table(SISsub$other_advoc)
table(SISsub$other_advoc,SISsub$LivingType)
fisher.test(table(SISsub$other_advoc,SISsub$LivingType), simulate.p.value = TRUE)
#################################################
# Section Three: Medical Supports
# Item: Raw Score
# Oxygen Therapy
table(SISsub$s3a_1_support)
table(SISsub$s3a_1_support,SISsub$LivingType)
chisq.test(table(SISsub$s3a_1_support,SISsub$LivingType))

# Postural Drainage
table(SISsub$s3a_2_support)
table(SISsub$s3a_2_support,SISsub$LivingType)
fisher.test(table(SISsub$s3a_2_support,SISsub$LivingType))

# Chest PT
table(SISsub$s3a_3_support)	
table(SISsub$s3a_3_support,SISsub$LivingType)
fisher.test(table(SISsub$s3a_3_support,SISsub$LivingType))

# Suctioning
table(SISsub$s3a_4_support)
table(SISsub$s3a_4_support,SISsub$LivingType)
fisher.test(table(SISsub$s3a_4_support,SISsub$LivingType))

# Oral Stimulation
table(SISsub$s3a_5_support)
table(SISsub$s3a_5_support,SISsub$LivingType)
fisher.test(table(SISsub$s3a_5_support,SISsub$LivingType))

# Tube Feeding
table(SISsub$s3a_6_support)
table(SISsub$s3a_6_support,SISsub$LivingType)
fisher.test(table(SISsub$s3a_6_support,SISsub$LivingType))

# Parental Feeding
table(SISsub$s3a_7_support)
table(SISsub$s3a_7_support,SISsub$LivingType)
chisq.test(table(SISsub$s3a_7_support,SISsub$LivingType))

# Positioning
table(SISsub$s3a_8_support)
table(SISsub$s3a_8_support,SISsub$LivingType)
chisq.test(table(SISsub$s3a_8_support,SISsub$LivingType))

# Dressing Wounds
table(SISsub$s3a_9_support)
table(SISsub$s3a_9_support,SISsub$LivingType)
chisq.test(table(SISsub$s3a_9_support,SISsub$LivingType))

# Prevent Infection
table(SISsub$s3a_10_support)
table(SISsub$s3a_10_support,SISsub$LivingType)
chisq.test(table(SISsub$s3a_10_support,SISsub$LivingType))

# Seizure Management
table(SISsub$s3a_11_support)
table(SISsub$s3a_11_support,SISsub$LivingType)
chisq.test(table(SISsub$s3a_11_support,SISsub$LivingType))

# Dialysis
table(SISsub$s3a_12_support)
table(SISsub$s3a_12_support,SISsub$LivingType)
fisher.test(table(SISsub$s3a_12_support,SISsub$LivingType))

# Ostomy Care
table(SISsub$s3a_13_support)
table(SISsub$s3a_13_support,SISsub$LivingType)
chisq.test(table(SISsub$s3a_13_support,SISsub$LivingType))

# Transfers
table(SISsub$s3a_14_support)
table(SISsub$s3a_14_support,SISsub$LivingType)
chisq.test(table(SISsub$s3a_14_support,SISsub$LivingType))

# Therapy Services
table(SISsub$s3a_15_support)
table(SISsub$s3a_15_support,SISsub$LivingType)
chisq.test(table(SISsub$s3a_15_support,SISsub$LivingType))

# Other Medical
table(SISsub$s3a_16_support)
table(SISsub$s3a_16_support,SISsub$LivingType)
chisq.test(table(SISsub$s3a_16_support,SISsub$LivingType))
#################################################
# Section Three: Behavioral Supports
# Item: Raw Score
# Assault
table(SISsub$s3b_1_support)	  
table(SISsub$s3b_1_support,SISsub$LivingType)
chisq.test(table(SISsub$s3b_1_support,SISsub$LivingType))

# Propery Destruction
table(SISsub$s3b_2_support)
table(SISsub$s3b_2_support,SISsub$LivingType)
chisq.test(table(SISsub$s3b_2_support,SISsub$LivingType))

# Stealing
table(SISsub$s3b_3_support)
table(SISsub$s3b_3_support,SISsub$LivingType)
chisq.test(table(SISsub$s3b_3_support,SISsub$LivingType))

# Self Injury
table(SISsub$s3b_4_support)
table(SISsub$s3b_4_support,SISsub$LivingType)
chisq.test(table(SISsub$s3b_4_support,SISsub$LivingType))

# Pica
table(SISsub$s3b_5_support)
table(SISsub$s3b_5_support,SISsub$LivingType)
chisq.test(table(SISsub$s3b_5_support,SISsub$LivingType))

# Suicide Attempts
table(SISsub$s3b_6_support)
table(SISsub$s3b_6_support,SISsub$LivingType) 
fisher.test(table(SISsub$s3b_6_support,SISsub$LivingType))

# Sexual Aggression
table(SISsub$s3b_7_support)
table(SISsub$s3b_7_support,SISsub$LivingType)
chisq.test(table(SISsub$s3b_7_support,SISsub$LivingType))

# Inappropriate
table(SISsub$s3b_8_support)
table(SISsub$s3b_8_support,SISsub$LivingType)
chisq.test(table(SISsub$s3b_8_support,SISsub$LivingType))

# Outbursts
table(SISsub$s3b_9_support)
table(SISsub$s3b_9_support,SISsub$LivingType) 
chisq.test(table(SISsub$s3b_9_support,SISsub$LivingType))

# Wandering
table(SISsub$s3b_10_support)
table(SISsub$s3b_10_support,SISsub$LivingType) 
chisq.test(table(SISsub$s3b_10_support,SISsub$LivingType))

# Substance Abuse
table(SISsub$s3b_11_support)
table(SISsub$s3b_11_support,SISsub$LivingType) 
fisher.test(table(SISsub$s3b_11_support,SISsub$LivingType))

# Mental Health Tx
table(SISsub$s3b_12_support)
table(SISsub$s3b_12_support,SISsub$LivingType) 
chisq.test(table(SISsub$s3b_12_support,SISsub$LivingType))

# Other Behavioral
table(SISsub$s3b_13_support)
table(SISsub$s3b_13_support,SISsub$LivingType)
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
#################################################
# Testing Collinearity
# Subsetting df to include only significant predictors
myvars <- c("s1_Toilet","s1_Clothes","s1_PrepFood","s1_EatFood","s1_HouseKeep","s1_Dress","s1_Hygiene","s1_Appliance",
            "s1_GetAround","s1_Rec","s1_PubService","s1_Visits","s1_PrefActiv","s1_Shop","s1_CommInt","s1_AccessSet",
            "s1_LearnInteract","s1_LearnDecsn","s1_ProbSolve","s1_UseTech","s1_AccessTrain","s1_Academic","s1_HealthSkill","s1_SelfDeterm","s1_SelfManage",
            "s1_JobAccom","s1_JobSkill","s1_CoInteract","s1_SupInteract","s1_WrkSpd","s1_WrkQual","s1_ChgAssign","s1_SeekAssist",
            "s1_TakeMeds","s1_AvoidHaz","s1_ObtainHC","s1_Moving","s1_EmergSvc","s1_Nutrition","s1_Fitness","s1_EmoWelBe",
            "s1_SocHome","s1_RecOthers","s1_SocOut","s1_Friends","s1_CommHelp","s1_AppropSS","s1_IntRela","s1_Volunteer",
            "self_advoc","money_mgmt","no_exploit","legal_resp","participate","legal_srvs","decisions","other_advoc",
            "s3a_3_support","s3a_8_support","s3a_9_support","s3a_10_support","s3a_11_support","s3a_14_support","s3a_15_support","s3a_16_support",
            "s3b_1_support","s3b_2_support","s3b_3_support","s3b_4_support","s3b_5_support","s3b_6_support","s3b_7_support","s3b_8_support","s3b_9_support","s3b_10_support","s3b_12_support"
            )
# New df colltest with predictors from above
colltest <- SISsub[myvars]

combos <- combn(ncol(colltest),2) # Create combination of all columns
options(max.print=10000) # Increase number of rows printed

adply(combos[1:2,2001:2850], 2, function(x) {
  test <- fisher.test(colltest[, x[1]], colltest[, x[2]], simulate.p.value = TRUE)
  
  out <- data.frame("Row" = colnames(colltest)[x[1]],
                    "Column" = colnames(colltest[x[2]]),
                    "p.value" = round(test$p.value, 3)
  )
  return(out)
})

fisher.test(colltest$s1_ObtainHC, colltest$s3b_6_support, simulate.p.value = TRUE)
