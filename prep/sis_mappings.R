library(dplyr)

## GET NEEDS ELEMENTS ##
##############################################################################
# Pull individual assessment elements from SIS data

sis_names <- data.frame(colnames(sis_full))
names(sis_names)[1] <- "field"

needs <-
  sis_names %>%
  mutate(section = ifelse(grepl("^s[0-9]", field), 
                          yes = gsub("_.*$", "", field),
                          no = NA),
         item = ifelse(grepl("^s[0-9]", field), 
                       yes = gsub("fqy|dst|tos|to|for|notes|support","",field),
                       no = NA),
         item_type = ifelse(grepl("^s[0-9]", field), 
                            yes = gsub("^.*_","",field),
                            no = NA),
         section_desc = car::recode(section,
                                    "'s1a' = 'Home Living';
                                    's1b' = 'Community Living';
                                    's1c' = 'Lifelong Learning';
                                    's1d' = 'Employment';
                                    's1e' = 'Health and Safety';
                                    's1f' = 'Social Activities';
                                    's2' = 'Protection and Advocacy';
                                    's3a' = 'Medical Supports';
                                    's3b' = 'Behavioral Supports'"),
         item_desc = car::recode(item,
                                 "'s1a_1_' = 'Toilet';
                                 's1a_2_' = 'Clothes';
                                 's1a_3_' = 'Preparing food';
                                 's1a_4_' = 'Eating food';
                                 's1a_5_' = 'Housekeeping';
                                 's1a_6_' = 'Dressing';
                                 's1a_7_' = 'Hygiene';
                                 's1a_8_' = 'Appliances';
                                 's1b_1_' = 'Getting Around';
                                 's1b_2_' = 'Recreation';
                                 's1b_3_' = 'Public Services';
                                 's1b_4_' = 'Visit Friends/Family';
                                 's1b_5_' = 'Preferred Activities';
                                 's1b_6_' = 'Shopping';
                                 's1b_7_' = 'Community interaction';
                                 's1b_8_' = 'Accessing Settings';
                                 's1c_1_' = 'Learning interaction';
                                 's1c_2_' = 'Learning decisions';
                                 's1c_3_' = 'Problem solving';
                                 's1c_4_' = 'Using technology';
                                 's1c_5_' = 'Accessing training';
                                 's1c_6_' = 'Academics';
                                 's1c_7_' = 'Learning health skills';
                                 's1c_8_' = 'Learning self-determination';
                                 's1c_9_' = 'Learning self-management';
                                 's1d_1_' = 'Job accomodations';
                                 's1d_2_' = 'Specific job skills';
                                 's1d_3_' = 'Co-worker interaction';
                                 's1d_4_' = 'Supervisor interaction';
                                 's1d_5_' = 'Work speed';
                                 's1d_6_' = 'Work quality';
                                 's1d_7_' = 'Changing assignments';
                                 's1d_8_' = 'Seeking assistance';
                                 's1e_1_' = 'Taking medications';
                                 's1e_2_' = 'Avoiding hazards';
                                 's1e_3_' = 'Obtaining health care';
                                 's1e_4_' = 'Moving about';
                                 's1e_5_' = 'Accessing emergency svs';
                                 's1e_6_' = 'Nutritional diet';
                                 's1e_7_' = 'Physical fitness';
                                 's1e_8_' = 'Emotional well-being';
                                 's1f_1_' = 'Socializing in home';
                                 's1f_2_' = 'Recreation with others';
                                 's1f_3_' = 'Socializing out of home';
                                 's1f_4_' = 'Making friends';
                                 's1f_5_' = 'Communicating with helpers';
                                 's1f_6_' = 'Appropriate social skills';
                                 's1f_7_' = 'Intimate relationships';
                                 's1f_8_' = 'Volunteer work';
                                 's2_1_' = 'Self-advocacy';
                                 's2_2_' = 'Money management';
                                 's2_3_' = 'Exploited by others';
                                 's2_4_' = 'Legal responsibility';
                                 's2_5_' = 'Participation';
                                 's2_6_' = 'Legal services';
                                 's2_7_' = 'Decision making';
                                 's2_8_' = 'Other advocacy';
                                 's3a_1_' = 'Oxygen therapy';
                                 's3a_2_' = 'Postural drainage';
                                 's3a_3_' = 'Chest PT';
                                 's3a_4_' = 'Suctioning';
                                 's3a_5_' = 'Oral stimulation';
                                 's3a_6_' = 'Tube feeding';
                                 's3a_7_' = 'Parental feeding';
                                 's3a_8_' = 'Positioning';
                                 's3a_9_' = 'Dressing wounds';
                                 's3a_10_' = 'Prevent infection';
                                 's3a_11_' = 'Seizure mgmt';
                                 's3a_12_' = 'Dialysis';
                                 's3a_13_' = 'Ostomy care';
                                 's3a_14_' = 'Transfers';
                                 's3a_15_' = 'Therapy svs';
                                 's3a_16_' = 'Other medical';
                                 's3b_1_' = 'Assault';
                                 's3b_2_' = 'Property destruction';
                                 's3b_3_' = 'Stealing';
                                 's3b_4_' = 'Self injury';
                                 's3b_5_' = 'Pica';
                                 's3b_6_' = 'Suicide attempts';
                                 's3b_7_'  = 'Sexual aggression';
                                 's3b_8_' = 'Inappropriate';
                                 's3b_9_' = 'Outbursts';
                                 's3b_10_' = 'Wandering';
                                 's3b_11_' = 'Substance abuse';
                                 's3b_12_' = 'Mental health tx';
                                 's3b_13_' = 'Other behavioral'"),
         qol = car::recode(item,
                           "'s1a_1_' = 'Physical Wellbeing';
                           's1a_2_' = 'Physical Wellbeing';
                           's1a_3_' = 'Physical Wellbeing';
                           's1a_4_' = 'Physical Wellbeing';
                           's1a_5_' = 'Physical Wellbeing';
                           's1a_6_' = 'Physical Wellbeing';
                           's1a_7_' = 'Physical Wellbeing';
                           's1a_8_' = 'Physical Wellbeing';
                           's1b_1_' = 'Social Inclusion';
                           's1b_2_' = 'Social Inclusion';
                           's1b_3_' = 'Social Inclusion';
                           's1b_4_' = 'Interpersonal Relations';
                           's1b_5_' = 'Social Inclusion';
                           's1b_6_' = 'Social Inclusion';
                           's1b_7_' = 'Social Inclusion';
                           's1b_8_' = 'Social Inclusion';
                           's1c_1_' = 'Social Inclusion';
                           's1c_2_' = 'Personal Development';
                           's1c_3_' = 'Personal Development';
                           's1c_4_' = 'Personal Development';
                           's1c_5_' = 'Personal Development';
                           's1c_6_' = 'Personal Development';
                           's1c_7_' = 'Physical Wellbeing';
                           's1c_8_' = 'Self Determination';
                           's1c_9_' = 'Personal Development';
                           's1d_1_' = 'Material Wellbeing';
                           's1d_2_' = 'Material Wellbeing';
                           's1d_3_' = 'Material Wellbeing';
                           's1d_4_' = 'Material Wellbeing';
                           's1d_5_' = 'Material Wellbeing';
                           's1d_6_' = 'Material Wellbeing';
                           's1d_7_' = 'Material Wellbeing';
                           's1d_8_' = 'Material Wellbeing';
                           's1e_1_' = 'Physical Wellbeing';
                           's1e_2_' = 'Physical Wellbeing';
                           's1e_3_' = 'Physical Wellbeing';
                           's1e_4_' = 'Physical Wellbeing';
                           's1e_5_' = 'Physical Wellbeing';
                           's1e_6_' = 'Personal Development';
                           's1e_7_' = 'Personal Development';
                           's1e_8_' = 'Emotional Wellbeing';
                           's1f_1_' = 'Interpersonal Relations';
                           's1f_2_' = 'Interpersonal Relations';
                           's1f_3_' = 'Interpersonal Relations';
                           's1f_4_' = 'Interpersonal Relations';
                           's1f_5_' = 'Self Determination';
                           's1f_6_' = 'Interpersonal Relations';
                           's1f_7_' = 'Interpersonal Relations';
                           's1f_8_' = 'Social Inclusion';
                           's2_1_' = 'Rights';
                           's2_2_' = 'Self Determination';
                           's2_3_' = 'Rights';
                           's2_4_' = 'Rights';
                           's2_5_' = 'Social Inclusion';
                           's2_6_' = 'Rights';
                           's2_7_' = 'Self Determination';
                           's2_8_' = 'Rights';
                           's3a_1_' = 'Physical Wellbeing';
                           's3a_2_' = 'Physical Wellbeing';
                           's3a_3_' = 'Physical Wellbeing';
                           's3a_4_' = 'Physical Wellbeing';
                           's3a_5_' = 'Physical Wellbeing';
                           's3a_6_' = 'Physical Wellbeing';
                           's3a_7_' = 'Physical Wellbeing';
                           's3a_8_' = 'Physical Wellbeing';
                           's3a_9_' = 'Physical Wellbeing';
                           's3a_10_' = 'Physical Wellbeing';
                           's3a_11_' = 'Physical Wellbeing';
                           's3a_12_' = 'Physical Wellbeing';
                           's3a_13_' = 'Physical Wellbeing';
                           's3a_14_' = 'Physical Wellbeing';
                           's3a_15_' = 'Physical Wellbeing';
                           's3a_16_' = 'Physical Wellbeing';
                           's3b_1_' = 'Emotional Wellbeing';
                           's3b_2_' = 'Emotional Wellbeing';
                           's3b_3_' = 'Emotional Wellbeing';
                           's3b_4_' = 'Emotional Wellbeing';
                           's3b_5_' = 'Emotional Wellbeing';
                           's3b_6_' = 'Emotional Wellbeing';
                           's3b_7_'  = 'Emotional Wellbeing';
                           's3b_8_' = 'Emotional Wellbeing';
                           's3b_9_' = 'Emotional Wellbeing';
                           's3b_10_' = 'Emotional Wellbeing';
                           's3b_11_' = 'Emotional Wellbeing';
                           's3b_12_' = 'Emotional Wellbeing';
                           's3b_13_' = 'Emotional Wellbeing'")
         ) %>%
  filter(is.na(item) == F) %>%
  group_by(section, section_desc, item, item_desc, qol) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>% # Remove notes and other extraneous fields
  # Flag necessary services (i.e. high risk)
  mutate(need_svc = item %in% c("s1a_1_","s1a_2_","s1a_3_","s1a_4_",
                                "s1a_5_","s1a_6_","s1a_7_","s1a_8_",
                                "s1b_1_","s1b_6_","s1b_7_","s1b_8_",
                                "s1e_1_","s1e_2_","s1e_3_","s1e_4_",
                                "s1e_5_","s3a_1_","s3a_10_","s3a_11_",
                                "s3a_12_","s3a_13_","s3a_14_","s3a_15_",
                                "s3a_16_","s3a_2_","s3a_3_","s3a_4_",
                                "s3a_5_","s3a_6_","s3a_7_","s3a_8_",
                                "s3a_9_","s3b_1_","s3b_10_","s3b_11_",
                                "s3b_12_","s3b_13_","s3b_2_","s3b_4_",
                                "s3b_6_","s3b_7_","s3b_8_","s3b_9_"),
         refer_ot = item %in% c("s1a_1_","s1a_2_","s1a_3_","s1a_4_",
                                "s1a_5_","s1a_6_","s1a_7_","s1b_2_",
                                "s1e_7_"),
         refer_nurs = item %in% c("s1e_1_","s3a_1_","s3a_10_","s3a_11_",
                                  "s3a_12_","s3a_13_","s3a_16_","s3a_2_",
                                  "s3a_3_","s3a_4_","s3a_9_"),
         refer_sp = item %in% c("s3a_5_","s3a_6_","s3a_7_"),
         refer_pt = item %in% c("s3a_2_","s3a_3_","s1e_7_","s1b_1_",
                                "s1b_8_","s1e_4_","s3a_14_","s3a_8_"),
         refer_diet = item %in% c("s1a_3_","s1a_4_","s1e_6_")
  ) 

# Write to file
write.csv(needs,"data/needs.csv", row.names = F)

## GET SERVICE CODES ##
##############################################################################
# Get standard cost data from 404 report
  open404 <- RCurl::getURL("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/Master.csv",
                    ssl.verifypeer = 0L, followlocation = 1L)
  writeLines(open404,'temp.csv')
  open404 <- read.csv('temp.csv')

# Get codes provided for DD population in most recent FY

  svs <-
  open404 %>%
    filter(Population == "DD" & FY == max(FY)) %>%
    filter(ServiceType != "Screening & Assessment") %>% # filter out scrn / assess
    filter(ServiceType != "Other") %>%
    group_by(Service, Description, Code, Code_Mod) %>%
    summarize(n = n(),
              pts = sum(SumOfCases, na.rm = T)) %>%
    filter(pts >= 50) %>%
    droplevels()

## COMBINE IN MATRIX ##
##############################################################################  
# combine Needs and Services in matrix

for (i in as.list(levels(as.factor(needs$item)))) {
    svs[i] <- NA
}
  
needs_matrix <- svs
rm(svs); rm(open404)

## MATCH NEEDS TO SERVICES ##
##############################################################################

library(magrittr)

# Match each need item to all possible services that could be used to address need

needs_matrix %<>%
  mutate(s1a_1_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020"), # Toilet
         s1a_2_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020"), # Clothes
         s1a_3_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020"), # Preparing food
         s1a_4_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020",
                              "92610","92507","92526"), # Eating food
         s1a_5_ = Code %in% c("H2015","H2016",
                              "T1020",
                              "S5165","E1399","T2028","T2029"), # Housekeeping
         s1a_6_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020"), # Dressing
         s1a_7_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020"), # Hygiene
         s1a_8_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020",
                              "H0043"), # Appliances
         s1b_1_ = Code %in% c("H2015","H2016","H0043",
                              "T1020",
                              "T5999"), # Getting Around
         s1b_2_ = Code %in% c("H2015","H2016","H0043"), # Recreation
         s1b_3_ = Code %in% c("H2015","H2016","H0043"), # Public Services
         s1b_4_ = Code %in% c("H2015","H2016","H0043"), # Visit Friends/Family
         s1b_5_ = Code %in% c("H2015","H2016","H0043"), # Preferred Activities
         s1b_6_ = Code %in% c("H2015","H2016","H0043"), # Shopping
         s1b_7_ = Code %in% c("H2015","H2016","H0043"), # Community interaction
         s1b_8_ = Code %in% c("H2015","H2016","H0043"), # Accessing Settings
         s1c_1_ = Code %in% c("H2015","H2016","H0043",
                              "H2019","S5108","S5111"), # Learning interaction
         s1c_2_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "T2025"), # Learning decisions
         s1c_3_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017"), # Problem solving
         s1c_4_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017"), # Using technology
         s1c_5_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017"), # Accessing training
         s1c_6_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017"), # Academics
         s1c_7_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "S9445","S9446"), # Learning health skills
         s1c_8_ = Code %in% c("T1016","T1017",
                              "T2025"), # Learning self-determination
         s1c_9_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017"), # Learning self-management
         s1d_1_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Job accomodations
         s1d_2_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Specific job skills
         s1d_3_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Co-worker interaction
         s1d_4_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Supervisor interaction
         s1d_5_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Work speed
         s1d_6_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Work quality
         s1d_7_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Changing assignments
         s1d_8_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Seeking assistance
         s1e_1_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "T1999",
                              "99506","99211","96372"), # Taking medications
         s1e_2_ = Code %in% c("H2000","S5160","S5161"), # Avoiding hazards
         s1e_3_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "S9445","S9446"), # Obtaining health care
         s1e_4_ = Code %in% c("T2028","T2029","S5199","E1399","T2039",
                              "97110","97112","97113","97116","97124",
                              "97140","97530","97532","97533","97535", 
                              "97537","97542","S8990","97750","97755",
                              "97760","97762","97150","97001","97002",
                              "97003","97004"), # Moving about
         s1e_5_ = Code %in% c("S5160","S5161"), # Accessing emergency svs
         s1e_6_ = Code %in% c("97802","97803","97804",
                              "S9470"), # Nutritional diet
         s1e_7_ = Code %in% c("97110","97112","97113","97116","97124",
                              "97140","97530","97532","97533","97535", 
                              "97537","97542","S8990","97750","97755",
                              "97760","97762","97150","97001","97002",
                              "97003","97004",
                              "T5999"), # Physical fitness
         s1e_8_ = Code %in% c("T1016","T1017",
                              "90837","90832","90834","90833","90836","90838"), # Emotional well-being
         s1f_1_ = Code %in% c("H2015","H2016","H0043",
                              "H2019","S5108","S5111",
                              "H2030",
                              "H0023","H0038","H0046",
                              "H2014"), # Socializing in home
         s1f_2_ = Code %in% c("H2015","H2016","H0043",
                              "H2019","S5108","S5111",
                              "H2030",
                              "H0023","H0038","H0046",
                              "H2014"), # Recreation with others
         s1f_3_ = Code %in% c("H2015","H2016","H0043",
                              "H2019","S5108","S5111",
                              "H2030",
                              "H0023","H0038","H0046",
                              "H2014"), # Socializing out of home
         s1f_4_ = Code %in% c(""), # Making friends
         s1f_5_ = Code %in% c("T2039"), # Communicating with helpers
         s1f_6_ = Code %in% c("H2015","H2016","H0043"), # Appropriate social skills
         s1f_7_ = Code %in% c(""), # Intimate relationships
         s1f_8_ = Code %in% c("H2015","H2016","H0043",
                              "H2014","T2015"), # Volunteer work
         s2_1_ = Code %in% c("T1016","T1017",
                             "H0023","H0038","H0046"), # Self-advocacy
         s2_2_ = Code %in% c("H2015","H2016","H0043",
                             "T1016","T1017",
                             "T2025"), # Money management
         s2_3_ = Code %in% c("T1016","T1017"), # Exploited by others
         s2_4_ = Code %in% c("T1016","T1017"), # Legal responsibility
         s2_5_ = Code %in% c("H2015","H2016","H0043",
                             "T1016","T1017"), # Participation
         s2_6_ = Code %in% c("T1016","T1017"), # Legal services
         s2_7_ = Code %in% c("H2015","H2016","H0043",
                             "T1016","T1017",
                             "T2025"), # Decision making
         s2_8_ = Code %in% c("H2015","H2016","H0043",
                             "T1016","T1017"), # Other advocacy
         s3a_1_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Oxygen therapy
         s3a_10_ = Code %in% c("H2015","H2016","H0043",
                               "T1001","T1002","H0034",
                               "T1020"), # Prevent infection
         s3a_11_ = Code %in% c("H2015","H2016","H0043",
                               "T1001","T1002","H0034",
                               "S9123","S9124","T1000",
                               "T1020"), # Seizure mgmt
         s3a_12_ = Code %in% c("T1001","T1002","H0034",
                               "S9123","S9124","T1000"), # Dialysis
         s3a_13_ = Code %in% c("H2015","H2016","H0043",
                               "T1001","T1002","H0034",
                               "S9123","S9124","T1000",
                               "T1020"), # Ostomy care
         s3a_14_ = Code %in% c("H2015","H2016","H0043",
                               "T1001","T1002","H0034",
                               "S9123","S9124","T1000",
                               "T1020"), # Transfers
         s3a_15_ = Code %in% c("H2015","H2016","H0043",
                               "T1001","T1002","H0034",
                               "S9123","S9124","T1000",
                               "T1020"), # Therapy svs
         s3a_16_ = Code %in% c(""), # Other medical
         s3a_2_ = Code %in% c("T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Postural drainage
         s3a_3_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Chest PT
         s3a_4_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Suctioning
         s3a_5_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Oral stimulation
         s3a_6_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Tube feeding
         s3a_7_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Parental feeding
         s3a_8_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Positioning
         s3a_9_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Dressing wounds
         s3b_1_ = Code %in% c("90837","90832","90834","90833","90836","90838",
                              "H2019","S5108","S5111",
                              "H2000",
                              "T1020"), # Assault
         s3b_10_ = Code %in% c("H2019","S5108","S5111"), # Wandering
         s3b_11_ = Code %in% c("T1016","T1017"), # Substance abuse
         s3b_12_ = Code %in% c("T1016","T1017"), # Mental health tx
         s3b_13_ = Code %in% c(""), # Other behavioral
         s3b_2_ = Code %in% c("H2019","S5108","S5111"), # Property destruction
         s3b_3_ = Code %in% c(""), # Stealing
         s3b_4_ = Code %in% c("H2019","S5108","S5111"), # Self injury
         s3b_5_ = Code %in% c(""), # Pica
         s3b_6_ = Code %in% c(""), # Suicide attempts
         s3b_7_ = Code %in% c("H2019","S5108","S5111"), # Sexual aggression
         s3b_8_ = Code %in% c("H2019","S5108","S5111"), # Inappropriate
         s3b_9_ = Code %in% c("") # Outbursts
         ) %>%
  ungroup() %>%
  select(Code, s1a_1_:s3b_9_) %>%
  distinct()

write.csv(needs_matrix, "data/needs_matrix.csv", row.names = F)


# tst
# # first remember the names
# n <- needs_matrix$Code
# 
# # transpose all but the first column (name)
# tst <- as.data.frame(t(needs_matrix[,-1]))
# colnames(tst) <- n
# tst$item <- row.names(tst)
# sis_desc <- needs %>% ungroup %>% select(item, item_desc)
# tst <-
# tst %>% 
#   left_join(sis_desc, by = "item") %>%
#   select(item, item_desc, H2019:H2023)
# 
# 
# tst %>%
#   mutate_each(funs(as.numeric)) %>%
#   d3heatmap(colors = "Blues", 
#             theme = "")         
         
  
