## global.R ##

# Load fun, data, libs, source files
  library(shinydashboard)
  library(shinythemes)
  library(DT)
  library(ggplot2)
  library(rcdimple)
  library(dygraphs)
  library(parsetR)
  library(d3heatmap)
  library(dplyr)
  library(magrittr)
  library(tidyr)
  library(plotly)
  library(xts)
  library(lubridate)
  library(RColorBrewer)
  library(car)

# Define begin & due dates
  begin <- as.Date("2014/07/01")
  due <- as.Date("2017/6/30")

# Load de-identified data
  scrub_sis <- read.csv("data/scrub_sis.csv")

# Get most recent SIS score
  most_recent <- max(as.Date(scrub_sis$sis_date)[as.Date(scrub_sis$sis_date) <= Sys.Date()])
    
# Load totals
  totals <- read.csv("data/totals.csv")
  
# CREATE NEEDS ELEMENTS MAPPING TABLE##
  
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
                                   's3b_13_' = 'Other behavioral'")
           ) %>%
    filter(is.na(item) == F) %>%
    group_by(section, section_desc, item, item_desc) %>%
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
  
# Process heavy computations up front to allow for cleaner performance
  # Section 1
  s1_tos <-
    scrub_sis %>%
    filter(is.na(fake_id) == FALSE) %>% # Remove empty randomized IDs
    group_by(fake_id) %>% 
    filter(as.Date(sis_date) == max(as.Date(sis_date))) %>% # Most recent per ID
    ungroup() %>% droplevels() %>%
    select(fake_id,agency,sis_date,ends_with("tos")) %>% 
    select(fake_id,agency,sis_date,starts_with("s1")) %>%
    mutate_each(funs(as.character), -fake_id, -agency, -sis_date) %>%
    gather(item, tos, s1a_1_tos:s1f_8_tos) %>%
    rename(type = tos) %>%
    mutate(type_n = as.numeric(type),
           item = gsub("tos","",item),
           id = as.factor(paste0(fake_id,item))
    ) %>%
    group_by(id, fake_id, agency, sis_date, item, type) %>%
    summarize(n = n_distinct(id),
              type_n = sum(type_n)) %>%
    ungroup() 
  
  s1_fqy <-
    scrub_sis %>%
    filter(is.na(fake_id) == FALSE) %>% # Remove empty Medicaid IDs
    group_by(fake_id) %>% 
    filter(as.Date(sis_date) == max(as.Date(sis_date))) %>% # Most recent per ID
    ungroup() %>% droplevels() %>%
    select(fake_id, agency, sis_date,ends_with("fqy")) %>% 
    select(fake_id, agency, sis_date, starts_with("s1")) %>%
    mutate_each(funs(as.character), -fake_id, -agency, -sis_date) %>%
    gather(item, fqy, s1a_1_fqy:s1f_8_fqy) %>%
    rename(frequency = fqy) %>%
    mutate(frequency_n = as.numeric(frequency),
           item = gsub("fqy","",item),
           id = as.factor(paste0(fake_id,item))
    ) %>%
    group_by(id, fake_id, agency, sis_date, item, frequency) %>%
    summarize(frequency_n = sum(frequency_n)) %>%
    ungroup() 
  
  s1_dst <-
    scrub_sis %>%
    filter(is.na(fake_id) == FALSE) %>% # Remove empty Medicaid IDs
    group_by(fake_id) %>% 
    filter(as.Date(sis_date) == max(as.Date(sis_date))) %>% # Most recent per ID
    ungroup() %>% droplevels() %>%
    select(fake_id, agency, sis_date, ends_with("dst")) %>% 
    select(fake_id, agency, sis_date, starts_with("s1")) %>%
    mutate_each(funs(as.character), -fake_id, -agency, -sis_date) %>%
    gather(item, dst, s1a_1_dst:s1f_8_dst) %>%
    rename(DST = dst) %>%
    mutate(DST_n = as.numeric(DST),
           item = gsub("dst","",item),
           id = as.factor(paste0(fake_id,item))
    ) %>%
    group_by(id, fake_id, agency, sis_date, item, DST) %>%
    summarize(DST_n = sum(DST_n)) %>%
    ungroup() 
  
  s1_to <-
    scrub_sis %>%
    filter(is.na(fake_id) == FALSE) %>% # Remove empty randomized IDs
    group_by(fake_id) %>% 
    filter(as.Date(sis_date) == max(as.Date(sis_date))) %>% # Most recent per ID
    ungroup() %>% droplevels() %>%
    select(fake_id,agency,sis_date,ends_with("to")) %>% 
    select(fake_id,agency,sis_date,starts_with("s1")) %>%
    mutate_each(funs(as.character), -fake_id, -agency, -sis_date) %>%
    gather(item, import_to, s1a_1_to:s1f_8_to) %>%
    mutate(import_to_n = as.numeric(import_to),
           item = gsub("to","",item),
           id = as.factor(paste0(fake_id,item))
    ) %>%
    group_by(id, fake_id, agency, sis_date, item, import_to) %>%
    summarize(n = n_distinct(id),
              import_to_n = sum(import_to_n)) %>%
    ungroup()
  
  s1_for <-
    scrub_sis %>%
    filter(is.na(fake_id) == FALSE) %>% # Remove empty randomized IDs
    group_by(fake_id) %>% 
    filter(as.Date(sis_date) == max(as.Date(sis_date))) %>% # Most recent per ID
    ungroup() %>% droplevels() %>%
    select(fake_id,agency,sis_date,ends_with("for")) %>% 
    select(fake_id,agency,sis_date,starts_with("s1")) %>%
    mutate_each(funs(as.character), -fake_id, -agency, -sis_date) %>%
    gather(item, import_for, s1a_1_for:s1f_8_for) %>%
    mutate(import_for_n = as.numeric(import_for),
           item = gsub("for","",item),
           id = as.factor(paste0(fake_id,item))
    ) %>%
    group_by(id, fake_id, agency, sis_date, item, import_for) %>%
    summarize(n = n_distinct(id),
              import_for_n = sum(import_for_n)) %>%
    ungroup()
  
  # Join intermediate tables
  s1 <- 
  s1_tos %>% 
    inner_join(s1_fqy, by = "id") %>%
    select(id, fake_id = fake_id.x, agency = agency.x, 
           sis_date = sis_date.x, item = item.x, 
           type, type_n, frequency, frequency_n) %>%
    inner_join(s1_dst, by = "id") %>%
    select(id, fake_id = fake_id.x, agency = agency.x, 
           sis_date = sis_date.x, item = item.x, 
           type, type_n, frequency, frequency_n, DST, DST_n) %>%
    inner_join(s1_to, by = "id") %>%
    select(id, fake_id = fake_id.x, agency = agency.x, 
           sis_date = sis_date.x, item = item.x, 
           type, type_n, frequency, frequency_n, DST, DST_n,
           import_to_n) %>%
    inner_join(s1_for, by = "id") %>%
    select(id, fake_id = fake_id.x, agency = agency.x, 
           sis_date = sis_date.x, item = item.x, 
           type, type_n, frequency, frequency_n, DST, DST_n,
           import_to_n, import_for_n)
  
  # Remove intermediate tables
  rm(s1_dst); rm(s1_fqy); rm(s1_tos); rm(s1_to); rm(s1_for)
  
  # Remove extra cols and recode vars  
  s1 <-
    s1 %>%
    left_join(needs, by = "item") %>% # add item level desc and groups
    mutate(type = car::recode(type,
                            "'0' = 'None';
                            '1' = 'Monitoring';
                            '2' = 'Coaching';
                            '3' = 'Partial Physical Assistance';
                            '4' = 'Full Physical Support'"),
         frequency = car::recode(frequency,
                                 "'0' = 'Minimal';
                                 '1' = 'Monthly';
                                 '2' = 'Weekly';
                                 '3' = 'Daily';
                                 '4' = 'Hourly'"),
         DST = car::recode(DST,
                           "'0' = 'None';
                           '1' = 'Under 30 min';
                           '2' = 'Under 2 hrs';
                           '3' = '2-4 hrs';
                           '4' = 'Over 4 hrs'"),
         score = type_n + frequency_n + DST_n,
         import_to = as.logical(import_to_n),
         import_for = as.logical(import_for_n),
         importance = ifelse(import_to == T & import_for == T, "To and For",
                             ifelse(import_to == T, "To",
                                    ifelse(import_for == T, "For",
                                           "Not endorsed")))) %>%
    select(-import_to_n, -import_for_n, -n)
  
  # Process Section 2
  s2_tos <-
    scrub_sis %>%
    filter(is.na(fake_id) == FALSE) %>% # Remove empty Medicaid IDs
    group_by(fake_id) %>% 
    filter(as.Date(sis_date) == max(as.Date(sis_date))) %>% # Most recent per ID
    ungroup() %>% droplevels() %>%
    select(fake_id,agency,sis_date,ends_with("tos")) %>% 
    select(fake_id,agency,sis_date,starts_with("s2")) %>%
    mutate_each(funs(as.character), -fake_id, -agency, -sis_date) %>%
    gather(item, tos, s2_1_tos:s2_8_tos) %>%
    rename(type = tos) %>%
    mutate(type_n = as.numeric(type),
           item = gsub("tos","",item),
           id = as.factor(paste0(fake_id,item))
    ) %>%
    group_by(id, fake_id, agency, sis_date, item, type) %>%
    summarize(n = n_distinct(id),
              type_n = sum(type_n)) %>%
    ungroup() 
  
  
  s2_fqy <-
    scrub_sis %>%
    filter(is.na(fake_id) == FALSE) %>% # Remove empty Medicaid IDs
    group_by(fake_id) %>% 
    filter(as.Date(sis_date) == max(as.Date(sis_date))) %>% # Most recent per ID
    ungroup() %>% droplevels() %>%
    select(fake_id, agency, sis_date, ends_with("fqy")) %>% 
    select(fake_id, agency, sis_date, starts_with("s2")) %>%
    mutate_each(funs(as.character), -fake_id, -agency, -sis_date) %>%
    gather(item, fqy, s2_1_fqy:s2_8_fqy) %>%
    rename(frequency = fqy) %>%
    mutate(frequency_n = as.numeric(frequency),
           item = gsub("fqy","",item),
           id = as.factor(paste0(fake_id,item))
    ) %>%
    group_by(id, fake_id, agency, sis_date, item, frequency) %>%
    summarize(frequency_n = sum(frequency_n)) %>%
    ungroup() 
  
  s2_dst <-
    scrub_sis %>%
    filter(is.na(fake_id) == FALSE) %>% # Remove empty Medicaid IDs
    group_by(fake_id) %>% 
    filter(as.Date(sis_date) == max(as.Date(sis_date))) %>% # Most recent per ID
    ungroup() %>% droplevels() %>%
    select(fake_id, agency, sis_date, ends_with("dst")) %>% 
    select(fake_id, agency, sis_date, starts_with("s2")) %>%
    mutate_each(funs(as.character), -fake_id, -agency, -sis_date) %>%
    gather(item, dst, s2_1_dst:s2_8_dst) %>%
    rename(DST = dst) %>%
    mutate(DST_n = as.numeric(DST),
           item = gsub("dst","",item),
           id = as.factor(paste0(fake_id,item))
    ) %>%
    group_by(id, fake_id, agency, sis_date, item, DST) %>%
    summarize(DST_n = sum(DST_n)) %>%
    ungroup() 
  
  s2_to <-
    scrub_sis %>%
    filter(is.na(fake_id) == FALSE) %>% # Remove empty randomized IDs
    group_by(fake_id) %>% 
    filter(as.Date(sis_date) == max(as.Date(sis_date))) %>% # Most recent per ID
    ungroup() %>% droplevels() %>%
    select(fake_id,agency,sis_date,ends_with("to")) %>% 
    select(fake_id,agency,sis_date,starts_with("s2")) %>%
    mutate_each(funs(as.character), -fake_id, -agency, -sis_date) %>%
    gather(item, import_to, s2_1_to:s2_8_to) %>%
    mutate(import_to_n = as.numeric(import_to),
           item = gsub("to","",item),
           id = as.factor(paste0(fake_id,item))
    ) %>%
    group_by(id, fake_id, agency, sis_date, item, import_to) %>%
    summarize(n = n_distinct(id),
              import_to_n = sum(import_to_n)) %>%
    ungroup()
  
  s2_for <-
    scrub_sis %>%
    filter(is.na(fake_id) == FALSE) %>% # Remove empty randomized IDs
    group_by(fake_id) %>% 
    filter(as.Date(sis_date) == max(as.Date(sis_date))) %>% # Most recent per ID
    ungroup() %>% droplevels() %>%
    select(fake_id,agency,sis_date,ends_with("for")) %>% 
    select(fake_id,agency,sis_date,starts_with("s2")) %>%
    mutate_each(funs(as.character), -fake_id, -agency, -sis_date) %>%
    gather(item, import_for, s2_1_for:s2_8_for) %>%
    mutate(import_for_n = as.numeric(import_for),
           item = gsub("for","",item),
           id = as.factor(paste0(fake_id,item))
    ) %>%
    group_by(id, fake_id, agency, sis_date, item, import_for) %>%
    summarize(n = n_distinct(id),
              import_for_n = sum(import_for_n)) %>%
    ungroup()
  
  # Join intermediate tables
  s2 <- 
    s2_tos %>% 
    inner_join(s2_fqy, by = "id") %>%
    select(id, fake_id = fake_id.x, agency = agency.x, 
           sis_date = sis_date.x, item = item.x, 
           type, type_n, frequency, frequency_n) %>%
    inner_join(s2_dst, by = "id") %>%
    select(id, fake_id = fake_id.x, agency = agency.x, 
           sis_date = sis_date.x, item = item.x, 
           type, type_n, frequency, frequency_n, DST, DST_n) %>%
    inner_join(s2_to, by = "id") %>%
    select(id, fake_id = fake_id.x, agency = agency.x, 
           sis_date = sis_date.x, item = item.x, 
           type, type_n, frequency, frequency_n, DST, DST_n,
           import_to_n) %>%
    inner_join(s2_for, by = "id") %>%
    select(id, fake_id = fake_id.x, agency = agency.x, 
           sis_date = sis_date.x, item = item.x, 
           type, type_n, frequency, frequency_n, DST, DST_n,
           import_to_n, import_for_n)
  
  # Remove intermediate tables
  rm(s2_dst); rm(s2_fqy); rm(s2_tos); rm(s2_to); rm(s2_for)
  
  # Remove extra cols and recode vars  
  s2 <-
  s2 %>%
    left_join(needs, by = "item") %>% # add item level desc and groups
    mutate(type = car::recode(type,
                              "'0' = 'None';
                            '1' = 'Monitoring';
                            '2' = 'Coaching';
                            '3' = 'Partial Physical Assistance';
                            '4' = 'Full Physical Support'"),
           frequency = car::recode(frequency,
                                   "'0' = 'Minimal';
                                 '1' = 'Monthly';
                                 '2' = 'Weekly';
                                 '3' = 'Daily';
                                 '4' = 'Hourly'"),
           DST = car::recode(DST,
                             "'0' = 'None';
                           '1' = 'Under 30 min';
                           '2' = 'Under 2 hrs';
                           '3' = '2-4 hrs';
                           '4' = 'Over 4 hrs'"),
           score = type_n + frequency_n + DST_n,
           import_to = as.logical(import_to_n),
           import_for = as.logical(import_for_n),
           importance = ifelse(import_to == T & import_for == T, "To and For",
                               ifelse(import_to == T, "To",
                                      ifelse(import_for == T, "For",
                                             "Not endorsed")))) %>%
    select(-import_to_n, -import_for_n, -n)
  