## global.R ##

# Load fun, data, libs, source files
  library(shinydashboard)
  library(shinythemes)
  library(DT)
  library(ggplot2)
  library(rcdimple)
  library(dygraphs)
  library(parsetR)
  library(visNetwork)
  library(d3heatmap)
  library(dplyr)
  library(broom)
  library(forcats)
  library(magrittr)
  library(tidyr)
  library(googlesheets)
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

# Load needs mapping table (created by "prep/sis_mappings.R" script)
  needs <- read.csv("data/needs.csv")
  
# Load service mapping table (created by "prep/sis_mappings.R" script)
  needs_matrix <- read.csv("data/needs_matrix.csv")
  
# Load HCPCS table (created by "prep/readServices.R" script)
  codemap <- read.csv("data/codemap.csv")
    
################################################################################ 
# DEFINE FUNCTION: svs2sis (Service codes to SIS needs)

# To get the SIS needs associated with a given HCPCS code
# Assumes existence of needs_matrix df to map needs to svs
# Can enter any list of HCPCS codes and fx will return related SIS needs
  
  svs2sis <- function(hcpcs){
    
    library(dplyr); library(tidyr); library(magrittr)
    
    needs_matrix %>%
      filter(Code %in% hcpcs) %>%
      gather(item,yn,-Code) %>%
      filter(yn == T) %>%
      distinct(item) %>% #de-dup
      as.list() %>%
      return()
    
  }
  
  # Example: 
  # res_svs <- svs2sis(c("T1020","H2016"))  
  
################################################################################  
# TRANSFORM SECTIONS TO BREAK DOWN TYPE OF SERVICE  
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
    select(-import_to_n, -import_for_n, -n) %>%
    droplevels()
  
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
           import_to_n, import_for_n) %>%
    droplevels()
  
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
  