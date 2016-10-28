## readSIS.R ##

  library(dplyr);library(car);library(magrittr);library(tidyr)
  library(lubridate); library(stringr)

# What does the data look like?
  ncol(sis_full)
  nrow(sis_full)
  nlevels(as.factor(sis_full$sis_id))
  nlevels(sis_full$user_id)
  
# Subset and clean
  
  sis <- 
    sis_full %>% 
    # Filter Status == Completed
    filter(statusText %in% c("COMPLETED")
           & deleted == "False") %>% 
    # Remove text fields
    select(-ends_with("notes")) %>% 
    # Format datetime fields
    mutate(# Remove hms from sis_completed_dt and convert it
      sis_completed_dt = mdy(gsub(" .*$","",as.character(sis_completed_dt))),
      # Add space between time and 'AM/PM' to allow conversion
      sis_startTime = gsub('([0-9])([[:alpha:]])', '\\1 \\2', sis_startTime),
      sis_endTime = gsub('([0-9])([[:alpha:]])', '\\1 \\2', sis_endTime),
      # Combine date and time to create POSIX object
      sis_startTime = paste(sis_completed_dt,as.character(sis_startTime),sep = " "),
      sis_endTime = paste(sis_completed_dt,as.character(sis_endTime),sep = " "),
      # Deal with date formatting across SIS-A update
      start = if_else(sis_completed_dt >= "2016-10-01",
                      true = ymd_hm(sis_startTime),
                      false = ymd_hms(sis_startTime)),
      end = if_else(sis_completed_dt >= "2016-10-01",
                    true = ymd_hm(sis_endTime),
                    false = ymd_hms(sis_endTime)),
      # Truncated arg deals with diff formating of SIS-A field
      sis_cl_dob_dt = mdy_hms(sis_cl_dob_dt, truncated = 3),
      statusChangeDate = mdy_hms(statusChangeDate),
      dateUpdated = mdy_hms(dateUpdated),
      isp_begin_date = mdy(isp_begin_date),
      # Calculated fields using datetime vars
      duration = as.numeric(difftime(end, start, units = "mins")),
      DaysSince = as.POSIXct(today()) - as.POSIXct(sis_completed_dt),
      age = floor((as.POSIXct(sis_completed_dt) - sis_cl_dob_dt)/365.242),
      # Create week and annual dates for grouping
      sis_wk = week(sis_completed_dt),
      sis_yr = year(sis_completed_dt),
      sis_yrwk = floor_date(sis_completed_dt, unit = "week")
    ) %>%
    # Clean Medicaid ID field
    mutate(
      mcaid_id = sis_track_num, # map correct field
      # Trim lead / trail whitespace
      mcaid_id = str_trim(mcaid_id),
      # Remove alpha and special chars
      mcaid_id = str_replace_all(mcaid_id, "[[:alpha:]]", ""),
      mcaid_id = str_replace_all(mcaid_id, "[[:punct:]]", ""),
      # Convert blanks to NA
      mcaid_id = ifelse(mcaid_id == "", yes = NA, no = mcaid_id), 
      # If string > 10 chars, include only last 10 chars
      mcaid_id = ifelse(nchar(as.character(mcaid_id)) > 10,
                        yes = substr(mcaid_id, 
                                     start = nchar(as.character(mcaid_id)) - 9, 
                                     stop = nchar(as.character(mcaid_id))),
                        no = mcaid_id),
      # If string < 10 chars, pad with leading zeroes
      mcaid_id = ifelse(nchar(as.character(mcaid_id)) < 10,
                        yes = sprintf("%010d", as.integer(mcaid_id)),
                        no = mcaid_id),
      # Make 'NA' & 0000000000 to NA
      mcaid_id = ifelse(mcaid_id %in% c("        NA","NA","0000000000"), 
                        yes = NA,
                        no = mcaid_id),
      # Convert to factor
      mcaid_id = as.factor(mcaid_id)
    ) %>%
    mutate(
      # Combine address fields for geomapping
      address = paste0(sis_cl_addr_line1, ", ", sis_cl_st, ", ", sis_cl_zip)
    ) %>%
    # mutate(
    #   # Make Living Situation Groupings
    #   # First, we've got to remove the "â€“" characters
    #   LivingType <- gsub("[^a-zA-Z0-9]","", LivingSituation),
    #   LivingType <- recode(LivingType,
    #                        "'AdultFosterCarehomecertified' = 'Facility';
    #                        'Agencyprovidedresidentialhomewith4to6people' = 'Facility';
    #                        'Agencyprovidedresidentialhomewith10ormorepeople' = 'Facility';
    #                        'Fosterfamilyhome' = 'Family';
    #                        'GeneralresidentialAFCNOTcertified' = 'Facility';
    #                        'Homeless' = 'Independent';
    #                        'Institutionalsetting' = 'Facility';
    #                        'Livingindependentlywithsupports' = 'Independent';
    #                        'Livingwithfamily' = 'Family';
    #                        'NursingCareFacility' = 'Facility';
    #                        'Prisonjailjuveniledetentioncenter' = 'Facility';
    #                        'Privateresidencealoneorwithspouseornonrelatives' = 'Independent';
    #                        'PrivateresidenceownedbythePIHPCMHSPorprovider' = 'Independent';
    #                        'PrivateresidenceownedbyPIHPorProvider' = 'Independent';
    #                        'Privateresidencewithfamily' = 'Family';
    #                        'Privateresidencewithfamilymembers' = 'Family';
    #                        'SpecializedresidentialAFC' = 'Facility';
    #                        '' = NA"),
    #   LivingType <- as.factor(LivingType)
    # ) %>%
    rename(
      sis_id = `ï..formResultId`,
      interviewer_orig = assignedLoginId,
      interviewer = lastModifiedByLoginId,
      agency = groupName,
      PIHP = enterpriseName,
      gender = sis_cl_sex_cd,
      race = sis_race,
      ethnic = sis_ethnic,
      sis_date = sis_completed_dt,
      state = sis_cl_st
    )
  
# Make subset for analysis
  
  sub_sis <-
  sis %>%
    select(
      # Identifiers 
      sis_id, mcaid_id,
      # Assessment info
      interviewer, interviewer_orig, agency, PIHP, sis_why, contains("reln"),
      # Assessment date fields
      sis_date, sis_wk, sis_yr, sis_yrwk, DaysSince, start, end, duration,
      dateUpdated, statusChangeDate, 
      # Demographics
      age, gender, race, ethnic, address, state, # LivingSituation, LivingType,
      # Assessment items
      Q1A1_ExMedSupport:Q1A21_Other,
      Q1B1_ExBehSupport:Q1B15_Other,
      Q2A1_TOS:Q2F8_ImportantFor,
      Q3A1_TOS:Q3A8_ImportantFor,
      Q4A1v1:sis_s44n,
      # Disability status
      contains("disab"),
      # Employment status
      contains("employ"),
      # Planning items
      isp_begin_date, starts_with("planning")
    ) %>% 
    # Default mark all interviewers as current unless in per-PIHP list
    mutate(current_int = TRUE) 

# Print names of unused fields to console
  paste0("The following fields were not included in the analysis dataset: ")
  setdiff(colnames(sis_full),colnames(sub_sis))
      
###########################################################################
  
# # Create subset of summary variables
#   sub_sis <-
#   sis %>%
#     select(sis_id, 
#            mcaid_id,
#            sis_cl_st,
#            ReasonCompleted, InterviewSetting,
#            IndividualParticipation,
#            LivingSituation,
#            sis_sup1_reln_typ_cd, sis_res1_reln_typ_cd,
#            homeliving_std = s1a_Score_Standard,
#            homeliving_pct = s1a_Score_Percent,
#            commliving_std = s1b_Score_Standard,
#            commliving_pct = s1b_Score_Percent,
#            hlthsafety_std = s1e_Score_Standard,
#            hlthsafety_pct = s1e_Score_Percent,
#            lifelearng_std = s1c_Score_Standard,
#            lifelearng_pct = s1c_Score_Percent,
#            employment_std = s1d_Score_Standard,
#            employment_pct = s1d_Score_Percent,
#            social_std = s1f_Score_Standard,
#            social_pct = s1f_Score_Percent,
#            SupportNeedsIndex,
#            TotalStandard,
#            TotalPercentile,
#            # And Section 1
#            s1a_1_fqy:s1f_8_for,s1f_Score_Raw,
#            # And Section 2
#            s2_1_fqy:s2_Score_Eight_Raw,
#            # And Section 3
#            s3a_1_support:s3b_Score_Total) %>%
#     mutate(ABE_std = homeliving_std + commliving_std + hlthsafety_std,
#            self_advoc = s2_1_fqy + s2_1_dst + s2_1_tos,
#            money_mgmt = s2_2_fqy + s2_2_dst + s2_2_tos,
#            no_exploit = s2_3_fqy + s2_3_dst + s2_3_tos,
#            legal_resp = s2_4_fqy + s2_4_dst + s2_4_tos,
#            participate = s2_5_fqy + s2_5_dst + s2_5_tos,
#            legal_srvs = s2_6_fqy + s2_6_dst + s2_6_tos,
#            decisions = s2_7_fqy + s2_7_dst + s2_7_tos,
#            other_advoc = s2_8_fqy + s2_8_dst + s2_8_tos) %>%
#     select(sis_id:TotalPercentile,ABE_std,
#            s1a_1_fqy:s1f_8_for,
#            s2_1_fqy:s2_Score_Eight_Raw,
#            self_advoc:other_advoc,
#            s3a_1_support:s3b_Score_Total) 
