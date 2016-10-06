## readServices.R 
## Read Service Code information ##

library(dplyr); library(magrittr); library(readxl); library(curl)

# Download option "Current LCDs" from CMS site:
# "https://www.cms.gov/medicare-coverage-database/downloads/downloadable-databases.aspx"
  
  hcpcs <- read.csv("data/hcpc_code_lookup.csv")
  hcpcs %<>%
    select(HCPCS = hcpc_code_id, 
           short_desc = short_description, 
           long_desc = long_description)
  
# Section 404 Sub-Element Cost Report  
# Get standard MDHHS behavioral health service use & cost data from 404 report
  
  open404 <- read.csv("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/Master.csv")

# Summarize avg and med unit cost from 404 data

filt404 <-
open404 %>%
  filter(FY == max(FY)) %>% # Keep most recent year
  rename(HCPCS = Code) %>%
  group_by(HCPCS) %>%
  # Summarize cost by HCPCS code (without modifiers)
  summarize(cost = sum(SumOfCost, na.rm = T),
            units = sum(SumOfUnits, na.rm = T),
            unit_hrs = max(Unit_Hours, na.rm = T),
            med_unitcost = median(CostPerUnit, na.rm = T),
            q1_unitcost = quantile(CostPerUnit, .25, na.rm = T),
            mad_unitcost = mad(CostPerUnit, na.rm = T )) %>%
  mutate(avg_unitcost = round(cost / units, digits =2)) %>%
  select(HCPCS, unit_hrs, avg_unitcost, med_unitcost, 
         q1_unitcost, mad_unitcost)

# Join RVU and 404 unit values to "codemap" table
codemap <-
  filt404 %>%
  left_join(hcpcs, by = "HCPCS")  %>%
  # Only distinct codes to avoid duplication of svs on join
  distinct(HCPCS, .keep_all = TRUE) %>%
  select(HCPCS,short_desc,long_desc,ServiceType,Service,unit_hrs,avg_unitcost,med_unitcost)

write.csv(codemap, "data/codemap.csv")


# #### NOT CURRENTLY RUN  ####
# # Potential reference: CMS Physician Fee Schedule Data
# 
# # Get standard cost data from CMS PPS
# # Data source available at CMS site: 
# # "https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/PhysicianFeeSched/PFS-Relative-Value-Files.html"
# 
# rvu <- read.csv("data/PPRRVU16_V0804.csv", skip = 9)
# gcpi <- read.csv("data/CY2016_GPCIs.csv")
# 
# # RVU
# # The geographic practice cost index (GPCIs) are applied in the 
# # calculation of a fee schedule payment amount by multiplying the 
# # RVU for each component times the GPCI for that component.
# 
# # Facility practice expense RVUs will be used for services performed 
# # in inpatient or outpatient hospital settings, emergency rooms, skilled 
# # nursing facilities, or ambulatory surgical centers (ASCs). The 
# # non-facility practice expense relative value units will be used for 
# # services furnished in all other settings
# 
# # Non-Facility Pricing Amount = 
# # [(Work RVU * Work GPCI) + (Non-Facility PE RVU * PE GPCI) + 
# # (MP RVU * MP GPCI)] * Conversion Factor (CF)
# 
# # Filter GCPI for geo region and clean names
# gcpi <-
#   gcpi %>%
#   filter(Locality.name == "Rest of Michigan") %>%
#   rename(work_gpci = Work......GPCI,
#          pe_gpci = PE......GPCI,
#          mp_gpci = MP.....GPCI) %>% droplevels
# 
# # Add GCPI values to RVU table
# rvu$work_gpci <- gcpi$work_gpci
# rvu$pe_gpci <- gcpi$pe_gpci
# rvu$mp_gpci <- gcpi$mp_gpci
# rm(gcpi)
# 
# # Rename RVU table variables
# rvu <-
#   rvu %>%
#   rename(work_rvu = RVU,
#          pe_rvu_nf = PE.RVU,
#          pe_rvu_f = PE.RVU.1,
#          mp_rvu = RVU.1,
#          cf = FACTOR) %>%
#   select(HCPCS, MOD, DESCRIPTION, 
#          work_rvu, work_gpci,
#          pe_rvu_nf, pe_rvu_f, pe_gpci,
#          mp_rvu, mp_gpci, cf)
# 
# # Facility practice expense RVUs will be used for services performed 
# # in inpatient or outpatient hospital settings, emergency rooms, skilled 
# # nursing facilities, or ambulatory surgical centers (ASCs). The 
# # non-facility practice expense relative value units will be used for 
# # services furnished in all other settings
# 
# # Non-Facility Pricing Amount = 
# # [(Work RVU * Work GPCI) + (Non-Facility PE RVU * PE GPCI) + 
# # (MP RVU * MP GPCI)] * Conversion Factor (CF)
# 
# rvu <-
#   rvu %>%
#   mutate(total_rvu = ((work_rvu * work_gpci) + 
#                         (pe_rvu_nf * pe_gpci) +
#                         (mp_rvu * mp_gpci)) * cf)
# 
# sub_rvu <- 
#   rvu %>% 
#   semi_join(codemap, by = "HCPCS") %>%
#   select(HCPCS, total_rvu) %>%
#   filter(total_rvu > 0)
