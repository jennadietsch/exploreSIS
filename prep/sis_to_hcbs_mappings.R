
# HCBS to SIS Element Mapping

sis_hcbs <-
scrub_sis %>%
  mutate(ProviderOperated = LivingSituation %in% c("Adult Foster Care home - certified ",
                                                   "Agency provided residential home with 10 or more people",  
                                                   "Agency provided residential home with 4 to 6 people",      
                                                   "General residential AFC â€“ NOT certified",
                                                   "Institutional setting",
                                                   "Nursing Care Facility",
                                                   "Specialized residential AFC"),
         )


# The NCI looks at the extent to which a person performs certain social 
# or community-based activities (e.g. interacts with neighbors, shopping, 
# errands, religious practice, entertainment, exercise, vacations, meetings).
# The SIS asks "how much support would be needed to assist a person in 
# performing these activities?" but does not assume that the activities 
# either are or are not occurring.  A high need score on the 'Socializing out 
# of home' question may be given even when that a person is, indeed, performing 
# this activity, so it won't necessarily be indicative of a lack of access to 
# the community

# Whether a given item was important to the individual.
# Whether a given item was important for the individual, as communicated by informants
# Who has the highest level of need noted for the community-based activities identified by the HCBS Final Rule?
# Who has one or more medical or behavioral issues which make it challenging to access the community?


## New HCBS Setting Requirements 

# Requirement: Is integrated in and supports access to the greater community
  # The SIS data does identify those residential settings that are provider 
  # owned/operated–a category that entails meeting the additional HCBS 
  # setting requirements.

# Requirement: Ensures the individual receives services in the community 
# with the same degree of access as individuals not receiving Medicaid HCBS

# Requirement: Provides opportunities to seek employment and work in 
# competitive integrated settings, engage in community life, and control 
# personal resources
  # SIS identifies areas where assistance is needed for work and interest 
  # in work.  It does not specify the setting of work, though this could be 
  # determined by joining BH-TEDS
  # SIS identifies needs and interst in engaging in community life and 
  # controlling personal resources

# Requirement: Ensures the individual receives services in the community 
# with the same degree of access as individuals not receiving Medicaid HCBS
  # SIS identifies a subset of community services (healthcare, education, 
  # group involvement) and identifies the level of need required to participate 
  # with the same degree as someone not receiving Medicaid HCBS

# Requirement: Ensures right to privacy, dignity and respect and freedom 
# from coercion and restraint
  # SIS identifies individuals with challenging behaviors who may be more 
  # likely victims of restraint
  # SIS identifies a person's susceptibility to being exploited by others

###################################################################
## HCBS Person-centered Service Plan Process Requirements

# Requirement: Optimizes autonomy and independence in making life choices

# Requirement: Facilitates choice of services and who provides them
  # Looking at services received which map to the SIS items which were 
  # identified as important to the individual.

# Requirement: Includes people chosen by the individual
  # If SIS is viewed as part of the planning process, it would be possible 
  # to identify the number of informants involved in the meeting and their 
  # relation to the individual

# Requirement: Includes individually identified goals and preferences 
# related to relationships, community participation, employment, 
# income and savings, healthcare and wellness, education and others 
  # Looking at SIS items in these areas which were identified as important 
  # to the individual.  These could be mapped to QoL domains as suggested 
  # by val Loon, et al.

# Requirement:  Identifies the strengths, preferences, needs 
# (clinical and support), and desired outcomes of individual
  # SIS can be useful in identifying preferences ("Important To") and needs 
  # (both clinical and support).  Desired outcomes of the individual could 
  # be identified by mapping to QoL domains as suggested by val Loon, et al.

# Requirement:  Includes risk factors and plans to minimize them
  # The SIS identifies both high risk medical and behavioral factors, though 
  # it does not identify plans to minimize these.

# Requirement: Conducted to reflect what is important to the individual 
# to ensure delivery of services in a manner reflecting personal 
# preferences and ensuring health and welfare
  # This would require analysis of SIS data along with service provision data

###########################################################
## New HCBS Person-centered Service Plan Documentation Requirements

# Requirement:Setting is chosen by the individual and supports 
# full access to the community
  # Living situation variable can be used to identify settings presumed not to 
  # be in complinace.  Those that are presumed not to be HCBS will require a 
  # closer analysis of characteristics. 

# Requirement: There are opportunities to seek employment and work 
# in competitive integrated settings 

# Requirement: Supports are in place to assist the individual to 
# engage in community life, control personal resources, and receive 
# services in the community

# Requirement: Supports and services are linked to individual’s 
# strengths and preferences 
  # This would require analysis of SIS data along with service provision data.
  # Since the absence of a need is not the same thing as a strength, strengths 
  # would not be able to be assessed relative to services.

# Requirement: Supports and services align with assessed clinical 
# and support needs
  # This would require analysis of SIS data along with service provision data.

# Requirement: Individual’s goals and desired outcomes are included
  # Standard goal statements could be derived by mapping to QoL domains as 
  # suggested by val Loon, et al.  Without IPOS data, it would be unclear 
  # whether these were included

# Requirement: Any risk factors are identified and measures are in 
# place to minimize risk
  # SIS shows presence of identified risk factors, but not whether measures are 
  # in place to address them.

# Requirement: Plan has been reviewed and revised upon reassessment 
# of functional need as required every 12 months, when the individual’s 
# circumstances or needs change significantly, and/or at the request of 
# the individual.
  # SIS is required every 3 years.

#####################################################

# Relevant Supporting data

# Service (Claims) Data
# BH-TEDS
# Consistent formatting of Individual Plan of Service (IPOS) data