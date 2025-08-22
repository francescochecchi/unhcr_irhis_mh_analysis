#...............................................................................
### +++++++ ANALYSIS OF UNHCR iRHIS MENTAL HEALTH CONSULTATION DATA ++++++++ ###
#...............................................................................

#...............................................................................
## --------- R SCRIPT TO READ AND PRE-PROCESS DATASETS FOR ANALYSIS  -------- ##
#...............................................................................



#...............................................................................
### Reading and cleaning demographic dataset
#...............................................................................

  #...................................      
  ## Read and clean
    
    # Read
    demog <- read.csv(paste0(dir_path, "in/demographics_residing_world.csv"))
    demog <- demog[-1, ]

    # Select and rename variables
    ages <- c("0to4", "5to11", "12to17", "18to59", "60+", "unknown", "total")
    colnames(demog) <- c("year", "country_origin_iso", "country_iso",
      "country_origin", "country", "pop_type", "site", "urban_rural",
      "accommodation", paste0("female_", ages), paste0("male_", ages), "total")
    
    # Isolate sites in 2024-2025
    sites_demog <- unique(demog[which(demog$year >= 2024), c("country","site")])
    
    
#...............................................................................
### Reading, appending and cleaning consultations by cause data
#...............................................................................

  #...................................      
  ## Read and append
  
    # Read and append
    mh <- read.csv(paste0(dir_path, "in/UNHCR_OPD_MH_morbidity_Q1-Q2_2024.csv"))
    x <- read.csv(paste0(dir_path, "in/UNHCR_OPD_MH_morbidity_Q3-Q4_2024.csv"))
    mh <- rbind(mh, x)
    x <- read.csv(paste0(dir_path, "in/UNHCR_OPD_MH_morbidity_Q1-Q2_2025.csv"))
    mh <- rbind(mh, x)

    # Rename columns
    colnames(mh) <- c("region", "country", "site", "mmyy", "cause", "pt_type",
      "n_cases", "age", "sex")

    # Inspect consistency of variable categories
    for (i in colnames(mh)) {print(table(mh[, i], useNA = "always"))}

  #...................................      
  ## Clean and reformat location variables
    
    # Tabulate country and site
    sites <- as.data.frame(table(mh[, c("country", "site")]))
    colnames(sites) <- c("country", "site", "n_obs")
    sites <- subset(sites, n_obs > 0)
    sites <- sites[order(sites$country, sites$site), ]  
    
    # Check that all also appear in the demographic dataset
    x <- sites_demog[grepl("admin3", sites_demog)]
    
  #...................................      
  ## Clean and reformat other variables
  
    # Clean month and year
    mh$mmyy <- lubridate::my(mh$mmyy)
    table(mh$mmyy)
    
    # Clean patient type
    mh$pt_type <- tolower(mh$pt_type)
        
    # Clean number of cases
        # exclude 1 obs with NA n_cases and 1 obs with 3768 n_cases 
        # (substance abuse - opiate in S Sudan, 60+yrs: implausible)
    mh <- subset(mh, ! is.na(n_cases) & ! (n_cases > 1000))

    # Clean age group (1 obs is ambiguous - >=5 yrs: impute as 18-59yrs)
    mh[which(mh$age == "< 5 yrs"), "age"] <- "0-4 yrs"
    mh[which(mh$age == ">= 5 yrs"), "age"] <- "18-59 yrs"
    mh$age <- gsub(" yrs", "", mh$age)
    mh$age <- gsub("Over ", "", mh$age)
    mh[which(mh$age == "60"), "age"] <- "60+"
    mh$age <- factor(mh$age, levels = c("0-4", "5-17", "18-59", "60+"),
      labels = c("0 to 4yrs", "5 to 17yrs", "18 to 59yrs", "60+ yrs"))
    table(mh$age)

    # Clean sex
    mh[which(mh$sex == "N/A"), "sex"] <- NA
    mh$sex <- tolower(mh$sex)    
    table(mh$sex, useNA = "always")
    table(mh[, c("sex", "pt_type")], useNA = "always")
      # all national observations have missing sex
      # remove 3 refugee observations with missing sex
      mh <- subset(mh, (pt_type == "refugee" & ! is.na(sex)) | 
          pt_type == "national")
    
      
      
#...............................................................................  
### ENDS
#...............................................................................
     
