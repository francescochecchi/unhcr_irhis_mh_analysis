#...............................................................................
### +++++++ ANALYSIS OF UNHCR iRHIS MENTAL HEALTH CONSULTATION DATA ++++++++ ###
#...............................................................................

#...............................................................................
## --------- R SCRIPT TO READ AND PRE-PROCESS DATASETS FOR ANALYSIS  -------- ##
#...............................................................................



#...............................................................................
### Reading and cleaning demographic datasets
#...............................................................................

  #...................................      
  ## Read and clean demographic dataset 
      # (https://data.humdata.org/dataset/unhcr-population-data-for-world)
    
    # Read
    demog <- read.csv(paste0(dir_path, "in/demographics_residing_world.csv"))
    demog <- demog[-1, ]

    # Rename variables
    ages <- c("0to4", "5to11", "12to17", "18to59", "60+", "unknown", "total")
    colnames(demog) <- c("year", "country_origin_iso", "country_iso",
      "country_origin", "country", "pop_type", "site", "urban_rural",
      "accommodation", paste0("female_", ages), paste0("male_", ages), "total")
    
    # Reformat
    x <- c("year", grep("male", colnames(demog), value = T), "total")
    for (i in x) {demog[, i] <- as.numeric(demog[, i])}
    
    # Streamline ages
    demog$female_5to17 <- demog$female_5to11 + demog$female_12to17
    demog$male_5to17 <- demog$male_5to11 + demog$male_12to17

  #...................................      
  ## Read and clean population 2024-2025 dataset provided by UNHCR

    # Read
    pop0405 <- read.csv(paste0(dir_path, 
      "in/UNHCR_Refugees_pop_Jan_2024-June_2025.csv"))
    
    # Rename columns
    colnames(pop0405) <- c("region", "country", "site", "pop", "year")
    
    
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
    
    # Correct country names
    mh[which(mh$country == "Tanzania"), "country"] <- 
      "United Republic of Tanzania"
    mh[which(mh$country == "Republic of the Congo"), "country"] <- 
      "Congo"
    mh[which(mh$country == "Democratic Republic of the Congo (DRC)"), 
      "country"] <- "Democratic Republic of the Congo"
    
    # Tabulate country and site
    sites <- as.data.frame(table(mh[, c("country", "site")]))
    colnames(sites) <- c("country", "site", "n_obs")
    sites <- subset(sites, n_obs > 0)
    sites <- sites[order(sites$country, sites$site), ]  
    
    # Check that all countries also appear in the demographic dataset
    x1 <- as.character(unique(sites$country))
    x2 <- unique(sites_demog$country)
    x1[! x1 %in% x2]
 
  #...................................      
  ## Link site names with site names in demographic dataset
    
    # Check that sites all also appear in the demographic dataset
    sites_demog <- unique(demog[which(demog$year >= 2024), c("country","site")])
    sites_demog <- subset(sites_demog, country %in% x1)
    x <- sapply(sites$site, grepl, sites_demog$site)
    nomatch <- colSums(x)
    x <- apply(x, 2, which)
    sites$site_demog <- lapply(x, function(xx) {sites_demog[xx, "site"]})
    sites$nomatch <- nomatch
    
    # Manually add/correct missing site correspondence
    x <- data.frame(
      site = sites[which(sites$nomatch != 1), "site"],
      site_demog = c(
        "Lóvua [Admin 3: POC]", "Camp 4 [Admin 3: POC]", NA, NA,
        "Nyankanda [Admin 3: POC]", "Borgop [Admin 3: POC]", 
        "Lolo [Admin 3: POC]", "Mbile [Admin 3: POC]", "Ngam [Admin 3: POC]",
        "Timangolo [Admin 3: POC]", "Aboutengué [Admin 3: POC]", 
        "Alacha [Admin 3: POC]", "Dar es Salam [Admin 3: POC]", 
        "Goz Amir [Admin 3: POC]", "Dilingala [Admin 3: POC]", 
        NA, "Bele [Admin 3: POC]", NA, "Meri [Admin 3: POC]", 
        "Mulongwe [Admin 3: POC]", NA, "Barahale [Admin 3: POC]", 
        "Helaweyn [Admin 3: POC]", "Kurmuk [Admin 3: POC]", NA, 
        "Ura [Admin 3: POC]", "Akre Camp [Admin 3: PRP]", 
        "Domiz Camp [Admin 3: PRP]", "Domiz Camp [Admin 3: PRP]", NA, NA, NA, 
        NA, NA, NA, NA, NA, NA, NA, NA, "Bashu 1 & 2 [Admin 3: POC]", NA, NA, 
        NA, "Ikyogem (Settlement ) [Admin 3: POC]", 
        "Okende (Settlement) [Admin 3: POC]", NA, NA, NA, "Kaya [Admin 3: POC]",
        "Al Redis2 [Admin 3: POC]", "Babikri [Admin 3: POC]", 
        "Shagarab-II [Admin 3: POC]", "Shagarab-III [Admin 3: POC]", NA,
        "Mae La (within Admin 1: Tak ) [Admin 3: POC]", 
        "Imvepi RC [Admin 3: POC]", NA, "Lamwo [Admin 2]", NA, NA, NA
      )
    )
    
    sites[which(sites$nomatch != 1), "site_demog"] <- x$site_demog  
    # NAs: could not be found or difficult to resolve
    # Domiz 1 and 2 need to be combined in mh dataset
    # info on Gawilan, Kawergosk, Qushtapa (Iraq), Sayam Forage, Tillia (Niger), 
    # Al Kharaz, Basatine (Yemen), Tongogara (Zimbabwe) online
    # pop_type 'National' in mh shouldn't be matched to demog

    
  #...................................      
  ## Link site names with site names in UNHCR-provided population dataset
        
    # Check whether all sites in the UNHCR population dataset appear on mh sites
    sites_pop0405 <- unique(pop0405[, c("country", "site")])
    sites_pop0405 <- sites_pop0405[
      order(sites_pop0405$country, sites_pop0405$site), ]
    sites[which(! sites$site %in% sites_pop0405$site), c("country", "site")]
    
    # Set non-matching sites to NA (not present in UNHCR population dataset)
    sites$site_pop0405 <- sites$site
    sites[which(! sites$site %in% sites_pop0405$site), "site_pop0405"] <- NA
      
    
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
      # all national observations are for both sexes
      mh[which(mh$pt_type == "national"), "sex"] <- "both"
      # remove 3 refugee observations with missing sex
      mh <- subset(mh, (pt_type == "refugee" & ! is.na(sex)) | 
          pt_type == "national")
    

#...............................................................................
### Reading, appending and cleaning consultation indicator data
#...............................................................................

  #...................................      
  ## Read and clean clinic functionality data
    
    # Read
    clinics <- read.csv(paste0(dir_path, 
      "in/UNHCR_OPD_consultation_indicators_Jan_2024-June_2025.csv"))
       
    # Rename columns
    colnames(clinics) <- c("region", "country", "site", "hf", "mmyy", "epiweek",
      "fte_clinicians", "days_open")
    str(clinics)               
      
    # Reformat columns
    clinics$mmyy <- lubridate::my(clinics$mmyy)
    
    # Remove implausible values
    clinics[which(clinics$fte_clinicians >= 35), "fte_clinicians"] <- NA
    clinics[which(clinics$days_open >= 75), "days_open"] <- NA
    
    # Aggregate to monthly data
    clinics <- aggregate(clinics[, c("fte_clinicians", "days_open")],
      by = clinics[, c("country", "site", "hf", "mmyy")], FUN = mean, na.rm = T)
    clinics[which(clinics$fte_clinicians == "NaN"), "fte_clinicians"] <- NA    
    clinics[which(clinics$days_open == "NaN"), "days_open"] <- NA
    
    # Aggregate to sites
    clinics <- aggregate(clinics[, c("fte_clinicians", "days_open")],
      by = clinics[, c("country", "site", "mmyy")], FUN = sum, na.rm = T)
    
    # Check whether all sites in mh are featured here
    sites_clinics <- unique(clinics[, c("country", "site")])
    sites_clinics <- sites_clinics[order(sites_clinics$country, 
      sites_clinics$site), ]
    sites[which(! sites$site %in% sites_clinics$site), c("country", "site")]
      # yes
    
  #...................................      
  ## Read and clean consultation tally data
    
    # Read
    cons <- read.csv(paste0(dir_path, 
      "in/UNHCR_OPD_consultations_Jan_2024-June_2025.csv"))
    
    # Rename columns
    colnames(cons) <- c("region", "country", "site", "mmyy", "pt_type", 
      "n_cases", "n_cases_new", "n_cases_mh_new", "sex")

    # Reformat columns
    cons$mmyy <- lubridate::my(cons$mmyy)    
    cons$pt_type <- tolower(cons$pt_type)
    cons$sex <- tolower(cons$sex)
    cons[which(cons$sex == "n/a"), "sex"] <- NA

    # Check whether all sites in mh are featured here
    sites_cons <- unique(cons[, c("country", "site")])
    sites_cons <- sites_cons[order(sites_cons$country, 
      sites_cons$site), ]
    sites[which(! sites$site %in% sites_cons$site), c("country", "site")]
      # yes
        
    # Aggregate consultations for both sexes (since sex is not
        # disaggregated in mh dataset and new cases are often not sex-specific)
    cons <- aggregate(cons[, c("n_cases", "n_cases_new", "n_cases_mh_new")], 
      by = cons[, c("region", "country", "site", "mmyy", "pt_type")], 
      sum, na.rm = T)

    # Add number of MH cases from mh dataset
    x <- aggregate(list(n_cases_mh = mh$n_cases), by = mh[, c("country", "site",
      "mmyy", "pt_type")], FUN = sum, na.rm = T)
    cons <- merge(cons, x, by = c("country", "site", "mmyy", "pt_type"),
      all.x = T)
    
    # Eliminate implausible values
    cons[which(cons$n_cases > 50000), "n_cases"] <- NA
    cons[which(cons$n_cases_new > cons$n_cases), "n_cases_new"] <- NA
    View(subset(cons, n_mh_cases_new/n_cases > 0.20))
################
    
    
                    
#...............................................................................  
### ENDS
#...............................................................................
     
