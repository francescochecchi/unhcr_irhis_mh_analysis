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

    # Unique sites
    sites_demog <- unique(demog[which(demog$year >= 2024), c("country","site")])

    
  #...................................      
  ## Read and clean population 2024-2025 dataset provided by UNHCR

    # Read
    pop0405 <- read.csv(paste0(dir_path, 
      "in/UNHCR_Refugees_pop_Jan_2024-June_2025.csv"))
    
    # Rename columns
    colnames(pop0405) <- c("region", "country", "site", "pop", "year")

    # Rename countries that don't match with mh dataset
    pop0405[which(pop0405$country == "Democratic Republic of the Congo (DRC)"),
      "country"] <- "Democratic Republic of the Congo"
    pop0405[which(pop0405$country == "Tanzania"),
      "country"] <- "United Republic of Tanzania"
    pop0405[which(pop0405$country == "Republic of the Congo"),
      "country"] <- "Congo"
    
    # Unique sites
    sites_pop0405 <- unique(pop0405[, c("country", "site")])
    sites_pop0405 <- sites_pop0405[
      order(sites_pop0405$country, sites_pop0405$site), ]
    
    
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
    sites <- as.data.frame(table(mh[, c("region", "country", "site")]))
    colnames(sites) <- c("region", "country", "site", "n_obs")
    sites <- subset(sites, n_obs > 0)
    sites <- sites[order(sites$country, sites$site), ]  
    
    # Check that all countries also appear in the demographic dataset
    x1 <- as.character(unique(sites$country))
    x2 <- unique(sites_demog$country)
    x1[! x1 %in% x2]
 
    # Restrict sites_demog to countries in mh dataset
    sites_demog <- subset(sites_demog, country %in% unique(sites$country))
    
  #...................................      
  ## Link site names with site names in demographic dataset
    
    # Check that sites all also appear in the demographic dataset
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
        NA, NA, NA, NA, NA, 
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
    sites$site_demog <- unlist(sites$site_demog)
    # NAs: could not be found or difficult to resolve
    # info on Gawilan, Kawergosk, Qushtapa (Iraq), Sayam Forage, Tillia (Niger), 
    # Al Kharaz, Basatine (Yemen), Tongogara (Zimbabwe) online
    # pop_type 'National' in mh shouldn't be matched to demog

    
  #...................................      
  ## Link site names with site names in UNHCR-provided population dataset
        
    # Check whether all sites in the UNHCR population dataset appear on mh sites
    sites[which(! sites$site %in% sites_pop0405$site), c("country", "site")]
    
    # Set non-matching sites to NA (not present in UNHCR population dataset)
    sites$site_pop0405 <- sites$site
    sites[which(! sites$site %in% sites_pop0405$site), "site_pop0405"] <- NA
      

  #...................................      
  ## Categorise mental health conditions according to ICD-11
    
    # First-level category
    icd <- data.frame(cause = unique(mh$cause))
    icd$cat1 <- NA
    icd[which(icd$cause %in% c("Epilepsy / seizures" )), 
      "cat1"] <- "08A6x - Epilepsy or seizures"       
    icd[which(icd$cause %in% c("Delirium", "Dementia", "Dementia or Delirium")), 
      "cat1"] <- "06Dxx - Neurocognitive disorders"       
    icd[which(icd$cause %in% c("Acute psychosis", "Chronic psychosis")), 
      "cat1"] <- "06A2x - Schizophrenia or other primary psychotic disorders"
    icd[which(icd$cause %in% c("Dissociative disorder")), 
      "cat1"] <- "06B6x - Dissociative disorders"
    icd[which(icd$cause %in% c("Developmental disorders", 
      "Intellectual disability", 
      "Intellectual disability and developmental disorders" )), 
      "cat1"] <- "06A0x - Neurodevelopmental disorders"
    icd[which(icd$cause %in% c("Alcohol-related disorders", 
      "Substance use disorders related to opiate use", 
      "Alcohol or other substance use disorder", 
      "Other substance use disorders",
      "Substance use disorders related to benzodiazepine or other prescription medication")), 
      "cat1"] <- "06C4x - Disorders due to substance use"
    icd[which(icd$cause %in% c("Bipolar disorders (Mania)", 
      "Psychosis (including bipolar disorder)")), 
      "cat1"] <- "06A6x - Bipolar or related disorders"       
    icd[which(icd$cause %in% c(
      "Moderate-severe forms of anxiety disorder and mixed presentations")), 
      "cat1"] <- "06B0x - Anxiety or fear-related disorders"
    icd[which(icd$cause %in% c("Posttraumatic stress disorder", "Acute Stress", 
      "Grief")), 
      "cat1"] <- "06B4x - Disorders specifically associated with stress"      
    icd[which(icd$cause %in% c("Moderate-severe depression")), 
      "cat1"] <- "06A7x - Depressive disorders"      
    icd[which(icd$cause %in% c("Other moderate-severe emotional disorders", 
      "Moderate-severe emotional disorder")), 
      "cat1"] <- "06A8Z - Mood disorders, unspecified"      
    icd[which(icd$cause %in% c("Medically unexplained somatic complaint")), 
      "cat1"] <- "06C2x - Disorders of bodily distress or bodily experience"
    icd[which(icd$cause %in% c("Other psychological complaint")), 
      "cat1"] <- 
      "06E8Z - Mental, behavioural or neurodevelopmental disorders, unspecified"
    icd[which(icd$cause %in% c("Self-harm/suicide attempt")), 
      "cat1"] <- "PD3Z - Intentional self-harm"
    icd <- icd[order(icd$cat1, icd$cause), ]        
  
    # Second-level category
    icd$cat2 <- NA
    icd[which(icd$cat1 %in% 
        c("06A2x - Schizophrenia or other primary psychotic disorders",
          "06A6x - Bipolar or related disorders",
          "06C2x - Disorders of bodily distress or bodily experience")), 
      "cat2"] <- "Schizophrenia, bipolar and related disorders"
    icd[which(icd$cat1 %in% 
        c("06A7x - Depressive disorders", "06A8Z - Mood disorders, unspecified",
          "06B0x - Anxiety or fear-related disorders")), 
      "cat2"] <- "Anxiety, depression and other mood disorders"
    icd[which(icd$cat1 %in% c("06C4x - Disorders due to substance use",
      "PD3Z - Intentional self-harm")), 
      "cat2"] <- "Substance abuse, self-harm"
    icd[which(icd$cat1 %in% c("06A0x - Neurodevelopmental disorders",
      "06Dxx - Neurocognitive disorders", 
      "06E8Z - Mental, behavioural or neurodevelopmental disorders, unspecified"
      )), 
      "cat2"] <- "Neurodevelopmental and neurocognitive disorders"
    icd[which(icd$cat1 %in% c("08A6x - Epilepsy or seizures")), 
      "cat2"] <- "Epilepsy or seizures"
    icd[which(icd$cat1 %in% c(
      "06B4x - Disorders specifically associated with stress",
      "06B6x - Dissociative disorders")), 
      "cat2"] <- "PTSD and dissociative disorders"

    # Apply categories to dataset
    mh <- merge(mh, icd, by = "cause", all.x = T)
              
    
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
    table(mh$sex, useNA = "always")    

    
    
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

    # Rename countries that don't match with mh dataset
    x <- unique(clinics$country)
    x[which(! x %in% unique(mh$country))]
    clinics[which(clinics$country == "Democratic Republic of the Congo (DRC)"),
      "country"] <- "Democratic Republic of the Congo"
    clinics[which(clinics$country == "Tanzania"),
      "country"] <- "United Republic of Tanzania"
    clinics[which(clinics$country == "Republic of the Congo"),
      "country"] <- "Congo"
        
    # Identify health facilities
    hfs <- unique(clinics[, c("country", "site", "hf")])
    hfs <- merge(hfs, sites[, c("country", "site")], by = c("country", "site"), 
      all.y = T)
    
    # Reformat columns
    clinics$mmyy <- lubridate::my(clinics$mmyy)
    
    # Remove implausible values
    clinics$implausible <- "plausible"
    clinics[which(clinics$fte_clinicians >= 35), "implausible"] <- 
      "implausible FTEs"
    clinics[which(clinics$fte_clinicians >= 35), "fte_clinicians"] <- NA
    clinics[which(clinics$days_open >= 75), "implausible"] <- 
      "implausible days open"
    clinics[which(clinics$days_open >= 75), "days_open"] <- NA
    cbind(table(clinics$implausible), prop.table(table(clinics$implausible)))
    
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
        
    # Aggregate national consultations for both sexes (since sex is not
        # disaggregated in mh dataset)
    cons[which(cons$pt_type == "national"), "sex"] <- "both"
    cons <- aggregate(cons[, c("n_cases", "n_cases_new", "n_cases_mh_new")], 
      by = cons[, c("region", "country", "site", "mmyy", "pt_type", "sex")], 
      sum, na.rm = T)

    # Add number of MH cases from mh dataset
    x <- aggregate(list(n_cases_mh = mh$n_cases), by = mh[, c("country", "site",
      "mmyy", "pt_type", "sex")], FUN = sum, na.rm = T)
    cons <- merge(cons, x, by = c("country", "site", "mmyy", "pt_type", "sex"),
      all.x = T)
    
    # Check completeness
    for (i in grep("cases", colnames(cons), value = T)) {
      print(paste0("completeness  of ", i))
      print(table(is.na(cons[, i])))
    }
    
    # Recode missing MH cases to 0 (assume there were no MH consultations)
    cons[which(is.na(cons$n_cases_mh)), "n_cases_mh"] <- 0
    
    # Eliminate implausible values
    cons$implausible <- "plausible"
    cons[which(cons$n_cases > 50000), "implausible"] <- 
      "implausible number of cases"
    cons[which(cons$n_cases > 50000), "n_cases"] <- NA
    cons[which(cons$n_cases_new > cons$n_cases), "implausible"] <- 
      "new cases > total cases"
    cons[which(cons$n_cases_new > cons$n_cases), "n_cases_new"] <- NA
    cons[which(cons$n_cases_mh_new > cons$n_cases_mh), "implausible"] <-
      "new MH cases > total MH cases"
    cons[which(cons$n_cases_mh_new > cons$n_cases_mh), "n_cases_mh_new"] <- NA
    cons[which(cons$n_cases_mh > cons$n_cases), "implausible"] <- 
      "MH cases > total cases"
    cons[which(cons$n_cases_mh > cons$n_cases), "n_cases"] <- NA
    cons[which((cons$n_cases_mh/cons$n_cases) > 0.50), "implausible"] <- 
      "MH cases > 50% of total cases"
    cons[which((cons$n_cases_mh/cons$n_cases) > 0.50), "n_cases"] <- NA
    x <- table(cons$implausible, useNA = "always")
    cbind(x, prop.table(x))    
    
    # Rename countries that don't match with mh dataset
    x <- unique(cons$country)
    x[which(! x %in% unique(mh$country))]
    cons[which(cons$country == "Democratic Republic of the Congo (DRC)"),
      "country"] <- "Democratic Republic of the Congo"
    cons[which(cons$country == "Tanzania"),
      "country"] <- "United Republic of Tanzania"
    cons[which(cons$country == "Republic of the Congo"),
      "country"] <- "Congo"
    
             
#...............................................................................
### Merging and aggregating different datasets
#...............................................................................
    
  #...................................      
  ## Create aggregate versions of mh dataset
    
    # Rectangularise dataset
    x <- expand.grid(
      site = sort(sites$site),
      mmyy = sort(unique(mh$mmyy)),
      age = sort(unique(mh$age)),
      sex = sort(unique(mh$sex)),
      cat1 = sort(unique(mh$cat1))
    )
    x$pt_type <- ifelse(x$sex == "both", "national", "refugee")
    x <- merge(x, sites[, c("region", "country", "site", "site_demog",
      "site_pop0405")], by = "site", all.x = T)
    x1 <- unique(icd[, c("cat1", "cat2")])
    colnames(x1) <- c("cat1", "cat2")
    x <- merge(x, x1, by = "cat1", all.x = T)
    
    # Aggregate by cat1
    mh1 <- aggregate(list(n_cases = mh$n_cases), 
      by = mh[, c("region", "country", "site", "mmyy", "cat1", 
        "pt_type", "age", "sex")], FUN = "sum", na.rm = TRUE)
    mh1 <- merge(x, mh1, by = c("region", "country", "site", "mmyy", "cat1",
      "pt_type", "age", "sex"), all.x = T)
    mh1[which(is.na(mh1$n_cases)), "n_cases"] <- 0
    mh1 <- mh1[, c("region", "country", "site", "site_demog", "site_pop0405",
      "mmyy", "pt_type", "cat1", "cat2", "age", "sex", "n_cases")]
    
    # Aggregate by cat2
    mh2 <- aggregate(list(n_cases = mh1$n_cases), 
      by = mh1[, c("region", "country", "site",
        "mmyy", "pt_type", "cat2", "age", "sex")], FUN = "sum", na.rm = TRUE)
    mh2 <- merge(mh2, sites[, c("region", "country", "site", "site_demog",
      "site_pop0405")], by = c("region", "country", "site"), all.x = T)
    
    # Aggregate by cat2 across all ages
    mh2a <- aggregate(list(n_cases = mh2$n_cases), 
      by = mh2[, c("region", "country", "site",
        "mmyy", "pt_type", "cat2", "sex")], FUN = "sum", na.rm = TRUE)
    mh2a <- merge(mh2a, sites[, c("region", "country", "site", "site_demog",
      "site_pop0405")], by = c("region", "country", "site"), all.x = T)
               
    # Aggregate by cat2 across all ages and sexes
    mh2b <- aggregate(list(n_cases = mh2a$n_cases), 
      by = mh2a[, c("region", "country", "site",
        "mmyy", "pt_type", "cat2")], FUN = "sum", na.rm = T)
    mh2b <- merge(mh2b, sites[, c("region", "country", "site", "site_demog",
      "site_pop0405")], by = c("region", "country", "site"), all.x = T)
    
  #...................................      
  ## Merge in consultations dataset
           
    # Merge (only where appropriate)
    colnames(cons) <- c("country", "site", "mmyy", "pt_type", "sex",
      "region", "n_cases_all", "n_cases_new_all", "n_cases_mh_new",
      "n_cases_mh", "implausible_cases")
    mh2a <- merge(mh2a, cons, by = c("region", "country", "site", "mmyy", 
      "pt_type", "sex"), all.x = T)    
    x <- aggregate(cons[,c("n_cases_all", "n_cases_new_all", "n_cases_mh_new")],
      by = cons[, c("region", "country", "site", "mmyy", "pt_type")], FUN = sum)
    mh2b <- merge(mh2b, x, by = c("region", "country", "site", "mmyy", 
      "pt_type"), all.x = T)    

  #...................................      
  ## Merge in clinics dataset
    
    # Merge
    mh1 <- merge(mh1, clinics, by = c("country", "site", "mmyy"), all.x = T)
    mh2 <- merge(mh2, clinics, by = c("country", "site", "mmyy"), all.x = T)
    mh2a <- merge(mh2a, clinics, by = c("country", "site", "mmyy"), all.x = T)
    mh2b <- merge(mh2b, clinics, by = c("country", "site", "mmyy"), all.x = T)
 
    # Exclude implausible clinics data
    for (i in c("mh1", "mh2", "mh2a", "mh2b")) {
      
      # get the dataframe    
      df <- get(i)
      
      # determine plausibility of days open
      df$days_open_implausible <- "plausible"
      df[which(is.na(df$days_open)), "days_open_implausible"] <- "missing"
      x <- which(df$days_open == 0 & df$n_cases_all > 0)
      df[x, "days_open_implausible"] <- 
        "implausible - zero days open but cases (all causes) > 0"
      df[x, "days_open"] <- NA

      # determine plausibility of FTE clinicians
      df$fte_clinicians_implausible <- "plausible"
      df[which(is.na(df$fte_clinicians)), "fte_clinicians_implausible"] <- 
        "missing"
      x <- which(df$fte_clinicians == 0 & df$n_cases_all > 0)
      df[x, "fte_clinicians_implausible"] <- 
        "implausible - zero clinician FTEs but cases (all causes) > 0"
      df[x, "fte_clinicians"] <- NA
      
      # reassign dataframe
      assign(i, df)
    }

        
  #...................................      
  ## Merge in population datasets (where appropriate)
  
    # Population by age and sex (demog)
      # subset
      x <- subset(demog, country %in% unique(sites$country) & 
          site %in% unique(sites$site_demog) & year %in% c(2024, 2025))
      
      # exclude sites where no population by age/sex is available
      x <- x[-which(x$male_unknown > 0 | x$female_unknown > 0), ]
      
      # aggregate by site across all countries of origin
        # (ok to combine all population types - have checked this)
      cols_pop <- c(paste0("female_", c("0to4", "5to17", "18to59", "60+")),
        paste0("male_", c("0to4", "5to17", "18to59", "60+")))
      x <- aggregate(x[, cols_pop], by = x[, c("country", "country_iso",
        "site", "year")], FUN = sum, na.rm = T)
      
      # check for site duplicates
      table(duplicated(x[, c("country", "site", "year")]))
      
      # reshape long
      x <- reshape(x, direction = "long", 
        varying = which(colnames(x) %in% cols_pop), sep = "_",
        idvar = c("country", "country_iso", "site", "year"), timevar = "age")
      x <- reshape(x, direction = "long", varying = c("female", "male"),
        idvar = c("country", "country_iso", "site", "year", "age"), 
        timevar = "sex", times = c("female", "male"), v.names = "pop")        
        
      # rename columns and categories ahead of merging
      colnames(x) <- c("country", "country_iso", "site_demog", "year", 
        "age", "sex", "pop_demog")
      x$age <- ifelse(x$age == "0to4", "0 to 4yrs", x$age)
      x$age <- ifelse(x$age == "5to17", "5 to 17yrs", x$age)
      x$age <- ifelse(x$age == "18to59", "18 to 59yrs", x$age)
      x$age <- ifelse(x$age == "60+", "60+ yrs", x$age)
      x$age <- factor(x$age, levels = levels(mh2$age))
      x$sex <- factor(x$sex, levels = levels(mh2$sex))
      x$country <- factor(x$country, levels = levels(mh1$country))   
      x <- data.frame(x)
      
      # merge where appropriate
      mh1$year <- year(mh1$mmyy)
      mh1 <- merge(mh1, x, by =c("country", "site_demog", "year", "age", "sex"),
        all.x = T)
      mh2$year <- year(mh2$mmyy)
      mh2 <- merge(mh2, x, by =c("country", "site_demog", "year", "age", "sex"),
        all.x = T)
      x <- aggregate(list(pop_demog = x$pop_demog), 
        by = x[, c("country", "country_iso", "site_demog", "year", "sex")], 
        FUN = sum, na.rm = T)
      mh2a$year <- year(mh2a$mmyy)
      mh2a <- merge(mh2a, x, by =c("country", "site_demog", "year", "sex"),
        all.x = T)
      x <- aggregate(list(pop_demog = x$pop_demog), 
        by = x[, c("country", "country_iso", "site_demog", "year")], 
        FUN = sum, na.rm = T)
      mh2b$year <- year(mh2b$mmyy)
      mh2b <- merge(mh2b, x, by =c("country", "site_demog", "year"), all.x = T)

    # Population provided by UNHCR
    colnames(pop0405) <- c("region", "country", "site_pop0405", "pop_0405", 
      "year")
    mh1 <- merge(mh1, pop0405, by = c("region", "country", "site_pop0405", 
      "year"), all.x = T)
    mh2 <- merge(mh2, pop0405, by = c("region", "country", "site_pop0405", 
      "year"), all.x = T)
    mh2a <- merge(mh2a, pop0405, by = c("region", "country", "site_pop0405", 
      "year"), all.x = T)
    mh2b <- merge(mh2b, pop0405, by = c("region", "country", "site_pop0405", 
      "year"), all.x = T)

    
  #...................................      
  ## Reconcile the two population sources
    
    # Compare two population sources
    df <- unique(mh2b[, c("country", "country_iso", "site", "year", "pop_demog",
      "pop_0405")])
    p1 <- ggplot(df, aes(x = pop_demog, pop_0405)) +
      geom_point(alpha = 0.50, fill = palette_gen[8], colour = palette_gen[12],
        stroke = 1) +
      theme_bw() +
      scale_x_continuous("population size, 2024 (public dataset)", 
        labels = comma) +
      scale_y_continuous("population size, 2024 (unpublished dataset)", 
        labels = comma) +
      geom_abline(slope = 1, intercept = c(0,0), colour = palette_gen[2],
        linetype = c("22"))
    ggsave(paste0(dir_path, "out/01_pop_sources_compared.png"), units = "cm",
      dpi = "print", width = 15, height = 10)        
    
        # DECISION: since demog only available for 2024, and given differences, 
            # use former only to construct age distributions
    
    # Come up with age-sex distribution
    agesex_dist <- 
      unique(mh2[, c("country", "site", "year", "age", "sex", "pop_demog")])
    agesex_dist <- agesex_dist[which(agesex_dist$year == 2024 
      & ! is.na(agesex_dist$pop_demog)), ]
    agesex_dist <- agesex_dist[order(agesex_dist$country, agesex_dist$site,
      agesex_dist$sex, agesex_dist$age), ]
    x <- by(agesex_dist, agesex_dist$site, 
      function(xx) {xx$pop_demog / sum(xx$pop_demog)})
    x <- do.call(rbind, x)
    agesex_dist <- data.frame(row.names(x), x)
    colnames(agesex_dist) <- c("site",
      c(paste0("propfemale_", levels(mh2$age)),
        paste0("propmale_", levels(mh2$age)))    
    )
    agesex_dist <- reshape(agesex_dist, direction = "long", 
      varying = grep("prop", colnames(agesex_dist)), sep = "_",
      idvar = "site", timevar = "age")
    agesex_dist <- reshape(agesex_dist, direction = "long",
      varying = c("propfemale", "propmale"), idvar = c("site", "age"),
      timevar = "sex", times = c("female", "male"), v.names = "prop_agesex")
    agesex_dist$age <- factor(agesex_dist$age, levels = levels(mh2$age))
    agesex_dist$sex <- factor(agesex_dist$sex, levels = levels(mh2$sex))

    # Come up with sex distribution
    sex_dist <- aggregate(list(prop_sex = agesex_dist$prop_agesex),
      by = agesex_dist[, c("site", "sex")], FUN = sum, na.rm= T)
                    
    # Merge into datasets
    mh1 <- merge(mh1, agesex_dist, by = c("site", "age", "sex"), all.x = T)
    mh2 <- merge(mh2, agesex_dist, by = c("site", "age", "sex"), all.x = T)
    mh2a <- merge(mh2a, sex_dist, by = c("site", "sex"), all.x = T)

  #...................................      
  ## Fix missing country ISO codes

    # Corrected dataset of country ISO codes
    x <- unique(mh1[which(! is.na(mh1$country_iso)), 
      c("country", "country_iso")])
    x <- merge(data.frame(country = unique(mh1$country)), x, by = "country",
      all.x =T)
    x[which(x$country == "Congo"), "country_iso"] <- "COG"
    x[which(x$country == "Zimbabwe"), "country_iso"] <- "ZWE"    
    
    # Merge into mh datasets
    for (i in c("mh1", "mh2", "mh2a", "mh2b")) {
      df <- get(i)
      df <- df[, colnames(df) != "country_iso"]
      df <- merge(df, x, by = "country", all.x = T)
      assign(i, df)
    }

    
#...............................................................................
### Reading and merging WHO Global Health Observatory mental health data
#...............................................................................

  #...................................      
  ## Read and clean WHO data, choosing most recent value if multiple
    
    # Psychiatrists
    who_psych <- read.csv(paste0(dir_path, "in/who_mh_psychiatrists.csv"))
    who_psych <- who_psych[, c("SpatialDimensionValueCode", "TimeDim", "Value")]
    colnames(who_psych) <- c("country_iso", "year_psych", "n_psych")
    
    # MH-related consultations
    who_cons <- read.csv(paste0(dir_path, "in/who_mh_consultations.csv"))
    who_cons <- who_cons[, c("SpatialDimValueCode", "Period", 
      "FactValueNumeric")]
    colnames(who_cons) <- c("country_iso", "year_cons", "cons_rate_mh_natl")
    
    # MH expenditure out of entire health expenditure
    who_exp <- read.csv(paste0(dir_path, "in/who_mh_expenditure.csv"))
    who_exp <- who_exp[, c("SpatialDimensionValueCode", "TimeDim", "Value")]
    colnames(who_exp) <- c("country_iso", "year_exp", "exp_mh")
    
  #...................................      
  ## Merge WHO datasets
    
    # Merge
    who <- merge(who_cons, who_exp, by = "country_iso", all = T)
    who <- merge(who, who_psych, by = "country_iso", all = T)    
    
    # Reformat
    who$cons_rate_mh_natl <- who$cons_rate_mh_natl / 1000
    who$exp_mh <- who$exp_mh / 100
    
    # Select only countries in mh dataset
    x <- unique(mh1$country_iso)
    who <- subset(who, country_iso %in% x)
        
#...............................................................................  
### ENDS
#...............................................................................
     
