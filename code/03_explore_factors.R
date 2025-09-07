#...............................................................................
### +++++++ ANALYSIS OF UNHCR iRHIS MENTAL HEALTH CONSULTATION DATA ++++++++ ###
#...............................................................................

#...............................................................................
## -------- R SCRIPT TO EXPLORE FACTORS ASSOCIATED WITH DATA PATTERNS ------- ##
#...............................................................................



#...............................................................................
### Exploring univariate associations with consultation rate
#...............................................................................

  #...................................      
  ## Prepare dataset

    # Explore missingness in clinics dataset
    df <- subset(mh2b, pt_type == "refugee")
    x <- table(df$days_open_implausible)
    cbind(x, prop.table(x))
    x <- table(df$fte_clinicians_implausible)
    cbind(x, prop.table(x))
    df$clinics_implausible <- NA
    df[which(df$days_open_implausible == "plausible" & 
        df$fte_clinicians_implausible == "plausible"), "clinics_implausible"] <-
      "plausible"
    df[which(df$days_open_implausible == "missing" | 
        df$fte_clinicians_implausible == "missing"), "clinics_implausible"] <-
      "missing"
    df[which(is.na(df$clinics_implausible)), "clinics_implausible"] <- 
      "implausible"
    x <- table(df$clinics_implausible)
    cbind(x, prop.table(x))
    
    # Visualise missingness
    df <- df[, c("region", "country", "site", "mmyy", "fte_clinicians", 
      "days_open")]
    df$missing <- is.na(df$fte_clinicians) | is.na(df$days_open)
    pl <- ggplot(df, aes(x = mmyy, y = site, fill = missing)) +
      geom_tile(colour = "black") +
      scale_y_discrete("site", limits = rev, expand = c(0,0)) +
      scale_x_date("month", date_breaks = "1 month", 
        date_labels = "%b-%Y", expand = c(0,0)) +
      scale_fill_manual("missing", values = c(palette_gen[c(8,16)])) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top", panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0)) +
      facet_nested(region + country ~ ., scales = "free_y", space = "free_y")
    ggsave(paste0(dir_path, "out/03_missing_clinic_data.png"), 
      dpi = "print", units = "cm", width = 45, height = 45*(hw-0.05))
      
    # Reduce dataset to non-missing observations
    mh3 <- subset(mh2b, pt_type == "refugee" & ! is.na(n_cases) & 
        ! is.na(n_cases_all) & days_open_implausible == "plausible" &
        fte_clinicians_implausible == "plausible" & ! is.na(pop_0405))

    # Aggregate across all MH causes
    x <- unique(mh3[, c("region", "country", "country_iso", "site", "mmyy",
      "n_cases_all", "fte_clinicians", "days_open", "pop_0405")])
    df <- aggregate(list(n_cases = mh3$n_cases), 
      by = mh3[, c("country", "site", "mmyy")], FUN = sum, na.rm = T)
    mh3 <- merge(df, x, by = c("country", "site", "mmyy"), all.x = T)        

    # Fix missing country ISO codes
    x <- unique(mh3[which(! is.na(mh3$country_iso)), 
      c("country", "country_iso")])
    x <- merge(data.frame(country = unique(mh3$country)), x, by = "country",
      all.x =T)
    x[which(x$country == "Congo"), "country_iso"] <- "COG"
    x[which(x$country == "Zimbabwe"), "country_iso"] <- "ZWE"
    mh3 <- mh3[, ! colnames(mh3) == "country_iso"]
    mh3 <- merge(mh3, x, by = "country", all.x = T)

    # Compute consultation rates
    mh3$cons_rate_mh <- mh3$n_cases * 1200 / mh3$pop_0405
    mh3$cons_rate_all <- mh3$n_cases_all * 1200 / mh3$pop_0405
    