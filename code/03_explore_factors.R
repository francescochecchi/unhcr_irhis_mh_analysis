#...............................................................................
### +++++++ ANALYSIS OF UNHCR iRHIS MENTAL HEALTH CONSULTATION DATA ++++++++ ###
#...............................................................................

#...............................................................................
## -------- R SCRIPT TO EXPLORE FACTORS ASSOCIATED WITH DATA PATTERNS ------- ##
#...............................................................................



#...............................................................................
### Univariate associations with consultation rate
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
      geom_tile() +
      scale_y_discrete("site", limits = rev, expand = c(0,0)) +
      scale_x_date("month", date_breaks = "1 month", 
        date_labels = "%b-%Y", expand = c(0,0)) +
      scale_fill_manual("missing", values = c(palette_gen[c(8,16)])) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top", panel.grid.major.x = element_blank(),
        strip.text.y = element_text(angle = 0)) +
      facet_nested(region + country ~ ., scales = "free_y", space = "free_y")
    ggsave(paste0(dir_path, "out/03_missing_clinic_data.png"), 
      dpi = "print", units = "cm", width = 45, height = 45*(hw-0.05))
      
        
    # Consultation rate per month
    
