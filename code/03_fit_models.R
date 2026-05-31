#...............................................................................
### +++++++ ANALYSIS OF UNHCR iRHIS MENTAL HEALTH CONSULTATION DATA ++++++++ ###
#...............................................................................

#...............................................................................
## -------- R SCRIPT TO EXPLORE FACTORS ASSOCIATED WITH DATA PATTERNS ------- ##
#...............................................................................



#...............................................................................
### Association of cases/clinician with the proportion of MH consultations
#...............................................................................

  #...................................      
  ## Prepare dataset

    # Explore missingness in clinics dataset
    df <- subset(mh1, pt_type == "refugee")
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
    df <- subset(mh1, pt_type == "refugee" & ! is.na(n_cases) & 
        ! is.na(prop_agesex) & days_open_implausible == "plausible" &
        fte_clinicians_implausible == "plausible" & ! is.na(pop_2425))

    # Aggregate across all MH causes
    x <- unique(df[, c("region", "country", "country_iso", "site", "mmyy",
      "fte_clinicians", "days_open", "pop_2425")])
    mh3 <- aggregate(list(n_cases = df$n_cases), 
      by = df[, c("country", "site", "mmyy")], FUN = sum, na.rm = T)
    mh3 <- merge(mh3, x, by = c("country", "site", "mmyy"), all.x = T)

    # Add age-sex proportions as two single variables
        # proportion aged 18+ and proportion female
    df <- agesex_dist
    df$prop_age18plus <- ifelse(df$age %in% c("0 to 4yrs", "5 to 17yrs"),
      0, df$prop_agesex)
    df$prop_f <- ifelse(df$sex == "male", 0, df$prop_agesex)
    df <- aggregate(df[, c("prop_age18plus", "prop_f")], 
      by = list(site = df$site), FUN = sum, na.rm = T)
    mh3 <- merge(mh3, df, by = "site", all.x = T)

    # Add total all-cause consultations
    df <- subset(mh2b, pt_type == "refugee" & ! is.na(n_cases) & 
        ! is.na(n_cases_all) & days_open_implausible == "plausible" &
        fte_clinicians_implausible == "plausible" & ! is.na(pop_2425))
    df <- aggregate(list(n_cases_all = df$n_cases_all), 
      by = df[, c("country", "site", "mmyy")], FUN = mean, na.rm = T)
    mh3 <- merge(mh3, df, by = c("country", "site", "mmyy"), all.x = T)
    
    # Compute annual consultation rate
    mh3$cons_rate_mh <- mh3$n_cases * 1200 / mh3$pop_2425
    
    # Add WHO data
    x <- who[, c("country_iso", "n_psych")]
    mh3 <- merge(mh3, x, by = "country_iso", all.x = T)
    # mh3 <- subset(mh3, ! is.na(n_psych))
    
    # Add HDI data
    mh3 <- merge(mh3, hdi, by = "country", all.x = T)
    mh3 <- subset(mh3, ! is.na(hdi))

    # Calculate daily consultations (all causes) per clinician FTE
    mh3$cases_fte <- mh3$n_cases_all / (mh3$fte_clinicians * mh3$days_open)
    
    
  #...................................      
  ## Visualise variable distributions

    # # MH consultation rate
    # pl <- ggplot(mh3, aes(x = cons_rate_mh)) +
    #   geom_histogram(alpha = 0.75, colour = "black", fill = palette_gen[10]) +
    #   scale_x_continuous("MHNSU-related consultation rate",
    #     trans = "sqrt", limits = c(NA, NA), breaks = c(0, 2, 5, 10, 20, 40, 
    #       60, 80, 100), expand = c(0,0)) +
    #   scale_y_continuous("number of site-months", 
    #     expand = expansion(add = c(0,20))) +
    #   theme_bw() +
    #   theme(panel.grid.major.x = element_blank())
    # ggsave(paste0(dir_path, "out/03_dist_cons_rate.png"), 
    #   dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    # 
    #   # remove outlier consultation rate values (n = 11 with value > 100)
    #   mh3 <- subset(mh3, cons_rate_mh < 100)

    # MH consultations proportion
    mh3$cons_prop_mh <- ifelse(mh3$n_cases_all > 0, 
      mh3$n_cases / mh3$n_cases_all, NA) 
    mh3$n_cases_not_mh <- mh3$n_cases_all - mh3$n_cases
    pl <- ggplot(mh3, aes(x = cons_prop_mh)) +
      geom_histogram(alpha = 0.75, colour = "black", fill = palette_gen[7]) +
      scale_x_continuous(
        "proportion of consultations that were MHNSU-related",
        expand = c(0,0), labels = percent) +
      scale_y_continuous("number of site-months", 
        expand = expansion(add = c(0,20))) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank())
    ggsave(paste0(dir_path, "out/03_dist_cons_prop.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    
    # Cases per clinician FTE
    pl <- ggplot(mh3, aes(x = cases_fte)) +
      geom_histogram(alpha = 0.75, colour = "black", fill = palette_gen[15]) +
      scale_x_continuous("FTE clinicians", expand = c(0,0)) +
      scale_y_continuous("number of site-months", 
        expand = expansion(add = c(0,20))) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank())
    ggsave(paste0(dir_path, "out/03_dist_cases_fte.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    
    # Health facility open days
    pl <- ggplot(mh3, aes(x = days_open)) +
      geom_histogram(alpha = 0.75, colour = "black", fill = palette_gen[5]) +
      scale_x_continuous("health facility opening days", expand = c(0,0)) +
      scale_y_continuous("number of site-months", 
        expand = expansion(add = c(0,20))) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank())
    ggsave(paste0(dir_path, "out/03_dist_days_open.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    
    # Psychiatrist density
    pl <- ggplot(mh3, aes(x = n_psych)) +
      geom_histogram(alpha = 0.75, colour = "black", fill = palette_gen[1]) +
      scale_x_continuous("psychiatrists per 100,000 population", 
        expand = c(0,0)) +
      scale_y_continuous("number of site-months", 
        expand = expansion(add = c(0,20))) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank())
    ggsave(paste0(dir_path, "out/03_dist_n_psych.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
  
    # Human Development Index
    pl <- ggplot(mh3, aes(x = hdi)) +
      geom_histogram(alpha = 0.75, colour = "black", fill = palette_gen[12]) +
      scale_x_continuous("Human Development Index of host country (2023)", 
        expand = c(0,0)) +
      scale_y_continuous("number of site-months", 
        expand = expansion(add = c(0,20))) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank())
    ggsave(paste0(dir_path, "out/03_dist_hdi.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    
    
    # Proportion of the population aged 18y+
    pl <- ggplot(mh3, aes(x = prop_age18plus)) +
      geom_histogram(alpha = 0.75, colour = "black", fill = palette_gen[7]) +
      scale_x_continuous("proportion of the population aged >=18 yo", 
        expand = c(0,0)) +
      scale_y_continuous("number of site-months", 
        expand = expansion(add = c(0,20))) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank())
    ggsave(paste0(dir_path, "out/03_dist_prop_aged_18plus.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
      
    # Proportion of the population who is female
    pl <- ggplot(mh3, aes(x = prop_f)) +
      geom_histogram(alpha = 0.75, colour = "black", fill = palette_gen[13]) +
      scale_x_continuous("proportion of the population who is female", 
        expand = c(0,0)) +
      scale_y_continuous("number of site-months", 
        expand = expansion(add = c(0,20))) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank())
    ggsave(paste0(dir_path, "out/03_dist_prop_f.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
         
  # #...................................      
  # ## Visualise univariate associations with consultation rate
  # 
  #   # Cases per clinician FTE
  #   pl <- ggplot(mh3, aes(x = cases_fte, y = cons_rate_mh, 
  #     colour = region)) +
  #     geom_point(alpha = 0.75) +
  #     scale_x_continuous("cases  per FTE", expand = expansion(add = 0.2,0), 
  #       trans = "sqrt") +
  #     scale_y_continuous("MHNSU-related consultation rate", 
  #       expand = expansion(add = c(0.2,0)), trans = "sqrt") +
  #     scale_colour_viridis_d() +
  #     theme_bw() +
  #     theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
  #     guides(colour = guide_legend(nrow = 2, reverse = T)) +
  #     geom_smooth(colour = palette_gen[15])
  #   ggsave(paste0(dir_path, "out/03_cons_rate_vs_cases_fte.png"), 
  #     dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))
  #   
  #   # Health facility open days
  #   pl <- ggplot(mh3, aes(x = days_open, y = cons_rate_mh, 
  #     colour = region)) +
  #     geom_point(alpha = 0.75) +
  #     scale_x_continuous("health facility opening days", 
  #       expand = expansion(add = 0.2,0), trans = "sqrt") +
  #     scale_y_continuous("MHNSU-related consultation rate", 
  #       expand = expansion(add = c(0.2,0)), trans = "sqrt") +
  #     scale_colour_viridis_d() +
  #     theme_bw() +
  #     theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
  #     guides(colour = guide_legend(nrow = 2, reverse = T)) +
  #     geom_smooth(colour = palette_gen[5])
  #   ggsave(paste0(dir_path, "out/03_cons_rate_vs_days_open.png"), 
  #     dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))
  # 
  #   # Psychiatrist density
  #   df <- aggregate(mh3[, c("n_cases", "pop_2425")], 
  #     by = mh3[, c("region", "country", "country_iso")], FUN = sum)
  #   df$cons_rate_mh <- df$n_cases * 1200 / df$pop_2425
  #   df <- merge(df, who[, c("country_iso", "n_psych")], by = "country_iso",
  #     all.x = T)
  #   pl <- ggplot(df, aes(x = n_psych, y = cons_rate_mh, 
  #     colour = region)) +
  #     geom_point(alpha = 0.75) +
  #     scale_x_continuous("psychiatrists per 100,000 population", 
  #       expand = expansion(add = 0.2,0), trans = "sqrt") +
  #     scale_y_continuous("MHNSU-related consultation rate", 
  #       expand = expansion(add = c(0.2,0)), trans = "sqrt") +
  #     scale_colour_viridis_d() +
  #     theme_bw() +
  #     theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
  #     guides(colour = guide_legend(nrow = 2, reverse = T)) +
  #     geom_smooth(colour = palette_gen[1])
  #   ggsave(paste0(dir_path, "out/03_cons_rate_vs_n_psych.png"), 
  #     dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))
  # 
  #   # Human Development Index    
  #   pl <- ggplot(mh3, aes(x = hdi, y = cons_rate_mh, 
  #     colour = region)) +
  #     geom_point(alpha = 0.75) +
  #     scale_x_continuous("Human Development Index of host country (2023)", 
  #       expand = expansion(add = 0.2,0), trans = "sqrt") +
  #     scale_y_continuous("MHNSU-related consultation rate", 
  #       expand = expansion(add = c(0.2,0)), trans = "sqrt") +
  #     scale_colour_viridis_d() +
  #     theme_bw() +
  #     theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
  #     guides(colour = guide_legend(nrow = 2, reverse = T)) +
  #     geom_smooth(colour = palette_gen[5])
  #   ggsave(paste0(dir_path, "out/03_cons_rate_vs_hdi.png"), 
  #     dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))
  # 
       
  #...................................      
  ## Visualise univariate associations with consultation proportion

    # Cases per clinician FTE
    pl <- ggplot(mh3, aes(x = cases_fte, y = cons_prop_mh, 
      colour = region)) +
      geom_point(alpha = 0.75) +
      scale_x_continuous("clinician FTEs", expand = expansion(add = 0.2,0), 
        trans = "sqrt") +
      scale_y_continuous(
        "proportion of consultations that were MHNSU-related", 
        expand = expansion(add = c(0.02,0)), trans = "sqrt", labels = percent) +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 2, reverse = T)) +
      geom_smooth(colour = palette_gen[15])
    ggsave(paste0(dir_path, "out/03_cons_prop_vs_cases_fte.png"), 
      dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))
    
    # Health facility open days
    pl <- ggplot(mh3, aes(x = days_open, y = cons_prop_mh, 
      colour = region)) +
      geom_point(alpha = 0.75) +
      scale_x_continuous("health facility opening days", 
        expand = expansion(add = 0.2,0), trans = "sqrt") +
      scale_y_continuous(
        "proportion of consultations that were MHNSU-related", 
        expand = expansion(add = c(0.02,0)), trans = "sqrt", labels = percent) +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 2, reverse = T)) +
      geom_smooth(colour = palette_gen[5])
    ggsave(paste0(dir_path, "out/03_cons_prop_vs_days_open.png"), 
      dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))

    # Psychiatrist density
    df <- aggregate(mh3[, c("n_cases", "n_cases_all")], 
      by = mh3[, c("region", "country", "country_iso")], FUN = sum)
    df$cons_prop_mh <- df$n_cases / df$n_cases_all
    df <- merge(df, who[, c("country_iso", "n_psych")], by = "country_iso",
      all.x = T)
    pl <- ggplot(df, aes(x = n_psych, y = cons_prop_mh, 
      colour = region)) +
      geom_point(alpha = 0.75) +
      scale_x_continuous("psychiatrists per 100,000 population", 
        expand = expansion(add = 0.2,0), trans = "sqrt") +
      scale_y_continuous(
        "proportion of consultations that were MHNSU-related", 
        expand = expansion(add = c(0.01,0)), trans = "sqrt") +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 2, reverse = T)) +
      geom_smooth(colour = palette_gen[1])
    ggsave(paste0(dir_path, "out/03_cons_prop_vs_n_psych.png"), 
      dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))
    
    # Human Development Index    
    pl <- ggplot(mh3, aes(x = hdi, y = cons_prop_mh, 
      colour = region)) +
      geom_point(alpha = 0.75) +
      scale_x_continuous("Human Development Index of host country (2023)", 
        expand = expansion(add = 0.2,0), trans = "sqrt") +
      scale_y_continuous(
        "proportion of consultations that were MHNSU-related", 
        expand = expansion(add = c(0.02,0)), trans = "sqrt", labels = percent) +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 2, reverse = T)) +
      geom_smooth(colour = palette_gen[5])
    ggsave(paste0(dir_path, "out/03_cons_prop_vs_hdi.png"), 
      dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))

    
  #...................................      
  ## Fit multivariate models
    
    # Categorise and rescale predictors
    mh3$cases_fte_cat <- cut(mh3$cases_fte, c(0, 50, 100, 150, 200, 10000),
      labels = c("<50", "50 to 99", "100 to 149", "150 to 199", ">= 200"),
      include.lowest = T, right = F)
    table(mh3$cases_fte_cat, useNA = "always")
    mh3$prop_age18plus_cat <- cut(mh3$prop_age18plus, c(0, 0.4, 0.5, 0.6, 1),
      labels = c("<40%", "40% to 49%", "50% to 59%", ">= 60%"),
      include.lowest = T, right = F)
    table(mh3$prop_age18plus_cat, useNA = "always")
    mh3$prop_f_cat <- cut(mh3$prop_f, c(0, 0.5, 0.55, 1),
      labels = c("<50%", "50% to 54%", ">= 55%"),
      include.lowest = T, right = F)
    table(mh3$prop_f_cat, useNA = "always")
    mh3$hdi_cat <- cut(mh3$hdi, c(0, 0.55, 0.7, 0.8, 1),
      labels = c("low", "medium", "high", "very high"),
      include.lowest = T, right = F)
    table(mh3$hdi_cat, useNA = "always")
    mh3$days_open_sc <- scale(mh3$days_open, center = F, scale = T)
    mh3$country <- as.character(mh3$country)
    mh3$site <- as.character(mh3$site)
    
    # # Fit model of consultation rate
    #   
    #   # GLMM
    #   mcr <- glmmTMB(n_cases ~ cases_fte_cat + days_open_sc + prop_f +
    #       prop_age18plus + (1  | country/site), offset = log(pop_2425), 
    #     data = mh3, family = "nbinom1", ziformula = ~0)
    #   summary(mcr)
    #   DHARMa::plotQQunif(mcr)
    #   DHARMa::plotResiduals(mcr)
    #   
    #   # # GLMM - zero-inflated (does not converge)
    #   # mcr <- glmmTMB(n_cases ~ cases_fte_cat + days_open_sc + prop_f +
    #   #     prop_age18plus + (1  | country/site), offset = log(pop_2425), 
    #   #   data = mh3, family = "nbinom1", ziformula = ~1)
    #   # summary(mcr)
    #   # DHARMa::plotQQunif(mcr)
    #   # DHARMa::plotResiduals(mcr)
    # 
    #   # Extract model output
    #   x <- parameters::model_parameters(mcr, exponentiate = T)
    #   write.csv(x, paste0(dir_path, "out/03_mcr.csv"), row.names = F)
    #   
    # Fit model of consultation proportion
      
      # Crude
      mcp <- glmmTMB(cbind(n_cases, n_cases_not_mh) ~ cases_fte_cat +
        (1  | country/site), 
        data = mh3, family = "binomial")
      summary(mcp)
      DHARMa::plotQQunif(mcp)
      DHARMa::plotResiduals(mcp)
      x <- parameters::model_parameters(mcp, exponentiate = T)
      write.csv(x, paste0(dir_path, "out/03_mcp_crude.csv"), row.names = F)
      
      # Adjusted
      mcp <- glmmTMB(cbind(n_cases, n_cases_not_mh) ~ cases_fte_cat +
          days_open_sc + prop_f + prop_age18plus_cat + (1  | country/site), 
        data = mh3, family = "binomial")
      summary(mcp)
      DHARMa::plotQQunif(mcp)
      DHARMa::plotResiduals(mcp)
      x <- parameters::model_parameters(mcp, exponentiate = T)
      write.csv(x, paste0(dir_path, "out/03_mcp_adjusted.csv"), row.names = F)
      
      # Number of cases retained
      df <- mh3[complete.cases(mh3[, c("n_cases", "n_cases_not_mh",
        "cases_fte_cat", "days_open_sc", "prop_f", "prop_age18plus_cat",
        "country", "site")]), ]
      sum(df$n_cases)
      
#...............................................................................
### ### Association of cases/clinician with cause of MH consultations
#...............................................................................

  #...................................      
  ## Prepare dataset
    
    # Reduce dataset to non-missing observations
    df <- subset(mh1, pt_type == "refugee" & ! is.na(n_cases) & 
        ! is.na(prop_agesex) & days_open_implausible == "plausible" &
        fte_clinicians_implausible == "plausible" & ! is.na(pop_2425))

    # Aggregate by broad MH category
    x <- unique(df[, c("region", "country", "country_iso", "site", "mmyy",
      "fte_clinicians", "days_open", "pop_2425")])
    mh4 <- aggregate(list(n_cases = df$n_cases), 
      by = df[, c("country", "site", "mmyy", "cat2")], FUN = sum, na.rm = T)
    mh4 <- merge(mh4, x, by = c("country", "site", "mmyy"), all.x = T)

    # Add age-sex proportions as two single variables
        # proportion aged 18+ and proportion female
    df <- agesex_dist
    df$prop_age18plus <- ifelse(df$age %in% c("0 to 4yrs", "5 to 17yrs"),
      0, df$prop_agesex)
    df$prop_f <- ifelse(df$sex == "male", 0, df$prop_agesex)
    df <- aggregate(df[, c("prop_age18plus", "prop_f")], 
      by = list(site = df$site), FUN = sum, na.rm = T)
    mh4 <- merge(mh4, df, by = "site", all.x = T)

    # Add total all-cause consultations
    df <- subset(mh2b, pt_type == "refugee" & ! is.na(n_cases) & 
        ! is.na(n_cases_all) & days_open_implausible == "plausible" &
        fte_clinicians_implausible == "plausible" & ! is.na(pop_2425))
    df <- aggregate(list(n_cases_all = df$n_cases_all), 
      by = df[, c("country", "site", "mmyy")], FUN = mean, na.rm = T)
    mh4 <- merge(mh4, df, by = c("country", "site", "mmyy"), all.x = T)
    
    # Compute annual consultation rate
    mh4$cons_rate_mh <- mh4$n_cases * 1200 / mh4$pop_2425
    
    # Add WHO data
    x <- who[, c("country_iso", "n_psych")]
    mh4 <- merge(mh4, x, by = "country_iso", all.x = T)
    # mh4 <- subset(mh4, ! is.na(n_psych))
    
    # Add HDI data
    mh4 <- merge(mh4, hdi, by = "country", all.x = T)
    mh4 <- subset(mh4, ! is.na(hdi))

    # Calculate daily consultations (all causes) per clinician FTE
    mh4$cases_fte <- mh4$n_cases_all / (mh4$fte_clinicians * mh4$days_open)
    
    # Categorise and rescale predictors
    mh4$cases_fte_cat <- cut(mh4$cases_fte, c(0, 50, 100, 150, 200, 10000),
      labels = c("<50", "50 to 99", "100 to 149", "150 to 199", ">= 200"),
      include.lowest = T, right = F)
    table(mh4$cases_fte_cat, useNA = "always")
    mh4$prop_age18plus_cat <- cut(mh4$prop_age18plus, c(0, 0.4, 0.5, 0.6, 1),
      labels = c("<40%", "40% to 49%", "50% to 59%", ">= 60%"),
      include.lowest = T, right = F)
    table(mh4$prop_age18plus_cat, useNA = "always")
    mh4$prop_f_cat <- cut(mh4$prop_f, c(0, 0.5, 0.55, 1),
      labels = c("<50%", "50% to 54%", ">= 55%"),
      include.lowest = T, right = F)
    table(mh4$prop_f_cat, useNA = "always")
    mh4$hdi_cat <- cut(mh4$hdi, c(0, 0.55, 0.7, 0.8, 1),
      labels = c("low", "medium", "high", "very high"),
      include.lowest = T, right = F)
    table(mh4$hdi_cat, useNA = "always")
    mh4$n_psych_cat <- cut(mh4$n_psych, c(0, 0.01, 0.1, 10),
      labels = c("< 0.1 per 100,000", "0.01 to 0.09 per 100,000", 
        ">= 0.1 per 100,000"),
      include.lowest = T, right = F)
    table(mh4$n_psych_cat, useNA = "always")
    mh4$cons_rate_mh_cat <- cut(mh4$cons_rate_mh, breaks = c(0, 0.1, 1, 10000),
      labels = c("<0.1", "0.1 to 0.9", ">= 1.0"),
      include.lowest = T, right = F)
    table(mh4$cons_rate_mh_cat, useNA = "always")
    mh4$days_open_sc <- scale(mh4$days_open, center = F, scale = T)
    mh4$country <- as.character(mh4$country)
    mh4$site <- as.character(mh4$site)
    mh4$cat2 <- factor(mh4$cat2)
    mh4$cat2 <- relevel(mh4$cat2, "Epilepsy or seizures")
    

  #...................................      
  ## Visualise univariate correlation between predictors and category share

    # Cases per clinician FTE
    df <- mh4
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("cases_fte_cat", "cat2")], FUN = sum)
    pl <- ggplot(df, aes(x = cases_fte_cat, y = n_cases,
      fill = cat2)) +
      geom_bar(stat = "identity", position = "fill", colour = "black",
        linewidth = 0.5) +
      theme_bw() +
      scale_x_discrete("Daily consultations per clinician FTE",
        expand = c(0,0)) +
      scale_y_continuous("proportion of all MHNSU-related consultations",
        labels = percent, breaks = seq(0, 1, 0.2), expand = c(0,0)) +
      scale_fill_viridis_d("") +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 3))      
    ggsave(paste0(dir_path, "out/03_cat2_share_vs_cases_fte.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    
    # Human Development Index
    df <- mh4
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("hdi_cat", "cat2")], FUN = sum)
    pl <- ggplot(df, aes(x = hdi_cat, y = n_cases,
      fill = cat2)) +
      geom_bar(stat = "identity", position = "fill", colour = "black",
        linewidth = 0.5) +
      theme_bw() +
      scale_x_discrete("Human Development Index of host country (2023)",
        expand = c(0,0)) +
      scale_y_continuous("proportion of all MHNSU-related consultations",
        labels = percent, breaks = seq(0, 1, 0.2), expand = c(0,0)) +
      scale_fill_viridis_d("") +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 3))      
    ggsave(paste0(dir_path, "out/03_cat2_share_vs_hdi.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    
    # Proportion of the population aged >= 18yo
    df <- mh4
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("prop_age18plus_cat", "cat2")], FUN = sum)
    pl <- ggplot(df, aes(x = prop_age18plus_cat, y = n_cases,
      fill = cat2)) +
      geom_bar(stat = "identity", position = "fill", colour = "black",
        linewidth = 0.5) +
      theme_bw() +
      scale_x_discrete("Proportion of the population aged >= 18 yo",
        expand = c(0,0)) +
      scale_y_continuous("proportion of all MHNSU-related consultations",
        labels = percent, breaks = seq(0, 1, 0.2), expand = c(0,0)) +
      scale_fill_viridis_d("") +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 3))      
    ggsave(paste0(dir_path, "out/03_cat2_share_vs_prop_age18plus.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))

    # Proportion of the population who is female
    df <- mh4
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("prop_f_cat", "cat2")], FUN = sum)
    pl <- ggplot(df, aes(x = prop_f_cat, y = n_cases,
      fill = cat2)) +
      geom_bar(stat = "identity", position = "fill", colour = "black",
        linewidth = 0.5) +
      theme_bw() +
      scale_x_discrete("Proportion of the population who is female",
        expand = c(0,0)) +
      scale_y_continuous("proportion of all MHNSU-related consultations",
        labels = percent, breaks = seq(0, 1, 0.2), expand = c(0,0)) +
      scale_fill_viridis_d("") +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 3))      
    ggsave(paste0(dir_path, "out/03_cat2_share_vs_prop_f.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))

    # Number of psychiatrists in country
    df <- mh4
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("n_psych_cat", "cat2")], FUN = sum)
    pl <- ggplot(df, aes(x = n_psych_cat, y = n_cases,
      fill = cat2)) +
      geom_bar(stat = "identity", position = "fill", colour = "black",
        linewidth = 0.5) +
      theme_bw() +
      scale_x_discrete(
        "Number of psychiatrists per 100,000 population (host country, 2014)",
        expand = c(0,0)) +
      scale_y_continuous("proportion of all MHNSU-related consultations",
        labels = percent, breaks = seq(0, 1, 0.2), expand = c(0,0)) +
      scale_fill_viridis_d("") +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 3))      
    ggsave(paste0(dir_path, "out/03_cat2_share_vs_n_psych.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    
    # MHNSU-related consultation rate
    df <- mh4 
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("cons_rate_mh_cat", "cat2")], FUN = sum)    
    pl <- ggplot(df, aes(x = cons_rate_mh_cat, y = n_cases,
      fill = cat2)) +
      geom_bar(stat = "identity", position = "fill", colour = "black",
        linewidth = 0.5) +
      theme_bw() +
      scale_x_discrete("MHNSU-related consultation rate per 100 person-years",
        expand = c(0,0)) +
      scale_y_continuous("proportion of all MHNSU-related consultations",
        labels = percent, breaks = seq(0, 1, 0.2), expand = c(0,0)) +
      scale_fill_viridis_d("") +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 3))      
    ggsave(paste0(dir_path, "out/03_cat2_share_vs_cons_rate.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    
  #...................................      
  ## Fit multivariate model
    
    # Crude association (method of weights - checked, OK!)
        # nested random effects require too much computational power, so
        # just kept country
    mcp <- mblogit(cat2 ~ cases_fte_cat, data = mh4, 
      weights = n_cases, random = ~1|country)
    x <- mtable(mcp, coef.style = "horizontal", summary.stats = 
        c("N", "AIC", "Deviance"))
    x$mcp$coef[,1,] <- exp(x$mcp$coef[,1,])
    write_html(x, paste0(dir_path, "out/03_mblogit_cat2_crude.html"))

    # Adjusted association (method of weights - checked, OK!)
    mcp <- mblogit(cat2 ~ cases_fte_cat + days_open_sc + prop_age18plus_cat +
      prop_f_cat + cons_rate_mh, data = mh4, weights = n_cases, 
      random = ~1|country)
    x <- mtable(mcp, coef.style = "horizontal", summary.stats = 
        c("N", "AIC", "Deviance"))
    x$mcp$coef[,1,] <- exp(x$mcp$coef[,1,])
    write_html(x, paste0(dir_path, "out/03_mblogit_cat2_adjusted.html"))
    
    # Number of cases retained
    df <- mh4[complete.cases(mh4[, c("n_cases", "cat2",
      "cases_fte_cat", "days_open_sc", "prop_f_cat", "prop_age18plus_cat",
      "country", "cons_rate_mh")]), ]
    sum(df$n_cases)

    
  # #...................................      
  # ## Draw conceptual diagram for discussion
  #   
  #   # Prepare dataset
  #   df <- data.frame(
  #     setting = rep(c("pre-displacement", "displacement",
  #     "seen at outpatient level"), 2),
  #     cause = c(rep("depression", 3), rep("all other causes", 3)),
  #     cases = c(16, 31, 3, 11, 17, 8)
  #   )
  #   df$setting <- factor(df$setting, levels = c("pre-displacement", 
  #     "displacement", "seen at outpatient level"))
  #   df$cause <- factor(df$cause, levels = c("depression", "all other causes"))
  #   
  #   # Plot
  #   ggplot(df, aes(x = setting, y = cases, fill = cause)) +
  #     geom_flow(aes(alluvium = cause), curve_type = "linear",
  #       alpha = 0.25, width = 0.5, colour = "black") +
  #     geom_col(width = 0.5, colour = "black", alpha = 0.75) +
  #     theme_bw() +
  #     theme(legend.position = "bottom", panel.grid.major.x = element_blank()) +
  #     scale_fill_manual(values = palette_gen[c(14, 7)]) +
  #     scale_x_discrete(expand = expansion(add = c(0.4,0.4))) +
  #     scale_y_continuous("cases per 100 population", 
  #       expand = expansion(add = c(0, 5)))
  #   ggsave(paste0(dir_path, "out/03_conceptual_diagram.png"), 
  #     dpi = "print", units = "cm", width = 17, height = 7*(hw-0.05))

    
#...............................................................................  
### ENDS
#...............................................................................
            