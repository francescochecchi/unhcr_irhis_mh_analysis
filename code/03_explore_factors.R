#...............................................................................
### +++++++ ANALYSIS OF UNHCR iRHIS MENTAL HEALTH CONSULTATION DATA ++++++++ ###
#...............................................................................

#...............................................................................
## -------- R SCRIPT TO EXPLORE FACTORS ASSOCIATED WITH DATA PATTERNS ------- ##
#...............................................................................



#...............................................................................
### Exploring factors associated with rate / proportion of MH consultations
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

  #...................................      
  ## Visualise variable distributions

    # MH consultation rate
    pl <- ggplot(mh3, aes(x = cons_rate_mh)) +
      geom_histogram(alpha = 0.75, colour = "black", fill = palette_gen[10]) +
      scale_x_continuous("mental health-related consultation rate",
        trans = "sqrt", limits = c(NA, 100), breaks = c(0, 2, 5, 10, 20, 40, 
          60, 80, 100), expand = c(0,0)) +
      scale_y_continuous("number of site-months", 
        expand = expansion(add = c(0,20))) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank())
    ggsave(paste0(dir_path, "out/03_dist_cons_rate.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    
      # remove outlier consultation rate values (n = 11 with value > 100)
      mh3 <- subset(mh3, cons_rate_mh < 100)

    # MH consultations proportion
    mh3$cons_prop_mh <- ifelse(mh3$n_cases_all > 0, 
      mh3$n_cases / mh3$n_cases_all, NA)  
    pl <- ggplot(mh3, aes(x = cons_prop_mh)) +
      geom_histogram(alpha = 0.75, colour = "black", fill = palette_gen[7]) +
      scale_x_continuous(
        "proportion of consultations that were mental health-related",
        expand = c(0,0), labels = percent) +
      scale_y_continuous("number of site-months", 
        expand = expansion(add = c(0,20))) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank())
    ggsave(paste0(dir_path, "out/03_dist_cons_prop.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    
    # Clinician FTEs
    pl <- ggplot(mh3, aes(x = fte_clinicians)) +
      geom_histogram(alpha = 0.75, colour = "black", fill = palette_gen[15]) +
      scale_x_continuous("FTE clinicians", expand = c(0,0)) +
      scale_y_continuous("number of site-months", 
        expand = expansion(add = c(0,20))) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank())
    ggsave(paste0(dir_path, "out/03_dist_fte_clinicians.png"), 
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
        
  #...................................      
  ## Visualise univariate associations with consultation rate

    # Clinician FTEs
    pl <- ggplot(mh3, aes(x = fte_clinicians, y = cons_rate_mh, 
      colour = region)) +
      geom_point(alpha = 0.75) +
      scale_x_continuous("clinician FTEs", expand = expansion(add = 0.2,0), 
        trans = "sqrt") +
      scale_y_continuous("mental health-related consultation rate", 
        expand = expansion(add = c(0.2,0)), trans = "sqrt") +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 2, reverse = T)) +
      geom_smooth(colour = palette_gen[15])
    ggsave(paste0(dir_path, "out/03_cons_rate_vs_fte_clinicians.png"), 
      dpi = "print", units = "cm", width = 15, height = 15*(hw-0.05))
    
    # Health facility open days
    pl <- ggplot(mh3, aes(x = days_open, y = cons_rate_mh, 
      colour = region)) +
      geom_point(alpha = 0.75) +
      scale_x_continuous("health facility opening days", 
        expand = expansion(add = 0.2,0), trans = "sqrt") +
      scale_y_continuous("mental health-related consultation rate", 
        expand = expansion(add = c(0.2,0)), trans = "sqrt") +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 2, reverse = T)) +
      geom_smooth(colour = palette_gen[5])
    ggsave(paste0(dir_path, "out/03_cons_rate_vs_days_open.png"), 
      dpi = "print", units = "cm", width = 15, height = 15*(hw-0.05))

  #...................................      
  ## Visualise univariate associations with consultation proportion

    # Clinician FTEs
    pl <- ggplot(mh3, aes(x = fte_clinicians, y = cons_prop_mh, 
      colour = region)) +
      geom_point(alpha = 0.75) +
      scale_x_continuous("clinician FTEs", expand = expansion(add = 0.2,0), 
        trans = "sqrt") +
      scale_y_continuous(
        "proportion of consultations that were mental health-related", 
        expand = expansion(add = c(0.02,0)), trans = "sqrt", labels = percent) +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 2, reverse = T)) +
      geom_smooth(colour = palette_gen[15])
    ggsave(paste0(dir_path, "out/03_cons_prop_vs_fte_clinicians.png"), 
      dpi = "print", units = "cm", width = 15, height = 15*(hw-0.05))
    
    # Health facility open days
    pl <- ggplot(mh3, aes(x = days_open, y = cons_prop_mh, 
      colour = region)) +
      geom_point(alpha = 0.75) +
      scale_x_continuous("health facility opening days", 
        expand = expansion(add = 0.2,0), trans = "sqrt") +
      scale_y_continuous(
        "proportion of consultations that were mental health-related", 
        expand = expansion(add = c(0.02,0)), trans = "sqrt", labels = percent) +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 2, reverse = T)) +
      geom_smooth(colour = palette_gen[5])
    ggsave(paste0(dir_path, "out/03_cons_prop_vs_days_open.png"), 
      dpi = "print", units = "cm", width = 15, height = 15*(hw-0.05))
    
  #...................................      
  ## Fit multivariate exploratory models
    
    # Categorise and rescale predictors
    mh3$fte_clinicians_cat <- cut(mh3$fte_clinicians, c(0, 2, 5, 10, 100),
      labels = c("<2.0", "2.0 to 4.9", "5.0 to 9.9", ">= 10.0"),
      include.lowest = T, right = F)
    table(mh3$fte_clinicians_cat)
    mh3$days_open_sc <- scale(mh3$days_open, center = F, scale = T)
    mh3$country <- as.character(mh3$country)
    mh3$site <- as.character(mh3$site)
    
    
    # Fit model of consultation rate
      
      # GAMM  
      # mcr <- mgcv::bam(n_cases ~ s(fte_clinicians) + days_open + 
      #     s(site, bs = "re"), offset = log(pop_0405), data = mh3,
      #   family = "nb")
      # summary(mcr)
      # mgcv::gam.check(mcr)
    
      # GLMM
      mcr <- glmmTMB(n_cases ~ fte_clinicians_cat + days_open_sc + 
        (1  | country/site), offset = log(pop_0405), data = mh3,
        family = "nbinom1", ziformula = ~0)
      summary(mcr)
      DHARMa::plotQQunif(mcr)
      DHARMa::plotResiduals(mcr)
      
      # # GLMM - zero-inflated (fits better but violates assumptions)
      # mcr <- glmmTMB(n_cases ~ fte_clinicians_cat + days_open_sc + 
      #   (1  | country/site), offset = log(pop_0405), data = mh3,
      #   family = "nbinom1", ziformula = ~1)
      # summary(mcr)
      # DHARMa::plotQQunif(mcr)
      # DHARMa::plotResiduals(mcr)
            