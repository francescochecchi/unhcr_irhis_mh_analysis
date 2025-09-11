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
    
    # Compute consultation rate
    mh3$cons_rate_mh <- mh3$n_cases * 1200 / mh3$pop_0405
    
    # Add WHO data
    x <- who[, c("country_iso", "n_psych")]
    mh3 <- merge(mh3, x, by = "country_iso", all.x = T)
    mh3 <- subset(mh3, ! is.na(n_psych))

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
    mh3$n_cases_not_mh <- mh3$n_cases_all - mh3$n_cases
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
      dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))
    
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
      dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))

    # Psychiatrist density
    df <- aggregate(mh3[, c("n_cases", "pop_0405")], 
      by = mh3[, c("region", "country", "country_iso")], FUN = sum)
    df$cons_rate_mh <- df$n_cases * 1200 / df$pop_0405
    df <- merge(df, who[, c("country_iso", "n_psych")], by = "country_iso",
      all.x = T)
    pl <- ggplot(df, aes(x = n_psych, y = cons_rate_mh, 
      colour = region)) +
      geom_point(alpha = 0.75) +
      scale_x_continuous("psychiatrists per 100,000 population", 
        expand = expansion(add = 0.2,0), trans = "sqrt") +
      scale_y_continuous("mental health-related consultation rate", 
        expand = expansion(add = c(0.2,0)), trans = "sqrt") +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 2, reverse = T)) +
      geom_smooth(colour = palette_gen[1])
    ggsave(paste0(dir_path, "out/03_cons_rate_vs_n_psych.png"), 
      dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))
    
    
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
      dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))
    
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
        "proportion of consultations that were mental health-related", 
        expand = expansion(add = c(0.01,0)), trans = "sqrt") +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), legend.position = "bottom") +
      guides(colour = guide_legend(nrow = 2, reverse = T)) +
      geom_smooth(colour = palette_gen[1])
    ggsave(paste0(dir_path, "out/03_cons_prop_vs_n_psych.png"), 
      dpi = "print", units = "cm", width = 15, height = 10*(hw-0.05))
    
        
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
      # mcr <- mgcv::bam(n_cases ~ s(fte_clinicians) + days_open + s(n_psych) +
      #     s(site, bs = "re"), offset = log(pop_0405), data = mh3,
      #   family = "nb")
      # summary(mcr)
      # mgcv::gam.check(mcr)
    
      # GLMM
      mcr <- glmmTMB(n_cases ~ fte_clinicians_cat + days_open_sc + n_psych +
        (1  | country/site), offset = log(pop_0405), data = mh3,
        family = "nbinom1", ziformula = ~0)
      summary(mcr)
      DHARMa::plotQQunif(mcr)
      DHARMa::plotResiduals(mcr)
      
      # # GLMM - zero-inflated (fits better but violates assumptions)
      # mcr <- glmmTMB(n_cases ~ fte_clinicians_cat + days_open_sc + n_psych +
      #   (1  | country/site), offset = log(pop_0405), data = mh3,
      #   family = "nbinom1", ziformula = ~1)
      # summary(mcr)
      # DHARMa::plotQQunif(mcr)
      # DHARMa::plotResiduals(mcr)
 
      # Extract model output
      x <- parameters::model_parameters(mcr, exponentiate = T)
      write.csv(x, paste0(dir_path, "out/03_mcr.csv"), row.names = F)
      
    # Fit model of consultation proportion
      
      # GLMM
      mcp <- glmmTMB(cbind(n_cases, n_cases_not_mh) ~ fte_clinicians_cat + 
          days_open_sc + n_psych + (1  | country/site), data = mh3,
        family = "binomial")
      summary(mcp)
      DHARMa::plotQQunif(mcp)
      DHARMa::plotResiduals(mcp)

      # Extract model output
      x <- parameters::model_parameters(mcp, exponentiate = T)
      write.csv(x, paste0(dir_path, "out/03_mcp.csv"), row.names = F)
      
      
#...............................................................................
### Exploring factors associated with cause of MH consultations (refugees only)
#...............................................................................

  #...................................      
  ## Prepare dataset
    
    # Prepare dataset
    mh4 <- subset(mh2, pt_type == "refugee")  
    mh4 <- mh4[, c("country", "country_iso", "site", "mmyy", "age", "sex", 
      "pop_0405", "prop_agesex", "cat2", "n_cases", "fte_clinicians", 
      "days_open")]  
    mh4$pop <- mh4$pop_0405 * mh4$prop_agesex
    mh4 <- merge(mh4, who[, c("country_iso", "n_psych")], by = "country_iso",
      all.x = T)
    x <- aggregate(list(n_cases_tot = mh4$n_cases), 
      by = mh4[, c("country", "site", "mmyy", "age", "sex")], FUN = sum)
    mh4 <- merge(mh4, x, by = c("country", "site", "mmyy", "age", "sex"), 
      all.x = T)
    mh4 <- merge(mh4, mh3[, c("country", "site", "mmyy", "cons_rate_mh")],
      by = c("country", "site", "mmyy"), all.x = T)  
      
      # for consultation rate
      x <- c("country", "site", "age", "sex", "cat2", "n_cases", "pop", 
        "fte_clinicians", "days_open", "n_psych", "cons_rate_mh")
      mh4r <- mh4[complete.cases(mh4[, x]), ]

      # for proportion of consultations
      x <- c("country", "site", "age", "sex", "cat2", "n_cases", "n_cases_tot", 
        "fte_clinicians", "days_open", "n_psych", "cons_rate_mh")
      mh4p <- mh4[complete.cases(mh4[, x]), ]

  #...................................      
  ## Visualise correlation between consultation rate and category share
    
    # Prepare dataset
    df <- mh4r  
    df$cons_rate_mh_cat <- cut(df$cons_rate_mh, breaks = c(0,5,10,15,20,1000),
      include.lowest = T, right = F, labels = c("<5.0", "5.0 to 9.9",
        "10.0 to 14.9", "15.0 to 19.9", ">=20.0"))
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("cons_rate_mh_cat", "cat2")], FUN = sum)
                  
    # Plot
    pl <- ggplot(df, aes(x = cons_rate_mh_cat, y = n_cases,
      fill = cat2)) +
      geom_bar(stat = "identity", position = "fill", colour = "black",
        linewidth = 0.5) +
      theme_bw() +
      scale_x_discrete("MH-related consultation rate per 100 person-years",
        expand = c(0,0)) +
      scale_y_continuous("proportion of all MH-related consultations",
        labels = percent, breaks = seq(0, 1, 0.2), expand = c(0,0)) +
      scale_fill_viridis_d("") +
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(nrow = 3))      
    ggsave(paste0(dir_path, "out/03_cat2_share_vs_cons_rate.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
    
      
  #...................................      
  ## Fit multivariate exploratory model
    
    # Categorise and rescale predictors
    mh4p$fte_clinicians_cat <- cut(mh4p$fte_clinicians, c(0, 2, 5, 10, 100),
      labels = c("<2.0", "2.0 to 4.9", "5.0 to 9.9", ">= 10.0"),
      include.lowest = T, right = F)
    table(mh4p$fte_clinicians_cat)
    mh4p$cons_rate_mh_cat <- cut(mh4p$cons_rate_mh, 
      breaks = c(0,5,10,15,20,1000),
      include.lowest = T, right = F, labels = c("<5.0", "5.0 to 9.9",
        "10.0 to 14.9", "15.0 to 19.9", ">=20.0"))
    table(mh4p$cons_rate_mh_cat)
    mh4p$days_open_sc <- scale(mh4p$days_open, center = F, scale = T)
    mh4p$country <- as.character(mh4p$country)
    mh4p$site <- as.character(mh4p$site)
    mh4p$cat2 <- factor(mh4p$cat2)
    mh4p$cat2 <- relevel(mh4p$cat2, "Epilepsy or seizures")
    
    # Fit model of consultation rate  
      # [TO DO LATER]
      
    # Fit model of consultation proportion (method of weights - checked, OK!)
        # nested random effects require too much computational power, so
        # just kept country
    mcp <- mblogit(cat2 ~ fte_clinicians_cat + days_open_sc + n_psych +
        cons_rate_mh_cat, 
      data = mh4p, weights = n_cases, random = ~1|country)
    x <- mtable(mcp, coef.style = "horizontal", summary.stats = 
        c("N", "AIC", "Deviance"))
    x$mcp$coef[,1,] <- exp(x$mcp$coef[,1,])
    write_html(x, paste0(dir_path, "out/03_mblogit_cat2.html"))


  #...................................      
  ## Draw conceptual diagram for discussion
    
    # Prepare dataset
    df <- data.frame(
      setting = rep(c("pre-displacement", "displacement",
      "seen at outpatient level"), 2),
      cause = c(rep("depression", 3), rep("all other causes", 3)),
      cases = c(16, 31, 4, 33, 44, 13)
    )
    df$setting <- factor(df$setting, levels = c("pre-displacement", 
      "displacement", "seen at outpatient level"))
    df$cause <- factor(df$cause, levels = c("depression", "all other causes"))
    
    # Plot
    ggplot(df, aes(x = setting, y = cases, fill = cause)) +
      geom_flow(aes(alluvium = cause), curve_type = "linear",
        alpha = 0.25, width = 0.5, colour = "black") +
      geom_col(width = 0.5, colour = "black", alpha = 0.75) +
      theme_bw() +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = palette_gen[c(14, 7)]) +
      scale_x_discrete(expand = expansion(add = c(0.4,0.4))) +
      scale_y_continuous("cases per 100 population", 
        expand = expansion(add = c(0, 5)))
    ggsave(paste0(dir_path, "out/03_conceptual_diagram.png"), 
      dpi = "print", units = "cm", width = 15, height = 6*(hw-0.05))

#...............................................................................  
### ENDS
#...............................................................................
            