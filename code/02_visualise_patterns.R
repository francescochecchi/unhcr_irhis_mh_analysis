#...............................................................................
### +++++++ ANALYSIS OF UNHCR iRHIS MENTAL HEALTH CONSULTATION DATA ++++++++ ###
#...............................................................................

#...............................................................................
## ----- R SCRIPT TO VISUALISE PATTERNS IN MENTAL HEALTH CONSULTATIONS  ----- ##
#...............................................................................


#...............................................................................
### Generating summary tables and figures
#...............................................................................

  #...................................      
  ## Visualise flow of MH condition classifications

    # Prepare aggregate dataset for plotting
      # aggregate
      df <- aggregate(list(n_cases = mh$n_cases), 
        by = mh[, c("cause", "cat1", "cat2")], FUN = sum, na.rm = T)
      df$prop_cases <- label_percent(accuracy = 0.1)(df$n_cases/sum(df$n_cases))
      df$cause <- paste0(df$cause, " (", df$prop_cases, ")")
      x <- unique(df[, c("cat2", "cat1", "cause")])

      # fix cause
      x1 <- data.frame(
        cause = unique(x$cause),
        cause_pl = paste(sprintf("%02d",1:length(unique(x$cause))),
          unique(x$cause), sep = ". ")
      )
      x1$cause_pl <- sapply(strwrap(x1$cause_pl, 55, simplify=F), paste, 
        collapse="\n" )    
      df <- merge(df, x1, by = "cause", all.x = T)
  
      # fix cat1  
      x1 <- data.frame(
        cat1 = unique(x$cat1),
        cat1_pl = unique(x$cat1)
      )
      x2 <- aggregate(list(n_cases = df$n_cases), by = list(cat1 = df$cat1), 
        FUN = sum)
      x1 <- merge(x1, x2, by = "cat1", all.x = T)
      x1$cat1_pl <- paste0(x1$cat1_pl, " (", 
        label_percent(accuracy = 0.1)(x1$n_cases / sum(x1$n_cases)), ")")
      x1$cat1_pl <- sapply(strwrap(x1$cat1_pl, 50, simplify=F), paste, 
        collapse="\n" )
      df <- merge(df, x1[, c("cat1", "cat1_pl")], by = "cat1", all.x = T)
    
      # fix cat2    
      x1 <- data.frame(
        cat2 = unique(x$cat2),
        cat2_pl = paste(LETTERS[1:length(unique(x$cat2))], unique(x$cat2), 
          sep = ". ")
      )
      x2 <- aggregate(list(n_cases = df$n_cases), by = list(cat2 = df$cat2), 
        FUN = sum)
      x1 <- merge(x1, x2, by = "cat2", all.x = T)
      x1$cat2_pl <- paste0(x1$cat2_pl, " (", 
        label_percent(accuracy = 0.1)(x1$n_cases / sum(x1$n_cases)), ")")
      x1$cat2_pl <- sapply(strwrap(x1$cat2_pl, 50, simplify=F), paste, 
        collapse="\n" )
      df <- merge(df, x1[, c("cat2", "cat2_pl")], by = "cat2", all.x = T)
      
      # finish preparing  
      df <- df[order(df$cat2, df$cat1_pl, df$cause_pl), ]
      x <- unique(df[, c("cat2_pl", "cat1_pl", "cause_pl")])
      df$cause_pl <- factor(df$cause_pl, levels = unique(x$cause_pl))    
      df$cat1_pl <- factor(df$cat1_pl, levels = unique(x$cat1_pl))    
      df$cat2_pl <- factor(df$cat2_pl, levels = unique(x$cat2_pl))    
      df <- df[order(df$cat2_pl, df$cat1_pl, df$cause_pl), ]
      df$prop_pl <- as.numeric(gsub("%", "", df$prop_cases))/100  + 0.2
    
    # Plot
    pl <- ggplot(data = df, aes(axis1 = cause_pl, axis2 = cat1_pl, 
      axis3 = cat2_pl, y = prop_pl)) +
      geom_stratum(width = 1, aes(fill = cat2), alpha = 0.25, colour="grey20") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
      scale_x_discrete("cause of consultation", 
        limits = c("cause_pl", "cat1_pl", "cat2_pl"), expand = c(0, 0),
        labels = c("iRHIS cause", "ICD code(s)", "broad category")) +
      scale_y_continuous(expand = c(0.0, 0)) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "none", panel.grid.major = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.x = element_text(size = 11,
          colour = "black"))
    ggsave(paste0(dir_path, "out/02_alluvial_causes.png"), dpi = "print",
      units = "cm", width = 30, height = 30*hw)
    
  #...................................      
  ## Tabulate data availability by country, pt_type, age and sex
        # include number of sites and months

    # Prepare dataset
    df <- mh2
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("region", "country", "pt_type", "sex", "age")], 
      FUN = sum, na.rm = T)
    df$agesex <- paste(df$sex, df$age, sep = "_")
    x1 <- subset(df, pt_type == "refugee")
    x1 <- reshape(x1, 
      direction = "wide", timevar = "agesex",
      idvar = c("region", "country"),
      drop = c("age", "sex", "pt_type"))
    colnames(x1) <- gsub("n_cases.", "", colnames(x1))
    x2 <- subset(df, pt_type == "national")
    x2 <- reshape(x2, 
      direction = "wide", timevar = "agesex",
      idvar = c("region", "country"),
      drop = c("age", "sex", "pt_type"))  
    colnames(x2) <- gsub("n_cases.both_", "", colnames(x2))
    df <- merge(x1, x2, by = c("region", "country"), all.x = T)
    df$female <- rowSums(df[, c("female_0 to 4yrs", "female_5 to 17yrs",
      "female_18 to 59yrs", "female_60+ yrs")])
    df$male <- rowSums(df[, c("male_0 to 4yrs", "male_5 to 17yrs",
      "male_18 to 59yrs", "male_60+ yrs")])
    df$tot_ref <- df$female + df$male
    df$tot_nat <- rowSums(df[, c("0 to 4yrs", "5 to 17yrs",
      "18 to 59yrs", "60+ yrs")])
    df <- df[, c("region", "country", 
      c("female_0 to 4yrs", "female_5 to 17yrs", 
        "female_18 to 59yrs", "female_60+ yrs", "female"),
      c("male_0 to 4yrs", "male_5 to 17yrs", 
        "male_18 to 59yrs", "male_60+ yrs", "male"), "tot_ref",
      c("0 to 4yrs", "5 to 17yrs", 
        "18 to 59yrs", "60+ yrs"), "tot_nat"
    )]  
    x1 <- colnames(df)[! colnames(df) %in% c("region", "country")]
    x <- colSums(df[, x1])
    df$region <- as.character(df$region)
    df$country <- as.character(df$country)
    df <- rbind(df, c("all", "all", x))
    for (i in x1) {df[, i] <- as.integer(df[, i])}
    x <- df[which(df$region == "all"), 
      c("female_0 to 4yrs", "female_5 to 17yrs", 
        "female_18 to 59yrs", "female_60+ yrs", "female", "tot_ref")] / 
      df[which(df$region == "all"), "tot_ref"]
    x2 <- apply(x, 2, function(xx) {label_percent(accuracy = 0.1)(xx)})
    x <- df[which(df$region == "all"), 
      c("male_0 to 4yrs", "male_5 to 17yrs", 
        "male_18 to 59yrs", "male_60+ yrs", "male", "tot_ref")] / 
      df[which(df$region == "all"), "tot_ref"]
    x <- apply(x, 2, function(xx) {label_percent(accuracy = 0.1)(xx)})
    x2 <- c(x2, x)
    x <- df[which(df$region == "all"), 
      c("0 to 4yrs", "5 to 17yrs", 
        "18 to 59yrs", "60+ yrs", "tot_nat")] / 
      df[which(df$region == "all"), "tot_nat"]
    x <- apply(x, 2, function(xx) {label_percent(accuracy = 0.1)(xx)})
    x2 <- c(x2, x)
    for (i in x1) {
      df[, i] <- scales::label_comma()(df[, i])
      x <- which(df$region == "all")
      df[x, i] <- paste0(df[x, i], " (", x2[i], ")")
    }
    sites$n_sites <- 1
    x <- aggregate(list(n_sites = sites$n_sites), by = sites[, c("region",
      "country")], FUN = sum, na.rm = T)
    df <- merge(df, x, by = c("region", "country"), all.x = T)
    df$region <- factor(df$region, levels = c(levels(mh2$region), "all"))
    df$country <- factor(df$country, levels = c(levels(mh2$country), "all"))
    df <- df[order(df$region, df$country), c("region", "country", "n_sites",x1)]
    df[which(df$region == "all"), "n_sites"] <- sum(na.omit(df$n_sites))
    
    # Save
    write.csv(df, paste0(dir_path, "out/tab_avail.csv"), row.names = F)    
    
    # How many health facilities?
    hfs <- merge(hfs, unique(mh1[, c("country", "site")]), 
      by = c("country", "site"), all.y = T)
    length(unique(hfs$hf))
    hfs$n_hf <- 1
    df <- aggregate(list(n_hf = hfs$n_hf), by = hfs[, c("country", "site")],
      FUN = sum, na.rm = T)
    mean(df$n_hf)
    range(df$n_hf)
    
#...............................................................................
### Visualising patterns in causes not related to geography (country / site)
#...............................................................................

  #...................................      
  ## Visualise ICD codes by age group and sex (refugees only)
    
    # Prepare dataset
    df <- subset(mh1, pt_type == "refugee")
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("age", "sex", "cat1")], FUN = sum, na.rm = T)
    x <- aggregate(list(n_cases_tot = df$n_cases), by = df[, c("age", "sex")],
      FUN = sum, na.rm = T)
    df <- merge(df, x, by = c("age", "sex"), all.x = T)
    df$prop_cases <- df$n_cases / df$n_cases_tot
    df$prop_cases <- label_percent(accuracy = 0.1)(df$prop_cases)
        
    # Plot
    pl <- ggplot(df, aes(x = cat1, y = n_cases, fill = cat1)) +
      geom_col(alpha = 0.75, colour = "black") +
      scale_x_discrete("ICD code(s)") +
      scale_y_continuous("number of consultations", labels = comma,
        expand = c(0,0), limits = c(0, max(df$n_cases * 1.1))) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45,
        hjust = 1, vjust = 1), panel.grid.major.x = element_blank(),
        plot.margin = margin(0, 0, 0, 3.5, unit = "cm")) +
      facet_grid(age ~ sex) +
      geom_text(aes(label = prop_cases), nudge_y = 1500, size = 2)
    ggsave(paste0(dir_path, "out/02_cat1_age_sex.png"), dpi = "print",
      units = "cm", width = 20, height = 20*hw)
   
    
  #...................................      
  ## Compare ICD codes by age group (refugees vs nationals)
    
    # Prepare dataset
    df <- aggregate(list(n_cases = mh1$n_cases), 
      by = mh1[, c("pt_type", "age", "cat1")], FUN = sum, na.rm = T)
    df <- df[order(df$pt_type, df$age, df$cat1), ]
    x <- by(df, df[, c("pt_type", "age")], 
      function(xx) {xx$n_cases / sum(xx$n_cases)})
    x <- as.data.frame(do.call(rbind, x))
    colnames(x) <- unique(df$cat1)
    x$pt_type <- as.vector(sapply(unique(df$pt_type), 
      function(xx) {rep(xx, length(unique(df$age)))}))
    x$age <- rep(unique(df$age), 2)
    df <- reshape(x, direction = "long", varying = 1:length(unique(df$cat1)),
      idvar = c("pt_type", "age"), timevar = "cat1", times = unique(df$cat1),
      v.names = "prop")
    df$prop_lab <- label_percent(accuracy = 0.1)(df$prop)
    
    # Plot
    pl <- ggplot(df, aes(x = cat1, y = prop, fill = cat1)) +
      geom_col(alpha = 0.75, position = "identity", colour = "black") +
      scale_x_discrete("ICD code(s)") +
      scale_y_continuous("percentage of consultations within age group", 
        labels = percent, expand = c(0,0), limits = c(0, 1), 
        breaks = seq(0, 0.8, 0.2)) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45,
        hjust = 1, vjust = 1), panel.grid.major.x = element_blank(),
        plot.margin = margin(0, 0, 0, 3.7, unit = "cm")) +
      facet_grid(age ~ pt_type) +
      geom_text(aes(label = prop_lab), nudge_y = 0.05, size = 2)
    ggsave(paste0(dir_path, "out/02_cat1_age_pt_type.png"), dpi = "print",
      units = "cm", width = 20, height = 20*hw)
   
     
  #...................................      
  ## Visualise supra-categories by age group and sex (refugees only)
    
    # Prepare dataset
    df <- subset(mh2, pt_type == "refugee")
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("age", "sex", "cat2")], FUN = sum, na.rm = T)
    x <- aggregate(list(n_cases_tot = df$n_cases), by = df[, c("age", "sex")],
      FUN = sum, na.rm = T)
    df <- merge(df, x, by = c("age", "sex"), all.x = T)
    df$prop_cases <- df$n_cases / df$n_cases_tot
    df$prop_cases <- label_percent(accuracy = 0.1)(df$prop_cases)
    
    # Plot
    pl <- ggplot(df, aes(x = cat2, y = n_cases, fill = cat2)) +
      geom_col(alpha = 0.75, colour = "black") +
      scale_x_discrete("category") +
      scale_y_continuous("number of consultations", labels = comma,
        expand = c(0,0), limits = c(0, max(df$n_cases * 1.1))) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45,
        hjust = 1, vjust = 1), panel.grid.major.x = element_blank(),
        plot.margin = margin(0, 0, 0, 2.5, unit = "cm")) +
      facet_grid(age ~ sex) +
      geom_text(aes(label = prop_cases), nudge_y = 1500, size = 2.5)
    ggsave(paste0(dir_path, "out/02_cat2_age_sex.png"), dpi = "print",
      units = "cm", width = 18, height = 18*hw)
   

  #...................................      
  ## Compare supra-categories by age group (refugees vs nationals)
    
    # Prepare dataset
    df <- aggregate(list(n_cases = mh2$n_cases), 
      by = mh2[, c("pt_type", "age", "cat2")], FUN = sum, na.rm = T)
    df <- df[order(df$pt_type, df$age, df$cat2), ]
    x <- by(df, df[, c("pt_type", "age")], 
      function(xx) {xx$n_cases / sum(xx$n_cases)})
    x <- as.data.frame(do.call(rbind, x))
    colnames(x) <- unique(df$cat2)
    x$pt_type <- as.vector(sapply(unique(df$pt_type), 
      function(xx) {rep(xx, length(unique(df$age)))}))
    x$age <- rep(unique(df$age), 2)
    df <- reshape(x, direction = "long", varying = 1:length(unique(df$cat2)),
      idvar = c("pt_type", "age"), timevar = "cat2", times = unique(df$cat2),
      v.names = "prop")
    df$prop_lab <- label_percent(accuracy = 0.1)(df$prop)
    
    # Plot
    pl <- ggplot(df, aes(x = cat2, y = prop, fill = cat2)) +
      geom_col(alpha = 0.75, position = "identity", colour = "black") +
      scale_x_discrete("category") +
      scale_y_continuous("percentage of consultations within age group", 
        labels = percent, expand = c(0,0), limits = c(0, 1), 
        breaks = seq(0, 1, 0.2)) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45,
        hjust = 1, vjust = 1), panel.grid.major.x = element_blank(),
        plot.margin = margin(0, 0, 0, 2.5, unit = "cm")) +
      facet_grid(age ~ pt_type) +
      geom_text(aes(label = prop_lab), nudge_y = 0.05, size = 2.5)      
    ggsave(paste0(dir_path, "out/02_cat2_age_pt_type.png"), dpi = "print",
      units = "cm", width = 18, height = 18*hw)
   
 
#...............................................................................
### Visualising patterns in causes related to geography (country / site)
#...............................................................................

  #...................................      
  ## Compare ICD codes by country (refugees only)
    
    # Prepare dataset
    df <- aggregate(list(n_cases = mh1$n_cases), 
      by = mh1[, c("region", "country", "cat1")], FUN = sum, na.rm = T)
    x <- aggregate(list(n_cases_tot = df$n_cases), 
      by = list(country = df$country), FUN = sum, na.rm = T)
    df <- merge(df, x, by = "country", all.x = T)
    df$prop <- df$n_cases / df$n_cases_tot
    df$region <- sapply(strwrap(df$region, 15, simplify=F), paste, 
        collapse = "\n" )
    df[which(df$n_cases_tot < 10), "prop"] <- NA 
      # exclude countries with <10 cases
    
    # Plot
    pl <- ggplot(df, aes(x = cat1, y = country, size = prop, colour = cat1)) +
      geom_point(alpha = 0.75) +
      scale_x_discrete("ICD code(s)") +
      scale_y_discrete("country", limits = rev) +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(legend.position = "none", panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.margin = margin(0, 0, 0, 0, unit = "cm")) +
      facet_grid(region ~ ., scales = "free_y", space = "free_y")
    ggsave(paste0(dir_path, "out/02_cat1_country.png"), dpi = "print",
      units = "cm", width = 18, height = 18*hw)
    
    
  #...................................      
  ## Compare supra-categories by country (refugees vs nationals)
    
    # Prepare dataset
    df <- aggregate(list(n_cases = mh2$n_cases), 
      by = mh2[, c("region", "country", "pt_type", "cat2")], 
      FUN = sum, na.rm = T)
    x <- aggregate(list(n_cases_tot = df$n_cases), 
      by = df[, c("country", "pt_type")], FUN = sum, na.rm = T)
    df <- merge(df, x, by = c("country", "pt_type"), all.x = T)
    df$prop <- df$n_cases / df$n_cases_tot
    df$region <- sapply(strwrap(df$region, 15, simplify=F), paste, 
        collapse = "\n" )
    df[which(df$n_cases_tot < 10), "prop"] <- NA 
      # exclude countries with <10 cases
    
    # Plot
    pl <- ggplot(df, aes(x = cat2, y = country, size = prop, colour = cat2)) +
      geom_point(alpha = 0.75) +
      scale_x_discrete("category") +
      scale_y_discrete("country", limits = rev) +
      scale_colour_viridis_d() +
      theme_bw() +
      theme(legend.position = "none", panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.margin = margin(0, 0, 0, 0, unit = "cm")) +
      facet_grid(region ~ pt_type, scales = "free_y", space = "free_y")
    ggsave(paste0(dir_path, "out/02_cat2_country_pt_type.png"), dpi = "print",
      units = "cm", width = 18, height = 18*hw)
     
  #...................................      
  ## Compare supra-categories by country - ALTERNATIVE (refugees vs nationals)
    
    # Prepare dataset
    df <- aggregate(list(n_cases = mh2$n_cases), 
      by = mh2[, c("region", "country", "pt_type", "cat2")], 
      FUN = sum, na.rm = T)
    x <- aggregate(list(n_cases_tot = df$n_cases), 
      by = df[, c("country", "pt_type")], FUN = sum, na.rm = T)
    df <- merge(df, x, by = c("country", "pt_type"), all.x = T)
    df$prop <- df$n_cases / df$n_cases_tot
    df$region <- sapply(strwrap(df$region, 15, simplify=F), paste, 
        collapse = "\n" )
    df[which(df$n_cases_tot < 10), "prop"] <- NA 
      # exclude countries with <10 cases
    labs_pl <- unique(df[, c("pt_type", "region", "country", "n_cases_tot")])
    labs_pl <- labs_pl[order(labs_pl$pt_type, labs_pl$region, labs_pl$country),]
    labs_pl$cat2 <- "Epilepsy or seizures"
    
    # Plot
    pl <- ggplot(df, aes(x = prop, y = country, fill = cat2)) +
      geom_bar(alpha = 0.75, stat = "identity", position = "fill", 
        colour = "black") +
      scale_x_continuous("percentage of all consultations", labels = percent) +
      scale_y_discrete("country", limits = rev) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 10), legend.justification = "right", 
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.margin = margin(0, 0, 0, 0, unit = "cm")) +
      guides(fill = guide_legend(nrow = 3, reverse = T)) +
      facet_grid(region ~ pt_type, scales = "free_y", space = "free_y") +
      geom_text(data = labs_pl, mapping = aes(x = 0.50, y = country, 
        label = comma(n_cases_tot)), size = 4, colour = "white")
    ggsave(paste0(dir_path, "out/02_cat2_country_pt_type_alt.png"), 
      dpi = "print", units = "cm", width = 18, height = 18*hw)

    
  #...................................      
  ## Compare supra-categories by site (refugees only)
    
    # Prepare dataset
    df <- subset(df, pt_type == "refugee")
    df <- aggregate(list(n_cases = mh2$n_cases), 
      by = mh2[, c("region", "country", "site", "cat2")], 
      FUN = sum, na.rm = T)
    x <- aggregate(list(n_cases_tot = df$n_cases), 
      by = df[, c("country", "site")], FUN = sum, na.rm = T)
    df <- merge(df, x, by = c("country", "site"), all.x = T)
    df$prop <- df$n_cases / df$n_cases_tot
    df$region <- sapply(strwrap(df$region, 15, simplify=F), paste, 
        collapse = "\n" )
    df[which(df$n_cases_tot < 10), "prop"] <- NA 
      # exclude sites with <10 cases
    labs_pl <- unique(df[, c("region", "country", "site", "n_cases_tot")])
    labs_pl$cat2 <- "Epilepsy or seizures"
    
    # Plot
    pl <- ggplot(df, aes(x = prop, y = site, fill = cat2)) +
      geom_bar(alpha = 0.75, stat = "identity", position = "fill", 
        colour = "black") +
      scale_x_continuous("percentage of all consultations", labels = percent,
        expand = expansion(mult = c(0.02, 0))) +
      scale_y_discrete("site", limits = rev) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 10), legend.justification = "right", 
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.margin = margin(1, 0, 0, 0, unit = "cm"),
        strip.text.y = element_text(angle = 0)) +
      guides(fill = guide_legend(nrow = 3, reverse = T)) +
      facet_nested(region + country ~ ., scales = "free_y", space = "free_y") +
      geom_text(data = labs_pl, mapping = aes(x = -0.02, y = site, 
        label = comma(n_cases_tot)), size = 3, colour = "grey20")
    ggsave(paste0(dir_path, "out/02_cat2_site.png"), 
      dpi = "print", units = "cm", width = 45, height = 45*hw)


#...............................................................................
### Visualising consultation rates by geography
#...............................................................................
    
  #...................................      
  ## Compute consultation rates (per capita/year; refugees only)
    
    # MH consultation rates by age and sex, for each site
    df <- subset(mh2, pt_type == "refugee")
    df$pop_agesex <- df$pop_0405 * df$prop_agesex
    x <- unique(df[which(! is.na(df$pop_agesex)),
      c("region", "country", "site", "mmyy", "age", "sex", "pop_agesex")])
    df <- aggregate(list(n_cases = df$n_cases), 
      by = df[, c("region", "country", "site", "mmyy", "age", "sex")], 
      FUN = sum, na.rm = T)
    df <- merge(df, x, by =c("region", "country", "site", "mmyy", "age", "sex"), 
      all.x = T)
    df <- aggregate(df[, c("n_cases", "pop_agesex")], 
      by = df[, c("region", "country", "site", "age", "sex")], FUN = sum)
    df$region <- sapply(strwrap(df$region, 15, simplify=F), paste, 
        collapse = "\n" )    
    mh2_cr_site <- df
    mh2_cr_site$cons_rate <- mh2_cr_site$n_cases * 1200 / mh2_cr_site$pop_agesex
    mh2_cr_site[which(mh2_cr_site$cons_rate %in% c(NaN, Inf)), 
      "cons_rate"] <-NA 
    
    # MH consultation rates by age and sex, for each country
    df <- subset(df, ! is.na(pop_agesex) & pop_agesex > 0)
    df <- aggregate(df[, c("n_cases", "pop_agesex")], 
      by = df[, c("region", "country", "age", "sex")], FUN = sum)
    mh2_cr_country <- df
    mh2_cr_country$cons_rate <- mh2_cr_country$n_cases * 1200 / 
      mh2_cr_country$pop_agesex
    mh2_cr_country[which(mh2_cr_country$cons_rate %in% c(NaN, Inf)),
      "cons_rate"] <-NA 

    # MH and all-cause consultation rates, by sex, for each site
    df <- subset(mh2a, pt_type == "refugee")
    df$pop_sex <- df$pop_0405 * df$prop_sex
    x <- unique(df[, c("region", "country", "site", "mmyy", "sex", 
      "n_cases_all", "pop_sex")])
    df <- aggregate(list(n_cases = df$n_cases), 
      by = df[, c("region", "country", "site", "mmyy", "sex")], FUN = sum, 
      na.rm = T)
    df <- merge(df, x, by = c("region", "country", "site", "mmyy", "sex"), 
      all.x = T)
    df <- aggregate(df[, c("n_cases", "n_cases_all", "pop_sex")], 
      by = df[, c("region", "country", "site", "sex")], FUN = sum, na.rm = T)
    df$region <- sapply(strwrap(df$region, 15, simplify=F), paste, 
        collapse = "\n" )    
    mh2a_cr_site <- df
    mh2a_cr_site$cons_rate_mh <- mh2a_cr_site$n_cases * 1200 / 
      mh2a_cr_site$pop_sex
    mh2a_cr_site$cons_rate_all <- mh2a_cr_site$n_cases_all * 1200 / 
      mh2a_cr_site$pop_sex
    mh2a_cr_site[which(mh2a_cr_site$cons_rate_mh %in% c(NaN, Inf)),
      "cons_rate_mh"] <-NA 
    mh2a_cr_site[which(mh2a_cr_site$cons_rate_all %in% c(NaN, Inf)),
      "cons_rate_all"] <-NA 

    # MH and all-cause consultation rates, by sex, for each country
    df <- subset(mh2a_cr_site, ! is.na(mh2a_cr_site$cons_rate_mh))
    df <- aggregate(df[, c("n_cases", "pop_sex")], 
      by = df[, c("region", "country", "sex")], FUN = sum)
    mh2a_cr_country <- df
    df <- subset(mh2a_cr_site, ! is.na(mh2a_cr_site$cons_rate_all))
    df <- aggregate(list(n_cases_all = df$n_cases_all), 
      by = df[, c("region", "country", "sex")], FUN = sum)
    mh2a_cr_country <- merge(mh2a_cr_country, df, 
      by = c("region", "country", "sex"), all.x = T)
    mh2a_cr_country$cons_rate_mh <- mh2a_cr_country$n_cases * 1200 / 
      mh2a_cr_country$pop_sex
    mh2a_cr_country$cons_rate_all <- mh2a_cr_country$n_cases_all * 1200 / 
      mh2a_cr_country$pop_sex
    mh2a_cr_country[which(mh2a_cr_country$cons_rate_mh %in% c(NaN, Inf)),
      "cons_rate_mh"] <-NA 
    mh2a_cr_country[which(mh2a_cr_country$cons_rate_all %in% c(NaN, Inf)),
      "cons_rate_all"] <-NA 
        
    # MH and all-cause consultation rates, for each site
    df <- subset(mh2b, pt_type == "refugee")
    df$pop <- df$pop_0405
    x <- unique(df[, c("region", "country", "site", "mmyy", 
      "n_cases_all", "pop")])
    df <- aggregate(list(n_cases = df$n_cases), 
      by = df[, c("region", "country", "site", "mmyy")], FUN = sum, na.rm = T)
    df <- merge(df, x, by = c("region", "country", "site", "mmyy"), all.x = T)
    df <- aggregate(df[, c("n_cases", "n_cases_all", "pop")], 
      by = df[, c("region", "country", "site")], FUN = sum, na.rm = T)
    df$region <- sapply(strwrap(df$region, 15, simplify=F), paste, 
        collapse = "\n" )    
    mh2b_cr_site <- df
    mh2b_cr_site$cons_rate_mh <- mh2b_cr_site$n_cases * 1200 / 
      mh2b_cr_site$pop
    mh2b_cr_site$cons_rate_all <- mh2b_cr_site$n_cases_all * 1200 / 
      mh2b_cr_site$pop
    mh2b_cr_site[which(mh2b_cr_site$cons_rate_mh %in% c(NaN, Inf)),
      "cons_rate_mh"] <-NA 
    mh2b_cr_site[which(mh2b_cr_site$cons_rate_all %in% c(NaN, Inf)),
      "cons_rate_all"] <-NA 

    # MH and all-cause consultation rates, for each country
    df <- subset(mh2b_cr_site, ! is.na(mh2b_cr_site$cons_rate_mh))
    df <- aggregate(df[, c("n_cases", "pop")], 
      by = df[, c("region", "country")], FUN = sum)
    mh2b_cr_country <- df
    df <- subset(mh2b_cr_site, ! is.na(mh2b_cr_site$cons_rate_all))
    df <- aggregate(list(n_cases_all = df$n_cases_all), 
      by = df[, c("region", "country")], FUN = sum)
    mh2b_cr_country <- merge(mh2b_cr_country, df, 
      by = c("region", "country"), all.x = T)
    mh2b_cr_country$cons_rate_mh <- mh2b_cr_country$n_cases * 1200 / 
      mh2b_cr_country$pop
    mh2b_cr_country$cons_rate_all <- mh2b_cr_country$n_cases_all * 1200 / 
      mh2b_cr_country$pop
    mh2b_cr_country[which(mh2b_cr_country$cons_rate_mh %in% c(NaN, Inf)),
      "cons_rate_mh"] <-NA 
    mh2b_cr_country[which(mh2b_cr_country$cons_rate_all %in% c(NaN, Inf)),
      "cons_rate_all"] <-NA 
    
    
  #...................................      
  ## Visualise MH age-sex consultation rates by site and country
    
    # Prepare dataset
    df <- mh2_cr_site
    x <- mh2_cr_country
    x$cons_rate_country <- x$cons_rate
    df <- merge(df, x[, c("region", "country", "age", "sex", 
      "cons_rate_country")], by = c("region", "country", "age", "sex"), 
      all.x = T)
    df$region <- gsub("South East\nAsia", "SE\nAsia", df$region)
    
    # Plot
    pl <- ggplot(df, aes(x = country, y = cons_rate, colour = region)) +
      geom_point(alpha = 1, stroke = 0.5, shape = 1) +
      geom_point(aes(y = cons_rate_country), alpha = 0.5, size = 5, 
        shape = 95) +
      scale_x_discrete("country") +
      scale_y_continuous(
        "mental health-related consultations per 100 person-years",
        trans = "sqrt", breaks = c(0, 5, 20, 50, 100))+
      scale_colour_viridis_d() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none", panel.grid.major.x = element_blank()) +
      facet_nested(sex + age ~ region, scales = "free_x", space = "free_x")
    ggsave(paste0(dir_path, "out/02_cr_site.png"), 
      dpi = "print", units = "cm", width = 17, height = 17*(hw-0.05))
    

  #...................................      
  ## Visualise MH/overall consultation rates by country, by sex and overall
    
    # Prepare dataset
    df <- mh2a_cr_country
    df <- df[, c("region", "country", "cons_rate_mh", "cons_rate_all", "sex")]
    x <- mh2b_cr_country
    x$sex <- "both sexes"
    x <- x[, c("region", "country", "cons_rate_mh", "cons_rate_all", "sex")]
    df <- rbind(df, x)
    df$region <- gsub("South East\nAsia", "SE\nAsia", df$region)
    df$percent_mh <- scales::label_percent(accuracy = 0.1)(df$cons_rate_mh / 
        df$cons_rate_all)
    df$country <- as.character(df$country)
    df$sex <- as.character(df$sex)
    
    # Plot
    pl <- ggplot(df, aes(x = country, y = cons_rate_mh, fill = region)) +
      geom_bar(alpha = 1, stat = "identity", colour = "black") +
      geom_bar(aes(y = cons_rate_all), alpha = 0.5, stat = "identity",
        colour = "black") +
      scale_x_discrete("country") +
      scale_y_continuous("consultations per 100 person-years", 
        expand = expansion(add = c(0, 1)),
        trans = "sqrt", breaks = c(0, 2, 10, 20, 50, 100, 200, 300, 400, 500)) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none", panel.grid.major.x = element_blank()) +
      facet_nested(sex ~ region, scales = "free_x", space = "free_x") +
      geom_label(aes(label = percent_mh, y = cons_rate_mh), size = 2, 
        colour = "grey20", fill = "white", alpha = 0.75, nudge_y = 1.5)
    ggsave(paste0(dir_path, "out/02_cr_country.png"), 
      dpi = "print", units = "cm", width = 15, height = 15*(hw-0.05))

  #...................................      
  ## Compare consultations rates for refugees and throughout the host country
    
    # Prepare dataset
    df <- mh2b_cr_country
    x <- unique(mh1[, c("country", "country_iso")])            
    df <- merge(df, x, by = "country", all.x = T)
    df <- merge(df, who[, c("country_iso", "cons_rate_mh_natl")], 
      by = "country_iso")
    df <- df[, c("region", "country", "cons_rate_mh", "cons_rate_mh_natl")]
    df <- reshape(df, direction = "long", varying = 
      c("cons_rate_mh", "cons_rate_mh_natl"), idvar = c("region", "country"),
      timevar = "population", 
      times = c("refugees (2024-2025)", "host country (2014)"),
      v.names = "cons_rate"
    )
    df$missing_lab <- ifelse(is.na(df$cons_rate), "n/a", NA)
    df$cons_rate_lab <- label_number(accuracy = 0.1)(df$cons_rate)
    df[which(is.na(df$cons_rate_lab)), "cons_rate_lab"] <- "*"
    df$cons_rate_lab_y <- ifelse(is.na(df$cons_rate), 0.005, 
      0.1 + df$cons_rate * 1.15)
    
    # Plot
    pl <- ggplot(df, aes(x = country, y = cons_rate, fill = region, 
      alpha = population, group = population)) +
      geom_bar(colour = "black", stat = "identity", 
        position = position_dodge2(width = 0.75)) +
      scale_x_discrete("country") +
      scale_y_continuous(
        "mental health-related consultations per 100 person-years",
        trans = "sqrt", breaks = c(0, 0.2, 0.5, 1, 2, 5, 10, 20), 
        expand = expansion(add = c(0,0.1)))+
      scale_fill_viridis_d() +
      scale_alpha_discrete() +
      theme_bw() +
      guides(fill = "none") +
      labs(tag = "* host country value unavailable") +      
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top", panel.grid.major.x = element_blank(),
        plot.tag.position = "bottomright", plot.tag.location = "plot",
        plot.tag = element_text(size = 10)) +
      facet_nested(. ~ region, scales = "free_x", space = "free_x") +
      geom_text(aes(label = cons_rate_lab, y = cons_rate_lab_y), size = 2.5, 
        alpha = 1, position = position_dodge2(width = 0.95))
    ggsave(paste0(dir_path, "out/02_cr_refugees_vs_host_country.png"), 
      dpi = "print", units = "cm", width = 20, height = 10*(hw-0.05))
      
    
#...............................................................................
### Visualising new vs. all consultations by geography
#...............................................................................
    
  #...................................      
  ## Visualise ratio of repeat to new consultations, by cause (MH, all), 
        # country, refugees vs nationals

    # Prepare dataset
    df <- unique(mh2b[, c("region", "country", "pt_type", "mmyy",
      "n_cases_mh_new", "n_cases_all", "n_cases_new_all")])
    df <- aggregate(df[, c("n_cases_mh_new", "n_cases_all", "n_cases_new_all")],
      by = df[, c("region", "country", "pt_type")], FUN = sum, na.rm = T)
    x <- aggregate(list(n_cases = mh2b$n_cases),
      by = mh2b[, c("region", "country", "pt_type")], FUN = sum, na.rm = T)
    df <- merge(df, x, by = c("region", "country", "pt_type"), all.x = T)
    df$ratio_mh <- (df$n_cases - df$n_cases_mh_new) / df$n_cases_mh_new
    df$ratio_all <- (df$n_cases_all - df$n_cases_new_all) / df$n_cases_new_all
    df[which(df$n_cases < 10), "ratio_mh"] <- NA
    df[which(df$n_cases_all < 10), "ratio_all"] <- NA
    df[which(df$ratio_all %in% c(NaN, Inf)), "ratio_all"] <- NA
    df[which(df$ratio_mh %in% c(NaN, Inf)), "ratio_mh"] <- NA
    df <- reshape(df[, c("region", "country", "pt_type", 
      "ratio_mh", "ratio_all")], direction = "long", 
      varying = c("ratio_mh", "ratio_all"), idvar = c("region", "country",
        "pt_type"), times = c("mh", "all"), timevar = "cause", v.names ="ratio")
    df$prop <- df$ratio / (df$ratio + 1)
    df$region <- sapply(strwrap(df$region, 15, simplify=F), paste, 
        collapse = "\n" )    
    df$region <- gsub("South East\nAsia", "SE\nAsia", df$region)    
    df$ratio <- scales::label_number(accuracy = 0.1)(df$ratio)
    df$cause <- ifelse(df$cause == "mh", "mental health-related", "all causes")
    
    # Plot
    pl <- ggplot(df, aes(x = country, y = prop, fill = region)) +
      geom_bar(alpha = 0.75, stat = "identity", colour = "black") +
      scale_x_discrete("country") +
      scale_y_continuous(
        "new consultations as a proportion of all consultations", 
        expand = c(0, 0), breaks = seq(0, 1, 0.2), labels = percent) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none", panel.grid.major.x = element_blank()) +
      facet_nested(cause + pt_type ~ region, 
        scales = "free_x", space = "free_x") +
      geom_label(aes(label = ratio, y = 0.05), size = 2, colour = "grey20",
        fill = "white", alpha = 1)
    ggsave(paste0(dir_path, "out/02_new_country_pt_type.png"), 
      dpi = "print", units = "cm", width = 15, height = 15*(hw-0.05))
    
    
  #...................................      
  ## Visualise ratio of repeat to new consultations, by sex and country
        # (refugees only)

    # Prepare dataset
    df <- subset(mh2a, pt_type == "refugee")
    df <- unique(df[, c("region", "country", "mmyy", "sex",
      "n_cases_mh_new", "n_cases_all", "n_cases_new_all")])
    df <- aggregate(df[, c("n_cases_mh_new", "n_cases_all", "n_cases_new_all")],
      by = df[, c("region", "country", "sex")], FUN = sum, na.rm = T)
    x <- subset(mh2a, pt_type == "refugee")
    x <- aggregate(list(n_cases = x$n_cases),
      by = x[, c("region", "country", "sex")], FUN = sum, na.rm = T)
    df <- merge(df, x, by = c("region", "country", "sex"), all.x = T)
    df$ratio_mh <- (df$n_cases - df$n_cases_mh_new) / df$n_cases_mh_new
    df$ratio_all <- (df$n_cases_all - df$n_cases_new_all) / df$n_cases_new_all
    df[which(df$n_cases < 10), "ratio_mh"] <- NA
    df[which(df$n_cases_all < 10), "ratio_all"] <- NA
    df[which(df$ratio_all %in% c(NaN, Inf)), "ratio_all"] <- NA
    df[which(df$ratio_mh %in% c(NaN, Inf)), "ratio_mh"] <- NA
    df <- reshape(df[, c("region", "country", "sex", 
      "ratio_mh", "ratio_all")], direction = "long", 
      varying = c("ratio_mh", "ratio_all"), idvar = c("region", "country",
        "sex"), times = c("mh", "all"), timevar = "cause", v.names ="ratio")
    df$prop <- df$ratio / (df$ratio + 1)
    df$region <- sapply(strwrap(df$region, 15, simplify=F), paste, 
        collapse = "\n" )    
    df$region <- gsub("South East\nAsia", "SE\nAsia", df$region)    
    df$ratio <- scales::label_number(accuracy = 0.1)(df$ratio)
    df$cause <- ifelse(df$cause == "mh", "mental health-related", "all causes")
    
    # Plot        
    pl <- ggplot(df, aes(x = country, y = prop, fill = region)) +
      geom_bar(alpha = 0.75, stat = "identity", colour = "black") +
      scale_x_discrete("country") +
      scale_y_continuous(
        "new consultations as a proportion of all consultations", 
        expand = c(0, 0), breaks = seq(0, 1, 0.2), labels = percent) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none", panel.grid.major.x = element_blank()) +
      facet_nested(cause + sex ~ region, 
        scales = "free_x", space = "free_x") +
      geom_label(aes(label = ratio, y = 0.05), size = 2, colour = "grey20",
        fill = "white", alpha = 1)
    ggsave(paste0(dir_path, "out/02_new_country_sex.png"), 
      dpi = "print", units = "cm", width = 15, height = 15*(hw-0.05))
    
    
#...............................................................................
### ENDS
#...............................................................................
   