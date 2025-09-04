#...............................................................................
### +++++++ ANALYSIS OF UNHCR iRHIS MENTAL HEALTH CONSULTATION DATA ++++++++ ###
#...............................................................................

#...............................................................................
## ----- R SCRIPT TO VISUALISE PATTERNS IN MENTAL HEALTH CONSULTATIONS  ----- ##
#...............................................................................



#...............................................................................
### Visualising patterns not related to geography (country / site)
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
      units = "cm", width = 30, height = 30*1.414)
    
    
  #...................................      
  ## Visualise ICD codes by age group and sex (refugees only)
    
    # Prepare dataset
    df <- subset(mh1, pt_type == "refugee")
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("age", "sex", "cat1")], FUN = sum, na.rm = T)
    
    # Plot
    pl <- ggplot(df, aes(x = cat1, y = n_cases, fill = cat1)) +
      geom_col(alpha = 0.75) +
      scale_x_discrete("ICD code(s)") +
      scale_y_continuous("number of consultations", labels = comma,
        expand = c(0,0), limits = c(0, max(df$n_cases * 1.05))) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45,
        hjust = 1, vjust = 1), panel.grid.major.x = element_blank(),
        plot.margin = margin(0, 0, 0, 3.5, unit = "cm")) +
      facet_grid(age ~ sex)
    ggsave(paste0(dir_path, "out/02_icd_age_sex.png"), dpi = "print",
      units = "cm", width = 20, height = 20*1.414)
   
    
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
    
    # Plot
    pl <- ggplot(df, aes(x = cat1, y = prop, fill = cat1)) +
      geom_col(alpha = 0.75, position = "identity") +
      scale_x_discrete("ICD code(s)") +
      scale_y_continuous("percentage of consultations within age group", 
        labels = percent, expand = c(0,0), limits = c(0, 1), 
        breaks = seq(0, 0.8, 0.2)) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45,
        hjust = 1, vjust = 1), panel.grid.major.x = element_blank(),
        plot.margin = margin(0, 0, 0, 3.7, unit = "cm")) +
      facet_grid(age ~ pt_type)
    ggsave(paste0(dir_path, "out/02_icd_age_pt_type.png"), dpi = "print",
      units = "cm", width = 20, height = 20*1.414)
   
     
  #...................................      
  ## Visualise supra-categories by age group and sex (refugees only)
    
    # Prepare dataset
    df <- subset(mh2, pt_type == "refugee")
    df <- aggregate(list(n_cases = df$n_cases),
      by = df[, c("age", "sex", "cat2")], FUN = sum, na.rm = T)
    
    # Plot
    pl <- ggplot(df, aes(x = cat2, y = n_cases, fill = cat2)) +
      geom_col(alpha = 0.75) +
      scale_x_discrete("category") +
      scale_y_continuous("number of consultations", labels = comma,
        expand = c(0,0), limits = c(0, max(df$n_cases * 1.05))) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45,
        hjust = 1, vjust = 1), panel.grid.major.x = element_blank(),
        plot.margin = margin(0, 0, 0, 2.5, unit = "cm")) +
      facet_grid(age ~ sex)
    ggsave(paste0(dir_path, "out/02_cat2_age_sex.png"), dpi = "print",
      units = "cm", width = 18, height = 18*1.414)
   

  #...................................      
  ## Compare ICD codes by age group (refugees vs nationals)
    
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
    
    # Plot
    pl <- ggplot(df, aes(x = cat2, y = prop, fill = cat2)) +
      geom_col(alpha = 0.75, position = "identity") +
      scale_x_discrete("category") +
      scale_y_continuous("percentage of consultations within age group", 
        labels = percent, expand = c(0,0), limits = c(0, 1), 
        breaks = seq(0, 0.8, 0.2)) +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45,
        hjust = 1, vjust = 1), panel.grid.major.x = element_blank(),
        plot.margin = margin(0, 0, 0, 2.5, unit = "cm")) +
      facet_grid(age ~ pt_type)
    ggsave(paste0(dir_path, "out/02_cat2_age_pt_type.png"), dpi = "print",
      units = "cm", width = 18, height = 18*1.414)
   
 
#...............................................................................
### Visualising patterns related to geography (country / site)
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
    ggsave(paste0(dir_path, "out/02_icd_country.png"), dpi = "print",
      units = "cm", width = 18, height = 18*1.414)
    
    
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
      units = "cm", width = 18, height = 18*1.414)
     

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
      geom_bar(alpha = 0.75, stat = "identity", position = "fill") +
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
      dpi = "print", units = "cm", width = 18, height = 18*1.414)
        