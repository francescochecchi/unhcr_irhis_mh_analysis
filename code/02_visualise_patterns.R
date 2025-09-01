#...............................................................................
### +++++++ ANALYSIS OF UNHCR iRHIS MENTAL HEALTH CONSULTATION DATA ++++++++ ###
#...............................................................................

#...............................................................................
## ----- R SCRIPT TO VISUALISE PATTERNS IN MENTAL HEALTH CONSULTATIONS  ----- ##
#...............................................................................



#...............................................................................
### XXX
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
    
    
    
    
    