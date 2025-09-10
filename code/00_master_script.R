#...............................................................................
### +++++++ ANALYSIS OF UNHCR iRHIS MENTAL HEALTH CONSULTATION DATA ++++++++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO LOAD PACKAGES AND SOURCE OTHER ANALYSIS SCRIPTS  ------ ##
#...............................................................................



#...............................................................................
### Preparatory steps
#...............................................................................

  #...................................      
  ## Install or load required R packages
  if (!"pacman" %in% rownames(installed.packages())){install.packages("pacman")}
  
    # Install or load packages from CRAN
    pacman::p_load(
      DHARMa,        # Model diagnostics
      ggalluvial,    # Prepare an alluvial plot
      ggh4x,         # For plots with nested facets      
      ggplot2,       # Visualise data
      ggpubr,        # Arrange multiple plots into a single plot
      ggrepel,       # Improve labelling of plots
      glmmTMB,       # Fit mixed models
      gtools,        # Assist various programming tasks
      lme4,          # Fit mixed models
      lubridate,     # Work with dates and times
      MASS,          # Implement various statistical methods
      mclogit,       # Multinomial logit models
      memisc,        # Get coefficients from multinomial logit models
      mgcv,          # Fit generalised additive models
      parameters,    # Extract model output
      readxl,        # Read Excel files
      scales,        # Scale and format data for visualisation
      sf,            # Work with shape files and maps
      tidyverse,     # Tidyverse suite of packages
      tmap,          # Produce maps and work with GIS data
      viridis,       # Colour-blind palette
      zoo)           # Compute running means


  #...................................      
  ## Starting setup

    # Clean up from previous code / runs
    rm(list=ls(all=TRUE) )
  
    # Set font for Windows or Mac
    suppressWarnings(windowsFonts(Arial = windowsFont("Arial")))
    suppressWarnings(par(family = "Arial"))

    # Set working directory to where this file is stored
    dir_path <- paste(dirname(rstudioapi::getActiveDocumentContext()$path  )
      , "/", sep = "")
    setwd(dir_path)
    print( getwd() )
    dir_path <- gsub("/code", "", dir_path)
    suppressWarnings(dir.create(paste0(dir_path, "out")))
    
    # Initialise random numbers
    set.seed(123)
    
    # Colour-blind palette for graphing
      # general palette
      palette_gen <- viridis(16)
      show_col(palette_gen)
       
    # height to width ratio for figures (A4 format: should be 1.414, but
      # leave some space for captions)
    hw <- 1.3

#...............................................................................
### Sourcing dependent scripts
#...............................................................................

  #...................................      
  ## Pre-processing data
  source(paste0(dir_path, "code/01_process_data.r") )

  #...................................      
  ## Visualising patterns
  source(paste0(dir_path, "code/02_visualise_patterns.r") )

  #...................................      
  ## Exploring factors associated with MH morbidity
  source(paste0(dir_path, "code/03_explore_factors.r") )
                    

#...............................................................................  
### ENDS
#...............................................................................
     
