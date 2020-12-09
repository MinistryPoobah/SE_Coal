# __ This script was designed for the Environmental Protection Division SE Coal Team
# __ with the purpose of evaluating selenium speciation among locations in the Elk Valley.

# The script can be found on GitHub at https://github.com/MinistryPoobah/SE_Coal/blob/main/2020-12-09_LargerSeleniumSpeciation.R


# Author: Keith Story, M.Sc., R.P. Bio.
# Ministry of Environment and Climate Change Strategy
# Environmental Protection Division

  library(tidyverse)
  library(dplyr)
  library(readxl)
  library(ggplot2)
  
# Load data from sheet (note that the data were copied from pdf and formatting errors were manually corrected)
# Change the path in the following line to point to the parent folder containing the data and the plot subfolders  
  setwd("\\Sfp.idir.bcgov\s140\s40086\WANSHARE\ROB\Mining\Authorizations 10400-60\107517 Teck Coal Limited (Valley Permit) PE\07a Reports-Data\1 WQ Annual Rpts\2018 2019 Se Speciation Plots")
  raw_se_spec <- read_excel(path = "2019 WQ Ann Rpt Se Speciation Data.xlsx")

  
# _______________________Data Cleaning_______________________________________  
  
# Tidy the data. Treat MDL/LOD values as NA. If statistics are performed, I recommend either MDL/2 or MDL/sqrt(2).  
  tdy_se_spec <- raw_se_spec %>% 
    mutate_all(funs(str_replace(., "< ", ""))) %>% 
    na_if(., "0.010") %>% 
    mutate(`Sample Date` = as.Date(`Sample Date`)) %>% 
    dplyr::rename("Methylseleninic acid" = "MeSe(IV) –\r\nmethylseleninic acid",
                  "Selenate" = "Se(VI) – selenate SeO4(-\r\n2)",
                  "Selenosulfate" = "Selenosulfate\r\nSeSO3",
                  "Selenomethionine" = "SeMe –\r\nselenomethionine",
                  "Selenite" = "Se(IV) – selenite SeO3(-\r\n2)",
                  "Selenocyanate" = "SeCN – selenocyanate\r\nSeCN(-1)",
                  "Unknown Parameter - Brooks" = "Unknown parameter\r\nfrom Brooks",
                  "Unknown Selenium Species" = "Unknown selenium\r\nspecies" 
                 ) %>% 
    pivot_longer(
      cols = `Dimethylseleneoxide`:`Unknown Selenium Species`,
      names_to = "SeleniumSpecies",
      values_to = "Concentration"
                ) %>% 
    as_tibble()
  
  # Convert the values from character to numeric 
  tdy_se_spec[[5]] <- as.numeric(tdy_se_spec[[5]])
  
  
# _______________________Plots_______________________________________  

# Look through selenium species at *EMS sites*
  plot_se_spec <- tdy_se_spec %>% 
    # filter(SeleniumSpecies == "Dimethylseleneoxide") %>% 
    group_by(SeleniumSpecies) %>% 
    do(plots =
      ggplot(data = .) +
      geom_point(aes(y = Concentration, x = `Sample Date`, group = `EMS ID`)) +
      labs(title = .$SeleniumSpecies, y = "Concentration (ug/L)", x = "Date" ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      facet_wrap(. ~ `EMS ID`)
      )
  
  
  # plot_se_spec$plots      # uncomment this line to actually view the plots in RStudio.
  
# Export the plots as png and save to local directory. If you want to change directory, use setwd("your path").
  plot_se_spec %>%              
    do(., 
       ggsave(.$plots, filename = paste(getwd(), 
                                        "/Se Speciation by EMS ID/", 
                                        "2020-12-09_EMS_ID_scatter ", 
                                        .$SeleniumSpecies, 
                                        ".png", 
                                        sep = ""), 
                  device = "png", height = 8.5, width = 11, units = "in"))

  dev.off()
  
  
  
  
  
# Look through selenium species at *Non-EMS* Sites

Loc_se_spec <- tdy_se_spec %>% 
  filter(is.na(`EMS ID`)) %>% 
  # na.omit(Concentration) %>% 
  group_by(SeleniumSpecies) %>% 
  do(plots =
       ggplot(data = .) +
       geom_point(aes(y = Concentration, x = `Sample Date`, group = Location)) +
       labs(title = .$SeleniumSpecies, y = "Concentration (ug/L)", x = "Date" ) +
       theme_bw() +
       theme(axis.text.x = element_text(angle = 90),
             strip.text.x = element_text(size = 6)) +
       facet_wrap(. ~ Location)
  )

  # Loc_se_spec$plots         # uncomment this line to actually view the plots.
  
# Export the plots as png and save to local directory. If you want to change directory, use setwd("your path").
  Loc_se_spec %>%              
    do(., 
       ggsave(.$plots, filename = paste(getwd(), 
                                        "/Se Speciation by Non-EMS ID/", 
                                        "2020-12-09 Non-EMS_scatter ", 
                                        .$SeleniumSpecies, 
                                        ".png", 
                                        sep = ""), 
                device = "png", height = 8.5, width = 11, units = "in"))
  
  dev.off()