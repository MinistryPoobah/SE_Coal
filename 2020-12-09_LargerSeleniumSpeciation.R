
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)


raw_se_spec <- read_excel(path = "C:/Users/kstory/Documents/R/SE_Coal/Data/2019 WQ Ann Rpt.xlsx")





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


for (i in 4:12) {
  tdy_se_spec[[i]] <- as.numeric(tdy_se_spec[[i]])
}


(ggplot(data = filter(tdy_se_spec, SeleniumSpecies == "Dimethylseleneoxide", ~is.na(`EMS ID`)),
        aes(y = Concentration, x = `Sample Date`, group = `EMS ID`)) +
    geom_point() +
    facet_wrap(. ~ `EMS ID`)

        )
)


