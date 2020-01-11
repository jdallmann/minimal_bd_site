###############################
## PRIORITIZATION EDA
###############################

library(tidyverse)
library(winnipegr)


data_list <- search_open_data("Expenditures|Budget|Human")


hr_2016 <- get_open_data(
    data_list$identifier[data_list$title == "Human Resources Report"]
    )

cap_ex <- get_open_data(
    data_list$identifier[data_list$title == "Capital Expenditures"]
)

op_budget <- get_open_data(
    data_list$identifier[str_detect(
        data_list$title, "Operating Budget")]
    ) %>%
    mutate(budget_year = as.numeric(budget_year))

cap_ex$date %>% max() # up to date
op_budget$year %>% max() # 2018
hr_2016$year %>% max() # 2016

# http://winnipegassessment.com/Asmttax/pdfs/rates/HistoricalCombinedMillRates.pdf
assessment <- get_open_data("d4mq-wa44") %>% # assessment values 20200102
    janitor::clean_names() %>%
    filter(str_detect(property_class_1, "RESIDENTIAL")) %>%
    summarise(mean_assessed_value_t = mean(as.numeric(total_assessed_value), na.rm = T),
              median_assessed_value_t = median(as.numeric(total_assessed_value), na.rm = T),
              mean_assessed_value_1 = mean(as.numeric(assessed_value_1), na.rm = T),
              median_assessed_value_1 = median(as.numeric(assessed_value_1), na.rm = T))
    
 # http://winnipegassessment.com/AsmtTax/English/SelfService/Statistics.stm   
ave_mill_rates <- tibble(
    year = c(2019, 2018, 2017, 2016),
    ave_total_muni_tax = c(2046, 2006, 1966, 1927), # Municipal tax + street renewal fee
    mill_rate = c(13.290, 12.987, 13.063, 12.766)
)


save.image("C:/Users/jdallman/Google Drive/Coding/HTML/working_posts/prioritization_data-202001.RData")