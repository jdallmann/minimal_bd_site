###############################
## GOOD FOR A CITY EDA
###############################

library(tidyverse)
library(magrittr)
library(winnipegr)

# Dataset drived from 'prioritization_eda.R' linked in post
# load("../../static/data/prioritization_data-202001.RData")
# assessments <- get_open_data("tax parcels") %>%
#     select(street_number, street_name, street_type,
#            full_address, assessed_value_1, multiple_residences,
#            water_frontage_measurement, sewer_frontage_measurement) %>%
#     mutate(mill_rate = ave_mill_rates$mill_rate[1],
#            tax = as.numeric(assessed_value_1) * mill_rate / 1000,
#            frontage_levy = ifelse(
#                !is.na(water_frontage_measurement),
#                5.45 * as.numeric(water_frontage_measurement),
#                273)) # Average frontage recorded on v1 p.209

# save.image("../../static/data/assessments_data-20200503.RData")
load("../../static/data/assessments_data-20200503.RData")



# http://winnipegassessment.com/Asmttax/pdfs/rates/HistoricalCombinedMillRates.pdf
med_assessment <- get_open_data("d4mq-wa44") %>% # assessment values 20200102
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

# https://winnipeg.ca/cao/pdfs/CommunityTrendsandPerformanceReportVolume1_2020.pdf (p.209)
op_ex_2019 <- data.frame(
    service = c("Police Service", "Fire Service", "Road Maintenance",
                "Public Transit", "Recreation", 
                "Parks and Urban Forestry", 
                "Roadway Snow Removal and Ice Clearing",
                "Libraries", 
                "Innovation, Transformation & Technology",
                "Organizational Support Service",
                "Solid Waste Collection", "City Beautification",
                "Assiniboine Park Conservancy", "Council Services",
                "Lighting", "Medical Response",
                "Assessment and Taxation",
                "Remaining Arts, Ent. & Culture",
                "Insect Control", "311 Contact Centre",
                "Community Livability", 
                "City Planning, Inspection & Housing",
                "Cemeteries", "Animal Services", 
                "Economic Development", "Golf Services"),
    perc_total = c(30.4, 14.7, 8.9, 8.4, 5.2, 4.2, 4.1,
                   3.5, 3.0, 2.7, 2.6, 2.1, 1.7, 1.6, 1.6,
                   1.2, 1.0, .9, .7, .6, .5, .2, .1, .1, 
                   .02, .02),
    stringsAsFactors = F)

assessments <- assessments %>%
    filter(!is.na(full_address),
           !is.na(assessed_value_1)) %>%
    distinct(full_address, .keep_all = TRUE)

budget_sum <- op_budget %>%
    filter(year == 2018,
           service != "Assessment, Tax & Corporate") %>%
    group_by(service, category) %>%
    summarise(budget = sum(budget_year)) %>%
    ungroup()

budget_sum <- rbind(
    budget_sum %>%
        mutate(budget = abs(budget)),
    budget_sum %>% 
        group_by(service) %>% 
        summarise(budget = sum(budget)) %>%
        mutate(category = "NET COST",
               budget = budget) %>%
        ungroup()
) %>%
    mutate(category = factor(category, 
                             levels = c("EXPENDITURES", "REVENUE", "NET COST")),
           budget = budget)

cap_ex_sum <- cap_ex %>%
    filter(date == "2018-12-31") %>%
    select(actual_costs_to_report_date, project_id,
           department, category, subcategory,
           project_description) %>%
    left_join(
        cap_ex %>%
            filter(date == "2017-12-31") %>%
            select(project_id, 
                   initial_costs = actual_costs_to_report_date)
    ) %>%
    mutate(initial_costs = as.numeric(initial_costs),
           actual_costs_to_report_date = as.numeric(
               actual_costs_to_report_date),
           cap_ex_2018 = ifelse(!is.na(initial_costs),
                                actual_costs_to_report_date -
                                    initial_costs,
                                actual_costs_to_report_date)
    )


cap_ex_pct <- sum(cap_ex_sum$cap_ex_2018, na.rm = T)

cap_ex_pct <- cap_ex_pct /
    (budget_sum %>% filter(category=="NET COST") %$%
         sum(budget, na.rm = T) +
         cap_ex_pct)


data_list$identifier[data_list$title == "Capital Expenditures"]

# Get updated capex for all of 2019
cap_ex <- RSocrata::read.socrata("https://data.winnipeg.ca/api/views/8xrn-n992")

cap_ex_sum <- cap_ex %>%
    filter(date == "2019-12-31") %>%
    select(actual_costs_to_report_date, project_id,
           department, category, subcategory,
           project_description) %>%
    left_join(
        cap_ex %>%
            filter(date == "2018-12-31") %>%
            select(project_id, 
                   initial_costs = actual_costs_to_report_date)
    ) %>%
    mutate(initial_costs = as.numeric(initial_costs),
           actual_costs_to_report_date = as.numeric(
               actual_costs_to_report_date),
           cap_ex_2019 = ifelse(!is.na(initial_costs),
                                actual_costs_to_report_date -
                                    initial_costs,
                                actual_costs_to_report_date)
    )


rm(hr_2016, op_budget, budget_sum, data_list)

# cap_ex_sum recoded in Excel
cap_ex_sum_2 <- read_csv("../../static/data/cap_ex_sum.csv") %>%
    group_by(service = op_map) %>%
    summarise(sum_total = sum(cap_ex_2019, na.rm = T)) %>%
    ungroup()

cap_ex_sum_2 <- cap_ex_sum_2 %>%
    mutate(perc_total = sum_total / sum(cap_ex_sum_2$sum_total),
           scaled_pct = perc_total * cap_ex_pct * 100) %>%
    select(service, perc_total, scaled_pct) %>%
    mutate(with_capex = 1)

scaled_op_ex <- op_ex_2019 %>%
    mutate(scaled_pct = perc_total * (1-cap_ex_pct)) %>%
    rbind(c("Capital Expenses", 0, cap_ex_pct * 100)) %>%
    mutate(scaled_pct = as.numeric(scaled_pct),
           perc_total = as.numeric(perc_total),
           with_capex = 0)

tax_pct <- scaled_op_ex %>%
    rbind(cap_ex_sum_2) %>% 
    filter(service != "Capital Expenses") %>%
    group_by(service) %>%
    summarise(scaled_pct = sum(scaled_pct, na.rm = T)) %>%
    ungroup()


save.image("../../static/data/good_for_a_city-202005.RData")
