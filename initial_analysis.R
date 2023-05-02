library(dplyr)
library(zoo)
library(fixest)
library(ggplot2)
library(data.table)
library(stringr)
library(texreg)
library(fixest)
#EDIT THIS LINE TO WHERE THE REPO WAS CLONED
path = "/Volumes/GoogleDrive/My Drive/Sophomore Spring/ES 94/Final Project/ES94-Final_Project"

data = read.csv(
  paste(path, "/data/VC_deals_exported.csv", sep = '')
)

source(paste(path,"/general_functions.R", sep = ''))

data = dataclean_initial(data)

aggregated_data = aggregated_deals(data)

aggregated_america_data = aggregated_deals(
  data[grepl("North America", data$PORTFOLIO_COMPANY_CONTINENT, fixed = TRUE),])

plot_time_series(aggregated_data$monthly_agg, DEAL_DATE,
                 num_of_deals, "VC Deals Over Time", "Year (and quarter)",
                 "Number of VC Deals")

plot_time_series(aggregated_data$quarterly_agg, DEAL_DATE,
                 deal_size, "VC Deal Size Over Time", "Year (and quarter)",
                 "VC Deal Size ($M)")

aggregated_data$quarterly_agg = 
  growth_rate_func(aggregated_data$quarterly_agg, "deal_size")
aggregated_data$quarterly_agg = 
  growth_rate_func(aggregated_data$quarterly_agg, "num_of_deals")
aggregated_america_data$quarterly_agg = 
  growth_rate_func(aggregated_america_data$quarterly_agg, "deal_size")
aggregated_america_data$quarterly_agg = 
  growth_rate_func(aggregated_america_data$quarterly_agg, "num_of_deals")

aggregated_data$monthly_agg = 
  growth_rate_func(aggregated_data$monthly_agg, "deal_size")
aggregated_data$monthly_agg = 
  growth_rate_func(aggregated_data$monthly_agg, "num_of_deals")
aggregated_america_data$monthly_agg = 
  growth_rate_func(aggregated_america_data$monthly_agg, "deal_size")
aggregated_america_data$monthly_agg = 
  growth_rate_func(aggregated_america_data$monthly_agg, "num_of_deals")

names = unique(data$DEAL_STAGE)

for (i in 1:length(names)) {
  deal_size = mean(data[data$DEAL_STAGE == names[i],]$DEAL_SIZE_USD, na.rm = TRUE)
  num_of_deals = nrow(data[data$DEAL_STAGE == names[i],])
  print(names[i])
  print(deal_size)
  print(num_of_deals)
}

early_stage_data = data[data$deal_class == "early",]

late_stage_data = data[data$deal_class == "late",]

early_stage_agg = aggregated_deals(early_stage_data)
late_stage_agg = aggregated_deals(late_stage_data)

early_stage_agg$quarterly_agg = 
  growth_rate_func(early_stage_agg$quarterly_agg, "deal_size")
late_stage_agg$quarterly_agg = 
  growth_rate_func(late_stage_agg$quarterly_agg, "deal_size")

regressions = list(
  "deal_size_reg" = summary(lm(DEAL_SIZE_USD~recession_ind +
                                 factor(DEAL_STAGE), data=data)),
  "monthly_deal_amount" = summary(lm(num_of_deals~recession_ind,
                                     data = aggregated_data$monthly_agg)),
  "quarterly_deal_size_recession_reg" =
    summary(lm(deal_size_growth_rate~recession_ind,
               data = aggregated_data$quarterly_agg)),
  "quarterly_early_stage_recession_reg" = 
    summary(lm(deal_size_growth_rate~recession_ind,
               data = early_stage_agg$quarterly_agg)),
  "quarterly_late_stage_recession_reg" = 
    summary(lm(deal_size_growth_rate~recession_ind,
               data = late_stage_agg$quarterly_agg)),
  "fed_funds_rate_deals_size_NA" = 
    summary(lm(DEAL_SIZE_USD~fed_funds_rate + factor(DEAL_STAGE),
               data = data[data$INVESTOR_CONTINENT == "North America",])),
  "fed_funds_rate_num_deals_NA" = 
    summary(lm(num_of_deals~fed_funds_rate,
               data = aggregated_america_data$quarterly_agg))
)

aggregated_data$quarterly_type_agg =
  growth_rate_with_type_func(aggregated_data$quarterly_type_agg,
                           "deal_size")

graph_binned(data.frame(aggregated_america_data$monthly_agg),
             "fed_funds_rate", "num_of_deals_growth_rate", 3,
             "Binned Fed funds rate", "VC deals in quarter",
             "Binned FED funds rate against VC deals")

graph_binned(data[data$deal_class == "early",],
             "fed_funds_rate", "DEAL_SIZE_USD", 3,
             "Binned Fed funds rate", "Avg VC Deal Size ($M)",
             "Avg VC Deal Size Against Binned FED Funds Rate")

graph_binned(data[data$deal_class == "late",],
             "fed_funds_rate", "DEAL_SIZE_USD", 3,
             "Binned Interest Rate", "Avg VC Deal Size ($M)",
             "Avg Late Stage Deal Size Against Binned Interest Rate")

graph_binned(late_stage_data,
             "tax_rate", "DEAL_SIZE_USD", 1,
             "Binned tax rate", "average deal size",
             "Avg Late Stage VC Deal Size Against Binned Tax Rate")

graph_binned(data.frame(aggregated_data$quarterly_agg),
             "fed_funds_rate", "num_of_deals", 4,
             "Binned Interest Rate", "Quarterly # of VC Deals",
             "Quarterly # of VC Deals Against Binned Interest Rate")

summary(lm(DEAL_SIZE_USD~tax_rate + factor(DEAL_STAGE) +
           factor(DEAL_YEAR) + fed_funds_rate, data = late_stage_data))

run_early_vs_late_regressions(data, "DEAL_SIZE_USD", "tax_rate")

agg_test = data %>% group_by(DEAL_YEAR, DEAL_STAGE,
                             deal_class, INVESTOR_COUNTRY) %>%
  summarise(count = n(), fed_funds_rate = mean(fed_funds_rate, na.rm = TRUE),
            tax_rate = mean(tax_rate, na.rm = TRUE),
            recession_ind = mean(recession_ind, na.rm = TRUE))

agg_test = agg_test[agg_test$count > 10,]

run_early_vs_late_regressions_2(data.frame(agg_test), "count", "tax_rate",
                                "Tax Rate")

run_early_vs_late_regressions(data,
                              "DEAL_SIZE_USD", "fed_funds_rate",
                              "FED Funds Rate")

run_early_vs_late_regressions_2(
  data.frame(agg_test), "count", "fed_funds_rate", "Interest Rate")

grouped_for_recession = data %>%
  group_by(DEAL_DATE = as.Date(as.yearqtr(df$DEAL_DATE, format="%Y-%m-%d")),
           deal_class) %>% 
  summarise(count = n(), recession_ind = mean(recession_ind, na.rm = TRUE))
  
recession_test = run_early_vs_late_regressions(data, "DEAL_SIZE_USD",
                                               "recession_ind", "Recession Indicator")


recession_reg_func(data.frame(agg_test),
                   "count", "recession_ind", "Recession Indicator")
