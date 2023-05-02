#This script contains the general functions used in analysis

dataclean_initial = function (df) {
  #round dates to start of each month
  df$DEAL_DATE = as.Date(
    as.yearmon(df$DEAL_DATE, format="%Y-%m-%d"))
  
  df = df[df$DEAL_SIZE_USD < 10000,]
  df = df[!is.na(df$DEAL_DATE),]
  
  df$DEAL_YEAR = as.numeric(format(
    as.Date(as.yearmon(df$DEAL_DATE, format="%Y-%m-%d")),
    '%Y'))
  
  #Adding FED funds rate
  funds_rate_data = read.csv(
    paste(path, "/data/fed_funds_rate_time_series.csv", sep = '')
  )
  funds_rate_data$date = as.Date(
    as.yearmon(funds_rate_data$date, format="%Y-%m-%d"))
  
  df = merge(df, funds_rate_data,
        by.x = "DEAL_DATE", by.y = "date", all.x = TRUE)
  
  #Adding in tax rate data
  tax_rate_data = read.csv(
    paste(path, "/data/tax_rate_by_country.csv", sep = '')
  )
  
  tax_rate_data = melt(
    setDT(tax_rate_data), id.vars = c("continent","country"),
    variable.name = "year")
  colnames(tax_rate_data)[4] = "tax_rate"
  tax_rate_data$year = as.numeric(str_sub(tax_rate_data$year, 2))
  
  df = merge(df, tax_rate_data,
             by.x = c("INVESTOR_COUNTRY", "DEAL_YEAR"),
             by.y = c("country", "year"), all.x = TRUE)
  
  df$deal_class = ifelse(df$DEAL_STAGE %in%
                           c("Series A", "Angel", "Seed", "Unspecified Round",
                             "Grant"),
                         "early", "late")
  
  df = df[df$DEAL_DATE > as.Date('1999-01-01') &
            df$DEAL_DATE < as.Date('2023-04-01'),]
  
  df$recession_ind = ifelse(as.numeric(format(df$DEAL_DATE,'%Y')) %in%
                              c(2001, 2002, 2009, 2020), 1, 0)
  
  non_recession_dates = c(
    as.Date("2001-01-01", format="%Y-%m-%d"),
    as.Date("2009-08-01", format="%Y-%m-%d"),
    as.Date("2009-09-01", format="%Y-%m-%d"),
    as.Date("2009-10-01", format="%Y-%m-%d"),
    as.Date("2009-11-01", format="%Y-%m-%d"),
    as.Date("2009-12-01", format="%Y-%m-%d"),
    as.Date("2020-01-01", format="%Y-%m-%d"),
    as.Date("2020-05-01", format="%Y-%m-%d"),
    as.Date("2020-06-01", format="%Y-%m-%d"),
    as.Date("2020-07-01", format="%Y-%m-%d"),
    as.Date("2020-08-01", format="%Y-%m-%d"),
    as.Date("2020-09-01", format="%Y-%m-%d"),
    as.Date("2020-10-01", format="%Y-%m-%d"),
    as.Date("2020-11-01", format="%Y-%m-%d"),
    as.Date("2020-12-01", format="%Y-%m-%d")
  )
  
  non_recession_deals = df[df$DEAL_DATE %in% non_recession_dates,]$recession_ind
  
  df[df$DEAL_DATE %in% non_recession_dates,]$recession_ind = 
    rep(0, length(non_recession_deals))
  
  return(df)
}

aggregated_deals = function (df) {
  
  grouped_df_month = df %>%
    group_by(DEAL_DATE) %>%
    summarise(deal_size = mean(DEAL_SIZE_USD, na.rm = TRUE),
              num_of_deals = n(),
              recession_ind = mean(recession_ind, na.rm = TRUE),
              fed_funds_rate = mean(fed_funds_rate, na.rm = TRUE),
              DEAL_YEAR = mean(DEAL_YEAR, na.rm = TRUE))
  
  grouped_df_quarter_type = df %>%
    group_by(DEAL_DATE = as.Date(as.yearqtr(df$DEAL_DATE, format="%Y-%m-%d")),
             DEAL_STAGE) %>%
    summarise(deal_size = mean(DEAL_SIZE_USD, na.rm = TRUE),
              num_of_deals = n(),
              recession_ind = mean(recession_ind, na.rm = TRUE),
              fed_funds_rate = mean(fed_funds_rate, na.rm = TRUE),
              DEAL_YEAR = mean(DEAL_YEAR, na.rm = TRUE))
  
  grouped_df_quarter = df %>%
    group_by(DEAL_DATE = as.Date(as.yearqtr(df$DEAL_DATE, format="%Y-%m-%d"))) %>%
    summarise(deal_size = mean(DEAL_SIZE_USD, na.rm = TRUE),
              num_of_deals = n(),
              recession_ind = mean(recession_ind, na.rm = TRUE),
              fed_funds_rate = mean(fed_funds_rate, na.rm = TRUE),
              DEAL_YEAR = mean(DEAL_YEAR, na.rm = TRUE))
  
  return(list(
    "monthly_agg" = grouped_df_month,
    "quarterly_agg" = grouped_df_quarter,
    "quarterly_type_agg" = grouped_df_quarter_type))
}

plot_time_series = function(df, x_var, y_var, title, xlab, ylab) {
  ggplot(df, aes(x = {{x_var}}, y = {{y_var}})) + geom_line() +
    annotate("rect", xmin=as.Date("2000-12-01"), xmax= as.Date("2001-11-01"),
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") +
    annotate("rect", xmin=as.Date("2007-12-01"), xmax= as.Date("2009-06-01"),
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") +
    annotate("rect", xmin=as.Date("2020-02-01"), xmax= as.Date("2020-04-01"),
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") +
    labs(x = xlab, y = ylab, title = title) +
    theme(plot.title = element_text(hjust = 0.5))
}

growth_rate_func = function(df, var) {
  df[,paste(var, "growth_rate", sep = '_')] = rep(NA, nrow(df)) 
  for (i in 2:nrow(df)) {
    df[i,paste(var, "growth_rate", sep = '_')] =
      (df[i,var] - df[i - 1,var]) / df[i - 1,var]
  } 
  
  df = na.omit(df)
  
  return(df)
}

growth_rate_with_type_func = function(df, var) {
  df = na.omit(df)
  df[,paste(var, "growth_rate", sep = '_')] = rep(NA, nrow(df))
  for (i in 2:nrow(df)) {
    if (i %% 25 == 0) {
      print(sprintf("Done with row: %.0f", i)) 
    }
    same_type_df = df[df$DEAL_STAGE == df[i,]$DEAL_STAGE,]
    
    current_ind = 
      which(same_type_df$DEAL_DATE == df[i,]$DEAL_DATE)
    
    previous_period_var = same_type_df[current_ind - 1,var]
    if (current_ind > 1) {
      df[i, paste(var, "growth_rate", sep = '_')] = 
        df[i, var] - previous_period_var
    }
  }
  
  df = na.omit(df)
  
  return(df)
}

graph_binned = function(df, x_axis, y_axis, digs,
                                     label_x, label_y, title) {
  df$x = df[,x_axis]
  df$y = df[,y_axis]
  df$x_agg = round(df$x * {{digs}}, digits = 0) / {{digs}}
  print(mean(df$y, na.rm = TRUE))
  
  binned_df = df %>% 
    group_by(x_agg) %>%
    summarise(mean_y = mean(y, na.rm = TRUE),
              deal_number = n())
  print(binned_df)
  ggplot(binned_df, aes(x=x_agg, y=mean_y)) + 
    geom_point() + geom_smooth(method = "lm", se = FALSE) + 
    labs(x = label_x, y = label_y, title = title) +
    theme(plot.title = element_text(hjust = 0.5))
}

run_early_vs_late_regressions = function (df, dep, reg, reg_name) {
  df$dep = df[,dep]
  df$reg = df[,reg]
  
  all_fixed_effects_late = 
    feols(dep~reg | DEAL_YEAR + DEAL_STAGE + PORTFOLIO_COMPANY_INDUSTRY,
          df[df$deal_class == "late",])
  time_and_stage_fixed_effects_late = 
    feols(dep~reg | DEAL_YEAR + DEAL_STAGE,
          df[df$deal_class == "late",])
  time_fixed_effects_late = 
    feols(dep~reg | DEAL_YEAR,
          df[df$deal_class == "late",])
  
  all_fixed_effects_early = 
    feols(dep~reg | DEAL_YEAR + DEAL_STAGE + PORTFOLIO_COMPANY_INDUSTRY,
          df[df$deal_class == "early",])
  time_and_stage_fixed_effects_early = 
    feols(dep~reg | DEAL_YEAR + DEAL_STAGE,
          df[df$deal_class == "early",])
  time_fixed_effects_early = 
    feols(dep~reg | DEAL_YEAR,
          df[df$deal_class == "early",])
  
  return_var = list(all_fixed_effects_late,
                    time_and_stage_fixed_effects_late,
                    time_fixed_effects_late,
                    all_fixed_effects_early,
                    time_and_stage_fixed_effects_early,
                    time_fixed_effects_early)
    
  return(list("regressions" = return_var,
              "tex" = texreg(return_var,
                             custom.coef.names = 
                               c(reg_name),
                             custom.model.names = c("All FE", "Time + Stage FE",
                                                    "Time FE", "All FE",
                                                    "Time + Stage FE", "Time FE"),
                             custom.header = list("Late VC Deals" = 1:3,
                                                  "Early VC Deals" = 4:6))))
}

run_early_vs_late_regressions_2 = function (df, dep, reg, reg_name) {
  df$dep = df[,dep]
  df$reg = df[,reg]
  print(df$dep)
  time_and_stage_fixed_effects_late = 
    feols(dep~reg | DEAL_YEAR + DEAL_STAGE,
          df[df$deal_class == "late",])
  time_fixed_effects_late = 
    feols(dep~reg | DEAL_YEAR,
          df[df$deal_class == "late",])

  time_and_stage_fixed_effects_early = 
    feols(dep~reg | DEAL_YEAR + DEAL_STAGE,
          df[df$deal_class == "early",])
  time_fixed_effects_early = 
    feols(dep~reg | DEAL_YEAR,
          df[df$deal_class == "early",])
  
  return_var = list(time_and_stage_fixed_effects_late,
                    time_fixed_effects_late,
                    time_and_stage_fixed_effects_early,
                    time_fixed_effects_early)
  
  return(list("regressions" = return_var,
              "tex" = texreg(return_var,
                             custom.coef.names = 
                               c(reg_name),
                             custom.model.names = c("Time + Stage FE",
                                                    "Time FE",
                                                    "Time + Stage FE", "Time FE"),
                             custom.header = list("Late VC Deals" = 1:2,
                                                  "Early VC Deals" = 3:4))))
}

recession_reg_func = function(df, dep, reg, reg_name) {
    df$dep = df[,dep]
    df$reg = df[,reg]
    print(df$dep)
    
    time_and_stage_fixed_effects_late = 
      feols(dep~reg | DEAL_YEAR, df)
    time_fixed_effects_late = 
      lm(dep~reg, df)
    
    time_and_stage_fixed_effects_early = 
      feols(dep~reg | DEAL_YEAR, df)
    time_fixed_effects_early = 
      lm(dep~reg, df)
    
    return_var = list(time_and_stage_fixed_effects_late,
                      time_fixed_effects_late,
                      time_and_stage_fixed_effects_early,
                      time_fixed_effects_early)
    
    return(list("regressions" = return_var,
                "tex" = texreg(return_var,
                               custom.model.names = c("Time + Stage FE",
                                                      "Time FE",
                                                      "Time + Stage FE", "Time FE"),
                               custom.header = list("Late VC Deals" = 1:2,
                                                    "Early VC Deals" = 3:4))))
  }

