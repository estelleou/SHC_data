library(tidyverse)

raw <- 
read_delim("https://ticdata.treasury.gov/resource-center/data-chart-center/tic/Documents/shchistdat.txt",
           skip=6, col_names = T)

#data_cleaning
cleaned_data <-
  raw %>% 
  select(-`...1`) %>% 
  pivot_longer(-...2) %>% 
  rename(country = ...2,
         year = name) %>% 
  filter(country != "Country") %>% 
  mutate(year = str_sub(year, 1,4),
         year = as.double(year)) %>% 
  filter(year >= 2003) %>% 
  #creating an artificial column to join asset class identifiers to main data
  bind_cols(tibble(asset_class = rep(c("Total", "Equity", "LT Debt", "ST Debt"),4302))) %>% 
  #reformating the numbers to get rid of the comma separating in the number
  mutate(value =as.numeric(gsub(",", "", value)),
         #turning millions into billions 
         value = value/1000,
         year = as.Date(year)) %>% 
  #turning millions into billions 
  select(country, year, asset_class, value)

#export data in long-form that's not publicly available
write_csv(cleaned_data, "us_foreign_portfolio_claims_2020.csv")




