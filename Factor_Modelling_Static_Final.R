# Author: Nikhil Feroze Ghosh
# Contact: nikhil.ghosh@york.ac..uk
# All data sources from LSEG Refinitiv except where mentioned, Accessed on: 13/11/2025. 
# Data download Python script available within downloaded folder. Run in LSEG Codebook.

# 1. Installing Libraries

library(tidyverse)
library(data.table)
library(tidyfinance)
library(lubridate)
library(zoo)
library(scales)
library(ggrepel)
library(ggplot2)
library(RColorBrewer)
library(modelsummary)
library(usethis)

expt <- "SPX_STOXX"

# Does this show?

#2. Defining Functions

# Function to Calculate Descriptive Statistics of Portfolio

descriptive_statistics <- function(factors_data){

d_temp <- factors_data %>%
  group_by(Date) %>%
  summarise(
    ret_port = weighted.mean(ret, MktCap, na.rm = TRUE), 
    .groups = "drop"
  )

risk_free <- factors_data %>%
  group_by(Date) %>%
  summarise(
    RF = mean(RF)
  )

d_temp <- d_temp %>%
  mutate(downside = ifelse(ret_port < 0, ret_port^2, 0))

DESC_STATS <-
  as_tibble(cbind(
    total_return = sum(d_temp$ret_port)*100,
    annualized_return = ((1 + mean(d_temp$ret_port))^(1/14) - 1)*1000,
    annualized_risk = sd(d_temp$ret_port) * sqrt(12)*100,
    sharpe_ratio = mean((d_temp$ret_port - risk_free$RF), na.rm = TRUE) / sd(d_temp$ret_port),
    sortino_ratio = mean((d_temp$ret_port - risk_free$RF), na.rm = TRUE) / sqrt(mean(d_temp$downside))
  ))


return(DESC_STATS)

}

# Format Descriptive Stats

desc_format <- function(DESC_STATS) {

cols_desc <- c( "Total Return", 
               "Annualised Return", 
               "Annualised Risk", 
               "Sharpe Ratio",
               "Sortino Ratio")

rows_desc <- c("Agri-food Composite",
               "Inputs", 
               "Processing", 
               "Distribution",
               "None")

ch <- DESC_STATS %>%
  unlist()

DESC_STATS <- data.frame(matrix(ch, nrow = length(cols_desc), ncol = length(rows_desc)))
DESC_STATS <- DESC_STATS %>%
  transpose()

rownames(DESC_STATS) = rows_desc
colnames(DESC_STATS) = cols_desc

return(DESC_STATS)

}


# Function to Calculate Cumulative Returns of Portfolio

cumulative_statistics <- function(factors_data){
  
c_temp <- d_temp <- factors_data %>%
  filter(Date >= Start_Date & Date <= End_Date) %>%
  group_by(Date) %>%
  summarise(
    ret_port = weighted.mean(ret, MktCap, na.rm = TRUE)*100, 
    .groups = "drop"
  )

CUM_STATS <- c_temp %>%
  reframe(ret_port_cumul = cumsum(ret_port))

return(CUM_STATS)

}



# # Function to Calculate Inferential Statistics (Factor Models)
 
inferential_statistics <- function(factors_data) {
 
factors_data <- factors_data %>%
    group_by(Date) %>%
    summarise(
      ret_port = weighted.mean(ret, MktCap, na.rm = TRUE),
      mkt = mean(Mkt.RF),
      smb = mean(SMB),
      hml = mean(HML),
      rmw = mean(RMW),
      cma = mean(CMA),
      RF = mean(RF),
      .groups = "drop"
    ) %>%
  mutate(ret_port = ret_port - RF)
  
INF_STATS <- lm(ret_port ~ mkt + smb + hml + rmw + cma, data = factors_data)

return(INF_STATS)
   
}

# # Portfolio Returns
# 
# PR <- factors_data %>%
#   filter(Date >= Start_Date & Date <= End_Date) %>%
#   group_by(Date) %>%
#   summarise(
#     ret_port = weighted.mean(ret, MktCap, na.rm = TRUE) - mean(r_f_r), 
#     .groups = "drop"
#   )
# 
# # Beta1 - Benchmark Market Return
# 
# RMRF <- factors_data %>%
#   filter(Date >= Start_Date & Date <= End_Date) %>%
#   group_by(Date) %>%
#   summarise(
#     ret_mkt = mean(ret_mkt) - mean(r_f_r),
#     .groups = "drop"
#   )
# 
# # Beta2 - Small Minus Big
# 
# SMB <- factors_data %>% 
#   filter(Date >= Start_Date & Date <= End_Date) %>%
#   group_by(Date) %>%
#   mutate(
#     sort = if_else(MktCap <= quantile(MktCap, 0.20, na.rm = TRUE), "Small", "Big")
#   ) %>%
#   group_by(Date, sort) %>%
#   summarise(
#     r = weighted.mean(ret, MktCap, na.rm = TRUE), 
#     .groups = "drop"
#   ) %>%
#   pivot_wider(names_from = sort, values_from = r) %>%
#   mutate(smb = Small - Big)
# 
# 
# # Beta3 - High Minus Low
# 
# HML <- factors_data %>%
#   filter(Date >= Start_Date & Date <= End_Date) %>%
#   mutate(BktoMkt = (BkEq + DefTax) / MktCap) %>%
#   group_by(Date) %>%
#   mutate(
#     sort = if_else(BktoMkt <= quantile(BktoMkt, 0.30, na.rm = TRUE), "Low", "High")
#   ) %>%
#   group_by(Date, sort) %>%
#   summarise(
#     r = weighted.mean(ret, MktCap, na.rm = TRUE), 
#     .groups = "drop"
#   ) %>%
#   pivot_wider(names_from = sort, values_from = r) %>%
#   mutate(hml = Low - High)  
# 
# # Beta4 - Momentum
# 
# b4_temps <- factors_data %>%
#   group_by(Symbol) %>%
#   mutate(ret_12roll = rollapplyr(ret, width = 12, FUN = sum, partial = TRUE))
# 
# MOM <- b4_temps %>%
#   filter(Date >= Start_Date & Date <= End_Date) %>%
#   group_by(Date) %>%
#   mutate(
#     sort = if_else(ret_12roll <= quantile(ret_12roll,0.30, na.rm = TRUE), "Bottom", "Top")
#   ) %>%
#   group_by(Date, sort) %>%
#   summarise(
#     r = weighted.mean(ret, MktCap, na.rm = TRUE), 
#     .groups = "drop"
#   ) %>%
#   pivot_wider(names_from = sort, values_from = r) %>%
#   mutate(mom = Top - Bottom) 
# 
# l <- list(PR, RMRF, SMB, HML, MOM)
# four_factors <- l %>% 
#   reduce(full_join, by = "Date") %>%
#   select(Date, ret_port, ret_mkt, smb, hml, mom)
# 
# INF_STATS <- lm(ret_port ~ ret_mkt + smb + hml + mom, data = four_factors)
# 
# return(INF_STATS)
# }


#3. Data

Start_Date <- as.Date("2016-06-01")
End_Date <- as.Date("2026-01-01")

# i. Reading Data

financials_spx <- read.csv("SP500_Historical_Monthly.csv", header = TRUE) %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Date = floor_date(Date, "month")) %>%
  filter(Date >= Start_Date & Date <= End_Date) %>%
  drop_na(Date)
financials_stoxx <- read.csv("STOXX600_Historical_Monthly.csv", header = TRUE) %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Date = floor_date(Date, "month")) %>%
  filter(Date >= Start_Date & Date <= End_Date) %>%
  drop_na(Date)
ff_spx <- read.csv("mkt_ref_data_spx.csv", header = TRUE) %>%
  mutate(Date = ym(Date)) %>%
  mutate(across(-Date, ~ as.numeric(.) / 100))
ff_stoxx <- read.csv("mkt_ref_data_stoxx.csv", header = TRUE) %>%
  mutate(Date = ym(Date)) %>%
  mutate(across(-Date, ~ as.numeric(.) / 100))
sectors_spx <- read.csv("sectors_spx.csv", header = TRUE)
sectors_stoxx <- read.csv("sectors_stoxx.csv", header = TRUE)


# ii. Tidying Data

tidy_data <- function(financials, ff, sectors){
  
tidy_sample <- financials %>%
  mutate(Price = as.numeric(Price)) %>%
  mutate(MktCap = as.numeric(MktCap)) %>%
  group_by(Symbol) %>%
  mutate(ret = (Price / lag(Price)) - 1) %>%
  na.omit() %>%
  left_join(ff, by = "Date") %>%
  left_join(sectors, by = "Symbol")

return(tidy_sample)
  
}

sample_input_spx <- tidy_data(financials_spx, ff_spx, sectors_spx)

sample_input_stoxx <- tidy_data(financials_stoxx, ff_stoxx, sectors_stoxx)

count_sample <- sample_input_stoxx %>%
  group_by(Symbol) %>%
  summarise(n=n())


# 3.Building Portfolios

build_portfolios <- function(sample_input) {

market <- sample_input
  
inputs <- sample_input %>%
  filter(TRBC.Industry.Name == "Agricultural Chemicals" |
         TRBC.Activity.Name == "Agricultural Machinery") %>%
  arrange(Symbol)

processing <- sample_input %>%
  filter(TRBC.Industry.Name == "Fishing & Farming" |
           TRBC.Industry.Name == "Food Processing" |
           TRBC.Industry.Name == "Non-Alcoholic Beverages") %>%
  arrange(Symbol)

distribution <- sample_input %>%
  filter(TRBC.Industry.Name == "Food Retail & Distribution" | 
         TRBC.Industry.Name == "Restaurants & Bars") %>%
  arrange(Symbol)

agrifoodall <- rbind(inputs, processing, distribution)

gafs_port_list <- vector(mode = "list", length = 5)

gafs_port_list[[1]] <- agrifoodall
gafs_port_list[[2]] <- inputs
gafs_port_list[[3]] <- processing
gafs_port_list[[4]] <- distribution
gafs_port_list[[5]] <- market

return(gafs_port_list)
  
}

# Sectoral Porfolio Returns


portfolios_spx <- build_portfolios(sample_input_spx)

portfolios_stoxx <- build_portfolios(sample_input_stoxx)


# Divestment

divest_port_list_spx <- vector(mode = "list", length = 5)

divest_port_list_spx[[1]] <- anti_join(sample_input_spx, portfolios_spx[[1]], by = "Symbol")
divest_port_list_spx[[2]] <- anti_join(sample_input_spx, portfolios_spx[[2]], by = "Symbol")
divest_port_list_spx[[3]] <- anti_join(sample_input_spx, portfolios_spx[[3]], by = "Symbol")
divest_port_list_spx[[4]] <- anti_join(sample_input_spx, portfolios_spx[[4]], by = "Symbol")
divest_port_list_spx[[5]] <- portfolios_spx[[5]]

divest_port_list_stoxx <- vector(mode = "list", length = 5)

divest_port_list_stoxx[[1]] <- anti_join(sample_input_stoxx, portfolios_stoxx[[1]], by = "Symbol")
divest_port_list_stoxx[[2]] <- anti_join(sample_input_stoxx, portfolios_stoxx[[2]], by = "Symbol")
divest_port_list_stoxx[[3]] <- anti_join(sample_input_stoxx, portfolios_stoxx[[3]], by = "Symbol")
divest_port_list_stoxx[[4]] <- anti_join(sample_input_stoxx, portfolios_stoxx[[4]], by = "Symbol")
divest_port_list_stoxx[[5]] <- portfolios_stoxx[[5]]

DIVEST_RESULTS_SPX <- map(divest_port_list_spx, descriptive_statistics) %>%
  desc_format()

DIVEST_RESULTS_STOXX <- map(divest_port_list_stoxx, descriptive_statistics) %>%
  desc_format()

PORT_RESULTS_SPX <- map(portfolios_spx, descriptive_statistics) %>%
  desc_format()

PORT_RESULTS_STOXX <- map(portfolios_stoxx, descriptive_statistics) %>%
  desc_format()

INF_RESULTS_SPX <- inferential_statistics(portfolios_spx[[1]])

INF_RESULTS_STOXX <- map(divest_port_list_stoxx, inferential_statistics)
summary(INF_RESULTS_SPX)

