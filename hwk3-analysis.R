## Title: ECON 470 HW3
## Author: Ben Yang
## Date Created: 3/1/2023
## Date Edited: 3/13/2023
## Descriptions: This file renders/runs all relevant R code for the assignment

## Preliminaries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr,
               knitr, data.table, kableExtra, tinytex, scales,
               lubridate, stringr, gdata,
               readxl, 
               modelsummary, fixest, AER)

## Read data and set workspace for knitr ---------------------------------------

cdc_tax_data <- read_rds('data/output/cdc-tax-data.rds')

## Create objects for markdown -------------------------------------------------

## Question 1 Proportion of States with a Change in Their Cigarette Tax --------

q1.data <- cdc_tax_data %>%
  group_by(state) %>%
  summarise(difference = diff(tax_state), year = c(1971:2019)) %>%
  mutate(change = difference!=0) %>%
  group_by(year) %>%
  summarise(change_ct = sum(change), change_pct = change_ct/length(state)*100) %>%
  filter(year %in% 1971:1985)

q1.plot <- q1.data  %>%
  ggplot(aes(x = factor(year), y = change_pct)) + geom_col(fill = "dodgerblue4") +
  scale_x_discrete(breaks = seq(1970, 1985, 1)) +
  ylim(0,100) +
  geom_text(label = round(q1.data$change_pct,1), size = 3, nudge_x = 0, nudge_y = 5, check_overlap = TRUE) +
  labs(x = "Year", y = "Proportion of States (%)", Title = "Proportion of States with a Change in Their Cigarette Tax in Each Year from 1970 to 1985") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 2 Average Tax on Cigarettes & Average Price of a Pack of Cigarettes-

q2.data <- cdc_tax_data %>%
  filter(Year %in% 1970:2018) %>%
  group_by(Year) %>%
  summarise(avg_tax = mean(tax_cpi), avg_price = mean(price_cpi)) %>%
  pivot_longer(!Year, names_to = "Average", values_to = "Value")

q2.plot <- q2.data %>%
  ggplot(aes(x = Year, y = Value, color = Average)) + geom_point() + geom_line() +
  scale_color_manual(values = c("dodgerblue4", "dodgerblue1"), labels = c("Average Price", "Average Tax")) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  geom_text(label = round(q2.data$Value,2), size = 2, nudge_x = 0, nudge_y = 0.25, check_overlap = TRUE) +
  geom_text(data = q2.data %>% filter(Year == 2016), aes(label = c("Mean Price", "Mean Tax"), x = Year, y = Value-0.5)) +
  labs(x = "Year", y = "Average Tax and Price (2012 dollars)", Title = "Average Tax (in 2012 dollars) on Cigarettes and Average Price of a Pack of Cigarettes in Each Year from 1970 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 3 States with Highest Increases in Cigarette Prices-----------------

cigPrice <- cdc_tax_data %>%
  filter(Year == 1970 | Year == 2018) %>%
  group_by(state) %>%
  summarise(diff_tax_dollar = diff(tax_dollar), diff_price_cpi = diff(price_cpi))

cigPrice_high5state <- cigPrice %>% select(-diff_tax_dollar) %>% arrange(desc(diff_price_cpi)) %>% head(5)
colnames(cigPrice_high5state) <- c("State", "Increases in Cigarette Prices")

q3.data <- cdc_tax_data %>% 
  filter(state %in% cigPrice_high5state$State) %>%
  group_by(Year, state) %>%
  summarise(avg_sales_per_capita = mean(sales_per_capita)) %>%
  mutate(state = factor(state, levels = cigPrice_high5state$State))

q3.plot <- q3.data %>%
  ggplot(aes(x = Year, y = avg_sales_per_capita, color = state)) +
  geom_point(alpha = 0.75) + geom_line(alpha = 0.75) +
  scale_color_manual(values = c("deepskyblue", "dodgerblue1", "dodgerblue3", "dodgerblue4", "darkblue"), labels = levels(factor(q3.data$state))) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  labs(x = "Year", y = "Average Pack Sales per Capita", Title = "Average Pack Sales per Capita in Each Year for District of Columbia, New York, Rhode Island, Massachusetts, Connecticut from 1970 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "right",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 4 States with Lowest Increases in Cigarette Prices -----------------

cigPrice_low5state <- cigPrice %>% select(-diff_tax_dollar) %>% arrange(diff_price_cpi) %>% head(5)
colnames(cigPrice_low5state) <- c("State", "Increases in Cigarette Prices")

q4.data <- cdc_tax_data %>% 
  filter(state %in% cigPrice_low5state$State) %>%
  group_by(Year, state) %>%
  summarise(avg_sales_per_capita = mean(sales_per_capita)) %>%
  mutate(state = factor(state, levels = cigPrice_low5state$State))

q4.plot <- q4.data %>%
  ggplot(aes(x = Year, y = avg_sales_per_capita, color = state)) +
  geom_point(alpha = 0.75) + geom_line(alpha = 0.75) +
  scale_color_manual(values = c("goldenrod1", "orange", "darkorange", "darkorange3", "darkorange4"), labels = levels(factor(q4.data$state))) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  labs(x = "Year", y = "Average Pack Sales per Capita", Title = "Average Pack Sales per Capita in Each Year for Missouri, Tennessee, North Dakota, Alabama, Georgia from 1970 to 2019") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "right",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 5 Compare ----------------------------------------------------------

q5.data <- cdc_tax_data %>% 
  filter(state %in% cigPrice_low5state$State | state %in% cigPrice_high5state$State) %>%
  group_by(Year, state) %>%
  summarise(avg_sales_per_capita = mean(sales_per_capita)) %>%
  mutate(state = factor(state, levels = c(cigPrice_high5state$State, cigPrice_low5state$State))) %>%
  mutate(high_low = ifelse(state %in% cigPrice_low5state$State, "L", "H"))
levels(factor(q5.data$high_low))

q5.plot <- q5.data %>%
  ggplot(aes(x = Year, y = avg_sales_per_capita, color = high_low)) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(name = "Group", values = c("dodgerblue1", "orange"),
                     labels = c("5 with Highest Increases", "5 with Lowest Increases")) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  labs(x = "Year", y = "Average Pack Sales per Capita", Title = "Average Pack Sales per Capita in Each Year for Selected States from 1970 to 2018") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 6 Regress log sales on log prices ----------------------------------

data_1970_1990 <- cdc_tax_data %>%
  mutate(ln_sales = log(sales_per_capita), ln_price = log(price_cpi)) %>%
  filter(Year %in% 1970:1990)

m1 <- lm(formula = ln_sales ~ ln_price, data = data_1970_1990)

## Question 7 Regress log sales on log prices, cigarette tax as instrument -----

m1.iv <- feols(fml = ln_sales ~ 1 | ln_price ~ tax_cpi, data = data_1970_1990)
m1.iv_alt <- ivreg(formula = ln_sales ~ ln_price | tax_cpi, data = data_1970_1990)

## Question 8 Two-stage Equivalence --------------------------------------------

m1.step1 <- lm(formula = ln_price ~ tax_cpi, data = data_1970_1990)
m1.step1.pricehat <- predict(m1.step1)
m1.step2 <- lm(formula = ln_sales ~ m1.step1.pricehat, data = data_1970_1990)
m1.rf <- lm(formula = ln_sales ~ tax_cpi, data = data_1970_1990)

### Fixed Effects --------------------------------------------------------------

m1.fe1.step1 <- feols(fml = ln_price ~ tax_cpi | Year, data = data_1970_1990)
m1.fe2.step1 <- feols(fml = ln_price ~ tax_cpi | state, data = data_1970_1990)
m1.fe3.step1 <- feols(fml = ln_price ~ tax_cpi | Year + state, data = data_1970_1990)

m1.fe1.rf <- feols(fml = ln_sales ~ tax_cpi | Year, data = data_1970_1990)
m1.fe2.rf <- feols(fml = ln_sales ~ tax_cpi | state, data = data_1970_1990)
m1.fe3.rf <- feols(fml = ln_sales ~ tax_cpi | Year + state, data = data_1970_1990)

m1.iv.fe1 <- feols(fml = ln_sales ~ 1 | Year | ln_price ~ tax_cpi, data = data_1970_1990)
m1.iv.fe2 <- feols(fml = ln_sales ~ 1 | state | ln_price ~ tax_cpi, data = data_1970_1990)
m1.iv.fe3 <- feols(fml = ln_sales ~ 1 | Year + state | ln_price ~ tax_cpi, data = data_1970_1990)

## Question 9 Repeat for 1991 to 2015 ------------------------------------------

data_1991_2015 <- cdc_tax_data %>%
  mutate(ln_sales = log(sales_per_capita), ln_price = log(price_cpi)) %>%
  filter(Year %in% 1991:2015)

m2 <- lm(formula = ln_sales ~ ln_price, data = data_1991_2015)

m2.iv <- feols(fml = ln_sales ~ 1 | ln_price ~ tax_cpi, data = data_1991_2015)
m2.iv_alt <- ivreg(formula = ln_sales ~ ln_price | tax_cpi, data = data_1991_2015)

m2.step1 <- lm(formula = ln_price ~ tax_cpi, data = data_1991_2015)
m2.step1.pricehat <- predict(m2.step1)
m2.step2 <- lm(formula = ln_sales ~ m2.step1.pricehat, data = data_1991_2015)
m2.rf <- lm(formula = ln_sales ~ tax_cpi, data = data_1991_2015)

### Fixed Effects --------------------------------------------------------------

m2.fe1.step1 <- feols(fml = ln_price ~ tax_cpi | Year, data = data_1991_2015)
m2.fe2.step1 <- feols(fml = ln_price ~ tax_cpi | state, data = data_1991_2015)
m2.fe3.step1 <- feols(fml = ln_price ~ tax_cpi | Year + state, data = data_1991_2015)

m2.fe1.rf <- feols(fml = ln_sales ~ tax_cpi | Year, data = data_1991_2015)
m2.fe2.rf <- feols(fml = ln_sales ~ tax_cpi | state, data = data_1991_2015)
m2.fe3.rf <- feols(fml = ln_sales ~ tax_cpi | Year + state, data = data_1991_2015)

m2.iv.fe1 <- feols(fml = ln_sales ~ 1 | Year | ln_price ~ tax_cpi, data = data_1991_2015)
m2.iv.fe2 <- feols(fml = ln_sales ~ 1 | state | ln_price ~ tax_cpi, data = data_1991_2015)
m2.iv.fe3 <- feols(fml = ln_sales ~ 1 | Year + state | ln_price ~ tax_cpi, data = data_1991_2015)

## Save data for markdown ------------------------------------------------------

#rm(list=c("cdc_tax_data"))
save.image("Hwk3_workspace.Rdata")
