library(tidyverse)
library(lme4)
library(rms)
library(doBy)
library(gtsummary)
library("zoo")
library("lubridate")
## import data
walmart <- read.csv("/Users/zengqingyi/Desktop/7430 correlated/project/walmart-sales-dataset-of-45stores.csv")
str(walmart)
# Convert dates to quarter 
walmart$Date <- parse_date_time(walmart$Date, orders = c('mdy', 'dmy'))
walmart$my_dates_quarters <- as.yearqtr(walmart$Date,           # Convert dates to quarterly
                                format = "%Y-%m-%d")
walmart$Weekly_Sales<-walmart$Weekly_Sales/1000000
walmart$Holiday_Flag <- as.factor(walmart$Holiday_Flag)

walmart <- subset(walmart,my_dates_quarters!='2012 Q4')
walmart$my_dates_quarters <- as.factor(walmart$my_dates_quarters)
## build lmm for three scenarios
## main effects
mod1 = lmer(Weekly_Sales ~ my_dates_quarters + Unemployment + Holiday_Flag + Fuel_Price+
            (1 | Store), data=walmart, REML=F)
summary(mod1)
## main effects + interaction between holifay and unemployment rate
mod2 = lmer(Weekly_Sales ~ my_dates_quarters + Unemployment + Holiday_Flag + Fuel_Price + Holiday_Flag:Unemployment  + ( 1| Store), data=walmart, REML=F)
summary(mod2)
## main effects + interaction between fuel price and unemployment rate
mod3 = lmer(Weekly_Sales ~ my_dates_quarters + Unemployment + Fuel_Price + Holiday_Flag + Fuel_Price:Unemployment + ( 1| Store), data=walmart, REML=F)
summary(mod3)

## get estimate 95%CIS pvalue and wrangle results by gt
fit1 <- esticon(mod1, diag(14))
rownames(fit1) <- c("Intercept","2010 Q2","2010 Q3","2010 Q4","2011 Q1","2011 Q2","2011 Q3","2011 Q4","2012 Q1","2012 Q2","2012 Q3", "Unemployment rate(%)","Holidayflag(1=yes,0=no)","Fuel Price" )
fit1 <- round(fit1, 3)
fit1_refined <- fit1
fit1_refined[1,4] <- "<0.001"
fit1_refined[4,4] <- "<0.001"
fit1_refined[5,4] <- "<0.001"
fit1_refined[7,4] <- "<0.001"
fit1_refined[8,4] <- "<0.001"
fit1_refined[9,4] <- "<0.001"
fit1_refined[10,4] <- "<0.001"
fit1_refined[11,4] <-"<0.001"
fit1_refined[12,4] <- "<0.001"
fit1_refined %>% 
  gt(rownames_to_stub = TRUE) %>%
  cols_label(
    estimate="Estimate",
    lwr= "Lwr",
    upr="Upr",
    p.value="P.value",
  ) %>% 
  tab_footnote("Linear mixed model1: Unemployment rate effect on weekly sales on 45 stores over time adjusting for holiday and fuel price main effects" ) %>% 
  tab_stubhead(label = "Coefficient") %>% 
  cols_move_to_end(p.value) %>% 
  cols_hide(columns = c(beta0,df,std.error,statistic))%>% 
  tab_header(title = "Summary Regression Results for LMM1") %>% 
  tab_spanner(label="95% CI",columns=c(lwr,upr))

fit2 <- esticon(mod2, diag(15))
rownames(fit2) <- c("intercept","2010 Q2","2010 Q3","2010 Q4","2011 Q1","2011 Q2","2011 Q3","2011 Q4","2012 Q1","2012 Q2","2012 Q3", "Unemployment rate (%)","Holidayflag (1=yes,0=no)","Fuel price","Unemployment rate:holidayflag (interaction)")
fit2 <- round(fit2, 3)
fit2_refined <- fit2
fit2_refined[1,4] <- "<0.001"
fit2_refined[4,4] <- "<0.001"
fit2_refined[5,4] <- "<0.001"
fit2_refined[7,4] <- "<0.001"
fit2_refined[8,4] <- "<0.001"
fit2_refined[9,4] <- "<0.001"
fit2_refined[10,4] <- "<0.001"
fit2_refined[11,4] <-"<0.001"
fit2_refined[12,4] <- "<0.001"
rownames(fit2_refined)[rownames(fit2_refined) == "intercept"] = "Intercept"
fit2_refined%>%
  gt(rownames_to_stub = TRUE) %>%
  cols_label(
    estimate="Estimate",
    lwr= "Lwr",
    upr="Upr",
    p.value="P.value",
  ) %>% 
  tab_footnote("Linear mixed model2: Unemployment rate effect on weekly sales on 45 stores over time adjusting for holiday" ) %>% 
  tab_stubhead(label = "Coefficient") %>% 
  cols_move_to_end(p.value) %>% 
  cols_hide(columns = c(beta0,df,std.error,statistic))%>% 
  tab_header(title = "Summary Regression Results for LMM2") %>% 
  tab_spanner(label="95% CI",columns=c(lwr,upr))

  

fit3 <- esticon(mod3, diag(15))
fit3
summary(mod3)
rownames(fit3) <- c("Intercept","2010 Q2","2010 Q3","2010 Q4","2011 Q1","2011 Q2","2011 Q3","2011 Q4","2012 Q1","2012 Q2","2012 Q3", "Unemployment rate (%)","Fuel Price","Holidayflag (1=yes,0=no)","Unemployment rate:Fuel Price (interaction)")
fit3 <- round(fit3, 3)
fit3
fit3_refined <- fit3
fit3_refined[2,4] <- "0.860"
fit3_refined[1,4] <- "<0.001"
fit3_refined[4,4] <- "<0.001"
fit3_refined[5,4] <- "<0.001"
fit3_refined[7,4] <- "<0.001"
fit3_refined[8,4] <- "<0.001"
fit3_refined[9,4] <- "<0.001"
fit3_refined[10,4] <- "<0.001"
fit3_refined[11,4] <-"<0.001"
rownames(fit3_refined)[rownames(fit3_refined) == "intercept"] = "Intercept"
fit3_refined%>%
  gt(rownames_to_stub = TRUE) %>%
  cols_label(
    estimate="Estimate",
    lwr= "Lwr",
    upr="Upr",
    p.value="P.value",
  ) %>% 
  tab_footnote("Linear mixed model3: Unemployment rate effect on weekly sales on 45 stores over time adjusting for fuel price" ) %>% 
  tab_stubhead(label = "Coefficient") %>% 
  cols_move_to_end(p.value) %>% 
  cols_hide(columns = c(beta0,df,std.error,statistic))%>% 
  tab_header(title = "Summary Regression Results for LMM3") %>% 
  tab_spanner(label="95% CI",columns=c(lwr,upr))



