---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Load data
```{r}
library(lubridate)
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/TelehealthCIN")
cin_therp = read.csv("CINTherapists_Productivity.csv", header = TRUE)
mt_product = read.csv("MT_Productivity.csv", header = TRUE)
sn_product = read.csv("SN_Productivity.csv", header = TRUE)
```
Create a therp var 
Then stack
Need to get rid of not need vars and missing variables.  
Then get date right
Get rid of percentage for product
Then create an intervention var
```{r}
cin_therp$treatment = rep(0, dim(cin_therp)[1])
mt_product$treatment = rep(1,dim(mt_product)[1])
sn_product$treatment = rep(1,dim(sn_product)[1])

### Now stack
telehealth_dat = rbind(cin_therp, mt_product, sn_product)
telehealth_dat
telehealth_dat = telehealth_dat[,c(1:2,9)]
colnames(telehealth_dat) = c("Year.Month", "Productivity", "Treatment")

### Adjust this
telehealth_dat$Year.Month = str_replace_all(telehealth_dat$Year.Month, c("Jan" = "1", "Feb"="2", "Mar"="3", "Apr"="4", "May"="5", "Jun"="6", "Jul"="7", "Aug"="8", "Sep" = "9", "Oct"="10", "Nov"="11", "Dec"="12"))
head(telehealth_dat)
#We do not have a complete date.  Need to add the day just assuming that day is the 1st for each data point.
telehealth_dat$Year.Month = paste("1-",telehealth_dat$Year.Month, sep = "")
head(telehealth_dat)
telehealth_dat$Year.Month = dmy(telehealth_dat$Year.Month)
telehealth_dat

## try getting rid of non-digits
telehealth_dat$Productivity = gsub("%", "", telehealth_dat$Productivity)
telehealth_dat
### Now get rid of Zeros and -
telehealth_dat$Productivity = gsub("-", "", telehealth_dat$Productivity)
telehealth_dat$Productivity = gsub("0.00", "", telehealth_dat$Productivity)
telehealth_dat$Productivity  = as.numeric(telehealth_dat$Productivity)
### change this later but add a treatment date 2019-06-01
telehealth_dat$Time = ifelse(telehealth_dat$Year.Month >= "2019-06-01", 1, 0)
telehealth_dat_complete = na.omit(telehealth_dat)
dim(telehealth_dat_complete)


```
Try graphing by treatment
```{r}
library(ggplot2)
telehealth_dat$Treatment = as.factor(telehealth_dat$Treatment)
ggplot(telehealth_dat, aes(Year.Month, Productivity, colour = Treatment))+
  geom_line()

```
Linear model to evaluate if differences exist
```{r}

telehealth_linear_model = lm(Productivity  ~ Treatment*Time, data = telehealth_dat_complete)
summary(telehealth_linear_model)

### Way too much error to manual t-tests

80/12
```


