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
cin_therp$Telehealth = rep(0, dim(cin_therp)[1])
mt_product$Telehealth = rep(1,dim(mt_product)[1])
sn_product$Telehealth = rep(1,dim(sn_product)[1])

cin_therp$Telehealth_Therp = rep(0, dim(cin_therp)[1])
mt_product$Telehealth_Therp = rep(1,dim(mt_product)[1])
sn_product$Telehealth_Therp = rep(2,dim(sn_product)[1])


### Now stack
telehealth_dat = rbind(cin_therp, mt_product, sn_product)
telehealth_dat
telehealth_dat = telehealth_dat[,c(1:2,9:10)]
colnames(telehealth_dat) = c("Year.Month", "Productivity", "Telehealth", "Telehealth_Therp")

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
telehealth_dat$Time = ifelse(telehealth_dat$Year.Month >= "2019-11-01", 1, 0)
telehealth_dat_complete = na.omit(telehealth_dat)
dim(telehealth_dat_complete)
telehealth_dat_complete
```
Try graphing by treatment
Therapists CIN do not include either Telehealth therapists
Break out for each therapist, add trend line

SN = Mid Jan
P = Mid November
```{r}
library(ggplot2)
telehealth_dat_complete$Telehealth = as.factor(telehealth_dat_complete$Telehealth)
ggplot(telehealth_dat_complete, aes(Year.Month, Productivity, colour = Telehealth))+
  geom_point()+ 
  geom_line()+
  ggtitle("Productivity for Telehealth Therapists for CIN vs. CIN Therapists")+
  geom_vline(xintercept = telehealth_dat_complete$Year.Month[50], colour="black")+
  xlab("Date")
```
Linear model to evaluate if differences exist
```{r}

telehealth_linear_model = lm(Productivity  ~ Treatment*Time, data = telehealth_dat_complete)
summary(telehealth_linear_model)

### Way too much error to manual t-tests

80/12
```
Try manual t-test so
x = mean
th = therapist
t = time
tele =  telehealth

diff_diff =  (x_th_1 - x_th_t0) - (x_tele_t1 - x_tele_t0)

diff_diff / sd(diff_diff)
```{r}
library(descr)
x_th_0 = subset(telehealth_dat_complete, Telehealth == 1 & Time == 0)
x_th_0 = mean(x_th_0$Productivity)


```


