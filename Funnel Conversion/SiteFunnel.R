setwd("/DS/Funnel Conversion/Funnel")

library(ggplot2)
library(dplyr)
#import datasets
home_page_table <- read.csv("/DS/Funnel Conversion/Funnel/home_page_table.csv")
search_page_table <- read.csv("/DS/Funnel Conversion/Funnel/search_page_table.csv")
payment_page_table <- read.csv("/DS/Funnel Conversion/Funnel/payment_page_table.csv")
payment_confirmation_table <- read.csv("/DS/Funnel Conversion/Funnel/payment_confirmation_table.csv")
user_table <- read.csv("/DS/Funnel Conversion/Funnel/user_table.csv")

#lets see the conversion ratio
conversion_rate<- (nrow(payment_confirmation_table)/nrow(home_page_table))*100.0
# conversion rate is 0.5% which is very low for the site. conversion rate generally hovers around 2-3%
#lets see where the traffic comes from
prop.table(table(user_table$device))
# Looks like two thirds of the traffic comes from desktop and one-third traffic comes from Mobile.
qplot(user_table$sex)
#Looks like men and women are of same proportion.
prop.table(table(user_table$sex))
#lets see conversion at each stage
stage1_conversion<-nrow(search_page_table)/nrow(home_page_table)
stage2_conversion<-nrow(payment_page_table)/nrow(search_page_table)
stage3_conversion<-nrow(payment_confirmation_table)/nrow(payment_page_table)
names <- c("stage1_conversion", "stage2_conversion", "stage3_conversion")
values <- c(stage1_conversion, stage2_conversion, stage3_conversion)
values<- as.character(values)
conversionmatrix <- rbind(names, values) 
# conversion at stage1 is 50%, conversion at stage2 is 13%, conversionat 3rd stage is 7.5%
#lets convert tables
stage1tbl<-left_join(user_table, home_page_table)
stage2tbl<- left_join(stage1tbl, search_page_table, by ="user_id")
stage3tbl<- left_join(stage2tbl, payment_page_table, by ="user_id")
funneltbl<- left_join(stage3tbl, payment_confirmation_table, by="user_id")

colnames(funneltbl)[5]<- "home.page"
colnames(funneltbl)[6]<- "search.page"
colnames(funneltbl)[7]<- "payment.page"
colnames(funneltbl)[8]<- "payment_conf.page"
funneltbl$home.page <-  plyr::mapvalues(funneltbl$home.page, from= c("home_page"), to = c(1), warn_missing=TRUE)
funneltbl$search.page <- plyr::mapvalues(funneltbl$search.page, from = c("search_page"), to =c(1), warn_missing=TRUE)
funneltbl$payment.page <- plyr::mapvalues(funneltbl$payment.page, from = c("payment_page"), to =c(1), warn_missing=TRUE)
funneltbl$payment_conf.page <- plyr::mapvalues(funneltbl$payment_conf.page, from = c("payment_confirmation_page"), to =c(1), warn_missing=TRUE)


#Replace NAs with Zeros
funneltbl$search.page[which(is.na(funneltbl$search.page))]<-0
funneltbl$payment.page[which(is.na(funneltbl$payment.page))]<-0
funneltbl$payment_conf.page[which(is.na(funneltbl$payment_conf.page))]<-0

#Lets see which device gets more conversion rate
funneltbl %>%
  group_by(device) %>%
  summarise(convert= mean(payment_conf.page))
#Mobile App is getting much more converted than Desktop App

#Lets see which gender gets converted more

funneltbl %>%
  group_by(sex) %>%
  summarise(convert= mean(payment_conf.page))
# Women purchase a bit more than Men.

#Furthur Analysis
#1. Where Do each gender drop off?
