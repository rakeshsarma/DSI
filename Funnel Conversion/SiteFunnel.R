setwd("/DS/Funnel Conversion/Funnel")

library(ggplot2)
library(dplyr)
#import datasets
home_page_table <- read.csv("/DS/Funnel Conversion/Funnel/home_page_table.csv", stringsAsFactors = F)
search_page_table <- read.csv("/DS/Funnel Conversion/Funnel/search_page_table.csv", stringsAsFactors = F)
payment_page_table <- read.csv("/DS/Funnel Conversion/Funnel/payment_page_table.csv", stringsAsFactors = F)
payment_confirmation_table <- read.csv("/DS/Funnel Conversion/Funnel/payment_confirmation_table.csv" ,stringsAsFactors = F)
user_table <- read.csv("/DS/Funnel Conversion/Funnel/user_table.csv" , stringsAsFactors = F)

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
View(conversionmatrix)
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
#funneltbl$search.page[is.na(funneltbl$search.page)]<-0

funneltbl$search.page[which(is.na(funneltbl$search.page))]<-0
funneltbl$payment.page[which(is.na(funneltbl$payment.page))]<-0
funneltbl$payment_conf.page[which(is.na(funneltbl$payment_conf.page))]<-0
#Change everything to numeric
funneltbl$home.page<- as.numeric(funneltbl$home.page)
funneltbl$search.page<- as.numeric(funneltbl$search.page)
funneltbl$payment.page<- as.numeric(funneltbl$payment.page)
funneltbl$payment_conf.page<- as.numeric(funneltbl$payment_conf.page)

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
#1. Where Do desktop users drop off?
funneltbl %>%
  group_by(device) %>%
  summarise(convert_to_searchpage=mean(search.page))
  
funneltbl %>%
  group_by(device) %>%
  summarise(convert_to_paymentpage=mean(payment.page))
# Conversion rate is dropping at payment page. So, users like mobile interface rather than Desktop. 
# Ask product to run an A/B test on search page layouts.
#2. Which gender is dropping off?

funneltbl %>%
  group_by(sex) %>%
  summarise(convert_to_searchpage=mean(search.page))

funneltbl %>%
  group_by(sex) %>%
  summarise(convert_to_paymentpage=mean(payment.page))

# Site looks to be female centric. So female conversion to payment page is slightly more than male.
# Tell marketing to market to female users.

# Lest see who's more price conscious. Females or males?
convert_to_payment_matrix<-funneltbl %>%
  group_by(sex) %>%s
  summarise(convert_to_paymentpage=sum(payment.page))

convert_to_payment.conf_matrix<-funneltbl %>%
  group_by(sex) %>%
  summarise(convert_to_payment.conf_page=sum(payment_conf.page))

conversion_matrix_join<-left_join(convert_to_payment.conf_matrix, convert_to_payment_matrix, by="sex")
conversion_matrix_join$ratio <- conversion_matrix_join$convert_to_payment.conf_page/conversion_matrix_join$convert_to_paymentpage
#Looks like conversion rate from payment to payment confirmation page is almost same at 7.7 and 7.2%. But is slightly higher for females
# We are also losing around ~93 % of the people from payment page to payment confirmation page. Tell marketing to send emails to these non converted people.
# The people might have dropped off looking at the price. Tell marketing to send a discount voucher which is valid for limited amt of time.

# Now, Lets see the conversion to payment conformation page from device perspective

conversion_to_payment_page_by_device_matrix<-funneltbl %>%
  group_by(device) %>%
  summarise(convert_to_paymentpage=sum(payment.page))

conversion_to_payment.conf_page_by_device_matrix<- funneltbl %>%
  group_by(device) %>%
  summarise(convert_to_payment_conf.page=sum(payment_conf.page))

conversion_matrix_by_device_join<-left_join(conversion_to_payment_page_by_device_matrix, conversion_to_payment.conf_page_by_device_matrix, by="device")
conversion_matrix_by_device_join$ratio<- conversion_matrix_by_device_join$convert_to_payment_conf.page/conversion_matrix_by_device_join$convert_to_paymentpage

#Seems like gender is not the issue but device used is an issue. There's a drastic change in conversion ratio when device is changed.
# It has more to do with product interface than gender. Mobile is doing much better than Desktop site. Conversion can be increased by 100% in desktop. 
#Ask product to run A/B tests on website interface.