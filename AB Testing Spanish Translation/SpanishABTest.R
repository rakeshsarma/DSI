library(dplyr)
setwd("/DS/Spanish Translation")

#import data sets
user_table <- read.csv("/DS/Spanish Translation/Translation_Test/user_table.csv", stringsAsFactors=TRUE)
test_table <- read.csv("/DS/Spanish Translation/Translation_Test/test_table.csv", stringsAsFactors=TRUE)

#Lets see if there are any duplicates
length(unique(user_table$user_id)) == length(user_table$user_id)
length(unique(test_table$user_id)) == length(test_table$user_id)
#There are no duplicates
#lets join both the the tables
data<- merge(user_table, test_table, by = "user_id", all=TRUE)
View(summary(data))
data$date <- as.Date(data$date)
View(summary(data))
data$source<- as.factor(data$source)

#lets check that true spain converts much better than rest before the test
data_conversion_country<- data %>%
  group_by(country) %>%
  summarise(country_conversion=mean(conversion[test==0])) %>%
  arrange(desc(country_conversion))
View(data_conversion_country)
# Yes, Spain converts better than rest od the countries
data_test = subset(data, country != "Spain")
t.test(data_test$conversion[data_test$test == 1], data_test$conversion[data_test$test == 0])