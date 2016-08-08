library(lubridate)
library(caret)
library(dplyr)
library(ggplot2)
library(randomForest)
library(rpart)

conversion_data <- read.csv("/DS/SiteConversion/CWD/conversion_data.csv")

class(conversion_data)
dim(conversion_data)  # consists of 316k rows with 6 cols
head(conversion_data, 10)
tail(conversion_data, 10)
str(conversion_data)
sapply(conversion_data, class)  # we'll know if the data is not in proper format we want number to be in int if not we'll convert after this step.
summary(conversion_data)  
# data looks suspicious as max(age) is 123!!!
#Lets see how many people have 123 age

sort(unique(conversion_data$age), decreasing =  T)
#Wow we have atleast 2 ppl of > 100 age. Looks fishy to me to furthur investigate

subset(conversion_data, age >79)
# only two records greater than 79 yrs of age. Somebody screwed up, IT aint me!!. 
# Developer dude, there may be a bug in your code. 
# Whoa don't be angry at me developer dude. Data neva' lies. (Rule #1 of a data scientist)
#back to the problem at hand, as there are only two rows. Lets eliminate them. Its okay to remove two rows in this massive amt of data. 
conversion_data <- subset(conversion_data, age<80)

summary(conversion_data$country)


#lets plot conversion rate of each country
conversion_country = conversion_data %>%
  group_by(country) %>%
  summarise(conversion_rate = mean(converted))
#Shows that Germany's conversion Rate is high ~6% and China's conversion rate is ~0.1% 
#Lets plot the values with mean(converted) group by country 

ggplot(data = conversion_country, aes(x= country, y= conversion_rate))+
  geom_bar(stat="identity", aes(fill=country))

data_pages<- conversion_data %>% 
  group_by(total_pages_visited) %>%
  summarise(conversion_rate=mean(converted))

View(data_pages)
qplot(total_pages_visited, conversion_rate, data = data_pages, geom="line")

#lets see if source of the person maters to conversion
source_converted<- conversion_data %>% 
  group_by(source) %>%
  summarise(conversion_rate = mean(converted))
View(source_converted)
ggplot(data = source_converted, mapping = aes(x=source, y=conversion_rate))+
  geom_bar(stat="identity", aes(fill=source))
#Surprisingly it doesn't, Lets check if we get the same thing while developing the model 

newOrOld<- conversion_data %>% 
  group_by(new_user) %>%
  summarise(conversion_rate= mean(converted))
newOrOld$new_user<-plyr::mapvalues(newOrOld$new_user,from= c(1,0), to = c("New_User","Old_User"))

ggplot(data = newOrOld, mapping = aes(x=new_user, y=conversion_rate))+
  geom_bar(stat = "identity", aes(fill=new_user))
#looks like old users are getting converted. May be they are getting acquinted and then getting 
#converted. 
filtered_data<- filter(conversion_data, new_user==0)
oldusers_pagevisits<- filtered_data %>% 
  group_by(total_pages_visited) %>%
  summarise(conversion_rate=mean(converted))
qplot(x= total_pages_visited, y= conversion_rate, data = oldusers_pagevisits, geom="line")
#Yeah! our hypothesis is correct, Old users are getting acquinted and are then getting converted. 
#But hold on, Lets check for new users too, Afterall it is just copy paste of previous code 
  
filtered_data<- filter(conversion_data, new_user==1)
newusers_pagevisits<- filtered_data %>% 
  group_by(total_pages_visited) %>%
  summarise(conversion_rate=mean(converted))
qplot(x= total_pages_visited, y= conversion_rate, data = newusers_pagevisits, geom="line")
#Whoops its the same graph. This is not an insight. 
#Anyways we've learnt that old and new users behave in same way. 

age_conversion<- conversion_data %>%
                  group_by(age) %>%
                  summarise(conversion_rate= mean(converted))
qplot(x=age, y=conversion_rate, data=age_conversion, geom="line" )
#conversion rate is maintained >3% upto 30 years of age. It drops steadily until about 55 yrs age.

#Now that we've analysed all the input variables wrt the predictir variable.Here are our conclusions
# 1. Ths site is a US based site but has strong prescence in China
# 2. The userbase is quite young
# 3. conversion rate falls with age.
# 4. Even though China's userbase is high the country's conversion rate is very low
# 5. Germany's conversion rate is awesome ~6%
# 6. Source of the person's visit to the website does not matter.
# 7. Old users and new users behave in the same way.

#data cleaning for building the model
class(conversion_data$converted)
conversion_data$converted<- as.factor(conversion_data$converted)
class(conversion_data$new_user)
conversion_data$new_user<- as.factor(conversion_data$new_user)

#Once done lets split the data into train and test sets
 trainindex<- createDataPartition(conversion_data$converted, p=0.75, list = FALSE, times = 1)
 conversion_data.train<- conversion_data[trainindex, ]
 conversion_data.test <- conversion_data[-trainindex, ]
 
 set.seed(400)
 
 #Lets build an RF model now.
 
 rfmodel<- randomForest(y= conversion_data.train$converted, x= conversion_data.train[, -ncol(conversion_data.test)], 
                        ytest = conversion_data.test$converted, xtest = conversion_data.test[, -ncol(conversion_data.test)], ntree=100, mtry=3, keep.forest = T)
 
 rfmodel
 
# The OOB error is pretty much similar to Training and Test sets. So we are good with overfitting(not over fitting). Accuracy is kinda okay 98.5%
# As the event rate is very low. So even if classify everything wrong(converted people as non converted) we will be at ~96.7% accuracy. So 98.5% is not great by any means but a starting point  
# Anyway lets check some more details about the model. 
 
 varImpPlot(rfmodel)
 #Gini says: "source is not at all important; Total pages visited very important."
# okay lets remove total pages visited and use RF again
 
 rfmodel2<- randomForest(y=conversion_data.train$converted, x= conversion_data.train[ , -c(5,ncol(conversion_data.train))],
                         ytest = conversion_data.test$converted, xtest = conversion_data.test[, -c(5,ncol(conversion_data.test))], 
                         mtry=3, ntree = 100, keep.forest = TRUE, classwt = c(0.7, 0.3))

rfmodel2
 # When an important predictor variable is removed the error rate has gone up.!
varImpPlot(rfmodel2)
 #Gini says "New user is the next most important variable! and Source is not at all an important variable"
 #Lets draw partial plots 
op<-par(mfrow=c(2,2))
partialPlot(rfmodel2, conversion_data.train, age, 1)
partialPlot(rfmodel2, conversion_data.train, country, 1)
partialPlot(rfmodel2, conversion_data.train, source, 1)
partialPlot(rfmodel2, conversion_data.train, new_user, 1)


#Insights from the model 
# 1. The site is working well for young people. Tell marketing to market in places where young people hang out
# 2. The Chinese Version of the site is not doing well. Ask product to fix this. Translation might not be good or some other issue or it is 
# english for people who only understand Chinese language
# 3. Germans are converting well but Germans are not coming to the site. Tell marketing to target germans.
# 4. If someone has visited and browsed through number of pages, She/he defnitely has purchase intent. Convert these kind of customers by sending 
# followup emails with product. Low hanging fruits who can easily convert.
# 5. Conversion rate is dropping after 30 yrs of age. The UI might not be good for older ppl. 
# 6. Users with Old accounts are easy targets, easily convertable. Ask marketing to run a campaign around those type of users. 


