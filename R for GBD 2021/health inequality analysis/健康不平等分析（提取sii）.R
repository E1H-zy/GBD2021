
sii_table <- data.frame(
  Year = c(1990, 2021),
  SII = c(coef(r.huber1)[2], coef(r.huber2)[2]),
  CI_lower = c(confint.default(r.huber1)[2, 1], confint.default(r.huber2)[2, 1]),
  CI_upper = c(confint.default(r.huber1)[2, 2], confint.default(r.huber2)[2, 2])
)

sii_table <- sii_table %>%
  mutate(across(SII:CI_upper, ~ round(.x, 2)))

print(sii_table)


setwd("F:\\")
#install.packages("ggbrace")

library(tidyverse)
library(data.table)
library(car)
library(MASS)
library(mgcv)
library(splines)
library(broom)
library(ggplot2)

path = "F:\\"
fileName = dir(path)
fileName
population <- data.frame()

#population<-data.frame()
for(k in 1:length(fileName)){
  data = read.csv(file = paste(path,fileName[k],sep = "\\"),
                  header = T,stringsAsFactors = F)
  population=rbind(population,data)
}


pop1 <- population %>%
  dplyr::select(location_name,sex_name,age_name,year,metric_name,val)

data1<- vroom::vroom("F:\\")
data2 <- subset(data1,data1$measure_name=="DALYs (Disability-Adjusted Life Years)"&
                  data1$sex_name=="Both"&
                  data1$age_name=="10-24 years")

data2$location_name[data2$location_name == "Republic of the Union of Myanmar"] = 'Myanmar'
data2$location_name[data2$location_name == "Republic of Tajikistan"] = 'Tajikistan'
data2$location_name[data2$location_name == "Republic of the Philippines"] = 'Philippines'
data2$location_name[data2$location_name == "People's Republic of Bangladesh"] = 'Bangladesh'
data2$location_name[data2$location_name == "People's Republic of China"] = 'China'
data2$location_name[data2$location_name == "Democratic Socialist Republic of Sri Lanka"] = 'Sri Lanka'
data2$location_name[data2$location_name == "Republic of Uzbekistan"] = 'Uzbekistan'
data2$location_name[data2$location_name == "Kingdom of Thailand"] = 'Thailand'
data2$location_name[data2$location_name == "Republic of Mauritius"] = 'Mauritius'
data2$location_name[data2$location_name == "Kingdom of Bhutan"] = 'Bhutan'
data2$location_name[data2$location_name == "Republic of Seychelles"] = 'Seychelles'
data2$location_name[data2$location_name == "Democratic Republic of Timor-Leste"] = 'Timor-Leste'
data2$location_name[data2$location_name == "Republic of Azerbaijan"] = 'Azerbaijan'
data2$location_name[data2$location_name == "Republic of Singapore"] = 'Singapore'
data2$location_name[data2$location_name == "Federal Democratic Republic of Nepal"] = 'Nepal'
data2$location_name[data2$location_name == "Republic of India"] = 'India'
data2$location_name[data2$location_name == "Republic of Indonesia"] = 'Indonesia'
data2$location_name[data2$location_name == "Republic of Armenia"] = 'Armenia'
data2$location_name[data2$location_name == "Islamic Republic of Pakistan"] = 'Pakistan'
data2$location_name[data2$location_name == "Socialist Republic of Viet Nam"] = 'Viet Nam'
data2$location_name[data2$location_name == "Republic of Kazakhstan"] = 'Kazakhstan'
data2$location_name[data2$location_name == "Kingdom of Cambodia"] = 'Cambodia'
data2$location_name[data2$location_name == "Kyrgyz Republic"] = 'Kyrgyzstan'

data2$location_name[data2$location_name == "Republic of Maldives"] = 'Maldives'
data2$location_name[data2$location_name == "People's Republic of China"] = 'China'
data2$location_name[data2$location_name == "Republic of the Philippines"] = 'Philippines'
data2$location_name[data2$location_name == "Brunei Darussalam"] = 'Brunei Darussalam'



sdi <- read.csv("",header = T,check.names = F)
sdi <- sdi %>%
  pivot_longer(cols = `1990`:`2021`,names_to = "year") %>%
  rename(sdi=value) %>%
  dplyr::select(location,year,sdi)
sdi$year <- as.integer(sdi$year)

names(sdi)[1]<-"location_name" 
data <- left_join(data2,sdi,by=c("location_name","year"))
data <- data %>% filter(!is.na(sdi))
data <- data %>%filter(age_name=="10-24 years")
unique(data$metric_name)
#unique(pop1$age)
pop1 <- pop1 %>%
  filter(age_name=="10-24 years") %>%
  dplyr::select("location_name","sex_name","year","val") %>%
  rename(pop=val)

mydata <- left_join(data,pop1,
                    by=c("location_name","sex_name","year"))

unique(mydata$metric_name)

library(dplyr)
library(MASS)  
rank <- mydata %>%
  group_by(year, metric_name) %>%
  arrange(sdi) %>%
  mutate(pop_global = sum(pop),  
         cummu = cumsum(pop),  
         half = pop / 2,       
         midpoint = cummu - half,
         weighted_order = midpoint / pop_global)

result <- rank %>%
  filter(metric_name == "Rate") %>%
  group_by(year) %>%
  do({
    fit <- rlm(val ~ weighted_order, data = .)
    coef_vals <- coef(fit) 
    conf_interval <- confint(fit)
    
    tibble(year = unique(.$year),
           intercept = coef_vals[1],
           slope = coef_vals[2],
           CI_lower = conf_interval[2, 1],
           CI_upper = conf_interval[2, 2])
  })

print(result)
#write.csv(result, "SII_results.csv", row.names = FALSE)


