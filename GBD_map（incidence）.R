#设置工作空间
setwd("F:\\table+figer资料")
#install.packages('ggmap')
#install.packages('maps')
#install.packages('dplyr')
library(ggmap)
library(maps)
library(dplyr)
library(RColorBrewer)

IBD<- vroom::vroom("F:\\table+figer资料\\map\\Urticaria-ditu1.csv")
head(IBD)
#2021年的全年龄段粗发病率(Crude Incidence Rate,CIR)


CIR_2021 <- subset(IBD,IBD$year==2021 & 
                     IBD$age_name=='Age-standardized' & 
                     IBD$metric_name== 'Rate' &
                     IBD$measure_name=='Incidence'&
                     IBD$sex_name=='Both') 
CIR_2021 <- CIR_2021[,c(4,14,15,16)]  ###选择位置与数据+95%UI


CIR_2021$val <- round(CIR_2021$val,2) ###率保留2位小数点
CIR_2021$lower <- round(CIR_2021$lower,2) 
CIR_2021$upper <- round(CIR_2021$upper,2) 


####  map for ASMR
worldData <- map_data('world')  #####使用map包提取世界·范围内的地图
country_asr <- CIR_2021         ####
country_asr$location <- as.character(country_asr$location_name) 

###以下代码的目的是让country_asr$location的国家名称与worldData的国家名称一致
### 这样才能让数据映射到地图上


country_asr$location[country_asr$location == "People's Republic of China"] = 'China'
#02-27以下代码用于修改
country_asr$location[country_asr$location == "Republic of Azerbaijan"] = 'Azerbaijan'
country_asr$location[country_asr$location == "Republic of Kazakhstan"] = 'Kazakhstan'
country_asr$location[country_asr$location == "Kyrgyz Republic"] = 'Kyrgyzstan'
country_asr$location[country_asr$location == "Republic of Tajikistan"] = 'Tajikistan'
country_asr$location[country_asr$location == "Republic of Uzbekistan"] = 'Uzbekistan'
country_asr$location[country_asr$location == "Republic of Singapore"] = 'Singapore'
country_asr$location[country_asr$location == "Republic of Bangladesh"] = 'Bangladesh'
country_asr$location[country_asr$location == "Kingdom of Bhutan"] = 'Bhutan'
country_asr$location[country_asr$location == "Republic of India"] = 'India'
country_asr$location[country_asr$location == "Islamic Republic of Pakistan"] = 'Pakistan'
country_asr$location[country_asr$location == "Kingdom of Cambodia"] = 'Cambodia'
country_asr$location[country_asr$location == "Republic of Maldives"] = 'Maldives'
country_asr$location[country_asr$location == "Republic of Seychelles"] = 'Seychelles'
country_asr$location[country_asr$location == "Republic of the Philippines"] = 'Philippines'
country_asr$location[country_asr$location == "Democratic Socialist Republic of Sri Lanka"] = 'Sri Lanka'
country_asr$location[country_asr$location == "Kingdom of Thailand"] = 'Thailand'
country_asr$location[country_asr$location == "Democratic Republic of Timor-Leste"] = 'Timor-Leste'
country_asr$location[country_asr$location == "Socialist Republic of Viet Nam"] = 'Vietnam'
country_asr$location[country_asr$location == "Republic of Indonesia"] = 'Indonesia'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
#02-28修改
country_asr$location[country_asr$location == "Republic of Armenia"] = 'Armenia'
country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'
country_asr$location[country_asr$location == "People's Republic of Bangladesh"] = 'Bangladesh'
country_asr$location[country_asr$location == "Republic of Mauritius"] = 'Mauritius'
country_asr$location[country_asr$location == "Republic of the Union of Myanmar"] = 'Myanmar'
country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
#03-01修改
country_asr$location[country_asr$location == "Federal Democratic Republic of Nepal"] = 'Nepal'
country_asr$location[country_asr$location == "Republic of Yemen"] = 'Yemen'
country_asr$location[country_asr$location == "Republic of Turkey"] = 'Turkey'
country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
country_asr$location[country_asr$location == "Kingdom of Saudi Arabia"] = 'Saudi Arabia'
country_asr$location[country_asr$location == "State of Qatar"] = 'Qatar'
country_asr$location[country_asr$location == "Sultanate of Oman"] = 'Oman'
country_asr$location[country_asr$location == "Hashemite Kingdom of Jordan"] = 'Jordan'
country_asr$location[country_asr$location == "Islamic Republic of Iran"] = 'Iran'
country_asr$location[country_asr$location == "Kingdom of Bahrain"] = 'Bahrain'
country_asr$location[country_asr$location == "Republic of Iraq"] = 'Iraq'
country_asr$location[country_asr$location == "Islamic Republic of Afghanistan"] = 'Afghanistan'
#03-02修改
country_asr$location[country_asr$location == "State of Kuwait"] = 'Kuwait'
country_asr$location[country_asr$location == "Lebanese Republic"] = 'Lebanon'
country_asr$location[country_asr$location == "State of Israel"] = 'Israel'
country_asr$location[country_asr$location == "Lebanese Republic"] = 'Lebanon'


total <- full_join(worldData,country_asr,by = c('region'='location'))##把两个数据根据地点合并起来
write.csv(total,"total.csv")
mycolor2 <- rev(brewer.pal(7, "Spectral"))  # 反转颜色顺序
##选择我们觉得好看的颜色，这里使用了brewer.pal

summary(total$val)

quantile(total$val,seq(0.1,1,0.1),na.rm = T)

#total <- total %>% mutate(val2 = cut(val, breaks = c(0,1156,1251,1503,1436,1539,2261),
# labels = c("0~1156", "1156~1251","1251~1503",
#"1503~1436","1436~1539","1539+"),  ## breaks需要根据自己的实际结果来调整
#include.lowest = T,right = T))

total <- total %>%
  mutate(val2 = cut(val, 
                    breaks = c(0,937.5,1337.6,1395.1,1546.2,1693.5,2563.3,76305), 
                    labels = c("0~937.5", "937.5~1337.6", 
                               "1337.6~1395.1", "1395.1~1546.2", "1546.2~1693.5", 
                               "1693.5~2563.3","2563.3+"), 
                    include.lowest = TRUE, 
                    right = TRUE))

total1<-na.omit(total)
p <- ggplot()
p1 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group = group,fill=val2),
                       colour="gray",size = .01) + 
  scale_fill_manual(values = mycolor2) +
  theme_void()+labs(x="", y="")+
  guides(fill = guide_legend(title='Age-standardized Incidence Rate\nPer 100 000 population'))+
  theme(legend.position = 'right')
p1
ggsave("p1 map（incidence）.pdf", p1, width = 10, height = 4, dpi = 300) 