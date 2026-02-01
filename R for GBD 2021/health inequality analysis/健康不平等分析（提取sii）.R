# 提取斜率指数（SII）及其95%置信区间
sii_table <- data.frame(
  Year = c(1990, 2021),
  SII = c(coef(r.huber1)[2], coef(r.huber2)[2]),
  CI_lower = c(confint.default(r.huber1)[2, 1], confint.default(r.huber2)[2, 1]),
  CI_upper = c(confint.default(r.huber1)[2, 2], confint.default(r.huber2)[2, 2])
)

# 保留两位小数
sii_table <- sii_table %>%
  mutate(across(SII:CI_upper, ~ round(.x, 2)))

# 查看表格
print(sii_table)

#####################################  05-01每年sii修改版############################
setwd("F:\\table+figer资料")
#install.packages("ggbrace")
# 加载需要的包 ------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(car)# 异方差诊断
library(MASS)# 稳健回归
library(mgcv)# 提供洛伦兹曲线拟合 (样条函数等)
library(splines)# 拟合样条函数
library(broom)
library(ggplot2)
# 数据准备--------------------------------------------------------------------

# 读取人口数据
path = "F:\\table+figer资料\\GBD_population"
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
# 读取并合并三个数据
# 疾病负担数据
data1<- vroom::vroom("F:\\table+figer资料\\Urticaria-ditu.csv")
data2 <- subset(data1,data1$measure_name=="DALYs (Disability-Adjusted Life Years)"&
                  data1$sex_name=="Both"&
                  data1$age_name=="10-24 years")
#03-16修改版
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


# sdi 数据
sdi <- read.csv("SDI2021.csv",header = T,check.names = F)
sdi <- sdi %>% # 宽数据转为长数据
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





# 合并人口数据, 生成 mydata
mydata <- left_join(data,pop1,
                    by=c("location_name","sex_name","year"))

unique(mydata$metric_name)

library(dplyr)
library(MASS)  # rlm回归

# 计算每年的斜度指数（SII）

rank <- mydata %>%
  group_by(year, metric_name) %>%
  arrange(sdi) %>%
  mutate(pop_global = sum(pop),  # 计算每年总人口
         cummu = cumsum(pop),   # 累积人口
         half = pop / 2,         # 该国家人口的一半
         midpoint = cummu - half, # 计算人口的中点
         weighted_order = midpoint / pop_global) # 计算相对位置

# 过滤"Rate"指标并按年份单独做回归
result <- rank %>%
  filter(metric_name == "Rate") %>%
  group_by(year) %>%
  do({
    # 每一年做回归
    fit <- rlm(val ~ weighted_order, data = .)
    coef_vals <- coef(fit)  # 提取回归系数
    conf_interval <- confint(fit)  # 计算95%可信区间
    
    # 提取回归系数和可信区间
    tibble(year = unique(.$year),
           intercept = coef_vals[1],
           slope = coef_vals[2],
           CI_lower = conf_interval[2, 1],
           CI_upper = conf_interval[2, 2])
  })

# 打印结果
print(result)
#write.csv(result, "SII_results.csv", row.names = FALSE)


