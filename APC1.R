
setwd("D:/课程材料/代码及实操/实操/06_1 年龄周期队列模型")
#install.packages("prepare_rates")


library(magrittr)
library(dplyr)
library(data.table)
source('source_apc.R')
source('function_year5.R')

#读取数据，查看数据结构
IBD_china <- fread('China_IBD.csv')
str(IBD_china)

#提取年龄分组
age1 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84","85-89","90-94","95+ years")   ###20个年龄组
age2 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")   ###20个年龄



####发生率的年龄周期队列####
#1.发生率的发生人数，数据提取
IBD_in_both<- subset(IBD_china,
                (IBD_china$age_name %in% age1 ) &
                 IBD_china$sex_name=="Both"&
                 IBD_china$location_name=='China'&
                 IBD_china$metric_name== 'Number' &
                 IBD_china$measure_name=='Incidence')
#查看年龄分组
unique(IBD_in_both$age_name)
#删减  years字样
IBD_in_both$age_name<-gsub(" years","",IBD_in_both$age_name)
unique(IBD_in_both$age_name)

#因子化
IBD_in_both$age_name <- factor(IBD_in_both$age_name, levels = c("<5", "5-9", "10-14", "15-19",
                          "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                          "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                          "90-94", "95+"))

#选择age_id age_name,year, val 四列
IBD_in_both <- IBD_in_both[,c("age_id","age_name","year","val")]

#长转宽，年份作为列
IBD_in_both_n <- dcast(data = IBD_in_both, age_id + age_name ~ year)

#转成5年一组, 从21年开始往前每5年一个组
IBD_in_both_g <- function_year5(IBD_in_both_n, 1990, 2021, 2021)

#列名
rownames(IBD_in_both_g) <- IBD_in_both_n$age_name


#####导入人口数据####

path = "D:\\课程材料\\代码及实操\\实操\\GBD_population"
fileName = dir(path)
fileName

var_name <- c("location_name","sex_name","year","age_id","age_name","val") 
population <- data.frame()

#population<-data.frame()
for(k in 1:length(fileName)){
  data = read.csv(file = paste(path,fileName[k],sep = "\\"),
                  header = T,stringsAsFactors = F)
  population=rbind(population,data)
}
population<-population%>% dplyr::select(var_name) %>% 
  filter(location_name %in% 'China' & age_name %in% age2 & sex_name %in% 'Both')

#去掉age_name的字样
population$age_name<-gsub(" years","",population$age_name)


#选择age_id age_name,year, val 四列
population <- population[,c("age_id","age_name","year","val")]

#人口数据的长转宽
population_n <- dcast(data = population, age_id + age_name ~ year) 

#转成五年一组
population_g <- function_year5(population_n, 1990, 2021, 2021)
rownames(population_g) <- population_n$age_name


######查看两个数据的年龄段是否有区别####
#取两个数据集的交集
name <- intersect(population_n$age_name,IBD_in_both_n$age_name)

#提取不同年龄段数据
population_g <- population_g[rownames(population_g) %in% name,]
IBD_in_both_g <- IBD_in_both_g[rownames(IBD_in_both_g) %in% name,]

####按数据所需格式排列结果数据
name2 <- paste0(names(population_g),"p")
population_g <- population_g %>% stats::setNames(name2)

IBD_in_both_population <- tibble(cbind(IBD_in_both_g,population_g)) %>% 
  dplyr::select(`1990-1991`,`1990-1991p`,`1992-1996`,`1992-1996p`,`1997-2001`,`1997-2001p`,
                `2002-2006`,`2002-2006p`,`2007-2011`,`2007-2011p`,`2012-2016`,`2012-2016p`,
                `2017-2021`,`2017-2021p`)

write.table(IBD_in_both_population,'IBD_in_both_population.csv',row.names = F,col.names = F,sep = ',')


###R中画图#####
## APC模型进一步处理
R <- prepare_rates(IBD_in_both_population,
                   StartYear=1990,StartAge=15,Interval=5,
                   fullname='',description='') 
## APC模型计算
M <- apc2fit(R)
## 画图
plot.apc1(M)


