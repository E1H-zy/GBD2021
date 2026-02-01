
setwd("F:\\我的工作文件夹\\13 SDI 22地区")
#install.packages("reshape")
library(reshape)
library(ggplot2)
library(ggrepel)
library(readxl)

IBD <- read.csv('IBD_region.csv',header = T)  # 读取21个地区的数据
IBD_super <-read.csv('IBD_super_region.csv',header = T)  ##读取超级地区及全球数据
IBD_Global<-subset(IBD_super,IBD_super$location_name=="Global")###提取全球数据
IBD_22 <- rbind(IBD,IBD_Global)       ###把全球数据与21个地区的数据合并
order_SDI <- read.csv('order_SDI.csv',header = F)
SDI_2021<-read.csv("SDI_2021.csv",header = T)
SDI_2021<-SDI_2021[,-1]
## 用到reshape包，将SDI数据格式从宽数据格式转换为长数据格式
SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))
names(SDI_2021) <- c('location','year','SDI')

#改变量名，SDI与GBD保持一致
#SDI_2021$location[which(SDI_2021$location =='Central sub-Saharan Africa')] <-'Central Sub-Saharan Africa'
#SDI_2021$location[which(SDI_2021$location =='Eastern sub-Saharan Africa')] <-'Eastern Sub-Saharan Africa'
#SDI_2021$location[which(SDI_2021$location =='Southern sub-Saharan Africa')] <-'Southern Sub-Saharan Africa'
#SDI_2021$location[which(SDI_2021$location =='Western sub-Saharan Africa')] <-'Western Sub-Saharan Africa'

### ASMR#####
IBD_ASMR <- subset(IBD_22, IBD_22$age_name=='Age-standardized' & 
                   IBD_22$metric_name== 'Rate' &
                   IBD_22$measure_name=='Incidence'&
                   IBD_22$sex_name=="Both")
IBD_ASMR <- IBD_ASMR[,c(4,13,14)] ###选择location year val
names(IBD_ASMR)[3] <- 'ASMR'
names(IBD_ASMR)[1] <- 'location'
### 合并SDI与ASR数据
IBD_ASMR_SDI <- merge(IBD_ASMR,SDI_2021,by=c('location','year'))

IBD_ASMR_SDI$location <- factor(IBD_ASMR_SDI$location, 
                              levels=order_SDI$V1, 
                              ordered=TRUE) ## location图例按照我们的顺序排列
write.csv(IBD_ASMR_SDI,"22_ASMR_SDI.csv") 
##开始作图，主变量为ASR以及SDI,图形的颜色和形状根据不同区域来调整即可
#同时以所有数据画出拟合曲线

size_breaks <- seq(min(IBD_ASMR_SDI$year), max(IBD_ASMR_SDI$year), by = 5)
size_labels <- size_breaks


ggplot(IBD_ASMR_SDI, aes(SDI,ASMR)) + geom_point(aes(color = location, shape= location,size=year))+
  scale_shape_manual(values = 1:22) + 
  labs(x = "Socio-Demographic Index(SDI)",  
       y = "Incidence Rate per 100,000 population") +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5)+
  scale_size_continuous(breaks = size_breaks, labels = size_labels,range = c(1, 3))



result <- cor.test(IBD_ASMR_SDI$ASMR, IBD_ASMR_SDI$SDI, method = "spearman", exact = FALSE)
summary(result)
