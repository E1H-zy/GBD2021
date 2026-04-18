
setwd("F:\\")
#install.packages("reshape")
library(reshape)
library(ggplot2)
library(ggrepel)
library(readxl)

IBD <- read.csv('IBD_region.csv',header = T)  
IBD_super <-read.csv('IBD_super_region.csv',header = T)  
IBD_Global<-subset(IBD_super,IBD_super$location_name=="Global")
IBD_22 <- rbind(IBD,IBD_Global)    
order_SDI <- read.csv('order_SDI.csv',header = F)
SDI_2021<-read.csv("SDI_2021.csv",header = T)
SDI_2021<-SDI_2021[,-1]

SDI_2021 <- melt(SDI_2021,id.vars ='Location')

SDI_2021$variable <- as.numeric(gsub('\\X',replacement = '', SDI_2021$variable))
names(SDI_2021) <- c('location','year','SDI')

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

IBD_ASMR_SDI <- merge(IBD_ASMR,SDI_2021,by=c('location','year'))

IBD_ASMR_SDI$location <- factor(IBD_ASMR_SDI$location, 
                              levels=order_SDI$V1, 
                              ordered=TRUE) 
write.csv(IBD_ASMR_SDI,"22_ASMR_SDI.csv") 

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
