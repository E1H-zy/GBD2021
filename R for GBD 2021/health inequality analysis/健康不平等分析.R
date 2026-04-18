setwd("F:\\")

library(tidyverse)
library(data.table)
library(car)
library(MASS)
library(mgcv)
library(splines)
library(broom)
library(ggplot2)

data1<- vroom::vroom("F:\\")

data2 <- subset(data1,data1$measure_name=="DALYs (Disability-Adjusted Life Years)"&
                      data1$sex_name=="Both"&
                  data1$age_name=="10-24 years"&
                  (data1$year==1990|data1$year==2021))


sdi <- read.csv("SDI.csv",header = T,check.names = F)
sdi <- sdi %>% 
  pivot_longer(cols = `1990`:`2021`,names_to = "year") %>%
  rename(sdi=value) %>%
  dplyr::select(location,year,sdi)
sdi$year <- as.integer(sdi$year)

names(sdi)[1]<-"location_name" 
data <- left_join(data2,sdi,by=c("location_name","year"))
data <- data %>%filter(age_name=="10-24 years")
#unique(data2$metric_name)
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

unique(pop1$age)
pop1 <- pop1 %>%
  filter(age_name=="10-24 years") %>%
  dplyr::select("location_name","sex_name","year","val") %>%
  rename(pop=val)

mydata <- left_join(data,pop1,
                    by=c("location_name","sex_name","year"))

#unique(mydata$metric_name)


a <- mydata %>%
  filter(metric_name=="Number") %>%
  group_by(year) %>%
  summarise(sum=sum(pop))
pop1990 <- a$sum[1]
pop2021 <- a$sum[2]

rank <- mydata %>%
  mutate(pop_global=ifelse(year==1990,pop1990,pop2021)) %>%
  group_by(year,metric_name) %>%
 # arrange(sdi) %>%
  mutate(cummu=cumsum(pop)) %>%
  mutate(half=pop/2) %>% 
  mutate(midpoint=cummu-half) %>% 
  mutate(weighted_order=midpoint/pop_global) 

rank$year <- factor(rank$year)

temp1 <- rank %>%
  filter(metric_name=="Rate") %>%
  filter(year==1990)
temp2 <- rank %>%
  filter(metric_name=="Rate") %>%
  filter(year==2021)

fit1 <- lm(data = temp1,val~weighted_order)
fit2 <- lm(data = temp2,val~weighted_order)
coef(fit1)
coef(fit2)

ncvTest(fit1)
ncvTest(fit2)

r.huber1 <- rlm(data = temp1,val~weighted_order)
r.huber2 <- rlm(data = temp2,val~weighted_order)

coef(r.huber1)
coef(r.huber2)
confint.default(r.huber1) 
confint.default(r.huber2) 

library(ggpubr)

color <- c("#6699FF","#990000")


colnames(rank)
p1 <- rank %>%
  filter(metric_name=="Rate") %>%
  ggplot(aes(x=weighted_order,y=val,fill=year,group=year,color=year))+
  geom_point(aes(color=year,size=pop/1e6),alpha=0.8,shape=21)+
  scale_size_area("Population\n(million)",breaks=c(200,400,600,800,1000,1200))+
  geom_smooth(method = "rlm",size=0.6,alpha=0.1)+
  scale_fill_manual(values = color)+
  scale_color_manual(values = color)+

  geom_segment(x=0.02,xend=0.99,
               y=8.254918,yend=8.254918, 
               color="#6699FF",linetype=2,size=0.4,alpha=0.4)+
  geom_segment(x=0.02,xend=0.99,
               y=17.196783,yend=17.196783, 
               color="#990000",linetype=2,size=0.4,alpha=0.4)+
  geom_text(aes(label=ifelse(location_name=="China"|location_name=="India",as.character(location_name),""),
                color=year),
            hjust=0,vjust=1.7,
            size=3)+
  annotate("text",label="Slope Index of Inequality",x=1.22,y=20.228261,size=4,angle=90)+
  annotate("text",label="20.23",x=1.05,y=20.228261-8.254918/2,size=3.5)+ 
  annotate("text",label="1.23",x=1.1,y=17.196783-1.234958/2,size=3.5)+ 
  scale_x_continuous(limits = c(0,1.22),labels = c("0","0.25","0.50","0.75","1.00",""))+
  xlab("Relative rank by SDI")+
  ylab("Crude DALY rate (per 100,000)")+
  theme_bw()

p1


a <- mydata %>%
  filter(metric_name=="Number") %>%
  group_by(year) %>%
  summarise(sum=sum(val))
daly1990 <- a$sum[1]
daly2021 <- a$sum[2]

ci <- rank %>%
  filter(metric_name=="Number") %>%
  mutate(total_daly=ifelse(year==1990,daly1990,daly2021)) %>%
  group_by(year) %>%
  arrange(sdi) %>%
  mutate(cummu_daly=cumsum(val)) %>% 
  mutate(frac_daly=cummu_daly/total_daly) %>% 
  mutate(frac_population=cummu/pop_global) 
temp3 <- ci %>%
  filter(metric_name=="Number") %>%
  filter(year==1990)
temp4 <- ci %>%
  filter(metric_name=="Number") %>%
  filter(year==2021)
##计算集中指数
CI_1990 <- 2 * (sum(temp3$frac_daly) / nrow(temp3)) - 1

CI_2021 <- 2 * (sum(temp4$frac_daly) / nrow(temp4)) - 1


p2 <- ci %>%
  ggplot(aes(x=frac_population,y=frac_daly,fill=year,color=year,group=year))+
  geom_segment(x=0,xend=1,
               y=0,yend=0,
               linetype=1,size=1,color="gray")+
  geom_segment(x=1,xend=1,
               y=0,yend=1,
               linetype=1,size=1,color="gray")+
  geom_segment(x=0,xend=1,
               y=0,yend=1,
               color="#CD853F",linetype=1,size=0.7,alpha=1)+
  geom_point(aes(fill=year,size=pop/1e6),alpha=0.75,shape=21)+
  scale_fill_manual(values = color)+
  scale_size_area("Population\n(million)",breaks=c(200,400,600,800,1000,1200))+
  geom_smooth(method = "gam", 
            formula = y ~ ns(x,
                             knots = c(0.0000000001,0.25,0.5,0.75,0.9999999),
                             Boundary.knots = c(0,1)),
            linetype=1,size=0.1,alpha=0.6,se=T)+
  scale_color_manual(values = color)+
  annotate("text",label="Concentration Index",x=0.75,y=0.35,size=5)+
  annotate("text",label="1990: 0.21",x=0.75,y=0.3,size=4,color="#6699FF")+
  annotate("text",label="2021: 0.04",x=0.75,y=0.25,size=4,color="#990000")+
  geom_text(aes(label=ifelse(location_name=="China"&year=="1990"|location_name=="India"&year=="1990",
                             as.character(location_name),"")),
            hjust=-0.6,vjust=0.8,
            size=3)+

  geom_text(aes(label=ifelse(location_name=="China"&year=="2021"|location_name=="India"&year=="2021",
                             as.character(location_name),"")),
            hjust=1.8,vjust=-0.0,
            size=3)+

  geom_text(aes(label=ifelse(location_name%in%a&year=="1990",
                             as.character(location_name),"")),
            hjust=-0.6,vjust=0.8,
            size=3)+
  geom_text(aes(label=ifelse(location_name%in%a&year=="2021",
                             as.character(location_name),"")),
            hjust=1.8,vjust=-0.0,
            size=3)+

  xlab("Cumulative fraction of population ranked by SDI")+
  ylab("Cumulative fraction of DALY")+
  theme_bw()
p2




sii_data <- data.frame(
  year = 1990:2019,
  SII = c(-120, -110, -110, -102, -100, -95, -90, -85, -80, -75, 
          -70, -65, -20, -50, -50, -40, -40, -35, -30, -25, 
          -20, -15, -10, -5, 0, 5, 10, 25, 40, 47)     
)

ggplot(sii_data, aes(x = year, y = SII)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) +  
  labs(x = "Year", y = "SII") +  
  theme_minimal()  
