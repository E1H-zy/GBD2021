#设置工作空间
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
# 读取并合并三个数据
# 疾病负担数据
data1<- vroom::vroom("F:\\table+figer资料\\Urticaria-ditu.csv")

data2 <- subset(data1,data1$measure_name=="DALYs (Disability-Adjusted Life Years)"&
                      data1$sex_name=="Both"&
                  data1$age_name=="10-24 years"&
                  (data1$year==1990|data1$year==2021))

# sdi 数据
sdi <- read.csv("SDI.csv",header = T,check.names = F)
sdi <- sdi %>% # 宽数据转为长数据
  pivot_longer(cols = `1990`:`2021`,names_to = "year") %>%
  rename(sdi=value) %>%
  dplyr::select(location,year,sdi)
sdi$year <- as.integer(sdi$year)

names(sdi)[1]<-"location_name" 
# 匹配 sdi 与疾病负担, 生成 data
data <- left_join(data2,sdi,by=c("location_name","year"))
data <- data %>%filter(age_name=="10-24 years")
#unique(data2$metric_name)
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

unique(pop1$age)
pop1 <- pop1 %>%
  filter(age_name=="10-24 years") %>%
  dplyr::select("location_name","sex_name","year","val") %>%
  rename(pop=val)
# 合并人口数据, 生成 mydata
mydata <- left_join(data,pop1,
                    by=c("location_name","sex_name","year"))

#unique(mydata$metric_name)

# 斜度指数的可视化 ----------------------------------------------------------------

## 1.绘图数据的准备 -----------------------------------------------------------------
# 计算总人口
a <- mydata %>%
  filter(metric_name=="Number") %>%
  group_by(year) %>%
  summarise(sum=sum(pop))
pop1990 <- a$sum[1]
pop2021 <- a$sum[2]
# 计算加权次序
rank <- mydata %>%
  mutate(pop_global=ifelse(year==1990,pop1990,pop2021)) %>%
  group_by(year,metric_name) %>%
 # arrange(sdi) %>%
  mutate(cummu=cumsum(pop)) %>% # 计算累积人口
  mutate(half=pop/2) %>% # 计算该国家人口的一半
  mutate(midpoint=cummu-half) %>% # 累积人口减去该国家人口一半即为人口中点
  mutate(weighted_order=midpoint/pop_global) # 人口中点与总人口相比即为改国的相对位置
# 把年份设置为 factor
rank$year <- factor(rank$year)
# 选择数据
temp1 <- rank %>%
  filter(metric_name=="Rate") %>%
  filter(year==1990)
temp2 <- rank %>%
  filter(metric_name=="Rate") %>%
  filter(year==2021)
# 建模计算斜度指数
fit1 <- lm(data = temp1,val~weighted_order)
fit2 <- lm(data = temp2,val~weighted_order)
coef(fit1)
coef(fit2)

# 查看是否存在异方差（存在异方差）
ncvTest(fit1)
ncvTest(fit2)
# 使用稳健（robust）回归：重复迭代加权
r.huber1 <- rlm(data = temp1,val~weighted_order)
r.huber2 <- rlm(data = temp2,val~weighted_order)
# 获得系数与截距
coef(r.huber1)
coef(r.huber2)
# 计算稳健回归的 95% 可信区间
confint.default(r.huber1)  ####这里weighted_order的系数即为斜率指数
confint.default(r.huber2)  ####这里weighted_order的系数即为斜率指数


library(ggpubr)
# 2.绘图 ----------------------------------------------------------------------
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
  #增加水平虚线
  geom_segment(x=0.02,xend=0.99,
               y=8.254918,yend=8.254918, # coef(r.huber1)截距的位置
               color="#6699FF",linetype=2,size=0.4,alpha=0.4)+
  geom_segment(x=0.02,xend=0.99,
               y=17.196783,yend=17.196783, # coef(r.huber2)截距的位置
               color="#990000",linetype=2,size=0.4,alpha=0.4)+
  # 增加某些国家的标签: 比如中国与印度
  geom_text(aes(label=ifelse(location_name=="China"|location_name=="India",as.character(location_name),""),
                color=year),
            hjust=0,vjust=1.7,# 避免点和文字重合
            size=3)+
  # # 增加斜度指数标签
  annotate("text",label="Slope Index of Inequality",x=1.22,y=20.228261,size=4,angle=90)+
  annotate("text",label="20.23",x=1.05,y=20.228261-8.254918/2,size=3.5)+ # coef(r.huber1) 权重的系数即斜率指数
  annotate("text",label="1.23",x=1.1,y=17.196783-1.234958/2,size=3.5)+  ##coef(r.huber2)  权重的系数即斜率指数
  scale_x_continuous(limits = c(0,1.22),labels = c("0","0.25","0.50","0.75","1.00",""))+
  xlab("Relative rank by SDI")+
  ylab("Crude DALY rate (per 100,000)")+
  theme_bw()

p1

# 集中指数的可视化 ----------------------------------------------------------------

# 1.绘图数据准备 ------------------------------------------------------------------
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
  mutate(cummu_daly=cumsum(val)) %>% # 计算累积 daly
  mutate(frac_daly=cummu_daly/total_daly) %>% # 计算累积 daly 所占总体的比例
  mutate(frac_population=cummu/pop_global) # 计算累积人口所占总体人口的比例
#####计算ci
# 选择数据
temp3 <- ci %>%
  filter(metric_name=="Number") %>%
  filter(year==1990)
temp4 <- ci %>%
  filter(metric_name=="Number") %>%
  filter(year==2021)
##计算集中指数
CI_1990 <- 2 * (sum(temp3$frac_daly) / nrow(temp3)) - 1

CI_2021 <- 2 * (sum(temp4$frac_daly) / nrow(temp4)) - 1

# 2.绘图 --------------------------------------------------------------------
p2 <- ci %>%
  ggplot(aes(x=frac_population,y=frac_daly,fill=year,color=year,group=year))+
  # 增加 X=0,y=0 两条线段
  geom_segment(x=0,xend=1,
               y=0,yend=0,
               linetype=1,size=1,color="gray")+
  geom_segment(x=1,xend=1,
               y=0,yend=1,
               linetype=1,size=1,color="gray")+
  # 对角线
  geom_segment(x=0,xend=1,
               y=0,yend=1,
               color="#CD853F",linetype=1,size=0.7,alpha=1)+
  # 散点
  geom_point(aes(fill=year,size=pop/1e6),alpha=0.75,shape=21)+
  scale_fill_manual(values = color)+
  scale_size_area("Population\n(million)",breaks=c(200,400,600,800,1000,1200))+
  # 立方样条函数拟合洛伦兹曲线 (设置节点，边界条件)
  geom_smooth(method = "gam", # 这里也可以直接用 geom_line 把点连起来
            formula = y ~ ns(x,
                             knots = c(0.0000000001,0.25,0.5,0.75,0.9999999),# 设置节点为
                             Boundary.knots = c(0,1)),
            linetype=1,size=0.1,alpha=0.6,se=T)+
  scale_color_manual(values = color)+
  # 增加两个年份的集中指数
  annotate("text",label="Concentration Index",x=0.75,y=0.35,size=5)+
  annotate("text",label="1990: 0.21",x=0.75,y=0.3,size=4,color="#6699FF")+
  annotate("text",label="2021: 0.04",x=0.75,y=0.25,size=4,color="#990000")+
  # 增加某些国家的标签，1990 年
  geom_text(aes(label=ifelse(location_name=="China"&year=="1990"|location_name=="India"&year=="1990",
                             as.character(location_name),"")),
            hjust=-0.6,vjust=0.8,
            size=3)+
  # 增加某些国家的标签，2021 年
  geom_text(aes(label=ifelse(location_name=="China"&year=="2021"|location_name=="India"&year=="2021",
                             as.character(location_name),"")),
            hjust=1.8,vjust=-0.0,
            size=3)+
  # 增加某些国家标签，人口大国
  geom_text(aes(label=ifelse(location_name%in%a&year=="1990",
                             as.character(location_name),"")),
            hjust=-0.6,vjust=0.8,
            size=3)+
  geom_text(aes(label=ifelse(location_name%in%a&year=="2021",
                             as.character(location_name),"")),
            hjust=1.8,vjust=-0.0,
            size=3)+
  # xy 标签
  xlab("Cumulative fraction of population ranked by SDI")+
  ylab("Cumulative fraction of DALY")+
  theme_bw()
p2



######斜率指数的年份图：
####利用上述方法计算出各个年份的SII数据

# 示例数据
sii_data <- data.frame(
  year = 1990:2019,
  SII = c(-120, -110, -110, -102, -100, -95, -90, -85, -80, -75, 
          -70, -65, -20, -50, -50, -40, -40, -35, -30, -25, 
          -20, -15, -10, -5, 0, 5, 10, 25, 40, 47)     #####这里的SII数值是瞎编的可以行按之前的方法计算
)
# 绘制SII趋势图
ggplot(sii_data, aes(x = year, y = SII)) +
  geom_point() +  # 绘制数据点
  geom_smooth(method = "lm", se = TRUE) +  # 添加回归线和置信区间
  labs(x = "Year", y = "SII") +  # 设置轴标签
  theme_minimal()  # 使用简洁主题
