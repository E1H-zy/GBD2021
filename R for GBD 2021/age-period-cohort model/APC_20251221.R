rm(list=ls())
getwd()
setwd("D:/zy/GBD作废版本初稿等/GBD作废版本初稿等/APC/")

# 加载必要的包
library(ggplot2)
library(dplyr)

# 读取数据
APC_both <- readRDS("APC_analysis_20250223_035746.969228_output.rds")
APC_male <- readRDS("APC_analysis_20250223_041747.996011_output.rds")
APC_female <- readRDS("APC_analysis_20250223_042330.582674_output.rds")

# 提取 Long Age Curve 数据并合并
long_age_both <- APC_both[["LongAge"]] %>% as.data.frame() %>% mutate(group = "both")
long_age_male <- APC_male[["LongAge"]] %>% as.data.frame() %>% mutate(group = "male")
long_age_female <- APC_female[["LongAge"]] %>% as.data.frame() %>% mutate(group = "female")
long_age <- bind_rows(long_age_both, long_age_male, long_age_female)

# 绘制 Long Age Curve
ggplot(data = long_age, aes(x = Age, y = Rate, color = group)) +
  geom_line() +  # 绘制折线
  geom_ribbon(aes(ymin = CILo, ymax = CIHi), fill = "grey", alpha = 0.3) +  # 绘制置信区间
  scale_x_continuous(
    limits = c(10, 25),      # 范围扩大到 10-25，包含整个 10-24 区间
    breaks = seq(10, 25, 5), # 刻度设为 10, 15, 20, 25，对应年龄组边界
    labels = c("10", "15", "20", "25")
  ) +  # 设置 x 轴范围和间隔
  labs(title = "Long Age Curve with Confidence Intervals",
       x = "Age",
       y = "Rate",
       color = "Group") +
  theme_bw()


p1 <- ggplot(data = long_age, aes(x = Age, y = Rate, color = group)) +
  geom_line() +  # 绘制折线
  geom_smooth(aes(ymin = CILo, ymax = CIHi), method = "loess", se = TRUE, fill = "grey", alpha = 0.3) +  # 绘制平滑曲线和置信区间
  scale_x_continuous(
    limits = c(10, 25),      # 范围扩大到 10-25，包含整个 10-24 区间
    breaks = seq(10, 25, 5), # 刻度设为 10, 15, 20, 25，对应年龄组边界
    labels = c("10", "15", "20", "25")
  ) +  # 设置 x 轴范围和间隔+
  labs(title = "Long Age Curve with Confidence Intervals",
       x = "Age",
       y = "Rate",
       color = "Group") +
  theme_bw()

ggsave("long age cure1221.pdf",p1,width=8,height=6)

# 提取 CohortRR 数据并合并
cohort_both <- APC_both[["CohortRR"]] %>% as.data.frame() %>% mutate(group = "both")
cohort_male <- APC_male[["CohortRR"]] %>% as.data.frame() %>% mutate(group = "male")
cohort_female <- APC_female[["CohortRR"]] %>% as.data.frame() %>% mutate(group = "female")

CohortRR <- bind_rows(cohort_both, cohort_male, cohort_female)

# 绘制 CohortRR
p2 <- ggplot(data = CohortRR, aes(x = Cohort, y = `Rate Ratio`, color = group)) +
  geom_line() +  # 绘制折线
  geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = group), alpha = 0.1,color = NA) +  # 绘制置信区间
  theme_bw() +
  geom_hline(yintercept = 1, linetype = 2, color = 'black') +  # 添加水平参考线
  geom_vline(xintercept = CohortRR[which(CohortRR$`Rate Ratio` == 1), 1],
             linetype = 2, color = 'black') +  # 添加垂直参考线
  labs(title = "Cohort Rate Ratio(1990-2019)",
       x = "Cohort",
       y = "Rate Ratio",
       color = "Group",
       fill = "Group")
p2
ggsave("cohortRR1221.pdf",p2,width=8,height=6)


# 提取 PeriodRR 数据并合并
period_both <- APC_both[["PeriodRR"]] %>% as.data.frame() %>% mutate(group = "both")
period_male <- APC_male[["PeriodRR"]] %>% as.data.frame() %>% mutate(group = "male")
period_female <- APC_female[["PeriodRR"]] %>% as.data.frame() %>% mutate(group = "female")

PeriodRR <- bind_rows(period_both, period_male, period_female)

# 绘制 PeriodRR
p3 <- ggplot(data = PeriodRR, aes(x = Period, y = `Rate Ratio`, color = group)) +
  geom_line() +  # 绘制折线
  geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = group), alpha = 0.1) +  # 绘制置信区间
  theme_bw() +
  geom_hline(yintercept = 1, linetype = 2, color = 'black') +  # 添加水平参考线
  geom_vline(xintercept = PeriodRR[which(PeriodRR$`Rate Ratio` == 1), 1],
             linetype = 2, color = 'black') +  # 添加垂直参考线
  labs(title = "Period Rate Ratio (1990-2019)",
       x = "Period",
       y = "Rate Ratio",
       color = "Group",
       fill = "Group")
p3

p3<- ggplot(data = PeriodRR, aes(x = Period, y = `Rate Ratio`, color = group)) +
  # 1. 绘制置信区间 (关键修改：color = NA)
  geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = group), 
              alpha = 0.1, color = NA) + 
  
  geom_line(size = 1) +
  theme_bw() +
  geom_hline(yintercept = 1, linetype = 2, color = 'black') +
  # 注意：PeriodRR 参照点通常是固定的，如果报错可以用固定年份代替
  geom_vline(xintercept = PeriodRR[which(PeriodRR$`Rate Ratio` == 1), 1][1],
             linetype = 2, color = 'black') +
  labs(title = "Period Rate Ratio (1990-2019)", # 标题已修正
       caption = "Note: Data from 2020-2021 excluded for APC model validity.",
       x = "Period", y = "Rate Ratio", color = "Group", fill = "Group")
ggsave("periodRR1221.pdf",p3,width=8,height=6)

# 提取 Local Drift 数据并合并
local_both <- APC_both[["LocalDrifts"]] %>% as.data.frame() %>% mutate(group = "both")
local_male <- APC_male[["LocalDrifts"]] %>% as.data.frame() %>% mutate(group = "male")
local_female <- APC_female[["LocalDrifts"]] %>% as.data.frame() %>% mutate(group = "female")

Local <- bind_rows(local_both, local_male, local_female)

# 提取 Net Drift 数据
Net <- APC_both[["NetDrift"]] %>% as.data.frame()

# 绘制 Local Drift 和 Net Drift
p4 <- ggplot(data = Local, aes(x = Age, y = `Percent per Year`, color = group)) +
  geom_line() +  # 绘制折线
  geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = group), alpha = 0.1) +  # 绘制置信区间
  scale_x_continuous(
    limits = c(10, 25),      # 范围扩大到 10-25，包含整个 10-24 区间
    breaks = seq(10, 25, 5), # 刻度设为 10, 15, 20, 25，对应年龄组边界
    labels = c("10", "15", "20", "25")
  ) +  # 设置 x 轴范围和间隔+
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2, color = 'black') +  # 添加水平参考线
  geom_hline(data = Net, aes(yintercept = `Net Drift (%/year)`), color = 'Blue',
             linetype = 1) +  # 添加 Net Drift 线
  geom_hline(data = Net, aes(yintercept = `CI Lo`), color = 'Blue',
             linetype = 2) +  # 添加 Net Drift 置信区间下限
  geom_hline(data = Net, aes(yintercept = `CI Hi`), color = 'Blue',
             linetype = 2) +  # 添加 Net Drift 置信区间上限
  labs(title = "Local Drift and Net Drift with Confidence Intervals",
       x = "Age",
       y = "Percent per Year",
       color = "Group",
       fill = "Group")
p4
p4 <- ggplot(data = Local, aes(x = Age, y = `Percent per Year`, color = group)) +
  # 1. 绘制置信区间 (关键修改：color = NA)
  geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = group), 
              alpha = 0.1, color = NA) + 
  
  geom_line(size = 1) +
  
  # 2. 修正 X 轴范围
  scale_x_continuous(limits = c(10, 25), breaks = seq(10, 25, by = 5)) +
  
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2, color = 'black') +
  
  # Net Drift 线
  geom_hline(data = Net, aes(yintercept = `Net Drift (%/year)`), 
             color = 'Blue', linetype = 1) +
  geom_hline(data = Net, aes(yintercept = `CI Lo`), 
             color = 'Blue', linetype = 2) +
  geom_hline(data = Net, aes(yintercept = `CI Hi`), 
             color = 'Blue', linetype = 2) +
  
  labs(title = "Local Drift and Net Drift with Confidence Intervals",
       x = "Age", y = "Percent per Year", color = "Group", fill = "Group")
p4
ggsave("Local Drift and Net Drift1221.pdf",p4,width=8,height=6)



