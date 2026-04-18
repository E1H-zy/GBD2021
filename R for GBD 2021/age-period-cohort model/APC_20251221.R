setwd("D:/")
library(ggplot2)
library(dplyr)

APC_both <- readRDS("APC_analysis_20250223_035746.969228_output.rds")
APC_male <- readRDS("APC_analysis_20250223_041747.996011_output.rds")
APC_female <- readRDS("APC_analysis_20250223_042330.582674_output.rds")

long_age_both <- APC_both[["LongAge"]] %>% as.data.frame() %>% mutate(group = "both")
long_age_male <- APC_male[["LongAge"]] %>% as.data.frame() %>% mutate(group = "male")
long_age_female <- APC_female[["LongAge"]] %>% as.data.frame() %>% mutate(group = "female")
long_age <- bind_rows(long_age_both, long_age_male, long_age_female)

ggplot(data = long_age, aes(x = Age, y = Rate, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = CILo, ymax = CIHi), fill = "grey", alpha = 0.3) +  
  scale_x_continuous(
    limits = c(10, 25),      
    breaks = seq(10, 25, 5), 
    labels = c("10", "15", "20", "25")
  ) + 
  labs(title = "Long Age Curve with Confidence Intervals",
       x = "Age",
       y = "Rate",
       color = "Group") +
  theme_bw()

p1 <- ggplot(data = long_age, aes(x = Age, y = Rate, color = group)) +
  geom_line() +  
  geom_smooth(aes(ymin = CILo, ymax = CIHi), method = "loess", se = TRUE, fill = "grey", alpha = 0.3) + 
  scale_x_continuous(
    limits = c(10, 25),    
    breaks = seq(10, 25, 5), 
    labels = c("10", "15", "20", "25")
  ) +
  labs(title = "Long Age Curve with Confidence Intervals",
       x = "Age",
       y = "Rate",
       color = "Group") +
  theme_bw()

ggsave("f",p1,width=8,height=6)

cohort_both <- APC_both[["CohortRR"]] %>% as.data.frame() %>% mutate(group = "both")
cohort_male <- APC_male[["CohortRR"]] %>% as.data.frame() %>% mutate(group = "male")
cohort_female <- APC_female[["CohortRR"]] %>% as.data.frame() %>% mutate(group = "female")

CohortRR <- bind_rows(cohort_both, cohort_male, cohort_female)

p2 <- ggplot(data = CohortRR, aes(x = Cohort, y = `Rate Ratio`, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = group), alpha = 0.1,color = NA) +  # 绘制置信区间
  theme_bw() +
  geom_hline(yintercept = 1, linetype = 2, color = 'black') +  
  geom_vline(xintercept = CohortRR[which(CohortRR$`Rate Ratio` == 1), 1],
             linetype = 2, color = 'black') + 
  labs(title = "Cohort Rate Ratio(1990-2019)",
       x = "Cohort",
       y = "Rate Ratio",
       color = "Group",
       fill = "Group")
p2
ggsave("cohortRR1221.pdf",p2,width=8,height=6)

period_both <- APC_both[["PeriodRR"]] %>% as.data.frame() %>% mutate(group = "both")
period_male <- APC_male[["PeriodRR"]] %>% as.data.frame() %>% mutate(group = "male")
period_female <- APC_female[["PeriodRR"]] %>% as.data.frame() %>% mutate(group = "female")

PeriodRR <- bind_rows(period_both, period_male, period_female)

p3 <- ggplot(data = PeriodRR, aes(x = Period, y = `Rate Ratio`, color = group)) +
  geom_line() +  
  geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = group), alpha = 0.1) + 
  theme_bw() +
  geom_hline(yintercept = 1, linetype = 2, color = 'black') +
  geom_vline(xintercept = PeriodRR[which(PeriodRR$`Rate Ratio` == 1), 1],
             linetype = 2, color = 'black') + 
  labs(title = "Period Rate Ratio (1990-2019)",
       x = "Period",
       y = "Rate Ratio",
       color = "Group",
       fill = "Group")
p3

p3<- ggplot(data = PeriodRR, aes(x = Period, y = `Rate Ratio`, color = group)) +
  geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = group), 
              alpha = 0.1, color = NA) + 
  
  geom_line(size = 1) +
  theme_bw() +
  geom_hline(yintercept = 1, linetype = 2, color = 'black') +
  geom_vline(xintercept = PeriodRR[which(PeriodRR$`Rate Ratio` == 1), 1][1],
             linetype = 2, color = 'black') +
  labs(title = "Period Rate Ratio (1990-2019)", 
       caption = "Note: Data from 2020-2021 excluded for APC model validity.",
       x = "Period", y = "Rate Ratio", color = "Group", fill = "Group")
ggsave(".pdf",p3,width=8,height=6)

local_both <- APC_both[["LocalDrifts"]] %>% as.data.frame() %>% mutate(group = "both")
local_male <- APC_male[["LocalDrifts"]] %>% as.data.frame() %>% mutate(group = "male")
local_female <- APC_female[["LocalDrifts"]] %>% as.data.frame() %>% mutate(group = "female")

Local <- bind_rows(local_both, local_male, local_female)

Net <- APC_both[["NetDrift"]] %>% as.data.frame()

p4 <- ggplot(data = Local, aes(x = Age, y = `Percent per Year`, color = group)) +
  geom_line() +  
  geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = group), alpha = 0.1) +
  scale_x_continuous(
    limits = c(10, 25),      
    breaks = seq(10, 25, 5), 
    labels = c("10", "15", "20", "25")
  ) +  
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2, color = 'black') +  
  geom_hline(data = Net, aes(yintercept = `Net Drift (%/year)`), color = 'Blue',
             linetype = 1) +  
  geom_hline(data = Net, aes(yintercept = `CI Lo`), color = 'Blue',
             linetype = 2) +  
  geom_hline(data = Net, aes(yintercept = `CI Hi`), color = 'Blue',
             linetype = 2) + 
  labs(title = "Local Drift and Net Drift with Confidence Intervals",
       x = "Age",
       y = "Percent per Year",
       color = "Group",
       fill = "Group")
p4
p4 <- ggplot(data = Local, aes(x = Age, y = `Percent per Year`, color = group)) +
  geom_ribbon(aes(ymin = CILo, ymax = CIHi, fill = group), 
              alpha = 0.1, color = NA) + 
  
  geom_line(size = 1) +
  scale_x_continuous(limits = c(10, 25), breaks = seq(10, 25, by = 5)) +
  
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2, color = 'black') +
  
  geom_hline(data = Net, aes(yintercept = `Net Drift (%/year)`), 
             color = 'Blue', linetype = 1) +
  geom_hline(data = Net, aes(yintercept = `CI Lo`), 
             color = 'Blue', linetype = 2) +
  geom_hline(data = Net, aes(yintercept = `CI Hi`), 
             color = 'Blue', linetype = 2) +
  
  labs(title = "Local Drift and Net Drift with Confidence Intervals",
       x = "Age", y = "Percent per Year", color = "Group", fill = "Group")
p4
ggsave(".pdf",p4,width=8,height=6)



