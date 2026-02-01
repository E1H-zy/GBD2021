library(tidyverse)
library(ggsci)
library(scales)
# 读取数据
setwd("F:/table+figer资料")
df <- read.csv("Urticaria.csv",header = T)
unique(df$measure)

fig1 <- df |>
  mutate(year = as.numeric(as.character(year))) |>  # 确保 year 是数值型
  filter(cause == "Urticaria") |>
  filter(sex == "Both") |>
  filter(measure == "Prevalence") |>
  filter(age %in% c("10-14 years", "15-19 years", "20-24 years")) |>
  filter(metric == "Number") |>
  filter(year %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2021))|>
  mutate(year = factor(year)) |> 
  mutate(age = factor(age, levels = c("10-14 years", "15-19 years", "20-24 years"))) |>
  ggplot(aes(x = year, y = val, fill = age)) +
  
  geom_col(position = position_dodge(width = 0.7), width = 0.8) +
  
  scale_y_continuous(labels = comma, limits = c(0, NA), expand = c(0, 0)) +
  
  # 强制显示所有年份标签，包括 2021
  scale_x_discrete(limits = as.character(c(seq(1990, 2020, by = 5), 2021))) +
  
  labs(y = "Prevalence number", x = "Year") +
  theme_classic() +
  theme(axis.line.x = element_line(color = "black")) +
  scale_fill_nejm()

fig1

ggsave(" 06-29figer1柱状图（P）.pdf",width = 6,height = 4,dpi = 300)