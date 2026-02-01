library(tidyverse)
# 读取数据
setwd("F:/table+figer资料")
df <- read.csv("Urticaria.csv",header = T)
unique(df$cause)
colnames(df)
unique(df$measure)
unique(df$location)
unique(df$sex)
unique(df$age)
unique(df$metric)
unique(df$year)
# 选择数据并且画图
fig1 <- df |>
  filter(cause == "Urticaria") |>
  filter(sex == "Both") |>
  filter(measure == "Incidence") |>
  filter(age %in% c("10-14 years", "15-19 years", "20-24 years")) |>  # 修正括号
  filter(metric == "Number") |>
  filter(year %in% seq(1990, 2019, by = 5)) |>
  mutate(year = factor(year)) |> 
  mutate(age = factor(age, levels = c("10-14 years", "15-19 years", "20-24 years"))) |>
  ggplot(aes(x = year, y = val, fill = age)) +
  geom_col(position = "dodge", width = 0.8)

fig1

library(scales)
fig1 + 
  scale_y_continuous(labels = comma)+
  labs(y="Incidence number")+
  theme(axis.title = element_text(size = 10))