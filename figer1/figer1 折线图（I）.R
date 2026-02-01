setwd("F:/table+figer资料")
df <- read.csv("Urticaria.csv",header = T)
unique(df$sex)
unique(df$metric)
unique(df$age)

library(ggplot2)
library(dplyr)
library(scales)
library(ggsci)  # 确保你安装了 ggsci 以使用 scale_color_nejm()

fig1 <- df |>
  filter(cause == "Urticaria") |>
  filter(sex %in% c("Male", "Female")) |>
  filter(measure == "Incidence") |>
  filter(age == "Age-standardized") |>
  filter(metric == "Rate") |>
  filter(year %in% c(seq(1990, 2020, by = 5), 2021)) |>  # 确保包含 2021
  mutate(year = as.numeric(as.character(year))) |>  # 确保 year 是数值型
  mutate(sex = factor(sex, levels = c("Male", "Female"))) |>
  ggplot(aes(x = year, y = val, color = sex, group = sex)) +
  
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex), alpha = 0.1, color = NA) +
  geom_line(size = 1) +  
  geom_point(size = 1.5) +  
  
  scale_x_continuous(breaks = seq(1990, 2021, by = 1), limits = c(1990, 2021)) +  # 确保 2021 显示
  
  scale_y_continuous(labels = scales::comma, limits = c(1000, 2500), expand = c(0, 0)) +
  
  labs(y = "Incidence rate (per 100,000 population)", x = "Year") +
  
  theme_classic() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  scale_color_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e")) +
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e"))

fig1



ggsave(" figer1折线图（I）.jpeg",width = 8,height = 6,dpi = 300)

