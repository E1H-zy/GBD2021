
# 计算每年总 DALY
a <- mydata %>%
  filter(metric_name == "Number") %>%
  group_by(year) %>%
  summarise(total_daly = sum(val))

rank <- mydata %>%
  group_by(year, metric_name) %>%
  mutate(pop_global = sum(pop, na.rm = TRUE)) %>%  # 每年总人口
  arrange(sdi) %>%
  mutate(
    cummu = cumsum(pop),              # 累积人口
    half = pop / 2,                   # 本国人口的一半
    midpoint = cummu - half,          # 人口中点
    weighted_order = midpoint / pop_global  # 相对位置
  )

# 合并总DALY到rank表
ci <- rank %>%
  filter(metric_name == "Number") %>%
  left_join(a, by = "year") %>%
  group_by(year) %>%
  arrange(sdi) %>%
  mutate(
    cummu_daly = cumsum(val),
    frac_daly = cummu_daly / total_daly,
    frac_population = cummu / pop_global
  )

# 计算每年的 Concentration Index
ci_result <- ci %>%
  group_by(year) %>%
  summarise(CI = 2 * mean(frac_daly) - 1)

print(ci_result)
#write.csv(ci_result, "CI_results.csv", row.names = FALSE)

