
a <- mydata %>%
  filter(metric_name == "Number") %>%
  group_by(year) %>%
  summarise(total_daly = sum(val))

rank <- mydata %>%
  group_by(year, metric_name) %>%
  mutate(pop_global = sum(pop, na.rm = TRUE)) %>%  
  arrange(sdi) %>%
  mutate(
    cummu = cumsum(pop),             
    half = pop / 2,               
    midpoint = cummu - half,       
    weighted_order = midpoint / pop_global 
  )


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

ci_result <- ci %>%
  group_by(year) %>%
  summarise(CI = 2 * mean(frac_daly) - 1)

print(ci_result)
#write.csv(ci_result, "CI_results.csv", row.names = FALSE)

