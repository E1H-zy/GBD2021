
setwd("F:\\")
#install.packages("forecast")
#install.packages("readxl")
library(forecast)
library(ggplot2)
library(readxl)
library(ggpubr)
#install.packages("tseries")
#install.packages("forecast")
library(tseries)


male_in<-read_excel("ARIMA.xlsx",sheet=2)

male_in_ts <- ts(male_in$val, start = 1990, frequency = 1)

plot(male_in_ts)
kpss.test(male_in_ts)

fit1 <- auto.arima(male_in_ts)   
fit1                             
summary(fit1)                   

forecasted_values1 <- forecast(fit1, h = 15)
forecast_df1 <- data.frame(             
  Year = c(time(male_in_ts), time(forecasted_values1$mean)),
  Value = c(as.numeric(male_in_ts), as.numeric(forecasted_values1$mean)),
  Type = c(rep("Actual", length(male_in_ts)), rep("Forecast", length(forecasted_values1$mean)))
)


p1<-ggplot() +
  geom_line(data = forecast_df1, aes(x = Year, y = Value, color = Type), size = 1.2) +
  geom_line(data = forecast_df1[forecast_df1$Type == "Forecast", ], 
            aes(x = Year, y = Value, color = Type), size = 1.2) +
  geom_point(data = forecast_df1[forecast_df1$Type == "Forecast", ], 
             aes(x = Year, y = Value, color = Type), size = 2, shape = 21, 
             fill = "yellow", color = "black", stroke = 0.5) +
  geom_ribbon(data = data.frame(
    Year = time(forecasted_values1$mean),
    ymin = forecasted_values1$lower[,2],
    ymax = forecasted_values1$upper[,2]
  ), aes(x = Year, ymin = ymin, ymax = ymax), fill = "yellow", alpha = 0.2) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "grey", size = 1) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "yellow")) +
  labs(title = "ASIR of Male", x = "Year", y = "ASIR") +
  ylim(1200, 1400) +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0, vjust = 1, face = "bold", size = 14),
        plot.margin = margin(10, 10, 10, 10))
p1


female_in<-read_excel("ARIMA.xlsx",sheet=1)

female_in_ts <- ts(female_in$val, start = 1990, frequency = 1)

plot(female_in_ts)

fit2 <- auto.arima(female_in_ts)
fit2
summary(fit2)

forecasted_values2 <- forecast(fit2, h = 15)
forecast_df2 <- data.frame(
  Year = c(time(female_in_ts), time(forecasted_values2$mean)),
  Value = c(as.numeric(female_in_ts), as.numeric(forecasted_values2$mean)),
  Type = c(rep("Actual", length(female_in_ts)), rep("Forecast", length(forecasted_values2$mean)))
)


p2<-ggplot() +
  geom_line(data = forecast_df2, aes(x = Year, y = Value, color = Type), size = 1.2) +
  geom_line(data = forecast_df2[forecast_df2$Type == "Forecast", ], aes(x = Year, y = Value, color = Type), size = 1.2) +
  geom_point(data = forecast_df2[forecast_df2$Type == "Forecast", ], aes(x = Year, y = Value, color = Type), size = 2, shape = 21, fill = "yellow", color = "black", stroke = 0.5) +
  geom_ribbon(data = data.frame(
    Year = time(forecasted_values2$mean),
    ymin = forecasted_values2$lower[,2],
    ymax = forecasted_values2$upper[,2]
  ), aes(x = Year, ymin = ymin, ymax = ymax), fill = "yellow", alpha = 0.2) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "grey", size = 1) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "yellow")) +
  labs(title = "ASIR of Female", x = "Year", y = "ASIR") +
  ylim(1750, 1950) +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0, vjust = 1, face = "bold", size = 14),
        plot.margin = margin(10, 10, 10, 10))
p2

both_in<-read_excel("ARIMA.xlsx",sheet=3)

both_in_ts <- ts(both_in$val, start = 1990, frequency = 1)

plot(both_in_ts)

fit3 <- auto.arima(both_in_ts)
fit3
summary(fit3)

forecasted_values3 <- forecast(fit3, h = 15)
forecast_df3 <- data.frame(
  Year = c(time(both_in_ts), time(forecasted_values3$mean)),
  Value = c(as.numeric(both_in_ts), as.numeric(forecasted_values3$mean)),
  Type = c(rep("Actual", length(both_in_ts)), rep("Forecast", length(forecasted_values3$mean)))
)


p3<-ggplot() +
  geom_line(data = forecast_df3, aes(x = Year, y = Value, color = Type), size = 1.2) +
  geom_line(data = forecast_df3[forecast_df3$Type == "Forecast", ], aes(x = Year, y = Value, color = Type), size = 1.2) +
  geom_point(data = forecast_df3[forecast_df3$Type == "Forecast", ], aes(x = Year, y = Value, color = Type), size = 2, shape = 21, fill = "yellow", color = "black", stroke = 0.5) +
  geom_ribbon(data = data.frame(
    Year = time(forecasted_values3$mean),
    ymin = forecasted_values3$lower[,2],
    ymax = forecasted_values3$upper[,2]
  ), aes(x = Year, ymin = ymin, ymax = ymax), fill = "yellow", alpha = 0.2) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "grey", size = 1) +
  scale_color_manual(values = c("Actual" = "red", "Forecast" = "yellow")) +
  labs(title = "ASIR of Both", x = "Year", y = "ASIR") +
  ylim(1525, 1600) +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0, vjust = 1, face = "bold", size = 14),
        plot.margin = margin(10, 10, 10, 10))
p3


#install.packages("gridExtra")
library(gridExtra)

grid.arrange(p1, p2, p3, ncol = 3) 
grid.arrange(p3,p1, p2,  ncol = 3, widths = c(1, 1, 1)) 
ggsave("ARIMA.pdf", grid.arrange(p3,p1, p2, ncol = 3), width = 14, height = 3,dpi = 300)




