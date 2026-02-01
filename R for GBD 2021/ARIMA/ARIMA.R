
setwd("F:\\我的工作文件夹\\11 ARIMA")
# 安装并加载必要的包
#install.packages("forecast")
#install.packages("readxl")
library(forecast)
library(ggplot2)
library(readxl)
library(ggpubr)
#install.packages("tseries")
#install.packages("forecast")
library(tseries)


#####男性ASIR 导入数据#####
male_in<-read_excel("ARIMA.xlsx",sheet=2)

# 将数据转换为时间序列对象
male_in_ts <- ts(male_in$val, start = 1990, frequency = 1)

# 检查数据
plot(male_in_ts)
kpss.test(male_in_ts)

# 拟合ARIMA模型
fit1 <- auto.arima(male_in_ts)   ###自动拟合出最佳的p,d,q的值
fit1                             ###查看p,d,q的值
summary(fit1)                    ###查看模型的AIC,BIC的指标

# 进行预测
forecasted_values1 <- forecast(fit1, h = 15) ###预测未来15年数据
# 将预测结果转换为数据框
forecast_df1 <- data.frame(             
  Year = c(time(male_in_ts), time(forecasted_values1$mean)),
  Value = c(as.numeric(male_in_ts), as.numeric(forecasted_values1$mean)),
  Type = c(rep("Actual", length(male_in_ts)), rep("Forecast", length(forecasted_values1$mean)))
)

# 绘制预测结果

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



#####女性ASIR导入数据#####
female_in<-read_excel("ARIMA.xlsx",sheet=1)

# 将数据转换为时间序列对象
female_in_ts <- ts(female_in$val, start = 1990, frequency = 1)

# 检查数据
plot(female_in_ts)

# 拟合ARIMA模型
fit2 <- auto.arima(female_in_ts)
fit2
summary(fit2)

# 进行预测
forecasted_values2 <- forecast(fit2, h = 15)
# 将预测结果转换为数据框
forecast_df2 <- data.frame(
  Year = c(time(female_in_ts), time(forecasted_values2$mean)),
  Value = c(as.numeric(female_in_ts), as.numeric(forecasted_values2$mean)),
  Type = c(rep("Actual", length(female_in_ts)), rep("Forecast", length(forecasted_values2$mean)))
)

# 绘制预测结果

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

#####Both ASIR导入数据#####
both_in<-read_excel("ARIMA.xlsx",sheet=3)

# 将数据转换为时间序列对象
both_in_ts <- ts(both_in$val, start = 1990, frequency = 1)

# 检查数据
plot(both_in_ts)

# 拟合ARIMA模型
fit3 <- auto.arima(both_in_ts)
fit3
summary(fit3)

# 进行预测
forecasted_values3 <- forecast(fit3, h = 15)
# 将预测结果转换为数据框
forecast_df3 <- data.frame(
  Year = c(time(both_in_ts), time(forecasted_values3$mean)),
  Value = c(as.numeric(both_in_ts), as.numeric(forecasted_values3$mean)),
  Type = c(rep("Actual", length(both_in_ts)), rep("Forecast", length(forecasted_values3$mean)))
)

# 绘制预测结果

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




# 安装并加载 gridExtra 包
#install.packages("gridExtra")
library(gridExtra)

# 假设 p1, p2, p3 是三个 ggplot 对象
grid.arrange(p1, p2, p3, ncol = 3)  # 横排排列
# 调整各个图形的宽度
grid.arrange(p3,p1, p2,  ncol = 3, widths = c(1, 1, 1))  # 可调整每个图形的宽度比例
ggsave("ARIMA.pdf", grid.arrange(p3,p1, p2, ncol = 3), width = 14, height = 3,dpi = 300)




