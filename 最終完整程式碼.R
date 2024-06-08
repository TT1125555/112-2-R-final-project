# 資料來源: 政府資料開放平台：https://data.gov.tw/
# 時間：2006年1月-2024年4月

# 台灣電力公司_水力購電實績: https://data.gov.tw/dataset/32988
# 台灣電力公司_風力購電實績: https://data.gov.tw/dataset/32987
# 台灣電力公司_太陽光電購電實績: https://data.gov.tw/dataset/32986
# 台灣電力公司_汽電共生購電實績: https://data.gov.tw/dataset/32985


#-----------------------------------------------------------------------------------------------------------------

# 加载所需的包
library(tidyverse)
library(lubridate)

#-----------------------------------------------------------------------------------------------------------------

#第一部分(資料整理和趨勢圖)

# 读取数据
hydro_data <- read_csv("台灣電力公司_水力購電實績.csv")
wind_data <- read_csv("台灣電力公司_風力購電實績.csv")
solar_data <- read_csv("台灣電力公司_太陽光電購電實績.csv")
cogeneration_data <- read_csv("台灣電力公司_汽電共生購電實績.csv")

# 确保列名一致
colnames(hydro_data) <- c("年度", "月份", "度數(千度)")
colnames(wind_data) <- c("年度", "月份", "度數(千度)")
colnames(solar_data) <- c("年度", "月份", "度數(千度)")
colnames(cogeneration_data) <- c("年度", "月份", "度數(千度)")

# 定义一个函数来处理数据
format_data <- function(data, source_name) {
  data %>%
    # 移除空白和非法字符
    mutate(across(everything(), ~str_replace_all(., "[[:space:]]", ""))) %>%
    # 将非法字符转为 NA
    mutate(
      `度數(千度)` = as.numeric(str_replace(`度數(千度)`, "[^0-9.]", NA_character_)),
      年度 = as.numeric(年度),
      月份 = as.numeric(月份)
    ) %>%
    # 创建日期列
    mutate(
      年度 = ifelse(年度 < 1911, 年度 + 1911, 年度), # 将民国年份转换为公历年份
      日期 = ymd(paste0(年度, "-", str_pad(月份, 2, pad = "0"), "-01")),
      Source = source_name
    ) %>%
    # 移除日期格式错误的数据
    filter(!is.na(日期)) %>%
    na.omit()
}

# 格式化数据
hydro_data <- format_data(hydro_data, "Hydro Power")
wind_data <- format_data(wind_data, "Wind Power")
solar_data <- format_data(solar_data, "Solar Power")
cogeneration_data <- format_data(cogeneration_data, "Cogeneration Power")

# 合并数据框
combined_data <- bind_rows(hydro_data, wind_data, solar_data, cogeneration_data)

# 調整趨勢圖
trend_plot <- ggplot(combined_data, aes(x = 日期, y = `度數(千度)`, color = Source)) +
  geom_line() +
  labs(title = "台灣電力公司購電實績趨勢 (2006年1月-2024年4月)", 
       x = "日期", y = "度數(千度)", 
       caption = "資料來源: 台灣電力公司") +
  scale_color_manual(values = c("Hydro Power" = "blue", "Wind Power" = "green", 
                                "Solar Power" = "yellow", "Cogeneration Power" = "red")) +
  theme_minimal()

# 顯示趨勢圖
print(trend_plot)


#----------------------------------------------------------------------------------------------------

#第二部分(堆疊圖)

# 生成堆疊圖
stacked_bar_plot <- ggplot(combined_data, aes(x = 年度, y = `度數(千度)`, fill = Source)) +
  geom_bar(stat = "identity") +
  labs(title = "台灣電力公司購電實績堆疊圖(2006年1月-2024年4月)", x = "年度", y = "度數(千度)", fill = "Source") +
  scale_fill_manual(values = c("Hydro Power" = "blue", "Wind Power" = "green", 
                               "Solar Power" = "yellow", "Cogeneration Power" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(combined_data$年度),
                     labels = function(x) substring(as.character(x), 3)) # 將年份前兩個數字去除

# 顯示堆疊圖
print(stacked_bar_plot)


#-------------------------------------------------------------------------------------------------------

#最終結果

# 顯示趨勢圖
print(trend_plot)
# 顯示堆疊圖
print(stacked_bar_plot)


#--------------------------------------------------------------------------------------------------------

#補充：如果以圓餅圖來呈現
library(ggplot2)

# 分割資料，2020年之前的資料和2020年之後的資料
before_2020 <- filter(combined_data, 年度 < 2020)
after_2020 <- filter(combined_data, 年度 >= 2020)

# 計算各種能源的總和
before_2020_sum <- before_2020 %>%
  group_by(Source) %>%
  summarise(total = sum(`度數(千度)`))

after_2020_sum <- after_2020 %>%
  group_by(Source) %>%
  summarise(total = sum(`度數(千度)`))

# 2020年之前的比例分布圓餅圖
before_plot <- ggplot(before_2020_sum, aes(x = "", y = total, fill = Source)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(total / sum(total) * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "2020年之前各種能源比例分布圓餅圖", fill = "Energy Source") +
  theme_void() +
  theme(legend.position = "right")

# 2020年之後的比例分布圓餅圖
after_plot <- ggplot(after_2020_sum, aes(x = "", y = total, fill = Source)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(total / sum(total) * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "2020年之後各種能源比例分布圓餅圖", fill = "Energy Source") +
  theme_void() +
  theme(legend.position = "right")

# 顯示圖表
before_plot
after_plot



