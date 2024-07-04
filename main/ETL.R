library(openxlsx)
library(data.table)
library(stringr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(glue)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(dplyr)
library(readxl)
library(stringr)
library(showtext)
library(ggstance)
library(ggbreak)
library(plotly)
library(ggpubr)
library(tidyverse)
library(ggrepel)
font_add("Canger", "/Library/Fonts/仓耳今楷01-W04.ttf")
font_families()
showtext_auto()  # 全局自动使用
#showtext_auto(FALSE) # 不需要就关闭

setwd('/Users/sousekilyu/Documents/GitHub/GaoKaoVer2')

# This code defines two vectors: .noun and .major.
.majorList <- list(
  c("新闻|传播", "新闻传播学"),
  c("法学|法律", "法学"),
  c("计算机", "计算机类"),
  c("软件", "软件工程"),
  c("土木", "土木工程类"),
  c("数据科学与大数据技术", "数据科学与大数据技术"),
  c("自然保护与环境生态|环境生态", "环境生态类"),
  c("轨道交通电气与控制", "轨道交通电气类"),
  c("旅游管理", "旅游管理")
)
majorData <- .majorList %>%
  data.frame() %>%
  t() %>%
  as.data.frame() %>%
  setNames(c("noun", "major"))
row.names(majorData) <- NULL

## 粗类
.majorList2 <- list(
  c("新闻|传播|广告|出版", "新闻传播学"),
  c("翻译|外语|外国语|.*?语$", "外国语言文学"),
  c("法学|法律", "法学"),
  c("中文|汉语言", "汉语言"),
  c("哲学", "哲学"),
  c("金融", "金融类"),
  c("经济|贸易", "经济学类"),
  c("历史|文物|考古|文博", "历史学类"),
  c("政治学|思想政治", "政治学类"),
  c("工商管理", "工商管理"),
  #c("管理科学", "管理科学与工程"),
  c("心理", "心理学"),
  c("公共管理|行政管理|社会保障", "公共管理类"),
  c("社会学|社会工作|人类学|民族学|民俗学", "社会学类"),
  c("数学", "数学类"),
  c("电气", "电气类"),
  c("通信", "通信类"),
  c("电子", "电子类"),
  c("机械", "机械类"),
  c("计算机|人工智能", "计算机类"),
  #c("人工智能", "计算机类(人工智能)"),
  c("软件", "软件工程"),
  c("土木", "土木工程类"),
  c("^统计学$|应用统计|经济统计", "统计学类"),
  c("建筑|城乡规划", "建筑学类"),
  c("生物", "生物类"),
  c("材料", "材料类"),
  c("化学", "化学类"),
  c("基地", "基地班"),
  c("拔尖", "拔尖班"),
  c("环境科学|环境工程", "环境科学类"),
  c("临床医学", "临床医学"),
  c("口腔", "口腔医学"),
  c("临床药学|药学", "药学类"),
  c("林学|林业|草|动物|水产|农业", "农业类"),
  c("信息管理|档案|图书", "信息管理与图书情报"),
  c("地球|地质", "地质学")
)
majorData_rough <- .majorList2 %>%
  data.frame() %>%
  t() %>%
  as.data.frame() %>%
  setNames(c("noun", "major"))
row.names(majorData_rough) <- NULL

# read data
## function to elt the web data
read_and_process_data <- function(year) {
  filepath <- paste0("data/", year, "年山东省普通一批投档线.xlsx")
  
  dt <- read_excel(filepath)
  colnames(dt) <- c("专业", "院校", "计划数", "位次")
  
  # include 中外合作
  dt %<>%
    dplyr::filter(!grepl("定向|预科", 专业))
  
  dt$院校 %<>%
    str_replace_all("\\（.*?\\）", "") %>%
    str_replace_all("\\(.*?\\)", "")
  
  dt$专业 %<>%
    str_replace_all("\\（.*?\\）", "") %>%
    str_replace_all("\\(.*?\\)", "")
  
  dt$位次 %<>%
    str_replace_all("前50名", "50") %>%
    as.numeric()
  
  # 院校位次取中位数
  dt_school <- dt %>%
    dplyr::filter(!is.infinite(位次), !is.na(位次)) %>%
    dplyr::group_by(`院校`) %>%
    dplyr::summarise(rank_by_school = median(`位次`, na.rm = TRUE),
                     .groups = "keep") %>%
    ungroup()
  
  return(list(dt = dt, dt_school = dt_school))
}

years <- 2020:2023
data_list <- lapply(years, read_and_process_data)
names(data_list) <- paste0("dt", years)
# split data
for (year in years) {
  assign(paste0("dt", year), data_list[[paste0("dt", year)]]$dt)
  assign(paste0("dt", year, "_school"), data_list[[paste0("dt", year)]]$dt_school)
}


# 去掉专业名称前的编码
process_data <- function(data) {
  data %>%
    mutate(`专业` = str_sub(`专业`, start = 3)) %>%
    group_by(`院校`, `专业`) %>%
    summarise(`计划数` = sum(`计划数`), `位次` = max(`位次`, na.rm = TRUE), .groups = "keep") %>%
    ungroup()
}

dt2023_cmb <- process_data(dt2023)
dt2022_cmb <- process_data(dt2022)
dt2021_cmb <- process_data(dt2021)
dt2020_cmb <- process_data(dt2020)

# 根据dt2023_cmb的`专业`字段，判断是否包含.majorList的noun字段，匹配.majorList的major字段
majorData$major <- as.character(majorData$major)

update_major <- function(df, majorData) {
  df$major <- NA
  for (i in 1:nrow(majorData)) {
    df$major[grepl(majorData$noun[i], df$专业)] <- majorData$major[i]
  }
  df %<>%
    mutate(major = ifelse(!is.na(major), major, `专业`))
  # 根据院校和专业类别分组，计算计划数和位次
  df %<>%
    group_by(院校, major) %>%
    summarise(计划数 = sum(计划数),
              rank_by_major = max(位次, na.rm = TRUE),
              .groups = "keep") %>%
    ungroup()
  
  return(df)
}

dt2023_cmb <- update_major(dt2023_cmb, majorData)
dt2022_cmb <- update_major(dt2022_cmb, majorData)
dt2021_cmb <- update_major(dt2021_cmb, majorData)
dt2020_cmb <- update_major(dt2020_cmb, majorData)

# Define a function to perform the operations
perform_operations <- function(data, dt_school, year) {
  data %>%
    left_join(dt_school, by = c("院校" = "院校")) %>%
    mutate(year = year)
}
# Apply the function to each dataset
dt2023_rank_cmb <- perform_operations(dt2023_cmb, dt2023_school, 2023)
dt2022_rank_cmb <- perform_operations(dt2022_cmb, dt2022_school, 2022)
dt2021_rank_cmb <- perform_operations(dt2021_cmb, dt2021_school, 2021)
dt2020_rank_cmb <- perform_operations(dt2020_cmb, dt2020_school, 2020)  # Assuming you have a dt2020_cmb data
dt_rank_cmb <- bind_rows(dt2023_rank_cmb, dt2022_rank_cmb, dt2021_rank_cmb, dt2020_rank_cmb)  %>% 
  mutate(school = substr(院校, 5, nchar(院校))) %>% 
  left_join(dplyr::select(
    read_excel("/Users/sousekilyu/Documents/GitHub/GaoKaoVer2/data/全国普通高等学校名单.xlsx"),
    school, city, province
  ))

# Calculate the scaled score for each major based on its rank
# The higher the score_by_major_scale, the more popular the major is
dt_rank_cmb <- dt_rank_cmb %>%
  group_by(year) %>%
  mutate(
    score_by_major_scale = 100 - (rank_by_major - min(rank_by_major)) / (max(rank_by_major) - min(rank_by_major)) * 100,
    score_by_school_scale = 100 - (rank_by_school - min(rank_by_school)) / (max(rank_by_school) - min(rank_by_school)) * 100
  ) %>%
  ungroup() %>%  
  rename(frequency = "计划数")

# Calculate the change in scores by major over time
score_by_major_change <- dt_rank_cmb %>%
  group_by(院校, major, province, city) %>%
  filter(year %in% c(2020, 2023)) %>%
  arrange(year) %>%
  summarise(
    # Calculate the change in scores by major over time
    countn = n(),
    score_by_major_early = first(score_by_major_scale),
    # Get the first score for each major
    score_by_major_later = last(score_by_major_scale),
    # Get the last score for each major
    score_by_major_change = score_by_major_later - score_by_major_early, # Calculate the change in scores
    .groups = "keep"
  ) %>%
  filter(countn > 1) %>%
  ungroup() %>%
  arrange(desc(score_by_major_change)) # Arrange the data by the change in scores in descending order
head(score_by_major_change)
# slice(1:100)
# The variable 'score_by_major_change' represents the change in popularity of a major.

# Roughly categorize the majors
majorData_rough$major <- as.character(majorData_rough$major)
update_major_rough <- function(df, majorData_rough) {
  df$major_rough <- NA
  for (i in 1:nrow(majorData_rough)) {
    df$major_rough[grepl(majorData_rough$noun[i], df$major)] <- majorData_rough$major[i]
  }
  df %<>%
    mutate(major_rough = ifelse(!is.na(major_rough), major_rough, major))

  return(df)
}
score_by_major_rough_change <- update_major_rough(score_by_major_change, majorData_rough)
dt_rank_cmb_rough <- update_major_rough(dt_rank_cmb, majorData_rough)
head(score_by_major_rough_change)

## 重点城市&高校
project211 <- c(
  "北京大学", "中国人民大学", "清华大学", "北京交通大学", "北京工业大学", "北京航空航天大学",
  "北京理工大学", "北京科技大学", "北京化工大学", "北京邮电大学", "中国农业大学", "北京林业大学",
  "北京中医药大学", "北京师范大学", "北京外国语大学", "中国传媒大学", "中央财经大学", "对外经济贸易大学",
  "北京体育大学", "中央音乐学院", "中央民族大学", "中国政法大学", "华北电力大学", "南开大学",
  "天津大学", "天津医科大学", "河北工业大学", "太原理工大学", "内蒙古大学", "辽宁大学",
  "大连理工大学", "东北大学", "大连海事大学", "吉林大学", "延边大学", "东北师范大学",
  "哈尔滨工业大学", "哈尔滨工程大学", "东北农业大学", "东北林业大学", "复旦大学", "同济大学",
  "上海交通大学", "华东理工大学", "东华大学", "华东师范大学", "上海外国语大学", "上海财经大学",
  "上海大学", "第二军医大学", "南京大学", "苏州大学", "东南大学", "南京航空航天大学",
  "南京理工大学", "中国矿业大学", "河海大学", "江南大学", "南京农业大学", "中国药科大学",
  "南京师范大学", "浙江大学", "安徽大学", "中国科学技术大学", "合肥工业大学", "厦门大学",
  "福州大学", "南昌大学", "山东大学", "中国海洋大学", "中国石油大学", "郑州大学",
  "武汉大学", "华中科技大学", "中国地质大学", "武汉理工大学", "华中农业大学", "华中师范大学",
  "中南财经政法大学", "湖南大学", "中南大学", "湖南师范大学", "国防科技大学", "中山大学",
  "暨南大学", "华南理工大学", "华南师范大学", "广西大学", "海南大学", "四川大学",
  "重庆大学", "西南交通大学", "电子科技大学", "四川农业大学", "西南大学", "西南财经大学",
  "贵州大学", "云南大学", "西藏大学", "西北大学", "西安交通大学", "西北工业大学",
  "西安电子科技大学", "长安大学", "西北农林科技大学", "陕西师范大学", "第四军医大学", "兰州大学",
  "青海大学", "宁夏大学", "新疆大学", "石河子大学"
)

