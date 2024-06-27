source('~/Documents/GitHub/GaoKaoVer2/main/ETL.R')

# Calculate the scaled score for each major based on its rank
# The higher the score_by_major_scale, the more popular the major is
dt_rank_cmb <- dt_rank_cmb %>%
  group_by(year) %>%
  mutate(
    score_by_major_scale = 100 - (rank_by_major - min(rank_by_major)) / (max(rank_by_major) - min(rank_by_major)) * 100,
    score_by_school_scale = 100 - (rank_by_school - min(rank_by_school)) / (max(rank_by_school) - min(rank_by_school)) * 100
  ) %>%
  ungroup()

# Calculate the change in scores by major over time
score_by_major_change <- dt_rank_cmb %>%
  group_by(院校, major) %>%
  filter(year %in% c(2020, 2023)) %>%
  arrange(year) %>%
  summarise(
    # Calculate the change in scores by major over time
    countn = n(),
    score_by_major_early = first(score_by_major_scale),
    # Get the first score for each major
    score_by_major_later = last(score_by_major_scale),
    # Get the last score for each major
    score_by_major_change = score_by_major_later - score_by_major_early,  # Calculate the change in scores
    .groups = "keep"
  ) %>%
  filter(countn > 1) %>%
  ungroup() %>%
  arrange(desc(score_by_major_change))  # Arrange the data by the change in scores in descending order
# slice(1:100)
# The variable 'score_by_major_change' represents the change in popularity of a major.
# A negative value indicates a decrease in popularity, making it less popular.
# A positive value indicates an increase in popularity, making it more popular.

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

### 热门专业与考生成绩分布关系
## 高分段考生 vs 低分段考生
# 2020年：Calculate the average scores by major on different levels of score_by_major_early
score_by_major_group_time <-  dt_rank_cmb %>%
  group_by(year) %>%
  mutate(
    score_group = cut(
      score_by_major_scale,
      breaks = c(-Inf, 50, 70, 90, Inf),
      labels = c("低分段", "中低分段", "中高分段", "高分段")
    ),
    score_group = cut(
      score_by_major_scale,
      breaks = c(-Inf, 50, 70, 90, Inf),
      labels = c("低分段", "中低分段", "中高分段", "高分段")
    )
  ) %>%
  filter(score_group %in% c("低分段", "高分段"))
# plot
generate_plot <- function(time, filename) {
  score_by_major_group_time %>%
    filter(year == time) %>%
    # Assuming df is your data frame and 'your_column' is the column you want to modify
    # mutate(major = paste0(substr(major, 1, 4), "\n", substr(major, 5, nchar(major)))) %>%
    group_by(score_group, major) %>%
    summarise(avg_scores = mean(score_by_major_scale, na.rm = TRUE),.groups = "keep") %>%
    group_by(score_group) %>%
    ggcharts::bar_chart(major, log(avg_scores), fill = score_group, facet = score_group, top_n = 30) +
    theme_bw() +
    theme(
      plot.margin=unit(c(1,1,1,1),"cm"),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        family = "Canger",
        size = 40
      ),
      axis.text.y = element_text(family = "Canger", size = 50),
      axis.title.x = element_text(size = 50),
      axis.title.y = element_text(size = 50),
      strip.text = element_text(size = 45),
      title = element_text(family = "Canger", size = 75),
      legend.position = "none"
    ) +
    labs(title = paste0(time, "年山东省报考热门专业"), x = "专业名称", y = "专业热度(对数值)")
  
  # save png
  ggsave(
    filename,
    width = 12,
    height = 16,
    dpi = 300
  )
}
# Generate plots
generate_plot(2020, "plot/major_by_score_2020.png")
generate_plot(2023, "plot/major_by_score_2023.png")


### 热门专业变化趋势分析
## 哪些专业变多？哪些专业消失？

## 从低分段 跃迁至高分段的 学校和专业

## 2020-2023年
# Plot the word cloud of the change in scores by major
# score_by_major_rough_change %>%
#   filter(major_rough %in% majorData_rough$major) %>%
#   mutate(color = ifelse(score_by_major_change >= 0, "上涨", "下降")) %>%
#   count(major_rough, color) %>%
#   ggplot(aes(
#     label = major_rough,
#     size = n,
#     color = color
#   )) +
#   geom_text_wordcloud_area() +
#   scale_size_area(max_size = 20) +
#   scale_color_manual(
#     values = c("上涨" = "#00BA38", "下降" = "#F8756D"),
#     labels = c("上涨" = "热度上涨", "下降" = "热度下降"),
#     name = "专业热度变化"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     strip.text = element_blank(),
#     title = element_text(family = "Canger", size = 65),
#     legend.position = "bottom",
#     legend.title = element_text(family = "Canger", size = 35),
#     legend.text = element_text(family = "Canger", size = 35)
#   ) +
#   labs(title = "各专业类别涨幅和降幅词云")
# # save png
# ggsave(
#   "plot/major_rough_by_score_change_wordcloud.png",
#   width = 12,
#   height = 12,
#   dpi = 300
# )


## 2020-2023专业热度变化分布
# Plot the distribution of the change in scores by major
# Calculate the average scores by major
avg_scores <- score_by_major_rough_change %>%
  filter(major_rough %in% majorData_rough$major) %>%
  group_by(major_rough) %>%
  summarise(avg_score = mean(score_by_major_change), .groups = "keep")

# Add the average scores to the graph
score_by_major_rough_change %>%
  filter(major_rough %in% majorData_rough$major) %>%
  mutate(color = ifelse(score_by_major_change >= 0, "上涨", "下降")) %>%
  ggplot(aes(x = score_by_major_change, fill = color)) +
  geom_histogram(bins = 100) +
  facet_wrap(~ reorder(major_rough, score_by_major_change, FUN = mean), dir = "h") +
  coord_cartesian(xlim = c(-30, 30), ylim = c(0, 150)) +
  scale_fill_manual(
    values = c("上涨" = "#00BA38", "下降" = "#F8756D"),
    labels = c("上涨" = "热度上涨", "下降" = "热度下降"),
    name = "专业热度变化"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      family = "Canger",
      size = 25
    ),
    axis.text.y = element_text(family = "Canger", size = 25),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    strip.text = element_text(size = 35),
    title = element_text(family = "Canger", size = 65),
    legend.position.inside = c(.8, .07),
    legend.title = element_text(family = "Canger", size = 35),
    legend.text = element_text(family = "Canger", size = 35),
    legend.spacing.y = unit(0.2, "cm")
  ) +
  labs(title = "各专业类别涨幅和降幅分布", x = "2020年 -> 2023年热度变化", y = "数量")
# save png
ggsave(
  "plot/score_by_major_rough_change.png",
  width = 15,
  height = 10,
  dpi = 300
)


### 重点高校热度变化
# !!! 院校位次分数根据中位数排名，而非最低位次
dt_school_top <- dt_rank_cmb %>%
  mutate(school = substr(院校, 5, nchar(院校))) %>%
  filter(year == 2023) %>%
  mutate(rank = dense_rank(desc(score_by_school_scale))) %>%
  #filter(rank <= 30) %>%
  ungroup()
dt_school_top_change <- score_by_major_rough_change %>% 
  filter(院校 %in% dt_school_top$院校) %>% 
  left_join(unique(select(dt_school_top, 院校, score_by_school_scale, school, rank)), 
  by = "院校")

dt_school_top_change %>%
  filter(rank <= 50) %>%
  ggplot(aes(x = score_by_major_change, 
             y = reorder(school, score_by_school_scale), 
             color = ifelse(score_by_major_change > 0, "#00BA38", "#F8756D"))) +
  geom_point(size = 5, alpha = .5) +
  scale_color_identity() +
  #scale_x_log10() +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      family = "Canger",
      size = 25
    ),
    axis.text.y = element_text(family = "Canger", size = 50),
    axis.title.x = element_text(size = 50),
    axis.title.y = element_text(size = 50),
    strip.text = element_text(size = 45),
    title = element_text(family = "Canger", size = 75)
  ) +
  labs(title = "Top50高校热度变化", x = "专业热度变化", y = "学校名称")
# save png
ggsave(
  "plot/top_uni_change_by_major.png",
  width = 10,
  height = 15,
  dpi = 300
)

dt_school_top <- dt_rank_cmb %>%
  mutate(school = substr(院校, 5, nchar(院校))) %>%
  filter(year == 2023) %>%
  mutate(rank = dense_rank(desc(score_by_school_scale))) %>%
  # filter(rank <= 30) %>%
  ungroup()
dt_school_top_change <- score_by_major_rough_change %>%
  filter(院校 %in% dt_school_top$院校) %>%
  left_join(unique(select(dt_school_top, 院校, score_by_school_scale, school, rank)),
    by = "院校"
  )
# zoom out
dt_school_top_change %>%
  filter(rank <= 50) %>%
  ggplot(aes(
    x = score_by_major_change,
    y = reorder(school, score_by_school_scale),
    color = ifelse(score_by_major_change > 0, "#00BA38", "#F8756D")
  )) +
  geom_point(size = 5, alpha = .5) +
  scale_color_identity() +
  coord_cartesian(xlim = c(-5, 5)) +
  # scale_x_log10() +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      family = "Canger",
      size = 25
    ),
    axis.text.y = element_text(family = "Canger", size = 50),
    axis.title.x = element_text(size = 50),
    axis.title.y = element_text(size = 50),
    strip.text = element_text(size = 45),
    title = element_text(family = "Canger", size = 75)
  ) +
  labs(title = "Top50高校热度变化", x = "专业热度变化", y = "学校名称")
# save png
ggsave(
  "plot/top_uni_change_by_major_zoom.png",
  width = 10,
  height = 15,
  dpi = 300
)





