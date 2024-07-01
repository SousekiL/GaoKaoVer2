my_theme_legend <- list(
    theme(
        text = element_text(
            family = "Canger",
            size = 50
        ),
        axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            family = "Canger",
            size = 50
        ),
        axis.text.y = element_text(family = "Canger", size = 50),
        axis.title.x = element_text(size = 50),
        axis.title.y = element_text(size = 50),
        strip.text = element_text(size = 50),
        title = element_text(family = "Canger", size = 75),
        legend.position = c(.8, .07),
        legend.title = element_text(family = "Canger", size = 50),
        legend.text = element_text(family = "Canger", size = 50),
        legend.spacing.y = unit(0.2, "cm")
    )
)

my_theme_legend_define <- list(
    theme(
        text = element_text(
            family = "Canger",
            size = 50
        ),
        axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            family = "Canger",
            size = 50
        ),
        axis.text.y = element_text(family = "Canger", size = 50),
        axis.title.x = element_text(size = 50),
        axis.title.y = element_text(size = 50),
        strip.text = element_text(size = 50),
        title = element_text(family = "Canger", size = 75),
        legend.title = element_text(family = "Canger", size = 50),
        legend.text = element_text(family = "Canger", size = 50),
        legend.spacing.y = unit(0.2, "cm")
    )
)

my_theme <- list(
    theme(text = element_text(
            family = "Canger",
            size = 50
        ),
        axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            family = "Canger",
            size = 50
        ),
        axis.text.y = element_text(family = "Canger", size = 50),
        axis.title.x = element_text(size = 50),
        axis.title.y = element_text(size = 50),
        strip.text = element_text(size = 50),
        title = element_text(family = "Canger", size = 75),
        legend.position = "none"
    )
)
ggsaveTheme <- function(p, mytheme, filename, width, height, dpi) {
    p <- p + mytheme
    ggsave(
        filename,
        width = width,
        height = height,
        dpi = dpi
    )
}





df <- data.frame(
    score = c(1, 2, 3, 4, 5),
    frequency = c(10, 20, 30, 25, 15)
)
df$cum_frequency <- cumsum(df$frequency)

total_frequency <- sum(df$frequency)
levels <- cut(df$cum_frequency, breaks = c(0, total_frequency * 0.2, total_frequency * 0.4, total_frequency * 0.6, total_frequency * 0.8, total_frequency), labels = c("Very Low", "Low", "Medium", "High", "Very High"))

df$level <- levels
