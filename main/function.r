my_theme_legend <- list(
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
        title = element_text(family = "Canger", size = 75),
        legend.position = c(.8, .07),
        legend.title = element_text(family = "Canger", size = 45),
        legend.text = element_text(family = "Canger", size = 45),
        legend.spacing.y = unit(0.2, "cm")
    )
)
my_theme <- list(
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
