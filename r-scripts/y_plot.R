y_plot.func <- function( df, y, ...) {
  df %>%
    ggplot() +
    geom_point(aes(y = !!sym(y), x = promotion.c, color = "#999999"), shape = 2) +
    geom_point(aes(y = !!sym(y), x = prevention.c,  color = "#E69F00"), shape = 16) +
    stat_smooth(aes(y = !!sym(y), x = promotion.c), color = "#999999", method = "lm", se = FALSE) +
    stat_smooth(aes(y = !!sym(y), x = prevention.c), color = "#E69F00", method = "lm", se = FALSE) +
    xlab(" ")+
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    scale_color_manual(name = "RFQ", values = c("#999999", "#E69F00"),
                       labels = c("promotion", "prevention")) +
    scale_shape_manual(name = "RFQ", values = c(2, 16),
                       labels = c("promotion", "prevention")) + 
    ...}
