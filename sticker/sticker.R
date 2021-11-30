library(hexSticker)
library(tidyverse)

Data <- data.frame(x = seq(0,8, by = 0.1)) %>%
        mutate(`No intervention` = dnorm(x, 2, 0.8), `With intervention` = dnorm(x, 5, 2)) %>%
        pivot_longer(-x, names_to = "intervention", values_to = "count") %>%
        mutate(zero = 0)

Plot <- ggplot(Data, aes(x, ymin = zero, ymax = count, fill = intervention)) +
        geom_ribbon(alpha = 0.8) +
        theme_void() +
        theme(legend.position = "none") 

Sticker <- sticker(Plot, package = "", p_fontface = "bold", h_color = "wheat1", h_fill = "snow", p_family = "sans", p_color = "black", p_size = 25, s_x = 1, s_y = 1.095, s_width = 1.9, s_height = 1.3, filename = "sticker/sticker.png")