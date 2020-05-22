ind_stats %>%
  ggplot() +
  geom_point(mapping = aes(x = AST, y = TOV, colour = TOV)) +
  scale_colour_gradient(low = "firebrick1", high = "black") +
  scale_x_continuous(limits = c(0, 800),
                     breaks = seq(0, 800, by = 100)) +
  scale_y_continuous(limits = c(0, 400),
                     breaks = seq(0, 400, by = 50)) +
  theme_classic()

