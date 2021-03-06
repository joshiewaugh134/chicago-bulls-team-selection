ggplot(data = team_stats2, aes(x = ATTOVR, y = (PTS/G))) + # graph of ATTOVR for teams
  geom_point() +
  geom_text(aes(label = TmShort), nudge_y = 0.5, size = 2.5) +
  scale_x_continuous(limits = c(1.4, 2.1),
                     breaks = seq(1.4, 2.1, by = 0.2)) +
  scale_y_continuous(limits = c(102, 119),
                     breaks = seq(102, 119, by = 2)) +
  theme_classic() +
  labs(title = "Relationship between ATTOVR and Points Scored in a game",
       subtitle = "A higher ATTOVR generally leads to more points",
       caption = "Data sourced from basketball-reference.com",
       x = "Assist to Turnover Ratio",
       y = "Points scored in a game")

pairs(scoring_dat,            # multicollinearity test showed X2P was showed strongest trend for PTS 
      labels = c("Points per game", "2-Point Goals per game", "3-Point Goals per game"),
      main = "Multilinear Regression of how points are scored in a game")

ind_stats %>%                 # 2-Point Goals per game vs Points per game
  ggplot() +
  geom_point(mapping = aes(x = (X2P/G), y = (PTS/G), colour = (X2P/G))) +
  scale_colour_gradient(low = "firebrick1", high = "black") +
  scale_x_continuous(limits = c(0, 10),
                     breaks = seq(0, 10, by = 2.5)) +
  scale_y_continuous(limits = c(0, 40),
                     breaks = seq(0, 40, by = 10)) +
  theme_classic() +
  labs(title = "Relationship between 2-Point Goals per Game and Points Scored per Game",
       subtitle = "2-Point Goals highest indicator of Points per game according to Multilinear Regression",
       caption = "Data sourced from basketball-reference.com",
       x = "2-Point Goals per Game",
       y = "Points per Game",
       colour = "2-Point Goals per Game")

ind_stats %>%                 # rebounds to 2-Point Goal Ratio
  ggplot() +
  geom_point(mapping = aes(x = (X2P/G), y = (ORB/G), colour = (X2P/G))) +
  scale_colour_gradient(low = "firebrick1", high = "black") +
  scale_x_continuous(limits = c(0, 10),
                     breaks = seq(0, 10, by = 2)) +
  scale_y_continuous(limits = c(0, 6),
                     breaks = seq(0, 6, by = 2)) +
  theme_classic() +
  labs(title = "Relationship between 2-Point Goals per Game and Rebounds per Game",
       subtitle = "Players tend to score more 2-Point goals when they have more Offensive rebounds",
       caption = "Data sourced from basketball-reference.com",
       x = "2-Point Goals per Game",
       y = "Offensive Rebounds per Game",
       colour = "2-Point Goals per Game")



