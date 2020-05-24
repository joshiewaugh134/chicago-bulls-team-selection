# PTS vs Scoring Type ----

ggplot(data = ind_stats, aes(x = X2P, y = PTS)) + # graph of PTS vs 2-Point Goals
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

ggplot(data = ind_stats, aes(x = X3P, y = PTS)) + # graph of PTS vs 3-Point Goals
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

# Rebounds vs X2P ----

ggplot(data = ind_stats, aes(x = ORB, y = X2P)) + # graph of Rebounds vs 2-Point Goals
  geom_point() +
  geom_smooth(method = "lm", colour = "red")



