RB_to_X2P <- ind_stats %>%
  select(Player:G, FG, FGp, X2P, X2Pp, X3P, X3Pp, ORB:TRB, PTS) %>%
  mutate(RB_per_game = (TRB/G), 
         X2P_per_game = (X2P/G),
         PTS_per_game = (PTS/G))

RB_MLR_dat <- select(RB_to_X2P, PTS_per_game, X2P_per_game, RB_per_game)

pairs(RB_MLR_dat,
      labels = c("Points per game", "2-Point Goals per game", "Rebounds per game"),
      main = "How often are 2-Point Goals scored after rebounds in a game")

RB_fit <- lm(X2P_per_game ~ RB_per_game, data = RB_to_X2P)
broom::tidy(RB_fit, conf.int = TRUE)

RB_plot <- ggplot(RB_to_X2P, aes(x = X2P_per_game, y = RB_per_game)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", colour = "red") 

RB_std_res <- rstandard(RB_fit)      #Detecting Outliers
RB_points <- 1:length(RB_std_res)
RB_res_labels <- if_else(abs(RB_std_res) >= 2.5, paste(RB_points), "")

ggplot(data = NULL, aes(x = RB_points, y = RB_std_res)) +
  geom_point() +
  geom_text(aes(label = RB_res_labels), nudge_y = 0.3) +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3,3), colour = "red", linetype = "dashed")

RB_plot +
  geom_text(aes(label = RB_res_labels), nudge_x = 0.002) #Outliers

RB_hats <- hatvalues(RB_fit)              
RB_hat_labels <- if_else(RB_hats >= 0.02, paste(RB_points), "")

ggplot(data = NULL, aes(x = RB_points, y = RB_hats)) +
  geom_point() + 
  geom_text(aes(label = RB_hat_labels), nudge_y = 0.0005) #Leverage Points

RB_plot +
  geom_text(aes(label = RB_hat_labels), nudge_x = 0.002) #Leverage Points

RB_cook <- cooks.distance(RB_fit)           
RB_cook_labels <- if_else(RB_cook >= 0.025, paste(RB_points), "")

ggplot(data = NULL, aes(x = RB_points, y = RB_cook)) +
  geom_point() +
  geom_text(aes(label = RB_cook_labels), nudge_y = 0.001) #Influence

RB_plot +
  geom_text(aes(label = RB_cook_labels), nudge_x = 0.002)

RB_res <- residuals(RB_fit)
RB_fitted <- predict(RB_fit)

ggplot(data = NULL, aes(x = RB_fitted, y = RB_res)) +
  geom_point(colour = "black") +
  geom_smooth(se = FALSE, colour = "red") #Homoscedasticity
