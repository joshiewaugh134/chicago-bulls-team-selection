library(broom)

#Multi Linear Regression - Scoring -----

scoring <- ind_stats %>%
  select(Player:G, FG, FGp, X2P, X2Pp, X3P, X3Pp, PTS_per_game) %>%
  mutate(
    FG_per_game = (FG/G),
    X2P_per_game = (X2P/G),
    X3P_per_game = (X3P/G)) %>%
  mutate_at(vars(FG_per_game, X2P_per_game, X3P_per_game), funs(round(., 3)))

pairs(formula = ~ PTS_per_game + FG_per_game + X2P_per_game + X3P_per_game, 
      data = scoring) #Multicollinearity test showed X2P was showed strongest trend for PTS 

fit <- lm(PTS_per_game ~ X2P_per_game + X3P_per_game, data = scoring)
tidy(fit, conf.int = TRUE) #Multi-Linear Regression

car::avPlots(fit) #Linearity

car::vif(fit) #Variance inflation factor



#Linear Regression - Scoring -----

##Used X2P_per_game due to findings in MLR

scoringplot <- ggplot(scoring, aes(x = X2P_per_game, y = PTS_per_game)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", colour = "red") 

std_res <- rstandard(fit)      #Detecting Outliers
points <- 1:length(std_res)

ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3,3), colour = "red", linetype = "dashed")

res_labels <- if_else(abs(std_res) >= 2.5, paste(points), "")

scoringplot +
  geom_text(aes(label = res_labels), nudge_x = 0.002) #Outliers

hats <- hatvalues(fit)

ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point()

hat_labels <- if_else(hats >= 0.025, paste(points), "")

ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point() + 
  geom_text(aes(label = hat_labels), nudge_y = 0.0005) #Leverage Points

scoringplot +
  geom_text(aes(label = hat_labels), nudge_x = 0.002) #Leverage Points

cook <- cooks.distance(fit)

ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point() #Influence

cook_labels <- if_else(cook >= 0.15, paste(points), "")

ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_y = 0.001) #Influence

scoringplot +
  geom_text(aes(label = cook_labels), nudge_x = 0.002)

res <- residuals(fit)
fitted <- predict(fit)

ggplot(data = NULL, aes(x = fitted, y = res)) +
  geom_point(colour = "black") +
  geom_smooth(se = FALSE, colour = "red") #Homoscedasticity

