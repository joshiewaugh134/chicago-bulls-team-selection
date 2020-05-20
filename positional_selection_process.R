#Point Guard Selection -----

pg_data %>%                                 #Observation of relationship of AST to TOV
  ggplot(mapping = aes(x = AST, y = TOV)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

pg_data <- pg_data %>%                      #Selects above average performers in AST, TOV and ATTOV
  filter(ATTOVR >= mean_ATTOVR, AST >= mean_AST, TOV >= mean_TOV) %>%
  arrange(desc(ATTOVR))

pg_data %>%               #Shows spread of data according to ATTOVR and Salary to indicate value. ie. how high ATTOV for each $1000
  ggplot(mapping = aes(x = ATTOVR, y = (Salary/1000), colour = Player)) + 
  geom_point()

##Darren Collison chosen due to being a low price, and the highest ATTOVR showing high value

pg_team_stats <- team_stats2 %>%       #Identifying teams with highest ATTOVR in NBA
  select(Team:G, AST, TOV) %>%
  mutate(ATTOVR = (AST/TOV)) %>%
  mutate_at(vars(ATTOVR), funs(round(., 3)))

best_ATTOVR <- ind_stats %>%           #Table shows PG players in top 5 teams for ATTOVR and compared to ATTOVR of Darren Collison
  select(Player:G, AST, TOV, Salary) %>%
  filter(Pos == "PG", Tm %in% c("GSW", "BOS", "DEN", "SAS", "ORL")) %>%
  mutate(ATTOVR = (AST/TOV)) %>%
  mutate_at(vars(ATTOVR), funs(round(., 3)))

darren_collison <- pg_data %>%
  select(Player:G, AST:ATTOVR) %>%
  filter(Player == "Darren Collison")

best_ATTOVR <- best_ATTOVR %>%      #Compared Darren COllison to players in teams found above
  rbind(darren_collison) %>%
  arrange(desc(ATTOVR))

#Shooting Guard Selection -----

source(local = TRUE, "funs/linear_regression_scoring.R") #contains Linear Regression for Shooting

#Small Forward Selection -----



#Power Forward Selection -----

##Multi Linear Regression - PF 

pairs(formula = ~ PTS_per_game + FG_per_game + X2P_per_game + RB_per_game, 
      data = pf_data) #Multicollinearity test

pf_fit <- lm(PTS_per_game ~ FG_per_game + X2P_per_game + RB_per_game, data = pf_data)
tidy(pf_fit, conf.int = TRUE) #Multi-Linear Regression vs PTS per game

car::avPlots(pf_fit) #Linearity

car::vif(pf_fit) #Variance inflation factor

##Linear Regression - PF

RB_to_X2P <- pf_data %>%
  select(Player:G, FG, FGp, X2P, X2Pp, X3P, X3Pp, RB_per_game, X2P_per_game)

RB_fit <- lm(X2P_per_game ~ RB_per_game, data = pf_data)
tidy(RB_fit, conf.int = TRUE)

RB_plot <- ggplot(pf_data, aes(x = X2P_per_game, y = RB_per_game)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", colour = "red") 

RB_std_res <- rstandard(RB_fit)      #Detecting Outliers
RB_points <- 1:length(RB_std_res)

ggplot(data = NULL, aes(x = RB_points, y = RB_std_res)) +
  geom_point() +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3,3), colour = "red", linetype = "dashed")

RB_res_labels <- if_else(abs(RB_std_res) >= 2, paste(RB_points), "")

RB_plot +
  geom_text(aes(label = RB_res_labels), nudge_x = 0.002) #Outliers

RB_hats <- hatvalues(RB_fit)              

ggplot(data = NULL, aes(x = RB_points, y = RB_hats)) +
  geom_point()

RB_hat_labels <- if_else(RB_hats >= 0.05, paste(RB_points), "")

ggplot(data = NULL, aes(x = RB_points, y = RB_hats)) +
  geom_point() + 
  geom_text(aes(label = RB_hat_labels), nudge_y = 0.0005) #Leverage Points

RB_plot +
  geom_text(aes(label = RB_hat_labels), nudge_x = 0.002) #Leverage Points

RB_cook <- cooks.distance(RB_fit)           

ggplot(data = NULL, aes(x = RB_points, y = RB_cook)) +
  geom_point() #Influence

RB_cook_labels <- if_else(RB_cook >= 0.20, paste(RB_points), "")

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

#Centre Selection -----



