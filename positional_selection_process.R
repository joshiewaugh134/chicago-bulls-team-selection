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

source(local = TRUE, "funs/linear_regression_scoring.R") #contains Linear Regression for Shooting



#Centre Selection -----



