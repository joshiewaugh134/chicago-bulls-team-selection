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

source(local = TRUE, "funs/linear_regression_RB_to_X2P.R") #contains Linear Regression for RB/X2P

##Positional Analysis

pf_data %>%
  ggplot(mapping = aes(x = G, y = RB_per_game)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

pf_data <- pf_data %>%
  filter(RPG_z >= 0)

pf_data %>%
  ggplot(mapping = aes(x = PTS_per_game, y = X2P_per_game)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

pf_data <- pf_data %>%
  filter(X2Pp_z >= 0)

pf_data <- pf_data %>%
  mutate(RBX2P = (X2P/TRB),
         RBX2P_z = (RBX2P - mean(RBX2P)) / sd(RBX2P)) %>%
  mutate_at(vars(RBX2P_z) , funs(round(., 3))) %>%
  filter(RBX2P_z >= 0)  %>%
  arrange(desc(RBX2P))

pf_data %>%               #Shows spread of data according to ATTOVR and Salary to indicate value. ie. how high ATTOV for each $1000
  ggplot(mapping = aes(x = RBX2P, y = (Salary/1000), colour = Player)) + 
  geom_point()

#Kyle Kuzma was chosen, and compared to Top 5 in that position

pf_team_stats <- team_stats2 %>%
  select(Team:G, FGp, X2P, TRB) %>%
  mutate(RPG = (TRB/G),
         RBX2P = (X2P/TRB)) %>%
  mutate_at(vars(RPG, RBX2P), funs(round(., 3))) %>%
  arrange(desc(RBX2P))

best_RBX2P <- ind_stats %>%           #Table shows PF players in top 5 teams for RBX2P and compared to ATTOVR of Darren Collison
  select(Player:G, X2P, TRB, Salary) %>%
  filter(Pos == "PF", Tm %in% c("PHO", "IND", "WAS", "SAS", "CHI")) %>%
  mutate(RBX2P = (X2P/TRB)) %>%
  mutate_at(vars(RBX2P), funs(round(., 3)))

kyle_kuzma <- pf_data %>%    #Choose 1 with Sal Remaining
  select(Player:G, X2P, TRB, Salary, RBX2P) %>%
  filter(Player == "Kyle Kuzma")

best_RBX2P <- best_RBX2P %>%      #Compared ____ to players in teams found above
  rbind(kyle_kuzma) %>%
  arrange(desc(RBX2P))

#Centre Selection -----
