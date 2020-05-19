#Point Guard Selection

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

##Darren Collison chosen due to being a low price, and the highest ATTOVR

pg_team_stats <- team_stats2 %>%       #identifying teams with highest ATTOVR in NBA
  select(Team:G, AST, TOV) %>%
  mutate(ATTOVR = (AST/TOV)) %>%
  mutate_at(vars(ATTOVR), funs(round(., 3)))

best_ATTOVR <- ind_stats %>%           #table shows PG players in top 5 teams for ATTOVR and compared to ATTOVR of Darren Collison
  select(Player:G, AST, TOV, Salary) %>%
  filter(Pos == "PG", Tm %in% c("GSW", "BOS", "DEN", "SAS", "ORL")) %>%
  mutate(ATTOVR = (AST/TOV)) %>%
  mutate_at(vars(ATTOVR), funs(round(., 3))) %>%
  arrange(desc(ATTOVR))

best_ATTOVR %>%               #Shows spread of data according to ATTOVR and Salary to indicate value. ie. how high ATTOV for each $1000
  ggplot(mapping = aes(x = ATTOVR, y = (Salary/1000), colour = Player)) + 
  geom_point()

#Shooting Guard Selection




#Small Forward Selection




#Power Forward Selection




#Centre Selection



