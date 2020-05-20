#Point Guard Selection -----

pg_data %>%                                 #Observation of relationship of AST to TOV
  ggplot(mapping = aes(x = AST, y = TOV)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

pg_data <- pg_data %>%                      #Selects above average performers in AST, TOV and ATTOV
  filter(ATTOVR >= mean_ATTOVR, AST >= mean_AST, TOV >= mean_TOV) %>%
  arrange(desc(ATTOVR))

pg_data %>%               #Shows spread of data according to ATTOVR and Salary to indicate value.
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

sg_data <- sg_data %>%
  select(Player:FTp, PTS, PTS_A:PTS_per_game, AST, ORB:TRB, ORB_z, DRB_z, TRB_z, 
         AST_z, Salary) 

sg_data %>%
  ggplot(mapping = aes(x = PTS_P, y = FGp)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

sg_data %>%
  ggplot(mapping = aes(x = PTS_P, y = X3Pp)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

sg_data <- sg_data %>%
  mutate(FGp_z = (FGp - mean(FGp)) / sd(FGp),
         X3Pp_z = (X3Pp - mean(X3Pp)) / sd(X3Pp)) %>%
  mutate_at(vars(FGp_z, X3Pp_z), funs(round(., 3))) %>%
  filter(FGp_z >= 0, X3Pp_z >= 0)

mean_PTS_per_game <- mean(sg_data$PTS_per_game)

sg_data <- sg_data %>%
  filter(PTS_per_game >= mean_PTS_per_game, TRB_z >=0)

sg_data <- arrange(sg_data, desc(PTS_per_game)) %>%
  select(Player:G, FG:FTp, PTS, PTS_A:PTS_P, 
         AST, TRB, PTS_per_1000_dollars:PTS_per_game, Salary)

sg_data %>%               #Shows spread of data according to ATTOVR and Salary to indicate value.
  ggplot(mapping = aes(x = PTS_per_game, y = (Salary/1000), colour = Player)) + 
  geom_point()

##Donovan Mitchell chosen due to being the most consistent player in set, and the best value pointscorer.

sg_team_stats <- team_stats2 %>%
  select(Team:G, FG:FTp, AST, TRB, PTS) %>%
  mutate(PTS_A = ((X3PA*3)+(X2PA*2)+(FTA*1)),
         PTS_P = (PTS/PTS_A),
         PTS_per_game = (PTS/G)) %>%
  mutate_at(vars(PTS_P, PTS_per_game), funs(round(., 3))) %>%
  arrange(desc(PTS_per_game))

best_sg_PTS_per_game <- ind_stats %>%
  select(Player:G, FG:X2Pp, FT:FTp, PTS, AST, TRB, Salary) %>%
  filter(Pos == "SG", Tm %in% c("MIL", "GSW", "NOP", "PHI", "LAC")) %>%
  mutate(PTS_per_1000_dollars = (PTS/(Salary/1000)),
         PTS_A = ((X3PA*3)+(X2PA*2)+(FTA*1)),
         PTS_P = (PTS/PTS_A),
         PTS_per_game = (PTS/G)) %>%
  mutate_at(vars(PTS_per_1000_dollars, PTS_P, PTS_per_game), funs(round(., 3)))

donovan_mitchell <- sg_data %>%
  select(Player:Salary) %>%
  filter(Player == "Donovan Mitchell")

best_sg_PTS_per_game <- best_sg_PTS_per_game %>%      #Compared Donovan Mitchell to players in teams found above
  rbind(donovan_mitchell) %>%
  arrange(desc(PTS_per_game))

#Small Forward Selection -----

source(local = TRUE, "funs/linear_regression_scoring.R") #contains Linear Regression for Shooting

sf_data <- sf_data %>%
  mutate(RBX2P = (X2P/TRB),
         RBX2P_z = (RBX2P - mean(RBX2P)) / sd(RBX2P)) %>%
  mutate_at(vars(RBX2P, RBX2P_z) , funs(round(., 3))) %>%
  filter(RBX2P_z >= 0)  %>%
  arrange(desc(RBX2P))

sf_data <- sf_data %>%
  mutate(AST_per_game = (AST/G),
         ASTPG_z = (AST_per_game - mean(AST_per_game)) / sd(AST_per_game)) %>%
  mutate_at(vars(AST_per_game, ASTPG_z) , funs(round(., 3))) %>%
  filter(ASTPG_z >= 0) %>%
  arrange(desc(AST_per_game))

sf_data %>%               #Shows spread of data according to AST and Salary to indicate value.
  ggplot(mapping = aes(x = AST, y = (Salary/1000), colour = Player)) + 
  geom_point()

##Kevin Durant was chosen despite being 2nd in AST. Similar stats to Lebron James (1st), for $5million less

sf_team_stats <- team_stats2 %>%
  select(Team:G, FGp:X2Pp, AST, TRB) %>%
  mutate(AST_per_game = (AST/G),
         RBX2P = (X2P/TRB)) %>%
  mutate_at(vars(AST_per_game, RBX2P) , funs(round(., 3))) %>%
  arrange(desc(AST_per_game))

kevin_durant <- sf_data %>%    
  select(Player:G, X3P:X3Pp, X2P:X2Pp, AST, TRB, Salary, RBX2P) %>%
  filter(Player == "Montrezl Harrell")

best_sf_ASTPG <- ind_stats %>%           #Table shows SF players in top 5 teams for AST/G and compared to Kevin Durant
  select(Player:G, X3P:X3Pp, X2P:X2Pp, AST, TRB, Salary) %>%
  filter(Pos == "SF", Tm %in% c("GSW", "DEN", "NOP", "PHI", "BOS")) %>%
  mutate(AST_per_game = (AST/G),
         RBX2P = (X2P/TRB)) %>%
  mutate_at(vars(AST_per_game, RBX2P), funs(round(., 3))) %>%
  arrange(desc(AST_per_game))

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

pf_data %>%               #Shows spread of data according to RBX2P and Salary to indicate value.
  ggplot(mapping = aes(x = RBX2P, y = (Salary/1000), colour = Player)) + 
  geom_point()

#Kyle Kuzma was chosen, and compared to Top 5 in that position

pf_team_stats <- team_stats2 %>%
  select(Team:G, FGp, X2P, TRB) %>%
  mutate(RPG = (TRB/G),
         RBX2P = (X2P/TRB)) %>%
  mutate_at(vars(RPG, RBX2P), funs(round(., 3))) %>%
  arrange(desc(RBX2P))

best_pf_RBX2P <- ind_stats %>%           #Table shows PF players in top 5 teams for RBX2P and compared to Kyle Kuzma
  select(Player:G, X2P, TRB, Salary) %>%
  filter(Pos == "PF", Tm %in% c("PHO", "IND", "WAS", "SAS", "CHI")) %>%
  mutate(RBX2P = (X2P/TRB)) %>%
  mutate_at(vars(RBX2P), funs(round(., 3)))

kyle_kuzma <- pf_data %>%    
  select(Player:G, X2P, TRB, Salary, RBX2P) %>%
  filter(Player == "Kyle Kuzma")

best_pf_RBX2P <- best_pf_RBX2P %>%      #Compared ____ to players in teams found above
  rbind(kyle_kuzma) %>%
  arrange(desc(RBX2P))

#Centre Selection -----

c_data <- c_data %>%
  filter(BLK_z >= 0)

c_data %>%
  ggplot(mapping = aes(x = PTS, y = X2P)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

c_data <- c_data %>%
  mutate(RBX2P = (X2P/TRB),
         RBX2P_z = (RBX2P - mean(RBX2P)) / sd(RBX2P)) %>%
  mutate_at(vars(RBX2P_z) , funs(round(., 3))) %>%
  filter(RBX2P_z >= 0)  %>%
  arrange(desc(RBX2P))

c_data %>%               #Shows spread of data according to RBX2P and Salary to indicate value. 
  ggplot(mapping = aes(x = RBX2P, y = (Salary/1000), colour = Player)) + 
  geom_point()

##Montrezl Harrell was chosen, and compared to Top 5 in that position

c_team_stats <- team_stats2 %>%
  select(Team:G, FGp, X2P, TRB) %>%
  mutate(RPG = (TRB/G),
         RBX2P = (X2P/TRB)) %>%
  mutate_at(vars(RPG, RBX2P), funs(round(., 3))) %>%
  arrange(desc(RBX2P))

best_c_RBX2P <- ind_stats %>%           #Table shows PF players in top 5 teams for RBX2P and compared to Montrezl Harrell
  select(Player:G, X2P, TRB, Salary) %>%
  filter(Pos == "C", Tm %in% c("PHO", "IND", "WAS", "SAS", "CHI")) %>%
  mutate(RBX2P = (X2P/TRB)) %>%
  mutate_at(vars(RBX2P), funs(round(., 3)))

montrezl_harrell <- c_data %>%    
  select(Player:G, X2P, TRB, Salary, RBX2P) %>%
  filter(Player == "Montrezl Harrell")

best_c_RBX2P <- best_c_RBX2P %>%      #Compared ____ to players in teams found above
  rbind(montrezl_harrell) %>%
  arrange(desc(RBX2P))

#Selected Team -----

selected_team <- ind_stats %>%
  filter(Player %in% c("Darren Collison", "Donovan Mitchell", "Kevin Durant", "Kyle Kuzma",
                       "Montrezl Harrell")) %>%
  select(Player:G, FG:X2Pp, FT:FTp, ORB:AST, BLK:TOV, PTS, PTS_per_game:Salary) %>%
  mutate(ATTOVR = (AST/TOV),
         PTS_A = ((X3PA*3)+(X2PA*2)+(FTA*1)),
         PTS_P = (PTS/PTS_A),
         RBX2P = (X2P/TRB),
         RPG = (TRB/G),
         AST_per_game = (AST/G),
         PTS_per_1000_dollars = (PTS/(Salary/1000))) %>%
  mutate_at(vars(ATTOVR, PTS_P, RBX2P, RPG, AST_per_game, PTS_per_1000_dollars), funs(round(., 3)))

selected_team <- selected_team %>%
  select(Player:G, FG:FTp, ORB:TRB, RBX2P, RPG, AST, AST_per_game, TOV, ATTOVR, BLK, PTS, PTS_A:PTS_P, 
         PTS_per_game:Salary, PTS_per_1000_dollars)
