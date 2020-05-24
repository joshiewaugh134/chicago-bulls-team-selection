#Point Guard Selection -----

pg_data <- pg_data %>%                 # selects above average performers in AST, TOV and ATTOVR
  filter(ATTOVR >= mean_ATTOVR, AST >= mean_AST, TOV <= mean_TOV) %>%
  arrange(desc(ATTOVR))

monte_morris <- pg_data %>%            # Monte Morris chosen for starting PG
  select(Player:G, AST:ATTOVR) %>%
  filter(Player == "Monte Morris")

pg_team_stats <- team_stats2 %>%       # identifying teams with highest ATTOVR in NBA
  select(Team:G, AST, TOV) %>%
  mutate(ATTOVR = (AST/TOV)) %>%
  mutate_at(vars(ATTOVR), funs(round(., 3)))

best_ATTOVR <- ind_stats %>%           # filters PG players in top 5 teams for ATTOVR and compared to Monte Morris (already in dataset)
  select(Player:G, AST, TOV, PTS, PTS_per_game, Salary) %>%
  filter(Pos == "PG", Tm %in% c("GSW", "BOS", "DEN", "SAS", "ORL")) %>%
  mutate(ATTOVR = (AST/TOV)) %>%
  mutate_at(vars(ATTOVR), funs(round(., 3))) %>%
  arrange(desc(ATTOVR))

#Shooting Guard Selection -----

sg_data <- sg_data %>%                 # arranging variables into different order for easier viewing
  select(Player:FTp, FGp_z, X3Pp_z, PTS, PTS_per_game, AST, AST_z, ORB:TRB, ORB_z, DRB_z, TRB_z, 
          Salary) 

mean_PTS_per_game <- mean(sg_data$PTS_per_game)   # formula for average points scored in a game

sg_data <- sg_data %>%                 # filtering above average players by PTS_per_game and TRB z-scores above 0
  filter(PTS_per_game >= mean_PTS_per_game, TRB_z >=0)

sg_data <- sg_data %>%                 # re-arranging cases by descending PTS_per_game values
  arrange(desc(PTS_per_game)) %>%
  select(Player:G, FG:FTp, PTS, AST, TRB, PTS_per_game, Salary)

bradley_beal <- sg_data %>%            # Bradley Beal chosen as starting SG
  select(Player:Salary) %>%
  filter(Player == "Bradley Beal")

sg_team_stats <- team_stats2 %>%       # identifying top 5 teams for points scored per game
  select(Team:G, FG:FTp, AST, TRB, PTS) %>%
  mutate(PTS_per_game = (PTS/G)) %>%
  mutate_at(vars(PTS_per_game), funs(round(., 3))) %>%
  arrange(desc(PTS_per_game))

best_sg_PPG <- ind_stats %>%           # filters ind_stats to identify SG in the top 5 teams identified above
  select(Player:G, FG:X2Pp, FT:FTp, PTS, AST, TRB, Salary) %>%
  filter(Pos == "SG", Tm %in% c("MIL", "GSW", "NOP", "PHI", "LAC")) %>%
  mutate(PTS_per_game = (PTS/G)) %>%
  mutate_at(vars(PTS_per_game), funs(round(., 3)))

best_sg_PPG <- best_sg_PPG %>%         # compared Bradley Beal to SG players in teams found above
  rbind(bradley_beal) %>%
  arrange(desc(PTS_per_game))

#Small Forward Selection -----

source(local = TRUE, "R/linear_regression_scoring_process.R") # contains Linear Regression for Shooting
source(local = TRUE, "R/linear_regression_RB_to_X2P.R") # contains Linear Regression and formulas for RTBG Ratio

sf_data <- sf_data %>%                 # creates variables and filters data by these new variables
  mutate(RBX2P = (ORB/X2P),
         RBX2P_z = (RBX2P - mean(RBX2P)) / sd(RBX2P)) %>%
  mutate_at(vars(RBX2P, RBX2P_z) , funs(round(., 3))) %>%
  filter(RBX2P_z >= 0)  %>%
  arrange(desc(RBX2P))

sf_data <- sf_data %>%                 # creates variables and filters data by these new variables
  mutate(AST_per_game = (AST/G),
         ASTPG_z = (AST_per_game - mean(AST_per_game)) / sd(AST_per_game)) %>%
  mutate_at(vars(AST_per_game, ASTPG_z) , funs(round(., 3))) %>%
  filter(ASTPG_z >= 0) %>%
  arrange(desc(AST_per_game))

nicolas_batum <- sf_data %>%           # Nicolas Batum chosen as starting SG   
  select(Player:G, FG:FGp, X3P:X3Pp, X2P:X2Pp, AST, AST_per_game, ORB:TRB, RBX2P, Salary ) %>%
  filter(Player == "Nicolas Batum")

sf_team_stats <- team_stats2 %>%       # identifying top 5 teams for assists per game
  select(Team:G, FGp:X2Pp, AST, ORB:TRB) %>%
  mutate(AST_per_game = (AST/G),
         RBX2P = (ORB/X2P)) %>%
  mutate_at(vars(AST_per_game, RBX2P) , funs(round(., 3))) %>%
  arrange(desc(AST_per_game))

best_sf_ASTPG <- ind_stats %>%         # filters ind_stats to identify SF in the top 5 teams identified above
  select(Player:G, FG:FGp, X3P:X3Pp, X2P:X2Pp, AST, ORB:TRB, Salary) %>%
  filter(Pos == "SF", Tm %in% c("GSW", "DEN", "NOP", "PHI", "BOS")) %>%
  mutate(AST_per_game = (AST/G),
         RBX2P = (X2P/TRB)) %>%
  mutate_at(vars(AST_per_game, RBX2P), funs(round(., 3))) %>%
  arrange(desc(AST_per_game))

best_sf_ASTPG <- best_sf_ASTPG %>%     # compared Bradley Beal to SF players in teams found above
  rbind(nicolas_batum) %>%
  arrange(desc(AST_per_game))

#Power Forward Selection -----

pf_data <- pf_data %>%                 # filtering PF to eliminate z-scores in these variables that are below 0   
  filter(RPG_z >= 0, X2Pp_z >= 0)

pf_data <- pf_data %>%                 # creating new variables and filtering by these variables
  mutate(RBX2P = (ORB/X2P),
         RBX2P_z = (RBX2P - mean(RBX2P)) / sd(RBX2P)) %>%
  mutate_at(vars(RBX2P_z) , funs(round(., 3))) %>%
  filter(RBX2P_z >= 0)  %>%
  arrange(desc(RBX2P))

john_collins <- pf_data %>%            # John Collins chosen as starting PF    
  select(Player:G, X2P:X2Pp, ORB:TRB, Salary, RBX2P) %>%
  filter(Player == "John Collins")

pf_team_stats <- team_stats2 %>%       # identifying top 5 teams for rebounds per game
  select(Team:G, FGp, X2P:X2Pp, ORB:TRB) %>%
  mutate(RPG = (TRB/G),
         RBX2P = (ORB/X2P)) %>%
  mutate_at(vars(RPG, RBX2P), funs(round(., 3))) %>%
  arrange(desc(RPG))

best_pf_RBX2P <- ind_stats %>%         # filters ind_stats to identify PF in the top 5 teams identified above
  select(Player:G, X2P:X2Pp, ORB:TRB, Salary) %>%
  filter(Pos == "PF", Tm %in% c("MIL", "OKC", "POR", "PHI", "NOP")) %>%
  mutate(RBX2P = (ORB/X2P)) %>%
  mutate_at(vars(RBX2P), funs(round(., 3)))


best_pf_RBX2P <- best_pf_RBX2P %>%     # compares John Collins to players in teams found above
  rbind(john_collins) %>%
  arrange(desc(RBX2P))

#Centre Selection -----

c_data <- c_data %>%                   # filtering C to eliminate BLK z-scores that are below 0
  filter(BLK_z >= 0)

c_data <- c_data %>%                   # creating new variables and filtering by these variables
  mutate(RBX2P = (ORB/X2P),
         RBX2P_z = (RBX2P - mean(RBX2P)) / sd(RBX2P)) %>%
  mutate_at(vars(RBX2P_z) , funs(round(., 3))) %>%
  filter(RBX2P_z >= 0)  %>%
  arrange(desc(RBX2P))

steven_adams <- c_data %>%             # Steven Adams chosen as starting PF     
  select(Player:G, X2P, BLK, ORB:TRB, Salary, RBX2P) %>%
  filter(Player == "Steven Adams")

c_team_stats <- team_stats2 %>%        # creating new variables and filtering by these variables
  select(Team:G, FGp, X2P, ORB:TRB) %>%
  mutate(RPG = (ORB/X2P),
         RBX2P = (X2P/TRB)) %>%
  mutate_at(vars(RPG, RBX2P), funs(round(., 3))) %>%
  arrange(desc(RBX2P))

best_c_RBX2P <- ind_stats %>%          # filters ind_stats to identify C in the top 5 teams identified above
  select(Player:G, X2P, BLK, ORB:TRB, Salary) %>%
  filter(Pos == "C", Tm %in% c("PHO", "IND", "WAS", "SAS", "CHI")) %>%
  mutate(RBX2P = (ORB/X2P)) %>%
  mutate_at(vars(RBX2P), funs(round(., 3)))

best_c_RBX2P <- best_c_RBX2P %>%       # compares Steven Adams to players in teams found above
  rbind(steven_adams) %>%
  arrange(desc(RBX2P))

#Selected Team -----

selected_team <- ind_stats %>%         # filtering selected lineup from ind_stats, and mutating important variables
  filter(Player %in% c("Monte Morris", "Bradley Beal", "Nicolas Batum", "John Collins",
                       "Steven Adams")) %>%
  select(Player:G, FG:X2Pp, FT:FTp, ORB:AST, BLK:TOV, PTS, PTS_per_game:Salary) %>%
  mutate(ATTOVR = (AST/TOV),
         RBX2P = (X2P/TRB),
         RPG = (TRB/G),
         AST_per_game = (AST/G)) %>%
  mutate_at(vars(ATTOVR, RBX2P, RPG, AST_per_game), funs(round(., 3)))

selected_team <- selected_team %>%     # arranging variables into different order for easier viewing
  select(Player:G, FG:FTp, ORB:TRB, RBX2P, RPG, AST, AST_per_game, TOV, ATTOVR, BLK, PTS, 
         PTS_per_game:Salary)
