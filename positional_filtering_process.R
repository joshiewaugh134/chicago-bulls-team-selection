#Point Guard (PG) -----

pg_data <- ind_stats %>%
  select(Player:G, MP, AST, TOV, Salary) %>%
  filter(Pos  == "PG") %>%
  mutate(ATTOVR = (AST/TOV)) %>%
  mutate_at(vars(ATTOVR), funs(round(., 3)))

pg_data <- pg_data[is.finite(pg_data$ATTOVR), ]

source(local = TRUE, "funs/pg_attovr_functions.R") #contains formula for ATTOVR functions

pg_data <- pg_data %>%
  mutate(ATTOVR_category = if_else(condition = ATTOVR < mean_ATTOVR,
                                   true = "below average", false = "above average"),
         mean_AST_category = if_else(condition = AST < mean_AST,
                                     true = "below average", false = "above average"),
         mean_TOV_category = if_else(condition = TOV < mean_TOV,
                                     true = "below average", false = "above average")) %>%
  select(Player:G, MP, AST, TOV, Salary, ATTOVR)                         

#Shooting Guard (SG) -----

sg_data <- ind_stats %>%
  select(Player:G, MP, FG:X3Pp, eFGp, AST, ORB:TRB, PTS, ORB_z, DRB_z, TRB_z, 
         AST_z, Salary) %>%
  filter(Pos %in% c("SG", "SF, SG", "PF, SG")) %>%
  mutate(PTS_per_1000_dollars = (PTS/(Salary/1000)),
         PTS_per_game = (PTS/G),
         FGp_z = (FGp - mean(FGp)) / sd(FGp),
         X3Pp_z = (X3Pp - mean(X3Pp)) / sd(X3Pp)) %>%
  mutate_at(vars(PTS_per_1000_dollars, PTS_per_game, FGp_z, X3Pp_z), funs(round(., 3)))

#Shot Forward (SF) -----

sf_data <- ind_stats %>%
  select(Player:G, MP, FG:eFGp, AST, ORB:TRB, PTS, Salary) %>%
  filter(Pos %in% c("SF", "SF, SG", "PF, SF")) %>%
  mutate(PTS_per_game = (PTS/G),
         AST_z = (AST - mean(AST) / sd(AST)),
         FGp_z = (FGp - mean(FGp)) / sd(FGp)) %>%
  mutate_at(vars(PTS_per_game, AST_z, FGp_z), funs(round(., 3)))
                  
#Power Forward (PF) -----

pf_data <- ind_stats %>%
  select(Player:G, MP, FG:eFGp, ORB:TRB, PTS, Salary) %>%
  filter(Pos %in% c("PF", "PF, SF", "PF, SG", "C, PF")) %>%
  mutate(RB_per_game = TRB/G, 
         RPG_z = (RB_per_game - mean(RB_per_game)) / sd(RB_per_game),
         FGp_z = (FGp - mean(FGp)) / sd(FGp),
         PTS_per_game = (PTS/G),
         FG_per_game = (FG/G),
         X2P_per_game = (X2P/G)) %>%
  mutate_at(vars(RB_per_game, PTS_per_game, FGp_z, RPG_z, FG_per_game, X2P_per_game), funs(round(., 3)))

#Centre (C) -----

c_data <- ind_stats %>%
  select(Player:G, MP:eFGp, ORB:TRB, BLK, PTS, Salary) %>%
  filter(Pos %in% c("C", "C, PF")) %>%
  mutate(PTS_per_game = (PTS/G),
         RBG = (TRB/G),
         PTS_per_1000_Dollars = (PTS/(Salary/1000)),
         BLK_z = (BLK - mean(BLK)) / sd(BLK),
         TRB_z = (TRB - mean(TRB)) / sd(TRB),
         X2Pp_z = (X2Pp - mean(X2Pp)) / sd(X2Pp)) %>%
  mutate_at(vars(PTS_per_game, RBG, PTS_per_1000_Dollars, BLK_z, TRB_z, X2Pp_z), funs(round(., 3)))
