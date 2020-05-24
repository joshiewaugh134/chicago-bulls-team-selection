#Point Guard (PG) -----

pg_data <- ind_stats %>%                             # filters Point Guards from ind_stats dataset
  select(Player:G, MP, AST, TOV, PTS, PTS_per_game, Salary) %>%
  filter(Pos  == "PG") %>%
  mutate(ATTOVR = (AST/TOV)) %>%                     # addition of new Assist-to-Turnover variable
  mutate_at(vars(ATTOVR), funs(round(., 3)))         # changes variables to 3 decimal places

pg_data <- pg_data[is.finite(pg_data$ATTOVR), ]      # removed cases with Inf variables from data

source(local = TRUE, "R/pg_attovr_functions.R")      # contains formula for ATTOVR functions

pg_data <- pg_data %>%
  mutate(ATTOVR_category = if_else(condition = ATTOVR < mean_ATTOVR,     # creates categories for analysis process
                                   true = "below average", false = "above average"),
         mean_AST_category = if_else(condition = AST < mean_AST,
                                     true = "below average", false = "above average"),
         mean_TOV_category = if_else(condition = TOV < mean_TOV,
                                     true = "below average", false = "above average")) %>%
  select(Player:G, MP, AST, TOV, PTS:PTS_per_game, Salary, ATTOVR)                         

#Shooting Guard (SG) -----

sg_data <- ind_stats %>%                             # filters Point Guards from ind_stats dataset
  select(Player:G, MP, FG:X2Pp, FT:FTp, AST, ORB:TRB, PTS, ORB_z, DRB_z, TRB_z, 
         AST_z, Salary) %>%
  filter(Pos %in% c("SG", "SF, SG", "PF, SG")) %>%
  mutate(PTS_per_game = (PTS/G),                     # adds variables used in further stages of analysis
         FGp_z = (FGp - mean(FGp)) / sd(FGp),        
         X3Pp_z = (X3Pp - mean(X3Pp)) / sd(X3Pp)) %>%
  mutate_at(vars(PTS_per_game, FGp_z, X3Pp_z), funs(round(., 3))) # changes variables to 3 decimal places

#Shot Forward (SF) -----

sf_data <- ind_stats %>%                             # filters Point Guards from ind_stats dataset
  select(Player:G, MP, FG:eFGp, AST, ORB:TRB, PTS, Salary) %>%
  filter(Pos %in% c("SF", "SF, SG", "PF, SF")) %>%
  mutate(PTS_per_game = (PTS/G),                     # adds variables used in further stages of analysis
         AST_z = (AST - mean(AST) / sd(AST)),
         FGp_z = (FGp - mean(FGp)) / sd(FGp)) %>%
  mutate_at(vars(PTS_per_game, AST_z, FGp_z), funs(round(., 3))) # changes variables to 3 decimal places
                  
#Power Forward (PF) -----

pf_data <- ind_stats %>%                             # filters Point Guards from ind_stats dataset
  select(Player:G, MP, FG:eFGp, ORB:TRB, PTS, Salary) %>%
  filter(Pos %in% c("PF", "PF, SF", "PF, SG", "C, PF")) %>%
  mutate(RB_per_game = TRB/G,                        # adds variables used in further stages of analysis
         RPG_z = (RB_per_game - mean(RB_per_game)) / sd(RB_per_game),
         X2Pp_z = (X2Pp - mean(X2Pp)) / sd(X2Pp),
         PTS_per_game = (PTS/G),
         FG_per_game = (FG/G),
         X2P_per_game = (X2P/G)) %>%
  mutate_at(vars(RB_per_game, PTS_per_game, X2Pp_z, RPG_z, FG_per_game, X2P_per_game), funs(round(., 3))) # changes variables to 3 decimal places

#Centre (C) -----

c_data <- ind_stats %>%                              # filters Point Guards from ind_stats dataset
  select(Player:G, MP:eFGp, ORB:TRB, BLK, PTS, Salary) %>%
  filter(Pos %in% c("C", "C, PF")) %>%
  mutate(PTS_per_game = (PTS/G),                     # adds variables used in further stages of analysis
         RBG = (TRB/G),
         BLK_z = (BLK - mean(BLK)) / sd(BLK),
         TRB_z = (TRB - mean(TRB)) / sd(TRB),
         X2Pp_z = (X2Pp - mean(X2Pp)) / sd(X2Pp)) %>%
  mutate_at(vars(PTS_per_game, RBG, BLK_z, TRB_z, X2Pp_z), funs(round(., 3))) # changes variables to 3 decimal places
