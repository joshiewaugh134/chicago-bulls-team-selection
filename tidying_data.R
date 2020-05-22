library(tidyverse)

#Load data into project ----------

ind_stats <- read.csv("data/raw/2018-19_nba_player_statistics.csv")
salaries <- read.csv("data/raw/2018-19_nba_player-salaries.csv")
team_stats1 <- read.csv("data/raw/2018-19_nba_team_statistics_1.csv")
team_stats2 <- read.csv("data/raw/2018-19_nba_team_statistics_2.csv")

#Renaming columns in data files --------

ind_stats <- rename(ind_stats,
                    FGp = 'FG.', X3Pp = 'X3P.', X2Pp = 'X2P.', eFGp = 'eFG.',
                    FTp = 'FT.', Player = 'ï..player_name')

salaries <- salaries %>%
  rename(player_id = 'ï..player_id', Player = 'player_name', Salary = 'salary') %>%
  select(Player:Salary)

salaries[,c("X","X.1","X.2", "X.3")] <- list(NULL)

team_stats1 <- team_stats1 %>%
  rename(Rk = 'ï..Rk', TSp = 'TS.', eFGp = 'eFG.', TOVp = 'TOV.', ORBp = 'ORB.', DRBp = 'DRB.')

team_stats1[,c("X","X.1","X.2")] <- list(NULL)

team_short <- data.frame("TmShort" = c("MIL", "GSW", "NOP", "PHI", "LAC", "POR", "OKC", "TOR", "SAC", "WAS", "HOU", "ATL",
                                       "MIN", "BOS", "BRK", "LAL", "UTA", "SAS", "CHO", "DEN", "DAL", "IND", "PHO", "ORL",
                                       "DET", "MIA", "CHI", "NYK", "CLE", "MEM"))


team_stats2 <- bind_cols(team_stats2, team_stats1[c(4:5)], team_short)  
  
team_stats2 <- team_stats2 %>%
  rename(FGp = 'FG.', X3Pp = 'X3P.', X2Pp = 'X2P.', FTp = 'FT.', Rk = 'ï..Rk') %>%
  mutate(ATTOVR = (AST/TOV)) %>%
  mutate_at(vars(ATTOVR), funs(round(., 3))) %>%
  select(Rk:Team, TmShort, G, W:L, FG:PTS, ATTOVR)

#Tidying ind_stats data by combining duplicates of players by team or position into same row and adding variables -----

ind_stats <- filter(ind_stats, Tm != "TOT") #Removing when Tm = TOT, as it means TOTAL if player is in >1 team

ind_stats <- data.frame(stringsAsFactors = FALSE, ind_stats)                

ind_stats <- ind_stats %>%                                                         
  group_by(Player, Age, Pos) %>% 
  summarise(Tm = paste(Tm, collapse = ", "), G = sum(G), GS = sum(GS), MP = sum(MP), FG = sum(FG), FGA = sum(FGA), 
            FGp = (FG/FGA), X3P = sum (X3P), X3PA = sum(X3PA), X3Pp = (X3P/X3PA), X2P = sum(X2P), X2PA = sum(X2PA),
            X2Pp = (X2P/X2PA), eFGp = ((FG + (0.5*X3P))/(FGA)), FT = sum(FT), FTA = sum(FTA), FTp = (FT/FTA),
            ORB = sum(ORB), DRB = sum(DRB), TRB = sum(TRB), AST = sum(AST), STL = sum(STL), BLK = sum(BLK),
            TOV = sum(TOV), PF = sum(PF), PTS = sum(PTS), FTF = (FTA/FGA), PTS_per_game = (PTS / G), FTF = (FTA/FGA),
            PTS_per_min = (PTS / MP))

ind_stats <- ind_stats %>%                                                        
  group_by(Player, Age) %>% 
  summarise(Pos = paste(Pos, collapse = ", "), Tm = paste(Tm, collapse = ", "), G = sum(G), GS = sum(GS), MP = sum(MP), 
            FG = sum(FG), FGA = sum(FGA), FGp = (FG/FGA), X3P = sum (X3P), X3PA = sum(X3PA), 
            X3Pp = (X3P/X3PA), X2P = sum(X2P), X2PA = sum(X2PA), X2Pp = (X2P/X2PA), 
            eFGp = ((FG + (0.5*X3P))/(FGA)), FT = sum(FT), FTA = sum(FTA), FTp = (FT/FTA),
            ORB = sum(ORB), DRB = sum(DRB), TRB = sum(TRB), AST = sum(AST), STL = sum(STL), 
            BLK = sum(BLK), TOV = sum(TOV), PF = sum(PF), PTS = sum(PTS), FTF = (FTA/FGA), 
            PTS_per_game = (PTS / G), FTF = (FTA/FGA), PTS_per_min = (PTS/MP))

ind_stats <- full_join(x = ind_stats, y = salaries) %>%                        
                   drop_na()

ind_stats <- ind_stats %>%
  data.frame(stringsAsFactors = FALSE, ind_stats) %>%
  select(Player:Salary) %>%
  mutate(
    ORB_z = (ORB - mean(ORB)) / sd(ORB),
    ORB_category = if_else(condition = ORB_z < 0,
                           true = "below average", false = "above average"),
    DRB_z = (DRB - mean(DRB)) / sd(DRB),
    DRB_category = if_else(condition = DRB_z < 0,
                           true = "below average", false = "above average"),
    TRB_z = (TRB - mean(DRB)) / sd(TRB),
    TRB_category = if_else(condition = TRB_z < 0,
                           true = "below average", false = "above average"),
    AST_z = (AST - mean(AST)) / sd(AST),
    AST_category = if_else(condition = AST_z <0, 
                           true = "below average", false = "above average"),
    BLK_z = (BLK - mean(BLK)) / sd(BLK),
    BLK_category = if_else(condition = BLK_z <0, 
                           true = "below average", false = "above average")) %>%
  arrange(str_extract(Player,'\\s.*$')) %>%
  mutate_at(vars(FGp, X3Pp, X2Pp, eFGp, FTp, FTF, PTS_per_game, PTS_per_min, ORB_z, DRB_z, TRB_z,
                 AST_z, BLK_z), funs(round(., 3)))
