#PG Graphs ----

pg_data %>%               # Shows spread of data according to ATTOVR and Salary to indicate value.
  ggplot(mapping = aes(x = ATTOVR, y = (Salary/1000), colour = Player)) + 
  geom_point()

#SG Graphs ----

sg_data %>%
  ggplot(mapping = aes(x = PTS_P, y = FGp)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

sg_data %>%
  ggplot(mapping = aes(x = PTS_P, y = X3Pp)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

sg_data %>%               #Shows spread of data according to ATTOVR and Salary to indicate value.
  ggplot(mapping = aes(x = PTS_per_game, y = (Salary/1000), colour = Player)) + 
  geom_point()


#SF Graphs ----

sf_data %>%               #Shows spread of data according to AST and Salary to indicate value.
  ggplot(mapping = aes(x = AST, y = (Salary/1000), colour = Player)) + 
  geom_point()

#PF Graphs ----

pf_data %>%
  ggplot(mapping = aes(x = G, y = RB_per_game)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

pf_data %>%
  ggplot(mapping = aes(x = PTS_per_game, y = X2P_per_game)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

pf_data %>%               #Shows spread of data according to RBX2P and Salary to indicate value.
  ggplot(mapping = aes(x = RBX2P, y = (Salary/1000), colour = Player)) + 
  geom_point()


#C Graphs ----

c_data %>%
  ggplot(mapping = aes(x = PTS, y = X2P)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "red")

c_data %>%               #Shows spread of data according to RBX2P and Salary to indicate value. 
  ggplot(mapping = aes(x = RBX2P, y = (Salary/1000), colour = Player)) + 
  geom_point()