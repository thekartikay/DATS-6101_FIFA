data <- read.csv('cleanedFifa.csv')
data_sub <- data[,c('Overall', 'Potential', 'Balance', 'Ball.control', 'Composure', 
                    'Crossing', 'Curve', 'Dribbling', 'Finishing', 'Free.kick.accuracy', 
                    'Marking', 'Penalties', 'Positioning', 'Reactions', 'Short.passing', 
                    'Shot.power','Sliding.tackle', 'Sprint.speed', 'Stamina', 'Standing.tackle', 
                    'Strength', 'Vision', 'Volleys', 'Player.Mentality', 'GK.diving', 
                    'GK.handling','GK.positioning','GK.kicking','GK.reflexes')]

subset_data <- function(df, val) {
  df$target[df$Player.Mentality==val] <- 1
  df$target[df$Player.Mentality!=val] <- 0
  reg.best <- regsubsets(Overall~. - target - Player.Mentality, data = df[df$target == 1,], 
                         nvmax = 10)
  return(reg.best)
}

#For attacking players
attack <- subset_data(data_sub, 'attack')
plot(attack, scale = "adjr2", main = "Adjusted R^2")
attack_best <- data_sub[,c('Ball.control','Dribbling', 'Finishing','Positioning','Reactions',
                           'Shot.power','Short.passing','Sprint.speed','Strength','target')]
attack_best$target <- factor(attack_best$target)
lm_attack <- glm(target~ ., data = attack_best, family = "binomial")


#For goalkeepers
keeper <- subset_data(data_sub, 'keeper')
plot(attack, scale = "adjr2", main = "Adjusted R^2")
keeper_best <- data_sub[,c('GK.handling', 'GK.positioning', 'GK.kicking', 'GK.reflexes',
                           'GK.diving','Reactions','Composure','Crossing','target')]
keeper_best$target <- factor(keeper_best$target)
lm_attack <- glm(target~ ., data = keeper_best, family = "binomial")