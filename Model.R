#model

#Attack

#Read the file
data <- data.read('cleanedFifa.csv')
data_sub <- data[,c('Overall', 'Potential', 'Balance', 'Ball.control', 'Composure', 
                    'Crossing', 'Curve', 'Dribbling', 'Finishing', 'Free.kick.accuracy', 
                    'Marking', 'Penalties', 'Positioning', 'Reactions', 'Short.passing', 
                    'Shot.power','Sliding.tackle', 'Sprint.speed', 'Stamina', 'Standing.tackle', 
                    'Strength', 'Vision', 'Volleys', 'Player.Mentality')]
#Creating the target variable
data_sub$target[data$Player.Mentality=='attack'] <- 1
data_sub$target[data$Player.Mentality!='attack'] <- 0

#Engineering the data,i.e, feature scaling and test-train split
randomize <- sample(2, nrow(data_sub), replace=TRUE, prob=c(0.8, 0.2))
scaled_fifa <- as.data.frame(scale(data_sub[1:23], center = TRUE, scale = TRUE))
scaled_fifa$target <- data_sub$target
X_train <- scaled_fifa[randomize==1,1:ncol(scaled_fifa)-1]
X_test <- scaled_fifa[randomize==2,1:ncol(scaled_fifa)-1]
y_train <- scaled_fifa[randomize==1,ncol(scaled_fifa)]
y_test <- scaled_fifa[randomize==2,ncol(scaled_fifa)]

#Create KNN classification model
library("FNN")
pred <- knn(train = X_train[1:2], test = X_test[1:2], cl=y_train, k=6)

#Parameter tuning to figure out the best value of k
for (kv in 2:11) {
  print(paste("******* k = ",kv," ***************************" ))
  pred <- knn(train = X_train, test = X_test, cl=y_train, k=kv)
  Cross <- CrossTable(y_test, pred, prop.chisq = FALSE)
  print( paste("total accuracy =  ",round( (Cross$prop.tbl[1,1] + Cross$prop.tbl[2,2])*100, 2),"%"   ) )
}
#The best value of k is 6-7, this k value yields a good accuracy of 93.14%