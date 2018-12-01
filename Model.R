# DATS 6101 - 2nd project - Group 5

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
loadPkg = function(x) { if (!require(x,character.only=T, quietly =T)) { install.packages(x,dep=T,repos="http://cran.us.r-project.org"); if(!require(x,character.only=T)) stop("Package not found") } }
```


#Slide 4

##Defense

```{r, echo=FALSE}
library("readxl")
#read in fifa csv file
fifa <- read.csv("~/Desktop/fifa.csv")
```

```{r, echo=FALSE}
def_data <- fifa[,c('Age', 'Potential', 'Acceleration', 'Agility', 'Balance', 'Ball.control', 'Composure', 'Crossing', 'Curve', 'Dribbling',  'Free.kick.accuracy', 'Heading.accuracy', 'Interceptions', 'Long.passing', 'Long.shots', 'Marking',  'Positioning', 'Reactions', 'Short.passing', 'Shot.power', 'Sliding.tackle', 'Sprint.speed', 'Stamina', 'Standing.tackle', 'Strength', 'Vision', 'Volleys', 'Player.Mentality')]
```

```{r, echo=FALSE}
def_data$defence[fifa$Player.Mentality=='defence'] <- 'defence'
def_data$defence[fifa$Player.Mentality!='defence'] <- 'other'
```

```{r, echo = FALSE}
def_data$target[def_data$defence=='defence'] <- 1
def_data$target[def_data$defence=='other'] <- 0
def_data$Player.Mentality <- NULL
```

```{r, echo=FALSE}
loadPkg("leaps")
reg.best <- regsubsets(target~. - defence, data = def_data, nbest=2, nvmax = 10, method="exhaustive")
plot(reg.best, scale = "adjr2", main = "Adjusted R^2")
plot(reg.best, scale = "bic", main = "BIC")
plot(reg.best, scale = "Cp", main = "Cp")
summary(reg.best)
```

```{r, echo=FALSE}
def_best <- def_data[,c('Crossing', 'Curve', 'Heading.accuracy', 'Long.passing', 'Long.shots', 'Marking', 'Short.passing', 'Sliding.tackle','Sprint.speed','Vision', 'defence', 'target')]
```


```{r, echo=FALSE}
loadPkg("FNN")
scaled_fifa <- as.data.frame(scale(def_best[1:10], center = TRUE, scale = TRUE))
scaled_fifa$target <- def_best$target
set.seed(1000)
fifa_sample <- sample(2, nrow(scaled_fifa), replace=TRUE, prob=c(0.75, 0.25))
fifa_training <- scaled_fifa[fifa_sample==1, 1:ncol(scaled_fifa)-1]
fifa_test <- scaled_fifa[fifa_sample==2, 1:ncol(scaled_fifa)-1]
```

```{r, echo=FALSE}
fifa.trainLabels <- scaled_fifa[fifa_sample==1, 11]
fifa.testLabels <- scaled_fifa[fifa_sample==2, 11]
fifa.trainLabels
```

```{r, echo=FALSE}
fifa_pred <- knn(train = fifa_training, test = fifa_test, cl=fifa.trainLabels, k=6)

loadPkg("gmodels")
IRISPREDCross <- CrossTable(fifa.testLabels, fifa_pred, prop.chisq = FALSE)
```

```{r, echo=FALSE}
for (k in 1:15) {
  pred <- knn(train = fifa_training, test = fifa_test, cl=fifa.trainLabels, k=k)
  Cross <- CrossTable(fifa.testLabels, pred, prop.chisq = FALSE)
  print(paste("k = ",k))
  print( paste("Total accuracy =  ",round( (Cross$prop.tbl[1,1] + Cross$prop.tbl[2,2]), 2)) )
}
```
#k=6 or 7 have the highest accuracy, at 95%


##Mid-field


#Read the file
data <- data.read('cleanedFifa.csv')
data_sub <- data[,c('Overall', 'Potential', 'Balance', 'Ball.control', 'Composure', 
                    'Crossing', 'Curve', 'Dribbling', 'Finishing', 'Free.kick.accuracy', 
                    'Marking', 'Penalties', 'Positioning', 'Reactions', 'Short.passing', 
                    'Shot.power','Sliding.tackle', 'Sprint.speed', 'Stamina', 'Standing.tackle', 
                    'Strength', 'Vision', 'Volleys', 'Player.Mentality')]
#Creating the target variable
data_sub$target[data$Player.Mentality=='mid'] <- 1
data_sub$target[data$Player.Mentality!='mid'] <- 0

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


#Slide 5







#Slide 6
                         
#Logistic Regression Model
                         
#Analysis

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
loadPkg = function(x) { if (!require(x,character.only=T, quietly =T)) { install.packages(x,dep=T,repos="http://cran.us.r-project.org"); if(!require(x,character.only=T)) stop("Package not found") } }
```


```{r, echo=FALSE}
library("readxl")
library(dplyr)
library(tidyr)
fifa <- read.csv("~/Desktop/fifa.csv")
```

```{r, echo=FALSE}
df <- fifa[,c('Age', 'Potential', 'Acceleration', 'Agility', 'Balance', 'Ball.control', 'Composure', 'Crossing', 'Curve', 'Dribbling',  'Free.kick.accuracy', 'Heading.accuracy', 'Interceptions', 'Long.passing', 'Long.shots', 'Marking',  'Positioning', 'Reactions', 'Short.passing', 'Shot.power', 'Sliding.tackle', 'Sprint.speed', 'Stamina', 'Standing.tackle', 'Strength', 'Vision', 'Volleys', 'Player.Mentality')]
```

```{r, echo=FALSE}
set.seed(1000)
fifa_sample <- sample(2, nrow(df), replace=TRUE, prob=c(0.75, 0.25))
fifa_training <- df[fifa_sample==1, 1:ncol(df)]
fifa_test <- df[fifa_sample==2, 1:ncol(df)]
head(fifa_training)
```

```{r, echo=FALSE}
fifa_training$y <- ifelse(fifa_training$Player.Mentality == "keeper",TRUE,FALSE)
fifa_training$y = factor(fifa_training$y)
```

```{r, echo=FALSE}
head(fifa_training)
```


```{r, echo=FALSE}
loadPkg("leaps")
reg.best <- regsubsets(y~. - Player.Mentality, data = fifa_training, nbest=1, nvmax = 10, method="exhaustive")
plot(reg.best, scale = "adjr2", main = "Adjusted R^2")
plot(reg.best, scale = "bic", main = "BIC")
plot(reg.best, scale = "Cp", main = "Cp")
summary(reg.best)
```

```{r, echo=FALSE}
fifa_training$keeper <- ifelse(fifa_training$y == "FALSE", 0, 1)
fifa_training$keeper = factor(fifa_training$keeper)

glm_keeper <- glm(keeper~ Age + Potential+ Acceleration+ Balance+ Composure+ Heading.accuracy+ Reactions, data = fifa_training, family = "binomial")
summary(glm_keeper)
```

```{r, echo=FALSE}
glm_keeper_probs = data.frame(probs = predict(glm_keeper, newdata = fifa_test, type="response"))
head(glm_keeper_probs)
```

```{r}
glm_pred = glm_keeper_probs %>% mutate(pred_keeper = ifelse(probs>.5, "keeper", "other"))

fifa_test = cbind(fifa_test, glm_pred)
head(fifa_test)
```

```{r}
fifa_training_defence <- subset(fifa_training, Player.Mentality != "keeper", select=Age:Player.Mentality)
head(fifa_training_defence)
```

```{r, echo=FALSE}
fifa_training_defence$y <- ifelse(fifa_training_defence$Player.Mentality == "defence",TRUE,FALSE)
fifa_training_defence$y = factor(fifa_training_defence$y)
```

```{r, echo=FALSE}
head(fifa_training_defence)
```

```{r, echo=FALSE}
loadPkg("leaps")
reg.best <- regsubsets(y~. - Player.Mentality, data = fifa_training_defence, nbest=1, nvmax = 10, method="exhaustive")
plot(reg.best, scale = "adjr2", main = "Adjusted R^2")
plot(reg.best, scale = "bic", main = "BIC")
plot(reg.best, scale = "Cp", main = "Cp")
summary(reg.best)
```

```{r, echo=FALSE}
fifa_training_defence$defence <- ifelse(fifa_training_defence$y == "FALSE", 0, 1)
fifa_training_defence$defence = factor(fifa_training_defence$defence)

glm_defence <- glm(defence~ Age + Potential+ Crossing+ Heading.accuracy+ Long.passing+ Long.shots+ Marking+ Short.passing+ Sliding.tackle+ Vision, data = fifa_training_defence, family = "binomial")
summary(glm_defence)
```

```{r, echo=FALSE}
glm_defence_probs = data.frame(probs = predict(glm_defence, newdata = fifa_test, type="response"))
head(glm_defence_probs)
```

```{r}
glm_pred = glm_defence_probs %>% mutate(pred_defence = ifelse(probs>.5, "defence", "other"))

fifa_test = cbind(fifa_test, glm_pred)
head(fifa_test)
```

```{r}
fifa_training_attack <- subset(fifa_training_defence, Player.Mentality != "defence", select=Age:Player.Mentality)
head(fifa_training_attack)
```

```{r, echo=FALSE}
fifa_training_attack$y <- ifelse(fifa_training_attack$Player.Mentality == "attack",TRUE,FALSE)
fifa_training_attack$y = factor(fifa_training_attack$y)
```

```{r, echo=FALSE}
head(fifa_training_attack)
```

```{r, echo=FALSE}
loadPkg("leaps")
reg.best <- regsubsets(y~. - Player.Mentality, data = fifa_training_attack, nbest=1, nvmax = 10, method="exhaustive")
plot(reg.best, scale = "adjr2", main = "Adjusted R^2")
plot(reg.best, scale = "bic", main = "BIC")
plot(reg.best, scale = "Cp", main = "Cp")
summary(reg.best)
```

```{r, echo=FALSE}
fifa_training_attack$attack <- ifelse(fifa_training_attack$y == "FALSE", 0, 1)
fifa_training_attack$attack = factor(fifa_training_attack$attack)

glm_attack <- glm(attack~ Age + Potential+ Crossing+ Heading.accuracy+ Interceptions+ Long.passing+ Positioning+ Reactions+ Short.passing+ Sliding.tackle+ Volleys, data = fifa_training_attack, family = "binomial")
summary(glm_attack)
```

```{r, echo=FALSE}
glm_attack_probs = data.frame(probs = predict(glm_attack, newdata = fifa_test, type="response"))
head(glm_attack_probs)
```

```{r}
glm_pred = glm_attack_probs %>% mutate(pred_attack = ifelse(probs>.5, "attack", "mid"))

fifa_test = cbind(fifa_test, glm_pred)
head(fifa_test)
```

```{r, echo=FALSE}
fifa_test <- fifa_test[-c(29, 31, 33)]
head(fifa_test)
```

```{r, echo=FALSE}
data = fifa_test %>% select(Player.Mentality, pred_keeper, pred_defence, pred_attack)
```

```{r}
data$predicted <- data$pred_attack
data$pred_attack <- NULL
head(data)
```

```{r}
data$predicted[data$pred_defence == "defence"] <- "defence"
data$pred_defence <- NULL
head(data)
```

```{r}
data$predicted[data$pred_keeper == "keeper"] <- "keeper"
data$pred_keeper <- NULL
head(data)
```

#Printing out the accuracy                          
```{r}
accuracy = round((sum(data$Player.Mentality == data$predicted)) / nrow(data) * 100, 2)
paste("Logistic Regression Accuracy:", accuracy, '%')
```

#Slide 7

#Create the table to check the accuracy for all position                         
```{r, echo=FALSE}
library(gmodels)
CrossTable(data$Player.Mentality, data$predicted)
```



#Slide 8

#Testing to predict classes
library(readr)
fifa <- read.csv("~/Desktop/cleanedfifa.csv")

selectedfifa <- fifa[-c(1,3,6,9,44)]

loadPkg("FNN")
loadPkg("gmodels")

scaled_selectedfifa <- as.data.frame(scale(selectedfifa[1:39], center = TRUE, scale = TRUE))

#Sample 80% as train date and 20% as test
set.seed(10)
sample <- sample(2, nrow(scaled_selectedfifa), replace=TRUE, prob=c(0.8, 0.2))

scaled_training <- scaled_selectedfifa[sample==1,1:39]
scaled_test <- scaled_selectedfifa[sample==2,1:39]

trainLabels <- selectedfifa[sample==1, 40]
testLabels <- selectedfifa[sample==2, 40]

loadPkg("class")
set.seed(10)
#Try k =5


chooseK = function(k, train_set, test_set, predict1, actual){
  pred <- knn(train = train_set, test = test_set, cl=predict1, k=k)
  Cross <- CrossTable(actual, pred, prop.chisq = FALSE)
  
  tab = table(pred, actual)
  accu = sum(tab[row(tab) == col(tab)]) / sum(tab)                        
  cbind(k = k, accuracy = accu)
  
}

knn_different_k = sapply(seq(1,24,by =2), function(x) chooseK(x,
                                                              train_set = scaled_training,
                                                              test_set = scaled_test,
                                                              predict1 = trainLabels,
                                                              actual = testLabels))


str(knn_different_k)

knn_different_k = data.frame(k = knn_different_k[1,], accuracy = knn_different_k[2,])


loadPkg("ggplot2")

ggplot(knn_different_k,
       aes(x = k, y = accuracy)) +
  geom_line(color = "orange", size = 1.5) +
  geom_point(size = 3)

