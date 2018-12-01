---
title: "2nd project"
author: "Group 5 DATS 6101"
date: "12/1/2018"
output: html_document
---

# DATS 6101 - 2nd project - Group 5

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
loadPkg = function(x) { if (!require(x,character.only=T, quietly =T)) { install.packages(x,dep=T,repos="http://cran.us.r-project.org"); if(!require(x,character.only=T)) stop("Package not found") } }
```


#Load the file

```{r, echo=FALSE}
library(readr)
#read in fifa csv file
fifa <- read.csv("~/Desktop/cleanedfifa.csv")  # REMEMBER TO CHANGE THE DIRECTORY
```
#Load all the packages

```{r}
loadPkg("leaps")
loadPkg("FNN")
loadPkg("gmodels")
loadPkg("ggplot2")
loadPkg("class")
loadPkg("pscl")
```


#Slide 4

##Defense


```{r, echo=FALSE}
def_data <- fifa[,c('Overall','Age', 'Potential', 'Acceleration', 'Agility', 'Balance', 'Ball.control', 'Composure', 'Crossing', 'Curve', 'Dribbling',  'Free.kick.accuracy', 'Heading.accuracy', 'Interceptions', 'Long.passing', 'Long.shots', 'Marking',  'Positioning', 'Reactions', 'Short.passing', 'Shot.power', 'Sliding.tackle', 'Sprint.speed', 'Stamina', 'Standing.tackle', 'Strength', 'Vision', 'Volleys', 'Player.Mentality')]
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
def_best <- def_data[,c('Age', 'Acceleration', 'Agility', 'Balance', 'Ball.control', 'Composure', 'Crossing', 'Curve', 'Dribbling',  'Free.kick.accuracy', 'Heading.accuracy', 'Interceptions', 'Long.passing', 'Long.shots', 'Marking',  'Positioning', 'Reactions', 'Short.passing', 'Shot.power', 'Sliding.tackle', 'Sprint.speed', 'Stamina', 'Standing.tackle', 'Strength', 'Vision', 'Volleys', 'target')]
```


```{r, echo = F}
scaled_fifa <- as.data.frame(scale(def_best[1:26], center = TRUE, scale = TRUE))
scaled_fifa$target <- def_best$target
set.seed(1000)
fifa_sample <- sample(2, nrow(scaled_fifa), replace=TRUE, prob=c(0.75, 0.25))
fifa_training <- scaled_fifa[fifa_sample==1, 1:ncol(scaled_fifa)-1]
fifa_test <- scaled_fifa[fifa_sample==2, 1:ncol(scaled_fifa)-1]
```

```{r, echo=FALSE}
fifa.trainLabels <- scaled_fifa[fifa_sample==1, 27]
fifa.testLabels <- scaled_fifa[fifa_sample==2, 27]
fifa.trainLabels
```

```{r, echo=FALSE}
fifa_pred <- knn(train = fifa_training, test = fifa_test, cl=fifa.trainLabels, k=6)
```

```{r, echo=FALSE}
for (k in 1:15) {
  pred <- knn(train = fifa_training, test = fifa_test, cl=fifa.trainLabels, k=k)
  Cross <- CrossTable(fifa.testLabels, pred, prop.chisq = FALSE)
  print(paste("k = ",k))
  print( paste("Total accuracy =  ",round( (Cross$prop.tbl[1,1] + Cross$prop.tbl[2,2]), 2)) )
}
```

##Mid-field

```{r, echo=FALSE}
midfield_data <- fifa[,c('Overall','Age', 'Potential', 'Acceleration', 'Aggression', 'Agility', 'Balance', 'Ball.control', 'Composure',
                         'Crossing', 'Curve', 'Dribbling', 'Finishing', 'Free.kick.accuracy', 'Heading.accuracy', 'Interceptions', 'Long.passing', 
                         'Long.shots', 'Penalties', 'Positioning', 'Reactions', 'Short.passing', 'Shot.power', 'Sliding.tackle', 'Sprint.speed'
                         , 'Stamina', 'Strength', 'Vision', 'Volleys', 'Player.Mentality')]
```

```{r, echo=FALSE}
midfield_data$midfield[fifa$Player.Mentality=='mid'] <- 'midfield'
midfield_data$midfield[fifa$Player.Mentality!='mid'] <- 'other'
```

```{r, echo = FALSE}
midfield_data$target[midfield_data$midfield=='midfield'] <- 1
midfield_data$target[midfield_data$midfield=='other'] <- 0
midfield_data$Player.Mentality <- NULL
```

```{r, echo=FALSE}
mid_best <- midfield_data[,c('Age', 'Acceleration', 'Aggression', 'Agility', 'Balance', 'Ball.control', 'Composure',
                             'Crossing', 'Curve', 'Dribbling', 'Finishing', 'Free.kick.accuracy', 'Heading.accuracy',
                             'Interceptions', 'Long.passing', 'Long.shots', 'Penalties', 'Positioning', 'Reactions', 
                             'Short.passing', 'Shot.power', 'Sliding.tackle', 'Sprint.speed', 'Stamina', 'Strength', 
                             'Vision', 'Volleys', 'target')]
```


```{r, echo=FALSE}
scaled_fifa <- as.data.frame(scale(mid_best[1:27], center = TRUE, scale = TRUE))
scaled_fifa$target <- mid_best$target
set.seed(1000)
fifa_sample <- sample(2, nrow(scaled_fifa), replace=TRUE, prob=c(0.75, 0.25))
fifa_training <- scaled_fifa[fifa_sample==1, 1:ncol(scaled_fifa)-1]
fifa_test <- scaled_fifa[fifa_sample==2, 1:ncol(scaled_fifa)-1]
```

```{r, echo=FALSE}
fifa.trainLabels <- scaled_fifa[fifa_sample==1, 28]
fifa.testLabels <- scaled_fifa[fifa_sample==2, 28]
```



```{r, echo=FALSE}
fifa_pred <- knn(train = fifa_training, test = fifa_test, cl=fifa.trainLabels, k=6)
```

```{r, echo=FALSE}
for (k in 1:15) {
  pred <- knn(train = fifa_training, test = fifa_test, cl=fifa.trainLabels, k=k)
  Cross <- CrossTable(fifa.testLabels, pred, prop.chisq = FALSE)
  print(paste("k = ",k))
  print( paste("Total accuracy =  ",round( (Cross$prop.tbl[1,1] + Cross$prop.tbl[2,2]), 2)) )
}
```




#Slide 5

```{r}
data_sub <- fifa[,c('Overall', 'Potential', 'Balance', 'Ball.control', 'Composure', 
                    'Crossing', 'Curve', 'Dribbling', 'Finishing', 'Free.kick.accuracy', 
                    'Marking', 'Penalties', 'Positioning', 'Reactions', 'Short.passing', 
                    'Shot.power','Sliding.tackle', 'Sprint.speed', 'Stamina', 'Standing.tackle', 
                    'Strength', 'Vision', 'Volleys', 'Player.Mentality', 'GK.diving', 
                    'GK.handling','GK.positioning','GK.kicking','GK.reflexes')]



```

```{r}
#For attacking players
subset_data <- function(df) {
  reg.best <- regsubsets(Overall~. -target - Player.Mentality, data = df[df$target == 1,], 
                         nvmax = 10)
  return(reg.best)
}
data_sub$target[data_sub$Player.Mentality=='attack'] <- 1
data_sub$target[data_sub$Player.Mentality!='attack'] <- 0
attack <- subset_data(data_sub)
plot(attack, scale = "adjr2", main = "Adjusted R^2")
attack_best <- data_sub[,c('Ball.control','Dribbling', 'Finishing','Positioning','Reactions',
                           'Shot.power','Short.passing','Sprint.speed','Strength','target')]
attack_best$target <- factor(attack_best$target)
lm_attack <- glm(target~ ., data = attack_best, family = "binomial")
pR2(lm_attack)
```

```{r}
#For goalkeepers
data_sub$target[data_sub$Player.Mentality=='keeper'] <- 1
data_sub$target[data_sub$Player.Mentality!='keeper'] <- 0
keeper <- subset_data(data_sub)
plot(keeper, scale = "adjr2", main = "Adjusted R^2")
keeper_best <- data_sub[,c('GK.handling', 'GK.positioning', 'GK.kicking', 'GK.reflexes',
                           'GK.diving','Reactions','Composure','Crossing','target')]
keeper_best$target <- factor(keeper_best$target)
lm_keeper <- glm(target~ ., data = keeper_best, family = "binomial")
pR2(lm_keeper)
```


#Slide 6



#Logistic Regression Model

#Analysis


```{r, echo=FALSE}
library(dplyr)
library(tidyr)
```

```{r, echo=FALSE}
df <- fifa[,c('Age', 'Potential', 'Acceleration', 'Agility', 'Balance', 'Ball.control', 'Composure', 'Crossing', 'Curve', 'Dribbling',  'Free.kick.accuracy', 'Heading.accuracy', 'Interceptions', 'Long.passing', 'Long.shots', 'Marking',  'Positioning', 'Reactions', 'Short.passing', 'Shot.power', 'Sliding.tackle', 'Sprint.speed', 'Stamina', 'Standing.tackle', 'Strength', 'Vision', 'Volleys', 'Player.Mentality')]
```

```{r, echo=FALSE}
set.seed(1000)
fifa_sample <- sample(2, nrow(df), replace=TRUE, prob=c(0.75, 0.25))
fifa_training <- df[fifa_sample==1, 1:ncol(df)]
fifa_test <- df[fifa_sample==2, 1:ncol(df)]
```

```{r, echo=FALSE}
fifa_training$y <- ifelse(fifa_training$Player.Mentality == "keeper",TRUE,FALSE)
fifa_training$y = factor(fifa_training$y)
```



```{r, echo=FALSE}
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
```

```{r}
glm_pred = glm_keeper_probs %>% mutate(pred_keeper = ifelse(probs>.5, "keeper", "other"))

fifa_test = cbind(fifa_test, glm_pred)
```

```{r}
fifa_training_defence <- subset(fifa_training, Player.Mentality != "keeper", select=Age:Player.Mentality)
```

```{r, echo=FALSE}
fifa_training_defence$y <- ifelse(fifa_training_defence$Player.Mentality == "defence",TRUE,FALSE)
fifa_training_defence$y = factor(fifa_training_defence$y)
```



```{r, echo=FALSE}
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
```

```{r}
glm_pred = glm_defence_probs %>% mutate(pred_defence = ifelse(probs>.5, "defence", "other"))

fifa_test = cbind(fifa_test, glm_pred)
```

```{r}
fifa_training_attack <- subset(fifa_training_defence, Player.Mentality != "defence", select=Age:Player.Mentality)
```

```{r, echo=FALSE}
fifa_training_attack$y <- ifelse(fifa_training_attack$Player.Mentality == "attack",TRUE,FALSE)
fifa_training_attack$y = factor(fifa_training_attack$y)
```


```{r, echo=FALSE}
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
```

```{r}
glm_pred = glm_attack_probs %>% mutate(pred_attack = ifelse(probs>.5, "attack", "mid"))

fifa_test = cbind(fifa_test, glm_pred)
```

```{r, echo=FALSE}
fifa_test <- fifa_test[-c(29, 31, 33)]
```

```{r, echo=FALSE}
data = fifa_test %>% select(Player.Mentality, pred_keeper, pred_defence, pred_attack)
```

```{r}
data$predicted <- data$pred_attack
data$pred_attack <- NULL
```

```{r}
data$predicted[data$pred_defence == "defence"] <- "defence"
data$pred_defence <- NULL
```

```{r}
data$predicted[data$pred_keeper == "keeper"] <- "keeper"
data$pred_keeper <- NULL
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

```{r}
selectedfifa <- fifa[-c(1,3,6,9,44)]
```

```{r}
scaled_selectedfifa <- as.data.frame(scale(selectedfifa[1:39], center = TRUE, scale = TRUE))
```
```{r}
#Sample 80% as train date and 20% as test
set.seed(10)
sample <- sample(2, nrow(scaled_selectedfifa), replace=TRUE, prob=c(0.8, 0.2))
scaled_training <- scaled_selectedfifa[sample==1,1:39]
scaled_test <- scaled_selectedfifa[sample==2,1:39]
```

# Create label

```{r}
trainLabels <- selectedfifa[sample==1, 40]
testLabels <- selectedfifa[sample==2, 40]
```

#Create a function 
```{r}
set.seed(10)
chooseK = function(k, train_set, test_set, predict1, actual){
  pred <- knn(train = train_set, test = test_set, cl=predict1, k=k)
  Cross <- CrossTable(actual, pred, prop.chisq = FALSE)
  
  tab = table(pred, actual)
  accu = sum(tab[row(tab) == col(tab)]) / sum(tab)                        
  cbind(k = k, accuracy = accu)
  
}
```

#Create a function to do different k
```{r}
knn_different_k = sapply(seq(1,24,by =2), function(x) chooseK(x,
                                                              train_set = scaled_training,
                                                              test_set = scaled_test,
                                                              predict1 = trainLabels,
                                                              actual = testLabels))
```

```{r}
str(knn_different_k)
knn_different_k = data.frame(k = knn_different_k[1,], accuracy = knn_different_k[2,])
```

#For k = 5

```{r}
final_pred <- knn(train = scaled_training, test = scaled_test, cl=trainLabels, k=5)
Cross_final <- CrossTable(testLabels, final_pred, prop.chisq = FALSE)

tab = table(final_pred, testLabels)
accu = sum(tab[row(tab) == col(tab)]) / sum(tab)  
print('k=5')
print(paste("total accuracy = ", accu))
```



#Create a visualization 
```{r}
ggplot(knn_different_k,
       aes(x = k, y = accuracy)) +
  geom_line(color = "orange", size = 1.5) +
  geom_point(size = 3)
```
