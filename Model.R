# DATS 6101 - 2nd project - Group 5

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
loadPkg = function(x) { if (!require(x,character.only=T, quietly =T)) { install.packages(x,dep=T,repos="http://cran.us.r-project.org"); if(!require(x,character.only=T)) stop("Package not found") } }
```


#Load the file

```{r, echo=FALSE}
library("readxl")
#read in fifa csv file
fifa <- read.csv("~/Desktop/fifa.csv") # REMEMBER TO CHANGE THE DIRECTORY
```
#Load all the packages

```{r}
loadPkg("leaps")
loadPkg("FNN")
loadPkg("gmodels")
loadPkg("ggplot2")
loadPkg("class")
```


#Slide 4

##Defense



##Mid-field


#Slide 5





#Slide 6
                         
#Logistic Regression Model
                         
#Analysis


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

