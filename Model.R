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


##Goal Keeper


```{r, echo = F}
#this is coder's comment
library(readr)
setwd("/Users/NN/Desktop/DATS 6101")
fifa <- read.csv("~/Desktop/cleanedfifa.csv")

#Here are my variables of interests
selectedfifa <-  fifa[,c("Age", "Strength","Stamina","Finishing","Penalties","Vision","Acceleration","Player.Mentality", "Heading.accuracy","Sprint.speed","Volleys","Balance","Composure")]

#I create a variable called y that is the binary variable 
selectedfifa$y <- ifelse(selectedfifa$Player.Mentality == "keeper",TRUE,FALSE)
selectedfifa$y = factor(selectedfifa$y)
selectedfifa <- selectedfifa[-c(8)]

```


```{r, echo = F,warning = F}
#this is coder's comment
loadPkg("leaps")
```

```{r, echo = T,warning = F}
#this is coder's comment
reg.leaps <- regsubsets(y~., data = selectedfifa, nbest = 1, method = "exhaustive") 
plot(reg.leaps, scale = "adjr2", main = "Adjusted R^2")
plot(reg.leaps, scale = "bic", main = "BIC")
plot(reg.leaps, scale = "Cp", main = "Cp")
```

#Based on the Adjusted R2, the model with the adjust R2 of 0.79 with: Age, Stamina, Heading.accuracy, and Balance

```{r, echo = F, warning = F}
#this is coder's comment
loadPkg("bestglm")
```


```{r, echo = T, warning = F}
#this is coder's comment
res.bestglm <- bestglm(Xy = selectedfifa, family = binomial,
                       IC = "BIC",                 # Information criteria for
                       method = "exhaustive")
summary(res.bestglm)
res.bestglm$BestModel
```


```{r, echo = F, warning = F}
#this is coder's comment
detach("package:bestglm", unload = T) 
```

#Here I createed another variable called goal keeper to turn y into numeric of 1 or 0

```{r, echo = T}
#this is coder's comment
selectedfifa$goalkeeper <- ifelse(selectedfifa$y == "FALSE", 0, 1)
selectedfifa$goalkeeper = factor(selectedfifa$goalkeeper)

#create a logit regression 
lmgoalkeeper <- glm(goalkeeper~ Age + Stamina+ Heading.accuracy + Balance, data = selectedfifa, family = "binomial")
summary(lmgoalkeeper)
```


#a) Hoslem and Lemeshow goodness-of-fit (GOF) test.
```{r, echo = T}
#this is coder's comment
loadPkg("ResourceSelection")
hoslem.test(selectedfifa$goalkeeper, fitted(lmgoalkeeper)) 
detach("package:ResourceSelection", unload = T) 
```
#pvalue is small => significant

```{r, echo = T}
#this is coder's comment
loadPkg("pROC")
prob=predict(lmgoalkeeper, type = c("response"))
selectedfifa$prob=prob
h <- roc(y~prob, data=selectedfifa)
auc(h)
plot(h)
detach("package:pROC", unload = T) 
```
#the area is 1, which is very good.

#c) The McFadden statistics.
```{r, echo = F}
#this is coder's comment
loadPkg("pscl") 
```

```{r, echo = T}
pR2(lmgoalkeeper)
```

#the McFadden value 0.996, which means the model explains about 99.6% of the probability of being a goal keeper

```{r, echo = F}
#this is coder's comment
detach("package:pscl", unload = T)
```



