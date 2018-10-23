#Analysis

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
loadPkg = function(x) { if (!require(x,character.only=T, quietly =T)) { install.packages(x,dep=T,repos="http://cran.us.r-project.org"); if(!require(x,character.only=T)) stop("Package not found") } }
```

#Slide 4
#Summary of the overall data
```{r, echo = FALSE}
library("readxl")
fifa <- read.csv("~/Desktop/fifa.csv")
data <- fifa[,c("Age","Overall","Value","Strength","Stamina","Finishing","Penalties","Vision","Acceleration","Player.Mentality")]
```

```{r, echo = TRUE}
str(data)
```

**The data have 41 variables. The data describes the players' ages, nationalities, clubs, wages, and other characteristics and potentials of a soccer players. Each characteristic including dribbling, volley, or visions describe specific ability of each player and is scored by the computer.**

```{r, echo = TRUE}
summary(fifa$Age)
summary(fifa$Overall)
summary(fifa$Potential)
summary(fifa$Value..in.million.)
```

#Slide 5
**Summary Statistics Table

```{r, echo = FALSE}
df <- fifa[,c("Age","Overall","Value","Strength","Stamina","Finishing","Penalties","Vision","Acceleration")]
s_table <- do.call(data.frame, 
           lapply(lapply(list(count = apply(df, 2, length),
                mean = apply(df, 2, mean),
                std = apply(df, 2, sd),
                min = apply(df, 2, min),
                Q25 = apply(df, 2, quantile, probs=0.25),
                median = apply(df, 2, median),
                Q75 = apply(df, 2, quantile, probs=0.75),
                max = apply(df, 2, max)),round,2), format, scientific=FALSE))
s_table
```

#Slide 6

```{r, echo = FALSE}
library("corrplot")
```

```{r, echo = TRUE}
corrplot(cor(df), method = "circle", type="upper")
```

**We find a relatively weak positive correlation between Age and Vision

**Variable Age seems to have a slight negative relationship with variable Value. Variable Overall has a positive relationship
with variable Value.**

#Slide 7
```{r, echo = TRUE}
hist(df$Age, xlab = 'Age', main = 'Age', col = 'lightpink')
boxplot(df$Age, xlab = 'Age', main = 'Age', col = 'lightpink')
```
#Slide 8
```{r, echo = TRUE}
hist(df$Vision, xlab = 'Vision', main = 'Vision', col = 'lightgreen')
boxplot(df$Vision, xlab = 'Vision', main = 'Vision', col = 'lightgreen')
```

#Slide 9 
```{r, echo = FALSE}
library(ggplot2)
ggplot(data=df)+ 
  geom_boxplot(mapping = aes(y=df$Age, x=fifa$Player.Mentality, color=fifa$Player.Mentality)) +
  labs(title="Age by Player Mentality",y="Age", x = "Player Mentality") + theme(axis.text=element_text(size=12))
ggplot(data=df)+ 
  geom_boxplot(mapping = aes(y=df$Vision, x=fifa$Player.Mentality, color=fifa$Player.Mentality)) +
  labs(title="Vision by Player Mentality",y="Vision", x = "Player Mentality") + theme(axis.text=element_text(size=12))
```

#Slide 13

```{r, echo=FALSE}
ggplot(df, aes(x=Age, y=Vision, 
               color=ifelse(Age < 25,"< 25", 
                            ifelse(Age >= 25 & Age < 35, "25 <= Age <35", ">= 35")), 
               shape=ifelse(Age < 25,"< 25", 
                            ifelse(Age >= 25 & Age < 35, "25 <= Age <35", ">=35")))) +  
  geom_point() + 
  ggtitle("Age vs Vision") + 
  scale_colour_discrete(name ="Age", labels=c("< 25",">= 35", "25 <= Age <35")) +  
  scale_shape_discrete(name ="Age", labels=c("< 25",">= 35", "25 <= Age <35"))
```

```{r, echo=TRUE}
cor(df$Age, df$Vision)
```

```{r, echo=FALSE}
paste("Original Dataset Correlation: ", round(cor(df$Age, df$Vision),2))
paste("Attack Correlation: ", round(cor(attack$Age, attack$Vision),2))
paste("Goalkeeper Correlation: ", round(cor(keeper$Age, keeper$Vision),2))
paste("Midfield Correlation: ", round(cor(mid$Age, mid$Vision),2))
paste("Defence Correlation: ", round(cor(defence$Age, defence$Vision),2))
```

#Slide 14

```{r, echo = FALSE}
loadPkg("MASS")
age25less <- subset(df, Age < 25)
age25to35 <- subset(df, Age >= 25 & Age < 35)
age35bigger <- subset(df, Age >= 35)
```

**We subset Age into 3 ranges.

```{r, echo = TRUE}
contable = table(age25less$Age, age25less$Vision)
chisq.test(contable)

contable = table(age25to35$Age, age25to35$Vision)
chisq.test(contable)

contable = table(age35bigger$Age, age35bigger$Vision)
chisq.test(contable)
```

**We perform chi-square tests to test the independence of the variables Age and Vision for the 3 Age subsets.

```{r, echo = FALSE}
fit1 <- lm(age25less$Vision ~ age25less$Age, data = df)
summary(fit1)
plot(age25less$Age, age25less$Vision, xlab = "Age (<25)", ylab = "Vision")
abline(fit1)
```

```{r, echo = FALSE}
fit2 <- lm(age25to35$Vision ~ age25to35$Age, data = df)
summary(fit2)
plot(age25to35$Age, age25to35$Vision, xlab = "Age (25 <= Age < 35)", ylab = "Vision")
abline(fit2)
```

```{r, echo = FALSE}
fit3 <- lm(age35bigger$Vision ~ age35bigger$Age, data = df)
summary(fit3)
plot(age35bigger$Age, age35bigger$Vision, xlab = "Age (>= 35)", ylab = "Vision")
abline(fit3)
```

#Slide 15

```{r, echo = TRUE}
t.test(age25less$Vision, age25to35$Vision, alternative = "less")
t.test(age25less$Vision, age35bigger$Vision, alternative = "less")
t.test(age25to35$Vision, age35bigger$Vision, alternative = "less")
```

**We perform t-tests comparing the difference in means between Vision across the different Age subsets.

#Slide 16
```{r, echo = FALSE}
attack <- subset(fifa, Player.Mentality == 'attack')
keeper <- subset(fifa, Player.Mentality == 'keeper')
mid <- subset(fifa, Player.Mentality == 'mid')
defence <- subset(fifa, Player.Mentality == 'defence')
```

```{r, echo = TRUE}
cor(attack$Age, attack$Vision)
cor(keeper$Age, keeper$Vision)
cor(mid$Age, mid$Vision)
cor(defence$Age, defence$Vision)
```

```{r, echo = FALSE}
library(mosaic)
ggplot(data=df)+ 
  geom_point(mapping = aes(x = Age, y=Vision, color=Age)) + 
  facet_wrap(~fifa$Player.Mentality) +
  labs(x = 'Age', y = 'Vision', colour = 'Age' ) + theme_dark() + theme(axis.text=element_text(size=12))
```

#Slide 17
```{r, echo = TRUE}
contable = table(attack$Age, attack$Vision)
chisq.test(contable)

contable = table(keeper$Age, keeper$Vision)
chisq.test(contable)

contable = table(mid$Age, mid$Vision)
chisq.test(contable)

contable = table(defence$Age, defence$Vision)
chisq.test(contable)
```

**Perform chi-square tests to test independence of the variables Age and Vision for each player position.

#Slide 19
```{r, echo = FALSE}
age25lessattack <- subset(attack, Age < 25)
age25lesskeeper <- subset(keeper, Age < 25)
age25lessmid <- subset(mid, Age < 25)
age25lessdefence <- subset(defence, Age < 25)
```

```{r, echo = TRUE}
t.test(age25lessattack$Vision, age25lesskeeper$Vision)
t.test(age25lesskeeper$Vision, age25lessmid$Vision)
t.test(age25lessmid$Vision, age25lessdefence$Vision)
t.test(age25lessattack$Vision, age25lessdefence$Vision)
t.test(age25lesskeeper$Vision, age25lessdefence$Vision)
t.test(age25lessattack$Vision, age25lessmid$Vision)
```

**We perform t-tests to compare mean Vision for player position of players with age less than 25.

#Slide 20

```{r, echo = FALSE}
att_data <- subset(attack, select= -c(Name, Nationality, Club,Player.Mentality,ID,GK.diving,GK.handling,GK.kicking,Potential,Special,GK.positioning,GK.reflexes))
corrplot(cor(att_data), method = "circle", type="upper")
```

```{r, echo = FALSE}
mid_data <- subset(mid, select= -c(Name, Nationality, Club,Player.Mentality, ID,GK.diving,GK.handling,GK.kicking,Potential,Special,GK.positioning,GK.reflexes))
corrplot(cor(mid_data), method = "circle", type="upper")
```

```{r, echo = FALSE}
def_data <- subset(defence, select= -c(Name, Nationality, Club,Player.Mentality, ID,GK.diving,GK.handling,GK.kicking,Potential,Special,GK.positioning,GK.reflexes))
corrplot(cor(def_data), method = "circle", type="upper")
```


