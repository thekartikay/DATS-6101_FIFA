#Analysis

#Summary of the data
fifa <- read_csv("~/Desktop/fifa.csv")
summary(fifa)
str(fifa)

**The data have 41 variables. The data describes the players' ages, nationalities, clubs, wages, and other characteristics and potentials of a soccer players. Each characteristic including dribbling, volley, or visions describe specific ability of each player and is scored by the computer.**

```{r, echo = TRUE}
summary(fifa$Age)
summary(fifa$Overall)
summary(fifa$Potential)
summary(fifa$Value..in.million.)
```

**Summary Statistics Table

```{r, echo = TRUE}
df <- fifa[,c("Age","Overall","Value","Strength","Stamina","Finishing","Penalties","Vision","Acceleration")]
s_table <- do.call(data.frame, 
           list(count = apply(df, 2, length),
                mean = apply(df, 2, mean),
                std = apply(df, 2, sd),
                min = apply(df, 2, min),
                Q25 = apply(df, 2, quantile, probs=0.25),
                median = apply(df, 2, median),
                Q75 = apply(df, 2, quantile, probs=0.75),
                max = apply(df, 2, max)))
s_table
```

```{r}
loadPkg("corrplot")
corrplot(cor(df), method = "circle", type="upper")
```

**We find a relatively weak positive correlation between Age and Vision


```{r, echo = TRUE}
new1 <- data.frame(fifa$Age, fifa$Overall, fifa$Value..in.million., fifa$Wage.in.thousands.)
pairs(new1)
```

**Variable Age seems to have a slight negative relationship with variable Value. Variable Overall has a positive relationship
with variable Value.**

```{r, echo = True}
hist(fifa$Age)
hist(fifa$Overall)
hist(fifa$Value..in.million.)
hist(fifa$Vision)
hist(fifa$Penalties)
hist(fifa$Acceleration)
hist(fifa$Strength)
hist(fifa$Stamina.enduration.)
hist(fifa$Finishing)
```
