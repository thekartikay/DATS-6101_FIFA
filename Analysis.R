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

```{r, echo = TRUE}
new1 <- data.frame(fifa$Age, fifa$Overall, fifa$Value..in.million., fifa$Wage.in.thousands.)
pairs(new1)
```

**Variable Age seems to have a slight negative relationship with variable Value. Variable Overall has a positive relationship
with variable Value.**
