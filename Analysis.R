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
fifa$Vision <- as.numeric(fifa$Vision)
fifa$Penalties <- as.numeric(fifa$Penalties)
fifa$Acceleration <- as.numeric(fifa$Acceleration)
fifa$Strength <- as.numeric(fifa$Strength)
fifa$Stamina.enduration. <- as.numeric(fifa$Stamina.enduration.)
fifa$Finishing <- as.numeric(fifa$Finishing)
```
