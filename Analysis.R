#Analysis

#Slide 4
#Summary of the overall data
```{r, echo = TRUE}
fifa <- read_csv("~/Desktop/fifa.csv")
summary(fifa)
str(fifa)
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

```{r, echo = TRUE}
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
```{r}
loadPkg("corrplot")
corrplot(cor(df), method = "circle", type="upper")
```

**We find a relatively weak positive correlation between Age and Vision


**Variable Age seems to have a slight negative relationship with variable Value. Variable Overall has a positive relationship
with variable Value.**

#Slide 7
```{r, echo = True}
hist(fifa$Age, xlab = 'Age', main = 'Age', col = 'lightpink')
boxplot(fifa$Age, xlab = 'Age', main = 'Age', col = 'lightpink')
```
#Slide 8
```{r, echo = True}
hist(fifa$Vision, xlab = 'Vision', main = 'Vision', col = 'lightgreen')
boxplot(fifa$Vision, xlab = 'Vision', main = 'Vision', col = 'lightgreen')
```

#Slide 9 
```{r, echo = True}
ggplot(data=fifa)+ 
  geom_boxplot(mapping = aes(y=fifa$Age, x=fifa$`Player Mentality`, color=fifa$`Player Mentality`)) +
  labs(title="Age by Player Mentality",y="Age", x = "Player Mentality")

ggplot(data=fifa)+ 
  geom_boxplot(mapping = aes(y=fifa$Vision, x=fifa$`Player Mentality`, color=fifa$`Player Mentality`)) +
  labs(title="Vision by Player Mentality",y="Vision", x = "Player Mentality")
```

#Slide 14 & 15

```{r, echo = True}
age25less <- subset(fifa, fifa$Age < 25)
age25to35 <- subset(fifa, fifa$Age >24 & fifa$Age <35)
age35bigger <- subset(fifa, fifa$Age >=35)

contable = table(age25less$Age, age25less$Vision)
chisq.test(contable)

contable = table(age35bigger$Age, age35bigger$Vision)
chisq.test(contable)

contable = table(age25to35$Age, age25to35$Vision)
chisq.test(contable)

t.test(age25less$Vision, age25to35$Vision, alternative = "less")
t.test(age25less$Vision, age35bigger$Vision, alternative = "less")
t.test(age25to35$Vision, age35bigger$Vision, alternative = "less")

```


#Slide 16
```{r, echo = T}
ggplot(data=fifa)+ 
  geom_boxplot(mapping = aes(x=fifa$Age, y=fifa$Vision, color=fifa$`Player Mentality`)) +
  labs(title="Age vs Vision by Player Mentality",x="Age", y = "Vision")

attack <- subset(fifa, fifa$`Player Mentality` == 'attack')
keeper <- subset(fifa, fifa$`Player Mentality` == 'keeper')
mid <- subset(fifa, fifa$`Player Mentality` == 'mid')
defence <- subset(fifa, fifa$`Player Mentality` == 'defence')

cor(attack$Age, attack$Vision)
cor(keeper$Age, keeper$Vision)
cor(mid$Age, mid$Vision)
cor(defence$Age, defence$Vision)
```

#Slide 17
```{r, echo = True}
contable = table(attack$Age, attack$Vision)
chisq.test(contable)

contable = table(keeper$Age, keeper$Vision)
chisq.test(contable)

contable = table(mid$Age, mid$Vision)
chisq.test(contable)

contable = table(defence$Age, defence$Vision)
chisq.test(contable)
```

#Slide 18
```{r, echo = True}
age25lessattack <- subset(attack, attack$Age < 25)
age25lesskeeper <- subset(keeper, keeper$Age < 25)
age25lessmid <- subset(mid, mid$Age < 25)
age25lessdefence <- subset(defence, defence$Age < 25)

t.test(age25lessattack$Vision, age25lesskeeper$Vision, alternative = "less")
t.test(age25lesskeeper$Vision, age25lessmid$Vision, alternative = "less")
t.test(age25lessmid$Vision, age25lessdefence$Vision, alternative = "less")
t.test(age25lessattack$Vision, age25lessdefence$Vision, alternative = "less")
t.test(age25lesskeeper$Vision, age25lessdefence$Vision, alternative = "less")
t.test(age25lessattack$Vision, age25lessmid$Vision, alternative = "less")

```



