# Stadistic_assigment_01

---
title: "Trabajo estadistica"
author: "Ajelandro Burtegue"
date: "01/06/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Análisis descriptivo


```{r}
StudentsPerformance<-read.csv(file='StudentsPerformance.csv', TRUE)
head(StudentsPerformance)
```

## Math score
```{r}
summary(StudentsPerformance$math.score)
sd(StudentsPerformance$math.score)
range(StudentsPerformance$math.score)
```

## Reading score
```{r}
summary(StudentsPerformance$reading.score)
sd(StudentsPerformance$reading.score)
range(StudentsPerformance$reading.score)
```

## Writing score
```{r}
summary(StudentsPerformance$writing.score)
sd(StudentsPerformance$writing.score)
range(StudentsPerformance$writing.score)
```

# Estimación de intervalos

## Resultados según puntuación de mates
```{r}
plot(StudentsPerformance$math.score,StudentsPerformance$reading.score)
mod1<-lm(StudentsPerformance$math.score ~ StudentsPerformance$reading.score)
abline(mod1, col=2, lwd = 3)
qqplot(StudentsPerformance$math.score,StudentsPerformance$reading.score)

plot(StudentsPerformance$math.score)
Y <- dnorm(StudentsPerformance$math.score)
plot(StudentsPerformance, Y)

plot(StudentsPerformance$math.score,StudentsPerformance$writing.score)
mod2<-lm(StudentsPerformance$math.score ~ StudentsPerformance$writing.score)
abline(mod2, col=2, lwd = 3)
qqplot(StudentsPerformance$math.score,StudentsPerformance$writing.score)
```

## ¿Sacaron las mujeres mejores notas que los hombres?

```{r}
table(StudentsPerformance$gender)
genero <- factor(StudentsPerformance$gender, levels = c("Masculino", "Femenino"))
data_gender <- data.frame(StudentsPerformance$gender)
plot(genero)
data_gender
#plot(StudentsPerformance$gender, StudentsPerformance$math.score)
#qqplot(StudentsPerformance$gender, StudentsPerformance$math.score)

```

## ¿Tuvo alguna importancia lan preparación de los padres?



## ¿Valió la pena el curso de preparación ante el examen?

## ¿Tuvo algo que ver lo que comieron antes de realizar cada examen?

_Tener en cuenta que sd es la desviación típica y afecta a la media_


### Posibles hipotesis a plantear:

- ¿Son los chicos son mejores?  
- ¿Tiene algo que ver la comida?  
- ¿El curso de preparación para el test valió la pena?
- ¿Tiene algo que ver el nivel de sus padres?
- ¿Algo que ver con la raza? (aqui no hay nombre de raza, solo: raza A vs raza B vs C vs D...)  
- ¿Los mejores en el reading son mejores en el writting?
- ¿Los mejores en mates lo hacen mejor en el reading writting?
