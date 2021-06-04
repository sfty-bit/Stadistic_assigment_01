---
title: "Trabajo estadistica"
author: "Ajelandro Burtegue"
date: "01/06/2021"
output:
  html_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```




En este trabajo nuestro grupo ha decidido realizar el análisis de una tabla de datos que recoge los resultados de mil estudiantes frente a exámenes de mátematicas, de lectura y escritura siendo un cero la mínima nota posible obtenida y 100 la máxima (lo pone en la página web). Además estos estudantes han sido clasificados por el nivel educativo de sus padres, el género del alumno, su etnia, su preparación frente al exmaen e incluso lo que comieron antes de realizarlo.


Para la fácil compresión dejaremos por aquí las primeras líneas de nuestros datos para que se vea claramente con que vamos a trabajar a lo largo de este pdf.

```{r}
StudentsPerformance<-read.csv(file='StudentsPerformance.csv', TRUE)
knitr::kable(head(StudentsPerformance))
```
En caso de quere consultarla con mayor detenimiento, la tabla de datos se puede encontrar en la página web de _"kaggle"_, y para acceder a ella [clique aquí](https://www.kaggle.com/spscientist/students-performance-in-exams).

# Análisis descriptivo

A continuación realizaremos un análisis descriptivo breve sobre los tres exámenes que se realizaron.


- Resultados en la prueba de escritura
```{r}
summary(StudentsPerformance$writing.score)
```
El primer exmane que veremos será el de escritura, siendo la mínima nota un 10 y la máxima un 100, su media y mediana muy parecidas, siendo estas 68 y 69 respectivamente.

- Resultados en la prueba de lectura
```{r}
summary(StudentsPerformance$reading.score)
```

En el examen de lectura salió mejor que el de escritura ya que tanto la media como la mediana son más grandes, ambas muy cercas del 70 y además la nota mínima subió 7 puntos.


- Resultados en la prueba de matemáticas
```{r}
summary(StudentsPerformance$math.score)
```

En de matemáticas, el rango  a diferencia de los dos primeros es de [0, 100], por lo que en este examen se logro obtener tanto la máxima como la mínima nota posible, luego vemos que la media y la mediana también son muy parecidas por hay más confianza al asegurar que no hay valores tan extremos que puedan alterar la media de resultados que como se puede apreciar es la menor de las tres medias que hemos visto, intuyendo que los estudantes se prepararon menos para este examen o que resultó más complicado que los otros desde el punto del tomador de la prueba.


# Estimación de intervalos y contraste de hipotesis

Ahora procederemos a escoger intervalos de confianza para cuatro posibilidades distintas, en todas buscaremos un intervalo de confianza del 95%, la razón de esto es que no conocemos sufiente del tema para proponer nosotros nuestras propias confianzas, por lo que usaremos la más común

Lo que haremos aquí será tomar como cierto la afirmación de que nuestra población total equivale a las 1000 personas que realizaron el test y cojeremos una muestra de esos 1000 para poder trabajar con ella.

### Estimación de intervalos en el examen de matemáticas

Antes que nada lo primero que haremos será ver con lo que estamos trabajando, por ello hemos decidido crear unos histogramas para interpretar los datos.
```{r}
mathex <- (StudentsPerformance$math.score)
math_mean <- mean(mathex)
Sample_math <- sample(StudentsPerformance$math.score, size=100)
sample_math_mean <- mean(Sample_math)
library(e1071)
hist(mathex)
hist(Sample_math)
#skewness(Sample_math)
#skewness(mathex)

```

Los histogramas como se puede apreciar, se alejan de la curva de distribución normal de Bell, por ello vamos a aplicar la teo´ria del teorema del límite central, concretamente la distribución de muestreo, que lo que hace es generar una distribución con la media de muchas muestras, esto será realizado con un simple bucle "for".

Y nos quedaría algo más manejable como esto:

```{r}
puntos <- c()
for (x in 1:200){
  sample_math_ <- sample(StudentsPerformance$math.score, size = 100)
  puntos <- c(puntos, mean(sample_math_))
  
}
plot(density(puntos))

```

Ahora vamos a establecer los intervalos, para ello nos ayudaremos de la fórmula de $error=z*\frac{\sigma}{\sqrt{n}}$ para que luego dicho error se lo sumaremos y restaremos a nuestra media de muestra para hallar el intervalo de confianza.
Z es el valor crítico, se puede calcular con la función qnorm en r y su valor es muy próximo a dos cuando la confianza es del 95%
sigma es la desviación estándar de la población total
n es el número total de población
notar que es bilateral por lo que 0.25-0.25

```{r}
#Intervalos de confiaza 95% de que pueda coger un resultado de Mates que de [65, 67]

math.mean <- mean(StudentsPerformance$math.score)
math.sd <- sd(StudentsPerformance$math.score)
error.n <- qnorm(0.975)*(math.sd/sqrt(1000))

x1 <- math.mean - error.n
y1 <- math.mean + error.n
Conf_math.interval_n <- c(x1,y1)
Conf_math.interval_n

```

También podríamos calcular el intervalo de confianza en caso de que desconocieramos la desviación estándar de la población total mediante el error t que se calcularia de esta manera $a$

```{r}
error.t <- qt(0.975, df = 999)*(math.sd/sqrt(1000))
x2 <- math.mean - error.t
y2 <- math.mean + error.t
Conf_math.interval_t <- c(x2,y2)
Conf_math.interval_t

```

Aunque no es necesario hacerlo manualmente ya que R viene provisto de la función "t.test" que te aporta los valores como veremos ahora en el contraste de hipotesis
El t.test que realizaremos será de una sola muestra.
```{r}
t.test(Sample_math, mu = math_mean, conf.level = 0.95, alternative = "two.sided")

```


```{r}

```

```{r}

```

### Resultados según puntuación de mates en reading y writing

```{r}
plot(StudentsPerformance$math.score,StudentsPerformance$reading.score)
mod1<-lm(StudentsPerformance$math.score ~ StudentsPerformance$reading.score)
abline(mod1, col=2, lwd = 3)
qqplot(StudentsPerformance$math.score,StudentsPerformance$reading.score)

plot(StudentsPerformance$math.score)

plot(StudentsPerformance$math.score,StudentsPerformance$writing.score)
mod2<-lm(StudentsPerformance$math.score ~ StudentsPerformance$writing.score)
abline(mod2, col=2, lwd = 3)
qqplot(StudentsPerformance$math.score,StudentsPerformance$writing.score)
```


```{r}
#Intervalos de confianza en segun la puntuación
```



### ¿Sacaron las mujeres mejores notas que los hombres?

```{r}

StudentsGender <- StudentsPerformance[,c(1,6,7,8)]
str(StudentsGender)

StudentsGenderD <- fastDummies::dummy_cols(StudentsGender, remove_first_dummy = TRUE)
str(StudentsGenderD)
head(StudentsGenderD)


plot(StudentsGenderD$gender_male, StudentsGenderD$math.score, xlab = "Mujer = 0, Hombre = 1", ylab = "Resultados matemáticas")
regresion_gender_math <- lm(StudentsGenderD$math.score ~ StudentsGenderD$gender_male)
abline(regresion_gender_math, lw = 2, col = 2)

qqplot(StudentsGenderD$gender_male, StudentsGenderD$math.score, xlab = "Mujer = 0, Hombre = 1", ylab = "Resultados matemáticas")

female_score <- StudentsGenderD[StudentsGenderD$gender_male==0,]
summary(female_score)
head(female_score)
number_females <- nrow(female_score)
number_females


male_score <- StudentsGenderD[StudentsGenderD$gender_male==1,]
summary(male_score)
head(male_score)
number_males <- nrow(male_score)
number_males


female_math.score <- female_score[,2]
head(female_math.score)
hist(female_math.score, freq=TRUE)
Female_math.mean <- mean(female_math.score)

male_math.score <- male_score[,2]
hist(male_math.score, freq=TRUE)
lines(density(male_math.score))
Male_math.mean <- mean(male_math.score)

Math.mean <- c(Male_math.mean, Female_math.mean)
male_fem <- c("Hombres", "Mujeres")
color.male_fem <- c("Blue", "Pink")
barplot(Math.mean, names.arg = male_fem, col = color.male_fem, xlab = "Media del examen de matemáticas")
```

```{r}
#Intervalos Mujer
fem_math.mean <- mean(female_math.score)
fem_math.sd <- sd(female_math.score)
fem_error.n <- qnorm(0.975)*(fem_math.sd/sqrt(482))
fem_error.t <- qt(0.975, df = 481)*(fem_math.sd/sqrt(482))


fx1 <- fem_math.mean - fem_error.n
fy1 <- fem_math.mean + fem_error.n
fem_Conf_math.interval_n <- c(fx1,fy1)
fem_Conf_math.interval_n

fx2 <- fem_math.mean - fem_error.t
fy2 <- fem_math.mean + fem_error.t
fem_Conf_math.interval_t <- c(fx2,fy2)
fem_Conf_math.interval_t
sample(female_math.score, 19, replace = TRUE)

#Intervalos Hombre
male_math.mean <- mean(male_math.score)
male_math.sd <- sd(male_math.score)
male_error.n <- qnorm(0.975)*(male_math.sd/sqrt(518))
male_error.t <- qt(0.975, df = 517)*(male_math.sd/sqrt(518))


mx1 <- male_math.mean - male_error.n
my1 <- male_math.mean + male_error.n
male_Conf_math.interval_n <- c(x1,y1)
male_Conf_math.interval_n

mx2 <- male_math.mean - male_error.t
my2 <- male_math.mean + male_error.t
male_Conf_math.interval_t <- c(x2,y2)
male_Conf_math.interval_t
sample(male_math.score, 19, replace = TRUE)
```

## ¿Tuvo alguna importancia lan preparación de los padres?

```{r}

StudentsParent_lvl_D <- StudentsPerformance[,c(3,6,7,8)]
StudentsParent_lvl <- fastDummies::dummy_cols(StudentsParent_lvl_D, remove_first_dummy = TRUE)
colnames(StudentsParent_lvl) <-  c("lvl","math","reading","writing","lvl_bachelors","lvl_hs","lvl_masters","lvl_some.hs","lvl_some.college")
#knitr::kable(StudentsParent_lvl)

boxplot(StudentsPerformance$math.score ~ StudentsPerformance$parental.level.of.education)


#Associated <- StudentsParent_lvl[StudentsParent_lvl$lvl_bachelors==1,Students,c(2,3,4,5)]
```
Bachelors <- StudentsParent_lvl[StudentsParent_lvl[,5]==1,Students,c(2,3,4,5)]
HighSchool <- StudentsParent_lvl[StudentsParent_lvl[,6]==1,Students,c(2,3,4,6)]
Masters <- StudentsParent_lvl[StudentsParent_lvl[,7]==1,Students,c(2,3,4,7)]
SomeCollege <- StudentsParent_lvl[StudentsParent_lvl[,8]==1,Students,c(2,3,4,8)]
SomeHighSchool <- StudentsParent_lvl[StudentsParent_lvl[,9]==1,Students,c(2,3,4,9)]

## ¿Valió la pena el curso de preparación ante el examen?

## ¿Tuvo algo que ver lo que comieron antes de realizar cada examen?

_Tener en cuenta que sd es la desviación típica y afecta a la media_

SE PUEDE HACER UNA REGRESIÓN LINEAL CUANDO LA VARIABLE X ES CATEGÓRICA, ES EL EJEMPLO DE DOS GRUPOS DE PUNTOS EN VERTICAL PARALELOS Y SE HACE LA REGRESIÓN

PARA LO DE ARRIBA HAY QUE PASAR LA CATEGÓRICA A DUMMY QUE ES EN (SI TENGO TRES CATEGORÍAS DOS DUMMIES, SI TENGO DOS TENGO QUE HACER UNA)

EL TEST-T ES UNA REGRESIÓN LINEAL PORQUE INDICA LA DIFERENCIA ENTRE LAS MEDIAS
LA REGRESIÓN LINEALM ES MÁS FLEXIBLE QUE EL TEST-T YA QUE PUEDE HAVER DIFERENTES REGRESIONES LINEALES QUE SE AJUSTEN A DIFERENTES VARIABLES
HACER UNA REGRESIÓN LINEAL


PROBABILIDAD =/= CONFIANZA

El R^2 es que porcentaje de la variabilidad en la variable respuesta que yo he conseguido averiguar en mi modelo
Y la R es la correlación entre las dos variables 

### Posibles hipotesis a plantear:

- ¿Son los chicos son mejores?  
- ¿Tiene algo que ver la comida?  
- ¿El curso de preparación para el test valió la pena?
- ¿Tiene algo que ver el nivel de sus padres?
- ¿Algo que ver con la raza? (aqui no hay nombre de raza, solo: raza A vs raza B vs C vs D...)  
- ¿Los mejores en el reading son mejores en el writting?
- ¿Los mejores en mates lo hacen mejor en el reading writting?


# QUIERO HACER LA DISTRIBUCIÓN NORMAL DE LOS HISTOGRAMAS Y PONERLOS JUNTX
