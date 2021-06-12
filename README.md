---
title: "Trabajo estadistica"
author: "Antonio Pedreño y Ajelandro Burtegue"
date: "01/06/2021"
output:
  pdf_document: default
  word_document: default
  html_document: default
---
---
___
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```




En este trabajo nuestro grupo ha decidido realizar el análisis de una tabla de datos que recoge los resultados de mil estudiantes frente a exámenes de mátematicas, de lectura y escritura siendo un cero la mínima nota posible obtenida y 100 la máxima (lo pone en la página web). Además estos estudantes han sido clasificados por el nivel educativo de sus padres, el género del alumno, su etnia, su preparación frente al examen e incluso lo que comieron antes de realizarlo.


Para la fácil compresión dejaremos por aquí las primeras líneas de nuestros datos para que se vea claramente con que vamos a trabajar a lo largo de este pdf.

```{r}
StudentsPerformance<-read.csv(file='StudentsPerformance.csv', TRUE)
Students_head <- head(StudentsPerformance)
colnames(Students_head) <- c("Género", "Raza", "Estudios parentales", "Comida", "Preparación", "Mates", "Lectura", "Escritura")
knitr::kable(Students_head)
```
En caso de querer consultarla con mayor detenimiento, la tabla de datos se puede encontrar en la página web de _"kaggle"_, y para acceder a ella [clique aquí](https://www.kaggle.com/spscientist/students-performance-in-exams).

# 1. Análisis descriptivo

A continuación realizaremos un análisis descriptivo breve sobre los tres exámenes que se realizaron.


- Resultados en la prueba de escritura
```{r}
Summary_writing <- c(summary(StudentsPerformance$writing.score))
Summary_writing
```
El primer examen que veremos será el de escritura, siendo la mínima nota un 10 y la máxima un 100, su media y mediana muy parecidas, siendo estas 68 y 69 respectivamente.

- Resultados en la prueba de lectura
```{r}
Summary_reading <- c(summary(StudentsPerformance$reading.score))
Summary_reading
```

En el examen de lectura salió mejor que el de escritura ya que tanto la media como la mediana son más grandes, ambas muy cercas del 70 y además la nota mínima subió 7 puntos.


- Resultados en la prueba de matemáticas
```{r}
summary_math <- c(summary(StudentsPerformance$math.score))
summary_math
```

En el de matemáticas, el rango  a diferencia de los dos primeros es de [0, 100], por lo que en este examen se logro obtener tanto la máxima como la mínima nota posible, luego vemos que la media y la mediana también son muy parecidas por hay más confianza al asegurar que no hay valores tan extremos que puedan alterar la media de resultados que como se puede apreciar es la menor de las tres medias que hemos visto, intuyendo que los estudiantes se prepararon menos para este examen o que resultó más complicado que los otros desde el punto del tomador de la prueba.

Para su comodidad, las agruparemos en una tabla.

```{r}
Resument <- data.frame(Summary_writing, Summary_reading, summary_math)
Resumen <- t(Resument)
rownames(Resumen) <- c("Escritura", "Lectura", "Matemáticas")
colnames(Resumen) <- c("Mínimo", "1er Cuart.", "Mediana","Media","3er Cuart.", "Máximo")
knitr::kable(Resumen)
```
![](C:/Users/Alejandro/Desktop/Trabajo estadística/Capturas jamovi/Desc.png)

# 2. Estimación de intervalos y contraste de hipotesis

Ahora procederemos a escoger intervalos de confianza para cuatro posibilidades distintas, en todas buscaremos un intervalo de confianza del 95%, la razón de esto es que no conocemos sufiente del tema para proponer nosotros nuestras propias confianzas, por lo que usaremos la más común.

Lo que haremos aquí será tomar como cierto la afirmación de que nuestra población total equivale a las 1000 personas que realizaron el test y cojeremos una muestra de esos 1000 para poder trabajar con ella.

## 2.1 Examen de matemáticas

Antes que nada lo primero que haremos será ver con lo que estamos trabajando, por ello hemos decidido crear unos histogramas para interpretar los datos.

```{r}
mathex <- (StudentsPerformance$math.score)
math_mean <- mean(mathex)
Sample_math <- sample(StudentsPerformance$math.score, size=100)
sample_math_mean <- mean(Sample_math)

hist(mathex)
hist(Sample_math)
#skewness(Sample_math)
#skewness(mathex)

```


Los histogramas como se puede apreciar, se alejan de la curva de distribución normal de Bell, por ello vamos a aplicar la teoría del teorema del límite central, concretamente la distribución de muestreo, que lo que hace es generar una distribución con la media de muchas muestras, esto será realizado con un simple bucle "for".

Y nos quedaría algo más manejable como esto:

```{r}
puntos <- c()
for (x in 1:200){
  sample_math_ <- sample(StudentsPerformance$math.score, size = 100)
  puntos <- c(puntos, mean(sample_math_))
  
}
plot(density(puntos))

```

Ahora vamos a establecer los intervalos, se puede realizar de varias formas, de manera manual sería con la fórmula $error=z*\frac{\sigma}{\sqrt{n}}$ para que luego dicho error se lo sumaremos y restaremos a nuestra media de muestra para hallar el intervalo de confianza.
Z es el valor crítico, se puede calcular con la función qnorm en r y su valor es muy próximo a dos cuando la confianza es del 95%, sigma es la desviación estándar de la población total y n es el número total de población.

Notar que es bilateral por lo que 0.25-0.25.

No obstante, esto en R también se puede obtener con las funciones de "qnorm()" o "qt()" en caso de que se desconociera la desviación estándar de la población.


Sin embargo, nosostros para ahorrar tiempo directamente lo veremos a través del test.t que nos facilatará dicha información.

Pero antes de hacerlo, primero plantearemos nuestra hipotesis alternativa que en este caso nuestra hipotesis será la de que los datos de la muestra tienen relación con los datos de la población total, por lo tanto nuestra hipotesis nula o de no reacción sería que no tuvieran. 

$H_0:\mu_m\neq\mu_p$

$H_1:\mu_m=\mu_p$
```{r}
t.test(Sample_math, mu = math_mean, conf.level = 0.95, alternative = "two.sided")
```
Como se puede observar en el test, el p valor en nuestro caso supera no supera al 0.5 por lo que tenemos la suficiente confianza para rechazar la hipotesis nula quedándonos por ahora con la hipotesis alternativa.


# 2.2 Relación de los resultados según puntuación de mates en lectura y escritura

Ahora vamos a analizar la relación que hay entre los tres exámenes, para ello nos vamos a preguntar lo siguiente: Los alumnos que mejores notas obtuvieron en los exámenes matemáticas, ¿también obtendran buenos resultados en los tests restantes?

Primero veremos la relación entre el examen de lectura con el de matemáticas. Para representar los resultados hemos decidido realizar una gráfica de puntos con regresión lineal junto con una gráfica de cuartiles.

Esto en jamovi se realizará clicando el icono de histogramas, elegimos una gráfica de puntos y le aplicamos la propiedad de _"Q-Q plot"_.

![](C:/Users/Alejandro/Desktop/Trabajo estadística/Capturas jamovi/QQ1.png)



```{r}
plot(StudentsPerformance$math.score,StudentsPerformance$reading.score)
mod1<-lm(StudentsPerformance$math.score ~ StudentsPerformance$reading.score)
abline(mod1, col=2, lwd = 3)
qqplot(StudentsPerformance$math.score,StudentsPerformance$reading.score)
```
Como se puede apreciar en ambas gráficas, hay una clara relación en los resultados de los estudiantes, al menos en el de matemáticas con el de lectura, ahora vamos a ver el de escritura, aunque preveemos que nos va a dar algo parecido.

```{r}
plot(StudentsPerformance$math.score,StudentsPerformance$writing.score)
mod2<-lm(StudentsPerformance$math.score ~ StudentsPerformance$writing.score)
abline(mod2, col=2, lwd = 3)
qqplot(StudentsPerformance$math.score,StudentsPerformance$writing.score)
```
Efectivamente, las gráficas de lectura sobre matemáticas y escritura sobre matemáticas son muy similares, entendiendo que los estudiantes no tuvieron que sacrificar ningún examen para obtener mejores notas en otro, sino que el nivel de cada alumno se vió reflejado en los tres exámenes más o menos por igual.

# 2.3 Relación de las medias de de mates con lectura y escritura

Ahora vamos a comparar sus medias para ver si son iguales, para ello nos ayudaremos del test de dos muestras con la corrección de Welch.

Primero iremos con el de lectura:
$H_0:\mu_m\neq\mu_l$
$H_1:\mu_m=\mu_l$

```{r}
t.test(StudentsPerformance$reading.score, StudentsPerformance$math.score,  alt="two.sided", conf = 0.95)
```
Como se puede apreciar, el p valor es muy pequeño, lo que indica que no tenemos pruebas suficientes para rechazar la hipotesis nula y quedarnos con la alternativa.

Ahora vamos a probar con el examen de escritura:
$H_0:\mu_m\neq\mu_l$
$H_1:\mu_m=\mu_l$
```{r}
t.test(StudentsPerformance$writing.score, StudentsPerformance$math.score,  alt="two.sided", conf = 0.95)

```
Al igual que en la prueba de lectura, no hay sufuciente confianza para rechazar la hipotesis nula de no reacción, por lo que por ahora afirmaremos que no son iguales las medias, no obstante notar como este p valor es más grande que el anterior, esto se debe a que estas dos medias se parecen un poco más, por lo que habiamos comentado al principio de que el examen de lectura salió mejor que el resto.


## 2.4 ¿Sacaron las mujeres mejores notas que los hombres?

Ahora, por último hemos decidido comparar ambos géneros, para ello, no es necesario pero debido a nuestra falta de conocimiento, en R primero crearemos variables dummies para trabajar más comodamente, esto en jamovi no será necesario.
```{r}

StudentsGender <- StudentsPerformance[,c(1,6,7,8)]

StudentsGenderD <- fastDummies::dummy_cols(StudentsGender, remove_first_dummy = TRUE)
knitr::kable(head(StudentsGenderD))
female_score <- StudentsGenderD[StudentsGenderD$gender_male==0,]
number_females <- nrow(female_score)



male_score <- StudentsGenderD[StudentsGenderD$gender_male==1,]
number_males <- nrow(male_score)
```

Lo siguiente que haremos será graficar dos histogramas y ver con que estamos trabajando.

```{r}

female_math.score <- female_score[,2]
hist(female_math.score, freq=TRUE, xlab = "Resultados", ylab = "Frecuencia", main = "Histograma de los chicos")
Female_math.mean <- mean(female_math.score)

male_math.score <- male_score[,2]
hist(male_math.score, freq=TRUE, xlab = "Resultados", ylab = "Frecuencia", main = "Histograma de las chicas")
lines(density(male_math.score))
Male_math.mean <- mean(male_math.score)

```
De los dos histogramas podemos ver que la mayoría de los resultados se encuentran entre [50-70], entendiendo así además que afortunadamente la mayoría de ellos lograron aprobar. Pero para poder diferencia mejor ambos grupos vamos a pasar por diferentes gráficos.

No obstante, el tamaño de ambos grupos _no_ es igual.
```{r, echo=T}
number_females
number_males
```

Esta vez para la realización de los intervalos únicamente nos centraremos en el test-t, puesto que la otra teoría manual, a nuestro criterio menos eficiente ya está explicada. 
En este caso plantearemos como $H_0$ que existe diferencia entre las medias y $H_1$ será por ende que no, para ello hemos hecho un t test tanto en R como en Jamovi:

![](C:/Users/Alejandro/Desktop/Trabajo estadística/Capturas jamovi/t_test_gender_math2.jpg)
```{r}
t.test(female_math.score, male_math.score)
```

En ambos test podemos observar que tenemos suficientes pruebas para decartar la hipotesis nula y quedarnos por el momento con la hipotesis alternativa de que ambos grupos obtuvieron puntuaciones diferentes. 

Para ello vamos a graficarlo con el fin de enteder cual de los dos grupos tuvo un mejor rendimiento.

Primero veremos la diferencia de sus medias

```{r}
Female_math.mean <- mean(female_math.score)
Male_math.mean <- mean(male_math.score)

Math.mean <- c(Male_math.mean, Female_math.mean)
male_fem <- c("Hombres", "Mujeres")
color.male_fem <- c("Blue", "Pink")
barplot(Math.mean, names.arg = male_fem, col = color.male_fem, xlab = "Media del examen de matemáticas")
```

Y luego veremos el comportamiento de la regresión lineal.

```{r}
plot(StudentsGenderD$gender_male, StudentsGenderD$math.score, xlab = "Mujer = 0, Hombre = 1", ylab = "Resultados matemáticas")
regresion_gender_math <- lm(StudentsGenderD$math.score ~ StudentsGenderD$gender_male)
abline(regresion_gender_math, lw = 2, col = 2)
```

El siguiente será una gráfica de puntos con regresión lineal que nos comparará ambas medias mostrándonos cual de los dos grupos obtuvo un mejor rendimiento ante la prueba, que como se verá por la regresión lineal ascendente, los chicos lograron obtener mejores resultados en general.
Esto también se puede presenciar en la gráfico de barras de arriba que nos compara las medias dando la clara ventaja a los varones. 

Notar que esta base de datos no refleja ninguna población real, sino que el autor de dicha base aclararó que son datos ficticios creados únicamente con el proposito de proporcionar información cómoda a aquellas personas que se esten iniciando en la estadística. Esto explica el porque los hombres tuvieron mejor rendimiento.

```{r}
# #Intervalos Mujer
# fem_math.mean <- mean(female_math.score)
# fem_math.sd <- sd(female_math.score)
# fem_error.n <- qnorm(0.975)*(fem_math.sd/sqrt(482))
# fem_error.t <- qt(0.975, df = 481)*(fem_math.sd/sqrt(482))
# 
# 
# fx1 <- fem_math.mean - fem_error.n
# fy1 <- fem_math.mean + fem_error.n
# fem_Conf_math.interval_n <- c(fx1,fy1)
# fem_Conf_math.interval_n
# 
# fx2 <- fem_math.mean - fem_error.t
# fy2 <- fem_math.mean + fem_error.t
# fem_Conf_math.interval_t <- c(fx2,fy2)
# fem_Conf_math.interval_t
# mean(sample(female_math.score, 19, replace = TRUE))
# 
# #Intervalos Hombre
# male_math.mean <- mean(male_math.score)
# male_math.sd <- sd(male_math.score)
# male_error.n <- qnorm(0.975)*(male_math.sd/sqrt(518))
# male_error.t <- qt(0.975, df = 517)*(male_math.sd/sqrt(518))
# 
# 
# mx1 <- male_math.mean - male_error.n
# my1 <- male_math.mean + male_error.n
# male_Conf_math.interval_n <- c(x1,y1)
# male_Conf_math.interval_n
# 
# mx2 <- male_math.mean - male_error.t
# my2 <- male_math.mean + male_error.t
# male_Conf_math.interval_t <- c(x2,y2)
# male_Conf_math.interval_t
# mean(sample(male_math.score, 19, replace = TRUE))
```

```{r}

#t.test(female_math.score ~ male_math.score, var.eq=T, conf.int=0.95)
```


Otras gráficas:

Gráfica que compara los resultados del examen de matemáticas con el nivel de educación que poseían los padres de los tomadores del examen.
Como se puede apreciar no hay suficiente confianza para afirmar que el nivel educativo de los padres fuera relevante, pues no parece ser que ningún grupo sea excesivamente mayor que otros
```{r}

StudentsParent_lvl_D <- StudentsPerformance[,c(3,6,7,8)]
StudentsParent_lvl <- fastDummies::dummy_cols(StudentsParent_lvl_D, remove_first_dummy = TRUE)
colnames(StudentsParent_lvl) <-  c("lvl","math","reading","writing","lvl_bachelors","lvl_hs","lvl_masters","lvl_some.hs","lvl_some.college")
#knitr::kable(StudentsParent_lvl)

boxplot(StudentsPerformance$math.score ~ StudentsPerformance$parental.level.of.education)


#Associated <- StudentsParent_lvl[StudentsParent_lvl$lvl_bachelors==1,Students,c(2,3,4,5)]
```


Trabajo realizado con Rstudio y jamovi simultáneamente.
