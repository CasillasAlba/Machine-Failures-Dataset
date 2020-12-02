---
title: "Machine Learning Model"
Autor: Alba Casillas
output: html_notebook
editor_options: 
  chunk_output_type: console
---
Opciones generales
```{r}

options(scipen=999)#Desactiva la notacion cientifica

```
Instalamos y cargamos las librerías necesarias 
```{r}

#Instalar librerías
install.packages('dplyr') #para manipular datos
install.packages('skimr') #para exploración inicial
install.packages('lubridate') #para manipular fechas
install.packages('tidyr') #para manipular datos
install.packages('ggplot2') #para hacer gráficos

#Cargar librerías
library(dplyr)
library(skimr)
library(lubridate)
library(tidyr)
library(ggplot2)

```
Cargamos los datos
```{r}

# <- asigna un valor a una variable

df <- read.csv(file = 'DataSetFallosMaquina.csv',sep=';')


```
Análisis inicial
```{r}

# Resumen con información general del dataset
# Rows == Observations y Columns == variables
# Failure es nuestra variable target (la que queremos predecir), nos dice si una
# maquina falla o no falla
# <fct> -> tipo factor / variables categóricas.
glimpse(df)

# Informacion sobre valores missing, media... + gráficos
skim(df)

```
Se usa  la función kable del paquete knitr (es una alternativa a cargrarlo con library si sólo vas a usarlo una vez como en nuestro caso), y lo que hace kable es dar un formato bonito a la salida de tablas, hay veces que si skimr tiene problemas para sacar los gráficos forzarlo con kable hace que salgan bien
```{r}

knitr::kable(skim(df))

```
Conclusiones:
No hay nulos
Problemas con tipos de variables:
- Measure2 y Measure3 también parecen más factores que enteros (pocos datos diferentes entre sí)
- Viendo el mínimo y el p25 de Temperature parece que tiene atípicos -> DISTRIBUCIÓN SESGADA
```{r}

# Analizamos la temperatura
# ggplot(conjunto datos, variable analizar) + tipo de grafo(variable=aes(pintar Temp en eje Y))
# Conclusion: la mayoria de los datos se engloban en el p25 y p75 y hay
# 4 datos (puntos) que se salen mucho de los datos de la variable
# Todo dato por debajo de los 60º es un dato atípico

ggplot(df,x=1) + geom_boxplot(aes(y=Temperature))


```
Calidad de datos
```{r}

# Corregimos los tipos de variables y los atípicos
# Mutate -> corrige variables / crea nuevas variables
# df <- df %>% => encadena instrucciones una detrás de otra (pipe)
# En este caso, al dataframe inicial, corriges Measure2 y Measure3 transformandolas
# en factores y eliminamos los valores de temperatura < 60º.
 
df <- df %>%
  mutate(Measure2 = as.factor(Measure2), #Corregimos Measure2
         Measure3 = as.factor(Measure3)) %>% #Corregimos Measure3 
  filter(Temperature > 50) #eliminamos los 4 atípicos de temperature


```
Análisis exploratorio de variables (EDA)
```{r}

# Exploramos las de tipo factor
# Analizas todas las variables del dataset con una misma tipología
# select_if -> selecciona si cada una de las variables es de tipo factor
# gather -> cambia el orden del dataset de formato horizontal a vertical para
# que ggplot pueda hacer todos los gráficos a la vez.
# ggplot(aes(value)) + geom_bar() -> a estas variables se genera un gráfico de barras
# facet_wrap -> obtener tantos gráficos como variables tenemos
# theme -> para cambiar el tamaño del texto del eje y que se lea bien.
# Conclusion -> target failure MUY DESBALANCEADO
# Operator -> Operador 2 tiene el doble de valores.

df %>%
  select_if(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) + geom_bar() + facet_wrap(~key,scales='free') +
  theme(axis.text=element_text(size=6))

#Y las de tipo entero

df %>%
  select_if(is.integer) %>%
  gather() %>%
  ggplot(aes(value)) + geom_density() + facet_wrap(~key,scales='free') +
  theme(axis.text=element_text(size=6))

# Hacemos análisis de correlaciones

df %>%
  select_if(is.integer) %>%
  cor() %>% 
  round(digits = 2)

# Hacemos un zoom sobre el desbalanceo de la variable target

table(df$Failure)

```
Transformación de variables

No son necesarias grandes transformaciones porque el fichero ya viene muy limpio
Tampoco vamos a crear variables sintéticas (nuevas variables) que sí haríamos en la realidad (por ej número de fallos del mismo equipo, etc.)

Pero sí vamos a tener que trabajar sobre el balanceo de la variable target
```{r}

# Vamos a balancear usando la técnica del inframuestreo:
# Comprobamos la penetración exacta de la target
# Tenemos 81 'sis'Si's que sobre el total de casos son un 0,9%:

81/nrow(df) * 100

# Para tener casi un 10% necesitaríamos incrementar la proporción aprox en x10
# Entonces vamos a reducir los 'No's para que salga aprox esa proporción

# Nuevo df de 'No's:
# sample_frac -> crea una muestra con una proporcion del 0.08 con los valores de 'No's
# Más o menos tendremos: 8699 * 0.08 = 695 ~ casos

set.seed(1234) 
df_nos <- df %>%
  filter(Failure == 'No') %>%
  sample_frac(size = 0.08)

# Df de 'Si's

df_sis <- df %>% filter(Failure == 'Yes')

# Y los unimos de nuevo en un nuevo df reducido

df_red <- rbind(df_nos,df_sis)

#Comprobamos de nuevo la penetación de la target
count(df_red,Failure)
81/nrow(df_red) * 100

```
Modelización

1 Dividir en entrentamiento y validación:
No lo vamos a hacer por simplicidad y porque tenemos pocos casos

2 Roles de las variables
```{r}

target <- 'Failure'
indep <- names(df_red)[-20] #Variables predicotras = todas menos la 20 (target)
formula <- reformulate(indep,target) #reformulate -> predecir target a partir de indep

```
Vamos a modelizar con una regresión logística, ya que queremos un resultado [0,1] = [No,Si]
glm -> modelos lineales generalizados
```{r}

rl <- glm(formula,df_red,family=binomial(link='logit'))
#Vemos el resultado
summary(rl) 


```
Sólo resultan predictivas al menos al 95% tres variables, que vamos a seleccionar como finales
```{r}

indep_fin <- c('Temperature','Humidity','Measure9')
formula <- reformulate(indep_fin,target) #actualizamos la fórmula

```
Y volvemos a modelizar
```{r}

rl <- glm(formula,df_red,family=binomial(link='logit'))
summary(rl)

```
Aplicamos nuestro modelo a los datos
```{r}

# predict -> realiza una predicción de que una máquina se estropee
# teniendo en cuenta las variables del modelo rl, sobre el total de los datos
# (Este paso se dbería hacer sobre la muestra de validación)
# type = response -> devuelve la probabilidad de que se rompa la maquina
# df$scoring -> el resultado se guardará en el fichero df en una nueva
# variable llamada scoring

df$scoring <- predict(rl,df,type='response')
head(df$scoring)

```
Tomamos la decisión de si pensamos que será un fallo o no
```{r}

# Como la penetración inicial era del 1%, vamos a poner un punto de corte muy alto, por ejemplo por encima del 80%
# ifelse -> función que constrasta una condición. Si se cumple, pondrá un 1 en prediccion
# si no se cumple, guardará un 0 en prediccion

df$prediccion <- ifelse(df$scoring > 0.8,1,0)
table(df$prediccion)

```
Evaluación del modelo
Vamos a contrastar la predicción contra la realidad
```{r}

table(df$prediccion,df$Failure)

```
De todos los que predigo que van a fallar la mayoría fallan, pero también me estoy dejando muchos fallos. Podemos poner el corte más bajo.
```{r}

#Vamos a ver qué pasa si bajamos la decisión al 60%
df$prediccion <- ifelse(df$scoring > 0.6,1,0)
table(df$prediccion)

```
Vamos a contrastar la predicción contra la realidad
```{r}
table(df$prediccion,df$Failure)
```
