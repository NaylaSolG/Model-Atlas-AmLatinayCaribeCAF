##EXPLORACION Y DESCRIPCIÓN DE LA BASE DE DATOS##

#Cargo librerias

library(tidyverse)
library(viridis)
library(ggridges)
library(dplyr)
library(tidymodels)
library(patchwork)
library(GDAtools)
library(ggdendro)
library(ggplot2)
library(corrplot)

#Cargo base de datos

base_muni_original <- read.csv('m4_atlas_argentina.csv')

#Uso la función names y head para comenzar a explorar mi base

names(base_muni_original)

head(base_muni_original)

#Buscamos medidas de tendencia central y medidas de dispersión (varianza y desvío estándar)

#La media, mediana, varianza y desvio de:

#POBLACIÓN 
#Esta variable está en numeros enteros por cantidad de habitantes.
#Útil para conocer los perfiles de municipios.

mean(base_muni_original$poblacion, na.rm = TRUE)
var(base_muni_original$poblacion, na.rm = TRUE) 
sd(base_muni_original$poblacion, na.rm = TRUE)
median(base_muni_original$poblacion, na.rm = TRUE)

#RECOLECCION DE RESIDUOS

mean(base_muni_original$recoleccion_residuos, na.rm = TRUE)
median(base_muni_original$recoleccion_residuos, na.rm = TRUE)
var(base_muni_original$recoleccion_residuos, na.rm = TRUE)
sd(base_muni_original$recoleccion_residuos, na.rm = TRUE)

#EMPLEO PÚBLICO

mean(base_muni_original$empleo_publico_categoria, na.rm = TRUE)
median(base_muni_original$empleo_publico_categoria, na.rm = TRUE)
var(base_muni_original$empleo_publico_categoria, na.rm = TRUE)
sd(base_muni_original$empleo_publico_categoria, na.rm = TRUE)

#ANALFABETISMO

mean(base_muni_original$analfabetismo, na.rm=TRUE)
median(base_muni_original$analfabetismo, na.rm = TRUE)
var(base_muni_original$analfabetismo, na.rm = TRUE)
sd(base_muni_original$analfabetismo, na.rm=TRUE)

#FIN DE SECUNDARIO ADULTOS

mean(base_muni_original$fin_secundaria_adultos, na.rm=TRUE)
median(base_muni_original$fin_secundaria_adultos, na.rm = TRUE)
var(base_muni_original$fin_secundaria_adultos, na.rm = TRUE)
sd(base_muni_original$fin_secundaria_adultos, na.rm=TRUE)

#PARTICIPACION LABORAL DE MUJERES

mean(base_muni_original$participacion_mujeres, na.rm=TRUE)
median(base_muni_original$participacion_mujeres, na.rm = TRUE)
var(base_muni_original$participacion_mujeres, na.rm = TRUE)
sd(base_muni_original$participacion_mujeres, na.rm=TRUE)

#PARTICIPACION LABORAL DE HOMBRES

mean(base_muni_original$participacion_hombres, na.rm=TRUE)
median(base_muni_original$participacion_hombres, na.rm = TRUE)
var(base_muni_original$participacion_hombres, na.rm = TRUE)
sd(base_muni_original$participacion_hombres, na.rm=TRUE)

#ELECTRICIDAD

mean(base_muni_original$electricidad, na.rm=TRUE)
median(base_muni_original$electricidad, na.rm = TRUE)
var(base_muni_original$electricidad, na.rm = TRUE)
sd(base_muni_original$electricidad, na.rm=TRUE)

#AGUA MEJORADA

mean(base_muni_original$agua_mejorada, na.rm=TRUE)
median(base_muni_original$agua_mejorada, na.rm = TRUE)
var(base_muni_original$agua_mejorada, na.rm = TRUE)
sd(base_muni_original$agua_mejorada, na.rm=TRUE)

#SANEAMIENTO

mean(base_muni_original$saneamiento_mejorado, na.rm=TRUE)
median(base_muni_original$saneamiento_mejorado, na.rm = TRUE)
var(base_muni_original$saneamiento_mejorado, na.rm = TRUE)
sd(base_muni_original$saneamiento_mejorado, na.rm=TRUE)

#Grafico distribuciones individuales de las variables elegidas para ver como se comportan y hacer una lectura de algunos de estos graficos

#ELECTRICIDAD

base_muni_original %>% 
  ggplot(aes(x=electricidad)) +
  geom_density(adjust=1.5, alpha=.4, fill = "red") +
  theme_minimal()+
  xlab("Proporcion de hogares con acceso a servicio de electricidad") +
  xlim(0,5)

#AGUA MEJORADA

base_muni_original %>% 
  ggplot(aes(x=agua_mejorada)) +
  geom_density(adjust=1.5, alpha=.4, fill = "yellow") +
  theme_minimal()+
  xlab("Proporcion de hogares con acceso a fuentes de agua mejorada") +
  xlim(0,5)

#SANEAMIENTO

base_muni_original %>% 
  ggplot(aes(x=saneamiento_mejorado)) +
  geom_density(adjust=1.5, alpha=.4, fill = "pink") +
  theme_minimal()+
  xlab("Proporcion de hogares con acceso a servicios de saneamiento mejorado") +
  xlim(0,5)

#RECOLECCION DE RESIDUOS

base_muni_original %>% 
  ggplot(aes(x=recoleccion_residuos)) +
  geom_density(adjust=1.5, alpha=.4, fill = "blue") +
  theme_minimal()+
  xlab("Proporcion de hogares con servicio de recoleccion de residuos") +
  xlim(0,5)

#EMPLEO PUBLICO

base_muni_original %>% 
  ggplot(aes(x=empleo_publico_categoria)) +
  geom_density(adjust=1.5, alpha=.4, fill = "green") +
  theme_minimal()+
  xlab("Proporción de personas ocupadas que se desempeñan como empleados del sector público según su categoría ocupacional") +
  xlim(0,2)+
  theme(axis.title.x = element_text(size = 6))

#FIN SECUNDARIA ADULTOS 

base_muni_original %>%
  ggplot(aes(x=fin_secundaria_adultos))+
  geom_density(adjust=1.5, alpha=0.6, fill = "lightblue")+
  theme_minimal()+
  xlab("Fin de secundaria adultos")+
  xlim(0,1)

#ANALFABETISMO

base_muni_original %>%
  ggplot(aes(x=analfabetismo))+
  geom_density(adjust=1.5, alpha=0.6, fill = "violet")+
  theme_minimal()+
  xlab("Analfabetismo")+
  xlim(0,1)

#PARTICIPACION LABORAL HOMBRES

base_muni_original %>%
  ggplot(aes(x=participacion_hombres))+
  geom_density(adjust=1.5, alpha=0.6, fill = "grey")+
  theme_minimal()+
  xlab("Participación laboral de hombres")+
  xlim(0,1)

#PARTICIPACION LABORAL MUJERES

base_muni_original %>%
  ggplot(aes(x=participacion_mujeres))+
  geom_density(adjust=1.5, alpha=0.6, fill = "lightgreen")+
  theme_minimal()+
  xlab("Participación laboral de mujeres")+
  xlim(0,1)

#Grafico correlaciones de 2 variables juntas de algunas variables elegidas para ver como se comportan y hacer una lectura de dichos graficos.

#SANEAMIENTO MEJORADO Y PARTICIPACION LABORAL MUJERES

base_muni_original%>% 
  ggplot(aes(x=saneamiento_mejorado, y=participacion_mujeres)) + 
  geom_point() + 
  theme_minimal()

#Codigo para sacar R de Pearson

cor(base_muni_original$saneamiento_mejorado, 
    base_muni_original$participacion_mujeres,
    use = "complete.obs")

#RECOLECCION DE RESIDUOS Y FIN DE SECUNDARIA ADULTOS

base_muni_original %>%
  ggplot(aes(x=recoleccion_residuos, y=fin_secundaria_adultos))+
  geom_point(alpha=0.3)+
  xlab("Recolección de residuos")+
  ylab("Fin de secundaria adultos")+
  theme_minimal()

#Codigo para sacar R de Pearson

cor(base_muni_original$recoleccion_residuos, 
    base_muni_original$fin_secundaria_adultos,
    use = "complete.obs")

#AGUA MEJORADA Y EMPLEO PUBLICO 

base_muni_original %>%
  ggplot(aes(x=agua_mejorada, y=empleo_publico_categoria))+
  geom_point(alpha=0.3)+
  xlab("Agua mejorada")+
  ylab("Empleo Publico")+
  theme_minimal()

#Codigo para sacar R de Pearson

cor(base_muni_original$agua_mejorada, 
    base_muni_original$empleo_publico_categoria,
    use = "complete.obs")

#PARTICIPACIÓN MUJERES Y FIN DE SECUNDARIO

base_muni_original %>%
  ggplot(aes(x=participacion_mujeres, y=fin_secundaria_adultos))+
  geom_point(alpha=0.3)+
  xlab("Participacion Laboral Mujeres")+
  ylab("Fin de secundaria adultos")+
  theme_minimal()

#Codigo para sacar R de Pearson

cor(base_muni_original$participacion_mujeres, 
    base_muni_original$fin_secundaria_adultos,
    use = "complete.obs")

#MATRIZ DE CORRELACION CON TODAS LAS VARIABLES

matriz_correla_total <- base_muni_original %>% 
  select(
    analfabetismo,
    fin_secundaria_adultos,
    urbano,
    electricidad,
    agua_mejorada,
    saneamiento_mejorado,
    participacion_mujeres,
    participacion_hombres,
    desempleo_adulto,
    desempleo_adulto_hombre,
    desempleo_adulto_mujer,
    recoleccion_residuos,
    empleo_publico_categoria,
    etnia_indigena,
    etnia_afro,
    tics_computadora,
    tics_celular
  ) %>% 
  select(where(~ sd(.x, na.rm = TRUE) > 0))

mat_cor_total <- cor(matriz_correla_total, use = "complete.obs")

mi_paleta <- colorRampPalette(c("navy","#91CBD765", "#CA225E"))

mat_cor_total%>%
  corrplot(col = mi_paleta(200), tl.col = "black", method = "square")


## PCA educación

basePCA_edu <- base_muni_original %>%
  select(codigo_unico,
         fin_secundaria_adultos,
         analfabetismo)%>%
  drop_na()

recipe_pca_edu <- basePCA_edu %>% 
  select(analfabetismo,
         fin_secundaria_adultos) %>%
  recipe(~.) %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric(), num_comp=2)

pca_estimates_edu <-  prep(recipe_pca_edu, training = basePCA_edu)
pca_data_edu <- bake(pca_estimates_edu, basePCA_edu)

tidy(pca_estimates_edu, 2, type="coef") %>%
  filter(component %in% c("PC1", "PC2")) %>%
  ggplot(aes(value, terms, fill=terms)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=round(value,2))) +
  labs(title="Cargas factoriales (comp. 1 y 2)",
       x="Valor",
       y="Variable") + 
  facet_wrap(~component, nrow=1) +
  theme_minimal()

tidy(pca_estimates_edu, 2, type="variance") %>%
  filter(terms=="percent variance") %>%
  mutate(component=paste0("PC", component)) %>%
  ggplot(aes(x=component,y=value,group=terms)) +
  geom_col() +
  ylim(0,100) +
  labs(title="% varianza",
       x="Componente",
       y="Valor") + 
  theme_minimal()

tidy(pca_estimates_edu, 2, type="variance") %>%
  filter(terms=="cumulative percent variance") %>%
  mutate(component=paste0("PC", component)) %>%
  ggplot(aes(x=component,y=value,group=terms)) +
  geom_col() +
  ylim(0,100) +
  labs(title="% varianza acum.",
       x="Componente",
       y="Valor") + 
  theme_minimal()

## Unión PCA_educacion a la base original 

basePCA_edu <- basePCA_edu %>%
  bind_cols(pca_data_edu)

base_muni_original <- base_muni_original %>%
  left_join(
    basePCA_edu %>% select(codigo_unico, PC1),
    by = "codigo_unico"
  )

base_muni_original <- base_muni_original%>%
  rename(pc1_educacion=PC1)

## PCA Servicios

basePCA_serv <- base_muni_original %>%
  select(codigo_unico,
         electricidad,
         agua_mejorada,
         saneamiento_mejorado,
         recoleccion_residuos)%>%
  drop_na()

recipe_pca_serv <- basePCA_serv %>% 
  select(electricidad,
         agua_mejorada,
         saneamiento_mejorado,
         recoleccion_residuos) %>%
  recipe(~.) %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric(), num_comp=2)

pca_estimates_serv <-  prep(recipe_pca_serv, training = basePCA_serv)
pca_data_serv <- bake(pca_estimates_serv, basePCA_serv)

tidy(pca_estimates_serv, 2, type="coef") %>%
  filter(component %in% c("PC1", "PC2")) %>%
  ggplot(aes(value, terms, fill=terms)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=round(value,2))) +
  labs(title="Cargas factoriales (comp. 1 y 2)",
       x="Valor",
       y="Variable") + 
  facet_wrap(~component, nrow=1) +
  theme_minimal()

tidy(pca_estimates_serv, 2, type="variance") %>%
  filter(terms=="percent variance") %>%
  mutate(component=paste0("PC", component)) %>%
  ggplot(aes(x=component,y=value,group=terms)) +
  geom_col() +
  ylim(0,100) +
  labs(title="% varianza",
       x="Componente",
       y="Valor") + 
  theme_minimal()

tidy(pca_estimates_serv, 2, type="variance") %>%
  filter(terms=="cumulative percent variance") %>%
  mutate(component=paste0("PC", component)) %>%
  ggplot(aes(x=component,y=value,group=terms)) +
  geom_col() +
  ylim(0,100) +
  labs(title="% varianza acum.",
       x="Componente",
       y="Valor") + 
  theme_minimal()

## Unión PCA_servicios a la base original 

basePCA_serv <- basePCA_serv %>%
  bind_cols(pca_data_serv)

base_muni_original <- base_muni_original %>%
  left_join(
    basePCA_serv %>% select(codigo_unico, PC1),
    by = "codigo_unico"
  )

base_muni_original <- base_muni_original%>%
  rename(pc1_servicios=PC1)

## Cambio signo PC1_servicios y PC1_educación. Ambos PC1 quedaron con signos negativos cuando significan mayor nivel educativo y mayor acceso a servicios.
## Modificamos el signo para que la lectura posterior sea más clara e intuitiva

base_muni_original$pc1_educacion <- - base_muni_original$pc1_educacion
base_muni_original$pc1_servicios <- - base_muni_original$pc1_servicios

## Clustering

##Creo una base nueva de clustering
base_cluster1 <- base_muni_original %>%
  select(
    codigo_unico,
    pc1_servicios,
    pc1_educacion,
    participacion_mujeres,
    participacion_hombres,
    empleo_publico_categoria,
    urbano, desempleo_adulto, tics_computadora
  ) %>%
  drop_na()

## K MEDIAS

## Primera prueba con k:2

recipe_km1 <- base_cluster1%>%
  recipe(~.) %>%
  step_normalize(c(pc1_servicios, pc1_educacion, empleo_publico_categoria))

set.seed(123)

km_2clusters <- recipe_km1%>%
  prep() %>%
  bake(base_cluster1) %>%
  select(pc1_educacion, pc1_servicios, empleo_publico_categoria) %>%
  kmeans(x=.,centers=2)

## Analizo los resutados

km_2clusters
tidy(km_2clusters)

## calidad de los clusters 39%

### POSIBLE ANÁLISIS DE GRÁFICOS ###

## Análisis de distintos k

#Generamos un vector con la secuencia de clústers
centers <- 1:10 

#Incializamos dos tibbles vacías para llenar con las asignaciones de clusters y con algunas métricas
assignments <- tibble()
clusterings <- tibble()

#Iteramos sobre cada uno de los elementos de centers
for (i in centers){
  
  #Corremos la receta, seleccionamos las variables para clusterizar y corremos el k-medias con el  
  km <- recipe_km1 %>%
    prep() %>%
    bake(base_cluster1) %>%
    select(pc1_educacion, pc1_servicios, empleo_publico_categoria) %>%
    kmeans(x=.,
           centers=i)
  
  #Ejecutamos glance sobre km para extraer las métricas de variabilidad intra y extracluster y las agregamos a una de las tibbles
  clusterings <- clusterings %>% 
    bind_rows(
      glance(km) %>% 
        mutate(k=i) %>% 
        select(k, everything())
    )
  
  #Ejecutamos augment sobre km y df para agregar las pertenencias a los clusters a la tabla original; luego, apilamos todas las tablas en una sola
  assignments <- assignments %>%
    bind_rows(
      augment(km, base_cluster1) %>%
        mutate(k = i) %>%
        select(k, everything())
    )
}

clusterings

assignments

##Evaluando K

ggplot(clusterings, aes(x=as.factor(k), y=tot.withinss, group=1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x="Cantidad clústers (k)",
       y="Variabilidad intra-cluster")

## El resultado me indica que lo mejor sería k:4 o k:5 clusters 

## Pruebo k: 4

set.seed(123)

km_4clusters <- recipe_km1%>%
  prep() %>%
  bake(base_cluster1) %>%
  select(pc1_educacion, pc1_servicios, empleo_publico_categoria) %>%
  kmeans(x=.,centers=4)

km_4clusters
tidy(km_4clusters)

## calidad de k:4 63,4%


## Pruebo k: 5

set.seed(123)

km_5clusters <- recipe_km1%>%
  prep() %>%
  bake(base_cluster1) %>%
  select(pc1_educacion, pc1_servicios, empleo_publico_categoria) %>%
  kmeans(x=.,centers=5)

km_5clusters
tidy_k5 <- tidy(km_5clusters)

## K:5 tiene mejor calidad 68,8%

## La iteración se estabiliza con X cantidad de ejecuciones

##prueba k:5 unicamente con servicios y educación (para ver si empleo cambia)

set.seed(123)

km_5sinempleo <- recipe_km1%>%
  prep() %>%
  bake(base_cluster1) %>%
  select(pc1_educacion, pc1_servicios) %>%
  kmeans(x=.,centers=5)

km_5sinempleo

tidy(km_5sinempleo)

## Lo uno a la base
base_cluster1 <- augment(km_5clusters, base_cluster1)

#### Gráficos boxplots para el análisis de los clusters
base_cluster1 %>%
  pivot_longer(cols = pc1_servicios:tics_computadora) %>%
  ggplot(aes(x = .cluster, y = value, fill = .cluster)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_wrap(~name, scales = "free_y")


base_cluster1 %>%
  pivot_longer(cols = tics_computadora) %>%
  ggplot(aes(x = .cluster, y = value, fill = .cluster)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_wrap(~name, scales = "free_y")

base_cluster1 %>%
  pivot_longer(cols = participacion_hombres) %>%
  ggplot(aes(x = .cluster, y = value, fill = .cluster)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_wrap(~name, scales = "free_y")

base_cluster1 %>%
  pivot_longer(cols = participacion_mujeres) %>%
  ggplot(aes(x = .cluster, y = value, fill = .cluster)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_wrap(~name, scales = "free_y")

base_cluster1 %>%
  pivot_longer (cols = c(pc1_servicios, pc1_educacion, empleo_publico_categoria)) %>%
  ggplot(aes(x = .cluster, y = value, fill = .cluster)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_wrap(~name, scales = "free_y")

base_cluster1 %>%
  pivot_longer(cols = desempleo_adulto) %>%
  ggplot(aes(x = .cluster, y = value, fill = .cluster)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  facet_wrap(~name, scales = "free_y")


## incorporo variables de división política para el análisis final

base_cluster1 <- base_cluster1%>%
  left_join(
    base_muni_original%>% select(codigo_unico, gob_regional, gob_local),
    by = "codigo_unico"
  )

## 
provincias_cluster <- base_cluster1%>%
  count(gob_regional, .cluster)%>%
group_by(gob_regional)%>%
  mutate(prop=n/sum(n))
