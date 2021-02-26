## nuevo analisis con modelo lineal
library(nlme)
library(ggplot2)
library(lsmeans)
# AVH
library("MuMIn") # para calcular AICc
library(lme4) #glmer
library(tidyverse)
library(Rmisc)

# importar datos ----

datos <- datos %>%
  mutate(Tratamiento_3cats = ifelse(Tratamiento %in% c("C", "A", "N"), "Dep", Tratamiento))
datos

# transformar: Despl y Cerca binarios ----
datos = datos %>%
  mutate(Despl_total_binario=as.numeric(Despl_total>0),
         Cerca_total_binario=as.numeric(Cerca_total>0),
         EnJaula_binario=as.numeric(EnJaula>0),
         jaula_cerca_binario=as.numeric(jaula_cerca>0))
datos

# explorar: NAs ----
sapply(datos, function(x)
  sum(is.na(x)))

# explorar: cuantos datos tenemos ----
table(datos$Ony)
table(datos$Ony,datos$Tratamiento)
table(datos$Ony,datos$Tratamiento_3cats)

# explorar: distribucion de variables ----

# Despl
datos %>%
  ggplot(aes(Despl_total))+
  geom_histogram()
# Despl binario
datos %>%
  ggplot(aes(Despl_total_binario))+
  geom_histogram()

# Cerca
datos %>%
  ggplot(aes(Cerca_total))+
  geom_histogram()
# Cerca binario
datos %>%
  ggplot(aes(Cerca_total_binario))+
  geom_histogram()


#### secundarias:
# EnJaula
datos %>%
  ggplot(aes(EnJaula))+
  geom_histogram()
# EnJaula binario
datos %>%
  ggplot(aes(EnJaula_binario))+
  geom_histogram()

# Modelos Desplazamiento: GLMMs ----
# Interpr. Biologica: comportamiento antidepredatorio dependiendo del depredador
m_tratamiento_glmer =   glmer(Despl_total ~ Tratamiento+1|Ony,
                              family = "binomial",
                              data = datos)
# Interpr. Biologica: se mueve mas en alguno entre: Ex Neo y Dep.
m_tratamiento3cats_glmer =   glmer(Despl_total ~ Tratamiento_3cats+1|Ony,
                              family = "binomial",
                              data = datos)
# Interpr. Biologica: se mueven al principio, una vez que conocen el chante se ahuevan
m_orden_glmer =   glmer(Despl_total ~ Orden+1|Ony,
                                   family = "binomial",
                                   data = datos)

# Interpr. Biologica: les vale verga nuestros tratamientos y no importa su tamano(edad). #elamornotieneedadnipuedeserdepredado
m_nulo_glmer =   glmer(Despl_total ~ 1|Ony ,
                       family = "binomial",
                       data = datos)
# no converge!!!! tampoco con Despl_total_binario
# Interpr. Biologica: tal vez los grandes se mueven en frente del depredador y los peques no, y tal vez en Neo y Expl se comportan iguales sin importar el tamano
# nota: yo le apostaba a este ):
m_tratamientotamano_glmer =   glmer(Despl_total ~ Tratamiento_3cats*Tamano+1|Ony ,
                                    family = "binomial",
                                    data = datos)
# binario tampoco:
m_tratamientotamano_glmer =   glmer(Despl_total_binario ~ Tratamiento_3cats*Tamano+1|Ony ,
                                    family = "binomial",
                                    data = datos)

# Interpr. Biologica: tratamiento importante si se controla por orden. No estoy seguro si asi se controla esto
# no converge ):
m_tratamientocontrolandoorden_glmer =   glmer(Despl_total_binario ~ Tratamiento+(1|Ony)+(1|Orden) ,
                                    family = "binomial",
                                    data = datos)
m_tamanocontrolandoorden_glmer =   glmer(Despl_total_binario ~ Tamano+(1|Ony)+(1|Orden) ,
                                              family = "binomial",
                                              data = datos)
m_tratamiento3catscontrolandoorden_glmer =   glmer(Despl_total_binario ~ Tratamiento_3cats+(1|Ony)+(1|Orden),
                                              family = "binomial",
                                              data = datos)
m_tratamientotamano_controlandoorden_glmer =   glmer(Despl_total_binario ~ Tratamiento*Tamano +(1|Ony)+(1|Orden),
                                                   family = "binomial",
                                                   data = datos)
m_tratamiento3catstamano_controlandoorden_glmer =   glmer(Despl_total_binario ~ Tratamiento_3cats*Tamano+ (1|Ony)+(1|Orden),
                                                     family = "binomial",
                                                     data = datos)


# Interpr. biologica: Tal vez el comportamiento depende solo del tamano? 
# no se si este tiene sentido porque los tamanos son propiedades de los onys que se estan tomando como efecto aleatorio
m_tamano_glmer =   glmer(Despl_total ~ Tamano+1|Ony,
                                    family = "binomial",
                                    data = datos)
# Desplazamiento GLMMs Model Selection ----
AIC(m_nulo_glmer)

AIC(m_nulo_glmer)-AIC(m_tamano_glmer)
AIC(m_nulo_glmer)-AIC(m_tratamiento_glmer)
AIC(m_nulo_glmer)-AIC(m_tratamientotamano_glmer)
AIC(m_nulo_glmer)-AIC(m_tratamiento3cats_glmer)
AIC(m_nulo_glmer)-AIC(m_orden_glmer)
AIC(m_nulo_glmer)-AIC(m_tratamientocontrolandoorden_glmer)
AIC(m_nulo_glmer)-AIC(m_tratamientocontrolandoorden_glmer)
AIC(m_nulo_glmer)-AIC(m_nulo_glmer)
# 1 ganador: m_orden_glmer. de fijo se cansaron los onysy el orden fue super importante. Recomendacion: no usar este dise;o experimental?
# 2 ganador: m_tratamiento3cats_glmer

# si tomaramos el otro nulo como base
AIC(m_nulo_controlandoorden_glmer)-AIC(m_tratamientocontrolandoorden_glmer)
AIC(m_nulo_controlandoorden_glmer)-AIC(m_tratamiento3catscontrolandoorden_glmer)
AIC(m_nulo_controlandoorden_glmer)-AIC(m_tratamientotamano_controlandoorden_glmer)
AIC(m_nulo_controlandoorden_glmer)-AIC(m_tratamiento3catstamano_controlandoorden_glmer)
AIC(m_nulo_controlandoorden_glmer)-AIC(m_tamanocontrolandoorden_glmer)

# Modelos Desplazamiento: GLM ----
m_tratamiento =   glm(Despl_total_binario ~ Tratamiento ,
             family = "binomial",
             data = datos)
m_tratamiento3cats =   glm(Despl_total_binario ~ Tratamiento_3cats ,
                      family = "binomial",
                      data = datos)
m_tamano =   glm(Despl_total_binario ~ Tamano ,
           family = "binomial",
           data = datos)
m_orden =   glm(Despl_total_binario ~ Orden ,
                 family = "binomial",
                 data = datos)

m_tamanotratamiento =   glm(Despl_total_binario ~ Tratamiento*Tamano ,
                family = "binomial",
                data = datos)

m_tamanotratamiento3cats =   glm(Despl_total_binario ~ Tratamiento_3cats*Tamano ,
                            family = "binomial",
                            data = datos)

m_tamanotratamiento3catsony =   glm(Despl_total_binario ~ Tratamiento_3cats*Ony ,
                                 family = "binomial",
                                 data = datos)

m_ony =   glm(Despl_total_binario ~ Ony ,
                                 family = "binomial",
                                 data = datos)

m_nulo =   glm(Despl_total_binario ~1 ,
                 family = "binomial",
                 data = datos)


# model selection ----
m_nulo$aic
# delta aic: mayor que 2?
m_nulo$aic-m_tratamiento$aic
m_nulo$aic-m_orden$aic
m_nulo$aic-m_tamano$aic
m_nulo$aic-m_tamanotratamiento$aic
m_nulo$aic-m_tamanotratamiento3cats$aic
m_nulo$aic-m_tamanotratamiento3catsony$aic
m_nulo$aic-m_tratamiento3cats$aic
m_nulo$aic-m_ony$aic

m_nulo$aic - AIC(m_tratamiento_glmer)
# depende del ony y no es por su tamano

?m4 =   glm(Despl_total_binario ~ Orden ,
           family = "binomial",
           data = datos)

m2 =   glm(Despl_total_binario ~ Tratamiento + Tamano,
           family = "binomial",
           data = datos)

m3 =   glm(Despl_total_binario ~ Tratamiento + Tamano + Orden,
           family = "binomial",
           data = datos)


m1

full <-
  glmer(Despl_total>0 ~ Tratamiento + Tamano + Orden + 1 | Ony ,
        family = "binomial",
        data = datos)
BBmm(fixed.formula = Despl_total ~ Tratamiento + Tamano, 
         random = 1~ Ony, data = datos,m=1)
         # family = "hubinomial",
      data = datos)
full
# no converge entonces no podemos incluir el Orden con esta cantidad de datos, se obvia entonces. De todas formas el AIC era muy alto
# se puede reportar justamente eso
datos
full_sinorden <-
  glmer(Despl_total ~ Tratamiento + Tamano + 1 | Ony ,
        family = "binomial",
        data = datos)

tratamiento <-
  glmer(Despl_total ~ Tratamiento + 1 | Ony ,
        family = "binomial",
        data = datos)

anova(full_sinorden)
summary(full_sinorden)
 <-
  glmer(Despl_total ~ Tratamiento + Tamano + 1 | Ony ,
        family = "binomial",
        data = datos)
summary((sinorden))
sin_ordenonytamano_tratamiento3cats <-
  glmer(Despl_total ~ Tratamiento_3cats + Tamano + 1|Ony,
      family = "binomial",
      data = datos)

nulo <-
  glm(Despl_total ~ 1,
      family = "binomial",
      data = datos)

# Pa Jorge: Tabla1 columnas: modelo (nombre), formula, y estos otros:
# AIC       BIC    logLik  deviance  df.resid
# 258.6371  366.9977 -100.3186  200.6371       281

AIC(
  full,
  sinorden,
  sin_ordenony,
  sin_ordenonytamano,
  sin_ordenonytamano_tratamiento3cats,
  nulo
)
summary(sin_ordenonytamano_tratamiento3cats)

lsmeans(sin_ordenonytamano_tratamiento3cats,
        pairwise ~ Tratamiento_3cats ,
        adjust = "Tukey")

head(datos)
graf_despl<- ggplot(datos)
graf_despl<- graf_despl + aes(x=Tratamiento_3cats, y=Despl_total, color=Tratamiento_3cats)
graf_despl<- graf_despl + geom_boxplot()
graf_despl


dat<- summarySE(datos, measurevar = "Despl_total", groupvars = "Tratamiento_3cats")
ptos<- ggplot(dat, aes(x=Tratamiento_3cats, y= Despl_total)) +
  geom_point(size= 3, )
graf_desplTotal <- ptos +  geom_errorbar(aes(ymin=Despl_total-ci, ymax=Despl_total+ci), width = 0.2) +  
  ylab("Tiempo invertido (%)")  + theme_classic() + xlab("Tratamientos" )


# CERCANIA -----

datos_cercania <- datos %>%
  filter(!is.na(Cerca_total))

cerca_full <-
  glmer(Cerca_total ~ Tratamiento + Tamano + Orden + 1 | Ony,
        family = "binomial",
        data = datos_cercania)

cerca_sinorden <-
  glmer(Cerca_total ~ Tratamiento + Tamano + 1 | Ony ,
        family = "binomial",
        data = datos_cercania)

cerca_sin_ordenony <-
  glm(Cerca_total ~ Tratamiento + Tamano ,
      family = "binomial",
      data = datos_cercania)
df.residual(cerca_sin_ordenony)

cerca_sin_ordenonytamano <-
  glm(Cerca_total ~ Tratamiento ,
      family = "binomial",
      data = datos_cercania)

cerca_sin_ordenonytamano_tratamiento3cats <-
  glm(Cerca_total ~ Tratamiento_3cats ,
      family = "binomial",
      data = datos_cercania)

cerca_nulo <-
  glm(Cerca_total ~ 1,
      family = "binomial",
      data = datos_cercania)

# cerca_sin_ordenony SUGIERE QUE HAGAMOS ESTO
cerca_soloTamano <-
  glm(Cerca_total ~ Tamano ,
      family = "binomial",
      data = datos_cercania)

# AICs

AIC(
  cerca_full,
  cerca_sinorden,
  cerca_sin_ordenony,
  cerca_sin_ordenonytamano,
  cerca_sin_ordenonytamano_tratamiento3cats,
  cerca_nulo,
  cerca_soloTamano
)

summary(cerca_soloTamano)

head(datos_cercania)
graf_ceracania<- ggplot(datos_cercania)
graf_ceracania<- graf_ceracania + aes(x=Tamano, y=jaula_cerca)
graf_ceracania<- graf_ceracania + geom_point()
graf_ceracania<- graf_ceracania + geom_smooth(method = "glm", method.args = list(family = "binomial"),se = T)
graf_ceracania<- graf_ceracania + ylab("Tiempo invertido (%)")  + theme_classic() + xlab("Tamaño corporal (mm)" )
graf_ceracania

# con tiempo de jaula
jaula_cerca_full <-
  glmer( jaula_cerca~ Tratamiento + Tamano + Orden + 1 | Ony,
        family = "binomial",
        data = datos_cercania)

jaula_cerca_sinorden <-
  glmer(jaula_cerca ~ Tratamiento + Tamano + 1 | Ony ,
        family = "binomial",
        data = datos_cercania)

jaula_cerca_sin_ordenony <-
  glm(jaula_cerca ~ Tratamiento + Tamano ,
      family = "binomial",
      data = datos_cercania)

jaula_cerca_sin_ordenonytamano <-
  glm(jaula_cerca ~ Tratamiento ,
      family = "binomial",
      data = datos_cercania)

jaula_cerca_sin_ordenonytamano_tratamiento3cats <-
  glm(jaula_cerca ~ Tratamiento_3cats,
      family = "binomial",
      data = datos_cercania)

jaula_cerca_nulo <-
  glm(jaula_cerca ~ 1,
      family = "binomial",
      data = datos_cercania)

jaula_cerca_soloTamano <-
  glm(jaula_cerca ~ Tamano,
      family = "binomial",
      data = datos_cercania)

AIC(
  jaula_cerca_full,
  jaula_cerca_sinorden,
  jaula_cerca_sin_ordenony, ## mejor modelo
  jaula_cerca_sin_ordenonytamano,
  jaula_cerca_sin_ordenonytamano_tratamiento3cats,
  jaula_cerca_nulo,
  jaula_cerca_soloTamano
)


# conclusion ----

# Desplazamiento: 
# 1. Ex tiene diferencia con Neo y Dep
# 2. no hay diferencias entre depredaor y neofilia
# contrast  estimate    SE  df z.rati0.0306o p.value
# Dep - Ex     -1.23 0.337 Inf -3.649  0.0008 
# Dep - Neo    -0.14 0.395 Inf -0.355  0.9329 
# Ex - Neo      1.09 0.430 Inf  2.531  

#  Cercania:
# 3. El mejor modelo es el que solo tiene a Tamano como predictor
# a mayor tamano menor porcentaje invertido cerca del estimulo, tanto jaula simple o depredador
# lo que determina que tanto tiempo esta cerca es el tamano, no el tratamiento
# esto sugiere que seria interesante estudiar por que los jovenes son mas jachudos
# Todavia tenemos que asegurarnos de entender esto: los jovenes son mas jachudos o los viejos???

# Rebitabilidad falta