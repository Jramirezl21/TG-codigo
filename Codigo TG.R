#Librerias ----
library(whomds)
library(EFAtools)
library(psych)
library(haven)
library(polycor)
library(ggcorrplot)
library(GPArotation)
library(psych)
library(lavaan)
library(polycor)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(ade4)
library(FactoMineR)
library(missMDA)
library(dplyr)
df <- df_adults
# BASE DE DATOS ----
ruta_del_archivo <- "D:/jorge/Semestre 9/TG 1/TG codigo/Base de Datos II Estudio Nacional de la Discapacidad.sav"
data <- read_sav(ruta_del_archivo)





dffiltrado <- subset(data, edad >= 18)

datosfil <- dffiltrado[,c(3,8,14,15,116:139,68:114,309:320)]


datosfilt <- data.frame(lapply(datosfil, function(x) if(is.labelled(x)) as_factor(x) else x))
datosfil1 <- data.frame(lapply(data_fil2, function(x) if(is.labelled(x)) as.numeric(x) else x))
# Primero, identificamos las filas que tienen valores mayores que 5 en las columnas de interés

# Lista de valores a excluir
valores_excluir <- c("No sabe", "Sin dato", "No responde")
valores_excluir2 <- c(88,96,99)
# Filtrar las filas que no contienen los valores deseados en las variables no generales
data_fil <- datosfilt %>%
  filter(
    across(-c(edad, sexo, region), ~ !(. %in% valores_excluir))
  )

data_fil2 <- datosfil %>%
  filter(
    across(-c(edad, sexo, region), ~ !(. %in% valores_excluir2))
  )
#Hay gente que no respondio na


cantidad_na <- rowSums(is.na(data_fil2))
Datos.num<- data_fil2 %>%
  filter(cantidad_na < 83)

Datos.cat <- data_fil %>%
  filter(cantidad_na < 83)
Datos.cat <- droplevels(Datos.cat)

wwwwwwwwwwwwwwwwwwwwwwwwwwwwws<-  data.frame(lapply(Datos.num, function(x) if(is.labelled(x)) as.numeric(x) else x))

cantidad_na2 <- rowSums(is.na(Datos.num))
colSums(is.na(Datos.num))

#REAGRUPACION CAPACIDAD----
#8, 14,1
Datos.num.cap_reagrupado <- dplyr::select(Datos.num,c2,c3,c5,c15,c19,c20,c21,c23,c6,
                               c7,c8,c9,c10,c11,c12,c13,c14,c16,c17,c18,c22,c24,c4)
Datos.cat.cap_reagrupado <- dplyr::select(Datos.cat,c2,c3,c5,c15,c19,c20,c21,c23,c6,
                                          c7,c8,c9,c10,c11,c12,c13,c14,c16,c17,c18,c22,c24
                                          ,c4)
#8,13,1
cap_reagrupado_NoNA_num <- dplyr::select(Datos.num,c2,c3,c5,c15,c19,c20,c21,c23,c6,
                                          c7,c8,c9,c10,c11,c12,c13,c14,c16,c17,c18,c22,c4)
cap_reagrupado_NoNA_cat <- dplyr::select(Datos.cat,c2,c3,c5,c15,c19,c20,c21,c23,c6,
                                          c7,c8,c9,c10,c11,c12,c13,c14,c16,c17,c18,c22,c4)
#REAGRUPACION DESEMPEÑO
#######
#REAGRUPACION DESEMPEÑO ----

#12,34
Datos.num.de_reagrupado <- dplyr::select(Datos.num,d15,d16,d17,d18,d19,d21,d22,d23,d24,d25,d34,d35,
                                         d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, 
                                         d14, d20, d26, d27, d28, d29, d30, d31, d32, d33, d36, d37, 
                                         d38, d39, d40, d41, d42, d43, d44, d45, d46)
Datos.cat.de_reagrupado <- dplyr::select(Datos.cat,d15,d16,d17,d18,d19,d21,d22,d23,d24,d25,d34,d35,
                                         d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, 
                                         d14, d20, d26, d27, d28, d29, d30, d31, d32, d33, d36, d37, 
                                         d38, d39, d40, d41, d42, d43, d44, d45, d46)
#12,33
de_reagrupado_NoNA_num <- dplyr::select(Datos.num,d15,d16,d17,d18,d19,d21,d22,d23,d24,d25,d34,d35,
                                        d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, 
                                        d14, d20, d26, d27, d28, d29, d30, d31, d32, d33, d36, d37, 
                                        d38, d39, d40, d41, d42, d43, d44, d45)
de.cat_reagrupado_NoNA_num <- dplyr::select(Datos.cat,d15,d16,d17,d18,d19,d21,d22,d23,d24,d25,d34,d35,
                                            d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, 
                                            d14, d20, d26, d27, d28, d29, d30, d31, d32, d33, d36, d37, 
                                            d38, d39, d40, d41, d42, d43, d44, d45)
#REAGRUPACION FACTORES AMBIENTALES----
#11,12
#7,1,1,1
Datos.num.fa_reagrupado<- dplyr::select(Datos.num,fa1,fa3,fa6,fa7,fa8,fa9,fa10,fa2,fa11,fa4,fa5)
Datos.cat.fa_reagrupado <- dplyr::select(Datos.cat,fa1,fa3,fa6,fa7,fa8,fa9,fa10,fa2,fa11,fa4,fa5)
#7,2,1,1

fa_reagrupado_NoNA_num <- dplyr::select(Datos.num,fa1,fa3,fa6,fa7,fa8,fa9,fa10,fa2,fa4,fa5)
fa_reagrupado_NoNA_cat <- dplyr::select(Datos.cat,fa1,fa3,fa6,fa7,fa8,fa9,fa10,fa2,fa4,fa5)


#Correr un EFA sin trabajo y sin educacion
#Y otro sin educacion
#De los adultos es mas importante la parte laboral que la 
#Hacer agrupaciones base para el EFA
#Codigo de clasificacion internacional de funcionalidad

# DESCRIPTIVAS ----
###ORDEN DE CAPACIDAD####
#CAPACIDAD
par(mfrow = c(2, 2))
boxplot(Datos.num$edad~Datos.num$c2,ylab = 'Edad', xlab='Ver, sin anteojos ópticos o lentes?') ; table(Datos.num$c2)
boxplot(Datos.num$edad~Datos.num$c3,ylab = 'Edad', xlab='Oír, sin dispositivo de ayuda para oír o audífono?') ; table(Datos.num$c3)
boxplot(Datos.num$edad~Datos.num$c4,ylab = 'Edad', xlab='Caminar o subir peldaños?') ; table(Datos.num$c4)
boxplot(Datos.num$edad~Datos.num$c5,ylab = 'Edad', xlab='Recordar cosas o concentrarse?') ; table(Datos.num$c5)
par(mfrow = c(2, 2))
boxplot(Datos.num$edad~Datos.num$c6,ylab = 'Edad', xlab='Asearse o vestirse?') ; table(Datos.num$c6)
boxplot(Datos.num$edad~Datos.num$c7,ylab = 'Edad', xlab='Comunicarse, por ejemplo comprender o ser entendido usando su lenguaje habitual?') ; table(Datos.num$c7)
boxplot(Datos.num$edad~Datos.num$c8,ylab = 'Edad', xlab='alimentarse?') ; table(Datos.num$c8)
boxplot(Datos.num$edad~Datos.num$c9,ylab = 'Edad', xlab='utilizar el baño?') ; table(Datos.num$c9)
par(mfrow = c(2, 2))
boxplot(Datos.num$edad~Datos.num$c10,ylab = 'Edad', xlab='acostarse o levantarse de la cama?') ; table(Datos.numl$c10)
boxplot(Datos.num$edad~Datos.num$c11,ylab = 'Edad', xlab='salir a la calle?') ; table(Datos.numl$c11)
boxplot(Datos.num$edad~Datos.num$c12,ylab = 'Edad', xlab='ir de compras o ir al medico?') ; table(Datos.numl$c12)
boxplot(Datos.num$edad~Datos.num$c13,ylab = 'Edad', xlab='Manipular objetos pequeños o abrir un envase?') ; table(Datos.numl$c13)
par(mfrow = c(2, 2))
boxplot(Datos.num$edad~Datos.num$c14, ylab = 'Edad', xlab='dormir?') ; table(Datos.numl$c14)
boxplot(Datos.num$edad~Datos.num$c15,ylab = 'Edad', xlab='Respirar?') ; table(Datos.numl$c15)
boxplot(Datos.num$edad~Datos.num$c16,ylab = 'Edad', xlab='Hacer las tareas de la casa como barrer, cocinar, hacer arreglos o sacar la basura?') ; table(Datos.numl$c16)
boxplot(Datos.num$edad~Datos.num$c17,ylab = 'Edad', xlab='Cuidar o dar apoyo a otros?')
par(mfrow = c(2, 2))
boxplot(Datos.num$edad~Datos.num$c18,ylab = 'Edad', xlab='Participar en fiestas, eventos religiosos, reuniones vecinales u otras actividades comunales?') ; table(Datos.numl$c18)
boxplot(Datos.num$edad~Datos.num$c19,ylab = 'Edad', xlab='Sentirse triste, bajo/a de ánimo, o deprimido/a?') ; table(datosfil$c19)
boxplot(Datos.num$edad~Datos.num$c20,ylab = 'Edad', xlab='Sentirse preocupado/a, nervioso/a o ansioso/a?') ; table(datosfil$c20)
boxplot(Datos.num$edad~Datos.num$c21,ylab = 'Edad', xlab='Llevarse bien con la gente cercana a usted, familia o amigos?'); table(datosfil$c21)
par(mfrow = c(1, 2))
boxplot(Datos.num$edad~Datos.num$c22,ylab = 'Edad', xlab='Sentir algún dolor físico, por ejemplo dolor de espalda, dolor de estómago o dolor de cabeza?') ; table(datosfil$c22)
boxplot(Datos.num$edad~Datos.num$c23,ylab = 'Edad', xlab='Sentir algún dolor físico, por ejemplo dolor de espalda, dolor de estómago o dolor de cabeza?') ; table(datosfil$c23)




###ORDEN DE DESEMPEÑO####
#DESEMPEÑO 
par(mfrow = c(2, 2))
boxplot(Datos.num$edad~Datos.num$d1,ylab = 'Edad', xlab='Ponerse de pie estando sentado/a?') ; table(Datos.num$d1)
plot(Datos.num$edad~datosfil$d2, ylab = 'Edad', xlab='Estar de pie durante largos períodos de tiempo, como por ejemplo 30 minutos?') ; table(Datos.num$d2)
plot(Datos.num$edad~Datos.num$d4,ylab = 'Edad', xlab='Caminar distancias cortas tales como una cuadra o 100 metros?') ; table(Datos.num$d4)
plot(Datos.num$edad~Datos.num$d5,ylab = 'Edad',xlab = 'Caminar diez cuadras o un kilómetro?') ; table(Datos.num$d5)
par(mfrow = c(2, 2))
plot(Datos.num$edad~Datos.num$d6,ylab = 'Edad', xlab='Realizar actividades físicamente exigentes, como jugar fútbol, levantar objetos pesados,andar en bicicleta o correr?'); table(Datos.num$d6) 
plot(Datos.num$edad~Datos.num$d7, ylab = 'Edad', xlab='Llegar a los lugares donde ha querido ir?') ; table(Datos.num$d7)
plot(Datos.num$edad~Datos.num$d8,ylab = 'Edad', xlab='Manipular objetos pequeños o abrir un envase?') ; table(Datos.num$d8)
plot(Datos.num$edad~Datos.num$d9,ylab = 'Edad',xlab = 'Levantar una botella de agua de dos litros llena, desde la cintura hasta el nivel de los ojos?') ; table(Datos.num$d9)
par(mfrow = c(2, 2))
plot(Datos.num$edad~Datos.num$d10,ylab = 'Edad', xlab=' Asearse o vestirse?'); table(Datos.num$d10) 
plot(Datos.num$edad~Datos.numl$d11, ylab = 'Edad', xlab='Alimentarse?') ; table(Datos.num$d11)
plot(Datos.num$edad~Datos.num$d12,ylab = 'Edad', xlab=' Utilizar el baño (W.C.)?') ; table(Datos.num$d12)
plot(Datos.num$edad~Datos.num$d13,ylab = 'Edad', xlab='Cortarse las uñas de los pies?') ; table(Datos.num$d13)
par(mfrow = c(2, 2))
plot(Datos.num$edad~Datos.num$d14,ylab = 'Edad', xlab='Cuidar de su salud?') ; table(Datos.num$d14)
plot(Datos.num$edad~Datos.num$d15,ylab = 'Edad', xlab='Ver objetos de lejos?') ; table(Datos.num$d15)
plot(Datos.num$edad~Datos.num$d16,ylab = 'Edad', xlab='Ver objetos de cerca?') ; table(Datos.num$d16)
plot(Datos.num$edad~Datos.num$d17,ylab = 'Edad', xlab='Oír una conversación en un lugar silencioso?') ; table(Datos.num$d17)
par(mfrow = c(2, 2))
plot(Datos.num$edad~Datos.num$d18,ylab = 'Edad', xlab=' Oír una conversación en una habitación con ruido alrededor?') ; table(Datos.num$d18)
plot(Datos.num$edad~Datos.num$d19,ylab = 'Edad', xlab='Sentir algún dolor físico, por ejemplo dolor de espalda, dolor de estómago o dolor de cabeza?') ; table(Datos.num$d19)
plot(Datos.num$edad~Datos.num$d20,ylab = 'Edad', xlab='Dormir?') ; table(Datos.num$d20)
plot(Datos.num$edad~Datos.num$d21,ylab = 'Edad',xlab = 'Sentirse cansado y no tener suficiente energía?') ; table(Datos.num$d21)
par(mfrow = c(2, 2))
plot(Datos.num$edad~Datos.num$d22,ylab = 'Edad',xlab = 'Sentir falta de aire?') ; table(Datos.num$d22)
plot(Datos.num$edad~Datos.num$d23,ylab = 'Edad',xlab = 'Toser o respirar con dificultad?') ; table(Datos.num$d23)
plot(Datos.num$edad~Datos.num$d24,ylab = 'Edad',xlab = 'Sentirse triste, bajo de ánimo o deprimido/a?') ; table(Datos.num$d24)
plot(Datos.num$edad~Datos.num$d25,ylab = 'Edad',xlab = 'Sentir preocupación, nerviosismo o ansiedad? d26 Llevarse bien con personas cercanas, incluyendo su familia y amigos?') ; table(Datos.num$d25)
par(mfrow = c(2, 2))
plot(Datos.num$edad~Datos.num$d26,ylab = 'Edad',xlab = '') ; table(Datos.num$d26)
plot(Datos.num$edad~Datos.num$d27,ylab = 'Edad',xlab = 'Relacionarse con personas que no conoce?') ; table(Datos.num$d27)
plot(Datos.num$edad~Datos.num$d28,ylab = 'Edad',xlab = 'Hacer nuevas amistades o mantener las actuales?') ; table(Datos.num$d28)
plot(Datos.num$edad~Datos.num$d29,ylab = 'Edad',xlab = 'Tener una relación de pareja?') ; table(Datos.num$d29)
par(mfrow = c(2, 2))
plot(Datos.num$edad~Datos.num$d30,ylab = 'Edad',xlab = 'Manejar el estrés?') ; table(Datos.num$d30)
plot(Datos.num$edad~Datos.num$d31,ylab = 'Edad',xlab = 'Enfrentar todas las tareas que tiene que hacer?') ; table(Datos.num$d31)
plot(Datos.num$edad~Datos.num$d32,ylab = 'Edad',xlab = 'Ser entendido, utilizando su lenguaje habitual?') ; table(Datos.num$d32)
plot(Datos.num$edad~Datos.num$d33,ylab = 'Edad',xlab = 'Entender a otros, utilizando su lenguaje habitual?') ; table(Datos.num$d33)
par(mfrow = c(2, 2))
plot(Datos.num$edad~Datos.num$d34,ylab = 'Edad',xlab = 'Olvidar cosas/tener mala memoria?') ; table(Datos.num$d34)
plot(Datos.num$edad~Datos.num$d35,ylab = 'Edad',xlab = 'Recordar las cosas importantes que tiene que hacer en su día a día?') ; table(Datos.num$d35)
plot(Datos.num$edad~Datos.num$d36,ylab = 'Edad',xlab = 'Encontrar soluciones a los problemas del día a día?') ; table(Datos.num$d36)
plot(Datos.num$edad~Datos.num$d37,ylab = 'Edad',xlab = 'Completar las tareas de la casa como barrer, cocinar, hacer arreglos o sacar la basura?') ; table(Datos.num$d37)
par(mfrow = c(2, 2))
plot(Datos.num$edad~Datos.num$d38,ylab = 'Edad',xlab = 'Administrar el dinero que posee?') ; table(Datos.num$d38)
plot(Datos.num$edad~Datos.num$d39,ylab = 'Edad',xlab = 'Hacer cosas que lo relajen o que lo hagan disfrutar?') ; table(Datos.num$d39)
plot(Datos.num$edad~Datos.num$d40,ylab = 'Edad',xlab = 'Participar en fiestas, eventos religiosos, reuniones vecinales u otras actividades comunales?') ; table(Datos.num$d40)
plot(Datos.num$edad~Datos.num$d41,ylab = 'Edad',xlab = 'Participar en la política local o nacional y en organizaciones de la sociedad civil?') ; table(Datos.num$d41)
par(mfrow = c(2, 2))
plot(Datos.num$edad~Datos.num$d42,ylab = 'Edad',xlab = 'Cuidar o dar apoyo a otros?') ; table(Datos.num$d42)
plot(Datos.num$edad~Datos.num$d43,ylab = 'Edad',xlab = 'Postular o conseguir un empleo?') ; table(Datos.num$d43)
plot(Datos.num$edad~Datos.num$d44,ylab = 'Edad',xlab = 'Acceder a una educación superior? Ejemplos: CFT, IP, Universidad') ; table(Datos.num$d44)
plot(Datos.num$edad~Datos.num$d45,ylab = 'Edad',xlab = 'Utilizar los servicios de transporte público?') ; table(Datos.num$d45)
par(mfrow = c(1, 2))
plot(Datos.num$edad~Datos.num$d46,ylab = 'Edad',xlab = 'Realizar las tareas que le solicitan en su empleo?') ; table(Datos.num$d46)
plot(Datos.num$edad~Datos.num$d47,ylab = 'Edad',xlab = 'Realizar las tareas que le solicitan en su establecimiento educacional?') ; table(Datos.num$d47)


###FACTORES AMBIENTALES----
par(mfrow = c(2, 2))
plot(Datos.num$edad~Datos.num$fa1,ylab = 'Edad',xlab='fa1:le resulta fácil o difícil utilizar los servicios sanitarios que necesita con regularidad?',cex=0.02) ; table(Datos.num$fa1)
plot(Datos.num$edad~Datos.num$fa2,ylab = 'Edad',xlab='fa2:¿le resultan fáciles o difíciles los lugares donde se relaciona y participa en actividades comunitarias?',cex=0.02) ; table(Datos.num$fa2)
plot(Datos.num$edad~Datos.num$fa3,ylab = 'Edad',xlab='fa3:¿le resulta fácil o difícil utilizar las tiendas, bancos y oficinas de correos de su barrio?',cex=0.02) ; table(Datos.num$fa3)
plot(Datos.num$edad~Datos.num$fa4,ylab = 'Edad',xlab='fa4:¿le resulta fácil o difícil practicar su religión en los lugares de culto que frecuenta habitualmente?
',cex=0.02) ; table(Datos.num$fa4)
plot(Datos.num$edad~Datos.num$fa5,ylab = 'Edad',xlab='fa5:los medios de transporte que necesita o desea utilizar, ¿le resultan fáciles o difíciles de utilizar?',cex=0.02) ; table(Datos.num$fa5)
plot(Datos.num$edad~Datos.num$fa6,ylab = 'Edad',xlab='fa6:¿le resulta fácil o difícil vivir en su vivienda (incluido el aseo y todas las habitaciones)?') ; table(Datos.num$fa6)
plot(Datos.num$edad~Datos.num$fa7,ylab = 'Edad',xlab='fa7:¿le facilitan o dificultan la vida la temperatura, el terreno y el clima del lugar donde vive habitualmente?') ; table(Datos.num$fa7)
plot(Datos.num$edad~Datos.num$fa8,ylab = 'Edad',xlab='fa8:¿la iluminación de su entorno le facilita o le dificulta la vida?') ; table(Datos.num$fa8)
plot(Datos.num$edad~Datos.num$fa9,ylab = 'Edad',xlab='fa9:¿el ruido de su entorno le facilita o le dificulta vivir en él?') ; table(Datos.num$fa9)
plot(Datos.num$edad~Datos.num$fa10,ylab = 'Edad',xlab='fa10:¿la aglomeración de gente en su entorno le facilita o le dificulta vivir allí?') ; table(Datos.num$fa10)
plot(Datos.num$edad~Datos.num$fa11,ylab = 'Edad',xlab='fa11:¿le resulta fácil o difícil trabajar o aprender en su lugar de trabajo?') ; table(Datos.num$fa11)
plot(Datos.num$edad~Datos.num$fa12,ylab = 'Edad',xlab='fa12:¿le resulta fácil o difícil trabajar o aprender en su centro de estudios?') ; table(Datos.num$fa12)
# ANALISIS FACTORIAL ----

colSums(is.na(df))
numero_filas_completas <- sum(complete.cases(df))

##Analisis Factorial exploratorio ----
RIncom <- cor(Incom_NoNA[,c(5:81)])
KMO(RIncom)
scree(RIncom)
fa.parallel(RIncom, fm="ml")
EFAIncom   <- fa(RIncom, nfactors = 3, rotate = "varimax",
             fm="ml")
print(EFAIncom)
biplot.psych(fa(Incom_NoNA[,c(5:81)],nfactors = 3,fm="ml",rotate = "varimax"),
             main = paste("Biplot con rotación varimax"),col=c(2,3,4),
             pch = c(21,18))
fa.diagram(EFAIncom )
EFAIncom$weights
colnames(EFAIncom$weights)[apply(EFAIncom$weights, 1, which.max)]

RCom <- cor(Datos.num.comple[,c(5:87)])
KMO(RCom)
fa.parallel(RCom , fm="ml")
EFACom   <- fa(RCom, nfactors = 3, rotate = "varimax",
                 fm="ml")
EFACom$weights
fa.diagram(EFACom )
fa.graph(EFACom )
colnames(EFACom$weights)[apply(EFACom$weights, 1, which.max)]

RCom <- cor(Datos.num.comple[,c(5:87)])
fa.parallel(RCom, fm="ml")
EFACom   <- fa(RIncom, nfactors = 3, rotate = "varimax",
                 fm="ml")


## ANALISIS FACTORIAL CONFIRMATORIO ----
modeloIncom<-"capacidad =~ c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10 + c11 + c12 + c13 + c14 + c15 + c16 + c17 + c18 + c19 + c20 + c21 + c22 + c23
desempeno =~ d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12 +
d13 + d14 + d15 + d16 + d17 + d18 + d19 + d20 + d21 + d22 + d23 + d24 + 
d25 + d26 + d27 + d28 + d29 + d30 + d31 + d32 + d33 + d34 + d35 + d36 + d37 
+ d38 + d39 + d40 + d41 + d42 + d43 + d44 + d45
factores_ambientales =~ fa1 + fa2 + fa3 + fa4 + fa5 + fa6 + fa7 + fa8 + fa9 + fa10
"

fitIncom<-cfa(model= modeloIncom,
         data = Incom_NoNA[,c(5:81)], orthogonal=T,ordered = F)
semPlot::semPaths(fitIncom,nCharNodes = 0,intercepts = F,edge.label.cex = 1,
         edge.color = "black",sizeMan = standardizedSolution(fitIncom)$loading * 10,label.prop=0.45,sizeLat = 2,"std",layout = "circle",exoVar = T)
semPlot::semPaths(fitIncom, "std",layout = "circle")
parameterEstimates(fitIncom, standardized = TRUE)
summary(fitIncom, standardized = TRUE, fit.measures = TRUE)
plot(fitIncom)

modeloCom<-"capacidad =~ c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10 + c11 + c12 + c13 + c14 + c15 + c16 + c17 + c18 + c19 + c20 + c21 + c22 + c23 + c24 + c25
desempeno =~ d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12 +
d13 + d14 + d15 + d16 + d17 + d18 + d19 + d20 + d21 + d22 + d23 + d24 + 
d25 + d26 + d27 + d28 + d29 + d30 + d31 + d32 + d33 + d34 + d35 + d36 + d37 
+ d38 + d39 + d40 + d41 + d42 + d43 + d44 + d45 + d46 + d47
factores_ambientales =~ fa1 + fa2 + fa3 + fa4 + fa5 + fa6 + fa7 + fa8 + fa9 + fa10 + fa11 + fa12
"

fitCom<-cfa(model= modeloCom,
              data = Datos.num.comple[,c(5:87)], orthogonal=T,ordered = F)
semPlot::semPaths(fitCom, "std",layout = "circle")
########## AFM con las variables seleccionadas por AFC para incompletos
Datos.num.incomp
#Preguntar al profe por la c7 0.59 cf
capIncomSelect_NoNA <- dplyr::select(Datos.num.incomp,c6,
                               c8,c9,c10,c11,c12,c16,c17,c18,c22,c4,c5)
MFAincompSelectCap <- MFA(capIncomSelect_NoNA,group=c(10,1,1))
fviz_mfa_var(MFAincompSelectCap )
fviz_mfa_ind(MFAincompSelectCap)

deIncomSelect_NoNA <- dplyr::select(Datos.num.incomp,d21,
                              d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, 
                              d14, d31, d36, d37, d42, d43, d45)
MFAincompSelectDe <- MFA(deIncomSelect_NoNA,group=c(1,20))
fviz_mfa_var(MFAincompSelectDe)
fviz_mfa_ind(MFAincompSelectDe)

faIncomSelect_NoNA <- dplyr::select(Datos.num.incomp,fa3,fa6,fa7,fa2,fa4)
MFAincompSelectFa <- MFA(faIncomSelect_NoNA,group =c(3,1,1))
fviz_mfa_var(MFAincompSelectFa)
fviz_mfa_ind(MFAincompSelectFa)

########## AFM con las variables seleccionadas por AFC para completos
Datos.num.comple <- na.omit(Datos.num)

capSelectCom<- dplyr::select(Datos.num.comple,c6,
                      c10,c11,c12,c16,c17,c22,c24,c25,c4)
MFAcompSelectCap <- MFA(capSelectCom,group=c(9,1))
fviz_mfa_var(MFAcompCap)

deSelectCom <- dplyr::select(Datos.num.comple,d18,d19,d21,d24,d25,d35,
                       d1, d2, d3, d4, d5, d6, d7, d20, d26, d27,
                       d29, d30, d36)
MFAcompSelectDe <- MFA(deSelectCom,group=c(6,13))
fviz_mfa_var(MFAcompSelectDe )
fviz_mfa_ind(MFAcompSelectDe )


faSelectCom<- dplyr::select(Datos.num.comple,fa3,fa7,fa9,fa10,fa11,fa2,fa4)
MFAcompSelecFa <- MFA(faSelectCom,group=c(4,2,1))
fviz_mfa_var(MFAcompSelecFa)
fviz_mfa_ind(MFAcompSelecFa)
###### Indicicadores ######
#Incompletos
T1.capIncomp <- MFAincompSelectCap$ind$coord[,1]
T1.deIncomp <- MFAincompSelectDe$ind$coord[,1]
T1.faIncomp <- MFAincompSelectFa$ind$coord[,1]
MFAincompSelectCap$eig[1,1]+MFAincompSelectDe$eig[1,1]+MFAincompSelectFa$eig[1,1]

MFAincompSelectCap$eig[1,1]/5.758771
MFAincompSelectDe$eig[1,1]/5.758771
MFAincompSelectFa$eig[1,1]/5.758771
I_1Select <- (MFAincompSelectCap$ind$coord[,1]*MFAincompSelectCap$eig[1,1]/5.758771)+
  (MFAincompSelectDe$ind$coord[,1]*MFAincompSelectDe$eig[1,1]/5.758771)+
  (MFAincompSelectFa$ind$coord[,1]*MFAincompSelectFa$eig[1,1]/5.758771)

Puntaje.I_1 <- 100-((I_1Select - min(I_1Select))*100/(max(I_1Select)-min(I_1Select)))
Puntaje.df <- data.frame(Puntaje.I_1)
dfedad <- data.frame(Puntaje.I_1,Datos.num.incomp$edad,Datos.num.incomp$sexo)
#Completos
T2.capcomp <- MFAcompSelectCap$ind$coord[,1]
T2.decomp <- MFAcompSelectDe$ind$coord[,1]
T2.facomp <- MFAcompSelecFa$ind$coord[,1]
MFAcompSelectCap$eig[1,1]+MFAcompSelectDe$eig[1,1]+MFAcompSelecFa$eig[1,1]

MFAcompSelectCap$eig[1,1]/5.613846
MFAcompSelectDe$eig[1,1]/5.613846
MFAcompSelecFa$eig[1,1]/5.613846
I_2Select <- (T2.capcomp*MFAcompSelectCap$eig[1,1]/5.613846)+
  (T2.decomp*MFAcompSelectDe$eig[1,1]/5.613846)+
  (T2.facomp*MFAcompSelecFa$eig[1,1]/5.613846)

Puntaje.I_2 <- 100-((I_2Select - min(I_2Select))*100/(max(I_2Select)-min(I_2Select)))
Puntaje2.df <- data.frame(Puntaje.I_2)
dfedad <- data.frame(Puntaje.I_2,Datos.num.incomp$edad,Datos.num.incomp$sexo)

