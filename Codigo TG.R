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

Datos.num <-  data.frame(lapply(Datos.num, function(x) if(is.labelled(x)) as.numeric(x) else x))

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
boxplot(Datos.num$edad~Datos.numl$c4,ylab = 'Edad', xlab='Caminar o subir peldaños?') ; table(Datos.num$c4)
boxplot(Datos.num$edad~Datos.numl$c5,ylab = 'Edad', xlab='Recordar cosas o concentrarse?') ; table(Datos.num$c5)
par(mfrow = c(2, 2))
boxplot(Datos.num$edad~Datos.numl$c6,ylab = 'Edad', xlab='Asearse o vestirse?') ; table(Datos.num$c6)
boxplot(Datos.num$edad~Datos.numl$c7,ylab = 'Edad', xlab='Comunicarse, por ejemplo comprender o ser entendido usando su lenguaje habitual?') ; table(Datos.num$c7)
boxplot(Datos.num$edad~Datos.numl$c8,ylab = 'Edad', xlab='alimentarse?') ; table(Datos.num$c8)
boxplot(Datos.num$edad~Datos.numl$c9,ylab = 'Edad', xlab='utilizar el baño?') ; table(Datos.num$c9)
par(mfrow = c(2, 2))
boxplot(Datos.num$edad~Datos.numl$c10,ylab = 'Edad', xlab='acostarse o levantarse de la cama?') ; table(Datos.numl$c10)
boxplot(Datos.num$edad~Datos.numl$c11,ylab = 'Edad', xlab='salir a la calle?') ; table(Datos.numl$c11)
boxplot(Datos.num$edad~Datos.numl$c12,ylab = 'Edad', xlab='ir de compras o ir al medico?') ; table(Datos.numl$c12)
boxplot(Datos.num$edad~Datos.numl$c13,ylab = 'Edad', xlab='Manipular objetos pequeños o abrir un envase?') ; table(Datos.numl$c13)
par(mfrow = c(2, 2))
boxplot(Datos.num$edad~Datos.numl$c14, ylab = 'Edad', xlab='dormir?') ; table(Datos.numl$c14)
boxplot(Datos.num$edad~Datos.numl$c15,ylab = 'Edad', xlab='Respirar?') ; table(Datos.numl$c15)
boxplot(Datos.num$edad~Datos.numl$c16,ylab = 'Edad', xlab='Hacer las tareas de la casa como barrer, cocinar, hacer arreglos o sacar la basura?') ; table(Datos.numl$c16)
boxplot(Datos.num$edad~Datos.numl$c17,ylab = 'Edad', xlab='Cuidar o dar apoyo a otros?')
par(mfrow = c(2, 2))
boxplot(Datos.num$edad~Datos.numl$c18,ylab = 'Edad', xlab='Participar en fiestas, eventos religiosos, reuniones vecinales u otras actividades comunales?') ; table(Datos.numl$c18)
boxplot(Datos.num$edad~Datos.numl$c19,ylab = 'Edad', xlab='Sentirse triste, bajo/a de ánimo, o deprimido/a?') ; table(datosfil$c19)
boxplot(Datos.num$edad~Datos.numl$c20,ylab = 'Edad', xlab='Sentirse preocupado/a, nervioso/a o ansioso/a?') ; table(datosfil$c20)
boxplot(Datos.num$edad~Datos.numl$c21,ylab = 'Edad', xlab='Llevarse bien con la gente cercana a usted, familia o amigos?'); table(datosfil$c21)
par(mfrow = c(1, 2))
boxplot(Datos.num$edad~Datos.numl$c22,ylab = 'Edad', xlab='Sentir algún dolor físico, por ejemplo dolor de espalda, dolor de estómago o dolor de cabeza?') ; table(datosfil$c22)
boxplot(Datos.num$edad~Datos.numl$c23,ylab = 'Edad', xlab='Sentir algún dolor físico, por ejemplo dolor de espalda, dolor de estómago o dolor de cabeza?') ; table(datosfil$c23)




###ORDEN DE DESEMPEÑO####
#DESEMPEÑO 
par(mfrow = c(2, 2))
boxplot(Datos.num$edad~Datos.num$d1,ylab = 'Edad', xlab='Ponerse de pie estando sentado/a?') ; table(datosfil$d1)
plot(Datos.num$edad~datosfil$d2, ylab = 'Edad', xlab='Estar de pie durante largos períodos de tiempo, como por ejemplo 30 minutos?') ; table(datosfil$d2)
plot(Datos.num$edad~Datos.num$d4,ylab = 'Edad', xlab='Caminar distancias cortas tales como una cuadra o 100 metros?') ; table(datosfil$d4)
plot(Datos.num$edad~Datos.num$d5,ylab = 'Edad',xlab = 'Caminar diez cuadras o un kilómetro?') ; table(datosfil$d5)
par(mfrow = c(2, 2))
plot(datosfil$edad~datosfil$d6,ylab = 'Edad', xlab='Realizar actividades físicamente exigentes, como jugar fútbol, levantar objetos pesados,andar en bicicleta o correr?'); table(datosfil$d6) 
plot(datosfil$edad~datosfil$d7, ylab = 'Edad', xlab='Llegar a los lugares donde ha querido ir?') ; table(datosfil$d7)
plot(datosfil$edad~datosfil$d8,ylab = 'Edad', xlab='Manipular objetos pequeños o abrir un envase?') ; table(datosfil$d8)
plot(datosfil$edad~datosfil$d9,ylab = 'Edad',xlab = 'Levantar una botella de agua de dos litros llena, desde la cintura hasta el nivel de los ojos?') ; table(datosfil$d9)
par(mfrow = c(2, 2))
plot(datosfil$edad~datosfil$d10,ylab = 'Edad', xlab=' Asearse o vestirse?'); table(datosfil$d10) 
plot(datosfil$edad~datosfil$d11, ylab = 'Edad', xlab='Alimentarse?') ; table(datosfil$d11)
plot(datosfil$edad~datosfil$d12,ylab = 'Edad', xlab=' Utilizar el baño (W.C.)?') ; table(datosfil$d12)
plot(datosfil$edad~datosfil$d13,ylab = 'Edad', xlab='Cortarse las uñas de los pies?') ; table(datosfil$d13)
par(mfrow = c(2, 2))
plot(datosfil$edad~datosfil$d14,ylab = 'Edad', xlab='Cuidar de su salud?') ; table(datosfil$d14)
plot(datosfil$edad~datosfil$d15,ylab = 'Edad', xlab='Ver objetos de lejos?') ; table(datosfil$d15)
plot(datosfil$edad~datosfil$d16,ylab = 'Edad', xlab='Ver objetos de cerca?') ; table(datosfil$d16)
plot(datosfil$edad~datosfil$d17,ylab = 'Edad', xlab='Oír una conversación en un lugar silencioso?') ; table(datosfil$d17)
par(mfrow = c(2, 2))
plot(datosfil$edad~datosfil$d18,ylab = 'Edad', xlab=' Oír una conversación en una habitación con ruido alrededor?') ; table(datosfil$d18)
plot(datosfil$edad~datosfil$d19,ylab = 'Edad', xlab='Sentir algún dolor físico, por ejemplo dolor de espalda, dolor de estómago o dolor de cabeza?') ; table(datosfil$d19)
plot(datosfil$edad~datosfil$d20,ylab = 'Edad', xlab='Dormir?') ; table(datosfil$d20)
plot(datosfil$edad~datosfil$d21,ylab = 'Edad',xlab = 'Sentirse cansado y no tener suficiente energía?') ; table(datosfil$d21)
par(mfrow = c(2, 2))
plot(datosfil$edad~datosfil$d22,ylab = 'Edad',xlab = 'Sentir falta de aire?') ; table(datosfil$d22)
plot(datosfil$edad~datosfil$d23,ylab = 'Edad',xlab = 'Toser o respirar con dificultad?') ; table(datosfil$d23)
plot(datosfil$edad~datosfil$d24,ylab = 'Edad',xlab = 'Sentirse triste, bajo de ánimo o deprimido/a?') ; table(datosfil$d24)
plot(datosfil$edad~datosfil$d25,ylab = 'Edad',xlab = 'Sentir preocupación, nerviosismo o ansiedad? d26 Llevarse bien con personas cercanas, incluyendo su familia y amigos?') ; table(datosfil$d25)
par(mfrow = c(2, 2))
plot(datosfil$edad~datosfil$d26,ylab = 'Edad',xlab = '') ; table(datosfil$d26)
plot(datosfil$edad~datosfil$d27,ylab = 'Edad',xlab = 'Relacionarse con personas que no conoce?') ; table(datosfil$d27)
plot(datosfil$edad~datosfil$d28,ylab = 'Edad',xlab = 'Hacer nuevas amistades o mantener las actuales?') ; table(datosfil$d28)
plot(datosfil$edad~datosfil$d29,ylab = 'Edad',xlab = 'Tener una relación de pareja?') ; table(datosfil$d29)
par(mfrow = c(2, 2))
plot(datosfil$edad~datosfil$d30,ylab = 'Edad',xlab = 'Manejar el estrés?') ; table(datosfil$d30)
plot(datosfil$edad~datosfil$d31,ylab = 'Edad',xlab = 'Enfrentar todas las tareas que tiene que hacer?') ; table(datosfil$d31)
plot(datosfil$edad~datosfil$d32,ylab = 'Edad',xlab = 'Ser entendido, utilizando su lenguaje habitual?') ; table(datosfil$d32)
plot(datosfil$edad~datosfil$d33,ylab = 'Edad',xlab = 'Entender a otros, utilizando su lenguaje habitual?') ; table(datosfil$d33)
par(mfrow = c(2, 2))
plot(datosfil$edad~datosfil$d34,ylab = 'Edad',xlab = 'Olvidar cosas/tener mala memoria?') ; table(datosfil$d34)
plot(datosfil$edad~datosfil$d35,ylab = 'Edad',xlab = 'Recordar las cosas importantes que tiene que hacer en su día a día?') ; table(datosfil$d35)
plot(datosfil$edad~datosfil$d36,ylab = 'Edad',xlab = 'Encontrar soluciones a los problemas del día a día?') ; table(datosfil$d36)
plot(datosfil$edad~datosfil$d37,ylab = 'Edad',xlab = 'Completar las tareas de la casa como barrer, cocinar, hacer arreglos o sacar la basura?') ; table(datosfil$d37)
par(mfrow = c(2, 2))
plot(datosfil$edad~datosfil$d38,ylab = 'Edad',xlab = 'Administrar el dinero que posee?') ; table(datosfil$d38)
plot(datosfil$edad~datosfil$d39,ylab = 'Edad',xlab = 'Hacer cosas que lo relajen o que lo hagan disfrutar?') ; table(datosfil$d39)
plot(datosfil$edad~datosfil$d40,ylab = 'Edad',xlab = 'Participar en fiestas, eventos religiosos, reuniones vecinales u otras actividades comunales?') ; table(datosfil$d40)
plot(datosfil$edad~datosfil$d41,ylab = 'Edad',xlab = 'Participar en la política local o nacional y en organizaciones de la sociedad civil?') ; table(datosfil$d41)
par(mfrow = c(2, 2))
plot(datosfil$edad~datosfil$d42,ylab = 'Edad',xlab = 'Cuidar o dar apoyo a otros?') ; table(datosfil$d42)
plot(datosfil$edad~datosfil$d43,ylab = 'Edad',xlab = 'Postular o conseguir un empleo?') ; table(datosfil$d43)
plot(datosfil$edad~datosfil$d44,ylab = 'Edad',xlab = 'Acceder a una educación superior? Ejemplos: CFT, IP, Universidad') ; table(datosfil$d44)
plot(datosfil$edad~datosfil$d45,ylab = 'Edad',xlab = 'Utilizar los servicios de transporte público?') ; table(datosfil$d45)
par(mfrow = c(1, 2))
plot(datosfil$edad~datosfil$d46,ylab = 'Edad',xlab = 'Realizar las tareas que le solicitan en su empleo?') ; table(datosfil$d46)
plot(datosfil$edad~datosfil$d47,ylab = 'Edad',xlab = 'Realizar las tareas que le solicitan en su establecimiento educacional?') ; table(datosfil$d47)


###FACTORES AMBIENTALES----
par(mfrow = c(2, 2))
plot(datosfil$edad~datosfil$fa1,ylab = 'Edad',xlab='fa1:le resulta fácil o difícil utilizar los servicios sanitarios que necesita con regularidad?',cex=0.02) ; table(datosfil$fa1)
plot(datosfil$edad~datosfil$fa2,ylab = 'Edad',xlab='fa2:¿le resultan fáciles o difíciles los lugares donde se relaciona y participa en actividades comunitarias?',cex=0.02) ; table(datosfil$fa2)
plot(datosfil$edad~datosfil$fa3,ylab = 'Edad',xlab='fa3:¿le resulta fácil o difícil utilizar las tiendas, bancos y oficinas de correos de su barrio?',cex=0.02) ; table(datosfil$fa3)
plot(datosfil$edad~datosfil$fa4,ylab = 'Edad',xlab='fa4:¿le resulta fácil o difícil practicar su religión en los lugares de culto que frecuenta habitualmente?
',cex=0.02) ; table(datosfil$fa4)
plot(datosfil$edad~datosfil$fa5,ylab = 'Edad',xlab='fa5:los medios de transporte que necesita o desea utilizar, ¿le resultan fáciles o difíciles de utilizar?',cex=0.02) ; table(datosfil$fa5)
plot(datosfil$edad~datosfil$fa6,ylab = 'Edad',xlab='fa6:¿le resulta fácil o difícil vivir en su vivienda (incluido el aseo y todas las habitaciones)?') ; table(datosfil$fa6)
plot(datosfil$edad~datosfil$fa7,ylab = 'Edad',xlab='fa7:¿le facilitan o dificultan la vida la temperatura, el terreno y el clima del lugar donde vive habitualmente?') ; table(datosfil$fa7)
plot(datosfil$edad~datosfil$fa8,ylab = 'Edad',xlab='fa8:¿la iluminación de su entorno le facilita o le dificulta la vida?') ; table(datosfil$fa8)
plot(datosfil$edad~datosfil$fa9,ylab = 'Edad',xlab='fa9:¿el ruido de su entorno le facilita o le dificulta vivir en él?') ; table(datosfil$fa9)
plot(datosfil$edad~datosfil$fa10,ylab = 'Edad',xlab='fa10:¿la aglomeración de gente en su entorno le facilita o le dificulta vivir allí?') ; table(datosfil$fa10)
plot(datosfil$edad~datosfil$fa11,ylab = 'Edad',xlab='fa11:¿le resulta fácil o difícil trabajar o aprender en su lugar de trabajo?') ; table(datosfil$fa11)
plot(datosfil$edad~datosfil$fa12,ylab = 'Edad',xlab='fa12:¿le resulta fácil o difícil trabajar o aprender en su centro de estudios?') ; table(datosfil$fa12)





# Mostrar el resultado
print(df_filtrado)
R2 <- hetcor(df_filtrado[,c(4:27)],type="Polychoric")$correlations#correla
polychoric(df_filtrado[,c(4:27)])
polychoric()
n_missing <- colSums(is.na(df_filtrado))# d46 d47 fa11 fa12
#leer cuales son estas variables
n_missing2 <- colSums(is.na(Datos.fil))
####
# Seleccionar columnas numéricas
num_cols <- sapply(df_filtrado, is.numeric)


cor_pol <- hetcor(df_filtrado[,c(4:27)], type = "Polychoric")$correlations
cor_poly_cap=polychoric(df_filtrado[,c(4:27)])
cor_poly=polychoric(df_filtrado[,c(4:86)])



# ANALISIS FACTORIAL ----

#PLS DATOS ----
#PLS 3 

MFA.nipals.3G <- function(data, group, it,ncp) {
  ## group: is a vector with the number variables per group.
  ## it: is the number iterations in NIPALS.
  
  nipalsg1 <- ade4::nipals(data[,1:group[1]], niter=it,nf=ncp)
  nipalsg2 <- ade4::nipals(data[,(group[1]+1):sum(group[1:2])], niter=it,nf=ncp)
  nipalsg3 <- ade4::nipals(data[,(sum(group[1:2])+1):sum(group)], niter=it,nf=ncp)
  
  dataZ.na <- cbind(data[,1:group[1]]/nipalsg1$eig[1],
                    data[,(group[1]+1):sum(group[1:2])]/nipalsg2$eig[1],
                    data[,(sum(group[1:2])+1):sum(group)]/nipalsg3$eig[1])
  
  nipals.global.na <- ade4::nipals(dataZ.na,niter=it,nf=ncp)
  
  ## individual factor map
   plot(nipals.global.na$li[,1],nipals.global.na$li[,2],
       xlab="dim1",ylab="dim2",pch=19,
       main="Individual factor map with MFA-NIPALS")
  text(nipals.global.na$li[,1]+0.001,nipals.global.na$li[,2]+0.2,
       1:50)
  abline(h=0)
  abline(v=0)
  
  ## corcircle
  
  #### circulo
  radio <- 1
  t <- seq(0, 2*pi, length.out = 100)
  
  x <- cos(t)*radio
  y <- sin(t)*radio
  
  ### corcircle
  
  plot(nipals.global.na$co[,1],nipals.global.na$co[,2],
       xlab="dim1",ylab="dim2",pch=19,
       main="Correlation circle map with MFA-NIPALS",
       ylim=c(-1.2,1.2),xlim=c(-1.1,1.1))
  text(nipals.global.na$co[,1],nipals.global.na$co[,2]+0.001,
       colnames(data),
       col=c(rep(2,group[1]),rep(3,group[2]), rep(4,group[3])))
  lines(x, y, lwd = 1)
  abline(h=0)
  abline(v=0)
  

  
  
  
  results = list(T = nipals.global.na$li, F= nipals.global.na$co,
                 lambda= nipals.global.na$eig)
}

#PLS 2 

MFA.nipals.2G <- function(data, group, it,ncp) {
  ## group: is a vector with the number variables per group.
  ## it: is the number iterations in NIPALS.
  
  nipalsg1 <- ade4::nipals(data[,1:group[1]], niter=it,nf=ncp)
  nipalsg2 <- ade4::nipals(data[,(group[1]+1):sum(group[1:2])], niter=it,nf=ncp)

  
  dataZ.na <- cbind(data[,1:group[1]]/nipalsg1$eig[1],
                    data[,(group[1]+1):sum(group[1:2])]/nipalsg2$eig[1]
                    )
  
  nipals.global.na <- ade4::nipals(dataZ.na,niter=it,nf=ncp)
  
  ## individual factor map
  plot(nipals.global.na$li[,1],nipals.global.na$li[,2],
       xlab="dim1",ylab="dim2",pch=19,
       main="Individual factor map with MFA-NIPALS")
  text(nipals.global.na$li[,1]+0.001,nipals.global.na$li[,2]+0.2,
       1:50)
  abline(h=0)
  abline(v=0)
  
 
  
  ## corcircle
  
  #### circulo
  radio <- 1
  t <- seq(0, 2*pi, length.out = 100)
  
  x <- cos(t)*radio
  y <- sin(t)*radio
  
  ### corcircle
  
  plot(nipals.global.na$co[,1],nipals.global.na$co[,2],
       xlab="dim1",ylab="dim2",pch=19,
       main="Correlation circle map with MFA-NIPALS",
       ylim=c(-1.2,1.2),xlim=c(-1.1,1.1))
  text(nipals.global.na$co[,1],nipals.global.na$co[,2]+0.001,
       colnames(data),
       col=c(rep(2,group[1]),rep(3,group[2])))
  lines(x, y, lwd = 1)
  abline(h=0)
  abline(v=0)
  
  results = list(T = nipals.global.na$li, F= nipals.global.na$co,
                 lambda= nipals.global.na$eig)
}

#PLS 4
MFA.nipals.4G <- function(data, group, it, ncp) {
  ## group: is a vector with the number variables per group.
  ## it: is the number iterations in NIPALS.
  ## ncp: number of axes to keep.
  
  nipalsg1 <- ade4::nipals(data[,1:group[1]], niter=it, nf=ncp)
  nipalsg2 <- ade4::nipals(data[,(group[1]+1):sum(group[1:2])], niter=it, nf=ncp)
  nipalsg3 <- ade4::nipals(data[,(sum(group[1:2])+1):sum(group[1:3])], niter=it, nf=ncp)
  nipalsg4 <- ade4::nipals(data[,(sum(group[1:3])+1):sum(group)], niter=it, nf=ncp)
  
  
  dataZ.na <- cbind(data[,1:group[1]]/nipalsg1$eig[1],
                    data[,(group[1]+1):sum(group[1:2])]/nipalsg2$eig[1],
                    data[,(sum(group[1:2])+1):sum(1:3)]/nipalsg3$eig[1],
                    data[,(sum(group[1:3])+1):sum(group)]/nipalsg4$eig[1])
  
  nipals.global.na <- ade4::nipals(dataZ.na,niter=it, nf=ncp)
  
  ## individual factor map
  plot(nipals.global.na$li[,1],nipals.global.na$li[,2],
       xlab="dim1",ylab="dim2",pch=19,
       main="Individual factor map with MFA-NIPALS")
  text(nipals.global.na$li[,1]+0.001,nipals.global.na$li[,2]+0.2,
       row.names(data), cex=0.7)
  abline(h=0)
  abline(v=0)
  
  
  
  ## corcircle
  
  #### circulo
  radio <- 1
  t <- seq(0, 2*pi, length.out = 100)
  
  x <- cos(t)*radio
  y <- sin(t)*radio
  
  ### corcircle
  
  plot(nipals.global.na$co[,1],nipals.global.na$co[,2],
       xlab="dim1",ylab="dim2",pch=19,
       main="Correlation circle map with MFA-NIPALS",
       ylim=c(-1.2,1.2),xlim=c(-1.1,1.1))
  text(nipals.global.na$co[,1],nipals.global.na$co[,2]+0.001,
       colnames(data),
       col=c(rep(2,group[1]),rep(3,group[2]), rep(4,group[3]), 
             rep(5,group[4])), cex=0.55)
  lines(x, y, lwd = 1)
  abline(h=0)
  abline(v=0)
  
  
  
  results = list(T = nipals.global.na$li, F= nipals.global.na$co,
                 lambda= nipals.global.na$eig)
  
}

MFAnipals.cap <- MFA.nipals.3G(Datos.num.cap_reagrupado, group=c(8,14,1), it=100,ncp=3)
MFAnipals.cap$lambda/23
MFA.cap.num <- MFA(cap_reagrupado_NoNA_num, group =c(8,13,1))
MFAnipals.cap$F


MFA.fa.num <- MFA(fa_reagrupado_NoNA_num, group =c(7,1,1,1))
MFAnipals.fa <- MFA.nipals.4G(Datos.num.fa_reagrupado, group =c(7,2,1,1),it=100,ncp=3)
MFAnipals.fa$lambda/11


MFAnipals.de <- MFA.nipals.2G(Datos.num.de_reagrupado, group=c(12,33), it=160,ncp=4)
MFAnipals.de$lambda/46
##Analisis Factorial (no NA)


MFA.de.num <- FactoMineR:::MFA(de_reagrupado_NoNA_num, group =c(12,33))
MFA.de.num$eig
library(factoextra)
# Calcular la varianza explicada por cada eje
x11()
fviz_mfa_var(MFA.de.num,axes = c(1, 3))

MFAnipals.de$
plot(MFAnipals.de)
#Correr el pls con mas factores y evaluar factores de decision
#Para saber con cuantos factores me quedo
#Presentacion de pls


valp.cap <- c(MFAnipals.cap$lambda)

# Crear un dataframe con los datos
df.valpCap <- data.frame(Factor = factor(1:length(valp.cap)), Valor = valp.cap)

# Crear el gráfico de sedimentación
ggplot(df.valpCap, aes(x = Factor, y = Valor)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_point(color = "red", size = 3) +
  geom_line(aes(group = 1), color = "red", size = 1) +
  labs(x = "Factores", y = "Valores", title = "Gráfico de Sedimentación")
MFAnipals.cap$lambda/23





valp.fa<- c(MFAnipals.fa$lambda)

# Crear un dataframe con los datos
df.valpFa <- data.frame(Factor = factor(1:length(valp.fa)), Valor = valp.fa)

# Crear el gráfico de sedimentación
ggplot(df.valpFa, aes(x = Factor, y = Valor)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_point(color = "red", size = 3) +
  geom_line(aes(group = 1), color = "red", size = 1) +
  labs(x = "Factores", y = "Valores", title = "Gráfico de Sedimentación")
MFAnipals.fa$lambda/11


valp.de<- c(MFAnipals.de$lambda)

# Crear un dataframe con los datos
df.valpDe <- data.frame(Factor = factor(1:length(valp.de)), Valor = valp.de)

# Crear el gráfico de sedimentación
ggplot(df.valpDe, aes(x = Factor, y = Valor)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_point(color = "red", size = 3) +
  geom_line(aes(group = 1), color = "red", size = 1) +
  labs(x = "Factores", y = "Valores", title = "Gráfico de Sedimentación")
MFAnipals.de$lambda/46
###
colSums(is.na(df))
numero_filas_completas <- sum(complete.cases(df))
######
#AFM con los para los datos incompletos quitando educacion y sexo ----
Datos.num.incomp <- Datos.num[rowSums(is.na(Datos.num)) > 0, ]
capIncom_NoNA <- dplyr::select(Datos.num.incomp,c2,c3,c5,c15,c19,c20,c21,c23,c6,
                                         c7,c8,c9,c10,c11,c12,c13,c14,c16,c17,c18,c22,c4)
MFAincompCap <- MFA(capIncom_NoNA,group=c(8,13,1))
fviz_mfa_var(MFAincompCap )
fviz_mfa_ind(MFAincompCap )
deIncom_NoNA <- dplyr::select(Datos.num.incomp,d15,d16,d17,d18,d19,d21,d22,d23,d24,d25,d34,d35,
                                        d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, 
                                        d14, d20, d26, d27, d28, d29, d30, d31, d32, d33, d36, d37, 
                                        d38, d39, d40, d41, d42, d43, d44, d45)
MFAincompDe <- MFA(deIncom_NoNA,group=c(12,33))
fviz_mfa_var(MFAincompDe)
fviz_mfa_ind(MFAincompDe)
faIncom_NoNA <- dplyr::select(Datos.num.incomp,fa1,fa3,fa6,fa7,fa8,fa9,fa10,fa2,fa4,fa5)
MFAincompFa <- MFA(faIncom_NoNA,group =c(7,1,1,1))
fviz_mfa_var(MFAincompFa)
fviz_mfa_ind(MFAincompFa)


#AFM con las 212 obs ----
Datos.num.comple <- na.omit(Datos.num)
#8, 15,1
capCom<- dplyr::select(Datos.num.comple,c2,c3,c5,c15,c19,c20,c21,c23,c6,
                                          c7,c8,c9,c10,c11,c12,c13,c14,c16,c17,c18,c22,c24,c25,c4)
MFAcompCap <- MFA(capCom,group=c(8,15,1))
fviz_mfa_var(MFAcompCap)
#12,35
deCom <- dplyr::select(Datos.num.comple,d15,d16,d17,d18,d19,d21,d22,d23,d24,d25,d34,d35,
                                         d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, 
                                         d14, d20, d26, d27, d28, d29, d30, d31, d32, d33, d36, d37, 
                                         d38, d39, d40, d41, d42, d43, d44, d45, d46,d47)
MFAcompDe <- MFA(deCom,group=c(12,35))
fviz_mfa_var(MFAcompDe)
#7,3,1,1
faCom<- dplyr::select(Datos.num.comple,fa1,fa3,fa6,fa7,fa8,fa9,fa10,fa11,fa12,fa2,fa4,fa5)
MFAcompFa <- MFA(faCom,group=c(7,3,1,1))
fviz_mfa_var(MFAcompFa)
#####
Incom_NoNA <- dplyr::select(Datos.num.incomp,-c(c24,c25,d46,d47,fa11,fa12))
MFAincompGen <- MFA(Incom_NoNA[,c(5:81)],group=c(22,45,10))
fviz_mfa_var(MFAincompGen)

MFAcompGen <- MFA(Datos.num.comple[,c(5:87)],group=c(24,47,12))
fviz_mfa_var(MFAcompGen)
#####
#Analisis Factorial exploratorio ----
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


# CFA ----
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

