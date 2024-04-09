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
rm(data)
datosfil <- dffiltrado[,c(3,8,14,15,116:139,68:114,309:320)]
rm(dffiltrado)

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

Datos.num<-  data.frame(lapply(Datos.num, function(x) if(is.labelled(x)) as.numeric(x) else x))
Datos.num$region <- as.factor(Datos.num$region)
Datos.num$sexo <- as.factor(Datos.num$sexo)

cantidad_na2 <- rowSums(is.na(Datos.num))
colSums(is.na(Datos.num))
rm(data_fil);rm(data_fil2);rm(datosfil);rm(datosfilt)
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
####
# Obtener el índice del máximo en cada fila
max_indices <- apply(EFAIncom$weights, 1, which.max)

# Crear una nueva matriz con todos los valores establecidos en cero
simplified_matrix <- matrix(0, nrow = nrow(EFAIncom$weights), ncol = ncol(EFAIncom$weights))

# Establecer el valor máximo en cada fila y dejar el resto en cero
for (i in 1:nrow(EFAIncom$weights)) {
  simplified_matrix[i, max_indices[i]] <- EFAIncom$weights[i, max_indices[i]]
}
####
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
Datos.num.incomp <- Datos.num[rowSums(is.na(Datos.num)) > 0, ]
Incom_NoNA <- subset(Datos.num.incomp, select = -which(colSums(is.na(Datos.num.incomp)) > 0))
fitIncom<-cfa(model= modeloIncom,
         data = Incom_NoNA[,c(5:81)], orthogonal=T,ordered = F)
summary(fitIncom,fit.measures = TRUE, standardized = TRUE)
semPlot::semPaths(fitIncom,nCharNodes = 0,intercepts = F,edge.label.cex = 1,
         edge.color = "black",sizeMan = standardizedSolution(fitIncom)$loading * 10,label.prop=0.45,sizeLat = 2,"std",layout = "circle",exoVar = T)
semPlot::semPaths(fitIncom, "std",layout = "circle3",edge.color = "black")
lavInspect(fitIncom, "std")

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
