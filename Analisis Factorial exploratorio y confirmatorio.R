#Analisis factorial confirmatorio y exploratorio DATOS INCOMPLETOS ####
##Analsis facotrial exploratorio para capacidad
RIncomCap <- cor(Incom_NoNA[,c(5:26)])
KMO(RIncomCap)
scree(RIncomCap)



EFAIncomCap_None <- principal(RIncomCap, nfactors = 4, rotate = "none")
EFAIncomCap_Varimax <- principal(RIncomCap, nfactors = 4, rotate = "varimax")

##Analsis facotrial exploratorio para desempeño
RIncomDe <- cor(Incom_NoNA[,c(27:71)])
KMO(RIncomDe)
scree(RIncomDe,pc=T)


EFAIncomDe_None <- principal(RIncomDe, nfactors = 9, rotate = "none")
EFAIncomDe_Varimax <- principal(RIncomDe, nfactors = 9, rotate = "varimax")
##Analsis facotrial exploratorio para factores ambientales
RIncomFa <- cor(Incom_NoNA[,c(72:81)])
KMO(RIncomFa)
scree(RIncomFa,pc=T)


EFAIncomFa_None <- principal(RIncomFa, nfactors = 2, rotate = "none")
EFAIncomFa_Varimax <- principal(RIncomFa, nfactors = 2, rotate = "varimax")
##Analisis factorial confirmatorio general datos incompletos
RIncom <- cor(Incom_NoNA[,c(5:81)])
scree(RIncom,pc=T)


EFAIncom_Varimax <- principal(RIncom, nfactors = 3, rotate = "varimax")
print(EFAIncom_Varimax$loadings,cutoff=0)
cargas1 <- as.data.frame(round(EFAIncom_Varimax$loadings,3))
CargasIncompVarimax <- cargas1[c(1:77),]
##Analisis factorial confirmatorio general datos completos
RCom <- cor(Datos.num.comple[,c(5:87)])
scree(RCom,pc=T)


EFACom_Varimax <- principal(RCom, nfactors = 3, rotate = "varimax")
print(EFACom_Varimax$loadings,cutoff=0)

cargas2 <- as.data.frame(round(EFACom_Varimax$loadings,2))
CargasCompVarimax <- cargas2[c(1:83),]
#Seleccion de variables-Cargas mayores por factor

# Crear una nueva matriz con todos los valores establecidos en cero ####

##Para datos incompletos
# Obtener el índice del máximo en cada fila
max_indicesIncomp <- apply(CargasIncompVarimax, 1, which.max)
simplified_matrixIncomp <- matrix(0, nrow = nrow(CargasIncompVarimax), 
                            ncol = ncol(CargasIncompVarimax))

# Establecer el valor máximo en cada fila y dejar el resto en cero
for (i in 1:nrow(CargasIncompVarimax)) {
  simplified_matrixIncomp[i, max_indicesIncomp[i]] <- CargasIncompVarimax[i, max_indicesIncomp[i]]
}
##Para datos completos
max_indicesComp <- apply(CargasCompVarimax, 1, which.max)
simplified_matrixComp <- matrix(0, nrow = nrow(CargasCompVarimax), 
                                  ncol = ncol(CargasCompVarimax))

# Establecer el valor máximo en cada fila y dejar el resto en cero
for (i in 1:nrow(CargasCompVarimax)) {
  simplified_matrixComp[i, max_indicesComp[i]] <- CargasCompVarimax[i, max_indicesComp[i]]
}
