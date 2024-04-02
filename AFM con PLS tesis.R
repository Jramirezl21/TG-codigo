#PLS DATOS
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