MFA.nipals <- function(data, group, it, ncp) {
  ## group: is a vector with the number variables per group.
  ## it: is the number iterations in NIPALS.
  ## ncp: number of axes to keep.
  
  nipalsg1 <- ade4::nipals(data[,1:group[1]], niter=it, nf=ncp)
  nipalsg2 <- ade4::nipals(data[,(group[1]+1):sum(group[1:2])], niter=it, nf=ncp)
  nipalsg3 <- ade4::nipals(data[,(sum(group[1:2])+1):sum(group[1:3])], niter=it, nf=ncp)
  nipalsg4 <- ade4::nipals(data[,(sum(group[1:3])+1):sum(group[1:4])], niter=it, nf=ncp)
  nipalsg5 <- ade4::nipals(data[,(sum(group[1:4])+1):sum(group)], niter=it, nf=ncp)
  
  dataZ.na <- cbind(data[,1:group[1]]/nipalsg1$eig[1],
                    data[,(group[1]+1):sum(group[1:2])]/nipalsg2$eig[1],
                    data[,(sum(group[1:2])+1):sum(1:3)]/nipalsg3$eig[1],
                    data[,(sum(group[1:3])+1):sum(1:4)]/nipalsg4$eig[1],
                    data[,(sum(group[1:4])+1):sum(group)]/nipalsg5$eig[1])
  
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
             rep(5,group[4]), rep(6,group[5])), cex=0.55)
  lines(x, y, lwd = 1)
  abline(h=0)
  abline(v=0)
  
  results = list(T = nipals.global.na$li, F= nipals.global.na$co,
                 lambda= nipals.global.na$eig)
  
}


MFAnipals.cap <- MFA.nipals(datos_filtrados.resp.num[,c(5:29)], group=c(5,7,4,4,4), it=100,
                             ncp=24)
nrow(MFAnipals.cap$T)




MFAImpute.cap <- imputeMFA(datos_filtrados.resp[,c(5:28)], group=c(5,7,4,4,4),
                       type=c(rep("n",5)), ncp=2, method = c("Regularized"))
CapImpute <- MFAImpute.cap$completeObs

MFA.CapImpute <- MFA(CapImpute, group=c(5,7,4,4,4), type=c(rep("n",5)))
library(dplyr)

cantidad_na <- rowSums(is.na(df_filtrado))
datos_filtrados.resp.num <- df_filtrado %>%
  filter(cantidad_na < 83)

datos_filtrados.resp.num <- df_filtrado %>%
  filter(cantidad_na < 83)

# Lista de valores a excluir
valores_excluir <- c("No sabe", "Sin dato", "No responde")

# Filtrar las filas que no contienen los valores deseados en las variables no generales
data_fil <- datosfil %>%
  filter(
    across(-c(edad, sexo, region), ~ !(. %in% valores_excluir))
  )
#Hay gente que no respondio na