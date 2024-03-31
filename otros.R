## AFE GENERAL

R <- hetcor(df_filtrado[,c(4:86)],type = "Polychoric")$correlations
KMO(R)
psych::pairs.panels(Datos.fil,method = "pearson",density = T,ellipses = F,show.points = F)
psych::describe(Datos.fil)
psych::mardia(Datos.fil)#No es conveniente correlaciones
#de pearson mejor correlaciones polycoricas

ggcorrplot(R,type="lower",hc.order = T)

psych::scree(df_filtrado[,c(4:86)], pc = TRUE, fa = FALSE, hline = -1, 
             main = "Scree Plot")
fa.parallel(R, fm="ml")

mod.general <- fa(R,nfactors = 6,rotate = "promax",fm="ml",cor = "poly")

fa.diagram(mod.general)

rot2<-c("none", "promax","varimax")
bi_mod1<-function(tipo){
  biplot.psych(fa(Datos.fil,nfactors = 6,fm="pa",rotate = tipo),main = paste("Biplot con rotación ",tipo),col=c(2,3,4),pch = c(21,18),group = bfi[,"gender"])  
}
sapply(rot2,bi_mod1)

biplot.psych(fa(Datos.fil,nfactors = 4,fm="pa",rotate = "promax"))
print(mod.general, sort = TRUE, cut = 0, digits = 3)


load <- mod.general$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(Datos.fil),cex=.7)

load <- mod.general$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(Datos.fil),cex=.7)

loads <- mod.general$loadings
fa.diagram(loads)

#CAPACIDAD EFA
df_NoNA <- subset(Datos.num, select = -c(c24, c25,d46,d47,fa11,fa12))
df_NoNA.cat <- subset(Datos.cat, select = -c(c24, c25,d46,d47,fa11,fa12))
out <- psych::polychoric(df_NoNA[,c(5:26)])

R <- out$rho
MFA.cap.num <- MFA(cap_reagrupado_NoNA_num, group =c(8,13,1))
fviz_mfa_var(MFA.cap.num)
fviz_mfa_ind(MFA.cap.num)
fivz_biplot()
MFA(cap_reagrupado_NoNA_cat, group =c(8,13,1),type = rep("n",3))

R2 <- hetcor(df_filtrado[,c(4:27)],type = "Polychoric")$correlations
KMO(R)
KMO(cor_poly$rho)
cortest.bartlett(cor_poly$rho)
fa.parallel(R, fm="ml")
mod.cap     <- fa(R, nfactors = 2,cor="poly",
                  fm="ml",rotate = "oblimin")
mod.cap$loadings
mod.cap$weights
fa.diagram(mod.cap)
mod.cap$V[,1]
ei2   <- eigen(cor_pol)
plot(ei2$values,type="b",pch=20,col="blue")
abline(h=1,lty=3,col="red")
psych::scree(cor_pol)



rot2<-c("none", "promax","varimax","oblimin")
bi_mod2<-function(tipo){
  biplot.psych(fa(df_NoNA[,c(5:26)],nfactors = 2,fm="ml",rotate = tipo,cor="poly"),main = paste("Biplot con rotación ",tipo),col=c(2,3,4),pch = c(21,18),group = bfi[,"gender"])  
}


MFA.CAP <- MFA(df_NoNA.cat[,c(5:26)],group =c(5,5,5,5,2),type=rep("n",5))
fviz_mfa(MFA.CAP)
eigen(R)$vectors

loadscap <- mod.cap$loadings
fa.diagram(loadscap,digits=2)


fa.diagram(modelo_promax2,digits=2)
fa.diagram(modelo_promax,digits=2)
print(modelo_promax$loadings,cut=0)
#######
#DESEMPEÑO EFA
R3 <- hetcor(df_filtrado[,c(28:74)],type = "Polychoric")$correlations
KMO(R3)
fa.parallel(R3, fm="ml")

polychoric(df_filtrado[,c(28:74)])


modelo_promax.d <- fa(R3,nfactors = 3,rotate = "promax",fm="ml",cor="poly")
fa.diagram(modelo_promax.d,digits=2)
#FACTORES AMBIENTALES EFA

R4 <- hetcor(df_filtrado[,c(75:86)],type = "Polychoric")$correlations
KMO(R4)

fa.parallel(R4, fm="ml")

cor_poly_FA <- polychoric(df_filtrado[,c(75:86)])


modelo_promax.fa.poly <- fa(cor_poly_FA$rho,nfactors = 2,rotate = "promax",fm="ml",cor="poly")
fa.diagram(modelo_promax.fa,digits=2)

modelo_promax.fa <- fa(R4,nfactors = 1,rotate = "promax",fm="ml",cor="poly")
fa.diagram(modelo_promax.fa,digits=2)
#Como mido las correlaciones entre variables tipo likert
#Depurar los registros que no son en likert
#Ver cuantos individuos tienen informacion en la escala likert
####


MFA.nipals <- function(data, group, it, ncp, qualitative_vars = NULL, supplementary_qualitative_vars = NULL) {
  ## group: is a vector with the number variables per group.
  ## it: is the number iterations in NIPALS.
  ## ncp: number of axes to keep.
  ## qualitative_vars: a list of indices indicating which variables are qualitative.
  ## supplementary_qualitative_vars: a list of indices indicating which supplementary variables are qualitative.
  
  # NIPALS for quantitative variables
  quant_data <- data[, -unlist(qualitative_vars)]
  nipals_list <- lapply(seq_along(group), function(i) {
    start_index <- sum(group[1:i-1]) + 1
    end_index <- sum(group[1:i])
    ade4::nipals(quant_data[, start_index:end_index], niter = it, nf = ncp)
  })
  
  # Scaling for quantitative variables
  scaled_data <- sapply(seq_along(group), function(i) {
    start_index <- sum(group[1:i-1]) + 1
    end_index <- sum(group[1:i])
    quant_data[, start_index:end_index] / nipals_list[[i]]$eig[1]
  })
  
  # NIPALS for qualitative variables
  if (!is.null(qualitative_vars)) {
    for (q_var_indices in qualitative_vars) {
      for (q_var_index in q_var_indices) {
        qual_nipals <- ade4::nipals(data[, q_var_index, drop = FALSE], niter = it, nf = ncp)
        scaled_data <- cbind(scaled_data, qual_nipals$li / qual_nipals$eig[1])
      }
    }
  }
  
  # NIPALS for supplementary qualitative variables
  if (!is.null(supplementary_qualitative_vars)) {
    for (supp_q_var_indices in supplementary_qualitative_vars) {
      for (supp_q_var_index in supp_q_var_indices) {
        supp_qual_nipals <- ade4::nipals(data[, supp_q_var_index, drop = FALSE], niter = it, nf = ncp)
        scaled_data <- cbind(scaled_data, supp_qual_nipals$li / supp_qual_nipals$eig[1])
      }
    }
  }
  
  # Global NIPALS
  nipals.global.na <- ade4::nipals(scaled_data, niter = it, nf = ncp)
  
  # Plotting
  plot(nipals.global.na$li[,1], nipals.global.na$li[,2],
       xlab="dim1", ylab="dim2", pch=19,
       main="Individual factor map with MFA-NIPALS")
  text(nipals.global.na$li[,1] + 0.001, nipals.global.na$li[,2] + 0.2,
       row.names(data), cex=0.7)
  abline(h=0)
  abline(v=0)
  
  # Correlation circle
  radio <- 1
  t <- seq(0, 2*pi, length.out = 100)
  x <- cos(t) * radio
  y <- sin(t) * radio
  
  plot(nipals.global.na$co[,1], nipals.global.na$co[,2],
       xlab="dim1", ylab="dim2", pch=19,
       main="Correlation circle map with MFA-NIPALS",
       ylim=c(-1.2,1.2), xlim=c(-1.1,1.1))
  text(nipals.global.na$co[,1], nipals.global.na$co[,2] + 0.001,
       colnames(data),
       col=c(rep(2,group[1]), rep(3,group[2]), rep(4,group[3]), 
             rep(5,group[4]), rep(6,group[5])), cex=0.55)
  lines(x, y, lwd = 1)
  abline(h=0)
  abline(v=0)
  
  # Return results
  results <- list(T = nipals.global.na$li, F = nipals.global.na$co, lambda = nipals.global.na$eig)
  return(results)
}


################

cargas <- as.matrix(MFAnipals.cap$F)
radio=1
cargasdf <- data.frame(variables = variables, 
                       carga1 = cargas[,1],
                       carga2 = cargas[,2],
                       carga3 = cargas[,3])

circle <- data.frame(
  x = cos(seq(0, 2*pi, length.out = 100)),
  y = sin(seq(0, 2*pi, length.out = 100))
)
variables <- paste0("c", 1:23)

cargasdf$longitud <- sqrt(cargasdf$carga1^2 + cargasdf$carga2^2)
cargasdf$dir_x <- cargasdf$carga1 / cargasdf$longitud
cargasdf$dir_y <- cargasdf$carga2 / cargasdf$longitud

ggplot(cargasdf, aes(x = carga1, y = carga2)) +
  geom_point() +  # Dibujar puntos para las cargas factoriales
  geom_text(aes(label = variables), size = 3, hjust = 0, vjust = 0) +  # Agregar etiquetas para las variables
  xlim(-1, 1) + ylim(-1, 1) +  # Establecer límites para el eje x e y
  geom_path(data = circle, aes(x = x, y = y), color = "blue") +  # Dibujar el círculo
  geom_segment(aes(x = 0, y = 0, xend = carga1, yend = carga2), arrow = arrow(length = unit(0.3, "cm")), color = "red") +  # Agregar flechas para las variables
  geom_vline(xintercept = 0, linetype = "dashed") +  # Agregar eje vertical a través del centro
  geom_hline(yintercept = 0, linetype = "dashed") +  # Agregar eje horizontal a través del centro
  theme_minimal() +  # Establecer un tema mínimo
  theme(axis.title = element_blank(),  # Eliminar los títulos de los ejes
        axis.text = element_blank(),  # Eliminar los textos de los ejes
        axis.ticks = element_blank())  # Eliminar las marcas de los ejes
####
# Normalizar las coordenadas
coordenadas_normalizadas <- (coordenadas - min(coordenadas)) / (max(coordenadas) - min(coordenadas))

# Convertir las coordenadas normalizadas a una escala de 0 a 100
indicador <- coordenadas_normalizadas * 100

# Invertir el indicador
indicador_inverso <- 100 - indicador

# Mostrar el resultado
print(indicador_inverso)
######################
#INDICADOOOOR

T1.cap <- MFAincompCap$ind$coord[,1]
T1.de <- MFAincompDe$ind$coord[,1]
T1.fa <- MFAincompFa$ind$coord[,1]
#MFAincompCap$eig[1,1]+MFAincompDe$eig[1,1]+MFAincompFa$eig[1,1]=6.572726
#Ponderanciones indicador
MFAincompCap$eig[1,1]/6.57207
MFAincompDe$eig[1,1]/6.57207
MFAincompFa$eig[1,1]/6.57207

I_1 <- (MFAincompCap$ind$coord[,1]*MFAincompCap$eig[1,1]/6.572726)+
  (MFAincompDe$ind$coord[,1]*MFAincompDe$eig[1,1]/6.572726)+
  (MFAincompFa$ind$coord[,1]*MFAincompFa$eig[1,1]/6.572726)
Puntaje.I_1 <- 100-((I_1 - min(I_1))*100/(max(I_1)-min(I_1)))
Puntaje.df <- data.frame(Puntaje.I_1)

dfedad <- data.frame(Puntaje.I_1,Datos.num.incomp$edad,Datos.num.incomp$sexo)
boxplot(Datos.num.incomp$sexo,Puntaje.I_1)
ggplot(dfedad, aes(x = Datos.num.incomp.edad, y = Puntaje.I_1, color = Datos.num.incomp.sexo)) +
  geom_point() +
  labs(x = "Edad", y = "Puntaje", color = "Sexo") +
  theme_minimal()
