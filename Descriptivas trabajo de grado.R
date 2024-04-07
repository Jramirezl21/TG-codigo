# DESCRIPTIVAS ----
##EDAD
(table(Datos.cat$region)/4966)*100
ggplot(Datos.num, aes(x = Datos.num$edad)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Edades de los Individuos",
       x = "Edad",
       y = "Frecuencia")
summary(Datos.num$edad)
##REGION
lib <- c('ggplot2','FactoMineR','factoextra','readxl','Factoshiny','ade4',
         'rgl','raster','sp','rgdal','leaflet','tmaptools','tmap','dplyr',
         'sf','plotly')
library(sf)
library(ggplot2)  # Gráficos de alta calidad 
library(viridis)  # Paleta de colores 
library(mapview)
shape <- st_read('Regiones')
summary(shape$Region)
x11()
library(mapview)

shape2=merge(shape,data,by='Region',all.x=TRUE)
mi_paleta <- c("#8B0000", "#4169E1", "#32CD32", "#FFD700", "#9932CC", "#FF4500", "#8A2BE2", "#00BFFF", "#7FFFD4", "#FF69B4", "#FF6347", "#00FA9A", "#20B2AA", "#FF8C00", "#2E8B57", "#DAA520", "#B22222", "#48D1CC")

# Crea un gráfico de barras con colores personalizados para cada región
ggplot(Datos.cat, aes(x = region)) +
  geom_bar(fill = c("#febfff", "#efb5fa", "#e1acf5", "#d2a2f0", "#c498eb", "#b58fe6", "#a785e1", "#987cdd", "#8972d8", "#7b68d3", "#6c5fce", "#5e55c9", "#4f4bc4", "#4142bf", "#3238ba")) + # Asigna la paleta de colores definida
  labs(title = "Cantidad de Individuos por Región de Chile",
       x = "Región",
       y = "Cantidad de Individuos") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) # Rota los nombres en el eje x
#SEXO
ggplot(Datos.cat, aes(x = sexo, fill = sexo)) +
  geom_bar() +
  labs(title = "Distribución del Sexo",
       x = "Sexo",
       y = "Cantidad de Individuos") +
  scale_fill_manual(values = c("Mujer" = "#FF69B4", "Hombre" = "#4169E1")) +  # Asigna colores personalizados a cada categoría
  theme_minimal() +  # Utiliza un tema minimalista
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))  # Ajusta la orientación del texto en el eje x
###ORDEN DE CAPACIDAD####
#CAPACIDAD
colores <- c("#fffd2d", "#c9d22f", "#94a730", "#5e7b32", "#285033")

library(paletteer)
x11()
par(mfrow = c(3, 3))
boxplot(Datos.num$edad~Datos.num$c2,ylab = 'Edad', xlab='C2: Ver, sin anteojos ópticos o lentes?', col= c("#fffd2d", "#c9d22f", "#94a730", "#5e7b32", "#285033")) ; table(Datos.num$c2)
boxplot(Datos.num$edad~Datos.num$c3,ylab = 'Edad', xlab='C3: Oír, sin dispositivo de ayuda para oír o audífono?', col= c("#fffd2d", "#c9d22f", "#94a730", "#5e7b32", "#285033")) ; table(Datos.num$c3)
boxplot(Datos.num$edad~Datos.num$c4,ylab = 'Edad', xlab='C4: Caminar o subir peldaños?', col= c("#fffd2d", "#c9d22f", "#94a730", "#5e7b32", "#285033")) ; table(Datos.num$c4)
boxplot(Datos.num$edad~Datos.num$c5,ylab = 'Edad', xlab='C5: Recordar cosas o concentrarse?', col= c("#fffd2d", "#c9d22f", "#94a730", "#5e7b32", "#285033")) ; table(Datos.num$c5)
boxplot(Datos.num$edad~Datos.num$c6,ylab = 'Edad', xlab='C6: Asearse o vestirse?', col= c("#fffd2d", "#c9d22f", "#94a730", "#5e7b32", "#285033")) ; table(Datos.num$c6)
boxplot(Datos.num$edad~Datos.num$c7,ylab = 'Edad', xlab='C7:Comunicarse, por ejemplo comprender o ser entendido usando su lenguaje habitual?', col= c("#fffd2d", "#c9d22f", "#94a730", "#5e7b32", "#285033")) ; table(Datos.num$c7)
#C8
boxplot(Datos.num$edad ~ Datos.num$c8, ylab = 'Edad', xlab = 'C8: ¿Se alimenta?', col = colores)
table(Datos.num$c8)
#C9
boxplot(Datos.num$edad ~ Datos.num$c9, ylab = 'Edad', xlab = 'C9: ¿Utiliza el baño?', col = colores)
table(Datos.num$c9)
#C10
boxplot(Datos.num$edad ~ Datos.num$c10, ylab = 'Edad', xlab = 'C10: ¿Se acuesta o levanta de la cama?', col = colores)
table(Datos.num$c10)
##
x11()
par(mfrow = c(3, 3))

#C11
boxplot(Datos.num$edad ~ Datos.num$c11, ylab = 'Edad', xlab = 'C11: ¿Sale a la calle?', col = colores)
table(Datos.num$c11)
#C12
boxplot(Datos.num$edad ~ Datos.num$c12, ylab = 'Edad', xlab = 'C12: ¿Va de compras o al médico?', col = colores)
table(Datos.num$c12)
#C13
boxplot(Datos.num$edad ~ Datos.num$c13, ylab = 'Edad', xlab = 'C13: ¿Manipula objetos pequeños o abre envases?', col = colores)
table(Datos.num$c13)
#C14
boxplot(Datos.num$edad ~ Datos.num$c14, ylab = 'Edad', xlab = 'C14: ¿Duerme?', col = colores)
table(Datos.num$c14)
#C15
boxplot(Datos.num$edad ~ Datos.num$c15, ylab = 'Edad', xlab = 'C15: ¿Respira?', col = colores)
table(Datos.num$c15)
#C16
boxplot(Datos.num$edad ~ Datos.num$c16, ylab = 'Edad', xlab = 'C16:  ¿Realiza tareas del hogar?', col = colores)
table(Datos.num$c16)
#C17
boxplot(Datos.num$edad ~ Datos.num$c17, ylab = 'Edad', xlab = 'C17: ¿Cuida o da apoyo a otros?', col = colores)
#C18
boxplot(Datos.num$edad ~ Datos.num$c18, ylab = 'Edad', xlab = 'C18: ¿Participa en eventos comunales?', col = colores)
table(Datos.num$c18)
#C19
boxplot(Datos.num$edad ~ Datos.num$c19, ylab = 'Edad', xlab = 'C19: ¿Se siente triste, bajo/a de ánimo o deprimido/a?', col = colores)
table(Datos.num$c19)
x11()
par(mfrow = c(2,2))
#C20
boxplot(Datos.num$edad ~ Datos.num$c20, ylab = 'Edad', xlab = 'C20: ¿Se siente preocupado/a, nervioso/a o ansioso/a?', col = colores)
table(Datos.num$c20)
#C21
boxplot(Datos.num$edad ~ Datos.num$c21, ylab = 'Edad', xlab = 'C21:¿Se lleva bien con la gente cercana, familia o amigos?', col = colores)
table(Datos.num$c21)
#C22
boxplot(Datos.num$edad ~ Datos.num$c22, ylab = 'Edad', xlab = 'C22: ¿Puede hacer frente a todas las tareas que tiene que hacer?', col = colores)
table(Datos.num$c22)
#C23
boxplot(Datos.num$edad ~ Datos.num$c23, ylab = 'Edad', xlab = '¿Siente algún dolor físico, como dolor de espalda, estómago o cabeza?', col = colores)
table(Datos.num$c23)



###ORDEN DE DESEMPEÑO####
#DESEMPEÑO 
color2<-c("#98fffe", "#72c1d3", "#4c83a7", "#26447c", "#000650")
x11()
par(mfrow = c(3, 3))
par(cex.lab = 0.9) 
boxplot(Datos.num$edad~Datos.num$d1,ylab = 'Edad', xlab='D1: Ponerse de pie estando sentado/a?', col=color2) ; table(Datos.num$d1)
boxplot(Datos.num$edad~Datos.num$d2, ylab = 'Edad', xlab='D2: Estar de pie durante largos períodos de tiempo, como por ejemplo 30 minutos?', col=color2) ; table(Datos.num$d2)
boxplot(Datos.num$edad~Datos.num$d3, ylab = 'Edad', xlab='D3: Salir fuera de casa?', col=color2) ; table(Datos.num$d3)
boxplot(Datos.num$edad~Datos.num$d4,ylab = 'Edad', xlab='D4: Caminar distancias cortas tales como una cuadra o 100 metros?', col=color2) ; table(Datos.num$d4)
boxplot(Datos.num$edad~Datos.num$d5,ylab = 'Edad',xlab = 'D5: Caminar diez cuadras o un kilómetro?', col=color2) ; table(Datos.num$d5)
boxplot(Datos.num$edad~Datos.num$d6,ylab = 'Edad', xlab='D6: Realizar actividades físicamente exigentes, como jugar fútbol, levantar objetos pesados,andar en bicicleta o correr?', col=color2); table(Datos.num$d6) 
boxplot(Datos.num$edad~Datos.num$d7, ylab = 'Edad', xlab='D7: Llegar a los lugares donde ha querido ir?', col=color2) ; table(Datos.num$d7)
boxplot(Datos.num$edad~Datos.num$d8,ylab = 'Edad', xlab='D8: Manipular objetos pequeños o abrir un envase?', col=color2) ; table(Datos.num$d8)
boxplot(Datos.num$edad~Datos.num$d9,ylab = 'Edad',xlab = 'D9: Levantar una botella de agua de dos litros llena, desde la cintura hasta el nivel de los ojos?', col=color2) ; table(Datos.num$d9)
par(mfrow = c(3, 3))
boxplot(Datos.num$edad~Datos.num$d10,ylab = 'Edad', xlab='D10: Asearse o vestirse?', col=color2); table(Datos.num$d10) 
boxplot(Datos.num$edad~Datos.num$d11, ylab = 'Edad', xlab='D11: Alimentarse?', col=color2) ; table(Datos.num$d11)
boxplot(Datos.num$edad~Datos.num$d12,ylab = 'Edad', xlab='D12: Utilizar el baño (W.C.)?', col=color2) ; table(Datos.num$d12)
boxplot(Datos.num$edad~Datos.num$d13,ylab = 'Edad', xlab='D13: Cortarse las uñas de los pies?', col=color2) ; table(Datos.num$d13)
boxplot(Datos.num$edad~Datos.num$d14,ylab = 'Edad', xlab='D14: Cuidar de su salud?', col=color2) ; table(Datos.num$d14)
boxplot(Datos.num$edad~Datos.num$d15,ylab = 'Edad', xlab='D15: Ver objetos de lejos?', col=color2) ; table(Datos.num$d15)
boxplot(Datos.num$edad~Datos.num$d16,ylab = 'Edad', xlab='D16: Ver objetos de cerca?', col=color2) ; table(Datos.num$d16)
boxplot(Datos.num$edad~Datos.num$d17,ylab = 'Edad', xlab='D17: Oír una conversación en un lugar silencioso?', col=color2) ; table(Datos.num$d17)
boxplot(Datos.num$edad~Datos.num$d18,ylab = 'Edad', xlab='D18:  Oír una conversación en una habitación con ruido alrededor?', col=color2) ; table(Datos.num$d18)
x11()
par(mfrow = c(3, 3))
boxplot(Datos.num$edad~Datos.num$d19,ylab = 'Edad', xlab='D19: Sentir algún dolor físico.Ejemplo dolor de: espalda,estómago ocabeza?', col=color2) ; table(Datos.num$d19)
boxplot(Datos.num$edad~Datos.num$d20,ylab = 'Edad', xlab='D20: Dormir?', col=color2) ; table(Datos.num$d20)
boxplot(Datos.num$edad~Datos.num$d21,ylab = 'Edad',xlab = 'D21: Sentirse cansado y no tener suficiente energía?', col=color2) ; table(Datos.num$d21)
boxplot(Datos.num$edad~Datos.num$d22,ylab = 'Edad',xlab = 'D22: Sentir falta de aire?', col=color2) ; table(Datos.num$d22)
boxplot(Datos.num$edad~Datos.num$d23,ylab = 'Edad',xlab = 'D23: Toser o respirar con dificultad?', col=color2) ; table(Datos.num$d23)
boxplot(Datos.num$edad~Datos.num$d24,ylab = 'Edad',xlab = 'D24: Sentirse triste, bajo de ánimo o deprimido/a?', col=color2) ; table(Datos.num$d24)
boxplot(Datos.num$edad~Datos.num$d25,ylab = 'Edad',xlab = 'D25: Sentir preocupación, nerviosismo o ansiedad?',angle = 20, col=color2) ; table(Datos.num$d25)
boxplot(Datos.num$edad~Datos.num$d26,ylab = 'Edad',xlab = 'D26: llevarser bien con personas cercanas?, incluyendo familia y amiggos?', col=color2) ; table(Datos.num$d26)
boxplot(Datos.num$edad~Datos.num$d27,ylab = 'Edad',xlab = 'D27: Relacionarse con personas que no conoce?', col=color2) ; table(Datos.num$d27)
x11()
par(mfrow = c(3, 3))
boxplot(Datos.num$edad~Datos.num$d28,ylab = 'Edad',xlab = 'D28: Hacer nuevas amistades o mantener las actuales?', col=color2) ; table(Datos.num$d28)
boxplot(Datos.num$edad~Datos.num$d29,ylab = 'Edad',xlab = 'D29: Tener una relación de pareja?', col=color2) ; table(Datos.num$d29)
boxplot(Datos.num$edad~Datos.num$d30,ylab = 'Edad',xlab = 'D30: Manejar el estrés?', col=color2) ; table(Datos.num$d30)
boxplot(Datos.num$edad~Datos.num$d31,ylab = 'Edad',xlab = 'D31: Enfrentar todas las tareas que tiene que hacer?', col=color2) ; table(Datos.num$d31)
boxplot(Datos.num$edad~Datos.num$d32,ylab = 'Edad',xlab = 'D32: Ser entendido, utilizando su lenguaje habitual?', col=color2) ; table(Datos.num$d32)
boxplot(Datos.num$edad~Datos.num$d33,ylab = 'Edad',xlab = 'D33: Entender a otros, utilizando su lenguaje habitual?', col=color2) ; table(Datos.num$d33)
boxplot(Datos.num$edad~Datos.num$d34,ylab = 'Edad',xlab = 'D34: Olvidar cosas/tener mala memoria?', col=color2) ; table(Datos.num$d34)
boxplot(Datos.num$edad~Datos.num$d35,ylab = 'Edad',xlab = 'D35: Recordar las cosas importantes que tiene que hacer en su día a día?', col=color2) ; table(Datos.num$d35)
boxplot(Datos.num$edad~Datos.num$d36,ylab = 'Edad',xlab = 'D36: Encontrar soluciones a los problemas del día a día?', col=color2) ; table(Datos.num$d36)
x11()
par(mfrow = c(3, 3))
boxplot(Datos.num$edad~Datos.num$d37,ylab = 'Edad',xlab = 'D37: Completar las tareas de la casa como:barrer,cocinar,hacer arreglos o sacar la basura?', col=color2) ; table(Datos.num$d37)
boxplot(Datos.num$edad~Datos.num$d38,ylab = 'Edad',xlab = 'D38: Administrar el dinero que posee?', col=color2) ; table(Datos.num$d38)
boxplot(Datos.num$edad~Datos.num$d39,ylab = 'Edad',xlab = 'D39: Hacer cosas que lo relajen o que lo hagan disfrutar?', col=color2) ; table(Datos.num$d39)
boxplot(Datos.num$edad~Datos.num$d40,ylab = 'Edad',xlab = 'D40: Participar en: fiestas, eventos religiosos, reuniones vecinales u otras actividades comunales?', col=color2) ; table(Datos.num$d40)
boxplot(Datos.num$edad~Datos.num$d41,ylab = 'Edad',xlab = 'D41: Participar en la política local o nacional y en organizaciones de la sociedad civil?', col=color2) ; table(Datos.num$d41)
boxplot(Datos.num$edad~Datos.num$d42,ylab = 'Edad',xlab = 'D42: Cuidar o dar apoyo a otros?', col=color2) ; table(Datos.num$d42)
boxplot(Datos.num$edad~Datos.num$d43,ylab = 'Edad',xlab = 'D43: Postular o conseguir un empleo?', col=color2) ; table(Datos.num$d43)
boxplot(Datos.num$edad~Datos.num$d44,ylab = 'Edad',xlab = 'D44: Acceder a una educación superior? Ejemplos: CFT, IP, Universidad', col=color2) ; table(Datos.num$d44)
boxplot(Datos.num$edad~Datos.num$d45,ylab = 'Edad',xlab = 'D45: Utilizar los servicios de transporte público?', col=color2) ; table(Datos.num$d45)
x11()
par(mfrow = c(1, 2))
boxplot(Datos.num$edad~Datos.num$d46,ylab = 'Edad',xlab = 'D46: Realizar las tareas que le solicitan en su empleo?', col=color2) ; table(Datos.num$d46)
boxplot(Datos.num$edad~Datos.num$d47,ylab = 'Edad',xlab = 'D47: Realizar las tareas que le solicitan en su establecimiento educacional?', col=color2) ; table(Datos.num$d47)


###FACTORES AMBIENTALES----
color3<-c("#b7ffa3", "#8bd37d", "#5fa857", "#327c30", "#06500a")
x11()
par(mfrow = c(3, 3))
boxplot(Datos.num$edad~Datos.num$fa1,ylab = 'Edad',xlab='fa1:le resulta fácil o difícil utilizar los servicios sanitarios que necesita con regularidad?',cex=0.02,col=color3) ; table(Datos.num$fa1)
boxplot(Datos.num$edad~Datos.num$fa2,ylab = 'Edad',xlab='fa2:¿le resultan fáciles o difíciles los lugares donde se relaciona y participa en actividades comunitarias?',cex=0.02,col=color3) ; table(Datos.num$fa2)
boxplot(Datos.num$edad~Datos.num$fa3,ylab = 'Edad',xlab='fa3:¿le resulta fácil o difícil utilizar las tiendas, bancos y oficinas de correos de su barrio?',cex=0.02,col=color3) ; table(Datos.num$fa3)
boxplot(Datos.num$edad~Datos.num$fa4,ylab = 'Edad',xlab='fa4:¿le resulta fácil o difícil practicar su religión en los lugares de culto que frecuenta habitualmente?',cex=0.02,col=color3) ; table(Datos.num$fa4)
boxplot(Datos.num$edad~Datos.num$fa5,ylab = 'Edad',xlab='fa5:los medios de transporte que necesita o desea utilizar, ¿le resultan fáciles o difíciles de utilizar?',cex=0.02,col=color3) ; table(Datos.num$fa5)
boxplot(Datos.num$edad~Datos.num$fa6,ylab = 'Edad',xlab='fa6:¿le resulta fácil o difícil vivir en su vivienda (incluido el aseo y todas las habitaciones)?',col=color3) ; table(Datos.num$fa6)
boxplot(Datos.num$edad~Datos.num$fa7,ylab = 'Edad',xlab='fa7:¿le facilitan o dificultan la vida la temperatura, el terreno y el clima del lugar donde vive habitualmente?',col=color3) ; table(Datos.num$fa7)
boxplot(Datos.num$edad~Datos.num$fa8,ylab = 'Edad',xlab='fa8:¿la iluminación de su entorno le facilita o le dificulta la vida?',col=color3) ; table(Datos.num$fa8)
boxplot(Datos.num$edad~Datos.num$fa9,ylab = 'Edad',xlab='fa9:¿el ruido de su entorno le facilita o le dificulta vivir en él?',col=color3) ; table(Datos.num$fa9)
x11()
boxplot(Datos.num$edad~Datos.num$fa10,ylab = 'Edad',xlab='fa10:¿la aglomeración de gente en su entorno le facilita o le dificulta vivir allí?',col=color3) ; table(Datos.num$fa10)


boxplot(Datos.num$edad~Datos.num$fa11,ylab = 'Edad',xlab='fa11:¿le resulta fácil o difícil trabajar o aprender en su lugar de trabajo?',col=color3) ; table(Datos.num$fa11)
boxplot(Datos.num$edad~Datos.num$fa12,ylab = 'Edad',xlab='fa12:¿le resulta fácil o difícil trabajar o aprender en su centro de estudios?',col=color3) ; table(Datos.num$fa12)

calcular_frecuencia_relativa <- function(columna) {
  frecuencia_absoluta <- table(columna)  # Calcula las frecuencias absolutas
  frecuencia_relativa <- frecuencia_absoluta / length(columna)  # Calcula las frecuencias relativas
  return(frecuencia_relativa)
}
frecuencias_relativas <- sapply(Datos.num.incomp, calcular_frecuencia_relativa)

