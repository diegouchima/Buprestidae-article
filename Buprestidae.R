library (dplyr)
library(tidyr)
library("readxl") #leer xlsx
library("ggplot2")
library (pivottabler)
getwd() #poner el puesto este como localidad de trabajo
bu <- read_excel("buprestidae_analisis_v4.xlsx", sheet = 1)
bu<-bu[-(15:18)] #base de datos general (elimino las columnas que sobran)
bu[is.na(bu)] <- "not_avaliable" #cambio los NA a not_avaliable
bu
head(bu)


#analisis especies 
sp<-bu%>%
  filter(categoria =="Especie")
head(sp)


#G1 Grafico abundancia generos moscas de las flores
#entero
G1<-ggplot(data=sp, aes(x=genero, y=conteo, fill=genero)) +
  geom_bar(stat="identity")+
  labs(title="Especies de buprestidae en Colombia"
       , legend.title="Protocolo de Muestreo",
       x="Genero",
       y="Numero de especies",
       caption="GEUA") +
  facet_wrap(~subfamilia, ncol = 1)+ #separar en dos graficos por complejo
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle =90, size = 8,color="black", face="italic"),
        legend.text = element_text(colour="black"),
  )
G1

#dividido
G1<-ggplot(data=sp, aes(x=reorder(genero,genero,function(x)-length(x)), y=conteo, fill=subfamilia)) +
geom_bar(stat="identity")+
  labs(title="Especies de buprestidae en Colombia"
       , legend.title="Protocolo de Muestreo",
       x="Genero",
       y="Numero de especies",
       caption="GEUA") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle =90, size = 8,color="black", face="italic"),
        legend.text = element_text(colour="black"),
  )
G1
#sin los ceros
G1<-ggplot(data=sp, aes(x=reorder(genero,genero,function(x)-length(x)), y=conteo )) + # se elimina fill=subfamilia para ponerlo en gris
  geom_bar(stat="identity")+
  labs(title="Especies de buprestidae en Colombia"
       , legend.title="Protocolo de Muestreo",
       x="Genero",
       y="Numero de especies",
       caption="GEUA") +
  theme_bw()+
  facet_grid(.~subfamilia,scales="free")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle =90, size = 8,color="black", face="italic"),
        legend.text = element_text(colour="black"),
  )
G1 + theme(axis.text.x=element_text(size=9, angle=90,hjust=0.95,vjust=0.2)) # esta parte justifica el texto

# netamente regiones 
G5<-ggplot(data=sp, aes(x=reorder(genero,genero,function(x)-length(x)), y=conteo)) +  # se elimina fill=subfamilia para ponerlo en gris
  geom_bar(stat="identity")+
  labs(title="Especies de buprestidae en Colombia"
       , legend.title="Protocolo de Muestreo",
       x="Genero",
       y="Numero de especies",
       caption="GEUA") +
  theme_bw()+
  facet_grid(.~subfamilia,scales="free")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle =90, size = 8,color="black", face="italic"),
        legend.text = element_text(colour="black"),
  )
G5


#ENdemismo
G2<-ggplot(data=sp, aes(x=endemismo, y=conteo, fill = factor(localidadespecifica))) +
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c("1" = "gray0",
                               "0" = "gray50"))+
  labs(title="Especies de buprestidae en Colombia"
       , legend.title="Protocolo de Muestreo",
       x="Endemismo",
       y="Numero de especies",
       caption="GEUA") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle =90, size = 8,color="black", face="italic"),
        legend.text = element_text(colour="black"))
G2

## regiones diversidad
bu <- read_excel("buprestidae_analisis_v4.xlsx", sheet = 3)
b<-copdat %>%
  gather(area,abundancia,"Pacifico", "Ninguna","Caribe", "Insular", "andes", "orinoquia","amazonas")

G3<-ggplot(data=b, aes(x=reorder(area,-abundancia), y=abundancia)) +  # se elimina fill=area para ponerlo en gris
  geom_histogram(position="stack", stat="identity")+
  labs(title="Especies de buprestidae en Colombia"
       , legend.title="Protocolo de Muestreo",
       x="Endemismo",
       y="Numero de especies",
       caption="GEUA") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle =90, size = 8,color="black", face="italic"),
        legend.text = element_text(colour="black"),
  )
G3






## Curva de rarefaccion utilizando individuos

#comunidad<-copdat
comunidad <- read_excel("buprestidae_analisis_v4.xlsx", sheet = "rarefaccion")

tot<-rarefy(comunidad[,2],sample=c(1:sum(comunidad$total)),se=T) 
plot(tot[1,],type="l",col="black",lwd = 1.5,
     xlab="Esfuerzo de muestreo en especies",
     ylab="G?neros encontrados",
     main = "Curva de rarefaccion para géneros de Buprestidae 
     en Colombia"
)
col1=rgb(red=255, green=0, blue=0, alpha=70, maxColorValue=255) 
arrows(x0=c(1:sum(comunidad$total)),y0=tot[1,],x1=c(1:sum(comunidad$total)),y1=tot[1,]+tot[2,],length=0, col=col1)
arrows(x0=c(1:sum(comunidad$total)),y0=tot[1,],x1=c(1:sum(comunidad$total)),y1=tot[1,]-tot[2,],length=0, col=col1)

#Curva rarefaccion con INEXT
library(iNEXT)
out.raw <- iNEXT(comunidad [2], datatype="abundance", endpoint=800, nboot=1000)
ggiNEXT(out.raw)


matriz_linea_tiempo

## linea de tiempo
copdat <- read.delim("clipboard",header=TRUE) #copio y pego la hoja de matriz linea de tiempo
copdat<- read_excel("buprestidae_analisis_v4.xlsx", sheet = "matriz_linea_tiempo")



da<-copdat %>% #FUSIONO LAS COLUMNAS DE LAS FECHAS
  gather(inicio,abundancia, 2:24)
da$inicio<-gsub("X", "", da$inicio)
da$inicio<-gsub("-", "/", da$inicio) # cambio el guion por por el slash

## convierto el eje a una fecha tener en euenta cual es es separador (/ o -) de ANO-MES-DIA
da[['inicio']] <- as.POSIXct(da[['inicio']],format = "%Y/%m/%d")

o<-da %>% # agrupn los datos por fecha
  group_by(inicio) %>%
  summarise_at(vars("abundancia"), sum)

G4<-o %>%
  ggplot(aes(x=inicio, y=abundancia,group=1)) +
  geom_line(data=o[!is.na(o$abundancia),])+
  geom_point()+
  labs(title="Variacion temporal de descripciones de Buprestidae Para Colombia",
       x="Salida",
       y="Especies descritas",
       caption="GEUA")+
  theme_minimal() +
  geom_smooth(se=F) #agrego linea detendencia
G4 + scale_x_date(date_breaks = "20 years", date_labels =  "%Y", limit=c(as.Date("1744-01-01"),as.Date("2020-01-01"))) +
  theme(axis.text.x = element_text(angle =90, size = 8,face="bold", color="black", hjust = 0.95, vjust = 0.2))




#curvas de acumulaci?n  para los paramos con ggplot
sa<- read_excel("buprestidae_analisis_v4.xlsx", sheet = "curva_acum_decada")#aca copio y pego la matriz de abundancia de la hoja de hoja_curva_acum_decada sin los titulos de filas ni columnas 
sa[is.na(sa)] <- 0
sa<-sa[-1] #quito la primera columna que no es numerica
curvasalidas<-specaccum(sa, method = "exact", permutations = 100,
                        conditioned =TRUE, gamma = "chao",  w = NULL) #usar Chao 1 para matrices de abundancias.

salidas<-t(sa) #la volteo de manera que queden las decadas de unidad muestreal 23 aprox
salidas

curva<-specaccum(salidas, method = "exact", permutations = 100,
                 conditioned =TRUE, gamma = "chao",  w = NULL) #usar Chao 1 para matrices de abundancias.

curva <- data.frame(parcela = curva$sites,
                    riqueza = curva$richness,
                    des_sta = curva$sd) 
curva

curva %>% 
  ggplot(aes(parcela, riqueza))+
  geom_line(linetype = 1, size = .5, color = "blue") +
  geom_errorbar(aes(ymin = riqueza - des_sta, ymax = riqueza + des_sta),
                width = 0.5) + geom_line() +
  geom_point() +
  xlab("Decadas de muestreo")+  ylab("Riqueza de generos") + theme(plot.title = element_text(hjust = .5, 
                                                                                             face = "bold"),
                                                                   legend.justification=c(0,1), 
                                                                   legend.position=c(.8, 1))+
  labs( color = "", fill="")+
  ggtitle("Curva de acumulacion de 
          generos de buprestidae para Colombia")
