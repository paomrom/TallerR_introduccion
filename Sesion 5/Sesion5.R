#Datos de incidencia delictiva del Secretariado ejecutivo

#Instalar paqueterías o librerias
install.packages("dplyr") 
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("esquisse")

#Cargar librerias
library(dplyr)   # Manipulación de datos o funciones anidadas
library(tidyverse)#Paqueterías de tidy
library(ggplot2) # Generación de gráficos 
library(esquisse)# Interfaz para generar gráfico de manera muy sencilla


#Importar o cargar base de datos

#Usaremos datos del Secretariado
victimas<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTrYBvTBhjBzuo3TZBA5xLpp9RFVs4kr76eean0eo6ZOvc9hs_4e09VcDRol8f5RsrOKsABoUCDLzEl/pub?output=csv", encoding = "UTF-8")

#Dimensión de la base filas y columnas
dim(victimas)

#Leer los nombres de las columnas
colnames(victimas)

#Veamos los delitos que integran la base
unique(victimas$Tipo.de.delito)

#Sólo trabajaremos con Feminicidios y Homicidios dolosos de mujeres -filtramos delitos
delitos<- c("Feminicidio", "Homicidio doloso")

#Hacemos sumatoria de los meses por los delitos que corresponden a mujeres en MX
victimas%>% filter(Sexo=="Mujer",
                   Entidad=="Jalisco",
                   Subtipo.de.delito%in%delitos) %>% 
  group_by(Año, Subtipo.de.delito) %>% 
  summarise(ene=sum(Enero, na.rm = T),
            feb=sum(Febrero, na.rm = T),
            mar=sum(Marzo, na.rm = T),
            abr=sum(Abril, na.rm = T),
            may=sum(Mayo, na.rm = T),
            jun=sum(Junio, na.rm = T),
            jul=sum(Julio, na.rm = T),
            ago=sum(Agosto, na.rm = T),
            sep=sum(Septiembre, na.rm = T),
            oct=sum(Octubre, na.rm = T),
            nov=sum(Noviembre, na.rm = T),
            dic=sum(Diciembre, na.rm = T),
            Total=sum(ene+feb+mar+abr+ 
                        may+jun+jul+ago+
                        sep+oct+nov+dic))->victimas_anual


#Muertes de mujeres en México al día "10 mujeres al día son asesinadas en México"
datos %>% group_by(Año) %>% 
  summarise(muertes_dia=sum(Total/dias))

#Crear objeto que incluya los feminicidios en MX por año
fem_anual<-victimas%>% 
  filter(Sexo=="Mujer",
         Subtipo.de.delito=="Feminicidio") %>% 
  group_by(Año) %>% 
  summarise(ene=sum(Enero, na.rm = T),
            feb=sum(Febrero, na.rm = T),
            mar=sum(Marzo, na.rm = T),
            abr=sum(Abril, na.rm = T),
            may=sum(Mayo, na.rm = T),
            jun=sum(Junio, na.rm = T),
            jul=sum(Julio, na.rm = T),
            ago=sum(Agosto, na.rm = T),
            sep=sum(Septiembre, na.rm = T),
            oct=sum(Octubre, na.rm = T),
            nov=sum(Noviembre, na.rm = T),
            dic=sum(Diciembre, na.rm = T),
            Total=sum(ene+feb+mar+abr+ 
                        may+jun+jul+ago+
                        sep+oct+nov+dic))

datos<- victimas_anual %>% 
  mutate(
    dias = case_when(
      Año==2015~365,
      Año==2016~365,
      Año==2017~365,
      Año==2018~365,
      Año==2019~365,
      Año==2020~365,
      Año==2021~365,
      Año==2022~211))
      
      
#Gráfico de columnas de feminicidios
ggplot(data=fem_anual)+
  aes(x=Año, y=Total)+
  geom_col()

#Mostrar valor a cada columna de mi X
fem_anual <- fem_anual %>%
  mutate(Año=factor(Año,
                    levels=c("2015", "2016", "2017", "2018", "2019", "2020", "2021","2022")))

#De nuevo el gráfico anterior
ggplot(fem_anual)+
  aes(x=Año, y=Total)+
  geom_col()

#Añadir relleno o color y etiquetas de valores a las columnas del gráfico
ggplot(fem_anual)+
  aes(x=Año, y=Total)+
  geom_col(fill = "#925cbf")+
  geom_text(aes(y = Total, label = Total,  vjust = -1.5))

#Añadir titulos y subtitulos al gráfico
ggplot(fem_anual)+
  aes(x=Año, y=Total)+
  geom_col(fill = "#925cbf")+
  geom_text(aes(y = Total, label = Total,  vjust = -0.5))+
  labs(title = "Registros de feminicidios por año en México",
       subtitle = "Periodo 2015 - agosto 2022",
       caption = "Elaborado con los datos del Secretariado Ejecutivo del 
       Sistema Nacional de Seguridad Pública (SESNSP)")+
  ylab("Total de registro de feminicidios") + 
  xlab("Año")


#Añadir theme
ggplot(fem_anual)+
  aes(x=Año, y=Total)+
  geom_col(fill = "#925cbf")+
  geom_text(aes(y = Total, label = Total,  vjust = -0.5))+
  labs(title = "Registros de feminicidios por año en México",
       subtitle = "Periodo 2015 - agosto 2022",
       caption = "Elaborado con los datos del Secretariado Ejecutivo del 
       Sistema Nacional de Seguridad Pública (SESNSP)")+
  ylab("Total de registro") + 
  xlab("Año")+
  theme_light()

#Qué se observa una tendencia a la alza en las víctimas de feminicidios en MX

#Gráfico de linea de tendencia
graf_muertes_vio<-ggplot(data=victimas_anual) + #leer datos
  aes(x = Año, y = Total, color = Subtipo.de.delito) + #xy del gráfico
  geom_line(size = 1.5) + #figura linea
  geom_point(size = 2.5)+ #figura puntos
  geom_text(label=victimas_anual$Total, vjust=1.5)+ #etiqueta de valores
  labs(title = "Muertes violentas de mujeres",
       subtitle = "Periodo 2015 - junio 2022",
       caption = "Elaboración propia con datos del SESNSP")+ #Titulos y subtitulos
  theme(legend.position='none', #Sin leyenda
        plot.title=element_text(family='Anton', face='bold',size=20), #formato texto del titulo
        plot.subtitle=element_text(family='Anton'), #formato texto del subtitulo
        axis.title.x = element_text(family='Anton', face='bold', hjust=1), #formato de texto eje x
        axis.title.y = element_text(family='Anton', face='bold', hjust=1)) #formato de texto eje y

library(ggplot2)

ggplot(fem_anual) +
 aes(x = Año, y = Total) +
 geom_col(fill = "#9230AD") +
 labs(title = "Feminicidios en México", 
 subtitle = "Periodo 2015-2022", caption = "Fuente: Elaboración proia") +
 ggthemes::theme_economist()

#####
#Crear objeto que incluya los feminicidios en MX por año
victimas%>% 
  filter(Sexo=="Mujer",
         Subtipo.de.delito=="Feminicidio") %>% 
  group_by(Año) %>% 
  summarise(ene=sum(Enero, na.rm = T),
            feb=sum(Febrero, na.rm = T),
            mar=sum(Marzo, na.rm = T),
            abr=sum(Abril, na.rm = T),
            may=sum(Mayo, na.rm = T),
            jun=sum(Junio, na.rm = T),
            jul=sum(Julio, na.rm = T),
            ago=sum(Agosto, na.rm = T),
            sep=sum(Septiembre, na.rm = T),
            oct=sum(Octubre, na.rm = T),
            nov=sum(Noviembre, na.rm = T),
            dic=sum(Diciembre, na.rm = T),
            Total=sum(ene+feb+mar+abr+ 
                        may+jun+jul+ago+
                        sep+oct+nov+dic))
