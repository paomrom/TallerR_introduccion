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
library(readxl)

#Importar o cargar base de datos

#Usaremos datos del Secretariado
base_2019<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQtGZGfusH-8gVTDx351hoy9T1FSZDVfITstcyH8ZON-Gr6mShC2_gZK-WZ7dEoWg/pub?output=csv", encoding = "UTF-8")
base_2020<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRx9Da_tvna2OHi09C0AFGOU-8Fb0Xvwrc7U3DwkijG8DRkbFHWWu7jwrQC1-JUgA/pub?output=csv", encoding = "UTF-8")
base_2021<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS0GyWuFlp67kBlSFcGQVb7_FlJLbShcMmEmfwlJiD7nemW6QH7taX-u7TZyhUcUg/pub?output=csv", encoding = "UTF-8")
base_2022<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR6i_-5QdIXGnUMSmB45vRI4pJNML_hFND0YmY-UPDgK61OV_tvzWdkre-xFhk9Hw/pub?output=csv", encoding = "UTF-8")

#Descargar los archivos
base_2019 <- read_excel("G:/Mi unidad/Almacenamiento/Rstudio/Material/Sesion 7/2019.xlsx")
base_2020 <- read_excel("G:/Mi unidad/Almacenamiento/Rstudio/Material/Sesion 7/2020.xlsx")
base_2021 <- read_excel("G:/Mi unidad/Almacenamiento/Rstudio/Material/Sesion 7/2021.xlsx")
base_2022 <- read_excel("G:/Mi unidad/Almacenamiento/Rstudio/Material/Sesion 7/2022.xlsx")

#Ahora son carpetas no víctimas, ojo!

#Dimensión de filas y columnas
dim(base_2019)

#Nombre de columnas - 21 variables
colnames(base_2019)

# Valores único de delitos que reportan por carpetas
unique(base_2019$`Tipo de delito`)

#Ya no son 13 ahora son 40 :o

#Que es lo chido de esta base se tiene desagregado por municipio :D
unique(base_2019$Municipio)

#¿Qué haremos hoy?

#Analizar la incidencia delictiva de los delitos de Abuso sexual, Feminicidio, Violación y Violencia familiar
#Por municipio en Jalisco


#Iniciamos con la base 2019

#Homologamos los dos tipos de Violación, renombramos como lo vimos en la sesión 4

#Año 2019
base_2019$`Tipo de delito`[base_2019$`Tipo de delito`=="Violación equiparada"] <- "Violación"
base_2019$`Tipo de delito`[base_2019$`Tipo de delito`=="Violación simple"] <- "Violación"

#Año 2020
base_2020$`Tipo de delito`[base_2020$`Tipo de delito`=="Violación equiparada"] <- "Violación"
base_2020$`Tipo de delito`[base_2020$`Tipo de delito`=="Violación simple"] <- "Violación"


#Año 2021
base_2021$`Tipo de delito`[base_2021$`Tipo de delito`=="Violación equiparada"] <- "Violación"
base_2021$`Tipo de delito`[base_2021$`Tipo de delito`=="Violación simple"] <- "Violación"


#Año 2022
base_2022$`Tipo de delito`[base_2022$`Tipo de delito`=="Violación equiparada"] <- "Violación"
base_2022$`Tipo de delito`[base_2022$`Tipo de delito`=="Violación simple"] <- "Violación"


#Vector con los delitos de interes
delitos<-c("Abuso sexual", "Feminicidio", "Violación", "Violencia familiar")


#Filtar para 2019 los delitos de interés en Jalisco

base_2019 %>% 
  filter(Entidad=="Jalisco",
         `Tipo de delito` %in% delitos) %>% 
  group_by(Año,`Tipo de delito`, Municipio) %>% 
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
                        sep+oct+nov+dic))->Municipal_jal_2019


#Filtar para 2020 los delitos de interés en Jalisco

base_2020 %>% 
  filter(Entidad=="Jalisco",
         `Tipo de delito` %in% delitos) %>% 
  group_by(Año,`Tipo de delito`, Municipio) %>% 
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
                        sep+oct+nov+dic))->Municipal_jal_2020


#Filtar para 2021 los delitos de interés en Jalisco

base_2021 %>% 
  filter(Entidad=="Jalisco",
         `Tipo de delito` %in% delitos) %>% 
  group_by(Año, `Tipo de delito`, Municipio) %>% 
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
                        sep+oct+nov+dic))->Municipal_jal_2021


#Filtar para 2022 los delitos de interés en Jalisco

base_2022 %>% 
  filter(Entidad=="Jalisco",
         `Tipo de delito` %in% delitos) %>% 
  group_by(Año, `Tipo de delito`, Municipio) %>% 
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
                        sep+oct+nov+dic)) ->Municipal_jal_2022



#Unir los 4 objetos para tener todo el histórico desde 2019 hasta julio 2022

# Deben tener el mismo nombre y número de columnas

rbind(Municipal_jal_2019, Municipal_jal_2020, Municipal_jal_2021, Municipal_jal_2022)-> base_jalisco

write.csv(Municipal_jal_2022, "base_jalisco_delitos.csv", fileEncoding="UTF8")

#Exportar en formato xlsx o excel
library(openxlsx)

write.xlsx(Municipal_jal_2022, "base_jalisco.xlsx")


#Formato PIVOT de base para hacer bases anchas a largas
#Se reduce variables

base_jalisco %>% 
  pivot_longer("ene":"dic", names_to = "Mes")->base_jalisco_largo

#Gráfico por facet de año violencia familiar

#Agrupar y sumar violencia familiar
base_jalisco_largo %>% 
  filter(`Tipo de delito`=="Violencia familiar") %>% 
  group_by(Mes, Año) %>% 
  summarise(total=sum(value))->base_vf

#Gráfico de barra
ggplot(data=base_vf) +
  aes(x = Mes, y = total) +
  geom_col(fill = "#8e39a8") +
  theme_minimal() +
  geom_text(aes(label = total, vjust=-0.2))+
  labs(title = "Registros de violencia familiar en la entidad de Jalisco",
       subtitle = "Periodo 2019 - julio 2022",
       caption = "Elaborado con los datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)")+
  ylab("Total de carpetas por violencia familiar") + 
  xlab("")+
  facet_wrap(vars(Año))



#Tratar meses a factor
base_vf <- base_vf %>%
  mutate(Mes=factor(Mes,
                    levels=c("ene", "feb", "mar", "abr", "may", "jun", "jul",
                             "ago", "sep", "oct", "nov", "dic")))

#Volver a ejecutar el gráfico


#Gráfico por facet de año asi

#Agrupas y sumar datos
base_jalisco_largo %>% 
  filter(`Tipo de delito`=="Abuso sexual") %>% 
  group_by(Mes, Año) %>% 
  summarise(total=sum(value))->asi




#Gráfico de barra asi
ggplot(asi) +
  aes(x = Mes, y = total) +
  geom_col(fill = "#a61f70") +
  theme_minimal() +
  geom_text(aes(label = total, vjust=-0.2))+
  labs(title = "Registros de abuso sexual en la entidad de Jalisco",
       subtitle = "Periodo 2019 - julio 2022",
       caption = "Elaborado con los datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)")+
  ylab("Total de carpetas por abuso sexual") + 
  xlab("")+
  facet_wrap(vars(Año))



#Tratar meses a factor
asi <- asi %>%
  mutate(Mes=factor(Mes,
                    levels=c("ene", "feb", "mar", "abr", "may", "jun", "jul",
                             "ago", "sep", "oct", "nov", "dic")))


#ASI anual
asi %>% 
  group_by(Año) %>% 
  summarise(anual=sum(total))->asi_anual

#Grfáfico lollipop
ggplot(asi_anual)+
  aes(x=Año, y=anual)+
  geom_segment( aes(x=Año, xend=Año, y=0, yend=anual)) +
  geom_point( size=5, color="#a61f70", fill=alpha("darkslateblue", 0.3), alpha=0.7, shape=21, stroke=5)+
  labs(title="Datos anuales de abuso sexual en Jalisco",
       caption = "Elaboración propia, con datos del SESNSP")+
  theme_minimal()


#Gráfico linea puntos

asi_anual %>%
  ggplot(aes(x=Año, y=anual)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6)+
  theme_minimal()+
  labs(title="Abuso sexual en Jalisco",
       caption= "Con datos de SESNSP")+
  ylab("Registro de carpetas")+
  xlab("")


base_jalisco %>% 
  group_by(`Tipo de delito`) %>% 
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

openxlsx::write.xlsx(base_jalisco, "base_jalisco2.xlsx")
