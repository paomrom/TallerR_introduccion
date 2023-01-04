# ------------------------- cargar librerías ---------------------------------#


library(dplyr)      # Manipulación de datos o funciones anidadas
library(tidyverse)  # Paqueterías de tidy
library(ggplot2)    # Generación de gráficos 
library(esquisse)   # Interfaz para generar gráfico de manera muy sencilla
library(readxl)     # Cargar archivos excel
library(mxmaps)     # Generación de mapas hexbin y coropletas
library(tidyr)      # Manejo de datos, en este caso usamos gather
library(plotly)     # Vuelve dinámico a los gráaficos
library(ggthemes)   # Añade colores chidos del theme_
library(janitor)    # Transformar las variables a minusculas
library(DT)         # Hacer tablitas sencillas y bonitas
library(scales)     # Permite añadir formato a los numeros como porcentaje, o separación de coma
library(viridis)    # Paleta de color amigable con el daltonismo
library(RColorBrewer) # Paleta de colores
library(devtools)   #Instala paquetrías desarrollada por otrxs


# ------------------- Cargar nuestro archivo de datos -------------------------#

base_2022<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR6i_-5QdIXGnUMSmB45vRI4pJNML_hFND0YmY-UPDgK61OV_tvzWdkre-xFhk9Hw/pub?output=csv", 
                    encoding = "UTF-8")

base_2022 <- setNames(base_2022, tolower(names(base_2022))) #nombres en minusculas con package janitor. 


base_2022 %>% 
gather(mes, carpetas, enero:diciembre) %>% 
  group_by(año,entidad, subtipo.de.delito) %>% 
  summarise(carpetas=sum(carpetas, na.rm = T)) -> base_2022

# Creación de nuestra data de feminicidios estatales
feminicidios <- base_2022 %>% 
  filter(año==2022,
         subtipo.de.delito=="Feminicidio") %>% 
  group_by(entidad, subtipo.de.delito)



# -----------------------------------------------------------------------------#

# Ejemplo con ggplot y maneras de pintar


feminicidios %>% 
  ggplot()+
  aes(x=entidad, y=carpetas, fill=entidad)+
  geom_col()+
  theme_minimal()


# Ejemplo con modificaciones
feminicidios %>% 
  ggplot()+
  aes(x=entidad, y=carpetas, fill=entidad)+
  geom_col()+
  theme_minimal()+
  theme(text=element_text(size=11),
        legend.position='none',
        plot.title = element_text(size = 11L, hjust = 0),
        plot.caption = element_text(size = 10L, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11))



feminicidios %>% 
  ggplot()+
  aes(x=entidad, y=carpetas, fill=entidad)+
  geom_col()+
  #scale_fill_manual(values = rainbow(32)) + #rainbow, se puede cambiar por heat.colors, terrain.colors,
  #scale_fill_viridis(discrete = T, option = "G", direction = -1) + #opción  A, B, C , D , E , F ,G
  scale_fill_brewer(name = "Dark2")+
  theme_minimal()+
  theme(text=element_text(size=11),
        legend.position='none',
        plot.title = element_text(size = 11L, hjust = 0),
        plot.caption = element_text(size = 10L, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11))



    # Con scale_fill_brewer(name = "Dark2") no se pintan los 32 estados por lo tanto debemos 
    # extender la paleta para ello los siguientes pasos
  
      nb.cols <-32
      mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols) # aquí en las comillas podemos cambiar por http://www.sthda.com/sthda/RDoc/figure/graphs/colors-in-r-rcolorbrewer-palettes.png


feminicidios %>% 
  ggplot()+
  aes(x=entidad, y=carpetas, fill=entidad)+
  geom_col()+
  #scale_fill_viridis(discrete = F, option = "G", direction = -1) + #opción 1
  scale_fill_manual(values = mycolors) +
  theme_minimal()+
  theme(text=element_text(size=11),
        legend.position='none',
        plot.title = element_text(size = 11L, hjust = 0),
        plot.caption = element_text(size = 10L, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11))





#------------------------------------------------------------------------------#
#------------------------- Generación de mapas mxmaps--------------------------#
#------------------------------------------------------------------------------#


# PASO 1: Esto sólo es para descargar la paquetería, entonces esto sólo se hace una vez

if (!require("devtools")) {
  install.packages("devtools", force=TRUE)}
devtools::install_github("diegovalle/mxmaps")

# PASO 2: Cargar la paquetería
library(mxmaps)

# PASO 3: cargar df_mxstate_2020
# De mxmaps el archivo de "df_mxstate_2020" que contiene el resumen del CENSO 2020
data("df_mxstate_2020") 

mxmaps::df_mxstate_2020


# Visualización simple de las distintas capas.
# Parte de las características de "mxmaps" recae en su base, para poder georreferenciar
# con esta paquetería es necesario que los datos que queremos pintar se encuentre en la variable:
# de df_mxstate_2020$value.

# Ejemplo cambiamos el nombre de la variable "pop" de la base de_mxstate_2020
df_mxstate_2020$value <- df_mxstate_2020$pop


# Ejemplos sencillos
mxstate_choropleth(df_mxstate_2020, 
                   num_colors = 3,
                   title = "Ejemplo: Poblacion 1",
                   legend = "Poblacion total")



mxhexbin_choropleth(df_mxstate_2020, 
                    num_colors=7,
                    legend = "Poblacion total")+ 
  labs(title = "Ejemplo: Poblacion total.",
       subtitle = "Mapa hexagonal")+
  scale_fill_viridis_d(option = "D")




# PASO 4: Crear un merge entre nuestra base de femincidio y la df_mxstate_2020

merge <- merge(feminicidios, df_mxstate_2020, 
               by.x="entidad", by.y="state_name_official")


# PASO 5: Creamos la variable de feminicidios por cada 100 mil habitantes.
merge %>% 
  group_by(entidad) %>% 
  summarise(Tasa= (carpetas/pop)*100000) -> merge_tasa


# PASO 6: Volvemos a juntar el merge anterior con la base de mxmaps
merge(df_mxstate_2020, merge_tasa, 
                    by.x="state_name_official", 
                    by.y="entidad") -> merge_final

# PASO 7: Cambiamos el nombre de nuestra variable de interes por "value"
merge_final$value <- merge_final$Tasa


# PASO 8: Crear mapa hexagonal
mxhexbin_choropleth(merge_final) +  
  labs(title="Feminicidios por cada 100 mil habitantes por entidad en México, 2020",
       fill="Tasa") +
  theme_minimal() 


# Añadimos más características de diseño
mxhexbin_choropleth(merge_final, num_colors = 1) +  
  labs(title="Feminicidios por cada 100 mil habitantes por entidad en México", 
       caption="Elaborado con base al SESNSP, agosto 2022.",
       fill="Tasa") +
  scale_fill_gradient(
  
    low = "#e8f1e2", 
    high = "#7E3794",
    guide = "colourbar")+
  theme_minimal()+
  theme(legend.position = "right",
        text=element_text(size=12),
        plot.title = element_text(size = 18L, hjust = 0), 
        plot.caption = element_text(size = 12L, hjust = 0),
        panel.background = element_rect(fill="white", colour = "white"))


ggsave(mapa_1, filename = "mapa_tasa_feminicidio.png")



#-----------------------------------------------------------------------------#
#------------------Hacer mapa municipal de feminicidios---------------------- #
#-----------------------------------------------------------------------------#



# PASO 1: Cargar archivo de y hacer manipulación para obtener los datos de feminicidios
base_2022 %>%
  filter(`Subtipo de delito`=="Feminicidio",
         Entidad=="Jalisco") %>% 
  gather(Mes, Carpetas, Enero:Diciembre) %>% 
  group_by(Año, Municipio) %>% 
  summarise(value=sum(Carpetas, na.rm = T)) %>% 
  arrange(-value)->df_mxmunicipio_2022_jal


# PASO 2: Cargar la base del CENSO 2020 de mxmaps
df_mxmunicipio_2020_jal <- df_mxmunicipio_2020 %>% 
  filter(state_name=="Jalisco")


# PASO 3: Hacer nuestro merge del censo y datos de feminicidios
df_mun <- merge(df_mxmunicipio_2020, df_mxmunicipio_2022_jal, 
          by.x="municipio_name", by.y="Municipio")


#PASO 4: Crear nuestro mapa municipal

mxmunicipio_choropleth(df_mun, num_colors = 1,
                       zoom = subset(df_mun, state_name %in% 
                                       c("Jalisco"))$region) +  
  labs(title="Feminicidios totales durante 2022 a agosto", 
       fill="Total") +
  # scale_fill_viridis_c(option = "F", direction = -1, alpha = .7)+
  scale_fill_gradient(
    low = "#decfe3",
    medium="#FFFFF",
    high = "#7E3794")+
  theme_minimal()+
  theme(legend.position = "right",
        text=element_text(size=12),
        plot.title = element_text(size = 18L, hjust = 0), 
        plot.caption = element_text(size = 12L, hjust = 0))#->mapa_2


        # Otra manera de asignar color es con:
        # scale_fill_gradient(
        #   low = "#e8f1e2", 
        #   high = "#7E3794",
        #   guide = "colourbar")


# PASO 5: Guardar la imagen del mapa.
ggsave(mapa_2, filename = "mapa_mun_feminicidio.png")




# -----------------------------------------------------------------------------#
# ----------------------------Crear tablas-------------------------------------#
# -----------------------------------------------------------------------------#

base_2022_mun %>% 
  datatable(
    filter = 'top',
    extensions = 'Buttons',
    options = list(
      dom = "tip",#'Blfrtip',
      buttons = c('copy', 'excel', 'print'),
      lengthMenu = list(c(10,10,50,100, 150, "All"),
                        c(10,10,50,100, 150, "All")),
      columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>% 
  formatStyle(
    columns = c(1:3),
    fontFamily = "Montserrat",
    fontSize = "12px",
    color = '#008080',
    textAlign = "center")


###############################################################################


merge_regional %>% 
  filter(Subtipo.de.delito=="Abuso sexual") %>% 
  group_by(Región, Año) %>% 
  summarise(Total_regional=sum(value)) %>% 
  arrange(-Total_regional) %>% 
  pivot_wider(names_from = Año, values_from = Total_regional)->regional_delito

merge_regional %>% 
  filter(Subtipo.de.delito=="Abuso sexual") %>% 
  group_by(Año) %>% 
  summarise(Total_regional=sum(value)) %>%
  pivot_wider(names_from = Año, values_from = Total_regional)-> entidad_delito

rbind(regional_delito, entidad_delito)->resumen
resumen[is.na(resumen)] <- "Total entidad"

resumen
