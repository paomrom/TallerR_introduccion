#Instalar paqueterías (se instala una vez)
install.packages("readxl")
install.packages("dplyr")

#Llamamos o cargamos librerias o paqueterias
library(readxl)
library(dplyr)

#Cargamos base de datos - 3 formas

#Cargar datos de un archivo de extension xlsx
datos <- read_excel("Telegram Desktop/ile_completa.xlsx")

install.packages("WriteXLS")
library("WriteXLS")
library("writexl")
library("openxlsx")

write_xlsx(datos, "nombre.xlsx")
write.xlsx(datos, "nombre.xlsx", fileEncoding = "UTF-8")


#Cargar datos de un archivo de extension csv 
datos<-read.csv("C:/Users/Paola Viridiana/Desktop/ile_completa.csv", encoding = "UTF-8")


#Cargas datos desde una una liga o sheet de google drive
datos<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRt8YMj-fA6OqTnSLlLnJoDtfYCE1mbtTwV0y7eds4pb9LG8MRBwUpYQoMuQN85CQ/pub?output=csv", encoding = "UTF-8")


#Visualizar datos importados
View(datos)

# Dimensión número de columnas y filas (observaciones)
dim(datos)

# str() para conocer la estructura de los datos
str(datos)

# el simbolo "$" nos ayuda a especificar una variable o columna de un objeto/base
datos$mes

#Con "unique" vemos los valores unicos
unique(datos$mes)


# Empezamos la limpieza de datos
# Problema detectado: hay observaciones con mayúsculas y minúsculas

datos$mes<-toupper(datos$mes)
datos$edocivil_descripcion<-toupper(datos$edocivil_descripcion)
  
#### PIPE
# ctr + shift + m %>%





################################################################
########################### Hospital ###########################
################################################################

unique(datos$hospital)

datos<- datos %>% 
  mutate(
    HOSPITAL_LIMPIA = case_when(
      hospital=="C. S. T III  Cuajimalpa" ~ "H. CUAJIMALPA",
      hospital=="C. S. T III Cuajimalpa" ~ "H. CUAJIMALPA",
      hospital=="C.S.T III Cuajimalpa" ~"H. CUAJIMALPA",
      hospital=="C.C. Santa Catarina" ~ "H. SANTA CATARINA",
      hospital=="C.S santa catarina" ~"H. SANTA CATARINA",
      hospital=="C.S.T III beatriz velasco de aleman" ~ "H. ALEMAN",
      hospital=="C.S.T III juan duque" ~ "H. JUAN DUQUE",
      hospital=="C.S.T III méxico españa" ~ "H. MÉXICO ESPAÑA",
      hospital=="HG DR. ENRIQUE CABRERA" ~ "H. ENRIQUE CABRERA",
      hospital=="HOSPITAL GENERAL DR. ENRIQUE CABRERA" ~ "H. ENRIQUE CABRERA",
      hospital=="HOSPITAL GENERAL AJUSCO MEDIO" ~ "H. AJUSCO MEDIO",
      hospital=="HOSPITAL GENERAL TICOMAN" ~ "H. TICOMÁN",
      hospital=="HOSPTAL GENERAL DE TICOMÁN" ~ "H. TICOMÁN",
      hospital=="HOSPITAL MATERNO INFANTIL  NICOLAS M. CEDILLO" ~ "H. CEDILLO",
      hospital=="HOSPITAL MATERNO INFANTIL DR. NICOLÃS M. CEDILLO" ~ "H. CEDILLO",
      hospital=="HOSPITAL MATERNO INFANTIL NICOLAS M. CEDILLO" ~ "H. CEDILLO",
      hospital=="HOSPITAL MATERNO INFANTIL CUAUTEPEC" ~ "H. CUAUTEPEC",
      hospital=="HOSPITAL MATERNO INFANTIL INGUARAN" ~ "H. INGUARÁN",
      hospital=="HOSPITAL MATERNO INFANTIL TLAHUAC" ~ "H. TLÁHUAC",
      hospital=="MILPA ALTA" ~ "H. MILPA ALTA",
      hospital=="HOSPITAL MATERNO INFANTIL INGUARAN" ~ "H. INGUARÁN",
      hospital=="HOSPITAL MATERNO PEDIÁTRICO XOCHIMILCO" ~ "H. XOCHIMILCO",
      hospital=="HOSPITAL PEDIATRICO XOCHIMILCO" ~ "H. XOCHIMILCO",
      hospital=="HOSPITAL MATERNO PEDIATRICO XOCHIMILCO" ~ "H. XOCHIMILCO"

      ))

#Validar
unique(datos$HOSPITAL_LIMPIA)


##################################################################################    
########################### descpción del estado civil ###########################
##################################################################################
# Problema: No están agrupados. No están en lenguaje femenino


#Leemos valores unicos como se ve que hay observaciones en minusculas y mayusculas
unique(datos$edocivil_descripcion)      

#Pasar toda la variable a mayúsculas
datos$edocivil_descripcion<- toupper(datos$edocivil_descripcion)


datos<- datos %>%
  mutate(
    EDO_CIVIL_DESCRIPCION_LIMPIA = case_when(
      edocivil_descripcion == "CASADO(A)" ~ "CASADA",
      edocivil_descripcion == "SOLTERA" ~ "SOLTERA",
      edocivil_descripcion == "UNIÓN LIBRE" ~ "UNIÓN LIBRE",
      edocivil_descripcion == "DIVORCIADA" ~ "DIVORCIADA",
      edocivil_descripcion == "CASADA (O)" ~ "CASADA",
      edocivil_descripcion == "UNION LIBRE" ~ "UNIÓN LIBRE",
      edocivil_descripcion == "SOLTERO(A)" ~ "SOLTERA",
      edocivil_descripcion == "DIVORCIADO(A)" ~ "DIVORCIADA",
      edocivil_descripcion == "VIUDO" ~ "VIUDA",
      edocivil_descripcion == "SEPARADO (A)" ~ "SEPARADA",
      edocivil_descripcion == "SOLTERO/A" ~ "SOLTERA",
      edocivil_descripcion == "CASADA" ~ "CASADA",
      edocivil_descripcion == "SOLTERO(A)" ~ "SOLTERA",
      edocivil_descripcion == "SEPARADO/A" ~ "SEPARADA",
      edocivil_descripcion == "DIVORCIADO/A" ~ "DIVORCIADA"
    ))


      

# Escolaridad
unique(datos$nivel_edu)

datos$nivel_edu<- toupper(datos$nivel_edu)

datos<- datos %>%
  mutate(
    NIVEL_EDU_LIMPIA = case_when(
      nivel_edu == "Bachillerato o Preparatoria" ~ "PREPARATORIA",
      nivel_edu == "Bachillerato O Preparatoria Completa" ~ "PREPARATORIA",
      nivel_edu == "Bachillerato o Preparatoria Completa" ~ "PREPARATORIA",
      nivel_edu == "Bachillerato O Preparatoria Incompleta" ~ "SECUNDARIA",
      nivel_edu == "carrera técnica" ~ "LICENCIATURA",
      nivel_edu == "carrera tecnica" ~ "LICENCIATURA",
      nivel_edu == "Carrera Trunca" ~ "PREPARATORIA",
      nivel_edu == "doctorado" ~ "DOCTORADO",
      nivel_edu == "doctorado incompleto" ~ "MAESTRIA",
      nivel_edu == "licenciatura" ~ "LICENCIATURA",
      nivel_edu == "licenciatura completa" ~ "LICENCIATURA",
      nivel_edu == "licenciatura incompleta" ~ "PREPARATORIA",
      nivel_edu == "maestria" ~ "MAESTRIA",
      nivel_edu == "Maestria" ~ "MAESTRIA",
      nivel_edu == "maestria incompleta" ~ "LICENCIATURA",
      nivel_edu == "N/E" ~ "NO ESPECIFICADO",
      nivel_edu == "n/e" ~ "NO ESPECIFICADO",
      nivel_edu == "NINGUNA" ~ "SIN ACCESO A LA EDUCACION FORMAL",
      nivel_edu == "ninguno" ~ "SIN ACCESO A LA EDUCACION FORMAL",
      nivel_edu == "NINGUNO" ~ "SIN ACCESO A LA EDUCACION FORMAL",
      nivel_edu == "Ninguno" ~ "SIN ACCESO A LA EDUCACION FORMAL",
      nivel_edu == "Otra" ~ "NO ESPECIFICADO",
      nivel_edu == "posgrado" ~ "MAESTRIA",
      nivel_edu == "POSGRADO" ~ "MAESTRIA",
      nivel_edu == "posgrado completo" ~ "MAESTRIA",
      nivel_edu == "Postgrado" ~ "MAESTRIA",
      nivel_edu == "preescolar" ~ "SIN ACCESO A LA EDUCACION FORMAL",
      nivel_edu == "PREPARATORIA" ~ "PREPARATORIA",
      nivel_edu == "preparatoria" ~ "PREPARATORIA",
      nivel_edu == "Preparatoria" ~ "PREPARATORIA",
      nivel_edu == "PREPARATORIA COMPLETA" ~ "PREPARATORIA",
      nivel_edu == "preparatoria incompleta" ~ "SECUNDARIA",
      nivel_edu == "Preparatoria InCompleta" ~ "SECUNDARIA",
      nivel_edu == "PREPARATORIA INCOMPLETA" ~ "SECUNDARIA",
      nivel_edu == "preparatoria o bachillerato" ~ "PREPARATORIA",
      nivel_edu == "PREPARATORIA O BACHILLERATO" ~ "PREPARATORIA",
      nivel_edu == "primaria" ~ "PRIMARIA",
      nivel_edu == "PRIMARIA" ~ "PRIMARIA",
      nivel_edu == "Primaria" ~ "PRIMARIA",
      nivel_edu == "PRIMARIA COMPLETA" ~ "PRIMARIA",
      nivel_edu == "Primaria Completa" ~ "PRIMARIA",
      nivel_edu == "primaria incompleta" ~ "SIN ACCESO A LA EDUCACION FORMAL",
      nivel_edu == "Primaria InCompleta" ~ "SIN ACCESO A LA EDUCACION FORMAL",
      nivel_edu == "PRIMARIA INCOMPLETA" ~ "SIN ACCESO A LA EDUCACION FORMAL",
      nivel_edu == "Primaria Incompleta" ~ "SIN ACCESO A LA EDUCACION FORMAL",
      nivel_edu == "PROFEIONAL INCOMPLETA" ~ "PREPARATORIA",
      nivel_edu == "profesional" ~ "LICENCIATURA",
      nivel_edu == "PROFESIONAL" ~ "LICENCIATURA",
      nivel_edu == "Profesional" ~ "LICENCIATURA",
      nivel_edu == "PROFESIONAL INCOMPLETO" ~ "PREPARATORIA",
      nivel_edu == "PROIMARIA COMPLETA" ~ "LICENCIATURA",
      nivel_edu == "secundaria" ~ "SECUNDARIA",
      nivel_edu == "SECUNDARIA" ~ "SECUNDARIA",
      nivel_edu == "Secundaria" ~ "SECUNDARIA",
      nivel_edu == "SECUNDARIA COMPLETA" ~ "SECUNDARIA",
      nivel_edu == "Secundaria Completa" ~ "SECUNDARIA",
      nivel_edu == "SECUNDARIA INCOMPELTA" ~ "PRIMARIA",
      nivel_edu == "secundaria incompleta" ~ "PRIMARIA",
      nivel_edu == "Secundaria InCompleta" ~ "PRIMARIA",
      nivel_edu == "SECUNDARIA INCOMPLETA" ~ "PRIMARIA",
      nivel_edu == "Secundaria Incompleta" ~ "PRIMARIA",
      nivel_edu == "SIN ESPECIFICAR" ~ "NO ESPECIFICADO",
      nivel_edu == "TECNICA" ~ "PREPARATORIA",
      nivel_edu == "LICENCIATURA" ~ "LICENCIATURA",
    ))


datos<- datos %>%
  mutate(
    DERECHO_HAB_LIMPIA = case_when(
      desc_derechohab == "ARMADA" ~ "OTRA",
      desc_derechohab == "CUOTA" ~ "OTRA",
      desc_derechohab == "CUOTA DE RECUPERACIÓN" ~ "OTRA",
      desc_derechohab == "EXCENTO" ~ "OTRA",
      desc_derechohab == "EXENTO" ~ "OTRA",
      desc_derechohab == "EXENTO 081" ~ "OTRA",
      desc_derechohab == "gratuidad" ~ "NO ESPECIFICADO",
      desc_derechohab == "Gratuidad" ~ "NO ESPECIFICADO",
      desc_derechohab == "GRATUIDAD" ~ "NO ESPECIFICADO",
      desc_derechohab == "Gratuidad / IMSS" ~ "IMSS",
      desc_derechohab == "I M S S" ~ "IMSS",
      desc_derechohab == "IMSS" ~ "IMSS",
      desc_derechohab == "imss" ~ "IMSS",
      desc_derechohab == "IMSS / EXENTO" ~ "IMSS",
      desc_derechohab == "IMSS / Gratuidad" ~ "IMSS",
      desc_derechohab == "IMSS / GRATUIDAD" ~ "IMSS",
      desc_derechohab == "IMSS / ISSSTE" ~ "IMSS",
      desc_derechohab == "IMSS / ISSSTE / Gratuidad" ~ "IMSS",
      desc_derechohab == "IMSS escolar" ~ "IMSS",
      desc_derechohab == "interrupcion completa" ~ "NO ESPECIFICADO",
      desc_derechohab == "isemin" ~ "OTRA",
      desc_derechohab == "ISEMYM" ~ "OTRA",
      desc_derechohab == "ISEMYN" ~ "OTRA",
      desc_derechohab == "ISSEMYN" ~ "OTRA",
      desc_derechohab == "ISSFAM" ~ "OTRA",
      desc_derechohab == "ISSSSTE" ~ "ISSSTE",
      desc_derechohab == "ISSSTE" ~ "ISSSTE",
      desc_derechohab == "issste" ~ "ISSSTE",
      desc_derechohab == "ISSSTE / GRATUIDAD" ~ "ISSSTE",
      desc_derechohab == "N/E" ~ "NO ESPECIFICADO",
      desc_derechohab == "Nada" ~ "NINGUNO",
      desc_derechohab == "NAVAL" ~ "OTRA",
      desc_derechohab == "ninguna" ~ "NINGUNO",
      desc_derechohab == "NINGUNA" ~ "NINGUNO",
      desc_derechohab == "Ninguna" ~ "NINGUNO",
      desc_derechohab == "NINGUNO" ~ "NINGUNO",
      desc_derechohab == "Ninguno" ~ "NINGUNO",
      desc_derechohab == "Otra" ~ "OTRA",
      desc_derechohab == "OTRA" ~ "OTRA",
      desc_derechohab == "OTRO" ~ "OTRA",
      desc_derechohab == "Otro" ~ "OTRA",
      desc_derechohab == "OTRO PAGO" ~ "OTRA",
      desc_derechohab == "OTROS" ~ "OTRA",
      desc_derechohab == "Paciente Particular" ~ "NO ESPECIFICADO",
      desc_derechohab == "PACIENTE PARTICULAR" ~ "NO ESPECIFICADO",
      desc_derechohab == "PAGO" ~ "OTRA",
      desc_derechohab == "Particular" ~ "OTRA",
      desc_derechohab == "PEMEX" ~ "NO ESPECIFICADO",
      desc_derechohab == "S/INFORMACION" ~ "NO ESPECIFICADO",
      desc_derechohab == "SE IGNORA" ~ "NO ESPECIFICADO",
      desc_derechohab == "SEDENA" ~ "OTRA",
      desc_derechohab == "sedesa" ~ "OTRA",
      desc_derechohab == "seguro popular" ~ "SEGURO POPULAR",
      desc_derechohab == "SEGURO POPULAR" ~ "SEGURO POPULAR",
      desc_derechohab == "Seguro Popular" ~ "SEGURO POPULAR",
      desc_derechohab == "Seguro popular" ~ "SEGURO POPULAR",
      desc_derechohab == "seguro popular y gratuidad" ~ "SEGURO POPULAR",
      desc_derechohab == "sermar" ~ "OTRA",
      desc_derechohab == "servicios estatales" ~ "OTRA",
      desc_derechohab == "SIN ESPECIFICAR" ~ "NO ESPECIFICADO"
    ))


datos %>% 
  select(nivel_edu, NIVEL_EDU_LIMPIA)


###############################

datos<- datos %>% mutate(
  EDOCIVIL_LIMPIA= case_when(
    edocivil_descripcion=="CASADO(A)" ~ "CASADA",
    edocivil_descripcion== "SOLTERA" ~ "SOLTERA"))

unique(datos$edocivil_descripcion)    

edocivil_sucio<-datos %>% 
  select(edocivil_descripcion) %>% 
  group_by(edocivil_descripcion) %>% 
  count()


datos<- datos %>%
  mutate(
    EDO_CIVIL_DESCRIPCION_LIMPIA = case_when(
      edocivil_descripcion == "CASADO(A)" ~ "CASADA",
      edocivil_descripcion == "SOLTERA" ~ "SOLTERA",
      edocivil_descripcion == "UNIÓN LIBRE" ~ "UNIÓN LIBRE",
      edocivil_descripcion == "DIVORCIADA" ~ "DIVORCIADA",
      edocivil_descripcion == "CASADA (O)" ~ "CASADA",
      edocivil_descripcion == "UNION LIBRE" ~ "UNIÓN LIBRE",
      edocivil_descripcion == "SOLTERO(A)" ~ "SOLTERA",
      edocivil_descripcion == "DIVORCIADO(A)" ~ "DIVORCIADA",
      edocivil_descripcion == "VIUDO" ~ "VIUDA",
      edocivil_descripcion == "SEPARADO (A)" ~ "SEPARADA",
      edocivil_descripcion == "SOLTERO/A" ~ "SOLTERA",
      edocivil_descripcion == "CASADA" ~ "CASADA",
      edocivil_descripcion == "SOLTERO(A)" ~ "SOLTERA",
      edocivil_descripcion == "SEPARADO/A" ~ "SEPARADA",
      edocivil_descripcion == "DIVORCIADO/A" ~ "DIVORCIADA"
    ))
