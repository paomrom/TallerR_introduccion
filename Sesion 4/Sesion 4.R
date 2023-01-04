#Instalar paqueterías (se instala una vez)
install.packages("readxl")
install.packages("dplyr")

#Llamamos o cargamos librerias o paqueterias
library(readxl)
library(dplyr)

#Cargamos base de datos - 3 formas

#Cargar datos de un archivo de extension xlsx
datos <- read_excel("Telegram Desktop/ile_completa.xlsx")

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
unique(datos$year)

############################# variable año ########################
#Verificar valores unicos y NA's
unique(datos$year)


#Conteo de variable year
datos %>% count(year)
#Hay 21 registros sin año o valores perdidos NA

############################# Variable mes #######################
# Valores unicos de variables MES
unique(datos$mes)


# Pasar a mayúsculas
datos$mes<-toupper(datos$mes)


#Contar meses
datos %>% count(mes)
# Hay 20 valores perdidos o NA

########################### Variable hospital #####################
# Verificar valores unicos
unique(datos$hospital)

#Renombrar observaciones de Hospital
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

#Conteo de datos renombrados o agrupados
datos %>% count(HOSPITAL_LIMPIA)

########################### fingreso ###########################
unique(datos$fingreso)

#Limpiamos las fechas que son menor al 2016 y mayor a 2020 pasan a NA
#En el conjunto de dato identifique fechas especifica que deben pasar a NA

#Remplazo valores no fecha a NA
datos$fingreso[datos$fingreso<=11112] <- NA_character_
datos$fingreso[datos$fingreso>43466] <- NA_character_

#Remplazo fechas fuera de la periodicidad 2016-2020 a NA
datos$fingreso[datos$fingreso=="30/04/2048"] <- NA_character_
datos$fingreso[datos$fingreso<="25/11/2015"] <- NA_character_

#conteo de fechas
count_fingreso<-datos %>% count(fingreso)

# Conclusión: 64,571 NA's o valores perdidos :(

######################## Variable autorreferida ################
#Valores unicos
unique(datos$autoref)

# Mayúsculas
datos$autoref<-toupper(datos$autoref) 

#me da los valores unicos entre ""
dput(unique(datos$autoref))

#Identifico cuales pasarán a SÍ, NO y NA
auto_na<-c(NA, "NE",  "N/E")

#Remplazar valores
#Da el valor de 1 aquellos valores diferentes a "auto_no" que son los NA
#Valores igual a NO toman el valor de 0
datos$AUTOREF_LIMPIA[datos$autoref != auto_na] <- 1
datos$AUTOREF_LIMPIA[datos$autoref == "NO"] <- 0
datos$AUTOREF_LIMPIA[datos$autoref %in% auto_na] <- NA_character_

#Validar renombres
unique(datos$AUTOREF_LIMPIA)

#conteo de nuevas categorías
datos %>% count(AUTOREF_LIMPIA)

########################### descpción del estado civil ###########################

# Problema: No están agrupados. No están en lenguaje femenino

#Leemos valores unicos como se ve que hay observaciones en minusculas y mayusculas
unique(datos$edocivil_descripcion)     

#Pasar toda la variable a mayúsculas
datos$edocivil_descripcion<- toupper(datos$edocivil_descripcion)

#Renombrar observaciones de variables
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

#Contar observaciones por las nuevas agrupaciones de observaciones
datos %>% count(EDO_CIVIL_DESCRIPCION_LIMPIA)



############################# Variable escolaridad ######################

# Valores unicos en las variables
unique(datos$nivel_edu)

# Minúsculas a mayúsculas
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

datos %>% count(NIVEL_EDU_LIMPIA)

############################ derechohabiencia #########################
# Valores unicos en las variables
unique(datos$desc_derechohab)

# Minúsculas a mayúsculas
datos$desc_derechohab<- toupper(datos$desc_derechohab)

#Renombrar variables
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

#Contar nuevos renombres
datos %>% count(datos$DERECHO_HAB_LIMPIA)

datos$DERECHO_HAB_LIMPIA[datos$DERECHO_HAB_LIMPIA=="NO ESPECIFICADO"]<-NA_character_

############################### Edad ##########################
#Validar la estructura de las variables u obeto
class(datos$edad)

# Valores unicos de la variable numérica edad
unique(datos$edad)

#Con la función summarise calculamos la edad mínima, máxima y el promedio
summary(datos$edad)

# En esta variable no hay nada que limpiar :D

# Por diversión se hace un rango de edad :)
datos<-datos %>% 
  mutate(RANGO_EDAD = case_when(edad < 17 ~ "MENOS DE 17 AÑOS",
                               between(edad, 18, 25) ~ "18 a 25 AÑOS",
                               between(edad, 26, 35) ~ "26 a 35 AÑOS",
                               between(edad, 36, 45) ~ "36 a 45 AÑOS",
                               edad > 46 ~ "MAYOR DE 46 AÑOS")) 


#Conteo de frecuencias en el rango de edad
datos %>% count(RANGO_EDAD)

class(datos$RANGO_EDAD)

########################### Ocupación #################################
#Valores unicos, se obsevan más de 600 valores unicos en ocupación :o
unique(datos$ocupacion)

#Renombrar y agrupar observaciones
datos<- datos %>%
  mutate(
    OCUPACION_LIMPIA = case_when(
      ocupacion == "agente de ventas" ~ "EMPLEADA",
      ocupacion == "ama de casa" ~ "TRABAJADORA DEL HOGAR NO REMUNERADA",
      ocupacion == "analista contable" ~ "EMPLEADA",
      ocupacion == "asistentes de profesor de educación preescolar y de jard??n de niños" ~ "EMPLEADA",
      ocupacion == "cocinera doméstica" ~ "EMPLEADA",
      ocupacion == "cocinero" ~ "EMPLEADA",
      ocupacion == "comerciante" ~ "EMPLEADA",
      ocupacion == "desempleado" ~ "EMPLEADA",
      ocupacion == "educadora" ~ "EMPLEADA",
      ocupacion == "empleado" ~ "EMPLEADA",
      ocupacion == "estilistas" ~ "EMPLEADA",
      ocupacion == "estudiante" ~ "ESTUDIANTE",
      ocupacion == "secretarias" ~ "EMPLEADA",
      ocupacion == "vendedor a domicilio" ~ "EMPLEADA",
      ocupacion == "edecán (enroller)" ~ "EMPLEADA",
      ocupacion == "artesanos" ~ "EMPLEADA",
      ocupacion == "dibujante técnico" ~ "EMPLEADA",
      ocupacion == "recepcionistas" ~ "EMPLEADA",
      ocupacion == "estilista" ~ "EMPLEADA",
      ocupacion == "mesero" ~ "EMPLEADA",
      ocupacion == "preparadores de comida rápida" ~ "EMPLEADA",
      ocupacion == "polic??a auxiliar" ~ "EMPLEADA",
      ocupacion == "artesano art??culos de piedra" ~ "EMPLEADA",
      ocupacion == "enfermeras" ~ "EMPLEADA",
      ocupacion == "costurera tela de tapicer??a" ~ "EMPLEADA",
      ocupacion == "vendedores y demostradores de puerta en puerta" ~ "EMPLEADA",
      ocupacion == "enfermera general" ~ "EMPLEADA",
      ocupacion == "empleada" ~ "EMPLEADA",
      ocupacion == "desempleada" ~ "DESEMPLEADA",
      ocupacion == "HOGAR" ~ "TRABAJADORA DEL HOGAR NO REMUNERADA",
      ocupacion == "EMPLEADA" ~ "EMPLEADA",
      ocupacion == "ESTUDIANTE" ~ "ESTUDIANTE",
      ocupacion == "COMERCIANTE" ~ "EMPLEADA",
      ocupacion == "EMPLEADA SERVICIOS PERSONALES" ~ "EMPLEADA",
      ocupacion == "DESEMPLEADA" ~ "DESEMPLEADA",
      ocupacion == "NINGUNA" ~ "DESEMPLEADA",
      ocupacion == "Hogar" ~ "TRABAJADORA DEL HOGAR NO REMUNERADA",
      ocupacion == "Empleada" ~ "EMPLEADA",
      ocupacion == "Estudiante" ~ "ESTUDIANTE",
      ocupacion == "psicólogo" ~ "ESTUDIANTE",
      ocupacion == "obrero de mantenimiento y reparación de edificios" ~ "ESTUDIANTE",
      ocupacion == "promotor" ~ "ESTUDIANTE",
      ocupacion == "compositor de música" ~ "ESTUDIANTE",
      ocupacion == "cajero de recepción" ~ "ESTUDIANTE",
      ocupacion == "costurera a mano de prendas de vestir" ~ "ESTUDIANTE",
      ocupacion == "maestro enseñanza preescolar" ~ "ESTUDIANTE",
      ocupacion == "cajero registrador" ~ "ESTUDIANTE",
      ocupacion == "administrador de empresa" ~ "ESTUDIANTE",
      ocupacion == "hogar" ~ "TRABAJADORA DEL HOGAR NO REMUNERADA",
      ocupacion == "otra" ~ "NO ESPECIFICADO",
      ocupacion == "bailar??n" ~ "EMPLEADA",
      ocupacion == "diseñador gráfico" ~ "EMPLEADA",
      ocupacion == "<NA>" ~ " ",
      ocupacion == "asistente de profesor" ~ "EMPLEADA",
      ocupacion == "auxiliar administrativo" ~ "EMPLEADA",
      ocupacion == "vendedor de piso" ~ "EMPLEADA",
      ocupacion == "capturista de datos" ~ "EMPLEADA",
      ocupacion == "ayudante de técnico de centro de recepción de control" ~ "EMPLEADA",
      ocupacion == "abogado" ~ "EMPLEADA",
      ocupacion == "actor" ~ "EMPLEADA",
      ocupacion == "profesor de educación secundaria" ~ "EMPLEADA",
      ocupacion == "demostrador casa por casa" ~ "EMPLEADA",
      ocupacion == "auxiliar de fisioterapeuta" ~ "EMPLEADA",
      ocupacion == "cantante" ~ "EMPLEADA",
      ocupacion == "mesera" ~ "EMPLEADA",
      ocupacion == "docente" ~ "EMPLEADA",
      ocupacion == "masajista" ~ "EMPLEADA",
      ocupacion == "comercianrte" ~ "EMPLEADA",
      ocupacion == "abogada" ~ "EMPLEADA",
      ocupacion == "estudiaante" ~ "ESTUDIANTE",
      ocupacion == "tecnico laboratorista clinico" ~ "EMPLEADA",
      ocupacion == "Comerciante" ~ "EMPLEADA",
      ocupacion == "S/INFORMACION" ~ "NO ESPECIFICADO",
      ocupacion == "oficiales de polic??a y guardias de seguridad" ~ "EMPLEADA",
      ocupacion == "supervisor av??cola" ~ "EMPLEADA",
      ocupacion == "ayudante carpintero de obra negra" ~ "EMPLEADA",
      ocupacion == "instructor de natación" ~ "EMPLEADA",
      ocupacion == "facturista" ~ "EMPLEADA",
      ocupacion == "obrero general en el procesamiento de materia orgánica" ~ "EMPLEADA",
      ocupacion == "asistente dentista" ~ "EMPLEADA",
      ocupacion == "nutriólogo" ~ "EMPLEADA",
      ocupacion == "supervisor de vendedores" ~ "EMPLEADA",
      ocupacion == "ayudante de cajero" ~ "EMPLEADA",
      ocupacion == "calderero cobre" ~ "EMPLEADA",
      ocupacion == "secretaria en general" ~ "EMPLEADA",
      ocupacion == "emplead" ~ "EMPLEADA",
      ocupacion == "empleado(a)" ~ "EMPLEADA",
      ocupacion == "comercianbte" ~ "EMPLEADA",
      ocupacion == "setudiante" ~ "ESTUDIANTE",
      ocupacion == "ABOGADA" ~ "EMPLEADA",
      ocupacion == "AMA DE CASA-HOGAR" ~ "TRABAJADORA DEL HOGAR NO REMUNERADA",
      ocupacion == "EMPLEADO(A)" ~ "EMPLEADA",
      ocupacion == "POR DEFINIR" ~ "NO ESPECIFICADO",
      ocupacion == "DESEMPLEADO O DESOCUPADO" ~ "DESEMPLEADA",
      ocupacion == "DOMESTICA" ~ "EMPLEADA",
      ocupacion == "ayudante de limpieza" ~ "EMPLEADA",
      ocupacion == "asesor jur??dico" ~ "EMPLEADA",
      ocupacion == "médico general" ~ "EMPLEADA",
      ocupacion == "músico" ~ "EMPLEADA",
      ocupacion == "ayudantes generales de operación de centrales eléctricas" ~ "EMPLEADA",
      ocupacion == "profesor enseñanza primaria" ~ "EMPLEADA",
      ocupacion == "dibujante art??stico" ~ "EMPLEADA",
      ocupacion == "agente de venta de publicidad" ~ "EMPLEADA",
      ocupacion == "modelo de artistas" ~ "EMPLEADA",
      ocupacion == "asistentes ejecutivos" ~ "EMPLEADA",
      ocupacion == "fotógrafos" ~ "EMPLEADA",
      ocupacion == "entrenadores o instructores en deporte y recreación" ~ "EMPLEADA",
      ocupacion == "auxiliares dentales" ~ "EMPLEADA",
      ocupacion == "demostradora" ~ "EMPLEADA",
      ocupacion == "hosstes" ~ "EMPLEADA",
      ocupacion == "recepcionista" ~ "EMPLEADA",
      ocupacion == "cocinera" ~ "EMPLEADA",
      ocupacion == "PROFESORA" ~ "EMPLEADA",
      ocupacion == "COCINERA" ~ "EMPLEADA",
      ocupacion == "DEFENSOR DE OFICIO" ~ "EMPLEADA",
      ocupacion == "VOLUNTARIA" ~ "EMPLEADA",
      ocupacion == "COSTURERA" ~ "EMPLEADA",
      ocupacion == "empresaria" ~ "EMPLEADA",
      ocupacion == "maestra" ~ "EMPLEADA",
      ocupacion == "asistente de contador área comercialización" ~ "EMPLEADA",
      ocupacion == "profesores de escuela secundaria" ~ "EMPLEADA",
      ocupacion == "bibliotecario" ~ "EMPLEADA",
      ocupacion == "vendedor técnico" ~ "EMPLEADA",
      ocupacion == "n/e" ~ "NO ESPECIFICADO",
      ocupacion == "SE IGNORA" ~ "NO ESPECIFICADO",
      ocupacion == "CONTADORA" ~ "EMPLEADA",
      ocupacion == "capacitadores e instructores" ~ "EMPLEADA",
      ocupacion == "trabajador social" ~ "EMPLEADA",
      ocupacion == "profesor de gimnasia" ~ "EMPLEADA",
      ocupacion == "asesor en distribución y log??stica" ~ "EMPLEADA",
      ocupacion == "analista de profesiones" ~ "EMPLEADA",
      ocupacion == "maquillista" ~ "EMPLEADA",
      ocupacion == "ejecutivo de cuenta" ~ "EMPLEADA",
      ocupacion == "agricultor (cultivos extensivos)" ~ "EMPLEADA",
      ocupacion == "PSICOLOGA" ~ "EMPLEADA",
      ocupacion == "TAXISTA" ~ "EMPLEADA",
      ocupacion == "AMA DE CASA" ~ "TRABAJADORA DEL HOGAR NO REMUNERADA",
      ocupacion == "2. DESEMPLEADA" ~ "DESEMPLEADA",
      ocupacion == "1. EMPLEADA" ~ "EMPLEADA",
      ocupacion == "4. ESTUDIANTE" ~ "ESTUDIANTE",
      ocupacion == "5. HOGAR" ~ "TRABAJADORA DEL HOGAR NO REMUNERADA",
      ocupacion == "coordinadores y supervisores de informática" ~ "EMPLEADA",
      ocupacion == "laboratorista de análisis cl??nicos" ~ "EMPLEADA",
      ocupacion == "asistente de gerencia" ~ "EMPLEADA",
      ocupacion == "chofer vendedor" ~ "EMPLEADA",
      ocupacion == "promotor de bebidas" ~ "EMPLEADA",
      ocupacion == "afanadora" ~ "EMPLEADA",
      ocupacion == "cirujano dentista" ~ "EMPLEADA",
      ocupacion == "jornalero agr??cola" ~ "EMPLEADA",
      ocupacion == "periodista" ~ "EMPLEADA",
      ocupacion == "jefe de polic??a estatal" ~ "EMPLEADA",
      ocupacion == "ama de cas" ~ "TRABAJADORA DEL HOGAR NO REMUNERADA",
      ocupacion == "comerciqante" ~ "EMPLEADA",
      ocupacion == "instructor" ~ "EMPLEADA",
      ocupacion == "asistentes y prefectos de educación primaria y secundaria" ~ "EMPLEADA",
      ocupacion == "mecánico automotriz de automóviles" ~ "EMPLEADA",
      ocupacion == "ESTILISTA" ~ "EMPLEADA",
      ocupacion == "OFICINISTA" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA" ~ "EMPLEADA",
      ocupacion == "ESTUDIENTE" ~ "EMPLEADA",
      ocupacion == "ESCOLTA" ~ "EMPLEADA",
      ocupacion == "empleado de cálculo de costos (calculista)" ~ "EMPLEADA",
      ocupacion == "trabajadores de servicio social y de la comunidad" ~ "EMPLEADA",
      ocupacion == "mozo de limpieza" ~ "EMPLEADA",
      ocupacion == "cajero de banco" ~ "EMPLEADA",
      ocupacion == "protesistas de audio" ~ "EMPLEADA",
      ocupacion == "encuestador" ~ "EMPLEADA",
      ocupacion == "publicista" ~ "EMPLEADA",
      ocupacion == "sociólogo" ~ "EMPLEADA",
      ocupacion == "cajista de composición a máquina" ~ "EMPLEADA",
      ocupacion == "profesores de idiomas" ~ "EMPLEADA",
      ocupacion == "cajero de oficina (empresa)" ~ "EMPLEADA",
      ocupacion == "PROMOTOR" ~ "EMPLEADA",
      ocupacion == "DOCENTE" ~ "EMPLEADA",
      ocupacion == "ESTUDIOANTE" ~ "ESTUDIANTE",
      ocupacion == "CASADA" ~ "NO ESPECIFICADO",
      ocupacion == "NO ESPECIFICADO" ~ "NO ESPECIFICADO",
      ocupacion == "SERVICIOS PERSONALES" ~ "EMPLEADA",
      ocupacion == "3. COMERCIANTE" ~ "EMPLEADA",
      ocupacion == "consejeros de empleo" ~ "EMPLEADA",
      ocupacion == "ayudante de fotograbado" ~ "EMPLEADA",
      ocupacion == "telefonista" ~ "EMPLEADA",
      ocupacion == "hostess (recepcionista de restaurante)" ~ "EMPLEADA",
      ocupacion == "fotografa" ~ "EMPLEADA",
      ocupacion == "analista de estudios de tiempos y movimientos" ~ "EMPLEADA",
      ocupacion == "modelos y edecanes" ~ "EMPLEADA",
      ocupacion == "EMPLEADA PUERICULTURISTA" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA MAESTRA" ~ "EMPLEADA",
      ocupacion == "PERIODISTA" ~ "EMPLEADA",
      ocupacion == "SERV. SOCIAL" ~ "EMPLEADA",
      ocupacion == "MAESTRA" ~ "EMPLEADA",
      ocupacion == "diseñador de interiores" ~ "EMPLEADA",
      ocupacion == "jefe de cocina" ~ "EMPLEADA",
      ocupacion == "empleado vendedor de boletos" ~ "EMPLEADA",
      ocupacion == "cosmetólogo" ~ "EMPLEADA",
      ocupacion == "vendedor ambulante alimentos y bebidas" ~ "EMPLEADA",
      ocupacion == "capacitador" ~ "EMPLEADA",
      ocupacion == "diseñador de prendas de vestir" ~ "EMPLEADA",
      ocupacion == "auxiliar de enfermer??a" ~ "EMPLEADA",
      ocupacion == "arquitecto" ~ "EMPLEADA",
      ocupacion == "PINTORA" ~ "EMPLEADA",
      ocupacion == "PROFESIONAL" ~ "EMPLEADA",
      ocupacion == "EMPLEADA  (CAJERA)" ~ "EMPLEADA",
      ocupacion == "EMPLEADA  (MESERA)" ~ "EMPLEADA",
      ocupacion == "EMPLEADA  (AYUDANTE GENERAL)" ~ "EMPLEADA",
      ocupacion == "DOMESTICA (AUXILIAR DE LIMPIEZA)" ~ "EMPLEADA",
      ocupacion == "maestro universitario" ~ "EMPLEADA",
      ocupacion == "esteticistas y masajistas" ~ "EMPLEADA",
      ocupacion == "veterinarios" ~ "EMPLEADA",
      ocupacion == "subcontador o asistente de contador público (nivel licenciatura o pasante)" ~ "EMPLEADA",
      ocupacion == "polic??a bancario" ~ "EMPLEADA",
      ocupacion == "chofer automóvil" ~ "EMPLEADA",
      ocupacion == "HOGAR-AMA DE CASA" ~ "TRABAJADORA DEL HOGAR NO REMUNERADA",
      ocupacion == "TECNICO LABORATORISTA CLINICO" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA DISE?'ADORA" ~ "EMPLEADA",
      ocupacion == "COMERCIANTE ARTESANA" ~ "EMPLEADA",
      ocupacion == "OBRERA" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA AUX. CONTADURIA" ~ "EMPLEADA",
      ocupacion == "EMPLEADA SECRETARIA" ~ "EMPLEADA",
      ocupacion == "EMPLEADA ASISTENTE" ~ "EMPLEADA",
      ocupacion == "EMPLEADA auxiliar de enfermer??a (domicilio)" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA analista contable" ~ "EMPLEADA",
      ocupacion == "EMPLEADO mesero" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA psicólogo" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA DE LS EDUCACIÓN asistente de profesor" ~ "EMPLEADA",
      ocupacion == "EMPLEADA analista de estudios de tiempos y movimientos" ~ "EMPLEADA",
      ocupacion == "EMPLEADA gerente de tienda" ~ "EMPLEADA",
      ocupacion == "EMPLEADA cajero de tienda de autoservicio" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA polic??a auxiliar" ~ "EMPLEADA",
      ocupacion == "EMPLEADA promotor de inversiones" ~ "EMPLEADA",
      ocupacion == "EMPLEADA cajero de oficina (empresa)" ~ "EMPLEADA",
      ocupacion == "EMPLEADA asesor de inversiones" ~ "EMPLEADA",
      ocupacion == "EMPLEADA ayudante de cocinero" ~ "EMPLEADA",
      ocupacion == "COMERCIANTE vendedores y demostradores de puerta en puerta" ~ "EMPLEADA",
      ocupacion == "EMPLEADA promotor" ~ "EMPLEADA",
      ocupacion == "EMPLEADA cajeros registradores" ~ "EMPLEADA",
      ocupacion == "EMPLEADA oficial de polic??a" ~ "EMPLEADA",
      ocupacion == "EMPLEADA cantante" ~ "EMPLEADA",
      ocupacion == "EMPLEADA analista contable" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTAS enfermeras" ~ "EMPLEADA",
      ocupacion == "EMPLEADA agente de ventas" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA analista de estudios de tiempos y movimientos" ~ "EMPLEADA",
      ocupacion == "EMPLEADA inhaloterapeutas" ~ "EMPLEADA",
      ocupacion == "EMPLEADA enfermera general" ~ "EMPLEADA",
      ocupacion == "EMPLEADA profesores de educación preescolar y jard??n de niños" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTAS DE LA EDUCACION profesor enseñanza primaria" ~ "EMPLEADA",
      ocupacion == "EMPLEADO compositor de música" ~ "EMPLEADA",
      ocupacion == "EMPLEADA vendedor de piso" ~ "EMPLEADA",
      ocupacion == "EMPLEADA cajero de desempeño" ~ "EMPLEADA",
      ocupacion == "EMPLEADA consejeros de empleo" ~ "EMPLEADA",
      ocupacion == "COMERCIANTE VENTAS demostrador casa por casa" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA consultor de sistemas" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA asistente de reclutamiento" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA abogado" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA psicoterapéuta" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA instructor" ~ "EMPLEADA",
      ocupacion == "DOMESTICA cocinera doméstica" ~ "EMPLEADA",
      ocupacion == "EMPLEADA mecánico automotriz de automóviles" ~ "EMPLEADA",
      ocupacion == "EMPLEADA ayudante general de operación" ~ "EMPLEADA",
      ocupacion == "DOMESTICA NI?'ERA" ~ "EMPLEADA",
      ocupacion == "EMPLEADA ayudante de cajero" ~ "EMPLEADA",
      ocupacion == "EMPLEADA cajero de recepción" ~ "EMPLEADA",
      ocupacion == "EMPLEADA afanadora" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA analista de profesiones" ~ "EMPLEADA",
      ocupacion == "COMERCIANTE vendedor a domicilio" ~ "EMPLEADA",
      ocupacion == "EMPLEADA fotógrafos" ~ "EMPLEADA",
      ocupacion == "EMPLEADA niñera en jard??n de niños" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA asistente de veterinario" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA nutriólogo" ~ "EMPLEADA",
      ocupacion == "TECNICO auxiliar de contabilidad (nivel técnico)" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA biólogo" ~ "EMPLEADA",
      ocupacion == "EMPLEADA fisioterapeuta" ~ "EMPLEADA",
      ocupacion == "EMPLEADA hostess (recepcionista de restaurante)" ~ "EMPLEADA",
      ocupacion == "COMERCIANTE vendedor ambulante de periódicos" ~ "EMPLEADA",
      ocupacion == "EMPLEADA recepcionistas" ~ "EMPLEADA",
      ocupacion == "EMPLEADA mozo de limpieza" ~ "EMPLEADA",
      ocupacion == "OBRERO ayudante carpintero de obra negra" ~ "EMPLEADA",
      ocupacion == "EMPLEADA operador cabrestante" ~ "EMPLEADA",
      ocupacion == "EMPLEADA modelo de artistas" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA administrador de empresa" ~ "EMPLEADA",
      ocupacion == "EMPLEADO AUXILIAR ingeniero civil" ~ "EMPLEADA",
      ocupacion == "EMPLEADA asistentes ejecutivos" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA bibliotecario" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA editores" ~ "EMPLEADA",
      ocupacion == "OTRA terapeuta de arte" ~ "EMPLEADA",
      ocupacion == "EMPLEADA agente de venta de publicidad" ~ "EMPLEADA",
      ocupacion == "EMPLEADO coordinador de agencias de viajes" ~ "EMPLEADA",
      ocupacion == "EMPLEADA oficiales de polic??a y guardias de seguridad" ~ "EMPLEADA",
      ocupacion == "EMPLEADA cajero registrador" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA técnico en radiolog??a médica" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA cirujano dentista" ~ "EMPLEADA",
      ocupacion == "EMPLEADO auxiliar de quiropráctico" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA trabajadores de servicio social y de la comunidad" ~ "EMPLEADA",
      ocupacion == "EMPLEADA auxiliares de enfermer??a" ~ "EMPLEADA",
      ocupacion == "EMPLEADA auxiliar de crédito y cobranza" ~ "EMPLEADA",
      ocupacion == "EMPLEADA asistente dentista" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA publicista" ~ "EMPLEADA",
      ocupacion == "EMPLEADA subcontador o asistente de contador público (nivel licenciatura o pasante)" ~ "EMPLEADA",
      ocupacion == "EMPLEADA auxiliar de dentista" ~ "EMPLEADA",
      ocupacion == "EMPLEADA telefonista" ~ "EMPLEADA",
      ocupacion == "EMPLEADA diseñadores gráficos" ~ "EMPLEADA",
      ocupacion == "EMPLEADA ensamblador de enseres domésticos" ~ "EMPLEADA",
      ocupacion == "EMPLEADA auxiliar arquitecto" ~ "EMPLEADA",
      ocupacion == "EMPLEADA auxiliar administrativo" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA médico veterinario" ~ "EMPLEADA",
      ocupacion == "EMPLEADA taquilleros" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA ingeniero civil" ~ "EMPLEADA",
      ocupacion == "EMPLEADA recepcionista en general" ~ "EMPLEADA",
      ocupacion == "EMPLEADA auxiliar de enfermer??a" ~ "EMPLEADA",
      ocupacion == "EMPLEADO diseñador de vestuario" ~ "EMPLEADA",
      ocupacion == "EMPLEADA ayudante de técnico de centro de recepción de control" ~ "EMPLEADA",
      ocupacion == "EMPLEADA cajero de banco" ~ "EMPLEADA",
      ocupacion == "PROFESIONALES DE LA EDUACION ayudantes de profesores universitarios" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA gu??a de turistas" ~ "EMPLEADA",
      ocupacion == "EMPLEADA bailarines" ~ "EMPLEADA",
      ocupacion == "EMPLEADA animador" ~ "EMPLEADA",
      ocupacion == "EMPLEADA  de venta y/o renta de bienes o servicios" ~ "EMPLEADA",
      ocupacion == "PROFESIONAL auditor" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTAS DE LA EDUCACION maestro enseñanza preescolar" ~ "EMPLEADA",
      ocupacion == "EMPLEADA fotógrafo en general" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTAS DE LA EDUCACION educadora" ~ "EMPLEADA",
      ocupacion == "EMPLEADA auxiliar de auditoria" ~ "EMPLEADA",
      ocupacion == "EMPLEADA asesor jur??dico" ~ "EMPLEADA",
      ocupacion == "COMERCIANTE ARTESANOS calderero cobre" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA director de fotograf??a" ~ "EMPLEADA",
      ocupacion == "EMPLEADA administrador de servicios educativos" ~ "EMPLEADA",
      ocupacion == "EMPLEADA ayudante de limpieza" ~ "EMPLEADA",
      ocupacion == "PROFESIONAL arquitecto" ~ "EMPLEADA",
      ocupacion == "PROFESIONALES DE LA EDUCACION ayudante de catedrático" ~ "EMPLEADA",
      ocupacion == "EMPLEADA modelos y edecanes" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA diseñador de interiores" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA médico familiar" ~ "EMPLEADA",
      ocupacion == "EMPLEADA vigilante" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA redactor" ~ "EMPLEADA",
      ocupacion == "EMPLEADA músico" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA editor literario" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA veterinarios" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA asistente dentista" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA optometrista" ~ "EMPLEADA",
      ocupacion == "obrero general en la industria metálica básica" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA artistas plásticos" ~ "EMPLEADA",
      ocupacion == "COMERCIANTE VENTAS demostrador de prácticas agr??colas" ~ "EMPLEADA",
      ocupacion == "EMPLEADA supervisor de vendedores" ~ "EMPLEADA",
      ocupacion == "EMPLEADA agente de seguros" ~ "EMPLEADA",
      ocupacion == "EMPLEADA facturista" ~ "EMPLEADA",
      ocupacion == "EMPLEADA almacenista" ~ "EMPLEADA",
      ocupacion == "OBRERA RECICLADORA" ~ "EMPLEADA",
      ocupacion == "OBRERA CAMPESINA" ~ "EMPLEADA",
      ocupacion == "EMPLEADA DEMOSTRADORA" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA BECARIA" ~ "EMPLEADA",
      ocupacion == "EMPLEADA MESERA" ~ "EMPLEADA",
      ocupacion == "EMPLEADA INSTRUCTORA" ~ "EMPLEADA",
      ocupacion == "EMPLEADA (ASISTENTE DE LICITACIONES)" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA (ASISTENTE DE DIRECCION)" ~ "EMPLEADA",
      ocupacion == "EMPLEADA (AYUDANTE GENERAL)" ~ "EMPLEADA",
      ocupacion == "EMPLEADA  (RECEPCIONISTA)" ~ "EMPLEADA",
      ocupacion == "DOMESTICA (AUXILIAR DE INTENDENCIA)" ~ "EMPLEADA",
      ocupacion == "EMPLEADA (SECTOR EDUCATIVO)" ~ "EMPLEADA",
      ocupacion == "EMPLEADA COORDINADORA DE EVENTOS" ~ "EMPLEADA",
      ocupacion == "OTRO (BARISTA)" ~ "EMPLEADA",
      ocupacion == "EMPLEADA  AUXILIAR  CONTABLE" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA EMPRESARIO" ~ "EMPLEADA",
      ocupacion == "EMPLEADOS DE SECTORES P?sBLICOS Y PRIVADOS" ~ "EMPLEADA",
      ocupacion == "No Remunerado - Ama de casa" ~ "TRABAJADORA DEL HOGAR NO REMUNERADA",
      ocupacion == "No Remunerado - Estudiante" ~ "ESTUDIANTE",
      ocupacion == "SUBEMPLEADA" ~ "EMPLEADA",
      ocupacion == "EMPLEADA mesero" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA auxiliar de enfermer??a" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA auxiliar administrativo" ~ "EMPLEADA",
      ocupacion == "EMPLEADA secretarias" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA especialista en métodos de enseñanza" ~ "EMPLEADA",
      ocupacion == "EMPLEADA (agente de ventas)" ~ "EMPLEADA",
      ocupacion == "EMPLEADA asesor en distribución y log??stica" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA ABOGADA" ~ "EMPLEADA",
      ocupacion == "PROFSIONISTA diseñador de interiores" ~ "EMPLEADA",
      ocupacion == "DOMESTICA lavandero a mano" ~ "EMPLEADA",
      ocupacion == "EMPLEADO promotor de ventas" ~ "EMPLEADA",
      ocupacion == "EMPLEADO Trabajadores De La Educación" ~ "EMPLEADA",
      ocupacion == "SIN ESPECIFICAR" ~ "NO ESPECIFICADO",
      ocupacion == "OBRERA costurera a mano de prendas de vestir" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA  profesor de danza" ~ "EMPLEADA",
      ocupacion == "COMERCIANTE  VENTAS (agentes y promotores)" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA asistente de gerencia" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA Psicólogo" ~ "EMPLEADA",
      ocupacion == "EMPLEADA enfermeras" ~ "EMPLEADA",
      ocupacion == "EMPLEADA auxiliares dentales" ~ "EMPLEADA",
      ocupacion == "EMPLEADA ama de llaves" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA dietistas y nutriólogos" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA asistente de profesor" ~ "EMPLEADA",
      ocupacion == "EMPLEADA instructor de aeróbics" ~ "EMPLEADA",
      ocupacion == "EMPLEADAS recepcionistas" ~ "EMPLEADA",
      ocupacion == "EMPLEADA coordinadores y supervisores de informática" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA asistentes ejecutivos" ~ "EMPLEADA",
      ocupacion == "OBRERA costurera prendas de cuero y piel" ~ "EMPLEADA",
      ocupacion == "EMPLEADA  de biblioteca" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA asistente de contador área comercialización" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA asistentes de profesor de educación preescolar y de jard??n de niños" ~ "EMPLEADA",
      ocupacion == "EMPLEADA profesor enseñanza primaria" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA asistentes y prefectos de educación primaria y secundaria" ~ "EMPLEADA",
      ocupacion == "EMPLEADA ayudante de farmacia" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA diseñadores gráficos" ~ "EMPLEADA",
      ocupacion == "EMPLEADO vendedor de aparatos domésticos" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA odontopediatra" ~ "EMPLEADA",
      ocupacion == "EMPLEADA diseñador gráfico" ~ "EMPLEADA",
      ocupacion == "EMPLEADA guardián" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA asesor de inversiones" ~ "EMPLEADA",
      ocupacion == "EMPLEADA jefe de polic??a estatal" ~ "EMPLEADA",
      ocupacion == "EMPLEADA operadores de teléfono" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA fotógrafos" ~ "EMPLEADA",
      ocupacion == "EMPLEADO polic??a auxiliar" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA enfermera general" ~ "EMPLEADA",
      ocupacion == "Otro" ~ "NO ESPECIFICADO",
      ocupacion == "OBRERA CAMPESINO" ~ "EMPLEADA",
      ocupacion == "EMPLEADA terapeuta ocupacional" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA odontogeriatra" ~ "EMPLEADA",
      ocupacion == "EMPLEADA ayudantes generales de operación de centrales eléctricas" ~ "EMPLEADA",
      ocupacion == "EMPLEADO enfermero de obstetricia" ~ "EMPLEADA",
      ocupacion == "EMPLEADO vendedor de piso" ~ "EMPLEADA",
      ocupacion == "EMPLEADA maestro enseñanza preescolar" ~ "EMPLEADA",
      ocupacion == "EMPLEADA agente de operaciones comerciales" ~ "EMPLEADA",
      ocupacion == "EMPLEADA profesor enseñanza especial" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA periodista" ~ "EMPLEADA",
      ocupacion == "EMPLEADO taxista" ~ "EMPLEADA",
      ocupacion == "EMPLEADA  de cálculo de costos (calculista)" ~ "EMPLEADA",
      ocupacion == "EMPLEADA Maestra Natación" ~ "EMPLEADA",
      ocupacion == "EMPLEADA seguridad" ~ "EMPLEADA",
      ocupacion == "obrero general en la industria manufacturera de alimentos y bebidas" ~ "EMPLEADA",
      ocupacion == "EMPLEADA operador equipos radio y televisión" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA arquitecto" ~ "EMPLEADA",
      ocupacion == "EMPLEADA inspector control de calidad" ~ "EMPLEADA",
      ocupacion == "obrero general en la elaboración de productos impresos" ~ "EMPLEADA",
      ocupacion == "EMPLEADA artistas plásticos" ~ "EMPLEADA",
      ocupacion == "TECNICO en radiolog??a médica" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA laboratorista de análisis cl??nicos" ~ "EMPLEADA",
      ocupacion == "PROFESIONAL auxiliar de contabilidad (nivel técnico)" ~ "EMPLEADA",
      ocupacion == "PROFESIONAL auxiliar administrativo" ~ "EMPLEADA",
      ocupacion == "EMPLEADA  supervisor de transporte terrestre" ~ "EMPLEADA",
      ocupacion == "PROFESIONALauxiliares contables y fiscales" ~ "EMPLEADA",
      ocupacion == "EMPLEADA asistente de profesor" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTASenfermera quirúrgica" ~ "EMPLEADA",
      ocupacion == "EMPLEADA director art??stico" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA gerente de comercialización" ~ "EMPLEADA",
      ocupacion == "administrador de hotel" ~ "EMPLEADA",
      ocupacion == "ayudante de cocinero" ~ "EMPLEADA",
      ocupacion == "empleados administrativos de producción" ~ "EMPLEADA",
      ocupacion == "oficial de polic??a" ~ "EMPLEADA",
      ocupacion == "gerente administrativo" ~ "EMPLEADA",
      ocupacion == "probador de equipo y l??neas telefónicas" ~ "EMPLEADA",
      ocupacion == "recepcionista en general" ~ "EMPLEADA",
      ocupacion == "analista en control de calidad" ~ "EMPLEADA",
      ocupacion == "pizzero" ~ "EMPLEADA",
      ocupacion == "empleado de ventanilla de correos" ~ "EMPLEADA",
      ocupacion == "auxiliar de terapista ocupacional" ~ "EMPLEADA",
      ocupacion == "gestor de cobranzas" ~ "EMPLEADA",
      ocupacion == "manicurista" ~ "EMPLEADA",
      ocupacion == "auxiliar de contabilidad (nivel técnico)" ~ "EMPLEADA",
      ocupacion == "tortillera" ~ "EMPLEADA",
      ocupacion == "obrero general en la industria manufacturera de textiles y prendas de vestir" ~ "EMPLEADA",
      ocupacion == "ayudante general de operación" ~ "EMPLEADA",
      ocupacion == "obrero general en la industria manufacturera de productos eléctricos y electrónicos" ~ "EMPLEADA",
      ocupacion == "demostrador de prácticas agr??colas" ~ "EMPLEADA",
      ocupacion == "meseros" ~ "EMPLEADA",
      ocupacion == "fotógrafo en general" ~ "EMPLEADA",
      ocupacion == "asesor de inversiones" ~ "EMPLEADA",
      ocupacion == "contadores y auditores" ~ "EMPLEADA",
      ocupacion == "enfermero militar" ~ "EMPLEADA",
      ocupacion == "secretaria ejecutiva" ~ "EMPLEADA",
      ocupacion == "supervisor de banco" ~ "EMPLEADA",
      ocupacion == "bioqu??mico" ~ "EMPLEADA",
      ocupacion == "auditor contable" ~ "EMPLEADA",
      ocupacion == "ayudante de electricista" ~ "EMPLEADA",
      ocupacion == "maestro contratista" ~ "EMPLEADA",
      ocupacion == "técnico en puericultura" ~ "EMPLEADA",
      ocupacion == "analista de crédito y cobranza" ~ "EMPLEADA",
      ocupacion == "ingeniero civil" ~ "EMPLEADA",
      ocupacion == "empleado información al público" ~ "EMPLEADA",
      ocupacion == "asistente de veterinario" ~ "EMPLEADA",
      ocupacion == "sobrecargos" ~ "EMPLEADA",
      ocupacion == "cajeros registradores" ~ "EMPLEADA",
      ocupacion == "consultor de recursos humanos" ~ "EMPLEADA",
      ocupacion == "auxiliares contables y fiscales" ~ "EMPLEADA",
      ocupacion == "ingeniero en computación (software)" ~ "EMPLEADA",
      ocupacion == "médico veterinario zootecnista" ~ "EMPLEADA",
      ocupacion == "supervisor de caja" ~ "EMPLEADA",
      ocupacion == "dama de compañ??a" ~ "EMPLEADA",
      ocupacion == "consejero en orientación profesional" ~ "EMPLEADA",
      ocupacion == "inspector control de calidad" ~ "EMPLEADA",
      ocupacion == "serigrafistas" ~ "EMPLEADA",
      ocupacion == "sobrecargo en general" ~ "EMPLEADA",
      ocupacion == "analista bursátil" ~ "EMPLEADA",
      ocupacion == "empleado de farmacia" ~ "EMPLEADA",
      ocupacion == "auxiliar de auditoria" ~ "EMPLEADA",
      ocupacion == "obrero general en la industria manufacturera de productos qu??micos" ~ "EMPLEADA",
      ocupacion == "portero de edificio" ~ "EMPLEADA",
      ocupacion == "OTRO(A)" ~ "NO ESPECIFICADO",
      ocupacion == "jefe de ayudantes de limpieza" ~ "EMPLEADA",
      ocupacion == "cantinero" ~ "EMPLEADA",
      ocupacion == "asistente social técnico" ~ "EMPLEADA",
      ocupacion == "profesor enseñanza especial (nivel medio)" ~ "EMPLEADA",
      ocupacion == "ingeniero qu??mico" ~ "EMPLEADA",
      ocupacion == "enfermero de salud pública" ~ "EMPLEADA",
      ocupacion == "Arquitecta" ~ "EMPLEADA",
      ocupacion == "auditor" ~ "EMPLEADA",
      ocupacion == "Independiente" ~ "EMPLEADA",
      ocupacion == "reportero" ~ "EMPLEADA",
      ocupacion == "sirvienta" ~ "EMPLEADA",
      ocupacion == "ayudantes de profesores universitarios" ~ "EMPLEADA",
      ocupacion == "guardián" ~ "EMPLEADA",
      ocupacion == "operador-conductor de tractor sobre orugas" ~ "EMPLEADA",
      ocupacion == "vendedor de alimentos y bebidas en espectáculos" ~ "EMPLEADA",
      ocupacion == "operador de cortadora de telas" ~ "EMPLEADA",
      ocupacion == "ingenieros qu??micos" ~ "EMPLEADA",
      ocupacion == "diseñador de redes de telecomunicaciones" ~ "EMPLEADA",
      ocupacion == "vendedores ambulantes" ~ "EMPLEADA",
      ocupacion == "Obrera" ~ "EMPLEADA",
      ocupacion == "repostero (restaurante)" ~ "EMPLEADA",
      ocupacion == "costurera colchas y colchones" ~ "EMPLEADA",
      ocupacion == "paramédico" ~ "EMPLEADA",
      ocupacion == "secretaria bilingüe" ~ "EMPLEADA",
      ocupacion == "cajero de desempeño" ~ "EMPLEADA",
      ocupacion == "PROFESIONISTA Arquitecta" ~ "EMPLEADA",
      ocupacion == "BAILARINA" ~ "EMPLEADA",
      ocupacion == "panadero" ~ "EMPLEADA",
      ocupacion == "nana" ~ "EMPLEADA",
      ocupacion == "diseñadores gráficos" ~ "EMPLEADA",
      ocupacion == "técnico ortopedista" ~ "EMPLEADA",
      ocupacion == "médico familiar" ~ "EMPLEADA",
      ocupacion == "gerente de mercadotecnia" ~ "EMPLEADA",
      ocupacion == "cajero de tienda de autoservicio" ~ "EMPLEADA",
      ocupacion == "Sin especificar" ~ "NO ESPECIFICADO",
      ocupacion == "niñera en jard??n de niños" ~ "EMPLEADA",
      ocupacion == "abogado civilista" ~ "EMPLEADA",
      ocupacion == "armador de relojes" ~ "EMPLEADA",
      ocupacion == "editores" ~ "EMPLEADA",
      ocupacion == "asistente de médico patólogo" ~ "EMPLEADA",
      ocupacion == "empleado de servicios de control de peso" ~ "EMPLEADA",
      ocupacion == "pastelero repostero" ~ "EMPLEADA",
      ocupacion == "CAMPO" ~ "EMPLEADA",
      ocupacion == "Terapeuta" ~ "EMPLEADA",
      ocupacion == "Sastre" ~ "EMPLEADA",
      ocupacion == "Sin Especificar" ~ "NO ESPECIFICADO",
      ocupacion == "pastelero" ~ "EMPLEADA",
      ocupacion == "electricista en general" ~ "EMPLEADA",
      ocupacion == "vendedor de telas y mercer??a" ~ "EMPLEADA",
      ocupacion == "empleado de biblioteca" ~ "EMPLEADA",
      ocupacion == "investigador criminalista" ~ "EMPLEADA",
      ocupacion == "mecánico instrumentos odontolog??a" ~ "EMPLEADA",
      ocupacion == "profesor enseñanza especial" ~ "EMPLEADA",
      ocupacion == "Maestra" ~ "EMPLEADA",
      ocupacion == "Profesionistas" ~ "EMPLEADA",
      ocupacion == "calculista de obras de ingenier??a" ~ "EMPLEADA",
      ocupacion == "terapeuta ocupacional" ~ "EMPLEADA",
      ocupacion == "taxista" ~ "EMPLEADA",
      ocupacion == "auxiliar de ventas" ~ "EMPLEADA",
      ocupacion == "seguridad" ~ "EMPLEADA",
      ocupacion == "secretaria" ~ "EMPLEADA",
      ocupacion == "admistracion" ~ "EMPLEADA",
      ocupacion == "ejecutivo telefónico" ~ "EMPLEADA",
      ocupacion == "comertiante" ~ "EMPLEADA",
      ocupacion == "enfermera" ~ "EMPLEADA",
      ocupacion == "tecnica en uñas" ~ "EMPLEADA",
      ocupacion == "odontóloga" ~ "EMPLEADA",
      ocupacion == "arquitecta" ~ "EMPLEADA",
      ocupacion == "ayudante general" ~ "EMPLEADA",
      ocupacion == "ayudante de oficina" ~ "EMPLEADA",
      ocupacion == "empleada nominas" ~ "EMPLEADA",
      ocupacion == "obrera" ~ "EMPLEADA",
      ocupacion == "auxiliar contable" ~ "EMPLEADA",
      ocupacion == "servidor público" ~ "EMPLEADA",
      ocupacion == "promotora" ~ "EMPLEADA",
      ocupacion == "terapeuta" ~ "EMPLEADA",
      ocupacion == "Docente" ~ "EMPLEADA",
      ocupacion == "profesores de escuela primaria" ~ "EMPLEADA",
      ocupacion == "Empleados De Sectores Público Y Privado" ~ "EMPLEADA",
      ocupacion == "Serv. Social" ~ "EMPLEADA",
      ocupacion == "VENTAS" ~ "EMPLEADA",
      ocupacion == "oficinista" ~ "EMPLEADA",
      ocupacion == "contadora" ~ "EMPLEADA",
      ocupacion == "comercio" ~ "EMPLEADA",
      ocupacion == "empleada federal" ~ "EMPLEADA",
      ocupacion == "costurera" ~ "EMPLEADA",
      ocupacion == "cosmetóloga" ~ "EMPLEADA",
      ocupacion == "freelancer" ~ "EMPLEADA",
      ocupacion == "asistente" ~ "EMPLEADA",
      ocupacion == "asesor de ventas" ~ "EMPLEADA",
      ocupacion == "Trabajo propio" ~ "EMPLEADA",
      ocupacion == "médico veterinario" ~ "EMPLEADA",
      ocupacion == "técnico en radiolog??a médica" ~ "EMPLEADA",
      ocupacion == "cajero de restaurante" ~ "EMPLEADA",
      ocupacion == "barnizador en general" ~ "EMPLEADA",
      ocupacion == "ama de casa - hogar" ~ "TRABAJADORA DEL HOGAR NO REMUNERADA",
      ocupacion == "COMECIANTE" ~ "EMPLEADA",
      ocupacion == "No Especificado" ~ "NO ESPECIFICADO",
      ocupacion == "orientador educativo" ~ "EMPLEADA",
      ocupacion == "gerente de tienda" ~ "EMPLEADA",
      ocupacion == "NO RESPONDE" ~ "NO ESPECIFICADO",
      ocupacion == "ADMINISTRADOR" ~ "EMPLEADA",
      ocupacion == "PROFESIONA" ~ "EMPLEADA",
      ocupacion == "SOBRECARGO" ~ "EMPLEADA",
      ocupacion == "MAQUILLISTA" ~ "EMPLEADA",
      ocupacion == "otro independiente" ~ "NO ESPECIFICADO",
      ocupacion == "Otros" ~ "NO ESPECIFICADO",
      ocupacion == "Asistente" ~ "EMPLEADA",
      ocupacion == "artesana" ~ "EMPLEADA",
      ocupacion == "empleada mesera" ~ "EMPLEADA",
      ocupacion == "Estilista" ~ "EMPLEADA",
      ocupacion == "profesional" ~ "EMPLEADA",
      ocupacion == "profesional enfermeras" ~ "EMPLEADA",
      ocupacion == "pintores" ~ "EMPLEADA",
      ocupacion == "fisioterapéutas y quiroprácticos" ~ "EMPLEADA",
      ocupacion == "profesores de educación media superior" ~ "EMPLEADA",
      ocupacion == "tatuadora" ~ "EMPLEADA",
      ocupacion == "Trabajadores De La Educación" ~ "EMPLEADA",
      ocupacion == "ENFERMERA" ~ "EMPLEADA",
      ocupacion == "POLICIA" ~ "EMPLEADA",
      ocupacion == "FISIOTERAPEUTA" ~ "EMPLEADA"
    ))


datos$OCUPACION_LIMPIA[datos$OCUPACION_LIMPIA=="NO ESPECIFICADO"]<-NA_character_

#Conteo de variable limpia
datos %>% count(OCUPACION_LIMPIA)


######################### Variable religión ############################
#Ver valores unicos
unique(datos$religion)

#Renombrar observaciones
datos<- datos %>%
  mutate(
    RELIGION_LIMPIA = case_when(
      religion == "adventista" ~ "OTRA",
      religion == "adventista del séptimo d??a" ~ "OTRA",
      religion == "afiliación de iglesias evangélicas libres" ~ "OTRA",
      religion == "agnostica" ~ "NINGUNA",
      religion == "AGNOSTICA" ~ "NINGUNA",
      religion == "agrupación evangélica del esp??ritu santo" ~ "OTRA",
      religion == "alianza apostólica luterana" ~ "OTRA",
      religion == "alianza cristiana y misionera" ~ "OTRA",
      religion == "alianza cristiana:" ~ "OTRA",
      religion == "alianza evangélica:" ~ "OTRA",
      religion == "alianza presbiteriana" ~ "OTRA",
      religion == "amistad cristiana:" ~ "OTRA",
      religion == "amnostica" ~ "NINGUNA",
      religion == "anglicana" ~ "OTRA",
      religion == "asamblea cristiana" ~ "OTRA",
      religion == "asambleas de iglesias cristianas" ~ "OTRA",
      religion == "atea" ~ "NINGUNA",
      religion == "ATEA" ~ "NINGUNA",
      religion == "Atea" ~ "NINGUNA",
      religion == "ATEO" ~ "NINGUNA",
      religion == "bautista" ~ "OTRA",
      religion == "bautista cristiano" ~ "OTRA",
      religion == "BIDUSTA" ~ "OTRA",
      religion == "budismo" ~ "OTRA",
      religion == "budista" ~ "OTRA",
      religion == "BUDISTA" ~ "OTRA",
      religion == "c reyente" ~ "NO ESPECIFICADO",
      religion == "Calvinista" ~ "OTRA",
      religion == "Carmelita Descalza" ~ "OTRA",
      religion == "CAT?LICA" ~ "CATOLICA",
      religion == "católica" ~ "CATOLICA",
      religion == "Católica" ~ "CATOLICA",
      religion == "Católico" ~ "CATOLICA",
      religion == "Católico Apostólico Romano" ~ "CATOLICA",
      religion == "Católico Romano" ~ "CATOLICA",
      religion == "catoica" ~ "CATOLICA",
      religion == "catolica" ~ "CATOLICA",
      religion == "CATOLICA" ~ "CATOLICA",
      religion == "Catolica" ~ "CATOLICA",
      religion == "CATOLINA" ~ "CATOLICA",
      religion == "CATOLLICA" ~ "CATOLICA",
      religion == "centro cristiano:" ~ "OTRA",
      religion == "centro evangel??stico:" ~ "OTRA",
      religion == "centro familiar amistad cristiana" ~ "OTRA",
      religion == "CIENCIA CRISTIANA" ~ "OTRA",
      religion == "comunión cristiana pentecostés independiente" ~ "OTRA",
      religion == "comunión de creyentes" ~ "OTRA",
      religion == "comunidad cristiana:" ~ "OTRA",
      religion == "creyente" ~ "NO ESPECIFICADO",
      religion == "CREYENTE" ~ "NO ESPECIFICADO",
      religion == "Creyente" ~ "NO ESPECIFICADO",
      religion == "CREYENTES" ~ "NO ESPECIFICADO",
      religion == "cristiana" ~ "OTRA",
      religion == "Cristiana" ~ "OTRA",
      religion == "CRISTIANA" ~ "OTRA",
      religion == "cristiana espiritual" ~ "OTRA",
      religion == "Cristiana Espiritual" ~ "OTRA",
      religion == "Cristianas y evangélicas sin? sustento actual pentecostal" ~ "OTRA",
      religion == "cristiano del esp??ritu santo" ~ "OTRA",
      religion == "Cristianos Tradicionalistas" ~ "OTRA",
      religion == "CRISTINA" ~ "OTRA",
      religion == "cristo en el poder" ~ "OTRA",
      religion == "cristo viene" ~ "OTRA",
      religion == "de iglesias pentecostales" ~ "OTRA",
      religion == "DIU" ~ "NO ESPECIFICADO",
      religion == "EMPLEADA" ~ "NO ESPECIFICADO",
      religion == "en cristo jesús" ~ "OTRA",
      religion == "espiritista" ~ "OTRA",
      religion == "espiritualista" ~ "OTRA",
      religion == "espiritualista para el divino maestro y la pureza de mar??a" ~ "OTRA",
      religion == "espiritualista trinitario mariano:" ~ "OTRA",
      religion == "evangélica" ~ "OTRA",
      religion == "evangelica" ~ "OTRA",
      religion == "EVANGELISTA" ~ "OTRA",
      religion == "evangelista" ~ "OTRA",
      religion == "hinduismo" ~ "OTRA",
      religion == "iglesia apostólica:" ~ "OTRA",
      religion == "iglesia b??blica metodista" ~ "OTRA",
      religion == "iglesia católica apostólica ortodoxa" ~ "CATOLICA",
      religion == "iglesia católica apostólica ortodoxa del patriarca de moscú" ~ "CATOLICA",
      religion == "iglesia católica griega" ~ "CATOLICA",
      religion == "iglesia de jesucristo de los santos de los últimos d??as" ~ "OTRA",
      religion == "Iglesia de Jesucristo de los Santos de los ?sltimos D??as" ~ "OTRA",
      religion == "iglesia de santidad" ~ "OTRA",
      religion == "iglesia metodista" ~ "OTRA",
      religion == "iglesia ortodoxa católica" ~ "CATOLICA",
      religion == "jesuita" ~ "OTRA",
      religion == "jud??a" ~ "OTRA",
      religion == "mesiánicas" ~ "OTRA",
      religion == "Metodista" ~ "OTRA",
      religion == "misionera comunión de creyentes" ~ "OTRA",
      religion == "mormón" ~ "OTRA",
      religion == "mormona" ~ "OTRA",
      religion == "MORMONA" ~ "OTRA",
      religion == "Mormona" ~ "OTRA",
      religion == "musulmana" ~ "OTRA",
      religion == "N/E" ~ "NO ESPECIFICADO",
      religion == "Nada" ~ "NINGUNA",
      religion == "ni" ~ "NINGUNA",
      religion == "niguna" ~ "NINGUNA",
      religion == "ninguna" ~ "NINGUNA",
      religion == "Ninguna" ~ "NINGUNA",
      religion == "NINGUNA" ~ "NINGUNA",
      religion == "ninguna religión" ~ "NINGUNA",
      religion == "Ninguna religión" ~ "NINGUNA",
      religion == "ninguna|" ~ "NINGUNA",
      religion == "NO ESPECIFICADO" ~ "NO ESPECIFICADO",
      religion == "ortodoxa" ~ "OTRA",
      religion == "OTRA" ~ "OTRA",
      religion == "Otra" ~ "OTRA",
      religion == "OTRA AGNOSTICA" ~ "NINGUNA",
      religion == "OTRA BUDISMO" ~ "OTRA",
      religion == "OTRA COMUNION DE CREYENTES" ~ "OTRA",
      religion == "OTRA CREYENTE" ~ "OTRA",
      religion == "OTRA Creyente" ~ "OTRA",
      religion == "OTRA ESPIRITUALISTA" ~ "OTRA",
      religion == "OTRA EVANGELICA" ~ "OTRA",
      religion == "OTRA JUDIA" ~ "OTRA",
      religion == "OTRA MORMON" ~ "OTRA",
      religion == "OTRA PAGANO" ~ "OTRA",
      religion == "OTRA PALO MAYOMBO" ~ "OTRA",
      religion == "OTRA PROTESTANTE" ~ "OTRA",
      religion == "OTRA SANTERIA" ~ "OTRA",
      religion == "OTRA TESTIGOS DE JEHOVA" ~ "OTRA",
      religion == "OTRA YORUBA" ~ "OTRA",
      religion == "pentecostés" ~ "OTRA",
      religion == "pentecostes" ~ "OTRA",
      religion == "politeista" ~ "OTRA",
      religion == "protestante" ~ "OTRA",
      religion == "SANTA MUERTE" ~ "OTRA",
      religion == "SANTERA" ~ "OTRA",
      religion == "SANTER?\u008dA" ~ "OTRA",
      religion == "SANTERIA" ~ "OTRA",
      religion == "santeria" ~ "OTRA",
      religion == "santos de los últimos dias" ~ "OTRA",
      religion == "SE IGNORA" ~ "NO ESPECIFICADO",
      religion == "SIN ESPECIFICAR" ~ "NO ESPECIFICADO",
      religion == "Sin especificar" ~ "NO ESPECIFICADO",
      religion == "SIN REFERIR" ~ "NO ESPECIFICADO",
      religion == "SIN RELIGIÓNN" ~ "NINGUNA",
      religion == "Sin religión" ~ "NINGUNA",
      religion == "SIN RELIGION" ~ "NINGUNA",
      religion == "sincretista???" ~ "OTRA",
      religion == "tea" ~ "OTRA",
      religion == "testigo de geova" ~ "OTRA",
      religion == "testigo de jehova" ~ "OTRA",
      religion == "TESTIGO DE JEHOVA" ~ "OTRA",
      religion == "TESTIGO DE JEHOV?\u0081" ~ "OTRA",
      religion == "testigos de jehová" ~ "OTRA",
      religion == "Testigos de Jehová" ~ "OTRA",
      religion == "tradicionalistas" ~ "OTRA",
      religion == "ukumi" ~ "OTRA",
      religion == "vels:  adventista ... yuruba" ~ "OTRA",
      religion == "wicca" ~ "OTRA",
      religion == "YORUBA" ~ "OTRA",
      religion == "Yoruba" ~ "OTRA",
      religion == "yuruba" ~ "OTRA"
    ))

#Conteo de observaciones agrupadas
datos %>%  count(RELIGION_LIMPIA)

# En resumen:

#¡Limpiamos variables con texto, numéricas y de fecha!
# Nos apoyamos de 3 funciones: unique(), mutate() y case_When()

