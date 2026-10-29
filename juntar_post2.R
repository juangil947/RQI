#### Lectura de datos y filtrado ------------------------
#Cargar librerías:
rm(list = ls()) 
setwd("C:/investigaciones/a2026/rqi")
# 1. Corpus
### ----- Paso 1.- Extracción de la información--------------
library(readr)
datmovistar<-read_csv("posts_movistar.csv")
datvodafone<-read_csv("posts_vodafone.csv")
datorange<-read_csv("posts_orange.csv")
datdigi<-read_csv("posts_digimobil.csv")
#str(top_net_urls)
datmovistar$id<-1 #movistar
datvodafone$id<-2 #vodafone
datorange$id<-3 #orange
datdigi$id<-4 #digi
datpost<-rbind(datmovistar,datvodafone,datorange,datdigi)
library(dplyr)
conten<-datpost |>
  select(id,text,tweet_url,view_count,created_at) |>
  mutate(envio=sub("^([^ ]*).*", "\\1", text), 
         texto=sub("^[^ ]* ", "", text))
library(lubridate)
fecha<-sub("^[^ ]* ", "", conten$created_at)
fecha<- parse_date_time(fecha, 
                        orders = "b d H:M:S z Y")
anio <- year(fecha)
mes  <- month(fecha)   # Devuelve el número del mes (6)
dia  <- day(fecha)
hora <- format(fecha, "%H:%M:%S")
n<-dim(datpost)[1]
datos<-data.frame(num=c(1:n),id=conten$id,denvio=conten$envio,texto=conten$texto,
                  oriurl=conten$tweet_url,viewc=conten$view_count,
                  anio,mes,dia,hora)
## Eliminar post de mensajes comerciales
vdat<-which(substr(datos$denvio,1,1)!='@') # Mensajes comerciales
datos<-datos[-vdat,] # mensajes válidos

###--------Limpieza de texto (IMPORTANTE)
library(stringr)
datos$texto <- tolower(datos$texto)
datos$texto <- gsub("http\\S+", "", datos$texto)   # URLs
datos$texto <- gsub("@\\w+", "", datos$texto)      # menciones
datos$texto <- gsub("#", "", datos$texto)          # hashtags símbolo
datos$texto <- gsub("[^[:alnum:][:space:]]", "", datos$texto)
datos$texto <- trimws(datos$texto) #Elimine los espacios en blanco 
   #iniciales y/o finales de las cadenas de caracteres.
datos <- datos[datos$texto != "" & !is.na(datos$texto), ]
###--------------------
table(datos$id)
### ### Estadística longitud del texto
lontex<-as.integer(sapply(datos$texto,nchar)) 
median(lontex) ### mediana
IQR(lontex) ### rango intercuartílico

#----------------------------------------------------------
postx<-datos$texto

# 1. PAQUETES ---------------------------
library(rollama)
library(jsonlite)
library(dplyr)
#### ------------------
# 2. FUNCIÓN DE LIMPIEZA ADAPTADA A TEXTOS SIN COMILLAS
limpiar_json <- function(txt){
  
  txt <- gsub("```json", "", txt, ignore.case = TRUE)
  txt <- gsub("```", "", txt)
  txt <- gsub("[\r\n\t]", " ", txt)
  
  txt <- gsub('"empatía"', '"empatia"', txt, ignore.case = TRUE)
  txt <- gsub('"personalización"', '"personalizacion"', txt, ignore.case = TRUE)
  txt <- gsub('"resolución"', '"resolucion"', txt, ignore.case = TRUE)
  txt <- gsub('"atención_cliente"', '"atencion_cliente"', txt, ignore.case = TRUE)
  
  trimws(txt)
}


# 3. FUNCIÓN PRINCIPAL DIRECTA Y NATIVA
clasificar_tweet <- function(tweet,
                             modelo=modelo,
                             debug=FALSE){
  
  prompt <- paste0(
    'Eres un investigador especializado en calidad de atención al cliente en el sector de las telecomunicaciones.

Debes analizar UNA respuesta de un operador de telecomunicaciones a un cliente.

Devuelve EXCLUSIVAMENTE un objeto JSON válido.

No escribas ningún texto antes ni después del JSON.

Utiliza EXACTAMENTE este esquema:

{
"sentimiento":"",
"empatia":0,
"resolucion":0,
"personalizacion":0,
"tono":"",
"tema_principal":"",
"urgencia":""
}

CRITERIOS DE EVALUACIÓN

1. sentimiento

Solo puede tomar uno de estos valores:

muy_negativo
negativo
neutro
positivo
muy_positivo

------------------------------------------------

2. empatia (1-5)

Evalúa el grado en que el operador comprende el problema del cliente.

1 = Ninguna empatía. Respuesta fría o automática.

2 = Escasa empatía.

3 = Empatía correcta pero limitada.

4 = Buena empatía. Reconoce claramente el problema.

5 = Excelente empatía. Comprende el problema, pide disculpas cuando procede y transmite interés por ayudar.

------------------------------------------------

3. resolucion (1-5)

Evalúa la capacidad de la respuesta para resolver la incidencia.

1 = No ofrece ninguna solución.

2 = Sugiere contactar pero sin aportar información útil.

3 = Propone una actuación básica.

4 = Ofrece una solución bastante concreta.

5 = La incidencia queda resuelta o explica claramente cómo resolverla.

------------------------------------------------

4. personalizacion (1-5)

Evalúa si la respuesta está adaptada al caso concreto.

1 = Respuesta totalmente genérica.

2 = Muy poco personalizada.

3 = Moderadamente personalizada.

4 = Bastante personalizada.

5 = Claramente personalizada al problema concreto del cliente.

------------------------------------------------

5. tono

Solo puede ser:

frio
neutro
amable
muy_amable

------------------------------------------------

6. tema_principal

Solo puede ser:

cobertura
velocidad_datos
precio
atencion_cliente
facturacion
otro

Si existen varios temas, selecciona únicamente el más importante.

------------------------------------------------

7. urgencia

Solo puede ser:

baja
media
alta

------------------------------------------------

IMPORTANTE

No cambies los nombres de las variables.

No inventes categorías.

No utilices tildes en los nombres de las variables.

No añadas comentarios.

La salida debe ser exclusivamente un JSON válido.

Texto a analizar:

Texto:

',tweet)
  
  df_error <- data.frame(
    
    sentimiento=NA,
    empatia=NA,
    resolucion=NA,
    personalizacion=NA,
    tono=NA,
    tema_principal=NA,
    urgencia=NA,
    
    stringsAsFactors=FALSE)
  
  res <- tryCatch(
    query(
      prompt,
      model = modelo,
      model_params = list(
        temperature = 0,
        top_p = 1,
        repeat_penalty = 1
      )
    ),
    error = function(e) NULL
  )
  
  
  
  if(is.null(res))
    return(df_error)
  
  #####################################################
  ## Detectar automáticamente la estructura de rollama
  #####################################################
  
  texto <- NULL
  
  if(!is.null(res$message$content))
    texto <- res$message$content
  
  if(is.null(texto))
    if(length(res)>0)
      if(!is.null(res[[1]]$message$content))
        texto <- res[[1]]$message$content
  
  if(is.null(texto))
    return(df_error)
  
  #####################################################
  
  if(debug){
    
    cat("\n======================\n")
    cat(texto)
    cat("\n======================\n")
    
  }
  
  #####################################################
  ## localizar JSON
  #####################################################
  
  ini <- regexpr("\\{",texto)
  
  fin <- tail(gregexpr("\\}",texto)[[1]],1)
  
  if(ini<1 | fin<1)
    return(df_error)
  
  texto_json <- substr(texto,ini,fin)
  
  texto_json <- limpiar_json(texto_json)
  
  if(debug){
    
    cat("\nJSON LIMPIO\n")
    cat(texto_json)
    cat("\n")
    
  }
  
  #####################################################
  
  parsed <- tryCatch(
    
    jsonlite::fromJSON(texto_json),
    
    error=function(e){
      
      if(debug){
        
        cat("\nERROR JSON\n")
        print(e)
        
      }
      
      NULL
      
    })
  
  if(is.null(parsed))
    return(df_error)
  
  #####################################################
  
  campos <- c(
    
    "sentimiento",
    "empatia",
    "resolucion",
    "personalizacion",
    "tono",
    "tema_principal",
    "urgencia"
    
  )
  
  for(v in campos){
    
    if(is.null(parsed[[v]]))
      parsed[[v]] <- NA
    
  }
  
  #####################################################
  
  data.frame(
    
    sentimiento=as.character(parsed$sentimiento),
    
    empatia=as.numeric(parsed$empatia),
    
    resolucion=as.numeric(parsed$resolucion),
    
    personalizacion=as.numeric(parsed$personalizacion),
    
    tono=as.character(parsed$tono),
    
    tema_principal=as.character(parsed$tema_principal),
    
    urgencia=as.character(parsed$urgencia),
    
    stringsAsFactors=FALSE)
  
}
################----------------- 

#4. PROCESAR LOS TWEETS---------------
tweet <- postx
set.seed(17)
inicio <- Sys.time() # tiempo de inicio
resultados <- lapply(tweet, function(x){
  cat("Procesando:", x, "\n")
  clasificar_tweet(x, modelo = "gemma3")
})

df <- bind_rows(resultados)
sal<-data.frame(datos,df)
final <- Sys.time() # final del proceso
print(final-inicio)

###------ salvar resultados----------------
write.table(sal, file = "post_gemma3v2.txt", 
            sep = "\t", row.names = FALSE)

