library(readr)


###-------------------------------------------------------------------------------
### DGOL, FEN, U. de Chile 
### Taller #1 de R
###-------------------------------------------------------------------------------
##Notar que todo lo que est? escrito en una l?nea despu?s del signo # es un comentario (recordatorio, indicaci?n propia), no es relevante como instrucci?n a R
#El comentario puede ir en cualquier lugar, antecedido por #, incluso despu?s de un comando relevante para R
#--------------------------#
#---- Importar (cargar) base de datos ----- #
#--------------------------#
# Comentarios: Para hacer observaciones del codigo que no sean codigo en si mismas.
## directorio de trabajo: este es el lugar donde por defecto R buscar? archivos o guardar? archivos (salidas de R) a menos que otro directorio sea especificado
# setwd("C:/Users/juadi/OneDrive/Escritorio/Diploma_Pronostico_Demanda")
setwd("C:/Users/gmrad/Dropbox/FEN/Seminarios/Pronostico de demanda/R")

##Este directorio de arriba debe ser modificado por Uds. dependiendo de la carpeta donde est?n trabajando en sus PCs
##OJO que R reconoce /  No reconoce \

# Los datos son descargados de kaggle.com
# https://www.kaggle.com/PromptCloudHQ/imdb-data
# Esta base de datos contiene la informaci?n de la 1000 mejores pel?culas (2006-2016) seg?n descargas en IMDb (Internet Movie Database)

imdb_data <- read.csv(file = "IMDB-Movie-Data.csv")

# a) Notar que al usar setwd m?s arriba, ya no es necesario poner todo la direcci?n donde est? la base, porque ya estamos en la carpeta 
# especificada en setwd y, por tanto, s?lo basta poner en read.csv() el nombre de la base de datos
# b) Notar que en read.csv estamos usando la opci?n "header=T" lo que le indica a R que la primera fila de nuestra data son los nombres de las variables
# y no valores
# c) Notar que el archivo que contiene la base de datos especificada en read.csv DEBE estar guardada en la carpeta (o directorio) especificado en setwd

#-----------------------------------#
# --- Chequeos de la base de datos ----- #
#-----------------------------------#
# Un consejo: ver la base de datos que cargaron antes de analizarla...
head(imdb_data)                   # Para ver las primeras (seis) l?neas de la base de datos, incluyendo los nombres de las variables
dim(imdb_data)                    # Para ver la dimensi?n (filas y columnas) de la base de datos, da como resultado un vector de largo 2, el primer valor que arroja este comando es el n?mero de filas (observaciones) y el segundo el n?mero de columnas (variables)

View(imdb_data)
#--------------------------------------------------------- #
# --- Llamando partes espec?ficas de la base de datos --- #
#---------------------------------------------------------#

## segunda fila, d?cima columna de la base de datos
b=imdb_data[2,10]
## quinta fila (todas la columnas) 
imdb_data[5,]
## 11va columna
imdb_data[,11]
##Notar que estas partes de la base de datos que es posible llamar pueden ser guardadas en otras variables (vectores, submatrices)
f=imdb_data[,11]
##Por ejemplo guardar en la variable f la 11va columna de la base de datos (entonces f es un vector)


## Llamando una columna por su nombre
imdb_data$Director
imdb_data[,5]
imdb_data[,"Director"]


## Ojo 1: hay que ser preciso en el nombre de la base de datos y el nombre de las columnas
## Ojo 2: estas nuevas variables pueden ser guardadas en otras variables, de ser requerido


#-------------------------------------------- #
# --- Estad?sticas descriptivas  --- #
#-------------------------------------------- #

## Para variable cuantitativas
resumen=summary(imdb_data$Rating)          # resumen r?pido de las observaciones, icnluyendo m?nimo, m?ximo, mediana, etc.
resumen[4]

summary(imdb_data)  

mean(imdb_data$Rating)              # promedio simple de las observaciones
sd(imdb_data$Rating)    # desviaci?n est?ndar de las observaciones
var(imdb_data$Rating)
quantile(imdb_data$Rating,probs=c(0.1,0.4,0.5,0.9,0.95)) # cuantiles de la distribuci?n emp?rica de las observaciones, en este caso los cuantiles 10% , 50% y 90%. Esta opci?n se puede cambiar, agrandar, acortar 


## Tabulaci?n para variables categ?ricas
table(imdb_data$Year)              # Aunque a?os es un entero, lo tabul? para ver el n?mero de pel?culas por a?o en el ranking  


#-------------------------------------------- #
# --- Obteniendo subconjunto relevante de los datos seg?n alg?n criterio l?gico ------ #
#-------------------------------------------- #

# a==b le da la instrucci?n a R de comparar si a es igual a  b elemento por elemento (a y b son vectores de igual dimensi?n o a es un vector y b es un n?mero)
# retorna un vector de igual tama?o que a donde
# cada elemento es T (verdadero) o F (falso)
# Si al vector de T/F se le aplica una funci?n para variables categ?ricas, R trata los T como True (verdadero) y los F como False (Falso)
# Si al vector de T/F se le aplica una funci?n para variables cuantitativas, R autom?ticamente trata los T como 1 y los F como 0
y2014indicador <- imdb_data$Year==2014
y2014indicador = imdb_data$Year==2014
table(y2014indicador) # Tabula la variable categ?rica del vector
sum(y2014indicador)   # Suma los elementos del vector
length(y2014indicador)

sum(y2014indicador)/length(y2014indicador)
mean(y2014indicador)

# a>b pregunta si a es mayor que b
ind1 = imdb_data$Year>2014  ## que pasar?a si en vez usamos imdb_data$Year>=2014
ind2 = imdb_data$Year == 2014 & imdb_data$Runtime..Minutes. > 160      # & es el operador l?gico "y"
ind3 = imdb_data$Rating > 8 | imdb_data$Revenue..Millions.> 100        # | es el operador l?gico "o"

ind4=(imdb_data$Year == 2014 & imdb_data$Runtime..Minutes. > 160) & (imdb_data$Rating > 8 | imdb_data$Revenue..Millions.> 100)


# Combinando argumentos l?gicos 
# a==b & c>d  # Ambos deben ser verdad
# a==b | c > d # Al menos uno es verdad

# Tambi?n se pueden utilizar argumentos l?gicos para extraer un subconjunto relevante de datos 
imdb_data[y2014indicador,] # Da como resultado las filas de la base de datos (y todas las columnas) asociadas a y2014indicador con True
summary(imdb_data$Runtime..Minutes.[y2014indicador]) # Resume Runtime s?lo para pel?culas de 2014 (las asociadas a y2014indicador con True)
# Ojo que no estoy guardando el resultado de estas l?neas anteriores en ninguna nueva variable, pero de ser necesario se puede hacer

imdb_data_2014 <- imdb_data[y2014indicador,]

# Otra forma de obtener un subconjunto relevante de informaci?n de la base datos es usando el comando "subset" (subconjunto en ingl?s)

imdb_data2014=subset(imdb_data,Year==2014)
# guarda en imdb_data2014 el subconjunto de imdb_data (todas las observaciones) que cumple con la condici?n que Year es 2014

imdb_data2014_a=subset(imdb_data, Year==2014 & Rating>8) #qu? hace esta l?nea?

imdb_data2014_b=subset(imdb_data, Year==2014 | Rating>8) #qu? hace esta l?nea?

imdb_data2014_c=subset(imdb_data, (Year<=2016 | Rating>8) & (Year==2015 | Runtime..Minutes.>130))

write.csv(imdb_data2014, "imdb_data2014.csv", row.names = F)
