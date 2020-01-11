library(corrplot)
library(Hmisc)
library(dplyr)
library(tidyr)
library(openxlsx)
library(rJava)
library(xlsx)

#------------------------------- LIMPIEZA DE DATOS
# Importamos el dataset para poder tranformar las variables y los valores que nos son necesarios.
df <- read.xlsx('pax_data.xlsx', sheet = 1)

# En primer lugar, transformaremos los valores que son categóricos ya que en la PEC3 se especificó que
# para hacer una comparación entre todos las variables categóricas se debían transformar a un solo grupo
# de categorías; en este caso tranformar las que tienen 4 categorías a variables booleanas.

# Primero exploraremos si existen valores perdidos. 
colSums(is.na(df)|df == '')

# Vemos que para estas variables no hay valores nulos. 
# Gracias al documento de la descripción del dataset entregado en la PEC anterior sabemos 
# qué variables debemos transformar a booleanas.
df[,c(21:75)] <- data.frame(apply(df[,c(21:75)], 2, as.factor))

cat_colnames <- list("GCh", "GDis", "GAge", "GMig", "GRa", "GRe", "GInd", "GOth", "GRef", "GSoc", "Pol", "ConRen"
                     , "Cons", "Ele", "ElecComm", "PolPar", "Pubad", "Polps", "Terps", "Eps", "Mps", "EqGen", "HrDem", "Prot", 
                     "HrFra", "HrNi", "HrIi", "Med", "HrCit", "Dev", "SsrGua", "Ce", "SsrPol", "SsrArm", "SsrDdr", "SsrInt", "Cor"
                     ,"SsrCrOcr", "SsrDrugs")


for (i in cat_colnames){
  levels(df[[i]]) <- list("0" = c("0", "1"), "1" = c("2", "3"))
  df[[i]] <- as.numeric(as.character((df[[i]])))
}

df[,c(21:75)] <- data.frame(apply(df[,c(21:75)], 2, as.numeric))

# Crearemos ahora una variable booleana en la que se podrá ver si hay implicación
# de terceras partes (1) o no (0)
df <- df%>%
  mutate(third_parties = ifelse(is.na(ThrdPart),
                                0, 1))

df_asp <- melt(df[,c(6,21:75)], id.vars=c("AgtId"))
df_asp <- df_asp%>%
  dplyr::filter(value > 0)

df_asp <- merge(df_asp, df[,c(1:12,76)], by = c("AgtId"))

write.csv(df, file = 'pax_data_cleaned.csv', row.names = FALSE)
write.csv(df_asp, file = 'aspect_data.csv', row.names = FALSE)

# A continuación veremos si hay añguna correlación entre las variables de aspectos abordados; para saber si
# la existencia de uno suele venir acompañada de otro o de manera inversa.
cor_matrix <- rcorr(as.matrix(df[,c(21:75)]), type = c("pearson"))
corrplot(cor_matrix$r, method = "number", type="upper", order="original", 
         p.mat = cor_matrix$P, sig.level = 0.05, insig = "blank")


#------------------------------- PAISES
# A continuación haremos las transformaciones para obtener la información necesaria para hacer el dashboard 1 gráfico 1
# en el cual se representan los países que han interaccionado entre ellos en un acuerdo de paz.

# Siguiendo los pasos de este blog de Tableau, transformaremos nuestro dataframe para que sea de la misma forma que el
# aportado en el blog: https://help.tableau.com/current/pro/desktop/en-us/maps_howto_origin_destination.htm#Order

# Primero seleccionaremos de entre nuestras variables solamente la referente a los países y una que nos sirva de ID.
# En este caso, AgtId.
df_con <- df%>%
  select(1,6)

# A continuación separaremos el string de los países para poner un país en un columna.
# Por si son factores, los transformamos a characters
con <- as.character(df_con$Con)

# Los separamos con el símbolo "/"
con <- strsplit(con, "/")

# Buscamos el mayor número de países involucrados en un mismo acuerdo.
maxLen <- max(sapply(con, length))

# fill in any blanks. The t() is to transpose the return from sapply
con <- 
  t(sapply(con, function(x)
    # append to x, NA's.  Note that if (0 == (maxLen - length(x))), then no NA's are appended 
    c(x, rep(NA, maxLen - length(x)))
  ))

# añadimos el nombre de las columnas
colnames(con) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

# Lo juntamos con el AgtId
df_con <- data.frame(AgtId=df_con$AgtId, con)

# Ahora tendremos que transponer la tabla para tener cada país en una fila, con su AgtID y el orden en el que
# estban los países en el string del principio.
library(reshape2)
df_con <- melt(df_con, id.vars=c("AgtId"))
df_con <- df_con[complete.cases(df_con), ]

# Eliminamos la X de los valores de la columna variable puesto que nos interesa que sea una variable numerica.
df_con$variable <- gsub("X", "\\1", df_con$variable)

# Renombramos las columnas del dataframe.
colnames(df_con) <- c("AgtId", "Order", "name")

# Importamos el dataset de países que hemos tratado previamente a mano.
# Los países o regiones que no se encontraban previamente en el dataset; las hemos introducido a mano en función
# de la proximidad territorial a un país que sí estaba en la listsa puesto que de esta manera, se podrá realizar 
# el mapa en tableau.
countries <- read.csv(file = 'paises.csv', sep = ';')

# Juntamos los dos dataframes para tener las variables con información geográfica.
country_geo <- merge(df_con, countries, by = c("name"))

# seleccionamos las variables que nos interesan de el conjunto de datos inicial sobre los acuerdos de paz.
agt_data <- df[,c(2,5,6,7,8,9,10,11,12,20)]

# hacemos un merge final con estos datos y los países por el ID del acuerdo. 
final_geo_df <- merge(country_geo, agt_data, by = c("AgtId"))

# Guardamos este dataset
write.csv(final_geo_df, file = 'geographic_data.csv', row.names = FALSE)
