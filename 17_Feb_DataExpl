

######
# Manipulacion y arreglo de datos (START)
######

dada <- c(1,2,3,4,5)
# remover datos del environment 
rm(list = ls())

# crear objectos
data <- mean(x = c(1,2,3,4,5))
# crear dataframes
myDF <- data.frame("a" = c(1:10),
                   "b" = c(21:30))

# examinar dataframe 
head(myDF, 5)
tail(myDF, 2)
# cambiar nombres de variables
names(myDF) <- c("c", "d")
# atributos del data.frame
attributes(myDF)
# dimensiones del dataframe
dim(myDF) # filas, columnas
dim(myDF)[2]

nrow(myDF) # filas
ncol(myDF) # columnas 


summary(myDF)# resumen estadistica simple
str(myDF) # estructura de datos

length(unique(myDF$d)) # cuantos unicos? 

apply(apply(myDF, 2, unique), 2, length) # cuantos unicos aplicado a todo el data.frame



####### Data cleaning ######


SaPhyto_7513 <- read.csv(file = "Saidenbach-Phytoplankton_1975-2013.csv", 
                         header = T, 
                         stringsAsFactors = F) # cargar data
head(SaPhyto_7513) 
dim(SaPhyto_7513)
names(SaPhyto_7513)

## check for errors 


# hay NA's in my data? 
table(is.na(SaPhyto_7513$Species))

table(complete.cases(SaPhyto_7513))

# crear un error
SaPhyto_7513$Species[303] <- ""
# investigar q celdas tienen error y eliminar del dataframe
which(SaPhyto_7513$Species %in% c("", "NA","na","none"))
SaPhyto_7513 <- SaPhyto_7513[-303,]

## tengo outliers? 

# datos per se
hist((SaPhyto_7513$Biovolume..cubic.micrometers.))
# datos transformados 
hist(log(SaPhyto_7513$Biovolume..cubic.micrometers.))
# applicar un treshold
SaPhyto_7513[SaPhyto_7513$Biovolume..cubic.micrometers. > 999,]


## By Date exploration 


SaPhyto_7513$Sampling.date..YYYYMMDD. <- as.POSIXct(as.character(SaPhyto_7513$Sampling.date..YYYYMMDD.), 
                                                    "%Y%m%d", tz = "CET")



range(SaPhyto_7513$Sampling.date..YYYYMMDD.)

newVar <- stringr::str_split(SaPhyto_7513$Sampling.date..YYYYMMDD., pattern = "-", simplify = T)

newVar <- apply(newVar,2, as.numeric)
colnames(newVar) <- c("Year", "Month", "Day")

SaPhyto_7513Sa <- cbind(SaPhyto_7513, data.frame(newVar))

ObsYear <- aggregate(SaPhyto_7513Sa$Biovolume..cubic.micrometers., 
                     by = list(SaPhyto_7513Sa$Year),
                     length)

ObsYear <- aggregate(SaPhyto_7513Sa$Biovolume..cubic.micrometers., 
                     by = list(SaPhyto_7513Sa$Year),
                     length)

sd(ObsYear$x)




######
# Manipulacion y arreglo de datos (END)
######










