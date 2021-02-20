

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




#### Feb 20

# Review 
# https://qcbsrworkshops.github.io/workshop05/pres-en/workshop05-pres-en.html#1
# Objects

c <- c(1,2,3,4)
c2 <- c(T,F,T,F)

myMatrix <- matrix(c(1,1,1,1,0,0,0,0,1,1,0,1,1,1,1), 4,5)

myDF <- data.frame("Var1" = c(1:10),
                   "Var2" = rep(c("a","b","c","d","e"),2),
                   "Var3" = sample(c(1:100),10))

myList <- list("c" = c,
               "c2" = c2,
               "Ambiente" = myMatrix, 
               "Species" = myDF)

str(myList)

# Functions

a <- 2
b <- a+a

c <- sqrt(2)

library(vegan)

resul <- vegan::decostand(c(1:30), "max")


myFunction <- function(a,b,c){
  g <- a+b
  g <- g/c
  g <- g^2
  return(g)
}


plot(myFunction(1,4,5),
     myFunction(1,4,5))


## Control flow 


## Decision making 
## if , else, ifelse 
r <- c(1:10)

## Evalua 1 elemento
if(r>5){
  print("blue")
}else{
  print("red")
}

# Evalua sobre un vector
ifelse(r>5,print("blue"), print("red"))

# condiciones mas complejas 

ifelse(r>5,
       print("blue"), 
       ifelse(r<3,
              print("red"),
              ifelse(r == 1,
                     print("UNO"),r
              )))

if((2+2) == 4){
  print("Arithmetic works.")
}else {
  print("Houston, we have a problem.")
  
}

  

a <- c(1:10,30:40)

ifelse(a>10, "mayor 10", "menor10")
c <- c()
for(i in 1:length(a)){
  
  c[i] <- if(a[i]>10){print("mayor10")}
  else{print("menor10")}
  
}



for(i in 1:length(a)){
  
  c[i] <- a[i]^2
  
}


### Apply family

myMat <- matrix(c(1:10,21:30), 10,2 )

apply(myMat,2,mean)

b <- myMat[,1]


c <-c()
t1 <- Sys.time()
for(i in 1:length(b)){
  c[i] <- b[i]/sum(b)
  
}
t2 <- Sys.time()
t2-t1

t3 <- Sys.time()
sapply(1:length(b), function(x) b[x]/sum(b))
t4 <- Sys.time()


lapply(1:length(b), function(x) data.frame(b[x]/sum(b), c))

#############
## for loop
## break and next 
## repeat
## while 

### Exercise with own data 













