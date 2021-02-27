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


# data management
# importing files
# exploring data 
## Dataset structure
## Cell characteristics 
# manipulating data 
## Subsets 
## Match and %in%  


#########
# Sat. Feb 27. 
#############


## Functions 


MyFunction <- function(a = 1,b = 2,c = 3){
  d <- a +b+c
  return(d)
}


MyFunction2 <- function(a,b,c){
  d <- a +b+c
  return(d)
}


## Aplicar una funcion 

MyFunction <- function(a = 1,b = 2,c = 3){
  d <- a +b+c
  return(d)
}
MyFunction2 <- function(a,b,c){
  d <- a +b+c
  return(d)
}
MyFunction()
MyFunction(2,3,4)
MyFunction(a = 2,b = 3,c = 4)
MyFunction(b = 2,a = 3,c = 4)
MyFunction <- function(a = 1,b = 2,c = 3){
  d <- a +b+c # primero suma
  d <- d*c # sobrescribe d
  return(d)
}
MyFunction(b = 2,a = 3,c = 4)
MyFunction(a = 2,b = 3,c = 4)
MyFunction(a = 2,c = 3,b = 4)
MyFunction2()
MyFunction2(1)
MyFunction2(1,2)
MyFunction2(1,2,3)
MyFunction2(b = 1, c= 2, a = 3)
MyFunction3 <- function(a = 1,b = 2,c){
  d <- a +b+c
  return(d)
}
MyFunction3(3)
MyFunction3 <- function(a = 1,b = 2,c){
  d <- a +b+c
  return(d)
}
MyFunction3(c= 3)
MyFunction4 <- function(a = 1,
                        b = 2,
                        c = T){
  d <- a+b
  if(c == T){
    e <- sin(d)
  }else{
    f
  }
  return(e)
}
MyFunction4(45,45,T)
sin(90)
MyFunction4(45,45,F)
MyFunction4 <- function(a = 1,
                        b = 2,
                        c = T){
  d <- a+b
  if(c == T){
    e <- sin(d)
  }else{
    f <- d
  }
  return(e)
}
MyFunction4(45,45,T)
MyFunction4(45,45,F)
MyFunction4 <- function(a = 1,
                        b = 2,
                        c = T){
  d <- a+b
  if(c == T){
    e <- sin(d)
  }else{
    f <- d
  }
  ifelse(c == T, return(e), return(f))
}
MyFunction4(45,45,F)
MyFunction5 <- function(a = 1,
                        b = 2,
                        c = T){
  d <- a+b
  if(c == T){
    e <- sin(d)
  }else{
    return(d)
  }
  return(e)
}
MyFunction5(45,45,F)
MyFunction6 <- function(a = 1,
                        b = 2,
                        c = T){
  d <- a+b
  if(c == T){
    d <- sin(d)
  }else{
    d
  }
  return(d)
}
MyFunction6(45,45,F)
MyFunction6 <- function(a = 1,
                        b = 2,
                        c = T){
  d <- a+b
  if(c == T){
    d <- sin(d)
  }else{
    d
  }
}
MyFunction6(45,45,F)
MyFunction6 <- function(a = 1,
                        b = 2,
                        c = T){
  print("Sumando d")
  d <- a+b
  print("Done")
  if(c == T){
    d <- sin(d)
  }else{
    d
  }
  return(d)
}

MyFunction6(45,45,F)
e <- MyFunction6(45,45,F)
e

MyFunction7 <- function(a = 90,
                        c = T){
  print("Sumando d")
  d <- a
  print("Done")
  if(c == T){
    d <- sin(d)
  }else{
    d
  }
  return(d)
}
MyFunction7()
MyFunction7 <- function(a = 90,
                        c = T){
  print("Sumando d")
  d <- a
  print("Done")
  if(c == T){
    d <- sin(d)
  }else{
    d
  }
  plot(d~a)
}

MyFunction7()

seq(1,360,1)

MyFunction7(a = seq(1,360,1))

MyFunction7 <- function(a = 90,
                        c = T){
  print("Sumando d")
  d <- a
  print("Done")
  if(c == T){
    d <- sin(d)
  }else{
    d
  }
  return(d)
}



MyFunction7(1)
MyFunction7(0)


plot(MyFunction7(0)~0)
plot(MyFunction7(0)~0,
     xlim = c(0,360),
     ylim = c(-1,1))
cat("\014")
print("hello")
cat("\014")


plot(MyFunction7(0)~0,
     xlim = c(0,360),
     ylim = c(-1,1))



for(i in seq(from = 1,to = 360,by = 0.1)){
  print(i)
  points(MyFunction7(i)~i,
         col = "red")
  cat("\014")
}



#############

sin(1)



par(mfrow = c(2,3), mar = c(2,2,2,2), oma = c(2,2,2,2))

for(i in 1:6){
  a <- sin(seq(1,360,0.1)*i)
  
  plot(a,
       type = "l",
       col = "red",
       main = paste("i being equal to: ", i),
       xlim = c(1,360), 
       ylim = c(-1,1),
       frame = F)
  
  
}





par(mfrow = c(2,3), mar = c(5,5,5,5), oma = c(2,2,2,2))

for(i in 1:6){
  a <- sin(seq(1,360,0.1)*i)
  
  plot(a,
       type = "l",
       col = "red",
       main = paste("i being equal to: ", i),
       xlim = c(1,360), 
       ylim = c(-1,1),
       frame = F)
  
  
}


par(mfrow = c(2,3), mar = c(2,2,2,2))

for(i in 1:6){
  a <- sin(seq(1,360,0.1)*i)
  
  plot(a,
       type = "l",
       col = "red",
       main = paste("i being equal to: ", i),
       xlim = c(1,360), 
       ylim = c(-1,1),
       frame = F)
  
  
}


dev.off() # borra los parametros

layout(matrix(c(1,2,3,5,4,6,6,6,6,6,6,6),2,4))

layout.show()

## Exploracion de datos y fundamentos de estadistica descriptiva

####

## load data
SaPhyto_7513 <- read.csv(file = "Saidenbach-Phytoplankton_1975-2013.csv", 
                         header = T, 
                         stringsAsFactors = F)

SaPhyto_7513$Sampling.date..YYYYMMDD. <- as.POSIXct(as.character(SaPhyto_7513$Sampling.date..YYYYMMDD.), 
                                                    "%Y%m%d", tz = "CET")


head(SaPhyto_7513)
dim(SaPhyto_7513)
names(SaPhyto_7513)


plot(SaPhyto_7513$Biovolume..cubic.micrometers.~SaPhyto_7513$Sampling.date..YYYYMMDD.,
     pch = "+")


SaPhyto_7513$Sampling.depth..cm.below.surface. <- as.factor(SaPhyto_7513$Sampling.depth..cm.below.surface.)
str(SaPhyto_7513)

## muestra aleatoria de los datos 
SaPhyto_7513b <- SaPhyto_7513[sample(x = 1:length(SaPhyto_7513$Sampling.date..YYYYMMDD.), 
                                    size = 3000),]
# revisar q la muestra fue correcta
length(SaPhyto_7513b$Sampling.date..YYYYMMDD.) == 3000


# usar muestra para plot

hist(log(SaPhyto_7513b$Biovolume..cubic.micrometers))
boxplot(SaPhyto_7513b$Biovolume..cubic.micrometers.~SaPhyto_7513b$Sampling.depth..cm.below.surface.,
        log = "y")

par(las = 2)

boxplot(log(SaPhyto_7513b$Biovolume..cubic.micrometers.)~SaPhyto_7513b$Sampling.depth..cm.below.surface., 
        frame = F)
abline(h=12)


SaPhyto_7513b[log(SaPhyto_7513b$Biovolume..cubic.micrometers.)>12, ]




## crear un vector de symbolos
pch <- ifelse(SaPhyto_7513b$Biovolume..cubic.micrometers. <1000, 1,
                 ifelse(SaPhyto_7513b$Biovolume..cubic.micrometers. < 2000, 2, 
                        ifelse(SaPhyto_7513b$Biovolume..cubic.micrometers. < 3000, 3,4)))

## crear un vector de colores 
colors <- ifelse(SaPhyto_7513b$Biovolume..cubic.micrometers. <1000, '#66c2a5',
              ifelse(SaPhyto_7513b$Biovolume..cubic.micrometers. < 2000, '#fc8d62', 
                     ifelse(SaPhyto_7513b$Biovolume..cubic.micrometers. < 3000, '#8da0cb','#e78ac3')))



plot(log(SaPhyto_7513b$Biovolume..cubic.micrometers.)~SaPhyto_7513b$Sampling.date..YYYYMMDD. ,
     frame = F,
     cex = 0.4,
     pch = pch, 
     col = colors)

######

## 
SaPhyto_7513b$taxa <- sample(1:6,length(SaPhyto_7513b$Species), replace = T)

boxplot(log(SaPhyto_7513b$Biovolume..cubic.micrometers.)~SaPhyto_7513b$taxa)

####
# load custom functions from wd. 
source("Functions.R")

## utilize the function we just created 
       
par(mfrow = c(1,2))

mean <- Fun2Sum(datos = SaPhyto_7513b,
                name = "Biovolume..cubic.micrometers.operacion",
                operacion = "mean")
sd <- Fun2Sum(datos = SaPhyto_7513b, operacion = "sd")


######


dev.off()


























