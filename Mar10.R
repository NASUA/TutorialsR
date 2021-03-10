#### Mar 10
## Descriptive community analysis 

library(DataExplorer)

## load data
SaPhyto_7513 <- read.csv(file = "Saidenbach-Phytoplankton_1975-2013.csv", 
                         header = T, 
                         stringsAsFactors = F)

SaPhyto_7513$Sampling.date..YYYYMMDD. <- as.POSIXct(as.character(SaPhyto_7513$Sampling.date..YYYYMMDD.), 
                                                    "%Y%m%d", tz = "CET")


SaPhyto_7513$Biovolume..cubic.micrometers. <- SaPhyto_7513$Cells..1.L.* (SaPhyto_7513$Cells..1.L./1000000000)
max(SaPhyto_7513$Biovolume..cubic.micrometers.)


## Testing DataExplorer for rapid EDAs

DataExplorer::plot_intro(SaPhyto_7513[1:100,])

DataExplorer::plot_correlation(SaPhyto_7513[1:100,])

DataExplorer::plot_qq(SaPhyto_7513[1:100,])

DataExplorer::plot_density(SaPhyto_7513[1:100,])

DataExplorer::create_report(SaPhyto_7513[1:100,])

########################################

head(SaPhyto_7513)

### Examinar variacion temporal en communidades a diferentes profundidaes
TimeXDepth <- xtabs(log1p(Cells..1.L.)~Sampling.date..YYYYMMDD.+Species + Sampling.depth..cm.below.surface.,
                    data = SaPhyto_7513 )



# Dimensiones 
dim(TimeXDepth)
# richness trought time 
plot(rowSums(TimeXDepth[,,1]))



paMAT <- TimeXDepth # presence / abscence 
paMAT[paMAT>1] <- 1 # 0/1



## Alpha diversity
par(mfrow = c(1,2))
plot(rowSums(TimeXDepth[,,1]))

# sampling effort
sefort <- xtabs(~Sampling.date..YYYYMMDD.+Sampling.depth..cm.below.surface., SaPhyto_7513)

par(mfrow = c(5,2), mar = c(0,0,0,0))

for(i in 1:10){
  plot(log(rowSums(paMAT[,,i])),
       pch = ".", 
       xaxt = "n",
       yaxt = "n",
       xlab = "")
  
plot(residuals(lm(log1p(
  rowSums(paMAT[,,i]))~sefort[,i])),
     pch = ".", 
     ylim = c(-2,2),
     xaxt = "n",
     yaxt = "n",
     xlab = "")
abline(h =0)
  
}

heatmap(log1p(apply(paMAT, 3, rowSums)), 
        Colv = NA,Rowv = NA)







# Same code for other matrices 
# What environmental variables? 



# function to remove 0z
rmoveZer <- function(mat){
  mat <- mat[!rowSums(mat) ==0,!colSums(mat) ==0 ]
  return(as.data.frame.matrix(mat))
  }



## Community ordination 

par(mfrow = c(2,2), mar = c(0,0,0,0))

for(i in 1:4){
  
mat <- rmoveZer(paMAT[,,i])
mat <- mat[sample(1:nrow(mat),300),]

mds <- vegan::metaMDS(mat, 
                      distance = "jaccard")

plot(mds,
     xaxt = "n",
     yaxt = "n")

}



###



