
## Descriptive statistics in large datasets 

# Dimensions 
dim(SaPhyto_7513)
# ncol / nrow 
ncol(SaPhyto_7513)
nrow(SaPhyto_7513)
# summary
summary(SaPhyto_7513)

# data distribution (density)

par(mfrow = c(1,3))

plot(density(as.numeric(
  as.character(
    SaPhyto_7513$Sampling.depth..cm.below.surface.)
  )),
     frame = F, 
     main = expression('Sampling depth cm'^3),
  xlab = expression("Subscripts are also possible"["see?"]))

plot(density(SaPhyto_7513$Biovolume..cubic.micrometers.), 
     frame = F, 
     main = " Biovolume ")
plot(density(SaPhyto_7513$Cells..1.L.), 
     frame = F,
     main = "")




## Is my data normal? 

## shaphiro.test
shapiro.test(sample(SaPhyto_7513$Cells..1.L.,
                    5000))
## kolmogorov smirnoff test 

myDist <- rnorm(n = length(SaPhyto_7513$Cells..1.L.),
                mean(SaPhyto_7513$Cells..1.L.), 
                sd= sd(SaPhyto_7513$Cells..1.L.))

hist(myDist, 
     col = scales::alpha("red", 0.4))

hist(myDist*3, 
     col = scales::alpha("blue", 0.4), 
     add = T)

# perform the actual test
ks.test(SaPhyto_7513$Cells..1.L.,myDist)


## transforming data 

SaPhyto_7513$BioVol_log <- log(SaPhyto_7513$Biovolume..cubic.micrometers.)
SaPhyto_7513$Cells..1.L._log <- log1p(SaPhyto_7513$Cells..1.L.)


## have we achieved normality? 
shapiro.test(sample(SaPhyto_7513$BioVol_log,
                    5000))
shapiro.test(sample(SaPhyto_7513$Cells..1.L._log,
                    5000))

hist(SaPhyto_7513$BioVol_log) # bimodal

hist(SaPhyto_7513$Cells..1.L._log) ## zero inflated! 


### lets dig further 

# what is the relationship btw the distribution of biovolume with depth? 


boxplot(SaPhyto_7513$BioVol_log~SaPhyto_7513$Sampling.depth..cm.below.surface.)
boxplot(SaPhyto_7513$Cells..1.L._log~SaPhyto_7513$Sampling.depth..cm.below.surface.)

#anova on the differences 
summary(aov(SaPhyto_7513$BioVol_log~SaPhyto_7513$Sampling.depth..cm.below.surface.))
summary(aov(SaPhyto_7513$Cells..1.L._log~SaPhyto_7513$Sampling.depth..cm.below.surface.))

# what if we bin the data coarser? 


SaPhyto_7513$DepthBins <- cut(SaPhyto_7513$Sampling.depth..cm.below.surface., 4)


boxplot(SaPhyto_7513$BioVol_log~SaPhyto_7513$DepthBins) # same 
boxplot(SaPhyto_7513$Cells..1.L._log~SaPhyto_7513$DepthBins) # same 


anova_bins <- aov(SaPhyto_7513$BioVol_log~SaPhyto_7513$DepthBins)
# pairwise comparisons 

TukeyHSD(anova_bins)

## repeat with date

names(SaPhyto_7513)



### Cheching the sampling effort 

sefort <- xtabs(~Sampling.date..YYYYMMDD.+DepthBins, SaPhyto_7513)

hist(rowSums(sefort))

# can we correct for sampling effor? 

# create sampling probabilities 

prob <- rowSums(sefort)/max(rowSums(sefort))
range(prob)

length(prob)
length(SaPhyto_7513$Sampling.date..YYYYMMDD.)

newprob <- prob[match(as.character(SaPhyto_7513$Sampling.date..YYYYMMDD.), names(prob))]

plot(SaPhyto_7513$BioVol_log~newprob, pch = ".")
plot(SaPhyto_7513$Cells..1.L._log~newprob, pch = ".", 
     col = SaPhyto_7513$DepthBins)






