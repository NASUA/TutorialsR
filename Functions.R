## Function to aggregate data by its means or sd. 

Fun2Sum <- function(datos, operacion = c("mean", "sd")){
  
  
  if(operacion == "mean"){
    agg <- aggregate(log(datos$Biovolume..cubic.micrometers.),
                     by = list(datos$taxa,
                               datos$Sampling.depth..cm.below.surface.),
                     mean)
    
    names(agg) <- c("taxa", "Depth", "Biovolume")
    
    
  }
  if(operacion == "sd"){
    agg <- aggregate(log(datos$Biovolume..cubic.micrometers.),
                     by = list(datos$taxa,datos$Sampling.depth..cm.below.surface.),
                     sd)
    
    names(agg) <- c("taxa", "Depth", "Biovolume")
    
  }else(print("error"))
  
  boxplot(agg$Biovolume~agg$Depth + agg$taxa)
  return(agg)
  
  
}
