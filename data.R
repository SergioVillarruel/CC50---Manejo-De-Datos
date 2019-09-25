# dataset1 #######################################################################################################
setwd("C:/Users/Pedro/Downloads/AdminInfo-TA1")
NAMES <- read.table("EGG.csv", nrow = 1, stringsAsFactors = FALSE, sep = ";")
DATA <- read.table("EGG.csv", skip = 1, stringsAsFactors = FALSE, sep = ";")
DATA <- DATA[, 1:134]
names(DATA) <- NAMES 

DATA$Vowel=factor(DATA$Vowel, levels = c('a','e','i','o','u'),labels=c(1,2,3,4,5))
DATA$Vowel= ifelse(is.na(DATA$Vowel), ave(DATA$Vowel, FUN = function(x) mean(x, na.rm = 'TRUE')), DATA$Vowel)
DATA$Vowel = as.numeric(format(round(DATA$Vowel, 0)))
DATA$Vowel=factor(DATA$Vowel, levels = c(1,2,3,4,5),labels=c('a','e','i','o','u'))


DATA$`Dialect/Village`=factor(DATA$`Dialect/Village`, levels = c('v1','v2','w','SMZ','SJG','BLACK'),labels=c(1,2,3,4,5,6))
DATA$`Dialect/Village`= ifelse(is.na(DATA$`Dialect/Village`), ave(DATA$Vowel, FUN = function(x) mean(x, na.rm = 'TRUE')), DATA$Vowel)
DATA$`Dialect/Village` = as.numeric(format(round(DATA$`Dialect/Village`, 0)))





maxCol = max(database$citation_count_sum)
minCol = min(database$citation_count_sum)

library(ggplot2)



# Normalización Lineal

nLineal <- function(x, maxCol, minCol) {
  result <- (x - minCol) / (maxCol - minCol)
  return(result)
}

maxValue <- max(database$citation_count_sum, na.rm = TRUE)
minValue <- min(database$citation_count_sum, na.rm = TRUE)

database$citation_count_sum <- nLineal(database$citation_count_sum, maxValue, minValue)

# Normalización por desviación estandar

nEstandar <- function(x, med, desv) {
  result <- (x - med) / desv
  return(result)
}

med <- mean(DB1$paper_count_sum, na.rm = TRUE)
desv <- sd(DB1$paper_count_sum, na.rm = TRUE)

DB1$paper_count_sum <- nLineal(DB1$paper_count_sum, med, desv)

DB1$paper_count_sum

# Normalización por valor Máximo

nMax <- function(x, max) {
  return <- (x/max)
}

max <- max(DB1$avg_cites_per_paper, na.rm = TRUE)

DB1$avg_cites_per_paper <- nMax(DB1$avg_cites_per_paper, max)
DB1$avg_cites_per_paper


#Normalizacion Z-score
Desviacion<-function(x,media){
  return <- sqrt(sum((x-media)^2)/length(x))
}

Zscore <- function(x,media){
  desviacion = Desviacion(x,media)
  
  return <- (x-media)/desviacion
}
#Comprobar datos numeros en DB1
numericos=c()
for (i in 1:length(DB1[1,])){
  if (is.numeric(DB1[1,i])){
    numericos=c(numericos,i)
  }
}


# 1
dist(DATA$peak_Vel_mean, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$peak_Vel_mean, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$peak_Vel_mean, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 2
dist(DATA$min_vel_mean, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$min_vel_mean, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$min_vel_mean, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 3
dist(DATA$min_vel_time_mean, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$min_vel_time_mean, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$min_vel_time_mean, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 4
dist(DATA$peak_vel_time_mean, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$peak_vel_time_mean, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$peak_vel_time_mean, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 5
dist(DATA$ratio_mean, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$ratio_mean, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$ratio_mean, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 6
dist(DATA$CQ_mean, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$CQ_mean, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$CQ_mean, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 7
dist(DATA$CQ_H_mean, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$CQ_H_mean, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$CQ_H_mean, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 8
dist(DATA$CQ_PM_mean, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$CQ_PM_mean, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$CQ_PM_mean, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 9
dist(DATA$CQ_HT_mean, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$CQ_HT_mean, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$CQ_HT_mean, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 10
dist(DATA$sq2_sq1_mean, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$sq2_sq1_mean, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$sq2_sq1_mean, method="minkowski", diag=TRUE, upper=FALSE, p=4)







# dataset2 ####################################################################################################

NAMES <- read.table("dataset2.csv", nrow = 1, stringsAsFactors = FALSE, sep = ",")
DATA <- read.table("dataset2.csv", skip = 1, stringsAsFactors = FALSE, sep = ",")
DATA <- DATA[, 1:5]
names(DATA) <- NAMES 

# 1
dist(DATA$NW_Plot, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$NW_Plot, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$NW_Plot, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 2
dist(DATA$NW_Title, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$NW_Title, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$NW_Title, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 3
dist(DATA$ND_Title, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$ND_Title, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$ND_Title, method="minkowski", diag=TRUE, upper=FALSE, p=4)

# 4
dist(DATA$ND_Plot, method="euclidean", diag=TRUE, upper=FALSE, p=2)
dist(DATA$ND_Plot, method="manhattan", diag=TRUE, upper=FALSE)
dist(DATA$ND_Plot, method="minkowski", diag=TRUE, upper=FALSE, p=4)
