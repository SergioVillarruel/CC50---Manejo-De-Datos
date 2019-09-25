setwd("C:/Users/Jhon/Desktop/Nueva carpeta/admiinfo/Nueva carpeta")
NAMES <- read.table("EGG.csv", nrow = 1, stringsAsFactors = FALSE, sep = ";")
DATA <- read.table("EGG.csv", skip = 1, stringsAsFactors = FALSE, sep = ";")
DATA <- DATA[, 1:134]
names(DATA) <- NAMES 



DATA$CQ_mean= ifelse(is.na(DATA$CQ_mean), ave(DATA$CQ_mean, FUN = function(x) mean(x, na.rm = 'TRUE')), DATA$CQ_mean)


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



#Zscore reutilizable

Zscore <- function(x,media){
  desviacion = Desviacion(x,media)
  
  return <- (x-media)/desviacion
}
DATA$peak_Vel_mean=Zscore(DATA$peak_Vel_mean,median(DATA$peak_Vel_mean))
DATA$min_Vel_mean=Zscore(DATA$min_Vel_mean,median(DATA$min_Vel_mean))
DATA$min_Vel_Time_mean=Zscore(DATA$min_Vel_Time_mean,median(DATA$min_Vel_Time_mean))
DATA$peak_Vel_Time_mean=Zscore(DATA$peak_Vel_Time_mean,median(DATA$peak_Vel_Time_mean))
DATA$ratio_mean=Zscore(DATA$ratio_mean,median(DATA$ratio_mean))
DATA$CQ_mean=Zscore(DATA$CQ_mean,median(DATA$CQ_mean))
DATA$CQ_H_mean=Zscore(DATA$CQ_H_mean,median(DATA$CQ_H_mean))
DATA$CQ_PM_mean=Zscore(DATA$CQ_PM_mean,median(DATA$CQ_PM_mean))
DATA$CQ_HT_mean=Zscore(DATA$CQ_HT_mean,median(DATA$CQ_HT_mean))
DATA$`SQ2-SQ1_mean`=Zscore(DATA$`SQ2-SQ1_mean`,median(DATA$`SQ2-SQ1_mean`))

#normalizacion maxreutilizable

nMax <- function(x, max) {
  return <- (x/max)
}
DATA$peak_Vel_mean=nMax(DATA$peak_Vel_mean,max(DATA$peak_Vel_mean))
DATA$min_Vel_mean=nMax(DATA$min_Vel_mean,max(DATA$min_Vel_mean))
DATA$min_Vel_Time_mean=nMax(DATA$min_Vel_Time_mean,max(DATA$min_Vel_Time_mean))
DATA$peak_Vel_Time_mean=nMax(DATA$peak_Vel_Time_mean,max(DATA$peak_Vel_Time_mean))
DATA$ratio_mean=nMax(DATA$ratio_mean,max(DATA$ratio_mean))
DATA$CQ_mean=nMax(DATA$CQ_mean,max(DATA$CQ_mean))
DATA$CQ_H_mean=nMax(DATA$CQ_H_mean,max(DATA$CQ_H_mean))
DATA$CQ_PM_mean=nMax(DATA$CQ_PM_mean,max(DATA$CQ_PM_mean))
DATA$CQ_HT_mean=nMax(DATA$CQ_HT_mean,max(DATA$CQ_HT_mean))
DATA$`SQ2-SQ1_mean`=Zscore(DATA$`SQ2-SQ1_mean`,median(DATA$`SQ2-SQ1_mean`))

write.table(DATA, file = "DATAZscore.dat")
write.table(DATA, file = "DATAValormaximo.dat")
