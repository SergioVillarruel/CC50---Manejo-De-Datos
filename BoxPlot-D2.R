NW_Title <- dataset$NW_Title
ND_Title <- dataset$ND_Title
NW_Plot <- dataset$NW_Plot
ND_Plot <- dataset$ND_Plot

NW_Title_norm <- rnorm(200,mean=mean(NW_Title, na.rm=TRUE), sd=sd(NW_Title, na.rm=TRUE))
ND_Title_norm <- rnorm(200,mean=mean(ND_Title, na.rm=TRUE), sd=sd(ND_Title, na.rm=TRUE))
NW_Plot_norm <- rnorm(200,mean=mean(NW_Plot, na.rm=TRUE), sd=sd(NW_Plot, na.rm=TRUE))
ND_Plot_norm <- rnorm(200,mean=mean(ND_Plot, na.rm=TRUE), sd=sd(ND_Plot, na.rm=TRUE))

boxplot(NW_Title, NW_Title_norm,
        main = "Distribución central del número de palabras por Titulo",
        xlab = "Número de palabras",
        names = c("Sin variar","Normalizado"),
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

boxplot(ND_Title,ND_Title_norm,
        main = "Distribución central del número de digitos por Titulo",
        xlab = "Número de digitos",
        names = c("Sin variar","Normalizado"),
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

boxplot(NW_Plot,NW_Plot_norm,
        main = "Distribución central del número de palabras por Plot",
        xlab = "Número de palabras",
        names = c("Sin variar","Normalizado"),
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

boxplot(ND_Plot,ND_Plot_norm,
        main = "Distribución central del número de digitos por Plot",
        xlab = "Número de palabras",
        names = c("Sin variar","Normalizado"),
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)