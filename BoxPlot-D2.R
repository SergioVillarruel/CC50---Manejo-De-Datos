boxplot(dataset$NW_Title,
        main = "Distribución central del número de palabras por Titulo",
        xlab = "Número de palabras",
        col = "orange",
        border = "brown",
        notch = TRUE
)

boxplot(dataset$ND_Title,
        main = "Distribución central del número de digitos por Titulo",
        xlab = "Número de digitos",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

boxplot(dataset$NW_Plot,
        main = "Distribución central del número de palabras por Plot",
        xlab = "Número de palabras",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

boxplot(dataset$ND_Plot,
        main = "Distribución central del número de digitos por Plot",
        xlab = "Número de palabras",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)