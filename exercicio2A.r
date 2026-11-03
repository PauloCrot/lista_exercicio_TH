diametros <- c(8.01, 7.99, 8.03, 8.02, 7.98, 8.00, 8.04, 8.01,
               7.97, 8.02, 8.00, 7.99, 8.03, 8.01, 7.98)


library(car)
qqPlot(diametros, main = "Q-Q Plot dos Diâmetros")

par(mfrow = c(1, 2))

boxplot(diametros,
        main = "Boxplot dos Diâmetros",
        ylab = "Diâmetro (mm)",
        col = "lightblue")

qqPlot(diametros,
       simulate = TRUE,
       main = "Gráfico Q-Q Simulado",
       xlab = "Quantis Teóricos Normalizados",
       ylab = "Diâmetros (mm)",
       pch = 16)

par(mfrow = c(1, 1))