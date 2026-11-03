alpha <- 0.05
gl <- 14
var_hipotese <- 0.0025 # (0,050)^2

valor_critico <- qchisq(alpha, df = gl)

var_amostral <- var(diametros)
estatistica_obs <- (gl * var_amostral) / var_hipotese

sink("resultados_exercicio2e.txt")
cat(" RESULTADOS DO TESTE (ITEM E) \n")
cat("Valor Crítico (qchisq):", round(valor_critico, 4), "\n")
cat("Região Crítica (RC): Valores de X^2 <", round(valor_critico, 4), "\n")
cat("Estatística Observada:", round(estatistica_obs, 4), "\n")
cat("Pertence à Região Crítica?", ifelse(estatistica_obs < valor_critico, "SIM (Rejeita H0)", "NÃO (Não rejeita H0)"), "\n")
sink()

png("grafico_exercicio2e.png", width = 800, height = 800, res = 120)

x <- seq(0, 35, length.out = 1000)
y <- dchisq(x, df = gl)

plot(x, y, type = "l", lwd = 2, col = "black",
     xlab = expression(chi^2), ylab = "Densidade",
     main = expression("Distribuição " * chi[14]^2 * " sob " * H[0]))

x_rc <- seq(0, valor_critico, length.out = 100)
y_rc <- dchisq(x_rc, df = gl)
polygon(c(0, x_rc, valor_critico), c(0, y_rc, 0), col = "lightcoral", border = NA)

abline(v = valor_critico, col = "red", lty = 2, lwd = 2)
text(valor_critico, max(y)*0.4, paste("Critico =", round(valor_critico, 2)), col="red", pos=4)

abline(v = estatistica_obs, col = "blue", lty = 1, lwd = 2)
text(estatistica_obs, max(y)*0.6, paste("Obs =", round(estatistica_obs, 2)), col="blue", pos=4)

legend("topright", legend=c("Região Crítica (RC)", "Valor Crítico", "Valor Observado"),
       fill=c("lightcoral", NA, NA), col=c(NA, "red", "blue"),
       lty=c(NA, 2, 1), lwd=c(NA, 2, 2))

dev.off()

cat("Sucesso! Arquivos 'resultados_exercicio2e.txt' e 'grafico_exercicio2e.png' gerados na pasta.\n")