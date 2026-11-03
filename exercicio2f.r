# exercicio2f.r - Cálculo do P-valor, Exportação do Gráfico e Resultados

gl <- 14
var_hipotese <- 0.0025 # (0,050)^2

var_amostral <- var(diametros)
estatistica_obs <- (gl * var_amostral) / var_hipotese

p_valor <- pchisq(estatistica_obs, df = gl)

sink("resultados_exercicio2f.txt")
cat(" RESULTADOS DO TESTE (ITEM F) \n")
cat("Estatística Observada (X^2_obs):", round(estatistica_obs, 4), "\n")
cat("P-valor (pchisq):", format.pval(p_valor, digits = 5), "\n")
cat("Nível de Significância (alpha): 0.05\n")
cat("Decisão:", ifelse(p_valor < 0.05, "Rejeita H0", "Não rejeita H0"), "\n")
sink()

png("grafico_exercicio2f.png", width = 800, height = 800, res = 120)

x <- seq(0, 35, length.out = 1000)
y <- dchisq(x, df = gl)

plot(x, y, type = "l", lwd = 2, col = "black",
     xlab = expression(chi^2), ylab = "Densidade",
     main = expression("Área do P-valor na Distribuição " * chi[14]^2))

x_pval <- seq(0, estatistica_obs, length.out = 100)
y_pval <- dchisq(x_pval, df = gl)
polygon(c(0, x_pval, estatistica_obs), c(0, y_pval, 0), col = "skyblue", border = NA)

abline(v = estatistica_obs, col = "blue", lty = 1, lwd = 2)
text(estatistica_obs, max(y)*0.5, paste("Obs =", round(estatistica_obs, 2)), col="blue", pos=4)

legend("topright", legend=c("Área do P-valor", "Estatística Observada"),
       fill=c("skyblue", NA), col=c(NA, "blue"),
       lty=c(NA, 1), lwd=c(NA, 2))

dev.off()

cat("Sucesso! Ficheiros 'resultados_exercicio2f.txt' e 'grafico_exercicio2f.png' foram gerados.\n")