
alpha <- 0.05
gl <- 14
var0 <- 0.050^2 # Variância sob H0
var1 <- 0.030^2 # Variância verdadeira sob H1

valor_critico <- qchisq(alpha, df = gl)


valor_critico_alt <- valor_critico * (var0 / var1)
beta <- 1 - pchisq(valor_critico_alt, df = gl)

poder <- 1 - beta

sink("resultados_exercicio2g.txt")
cat(" Código R \n")
cat("var0 <- 0.050^2\nvar1 <- 0.030^2\n")
cat("valor_critico <- qchisq(0.05, df = 14)\n")
cat("beta <- 1 - pchisq(valor_critico * (var0 / var1), df = 14)\n")
cat("poder <- 1 - beta\n\n")
cat(" Output R \n")
cat("Probabilidade de Erro Tipo II (\u03b2):", round(beta, 4), "\n")
cat("Poder do Teste (1 - \u03b2):", round(poder, 4), "\n")
sink()

png("grafico_exercicio2g.png", width = 800, height = 500, res = 120)

x <- seq(0, 35, length.out = 1000)
y_h0 <- dchisq(x, df = gl)
k <- var1 / var0
y_h1 <- (1/k) * dchisq(x/k, df = gl)

plot(x, y_h0, type = "l", lwd = 2, col = "black", ylim = c(0, max(y_h1)*1.1),
     xlab = "Estatística de teste", ylab = "Densidade",
     main = "Distribuições sob H0 e sob a alternativa (\u03c3 = 0,030 mm)")
lines(x, y_h1, lwd = 2, col = "gray40", lty = 2)

x_beta <- seq(valor_critico, max(x), length.out = 500)
y_beta <- (1/k) * dchisq(x_beta/k, df = gl)
polygon(c(valor_critico, x_beta, max(x)), c(0, y_beta, 0), col = rgb(1, 0, 0, 0.3), border = NA)

abline(v = valor_critico, col = "red", lty = 1, lwd = 2)
text(valor_critico, max(y_h1)*0.8, paste("Crítico =", round(valor_critico, 2)), col="red", pos=4)

legend("topright", legend=c("Sob H0: \u03c3 = 0,050 mm", "Sob H1: \u03c3 = 0,030 mm", "\u03b2 (Erro Tipo II)"),
       col=c("black", "gray40", NA), lty=c(1, 2, NA), lwd=c(2, 2, NA),
       fill=c(NA, NA, rgb(1, 0, 0, 0.3)), border=NA)

dev.off()

cat("Sucesso! 'resultados_exercicio2g.txt' e 'grafico_exercicio2g.png' gerados.\n")