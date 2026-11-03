# exercicio2b.r - Tabela de Testes de Normalidade (Significância de 5%)

if (!require(nortest)) install.packages("nortest")
library(nortest)

teste_sw  <- shapiro.test(diametros)
teste_ad  <- ad.test(diametros)
teste_lil <- lillie.test(diametros)
teste_cvm <- cvm.test(diametros)
teste_sf  <- sf.test(diametros)

tabela_norm <- data.frame(
  Teste = c("Shapiro–Wilk", "Anderson–Darling", "Lilliefors", "Cramér–von Mises", "Shapiro–Francia"),
  Estatistica = round(c(teste_sw$statistic, teste_ad$statistic, teste_lil$statistic, teste_cvm$statistic, teste_sf$statistic), 4),
  P_valor = round(c(teste_sw$p.value, teste_ad$p.value, teste_lil$p.value, teste_cvm$p.value, teste_sf$p.value), 4)
)

tabela_norm$Decisao <- ifelse(tabela_norm$P_valor > 0.05, 
                              "Não rejeita H0 (Normal)", 
                              "Rejeita H0 (Não Normal)")

print(tabela_norm)