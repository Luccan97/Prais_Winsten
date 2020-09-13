install.packages("prais")

library(readxl)
library(prais)

setwd('C:\\Users\\User\\Desktop\\projetos\\Analise_temporal\\mortalidade_juvenil')


Dados <- read.csv("base.csv", header = TRUE, sep = ";", dec = ",")

dataset_length <- length(names(Dados))

for (i in 2:dataset_length) {
  Dados[,i] <- log10(Dados[,i])
}

pw_resultados <- lapply(names(Dados)[-1], function(a){
  resp <- paste0("`", a, "`")
  fmla <- paste(resp, "Ano", sep = "~")
  fmla <- as.formula(fmla)
  pw <- prais_winsten(fmla, data = Dados)
  cf <- coef(summary(pw))[2, ]
  cbind.data.frame(coluna = a, t(cf))
})
pw_resultados <- do.call(rbind, pw_resultados)

head(pw_resultados)


library(dplyr)

bmin <- pw_resultados$Estimate - (1.96 *pw_resultados$`Std. Error`)
bmax <- pw_resultados$Estimate + (1.96 *pw_resultados$`Std. Error`)

ICmin <- (-1 + exp(bmin))*100
ICmax <- (-1 + exp(bmax))*100
ICmin <- format(round(ICmin, 2), nsmall = 2)
ICmax <- format(round(ICmax, 2), nsmall = 2)

pw_resultados <- mutate(pw_resultados, APC = (-1 + exp(Estimate))*100,  IC = paste("(", ICmax," / ", ICmin, ")"))

write.table(pw_resultados, file = "pw_resultados.csv", sep = "\t", na = "", quote = FALSE, dec = ",")

