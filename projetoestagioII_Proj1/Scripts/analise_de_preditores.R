# Carregar os pacotes

library(psych)
library(ppcor)
library(car)

# Importar dados

info <- read.csv("C:/Users/jmrc2/Desktop/balançovidatrabalho.csv")

# Ver as correlações 

with(info, cor(balanço_vida_trabalho, população_trabalhar_49houmais)) # Correlação forte com as horas de trabalho
with(info, cor(balanço_vida_trabalho, indice_conciliação_trabalho)) # Correlação não tão forte mas não fraca com o indice de conciliação entre vida e trabalho

# Equação do modelo (Regressão Linear Multipla)

mod <- lm(formula = balanço_vida_trabalho ~ população_trabalhar_49houmais + indice_conciliação_trabalho, data = info)

# Ver os resultados 

summary(mod)

# Ver o vif para ver se há Ausência de multicolinearidade

vif(mod)

