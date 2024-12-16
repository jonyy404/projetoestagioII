# Carregar pacotes

library(ggplot2)
library(dplyr)
library(factoextra)
library(ape)
library(purrr)
library(psych)
library(ppcor)
library(car)
library(QuantPsyc)
library(pscl)
library(caret)

# Importar dados 

dados <- read.csv("C:/Users/jmrc2/Desktop/indicebemestar.csv", skip = 6, header = TRUE, stringsAsFactors = FALSE)

# Remover linhas e colunas com valores NA ou vazios

dados <- dados[rowSums(is.na(dados) | dados == "") != ncol(dados), ] 
dados <- dados[, colSums(is.na(dados) | dados == "") != nrow(dados)]

# Exibir a estrutura e as primeiras linhas para verificar os dados

str(dados)
head(dados)

# Selecionar apenas os dados relevantes (valores numéricos)

dados_selecionados <- dados[2:24, ] # Sao as linhas que incluem os valores numericos do ano 2004 a 2022

# Converter a coluna 'Ano' para valores numéricos

anos <- as.numeric(dados$Ano)  

# O mesmo para os Indices
 
indice_global <- as.numeric(dados$`Índice.global.de.bem.estar`)
indice_mat <- as.numeric(dados$`Índice.condições.materiais.de.vida`)
indice_qua <- as.numeric(dados$`Índice.qualidade.de.vida`)

# E para os Indicadores 

bemestar_eco <- as.numeric(dados$`X`)
vuln_eco <- as.numeric(dados$`X.1`)
emprego <- as.numeric(dados$`X.2`)

saude <- as.numeric(dados$`X.3`)
balançovidatrab <- as.numeric(dados$`X.4`)
educacao <- as.numeric(dados$`X.5`)
relacoessociais <- as.numeric(dados$`X.6`)
part_civica <- as.numeric(dados$`X.7`)
segurança <-as.numeric(dados$`X.8`)
ambiente <- as.numeric(dados$`X.9`)

# Tabelas 

# Tabelas com os indices

dados_ano_indicetotal <- na.omit(data.frame(Ano = anos, ÍndiceGlobal = indice_global)) # So apresenta o ano e o indice de bem estar respetivo
dados_ano_indicemat <- na.omit(data.frame(Ano = anos, ÍndiceMateriais = indice_mat)) # So apresenta o ano e o indice de condições materiais respetivo
dados_ano_indicequa <- na.omit(data.frame(Ano = anos, ÍndiceQualidadeVida = indice_qua)) # So apresenta o ano e o indice de qualidade de vida respetivo

# Tabela com o ano e os 3 indices

dados_ano_indices <- na.omit(data.frame(Ano = anos, 
                                       ÍndiceGlobal = indice_global, 
                                       ÍndiceCondicoes = indice_mat, 
                                       ÍndiceQualidade = indice_qua)) 

# Tabelas com o ano e o respetivo indicador

dados_ano_bemestareco <- na.omit(data.frame(Ano = anos, Bem_Estar_Economico = bemestar_eco ))
dados_ano_vuln_eco <- na.omit(data.frame(Ano = anos, Vulnerabidade_Economica = vuln_eco))
dados_ano_emprego <- na.omit(data.frame(Ano = anos, Emprego = emprego))

dados_ano_saude <- na.omit(data.frame(Ano = anos, Saúde = saude))
dados_ano_balançovidatrab <- na.omit(data.frame(Ano = anos, Balanço_Vida_Trab = balançovidatrab))
dados_ano_edu <- na.omit(data.frame(Ano = anos, Educação = educacao))
dados_ano_rs <- na.omit(data.frame(Ano = anos, Relações_Sociais = relacoessociais))
dados_ano_pc <- na.omit(data.frame(Ano = anos, Participação_Civica = part_civica))
dados_ano_seg <- na.omit(data.frame(Ano = anos, Segurança = segurança))
dados_ano_amb <- na.omit(data.frame(Ano = anos, Ambiente = ambiente))

# Tabela com o ano e os indicadores do indice de condições materiais

dados_ano_indic_cm <- na.omit(data.frame(Ano = anos, 
                                        Bem_Estar_Economico = bemestar_eco, 
                                        Vulnerabidade_Economica = vuln_eco, 
                                        Emprego = emprego))

# Tabela com o ano e os indicadores do indice de qualidade de vida

dados_ano_indic_qv <- na.omit(data.frame(Ano = anos, 
                                        Saúde = saude,
                                        Balanço_Vida_Trab = balançovidatrab,
                                        Educação = educacao,
                                        Relações_Sociais = relacoessociais, 
                                        Participação_Civica = part_civica,
                                        Segurança = segurança,
                                        Ambiente = ambiente))

# Prints

print(dados_ano_indicetotal)
print(dados_ano_indicemat)
print(dados_ano_indicequa)
print(dados_ano_indices)
print(dados_ano_bemestareco)
print(dados_ano_vuln_eco)
print(dados_ano_emprego)
print(dados_ano_saude)
print(dados_ano_balançovidatrab)
print(dados_ano_edu)
print(dados_ano_rs)
print(dados_ano_pc)
print(dados_ano_seg)
print(dados_ano_amb)
print(dados_ano_indic_cm)
print(dados_ano_indic_qv)

# Graficos 

# Grafico do Ano com a evolução do Indice bem-estar total

ggplot(dados_ano_indicetotal, aes(x = Ano, y = ÍndiceGlobal)) +
  geom_line(color = "blue") + 
  geom_point(color = "red") +
  labs(title = "Evolução do Índice Global de Bem-Estar",
       x = "Ano",
       y = "Índice Global de Bem-Estar") +
  theme_minimal()

# Grafico do Ano com a evolução dos Indicadores do indice de Condições de Materiais

ggplot() +
  geom_line(data = dados_ano_indic_cm, aes(x = Ano, y = Bem_Estar_Economico, color = "Bem-estar Econômico")) +
  geom_line(data = dados_ano_indic_cm, aes(x = Ano, y = Vulnerabidade_Economica, color = "Vulnerabilidade Econômica")) +
  geom_line(data = dados_ano_indic_cm, aes(x = Ano, y = Emprego, color = "Emprego")) +
  labs(title = "Evolução dos Indicadores do Índice de Condições Materiais",
       x = "Ano",
       y = "Valor do Indicador") +
  scale_color_manual(values = c("blue", "green", "red")) +
  theme_minimal() 

# Grafico do Ano com a evolução dos Indicadores do indice de Qualidade de Vida

ggplot() +
  geom_line(data = dados_ano_indic_qv, aes(x = Ano, y = Saúde, color = "Saúde")) +
  geom_line(data = dados_ano_indic_qv, aes(x = Ano, y = Balanço_Vida_Trab, color = "Balanço Vida-Trabalho")) +
  geom_line(data = dados_ano_indic_qv, aes(x = Ano, y = Educação, color = "Educação")) +
  geom_line(data = dados_ano_indic_qv, aes(x = Ano, y = Relações_Sociais, color = "Relações Sociais")) +
  geom_line(data = dados_ano_indic_qv, aes(x = Ano, y = Participação_Civica, color = "Participação Cívica")) +
  geom_line(data = dados_ano_indic_qv, aes(x = Ano, y = Segurança, color = "Segurança")) +
  geom_line(data = dados_ano_indic_qv, aes(x = Ano, y = Ambiente, color = "Ambiente")) +
  labs(title = "Evolução dos Indicadores do Índice de Qualidade de Vida",
       x = "Ano",
       y = "Valor do Indicador") +
  scale_color_manual(values = c("purple", "orange", "cyan", "magenta", "brown", "red", "green", "blue")) +
  theme_minimal() 

# Grafico do Ano com a evolução do Indice Global de Bem-Estar com o Balanço Vida-Trabalho

ggplot() +
  geom_line(data = dados_ano_indicetotal, aes(x = Ano, y = ÍndiceGlobal, color = "Índice Global")) +  
  geom_line(data = dados_ano_indic_qv, aes(x = Ano, y = Balanço_Vida_Trab, color = "Balanço Vida-Trabalho")) +  
  labs(title = "Comparação do Balanço Vida-Trabalho com o Indice Global Bem-Estar ao Longo dos Anos",
       x = "Ano",
       y = "Valor do Indicador") +
  scale_color_manual(values = c("Índice Global" = "black", "Balanço Vida-Trabalho" = "red")) +  
  theme_minimal() +
  theme(legend.title = element_blank()) 