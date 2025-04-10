######################### ANALISE ESTATISTICA DESCRITIVA #########################

# Import das librarys

library(dplyr)
library(ggplot2)
library(tidyr)

# Carregar os dados

dados <- read.csv("C:/Users/Utilizador/Desktop/dadosai.csv", sep = ";")

## Tratamento de dados 

# Separar a coluna "Tarefas" em várias colunas

dados <- dados %>% separate(Tarefas, into = paste0("Tarefa", 1:5), sep = ",", fill ="right", extra = "drop")

# O mesmo para a coluna das "Motivações"

dados <- dados %>% seperate(Motivações, into = paste0("Motivacao", 1:5), sep = ",", fill = "right", extra = "drop")

# Remover as linhas com valores vazios (NAs)

dados_filtrados <- dados %>% filter(!is.na(Tarefa1) & Tarefa1 != "") # fiz isto para as tarefas porque foi onde tive problemas

## Análise de dados

# Summary dos resultados das idades obtidas

summary(dados$Idade)

# Criação de tabelas de frequência para cada coluna

freq_curso <- table(dados$Curso)
freq_grau <- table(dados$Grau_de_escolaridade)
freq_ai_user <- table(dados$AI_User)
freq_frequencia <- table(dados$Frequencia_de_uso)
freq_tarefas <- table(unlist(dados_filtrados[, grep("Tarefa", names(dados_filtrados))]))
freq_motivacoes <- table(unlist(dados_filtrados[, grep("Motivacao", names(dados_filtrados))]))
freq_confianca <- table(dados$Confiança_na_AI)

# Convertemos as tabelas de frequência em dataframes e calculamos as Percentagens do que os participantes votaram

freq_curso_df <- as.data.frame(freq_curso) %>% mutate(Percentagem = Freq / sum(Freq) * 100)
freq_grau_df <- as.data.frame(freq_grau) %>% mutate(Percentagem = Freq / sum(Freq) * 100)
freq_ai_user_df <- as.data.frame(freq_ai_user) %>% mutate(Percentagem = Freq / sum(Freq) * 100)
freq_frequencia_df <- as.data.frame(freq_frequencia) %>% mutate(Percentagem = Freq / sum(Freq) * 100)
freq_tarefas_df <- as.data.frame(freq_tarefas) %>% mutate(Percentagem = Freq / sum(Freq) * 100)
freq_motivacoes_df <- as.data.frame(freq_motivacoes) %>% mutate(Percentagem = Freq / sum(Freq) * 100)
freq_confianca_df <- as.data.frame(freq_confianca) %>% mutate(Percentagem = Freq / sum(Freq) * 100)

# Print das Tabelas

print(freq_curso_df)
print(freq_grau_df)
print(freq_ai_user_df)
print(freq_frequencia_df)
print(freq_tarefas_df)
print(freq_motivacoes_df)
print(freq_confianca_df)

# Tabela de contingência para ver como a utilização de AI e a confiança na AI se relacionam

tabela_contingencia <- table(dados$AI_User, dados$Confiança_na_AI)
print(tabela_contingencia)

## Gráficos

# Renomear as variáveis para termos as labels em vez dos números

freq_frequencia_df$Var1 <- factor(freq_frequencia_df$Var1, levels = c("1", "2", "3", "4", "5", "6"), labels = c("Diariamente", "3+ vezes/semana", "1-2 vezes/semana", "1-2 vezes/mês", "Quase todos os meses", "1-2 vezes/ano"))

nomes_tarefas <- c("1" = "Correção e avaliação", "2" = "Resumos e explicações", "3" = "Apoio à escrita", "4" = "Pesquisa", "5" = "Criação", "6" = "Planeamento", "7" = "Análise", "8" = "Chatbots", "9" = "Automatização"))
freq_tarefas_df$Var1 <- nomes_tarefas[as.character(freq_tarefas_df$Var1)]

nomes_motivacoes <- c("1" = "Curiosidade e vontade de aprender", "2" = "Aprimoramento de Competências", "3" = "Facilidade na aprendizagem", "4" = "Desafio intelectual","5" = "Autonomia no estudo", "6" = "Criatividade e inovação", "7" = "Explorar de novas formas de conhecimento", "8" = "Satisfação pessoal ao resolver problemas", "9" = "Gosto pela tecnologia", "10" = "Experiência prática para o futuro")
freq_motivacoes_df$Var1 <- nomes_motivacoes[as.character(freq_motivacoes_df$Var1)]

tabela_contingencia_df <- as.data.frame(tabela_contingencia) 
colnames(tabela_contingencia_df) <- c("AI_User", "Confianca", "Frequencia")

# Gráfico de Barras para as Tarefas mais votadas

ggplot(freq_tarefas_df, aes(x = Var1, y = Percentagem, fill = Var1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentagem, 1), "%")), vjust = -0.3, size = 3.5) +
  labs(title = "Frequência das Tarefas Mais Votadas", x = "Tarefas", y = "Percentagem (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de Barras para as Motivações mais votadas

ggplot(freq_motivacoes_df, aes(x = reorder(Var1, Percentagem), y = Percentagem)) +
  geom_col(fill = "red") +
  geom_text(aes(label = paste0(round(Percentagem, 1), "%")), vjust = 0.2, size = 3.5) +
  labs(title = "Motivações para Uso de IA", x = "Motivação", y = "Percentagem") +
  coord_flip() +
  theme_minimal()

# Gráfico de Barras para a Frequência de utilização de AI

ggplot(freq_frequencia_df, aes(x = reorder(Var1, Percentagem), y = Percentagem)) +
  geom_col(fill = "red") +
  geom_text(aes(label = paste0(round(Percentagem, 1), "%")), vjust = 0.2, size = 3.5) +
  labs(title = "Frequência de Uso de IA", x = "Frequência", y = "Percentagem") +
  coord_flip() +
  theme_minimal()

# Gráfico Heatmap para o grau de confiança e utilização de AI

ggplot(tabela_contingencia_df, aes(x = Confiança, y = as.factor(AI_User), fill = Frequencia)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confiança na IA vs Uso da IA", x = "Nível de Confiança (de 1 a 5)", y = "Utiliza IA (1 = Sim, 2 = Não)") +
  theme_minimal()

######################### ANALISE ESTATISTICA NÃO-DESCRITIVA #########################

# Teste de diferença entre a utilização de AI e a confiança na AI

t_testdif <- t.test(Confiança_na_AI ~ AI_User, data = dados)
print(t_testdif)