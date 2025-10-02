
library(ggplot2)
library(dplyr)
library(readxl)
library(magrittr)
library(tidyr)
library(tidyverse)
library(janitor)
install.packages("gt")  
library(gt)

# dataset
banco_dados <- read_excel("analise_r/dados_ecommerce.xlsx")

#---------------------------------------------------

# Contagem e cálculo das porcentagens - Modo de envio
banco_dados_summary <- banco_dados %>%
  count(Modo_de_envio) %>%
  mutate(Percentual = n / sum(n) * 100)

# Criar o gráfico de rosca - Modo de envio
ggplot(banco_dados_summary, aes(x = "", y = Percentual, fill = Modo_de_envio)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +  
  geom_text(aes(label = paste0(round(Percentual, 1), "%")),position = position_stack(vjust = 0.5)) +
  labs(title = "Modo de Envio da compra") +
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +  
  scale_fill_manual(values = c("Aeroviario" = "#cccccc", "Rodoviario" = "#783f04","Aquaviario" = "#3d85c6"))


#------------------------------------------------------

# Contagem das avaliações
banco_dados_summary <- banco_dados %>%
  count(Avaliacao) %>%
  mutate(Avaliacao = as.factor(Avaliacao))  

# Criar o gráfico de colunas - Avaliação
ggplot(banco_dados_summary, aes(x = Avaliacao, y = n, fill = Avaliacao)) +
  geom_col() +
  geom_text(aes(label = n), vjust = 1.5) +
  labs(title = "Avaliações dos cliente (1 a 5)", x = "Avaliação (1-5)", y = "Número de avaliações", fill = "Avaliação") +
  theme_minimal() +
  scale_fill_manual(values = c("#cc0000", "#e06666", "#ffd966", "#6aa84f" ,"#34a853")) +
  theme(plot.title = element_text(hjust = 0.5))
  
media_avaliacao <- mean(banco_dados$Avaliacao, na.rm = TRUE)
mediana_avaliacao <- median(banco_dados$Avaliacao, na.rm = TRUE)
moda_avaliacao <- as.numeric(names(sort(table(banco_dados$Avaliacao), decreasing = TRUE)[1]))

media_avaliacao
mediana_avaliacao
moda_avaliacao
#----------------------------------------------------------------

# Cálculo das porcentagens - Chegou a tempo
banco_dados_summary <- banco_dados %>%
  count(Chegou_a_Tempo) %>%
  mutate(Percentual = n / sum(n) * 100)

# Criar o gráfico de colunas - Chegou a tempo
ggplot(banco_dados_summary, aes(x = Chegou_a_Tempo, y = Percentual, fill = Chegou_a_Tempo)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Percentual, 2), "%")), vjust = 1.5) +
  labs(title = "Entrega chegou a tempo?", x = "Sim ou não?", y = "Porcentagem das entregas totais",  fill = "Chegou a tempo?") +
  theme_minimal() +
  scale_fill_manual(values = c("#cc0000", "#34a853")) +
  theme(plot.title = element_text(hjust = 0.5))

#-----------------------------------------------------------------

# Contagem dos atendimentos
banco_dados_summary <- banco_dados %>%
  count(Chamadas_de_atendimento) %>%
  mutate(Chamadas_de_atendimento = as.factor(Chamadas_de_atendimento))  

# Criar o gráfico de colunas - Chamadas de atendimento
ggplot(banco_dados_summary, aes(x = Chamadas_de_atendimento, y = n, fill = Chamadas_de_atendimento)) +
  geom_col() +
  geom_text(aes(label = n), vjust = 1.5) +
  labs(title = "Chamadas de atendimento feitas pelos clientes", x = "Quantidade de chamadas realizadas pelo cliente", y = "Frequência das quantidades de chamadas", fill = "Chamadas de atendimento") +
  theme_minimal() +
  scale_fill_manual(values = c("#ffcccc", "#ff9999", "#ff6666", "#ff4d4d", "#e60000", "#cc0000")) +
  theme(plot.title = element_text(hjust = 0.0))

media_chamadas <- mean(banco_dados$Chamadas_de_atendimento, na.rm = TRUE)
mediana_chamadas <- median(banco_dados$Chamadas_de_atendimento, na.rm = TRUE)
moda_chamadas <- as.numeric(names(sort(table(banco_dados$Chamadas_de_atendimento), decreasing = TRUE)[1]))
media_chamadas
mediana_chamadas
moda_chamadas

#----------------------------------------------------------------------

# Dispersão do Valor das compras
ggplot(banco_dados, aes(x = Valor_da_compra, y = ..count..)) +
  geom_point(aes(y = after_stat(count)), stat = "count") +
  labs(title = "Dispersão do Valor das Compras realizadas", x = "Valor da Compra, em dólares", y = "Frequência de pedidos com o valor x") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

amplitude_total <- max(banco_dados$Valor_da_compra, na.rm = TRUE) - min(banco_dados$Valor_da_compra, na.rm = TRUE)
media_valor_compra <- mean(banco_dados$Valor_da_compra, na.rm = TRUE)
mediana_valor_compra <- median(banco_dados$Valor_da_compra, na.rm = TRUE)
moda_valor_compra <- as.numeric(names(sort(table(banco_dados$Valor_da_compra), decreasing = TRUE)[1]))

amplitude_total
media_valor_compra
mediana_valor_compra
moda_valor_compra

#-----------------------------------------------------------------------

#Bivariada Chegou_a_tempo X Avaliacao

# Contagem das avaliações por Chegou_a_Tempo
banco_dados_summary <- banco_dados %>%
  count(Chegou_a_Tempo, Avaliacao) %>%
  mutate(Chegou_a_Tempo = as.factor(Chegou_a_Tempo),
         Avaliacao = as.factor(Avaliacao))

# Gráfico de colunas 
ggplot(banco_dados_summary, aes(x = Avaliacao, y = n, fill = Chegou_a_Tempo)) +
  geom_col(position = "dodge") +  # Agrupa as colunas
  labs(title = "Distribuição das Avaliações por tempo de entrega", 
       x = "Avaliação (1-5)", 
       y = "Número de Avaliações",
       fill = "Chegou a Tempo") +
  theme_minimal() +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = 1.5) +  
  theme(plot.title = element_text(hjust = 0.5))  


#---------------------------------------------------

# Tabela bivariada de Avaliações e Chamadas de Atendimento

tabela_bivariada <- banco_dados %>%
  count(Avaliacao, Chamadas_de_atendimento) %>%
  pivot_wider(names_from = Chamadas_de_atendimento, values_from = n, values_fill = 0) %>%
  rename(`Avaliação (1-5)` = Avaliacao) %>%
  adorn_totals("row") %>%  # Adiciona total por linha
  adorn_totals("col")      # Adiciona total por coluna


# Tabela formatada 
tabela_formatada <- tabela_bivariada %>%
  gt() %>%
  tab_header(
    title = "Distribuição das Avaliações por número de Chamadas de Atendimento"
  ) %>%
  tab_spanner(
    label = "Chamadas de Atendimento",  
    columns = c(-`Avaliação (1-5)`)
  ) %>%
  cols_label(`Avaliação (1-5)` = "Avaliação (1-5)") %>%
  cols_align(
    align = "center",
    columns = everything()
  ) 

tabela_formatada

# Os gráficos presentes nesse código foram salvos como png a partir da funcionalidade export do r.studio e anexados ao relatório


