
# Carregar a biblioteca necessária
library(dplyr)
library(ggplot2)

# Ler o arquivo CSV
df <- read.csv("C:/LabDadosR/cursos-prouni.csv")

# Calcular o total de bolsas para cada curso
df$total_bolsas <- rowSums(df[, c("bolsa_integral_cotas", "bolsa_integral_ampla", "bolsa_parcial_cotas", "bolsa_parcial_ampla")], na.rm = TRUE)



grouped <- df %>%
  group_by(curso_busca) %>%
  summarise(mensalidade_total = sum(mensalidade, na.rm = TRUE),
            total_bolsas = sum(total_bolsas, na.rm = TRUE))

# Calcular a mensalidade por bolsa
grouped <- grouped %>%
  mutate(mensalidade_por_bolsa = mensalidade_total / total_bolsas)

# Selecionar os top 10 cursos com maior valor de mensalidade por bolsa
top_10_cursos <- grouped %>%
  arrange(desc(mensalidade_por_bolsa)) %>%
  head(10)

# Exibir os top 10 cursos
print("Top 10 cursos com maior valor de mensalidade pelo total de bolsas:")
print(top_10_cursos)

# Calcular a correlação entre o valor total de mensalidade e o total de bolsas
correlacao <- cor(grouped$mensalidade_total, grouped$total_bolsas, use = "complete.obs")

# Exibir a correlação
print("Correlação entre o valor total de mensalidade e o total de bolsas por curso:")
print(correlacao)



# Criar o gráfico de barras
ggplot(top_10_cursos, aes(x = reorder(curso_busca, -mensalidade_por_bolsa), y = mensalidade_por_bolsa)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(mensalidade_por_bolsa, 2)), vjust = -0.5, color = "white") +
  geom_text(aes(label = round(total_bolsas, 2)), vjust = -0.5, color = "black") +
  labs(title = "Top 10 Cursos com Maior Valor de Mensalidade por Bolsa",
       x = "Curso",
       y = "Mensalidade por Bolsa (R$)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






