
# Carregar as bibliotecas necessárias
library(dplyr)
library(ggplot2)

# Ler o arquivo CSV
df <- read.csv("C:/LabDadosR/cursos-prouni.csv")

# Filtrar os cursos de interesse
cursos_interesse <- df %>%
  filter(curso_busca %in% c("Ciência da Computação", "Engenharia da Computação", "Sistemas de Informação"))

# Calcular o total de bolsas para cada curso e modalidade
bolsas_por_modalidade <- cursos_interesse %>%
  group_by(curso_busca, turno) %>%
  summarise(
    total_bolsas = sum(bolsa_integral_cotas, na.rm = TRUE) + 
      sum(bolsa_integral_ampla, na.rm = TRUE) + 
      sum(bolsa_parcial_cotas, na.rm = TRUE) + 
      sum(bolsa_parcial_ampla, na.rm = TRUE)
  )

# Calcular o percentual de bolsas para cada modalidade por curso
percentual_bolsas <- bolsas_por_modalidade %>%
  group_by(curso_busca) %>%
  mutate(percentual = total_bolsas / sum(total_bolsas) * 100)

# Exibir os resultados
print("Percentual de bolsas ofertadas para cada modalidade por curso:")
print(percentual_bolsas)

# Criar o gráfico de barras
ggplot(percentual_bolsas, aes(x = turno, y = percentual, fill = curso_busca)) +
  geom_text(aes(label = round(percentual, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentual de Bolsas Ofertadas por Modalidade",
       x = "Modalidade",
       y = "Percentual de Bolsas (%)",
       fill = "Curso") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
