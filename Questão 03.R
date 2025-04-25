
# Carregar as bibliotecas necessárias
library(dplyr)
library(ggplot2)

# Ler o arquivo CSV
df <- read.csv("C:/LabDadosR/cursos-prouni.csv")

# Filtrar os cursos tecnológicos do estado da Paraíba
cursos_tecnologicos_pb <- df %>%
  filter(grau == "Tecnológico" & uf_busca == "PB")

# Calcular o total de bolsas para cada curso
cursos_tecnologicos_pb <- cursos_tecnologicos_pb %>%
  mutate(total_bolsas = rowSums(select(., bolsa_integral_cotas, bolsa_integral_ampla, bolsa_parcial_cotas, bolsa_parcial_ampla), na.rm = TRUE))

# Agrupar por curso e calcular o total de bolsas de cada tipo
bolsas_por_curso <- cursos_tecnologicos_pb %>%
  group_by(curso_busca) %>%
  summarise(
    total_bolsas = sum(total_bolsas, na.rm = TRUE),
    bolsa_integral_cotas = sum(bolsa_integral_cotas, na.rm = TRUE),
    bolsa_integral_ampla = sum(bolsa_integral_ampla, na.rm = TRUE),
    bolsa_parcial_cotas = sum(bolsa_parcial_cotas, na.rm = TRUE),
    bolsa_parcial_ampla = sum(bolsa_parcial_ampla, na.rm = TRUE)
  ) %>%
  arrange(desc(total_bolsas))


# Criar o gráfico de barras
ggplot(bolsas_por_curso, aes(x = reorder(curso_busca, -total_bolsas), y = total_bolsas)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = total_bolsas), vjust = -0.5, color = "white") +
  geom_text(aes(label = round(total_bolsas, 2)), vjust = -0.5, color = "black") +
  labs(title = "Cursos Tecnológicos da Paraíba com Mais Bolsas de Estudo",
       x = "Curso",
       y = "Total de Bolsas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

