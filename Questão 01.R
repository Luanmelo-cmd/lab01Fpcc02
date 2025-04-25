library(readr)
library(dplyr)
library(ggplot2)

dff<- read_csv("C:/LabDadosR/cursos-prouni.csv")

dp <- dff %>% 
  
  #Filtra Curso de ciência da computação
  filter(curso_busca=="Ciência da Computação")
  
  
  # Filtrar por regiões Nordeste e Sudeste
  cursos_cc_ne <- dp %>% filter(uf_busca %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"))
  
  cursos_cc_se <- dp %>% filter(uf_busca %in% c("ES", "MG", "RJ", "SP"))

  
  

# Calcular médias e medianas das mensalidades
media_ne <- mean(cursos_cc_ne$mensalidade)
mediana_ne <- median(cursos_cc_ne$mensalidade)
media_se <- mean(cursos_cc_se$mensalidade)
mediana_se <- median(cursos_cc_se$mensalidade)




# Teste t para comparar as médias
teste_t <- t.test(cursos_cc_ne$mensalidade, cursos_cc_se$mensalidade, na.rm = TRUE)

# Exibir resultado do teste t
print(teste_t)


# Calcular a diferença entre médias e medianas
dif_media <- media_se - media_ne
dif_mediana <- mediana_se - mediana_ne


resultados <- data.frame(
  Regiao = c("Nordeste", "Nordeste", "Sudeste", "Sudeste"),
  Tipo = c("Média", "Mediana", "Média", "Mediana"),
  Mensalidade = c(media_ne, mediana_ne, media_se, mediana_se)

)

# Plotar o gráfico de barras
ggplot(resultados, aes(x = Regiao, y = Mensalidade, fill = Tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  
  geom_text(aes(label = round(Mensalidade, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  
  labs(title = "Mensalidades Médias e Medianas dos Cursos de Ciência da Computação",
       x = "Região",
       y = "Mensalidade (R$)",
       fill = "Tipo") +
  theme_minimal()

