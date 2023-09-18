library("dplyr")
library("readxl")
library("tidyverse")
library("ggplot2")
library("scales")#fomatação de escalas e rótulos em gráficos(no caso,converter os rótulos de dados em R$ e porcentagens do valor total)
# obs: scales é frequentemente utilizado em conjunto com o ggplot2.
library("gridExtra")#(para plotar mais de um gráfico na área de plotagem)

# Lê os dados do Excel
percentual2022 <- read_excel("RESULTADO_MENSAL_2022.xlsx")

# Calcula a diferença percentual
dif_percentual_mensal_2022 <- percentual2022 %>%
  mutate(
    Previous = lag(RESULTADO_LIQUIDO),
    Change = RESULTADO_LIQUIDO - Previous,
    ChangePercentage = ifelse(is.na(Previous), 0, (Change / Previous) * 100)
  ) %>%
  mutate(ChangePercentage = round(ChangePercentage, 2))

# Ordena os meses (precisei criar um factor e levels porque os nomes não estavam em inglês)(ok)
ordem_meses <- c("JAN", "FEV", "MAR", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOST", "SET", "OUT", "NOV", "DEZ")
resultado_teste <- percentual2022 %>%
  mutate(MES = factor(MES, levels = ordem_meses)) %>%
  mutate(RESULTADO_FORMATADO = scales::dollar(RESULTADO_LIQUIDO, scale = 0.001, prefix = "R$", accuracy = 0.001))

# Combina os dois dataframes usando a coluna MES como chave
merged_data <- left_join(resultado_teste, dif_percentual_mensal_2022, by = "MES")

# Define a ordem desejada dos meses
ordem_meses <- c("JAN", "FEV", "MAR", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOST", "SET", "OUT", "NOV", "DEZ")

# Aplica a ordem aos níveis da coluna MES
merged_data$MES <- factor(merged_data$MES, levels = ordem_meses)

# Define manualmente as cores sem usar ifelse(por causa que não queria legenda)
merged_data$LabelColor <- ifelse(merged_data$ChangePercentage >= 0, "Positivo", "Negativo")

# Cria o gráfico usando o dataframe combinado
df_base <- ggplot(data = merged_data, aes(x = MES, y = RESULTADO_LIQUIDO.y)) +
  geom_bar(stat = "identity", fill = "gray", alpha = 0.2) +
  geom_line(aes(group = 1), color = "red") +
  geom_label(aes(label = RESULTADO_FORMATADO)) +
  
  # Adiciona rótulo de diferença percentual acima das outras labels + negrito
  geom_text(
    aes(label = paste0(ChangePercentage, "%"), color = LabelColor),
    vjust = 3.5,
    fontface = "bold" 
  ) +
  
  xlab("Mês") + ylab("Resultado Líquido") +
  scale_y_continuous(breaks = NULL) + labs(x = NULL, y = NULL) +
  ggtitle("Resultado Líquido Mensal 2022") +
  theme(plot.title = element_text(hjust = 0, color = "darkgray")) + # Define a cor do título
  theme(axis.text.x = element_text(face = "bold")) +
  theme_classic() +
  
  # Remove a legenda de cores
  scale_color_manual(values = c("Positivo" = "darkgreen", "Negativo" = "red"), guide = "none")

# Exibe o gráfico
print(df_base)




