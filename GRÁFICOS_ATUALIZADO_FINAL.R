library("dplyr")
library("readxl")
library("tidyverse")
library("ggplot2")
library("scales")
library("gridExtra")
library("tidyr")

percentual2022 <- read_excel("RESULTADO_MENSAL_2022.xlsx")
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
merged_data$LabelColor <- ifelse(merged_data$ChangePercentage >= 0, "Positivo", "Negativo")

# Cria o gráfico usando o dataframe combinado
df_base <- ggplot(data = merged_data, aes(x = MES, y = RESULTADO_LIQUIDO.y)) +
  geom_bar(stat = "identity", fill = "gray", alpha = 0.2) +
  geom_line(aes(group = 1), color = "red") +
  geom_label(aes(label = RESULTADO_FORMATADO)) +
  geom_text(
    aes(label = paste0(ChangePercentage, "%"), color = LabelColor),
    vjust = 3.5,
    fontface = "bold" ) +
  xlab("Mês") + ylab("Resultado Líquido") +
  scale_y_continuous(breaks = NULL) + labs(x = NULL, y = NULL) +
  ggtitle("Resultado Líquido Mensal 2022") +
  theme(plot.title = element_text(hjust = 0, color = "darkgray")) + # Define a cor do título
  theme(axis.text.x = element_text(face = "bold")) +
  theme_classic() +
  scale_color_manual(values = c("Positivo" = "darkgreen", "Negativo" = "red"), guide = "none")
print(df_base)





##variação anual por período 

resultado_anual_ipca <- read_excel("RES_IPCA.xlsx")
as.data.frame(resultado_anual_ipca)
resultado_anual_ipca <- resultado_anual_ipca %>%
rename(MES = MÊS)
resultado_anual_ipca <- resultado_anual_ipca %>%
mutate(VAR_R_LIQ = ((RESULTADO_LIQ_2023 - RESULTADO_LIQ_2022)/ RESULTADO_LIQ_2022) * 100)
resultado_anual_ipca <- resultado_anual_ipca %>%
select(MES, RESULTADO_LIQ_2022, RESULTADO_LIQ_2023, VAR_R_LIQ, `VAR_IPCA_ANUAL`)

#remove linhas com NA para evitar erro na plotagem 

resultado_anual_ipca <- resultado_anual_ipca %>%
  drop_na()




# Crie o gráfico de barras e linha

# Define a ordem desejada dos meses

ordem_meses <- c("JAN", "FEV", "MAR", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGO") 
resultado_anual_ipca$MES <- factor(resultado_anual_ipca$MES, levels = ordem_meses)

pl <- ggplot(data = resultado_anual_ipca)
pl <- pl + geom_line(aes(x = factor(MES), y = VAR_IPCA_ANUAL), color = "blue", group = 1)
pl <- pl + geom_point(aes(x = factor(MES), y = VAR_IPCA_ANUAL))
pl <- pl + geom_col(aes(x = factor(MES) , y = VAR_R_LIQ, fill = VAR_R_LIQ > 0), alpha = 0.2)
pl <- pl + theme_classic()
pl <- pl + scale_fill_manual(values = c("red", "darkgreen"))
pl <- pl + geom_label(aes(x = factor(MES), y = VAR_R_LIQ, label = paste0(round(VAR_R_LIQ,2), "%")), size = 3, vjust = -0.5)
pl <- pl + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
pl <- pl + labs(title = "Acompanhamento período de 12 meses (2022-2023 mensal)")
l <- pl + labs(title = "Acompanhamento período de 12 meses (2022-2023 mensal)") +
  theme(plot.title = element_text(size = 12))
pl <- pl + geom_line(aes(x = factor(MES), y = VAR_IPCA_ANUAL), color = "blue", group = 1) +
  geom_text(aes(x = factor(MES), y = VAR_IPCA_ANUAL, label = paste0(round(VAR_IPCA_ANUAL, 2), "%")), 
            vjust = -0.5, hjust = 0, color = "black")
pl <- pl + scale_y_continuous(breaks = c(0))

pl