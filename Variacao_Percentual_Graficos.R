# código 2

#cálculo das variações de porcentagem e criação dos gráficos para visualização dos dados

#percentage change formula:
##((new Amount - original amount)/original amount) * 100

#código

library("dplyr")
library("readxl")
library("tidyverse")
library("ggplot2")
getwd()

percentual2022 <- read_excel("RESULTADO_MENSAL_2022.xlsx")
View(percentual2022)    #conferindo se a tabela está certa (ok)

percentual2022novo <- percentual2022%>%
  mutate(Previous = lag(RESULTADO_LIQUIDO),
         Change = RESULTADO_LIQUIDO - Previous,
         ChangePercentage = (Change/Previous)*100)
         


