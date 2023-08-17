getwd()

library("readxl")
library("tidyverse")
library("dplyr")


df_teste <- read_excel("TESTE.xlsx")
View(df_teste)  

colnames(df_teste)[1] = "tipo" 
colnames(df_teste)[2] = "categoria"
colnames(df_teste)[3] = "descricao"
colnames(df_teste)[4] = "documento"
colnames(df_teste)[5] = "responsavel"
colnames(df_teste)[6] = "datavencimento"
colnames(df_teste)[7] = "moeda"
colnames(df_teste)[8] = "valor"
colnames(df_teste)[9] = "situacao"
colnames(df_teste)[10] = "recibo"
colnames(df_teste)[11] = "datadepagamento"
colnames(df_teste)[12] = "moeda2"
colnames(df_teste)[13] = "valorpago"
colnames(df_teste)[14] = "valorliquido"
colnames(df_teste)[15] = "meiodepagamento"
colnames(df_teste)[16] = "dadosdopagamento"
colnames(df_teste)[17] = "caixa"
colnames(df_teste)[18] = "observacao"

View(df_teste)  

df_teste2 <-df_teste %>% select(-c(categoria, descricao, documento, responsavel, 
                                   datavencimento, moeda, valor, situacao, recibo, 
                                   datadepagamento, moeda2, valorpago,
                                   meiodepagamento, dadosdopagamento, caixa, observacao))     

entrada_sum <- df_teste2[df_teste2$tipo == "Receita" & df_teste2$valorliquido > 0,]
entrada_sum1 <- entrada_sum$valorliquido
entrada_sum_total <- sum(entrada_sum1)
saida_sum <- df_teste2[df_teste2$tipo == "Despesa" & df_teste2$valorliquido > 0,]
saida_sum_total <- sum(df_teste2$valorliquido[df_teste2$tipo == "Despesa"])

resultado_liquido <- entrada_sum_total - saida_sum_total
resultado_liquido


#Material extra de análise: descritiva das receitas e despesas:

descritivas_entradas <- summarise (entrada_sum,
                                   observações = n(),
                                   média = mean(valorliquido),
                                   mediana = median(valorliquido),
                                   desv_pad = sd(valorliquido),
                                   mínimo = min(valorliquido),
                                   máximo = max(valorliquido))

print(descritivas_entradas)

descritivas_saídas <- summarise (saida_sum,
                                 observações = n(),
                                 média = mean(valorliquido),
                                 mediana = median(valorliquido),
                                 desv_pad = sd(valorliquido),
                                 mínimo = min(valorliquido),
                                 máximo = max(valorliquido))

print(descritivas_saídas)

#fim do código de controle mensal.
