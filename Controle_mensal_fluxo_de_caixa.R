#script para acompanhamento mensal do lucro líquido comparado com a inflação
#1 passo: definir diretorio de trabalho e conferir:

setwd("C:/Users/RNPG/Desktop/TCC/CÓDIGO")
getwd()

#2passo: carregar pacote( já instalado) que me permite trabalhar com planilhas excel:
library(readxl)
library("tidyverse")

#3 passo:importando a planilha Excel e atribuindo para um dataframe para possibilitar
#mais tipos de análises
df_teste <- read_excel("TESTE.xlsx")
View(df_teste)    #conferir se a (xmldataframe (nomedodf)<- xmlToDataFrame("TESTE.xml") é melhor ou se não faz diferença
is.data.frame(df_teste)

#conferir um resumo do dataframe
glimpse(df_teste)   #Para visualizar a estrutura do banco de dados
names(df_teste)  #para visualizar o nome das variáveis
# 4 passo: renomear e remover as colunas que não precisamos para a análise financeira:
#renomear as variáveis:
nova_base <- rename(df_teste, 
                   tipo = "1-Tipo-",
                    categoria = "2-Categoria",
                    descricao = "3-Descrição",
                    documento = "4-Documento",
                    responsavel = "5-Responsável",
                    datavencimento = "6-Data de Vencimento",
                   moeda = "7-Moeda",
                   valor = "8-Valor",
                   situacao = "9-Situação",
                   recibo = "10-Recibo",
                   datadepagamento = "11-Data de Pagamento",
                   moeda2 = "12-Moeda" ,
                   valorpago = "13-Valor Pago",
                   valorliquido =  "14-Valor Líquido",
                   meiodepagamento = "15-Meio de Pagamento",
                   dadosdopagamento = "16-Dados do Pagamento",
                   caixa = "17-Caixa",
                   observacao = "18-Observação")

View(nova_base)   #conferir se as colunas foram renomeadas corretamente

#remover colunas que não iremos utilizar

df_teste2 <- nova_base %>% select(-c(categoria, descricao, documento, responsavel, 
                        datavencimento, moeda, valor, situacao, recibo, 
                        datadepagamento, moeda2, valorpago,
                        meiodepagamento, dadosdopagamento, caixa, observacao))     
 View(df_teste2)         



#5- separar receitas de despesas e calcular o resultado líquido :

entrada_sum <- df_teste2[df_teste2$tipo == "Receita" & df_teste2$valorliquido > 0,]#ok
entrada_sum1 <- entrada_sum$valorliquido
entrada_sum_total <- sum(entrada_sum1)
saida_sum <- df_teste2[df_teste2$tipo == "Despesa" & df_teste2$valorliquido > 0,]
saida_sum_total <- sum(df_teste2$valorliquido[df_teste2$tipo == "Despesa"])

#6-cálculo do resultado líquido

resultado_liquido <- entrada_sum_total - saida_sum_total
resultado_liquido


#7-Material extra de análise: descritiva das receitas e despesas:

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

