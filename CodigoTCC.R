#script para acompanhamento mensal do lucro líquido comparado com a inflação
#1 passo: definir diretorio de trabalho e conferir:

setwd("C:/Users/RNPG/Desktop/TCC/CÓDIGO")
getwd()

#2passo: carregar pacote( já instalado) que me permite trabalhar com planilhas excel:
library(readxl)
library("tidyverse")
#Listar e visualizar as worksheets no arquivo Excel
excel_sheets("TESTE.xlsx")
View(read_excel("TESTE.xlsx"))

#3 passo:importando a planilha Excel e atribuindo para um dataframe para possibilitar
#mais tipos de análises
df_teste <- read_excel("TESTE.xlsx")
View(df_teste)    #conferir se a (xmldataframe (nomedodf)<- xmlToDataFrame("TESTE.xml") é melhor ou se não faz diferença
is.data.frame(df_teste)

#conferir um resumo do dataframe
summary(df_teste)   # visualizar um resumo do banco de dados
glimpse(df_teste)   #Para visualizar a estrutura do banco de dados
names(df_teste)  #para visualizar o nome das variáveis
# 4 passo: renomear e remover as colunas que não precisamos para a análise financeira:

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

View(nova_base)   #conferir
#remover colunas que não iremos utilizar

df_teste2 <- nova_base %>% select(-c(categoria, descricao, documento, responsavel, 
                        datavencimento, moeda, valor, situacao, recibo, 
                        datadepagamento, moeda2, valorpago,
                        meiodepagamento, dadosdopagamento, caixa, observacao))     
 View(df_teste2)         



#5- separar receitas de despesas:
 entrada_sum <- df_teste2[df_teste2$tipo == "Receita" > 0,]  #conferir aula 2 1:32""
 entrada_sum <- sum(df_teste2$valorliquido[df_teste2$tipo == "Receita"])
 saida_sum <- sum(df_teste2$valorliquido[df_teste2$tipo == "Despesa"])
 
View(entrada_sum)#conferir soma das entradas
View(saida_sum) #conferir soma das despesas

#6- Dimunir as despesas do total de receitas para criar o resultado líquido

#7- exemplo do uso da função sumarrise (adaptar para meu df)
 # Função "summarise": função que resume o dataset, podendo criar outros
# Abaixo, as observações da variável "tempo" são resumidas em descritivas

descritivas_nova_base <- summarise(nova_base,
                                   observações = n(),
                                   média = mean(tempo),
                                   mediana = median(tempo),
                                   desv_pad = sd(tempo),
                                   mínimo = min(tempo),
                                   máximo = max(tempo),
                                   quartil_3 = quantile(tempo, probs = 0.75))

print(descritivas_nova_base)


#testando group by para análise estatítica de receitas e despesas
teste_groupby <- group_by(df_teste2, tipo)

descritivas_df_teste2 <- teste_groupby %>% 
  summarise(média = mean(tipo),
            desvio_pad = sd(tipo),
            n_obs = n())
View(descritivas_df_teste2)

# adicionar um novo vetor a nível de coluna ao dataframe ( inflação)
#criar inflacao_mensal = c()
# criar df_teste = cbind(df_teste, inflacao_mensal)