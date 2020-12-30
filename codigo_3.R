setwd("C:/Users/Andre/OneDrive/Documentos/Estatística/Métodos estatísticos 2/Trabalho final")
dir()
original<-read.csv("Amostra trabalho.csv",encoding = "UTF-8")

install.packages("DT")

install.packages("DescTools")

library(DT)

library(tidyverse)

library(DescTools)

#Selecionando Amostra----
set.seed(21)

amostra_de_500<-sample_n(original,500, replace = FALSE)


# 3.1 Localização----

locais_notas<-slice(amostra_de_500)%>%
        group_by(LOCALIZACAO, NOTA_MT)%>%
        summarise(Frequencia= n())

rural_notas<-locais_notas$NOTA_MT[1:60]

urbana_notas<-locais_notas$NOTA_MT[61:500]

#_____Teste de variâncias----

var.test(rural_notas,urbana_notas)

#_____Teste t----

t.test(rural_notas, urbana_notas, var.equal = TRUE)


colunas<-c("Estatística T", "Graus de liberdade", "P-valor")

valores<-c(-2.025, 498, 0.02809)


tabelinha_loc<-data.frame(colunas, valores)


tabelinha_loc %>%
        datatable(colnames = c("Dados","Valores"),
                  rownames = FALSE,
                  options = list(paging=FALSE,
                                 ordering=FALSE,
                                 info=FALSE,
                                 searching=FALSE,
                                 columnDefs = list(list(className = 'dt-center',
                                                        targets = "_all"))),
                  width = 500,
                  height = 175,
                  caption = "Dados do teste T de Student")



#3.2 Computador----

#_____Teste de Bartlett----

 
bartlett.test( amostra_de_500$NOTA_MT ~ amostra_de_500$COMPUTADOR)

#_____Anova----

aov_res<- aov(amostra_de_500$NOTA_MT ~ amostra_de_500$COMPUTADOR)

summary(aov_res)

colunas_pc<-c("Estatística F", "Graus de liberdade do numerador","Graus de liberdade do denominador", "P-Valor")

valores_pc<-c(11.62, 4, 484, 5.09e-09)

tabelinha_pc<-data.frame(colunas_pc, valores_pc)


tabelinha_pc %>%
        datatable(colnames = c("Dados","Valores"),
                  rownames = FALSE,
                  options = list(paging=FALSE,
                                 ordering=FALSE,
                                 info=FALSE,
                                 searching=FALSE,
                                 columnDefs = list(list(className = 'dt-center',
                                                        targets = "_all"))),
                  width = 500,
                  height = 210,
                  caption = "Dados do teste ANOVA")
?pairwise.t.test
#_____Comparação múltipla de médias----

pairwise.t.test ( amostra_de_500$NOTA_MT, amostra_de_500$COMPUTADOR, p.adjust.method = "bonferroni" )

#3.3 Escolaridade da mãe----

#_____Teste de Bartlett----

bartlett.test( amostra_de_500$NOTA_MT ~ amostra_de_500$ESC_MAE)

#_____Anova----

aov_res<- aov(amostra_de_500$NOTA_MT ~ amostra_de_500$ESC_MAE)

summary(aov_res)

colunas_mae<-c("Estatística F", "Graus de liberdade do numerador","Graus de liberdade do denominador", "P-Valor")

valores_mae<-c(5.198, 6, 484, 3.38e-05)

tabelinha_mae<-data.frame(colunas_pc, valores_pc)


tabelinha_mae %>%
        datatable(colnames = c("Dados","Valores"),
                  rownames = FALSE,
                  options = list(paging=FALSE,
                                 ordering=FALSE,
                                 info=FALSE,
                                 searching=FALSE,
                                 columnDefs = list(list(className = 'dt-center',
                                                        targets = "_all"))),
                  width = 500,
                  height = 210,
                  caption = "Dados do teste ANOVA")

#_____Comparação múltipla de médias----

pairwise.t.test ( amostra_de_500$NOTA_MT, amostra_de_500$ESC_MAE, p.adjust.method = "bonferroni" )

#3.4 Trabalha fora de casa----

trabalha_notas<-slice(amostra_de_500)%>%
        group_by(TRABALHO, NOTA_MT)%>%
        summarise(Frequencia= n())

Nao_notas<-trabalha_notas$NOTA_MT[1:409]

Sim_notas<-trabalha_notas$NOTA_MT[410:480]

#_____Teste de Variância----

var.test(Nao_notas, Sim_notas)

#_____Teste t de Student----

t.test(Nao_notas, Sim_notas, var.equal = FALSE)

colunas_job<-c("Estat�stica T", "Graus de liberdade", "P-valor")

valores_job<-c(-31.017, 328, 2.2*10^-16)

tabelinha_job<-data.frame(colunas_job, valores_job)


tabelinha_job %>%
        datatable(colnames = c("Dados","Valores"),
                  rownames = FALSE,
                  options = list(paging=FALSE,
                                 ordering=FALSE,
                                 info=FALSE,
                                 searching=FALSE,
                                 columnDefs = list(list(className = 'dt-center',
                                                        targets = "_all"))),
                  width = 500,
                  height = 175,
                  caption = "Dados do teste T de Student")

#3.5 Tabela Perspectivas----

colunas_perspec<-c("Estatística Qui-quadrado","Graus de liberdade" , "P-Valor")

valores_perspec<-c(4.8112, 3, 0.1862)

tabelinha_perspec<-data.frame(colunas_perspec, valores_perspec)


tabelinha_perspec %>%
        datatable(colnames = c("Dados","Valores"),
                  rownames = FALSE,
                  options = list(paging=FALSE,
                                 ordering=FALSE,
                                 info=FALSE,
                                 searching=FALSE,
                                 columnDefs = list(list(className = 'dt-center',
                                                        targets = "_all"))),
                  width = 500,
                  height = 175,
                  caption = "Dados do teste Qui-quadrado de independência")


