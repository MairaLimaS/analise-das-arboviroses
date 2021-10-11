#####Criação da base multinivel

library(dplyr)
library(readxl)
library(utils)
library(stringr)
library(data.table)

######################## Base de incidência, saude familia e bioma = data_HLM_arbo_2016_2019_v2
#load("dataset_HLM_v1.RData")
load("dataset_HLM_v2.RData")

##IncidÊncia e covariáveis
#você não precisa ter os dados dessas variáveis repetidas a não ser que você tenha interesse em estuda-las desta forma.
#Apenas o desfecho pode ser longitudinal...não necessariamente as covariaveis precisam estar.
#O menos complicado é deixar somente com 1 ano...o mais atual

## Deixando o desfecho "long"

variaveis.freq <- colnames(data_HLM_arbo_2016_2019_v2)
variaveis.interesse = c(
  colnames(data_HLM_arbo_2016_2019_v2[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'Regiao.de.Saude')],
  colnames(data_HLM_arbo_2016_2019_v2[, variaveis.freq])[str_detect(variaveis.freq, pattern = '2019')])


dataHLM<-data_HLM_arbo_2016_2019_v2[,variaveis.interesse]

variaveis.notinter<-c("5","8","9")
dataHLM<-dataHLM[,-as.integer(variaveis.notinter)]


#funcao para retirar % da variavel percent.AB
formata_percent <- function(num) {
  valor <-c("")/
  tam<-nchar(num)
  #valor<-ifelse(str_detect(num, "%"),str_extract(num,'[1-9]'),num)
  #valor<-gsub('.{2}$', '', name)
  #valor<-grep("[1-9]",num,value=TRUE)
  valor<-substr(num,1,tam-1)
  valor<-gsub(',', '.', valor)
return(valor)
}

dataHLM["percent.AB_2019"] <-formata_percent(dataHLM$percent.AB_2019)

# categorizando base biomas
#http://www2.cemaden.gov.br/mapainterativo/#
#http://www.seia.ba.gov.br/monitoramento-ambiental/pluviom-trico

#Legenda bioma
#1. floresta	1	mosaico	Cobertura/uso
#2. formação natural nao florestal	10	natural	Cobertura
#3. agropecuaria	14	antropico	Uso
#4. area nao vegetada	22	mosaico	Cobertura/uso
#5. corpos dagua	26	mosaico	Cobertura/uso

var.interesse<-colnames(dataHLM)[str_detect(colnames(dataHLM),regex('tipo.bioma', ignore_case = TRUE))]

library(magrittr)

# categorizando o bioma
for (var in var.interesse){ 
  cod.tipo.bioma= case_when(str_detect(dataHLM[, var], pattern = 'floresta') ~'1',
                              str_detect(dataHLM[, var], pattern = 'nao florestal') ~'10',
                              str_detect(dataHLM[, var], pattern = 'agropecuaria') ~'14',
                              str_detect(dataHLM[, var], pattern = 'vegetada') ~'22', TRUE ~ '26')
  dataHLM[, var]<-cod.tipo.bioma                        
}

grupo.2<-"Barreiras"
grupo.3<-c("Feira de Santana","Camaçari")
grupo.5<-"Salvador"

dataHLM<-dataHLM%>%mutate(cod.regiao.saude= case_when(str_detect(Regiao.de.Saude, pattern = 'Alago') ~'1',
                                   str_detect(Regiao.de.Saude, pattern = 'Barr') ~'2',
                                   str_detect(Regiao.de.Saude, pattern = 'Brum') ~'3',
                                   str_detect(Regiao.de.Saude, pattern = 'Camaç') ~'4',
                                   str_detect(Regiao.de.Saude, pattern = 'Cruz') ~'5',
                                   str_detect(Regiao.de.Saude, pattern = 'Feira') ~'6',
                                   str_detect(Regiao.de.Saude, pattern = 'Guana') ~'7',
                                   str_detect(Regiao.de.Saude, pattern = 'Ibot') ~'8',
                                   str_detect(Regiao.de.Saude, pattern = 'Ilh') ~'9',
                                   str_detect(Regiao.de.Saude, pattern = 'Ire') ~'10',
                                   str_detect(Regiao.de.Saude, pattern = 'Itabe') ~'11',
                                   str_detect(Regiao.de.Saude, pattern = 'Itab') ~'12',
                                   str_detect(Regiao.de.Saude, pattern = 'Itape') ~'13',
                                   str_detect(Regiao.de.Saude, pattern = 'Jaco') ~'14',
                                   str_detect(Regiao.de.Saude, pattern = 'Jequi') ~'15',
                                   str_detect(Regiao.de.Saude, pattern = 'Jua') ~'16',
                                   str_detect(Regiao.de.Saude, pattern = 'Paulo') ~'17',
                                   str_detect(Regiao.de.Saude, pattern = 'Porto') ~'18',
                                   str_detect(Regiao.de.Saude, pattern = 'Ribeira') ~'19',
                                   str_detect(Regiao.de.Saude, pattern = 'Salv') ~'20',
                                   str_detect(Regiao.de.Saude, pattern = 'da Vitória') ~'21',
                                   str_detect(Regiao.de.Saude, pattern = 'Santo') ~'22',
                                   str_detect(Regiao.de.Saude, pattern = 'Seabra') ~'23',
                                   str_detect(Regiao.de.Saude, pattern = 'Senhor') ~'24',
                                   str_detect(Regiao.de.Saude, pattern = 'Serr') ~'25',
                                   str_detect(Regiao.de.Saude, pattern = 'Teix') ~'26',
                                   str_detect(Regiao.de.Saude, pattern = 'Val') ~'27',TRUE ~ '28'))%>% 
  mutate(grupo.SES.2014 = case_when(Regiao.de.Saude %in% grupo.2 ~ '2',
                                    Regiao.de.Saude %in% grupo.3 ~ '3',
                                    Regiao.de.Saude %in% grupo.5 ~ '5'
                                    ,TRUE ~ '1'))
dataHLM<-dataHLM%>% relocate(cod.regiao.saude, .after = Regiao.de.Saude)
dataHLM<-dataHLM[,-1]

#library(reshape)
#library(reshape2)
#dta<-melt(zika)


rm(variaveis.freq,variaveis.interesse, cod.tipo.bioma,grupo.2,grupo.3,grupo.5,var,variaveis.notinter)

######## Base vetorial indicador22 de ciclos (PVQAS), risco com base Liraa, criadouros = vetor

load("dataset_HLM_v3.RData")

variaveis.interesse<-colnames(vetor)[str_detect(colnames(vetor),regex('^cod.|_2019|_CR_|^Risco', ignore_case = TRUE))]

#dataHLM_2<-subset(vetor,variaveis.interesse)
dataHLM_2<-vetor %>% select(variaveis.interesse)

######################## dados IFDM, PIB, CNIS =dta.2017
load("dataset_HLM_v4.RData")

variaveis.interesse<-colnames(dta.2017)[str_detect(colnames(dta.2017),regex('^cod|_PIB.percapita|SNIS', ignore_case = TRUE))]

dataHLM_3<-dta.2017 %>% select(variaveis.interesse)

##################################

data_MLH_final<-left_join(dataHLM,dataHLM_3,by= c("cod.regiao.saude"="cod.regiao.saude"))
data_MLH_final<-left_join(data_MLH_final,dataHLM_2,by= c("cod.regiao.saude"="cod.regiao.saude"))

save(data_MLH_final,file = "dataset_multinivel.RData")


save(dataHLM,dataHLM_2,dataHLM_3,file="dataset_HLM_v5.RData")

library(haven)
library(foreign)
names(data_MLH_final) <- str_replace_all(names(data_MLH_final), pattern = "\\.", replacement = "_") # to replace illegal character
data_MLH_final[is.na(data_MLH_final)] <- 0
write_dta(data_MLH_final, "MLH.dta")
#d<-read_dta("MLH.dta")
