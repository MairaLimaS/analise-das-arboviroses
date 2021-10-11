#carregar 
setwd("G:/backup_maira/SINAN/analisebruno")
load("dataset_arboviroses_2016_2019.RData") # contêm datasets de todos casos suspeitos e confirmados e dataset de incidência. Dados separardos por arbo  
load("dataset_HLMs_v1.RData")

library(dplyr)
library("data.table")
require("stringr")
library(stringr)
library(readxl)
library(tidyselect)

#######################
# Data cobertura AB
#https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/relHistoricoCoberturaAB.xhtml
#https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/nota_tecnica/nota_tecnica_relatorio_de_cobertura_AB.pdf
#Cobertura ab é cobertura de atenção básica, neste calculo incluí as equipes de saúde da família e de atenção básica convencional (o antigo posto de saúde);
#cobertura sf = cobertura de saúde da família, considera apenas as equipes de Saúde da Família.
#cobertura_ab<-read_excel("G:\\SINAN\\Nova pasta\\AB_ESF_BA_CIR_2016_2019.xls")
#Número de pessoas vinculadas ao SUS,
#https://sisab.saude.gov.br/paginas/acessoRestrito/relatorio/federal/indicadores/indicadorCadastro.xhtml

variaveis.freq <- colnames(cobertura_ab)
variaveis.interesse = c(
  colnames(cobertura_ab[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'Regiao.')],
  colnames(cobertura_ab[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'percent.')])

cobertura_ab<-cobertura_ab[,variaveis.interesse]
####################
# http://idsus.saude.gov.br/simplificadas.html

##Indicador nº 1

#Cobertura populacional estimada pelas equipes básicas de saúde.

#Definição: Nº de equipes de saúde da família (ESF) + nº de equipes da atenção básica, formada por 60h semanais de clínica médica, ginecologia e pediatria, para cada 3 mil pessoas residentes no município, no ano.

#Interpretação: Mede a cobertura das equipes básicas de saúde (ESF ou clínica médica, ginecologia e pediatria).Maior cobertura indicaria maior oferta de serviços das clínicas básicas e facilidade de acesso.

#Método de Cálculo:(Nº médio anual de equipes da saúde da família + nº médio anual de cargas horárias de 60h semanais da clínica médica, ginecologia e pediatria) x por 3 mil ÷ pela população residente no município.

#Parâmetro:100% cobertura considerando uma equipe para 3 mil habitantes.

#Pontuação: SE resultado ??? parâmetro nota = 10.;SE resultado < parâmetro nota decrescente proporcional ao % do parâmetro.

############################

# Renomeando as colunas das bases arboviroses 

##CHIK

variaveis.freq <- colnames(data_cases_incd_chik_2016_2019)
variaveis.interesse = c(
  colnames(data_cases_incd_chik_2016_2019[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'Regiao.')],
  colnames(data_cases_incd_chik_2016_2019[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'incid_')])

chik<-data_cases_incd_chik_2016_2019[,variaveis.interesse]
  
names(chik)<-c("Regiao.de.Saude","chik_incid_2016","chik_incid_2017","chik_incid_2018","chik_incid_2019")

##ZIKA

variaveis.freq <- colnames(data_cases_incd_zika_2016_2019)
variaveis.interesse = c(
  colnames(data_cases_incd_zika_2016_2019[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'Regiao.')],
  colnames(data_cases_incd_zika_2016_2019[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'incid_')])

zika<-data_cases_incd_zika_2016_2019[,variaveis.interesse]

names(zika)<-c("Regiao.de.Saude","zika_incid_2016","zika_incid_2017","zika_incid_2018","zika_incid_2019")

##DENGUE

variaveis.freq <- colnames(data_cases_incd_dengue_2016_2019)
variaveis.interesse = c(
  colnames(data_cases_incd_dengue_2016_2019[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'Regiao.')],
  colnames(data_cases_incd_dengue_2016_2019[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'incid_')])

dengue<-data_cases_incd_dengue_2016_2019[,variaveis.interesse]

names(dengue)<-c("Regiao.de.Saude","dengue_incid_2016","dengue_incid_2017","dengue_incid_2018","dengue_incid_2019")

############################
# unindo os datasets: arbo e cobertura_ab

data_HLM_v1<-merge(dengue,chik,by.x = "Regiao.de.Saude", by.y="Regiao.de.Saude",all.x = TRUE)
data_HLM_v2<-merge(data_HLM_v1,zika,by.x = "Regiao.de.Saude", by.y="Regiao.de.Saude",all.x = TRUE)
data_HLM_arbo_2016_2019_v1<-merge(data_HLM_v2,cobertura_ab,by.x = "Regiao.de.Saude", by.y="Regiao.de.Saude",all.x = TRUE)

save(dengue,chik,zika, file = "dataset_arbovirose_incidencia.RData") #
save(cobertura_ab,data_HLM_arbo_2016_2019_v1,file='dataset_HLM_v1.RData')

############################
# unindo os datasets: data_HLM_arbo_2016_2019_v1 e bioma
regional<-read_excel("G:\\SINAN\\Nova pasta\\cod_regionalizacao_ba.xlsx")
bioma<-read_excel("bioma_ba_2016_2019.xls")

# armazeno as linhas que contêm a mesma cidade
# mantem no dataset informações unicas sobre o bioma.
# recupero o valor maior do bioma por ano.

# Calculo o valor de ocupação total do bioma no municipio
coluna<-unique(bioma$municipality)
bioma_agrupado_2016_2019<-data.frame()
for(i in 1:length(coluna)){
#for(i in 1:1){
  cidade=coluna[i]
  data<-bioma[bioma$municipality==cidade,]
  aux<-
  all_bioma_2016<-aggregate(data$bioma_2016, by=list(municipio.tipo.bioma.2016=data$tipo_bioma), FUN=sum)%>%rename(area.total.bioma.2016=x)%>%arrange(desc(area.total.bioma.2016))
  all_bioma_2017<-aggregate(data$bioma_2017, by=list(municipio.tipo.bioma.2017=data$tipo_bioma), FUN=sum)%>%rename(area.total.bioma.2017=x)%>%arrange(desc(area.total.bioma.2017))
  all_bioma_2018<-aggregate(data$bioma_2018, by=list(municipio.tipo.bioma.2018=data$tipo_bioma), FUN=sum)%>%rename(area.total.bioma.2018=x)%>%arrange(desc(area.total.bioma.2018))
  all_bioma_2019<-aggregate(data$bioma_2019, by=list(municipio.tipo.bioma.2019=data$tipo_bioma), FUN=sum)%>%rename(area.total.bioma.2019=x)%>%arrange(desc(area.total.bioma.2019))
  aux<-cbind(all_bioma_2016,all_bioma_2017,all_bioma_2018,all_bioma_2019)
  tam<-nrow(aux)
  aux_2<-data[1:tam,1:2]
  aux<-cbind(aux,aux_2)%>%select(IBGE,municipality, everything())
  bioma_agrupado_2016_2019<-rbind(bioma_agrupado_2016_2019,aux)
  rm(all_bioma_2016,all_bioma_2017,all_bioma_2018,all_bioma_2019,aux)
}


# seleciono o valor maior
coluna<-unique(bioma_agrupado_2016_2019$municipality)
bioma_selected<-data.frame()
for(i in 1:length(coluna)){
  cidade=coluna[i]
  df<-bioma_agrupado_2016_2019[bioma_agrupado_2016_2019$municipality==cidade,]
  bioma_2016<-df%>%slice_max(area.total.bioma.2016)%>%select(municipio.tipo.bioma.2016, area.total.bioma.2016)
  bioma_2017<-df%>%slice_max(area.total.bioma.2017)%>%select(municipio.tipo.bioma.2017, area.total.bioma.2017)
  bioma_2018<-df%>%slice_max(area.total.bioma.2018)%>%select(municipio.tipo.bioma.2018, area.total.bioma.2018)
  bioma_2019<-df%>%slice_max(area.total.bioma.2019)%>%select(municipio.tipo.bioma.2019, area.total.bioma.2019)
  bioma_2016_2019<-bind_cols(bioma_2016,bioma_2017,bioma_2018,bioma_2019)
  df<-df%>%select(IBGE,municipality)%>%distinct()
  bioma_2016_2019<-bind_cols(bioma_2016_2019,df) %>% select(IBGE, municipality, everything())
  bioma_selected<-bind_rows(bioma_selected,bioma_2016_2019)
  rm(bioma_2016,bioma_2017,bioma_2018,bioma_2019,df,bioma_2016_2019)
}


#dataset com os biomas por municipio e com informações de regiao de saude
regional_bioma<-merge(regional,bioma_selected,by.x = "IBGE_7", by.y="IBGE",all.x = TRUE)


##########################
# bioma contêm áreas(ha) por classe de cobertura/uso de solo do preiodo de 2016 a 2019
# unidade medida em bioma é municipio 
# necessita calcular porcentagem de ocupação do bioma por regiao de saude.
# para isso, uso a base estimativa municipios fornecida pelo sei ba.
# dado contém informação de area do municipo

regional_area<-read_excel("G:\\SINAN\\SEI\\Estimativa_municipios.xlsx", sheet="Tabela 1")

# dataset bioma e área municipio
regional_bioma_area<-merge(regional_area,regional_bioma,by.x = "IBGE7", by.y="IBGE_7",all.x = TRUE)

###### calcular percentual de ocupação bioma na regiao de saude

# #Abordagem: contagem area por Regiao de Saude
# # Bioma ocupa HA. 1km2 = 100HA. 1 HA = 1/100 km2

#conta_area_regi<- regional_bioma_area%>%select(Regiao.de.Saude,area.2019)%>%group_by(Regiao.de.Saude)%>%summarize(area_total.2019=sum(area.2019)) 
conta_area_regi<- aggregate(regional_bioma_area$area.2019, by=list(RegiaoSaude=regional_bioma_area$Regiao.de.Saude), FUN=sum)%>%rename(area_total.2019=x)


bioma_regiao_2016_2017<-data.frame()
for (k in 1:nrow(conta_area_regi)) {
  data<-regional_bioma[regional_bioma$Regiao.de.Saude==conta_area_regi[k,"RegiaoSaude"],]
  #variaveis<- colnames(data)[str_detect(variaveis.freq, regex('^Regiao|^tipo_|^bioma', ignore_case = TRUE))]
  #data<-data[,variaveis]
  bioma_2016<-aggregate(data$area.total.bioma.2016, by=list(regiaosaude.tipo.bioma.2016=data$municipio.tipo.bioma.2016), FUN=sum)%>%rename(area.total.bioma.2016=x)%>%slice_max(area.total.bioma.2016)%>%mutate(percent.area.ocupada_2016=(((area.total.bioma.2016/100)/conta_area_regi[k,2]) *100))
  bioma_2017<-aggregate(data$area.total.bioma.2017, by=list(regiaosaude.tipo.bioma.2017=data$municipio.tipo.bioma.2017), FUN=sum)%>%rename(area.total.bioma.2017=x)%>%slice_max(area.total.bioma.2017)%>%mutate(percent.area.ocupada_2017=(((area.total.bioma.2017/100)/conta_area_regi[k,2]) *100))
  bioma_2018<-aggregate(data$area.total.bioma.2018, by=list(regiaosaude.tipo.bioma.2018=data$municipio.tipo.bioma.2018), FUN=sum)%>%rename(area.total.bioma.2018=x)%>%slice_max(area.total.bioma.2018)%>%mutate(percent.area.ocupada_2018=(((area.total.bioma.2018/100)/conta_area_regi[k,2]) *100))
  bioma_2019<-aggregate(data$area.total.bioma.2019, by=list(regiaosaude.tipo.bioma.2019=data$municipio.tipo.bioma.2019), FUN=sum)%>%rename(area.total.bioma.2019=x)%>%slice_max(area.total.bioma.2019)%>%mutate(percent.area.ocupada_2019=(((area.total.bioma.2019/100)/conta_area_regi[k,2]) *100))
  aux<-cbind(conta_area_regi[k,1],bioma_2016,bioma_2017,bioma_2017,bioma_2018,bioma_2019)
  names(aux)[1]="Regiao.de.Saude"
  bioma_regiao_2016_2017<-rbind(bioma_regiao_2016_2017,aux)
  rm(bioma_2016,bioma_2017,bioma_2018,bioma_2019,aux)
}

#################
# Dataset com informações de incidencia, cobertura ab, bioma
data_HLM_arbo_2016_2019_v2<-merge(data_HLM_arbo_2016_2019_v1,bioma_regiao_2016_2017,by = "Regiao.de.Saude")

save(bioma_agrupado_2016_2019,bioma_selected,regional_bioma,conta_area_regi,bioma_regiao_2016_2017, file="dataset_bioma.RData")
save(data_HLM_arbo_2016_2019_v2, file="dataset_HLM_v2.RData")


######################
