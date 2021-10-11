#carregar 
load("dataset_arboviroses.RData")
load("dataset_arboviroses_fase1.RData")

library(dplyr)
library("data.table")

#Abordagem: considerar casos prováveis = confirmados+ inconclusivos(8)

dengue_notdesc <- dengue %>% filter(!CLASSI_FIN%in%c(5))
zika_notdesc <- zika %>% filter(!CLASSI_FIN%in%c(5))
chik_notdesc <-  chik %>% filter(!CLASSI_FIN%in%c(5))

save(chik, chik_conf, chik_notdesc, dengue, dengue_conf,dengue_notdesc, zika, zika_conf,zika_notdesc, file = 'dataset_arboviroses_fase1.RData')
#Abordagem: filtrar anos 2016-2019

dengue_notdesc_16_19 <-  dengue_notdesc %>% filter(NU_ANO%in%c(2016,2017,2018,2019))
zika_notdesc_16_19 <-  zika_notdesc %>% filter(NU_ANO%in%c(2016,2017,2018,2019))
chik_notdesc_16_19 <-  chik_notdesc %>% filter(NU_ANO%in%c(2016,2017,2018,2019))

# #Abordagem: contagem casos por Regiao de Saude

zika_notdesc_regi<- as.data.table(table(zika_notdesc_16_19$NU_ANO, zika_notdesc_16_19$Região.de.Saúde))%>%data.frame
chik_notdesc_regi<- as.data.table(table(chik_notdesc_16_19$NU_ANO, chik_notdesc_16_19$Região.de.Saúde))%>%data.frame
dengue_notdesc_regi<- as.data.table(table(dengue_notdesc_16_19$NU_ANO, dengue_notdesc_16_19$Região.de.Saúde))%>%data.frame

names(zika_notdesc_regi)<-c("NU_ANO","Regiao.de.Saude","CIR_casos_zika")
names(chik_notdesc_regi)<-c("NU_ANO","Regiao.de.Saude","CIR_casos_chik")
names(dengue_notdesc_regi)<-c("NU_ANO","Regiao.de.Saude","CIR_casos_dengue")

## Cálculo incidência

# Abordagem: merge população anual da regiao saude

# a) recuperando o banco com informacoes da populacao na regiao de saude

CIR <- read.csv("pop_regiaosaude_bahia_2016_2019.csv", sep=";", header = T)

# b) inserindo informacoes da populacao na regiao de saude para as bases zika, dengue e chik

zika_notdesc_regi <- merge(zika_notdesc_regi, CIR, by.x = "Regiao.de.Saude", by.y="Regiao.de.Saude",all.x = TRUE)
dengue_notdesc_regi <- merge(dengue_notdesc_regi, CIR, by.x = "Regiao.de.Saude", by.y="Regiao.de.Saude",all.x = TRUE)
chik_notdesc_regi <- merge(chik_notdesc_regi, CIR, by.x = "Regiao.de.Saude", by.y="Regiao.de.Saude",all.x = TRUE)



### Abordagem: cálculo incidencia ano

require("stringr")
library(stringr)

################ ZIKA

for(i in 1:nrow(zika_notdesc_regi)){
  ano<-na.omit(zika_notdesc_regi$NU_ANO[i])
  indice = which(str_detect(colnames(zika_notdesc_regi), pattern = ano) == TRUE)
  incid= ((zika_notdesc_regi$CIR_casos_zika[i]/zika_notdesc_regi[i,indice]) *100000)
  incid=round(incid,2)
  #nomecol=paste0("CIR_incid_",ano)
  #zika_notdesc_regi[i,nomecol]=incid
  zika_notdesc_regi[i,"INCID"]=incid
  
}

#### criando base formato wide para incidencia e casos
library(reshape2)
data_incd_zika<-dcast(zika_notdesc_regi, Regiao.de.Saude ~ NU_ANO)
data_cases_zika<-dcast(zika_notdesc_regi, Regiao.de.Saude ~ NU_ANO, value.var = "CIR_casos_zika")

names(data_incd_zika)<-c("Regiao.de.Saude","incid_2016","incid_2017","incid_2018","incid_2019")
names(data_cases_zika)<-c("Regiao.de.Saude","casos_2016","casos_2017","casos_2018","casos_2019")

data_cases_incd_zika_2016_2019<-merge(data_cases_zika,data_incd_zika, by.x = "Regiao.de.Saude", by.y="Regiao.de.Saude",all.x = TRUE)

################ DENGUE

for(i in 1:nrow(dengue_notdesc_regi)){
  ano<-na.omit(dengue_notdesc_regi$NU_ANO[i])
  indice = which(str_detect(colnames(dengue_notdesc_regi), pattern = ano) == TRUE)
  incid= ((dengue_notdesc_regi$CIR_casos_dengue[i]/dengue_notdesc_regi[i,indice]) *100000)
  incid=round(incid,2)
  #nomecol=paste0("CIR_incid_",ano)
  #dengue_notdesc_regi[i,nomecol]=incid
  dengue_notdesc_regi[i,"INCID"]=incid
  
}

#### criando base formato wide para incidencia e casos
library(reshape2)
data_incd_dengue<-dcast(dengue_notdesc_regi, Regiao.de.Saude ~ NU_ANO)
data_cases_dengue<-dcast(dengue_notdesc_regi, Regiao.de.Saude ~ NU_ANO, value.var = "CIR_casos_dengue")

names(data_incd_dengue)<-c("Regiao.de.Saude","incid_2016","incid_2017","incid_2018","incid_2019")
names(data_cases_dengue)<-c("Regiao.de.Saude","casos_2016","casos_2017","casos_2018","casos_2019")

data_cases_incd_dengue_2016_2019<-merge(data_cases_dengue,data_incd_dengue, by.x = "Regiao.de.Saude", by.y="Regiao.de.Saude",all.x = TRUE)


################ CHIK

for(i in 1:nrow(chik_notdesc_regi)){
  ano<-na.omit(chik_notdesc_regi$NU_ANO[i])
  indice = which(str_detect(colnames(chik_notdesc_regi), pattern = ano) == TRUE)
  incid= ((chik_notdesc_regi$CIR_casos_chik[i]/chik_notdesc_regi[i,indice]) *100000)
  incid=round(incid,2)
  #nomecol=paste0("CIR_incid_",ano)
  #chik_notdesc_regi[i,nomecol]=incid
  chik_notdesc_regi[i,"INCID"]=incid
  
}

#### criando base formato wide para incidencia e casos
library(reshape2)
data_incd_chik<-dcast(chik_notdesc_regi, Regiao.de.Saude ~ NU_ANO)
data_cases_chik<-dcast(chik_notdesc_regi, Regiao.de.Saude ~ NU_ANO, value.var = "CIR_casos_chik")

names(data_incd_chik)<-c("Regiao.de.Saude","incid_2016","incid_2017","incid_2018","incid_2019")
names(data_cases_chik)<-c("Regiao.de.Saude","casos_2016","casos_2017","casos_2018","casos_2019")

data_cases_incd_chik_2016_2019<-merge(data_cases_chik,data_incd_chik, by.x = "Regiao.de.Saude", by.y="Regiao.de.Saude",all.x = TRUE)

###################

save(zika_notdesc_regi, dengue_notdesc_regi, chik_notdesc_regi,
     data_cases_incd_zika_2016_2019, data_cases_incd_dengue_2016_2019, data_cases_incd_chik_2016_2019, file = 'dataset_arboviroses_2016_2019.RData')

#########################################################################

