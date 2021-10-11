library(dplyr)
library(readxl)
library(utils)
library(stringr)
library(data.table)
library(foreign)

# Dados populacionais Brasil
#http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/poptbr.def
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?popsvs/cnv/popbr.def # pop por sexo/idade

# Dados populacionais Bahia
# Podemos baixar os dados destas fontes, 
#http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/poptba.def
# http://www3.saude.ba.gov.br/cgi/tabcgi.exe?populacao/poprestcu.def
# ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPTCU/

# mas utilizei os dados da sei, criada a partir do levantamento pelo TCU
# G://sinan//SEI

# Producao ambulatorial
#http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sia/cnv/qaba.def

#pop_2016<-read.dbf("G:\\SINAN\\ftp.datasus.gov.brdisseminpublicosIBGEPOPTCU\\POPTBR16\\POPTBR16.dbf", as.is = FALSE)
#projpo_2016<- read.dbf("G:\\SINAN\\ftp.datasus.gov.brdisseminpublicosIBGEPOPTCU\\projbr16.dbf")

##############BIOMA

bioma<-read_excel("G:\\SINAN\\httpsbasedosdados.org\\mapbiomas(httpsmapbiomas.orgdownload)\\bioma_mun_ba_2014_2019_v2.xls")

# mantem no dataset informações unicas sobre o bioma.
# recupero o valor maior do bioma por ano.

# Calculo o valor de ocupação total do bioma no municipio
coluna<-unique(bioma$municipality)
bioma_agrupado_2014_2019<-data.frame()
for(i in 1:length(coluna)){
  #for(i in 1:1){
  cidade=coluna[i]
  data<-bioma[bioma$municipality==cidade,]
  aux<-""
  aux_2<-""
  all_bioma_2014<-aggregate(data$bioma_2014, by=list(municipio.tipo.bioma.2014=data$nivel_1), FUN=sum)%>%rename(area.total.bioma.2014=x)%>%arrange(desc(area.total.bioma.2014))
  all_bioma_2015<-aggregate(data$bioma_2015, by=list(municipio.tipo.bioma.2015=data$nivel_1), FUN=sum)%>%rename(area.total.bioma.2015=x)%>%arrange(desc(area.total.bioma.2015))
  all_bioma_2016<-aggregate(data$bioma_2016, by=list(municipio.tipo.bioma.2016=data$nivel_1), FUN=sum)%>%rename(area.total.bioma.2016=x)%>%arrange(desc(area.total.bioma.2016))
  all_bioma_2017<-aggregate(data$bioma_2017, by=list(municipio.tipo.bioma.2017=data$nivel_1), FUN=sum)%>%rename(area.total.bioma.2017=x)%>%arrange(desc(area.total.bioma.2017))
  all_bioma_2018<-aggregate(data$bioma_2018, by=list(municipio.tipo.bioma.2018=data$nivel_1), FUN=sum)%>%rename(area.total.bioma.2018=x)%>%arrange(desc(area.total.bioma.2018))
  all_bioma_2019<-aggregate(data$bioma_2019, by=list(municipio.tipo.bioma.2019=data$nivel_1), FUN=sum)%>%rename(area.total.bioma.2019=x)%>%arrange(desc(area.total.bioma.2019))
  aux<-cbind(all_bioma_2014,all_bioma_2015,all_bioma_2016,all_bioma_2017,all_bioma_2018,all_bioma_2019)
#  aux<-cbind(all_bioma_2016,all_bioma_2017[,-1],all_bioma_2018[,-1],all_bioma_2019[,-1])
#  names(aux)[1:5]<-c("tipo_bioma","all_bioma_2016","all_bioma_2017","all_bioma_2018","all_bioma_2019")
  tam<-nrow(aux)
  aux_2<-data[1:tam,1:2]
  aux<-cbind(aux,aux_2)%>%select(IBGE,municipality, everything())
  bioma_agrupado_2014_2019<-rbind(bioma_agrupado_2014_2019,aux)
  rm(cidade,data,aux_2,all_bioma_2014,all_bioma_2015,all_bioma_2016,all_bioma_2017,all_bioma_2018,all_bioma_2019,aux)
}


# seleciono o valor maior
coluna<-unique(bioma_agrupado_2014_2019$municipality)
bioma_selected<-data.frame()
for(i in 1:length(coluna)){
  cidade=coluna[i]
  df<-bioma_agrupado_2014_2019[bioma_agrupado_2014_2019$municipality==cidade,]
  bioma_2014<-df%>%slice_max(area.total.bioma.2014)%>%select(municipio.tipo.bioma.2014, area.total.bioma.2014)%>%rename(bioma_2014=municipio.tipo.bioma.2014)
  bioma_2015<-df%>%slice_max(area.total.bioma.2015)%>%select(municipio.tipo.bioma.2015, area.total.bioma.2015)%>%rename(bioma_2015=municipio.tipo.bioma.2015)
  bioma_2016<-df%>%slice_max(area.total.bioma.2016)%>%select(municipio.tipo.bioma.2016, area.total.bioma.2016)%>%rename(bioma_2016=municipio.tipo.bioma.2016)
  bioma_2017<-df%>%slice_max(area.total.bioma.2017)%>%select(municipio.tipo.bioma.2017, area.total.bioma.2017)%>%rename(bioma_2017=municipio.tipo.bioma.2017)
  bioma_2018<-df%>%slice_max(area.total.bioma.2018)%>%select(municipio.tipo.bioma.2018, area.total.bioma.2018)%>%rename(bioma_2018=municipio.tipo.bioma.2018)
  bioma_2019<-df%>%slice_max(area.total.bioma.2019)%>%select(municipio.tipo.bioma.2019, area.total.bioma.2019)%>%rename(bioma_2019=municipio.tipo.bioma.2019)
  bioma_2014_2019<-bind_cols(bioma_2014,bioma_2015,bioma_2016,bioma_2017,bioma_2018,bioma_2019)
  df<-df%>%select(IBGE,municipality)%>%distinct()
  bioma_2014_2019<-bind_cols(bioma_2014_2019,df) %>% select(IBGE, municipality, everything())
  bioma_selected<-bind_rows(bioma_selected,bioma_2014_2019)
  rm(cidade,df,bioma_2014,bioma_2015,bioma_2016,bioma_2017,bioma_2018,bioma_2019,df,bioma_2016_2019)
}

save(bioma,bioma_agrupado_2014_2019,bioma_selected, file="datasets_bioma.RData")

######################## INCIDENCIA
load("dataset_arboviroses_fase1.RData")

#Abordagem: filtrar anos 2016-2019

dengue_notdesc_16_19 <-  dengue_notdesc %>% filter(NU_ANO%in%c(2016,2017,2018,2019))
zika_notdesc_16_19 <-  zika_notdesc %>% filter(NU_ANO%in%c(2016,2017,2018,2019))
chik_notdesc_16_19 <-  chik_notdesc %>% filter(NU_ANO%in%c(2016,2017,2018,2019))

# #Abordagem: contagem casos por municipio

zika_notdesc_muni<- as.data.table(table(zika_notdesc_16_19$NU_ANO, zika_notdesc_16_19$MUN_RESI))%>%data.frame
chik_notdesc_muni<- as.data.table(table(chik_notdesc_16_19$NU_ANO, chik_notdesc_16_19$MUN_RESI))%>%data.frame
dengue_notdesc_muni<- as.data.table(table(dengue_notdesc_16_19$NU_ANO, dengue_notdesc_16_19$MUN_RESI))%>%data.frame

names(zika_notdesc_muni)<-c("NU_ANO","Mun","casos_zika")
names(chik_notdesc_muni)<-c("NU_ANO","Mun","casos_chik")
names(dengue_notdesc_muni)<-c("NU_ANO","Mun","casos_dengue")

####################################



### Abordagem: cálculo incidencia ano

require("stringr")
library(stringr)
require(abjutils)

################ ZIKA

aux_pop<-read_excel("G:\\SINAN\\Nova pasta\\municipiogeral.xlsx")

for(i in 1:nrow(aux_pop)){
#for(i in 1:1){
 r <-which(abjutils::rm_accent(toupper(zika_notdesc_muni$Mun))==aux_pop$mun[i])
 if(length(r)>=1) # executo se achar a cidade 
 {
   for (j in seq(1,length(r),1)){
     linha<-r[j]
     ano<-na.omit(zika_notdesc_muni$NU_ANO[linha])
     coluna=paste0("^pop.",ano)
     indice = which(str_detect(colnames(aux_pop), regex(coluna)) == TRUE)
     incid= ((zika_notdesc_muni$casos_zika[linha]/aux_pop[i,indice]) *100000)
     incid=round(incid,2)
     nomecol=paste0("ZK_incid_",ano)
     aux_pop[i,nomecol]=incid
   }
 }
 else{
   if(length(r)==0) next()    
 } 

}

for(i in 1:nrow(aux_pop)){
  #for(i in 1:1){
  r <-which(abjutils::rm_accent(toupper(chik_notdesc_muni$Mun))==aux_pop$mun[i])
  if(length(r)>=1) # executo se achar a cidade 
  {
    for (j in seq(1,length(r),1)){
      linha<-r[j]
      ano<-na.omit(chik_notdesc_muni$NU_ANO[linha])
      coluna=paste0("^pop.",ano)
      indice = which(str_detect(colnames(aux_pop), regex(coluna)) == TRUE)
      incid= ((chik_notdesc_muni$casos_chik[linha]/aux_pop[i,indice]) *100000)
      incid=round(incid,2)
      nomecol=paste0("chik_incid_",ano)
      aux_pop[i,nomecol]=incid
    }
  }
  else{
    if(length(r)==0) next()    
  } 
  
}

for(i in 1:nrow(aux_pop)){
  #for(i in 1:1){
  r <-which(abjutils::rm_accent(toupper(dengue_notdesc_muni$Mun))==aux_pop$mun[i])
  if(length(r)>=1) # executo se achar a cidade 
  {
    for (j in seq(1,length(r),1)){
      linha<-r[j]
      ano<-na.omit(dengue_notdesc_muni$NU_ANO[linha])
      coluna=paste0("^pop.",ano)
      indice = which(str_detect(colnames(aux_pop), regex(coluna)) == TRUE)
      incid= ((dengue_notdesc_muni$casos_dengue[linha]/aux_pop[i,indice]) *100000)
      incid=round(incid,2)
      nomecol=paste0("dg_incid_",ano)
      aux_pop[i,nomecol]=incid
    }
  }
  else{
    if(length(r)==0) next()    
  } 
  
}
################################ classificação do municipio - RURAL e URBANO
#2017: IBGE, propõe a discussão sobre os critérios utilizados na delimitação do território nacional. O objetivo do estudo é aprimorar a divulgação do Censo Demográfico de 2020 e oferecer 
#à sociedade avanços na diferenciação de áreas rurais e urbanas que possam servir de base para a otimização de políticas públicas e do planejamento privado
#O estudo apresenta uma classificação dos espaços rurais e urbanos por município e define critérios comuns para todo o país. São três critérios básicos para a elaboração dessa classificação: 
#a densidade demográfica, a localização em relação aos principais centros urbanos e o tamanho da população. Após a análise dos critérios, os municípios foram caracterizados como "urbanos", "rurais" ou "intermediários". A metodologia aplicada está alinhada a de organizações internacionais como a União Europeia,
#e a de países como os Estados Unidos, o que permite a comparabilidade dos resultados brasileiros.
#https://agenciadenoticias.ibge.gov.br/agencia-sala-de-imprensa/2013-agencia-de-noticias/releases/15003-ibge-propoe-debate-de-nova-classificacao-para-os-espacos-rurais-e-urbanos
#https://www.ibge.gov.br/geociencias/organizacao-do-territorio/tipologias-do-territorio/15790-classificacao-e-caracterizacao-dos-espacos-rurais-e-urbanos-do-brasil.html?=&t=downloads

dt<-read.dbf("G:\\SINAN\\httpswww.ibge.gov.brgeocienciasorganizacao-do-territoriotipologias-do-territorio15790-classificacao-e-caracterizacao-dos-espacos-rurais-e-urbanos-do-brasil.html=&t=downloads\\RurUrb\\RurUrb.dbf", as.is = FALSE)%>%filter(BaseRurU_3==29)%>%data.table()

summary(dt$Tipologia_)
levels(dt$Tipologia_)

case_character_type <- function(tipo) {
    ifelse(str_detect(tipo, pattern = 'intermediarioadjacente'),1,
           ifelse(str_detect(tipo, pattern = "intermediarioremoto"),2,
                  ifelse(str_detect(tipo, pattern = "ruraladjacente"),3,
                         ifelse(str_detect(tipo, pattern = "ruralremoto"),4,5)
                  )))
}


dt$newtipology<-case_character_type(tolower(dt$Tipologia_))
#########################################################
# UNINDO BASE POP COM BASE dt pelo codigo IBGE

aux_pop<-left_join(aux_pop,dt,by=c("IBGE_7"="BaseRurU_1"))
var_eliminadas<-c("BaseRurU_4","BaseRurUrb","BaseRurU_2","BaseRurU_3")
aux_pop<-aux_pop[,!names(aux_pop)%in%var_eliminadas]
aux_pop$OBJECTID<-seq(1,nrow(aux_pop))
aux_pop<-aux_pop%>%select(OBJECTID,everything())

colnames(aux_pop)[6:12]<-c("altitude","latitude","longitude","area_km","distancia.capital","semi.arido","regiao.de.saude")

aux_pop$semi.arido<-ifelse(str_detect(aux_pop$semi.arido, "X"),0,1)

library(dplyr)
library(tidyselect)
t<-aux_pop
#aux_pop<-t

# densidade populacional media para os 4 anos

ano<-c("2016","2017","2018","2019")
for(i in 1:nrow(aux_pop)){
  #for(i in 1:1){
      #variavel<-colnames(aux_pop)[str_detect(colnames(aux_pop),regex('^pop', ignore_case = TRUE))]
      for(k in ano){
        coluna=paste0("^pop.",k)
        indice = which(str_detect(colnames(aux_pop), regex(coluna)) == TRUE)
        dns<-((aux_pop[i,indice]/aux_pop$area_km[i]))
        dns<-round(dns,2)
        nomecol=paste0("densidade_",k)
        aux_pop[i,nomecol]=dns
      }
  coluna1=paste0("dens_mean")
  coluna2=paste0("dens_median")
  #aux<-tidyselect::vars_select(names(aux_pop), starts_with('densidade_', ignore.case = TRUE)) 
  aux_pop[i,coluna1]<-mean(aux_pop$densidade_2016[i], aux_pop$densidade_2017[i],aux_pop$densidade_2018[i],aux_pop$densidade_2019[i], na.rm=TRUE)
  aux_pop[i,coluna2]<-median(aux_pop$densidade_2016[i], aux_pop$densidade_2017[i],aux_pop$densidade_2018[i],aux_pop$densidade_2019[i], na.rm=TRUE)
  }



#######################################################
# base mandada por Márcio não será utilizada###
#loc<-read_excel("G:\\SINAN\\egestor(httpsegestorab.saude.gov.brpaginasacessoPublicorelatoriosrelHistoricoCoberturaAB.xhtml)\\municipio\\Localidades.xlsx")
#var<-c("NM_MUNICIP","TIPO","NM_CATEGOR","CD_CATEGOR")
#var_interesse<-c("NM_MUNICIP","TIPO")
#loc<-loc[,var_interesse]
#loc$NM_MUNICIP<-abjutils::rm_accent(toupper(loc$NM_MUNICIP))

#data_POP<-left_join(aux_pop,loc,by=c("mun"="NM_MUNICIP"))

#DATA<-data_POP%>%select(mun,TIPO)%>%group_by(mun,TIPO)%>%dplyr::summarize(n())%>%ungroup()
#names(DATA)<-c("mun","TIPO","n")

# seleciono o valor maior
#coluna<-unique(DATA$mun)
#data_tipo<-data.frame()
#for(i in 1:length(coluna)){
#for(i in 1:2){
#  cidade=coluna[i]
#  aux_loc<-DATA[DATA$mun==cidade,]%>%slice_max(n)
#  if (nrow(aux_loc)>1) aux_loc<-aux_loc%>%slice_head()
#  data_tipo<-bind_rows(data_tipo,aux_loc)
 # rm(cidade,df,bioma_2014,bioma_2015,bioma_2016,bioma_2017,bioma_2018,bioma_2019,df,bioma_2016_2019)
#}



#rm(DATA)

########### COBERTURA AB
#https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/relHistoricoCoberturaAB.xhtml
cobertura_ab<-read.csv("G:\\SINAN\\egestor(httpsegestorab.saude.gov.brpaginasacessoPublicorelatoriosrelHistoricoCoberturaAB.xhtml)\\municipio\\Cobertura_AB_mun_2016_2019.csv", sep=";")
cobertura_ab$Mun<-abjutils::rm_accent(cobertura_ab$Mun)
variaveis.freq <- colnames(cobertura_ab)
variaveis.interesse = c(
  colnames(cobertura_ab[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'Mun')],
  colnames(cobertura_ab[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'CoberturaAB')])

cobertura_ab<-cobertura_ab[,variaveis.interesse]

#funcao para retirar % da variavel percent.AB
formata_percent <- function(num) {
  valor <-c("")
  tam<-nchar(as.character(num))
  #valor<-ifelse(str_detect(num, "%"),str_extract(num,'[1-9]'),num)
  #valor<-gsub('.{2}$', '', name)
  #valor<-grep("[1-9]",num,value=TRUE)
  valor<-substr(num,1,tam-1)
  valor<-gsub(',', '.', valor)

  valor<-as.numeric(valor)
  return(valor)
}

cobertura_ab["CoberturaAB_2018"] <-formata_percent(cobertura_ab$CoberturaAB_2018)
cobertura_ab["CoberturaAB_2016"] <-formata_percent(cobertura_ab$CoberturaAB_2016)
cobertura_ab["CoberturaAB_2017"] <-formata_percent(cobertura_ab$CoberturaAB_2017)
cobertura_ab["CoberturaAB_2019"] <-formata_percent(cobertura_ab$CoberturaAB_2019)

#cobertura_ab$cobertura_media<-median(cobertura_ab$CoberturaAB_2016,cobertura_ab$CoberturaAB_2017,cobertura_ab$CoberturaAB_2018,cobertura_ab$CoberturaAB_2019, na.rm=TRUE)
cobertura_ab$percent_medio_ab<-round((cobertura_ab$CoberturaAB_2016+cobertura_ab$CoberturaAB_2017+cobertura_ab$CoberturaAB_2018+cobertura_ab$CoberturaAB_2019)/4,2)
variaveis.freq <- colnames(cobertura_ab)
variaveis.interesse = c(
  colnames(cobertura_ab[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'Mun')],
  colnames(cobertura_ab[, variaveis.freq])[str_detect(variaveis.freq, pattern = 'percent_')])

cobertura_ab<-cobertura_ab[,variaveis.interesse]

########################## AUX_POP + COBERTURA_AB

aux_pop<-left_join(aux_pop,cobertura_ab,by=c("mun"="Mun"))

##########################

bioma_selected$municipality<-toupper(abjutils::rm_accent(bioma_selected$municipality))

data_ab_bioma<-left_join(cobertura_ab,bioma_selected,by=c("Mun"="municipality"))

######################## 
#IFDM<-read_excel("G:\\SINAN\\httpswww_firjan.com.brifdmdownloads\\IFDM BA.xlsx", sheet="data")%>%arrange(mun)%>%select(!(UF)) # elimino coluna UF
IFDM<-read_excel("G:\\SINAN\\httpswww_firjan.com.brifdmdownloads\\IFDM BA.xlsx", sheet="data")%>%arrange(mun)%>%select(mun,IFDM_geral_2016) # elimino coluna UF
IFDM$mun<-toupper(abjutils::rm_accent(IFDM$mun))

IFDM$IFDM_geral_2016<-ifelse(str_detect(IFDM$IFDM_geral_2016, "ND"),NA,IFDM$IFDM_geral_2016)
#IFDM$IFDM_emprego.renda_2016<-ifelse(str_detect(IFDM$IFDM_emprego.renda_2016, "ND"),NA,IFDM$IFDM_emprego.renda_2016)
#IFDM$IFDM_educacao_2016<-ifelse(str_detect(IFDM$IFDM_educacao_2016, "ND"),NA,IFDM$IFDM_educacao_2016)
#IFDM$IFDM_saude_2016<-ifelse(str_detect(IFDM$IFDM_saude_2016, "ND"),NA,IFDM$IFDM_saude_2016)
############################### IFDM x AUX_POP
aux_pop<-left_join(aux_pop,IFDM,by=c("mun"="mun"))

########## PRODUTO INTERNO BRUTO MUNICIPAL
#PIB obtido no site do SEI
#https://www.sei.ba.gov.br/index.php?option=com_content&view=article&id=561&Itemid=335
#PIB_2017(R$ milhões)
#PIB_Per_Capita_2017 (R$1,00)
#seleciono dados sobre o ano 2017

PIB<-read_excel("G:\\SINAN\\httpswww.sei.ba.gov.brindex.phpoption=com_content&view=article&id=561&Itemid=308\\pib_2018.xls",sheet = "dados")
#PIB<-PIB[,str_detect(colnames(PIB),regex('^Mun|_2018', ignore_case = TRUE))]
colnames(PIB)<-c("mun","PIB_2018","PIB_percapita_2018")
PIB$mun<-toupper(abjutils::rm_accent(PIB$mun))



data_PIB_IFDM<-left_join(PIB,IFDM,by=c("mun"="mun"))

#data_PIB_IFDM[is.na(data_PIB_IFDM)]= 0

###########################################
# variaveis de interesse
# G06A	População urbana residente do(s) município(s) com abastecimento de água/Habitantes
# G06B	População urbana residente do(s) município(s) com esgotamento sanitário/	Habitantes
# G12A	População total residente do(s) município(s) com abastecimento de água, segundo o IBGE/	Habitantes
# G12B	População total residente do(s) município(s) com esgotamento sanitário, segundo o IBGE/	Habitantes
# AG001	População total atendida com abastecimento de água	Habitantes

SNIS<- read_excel("G:\\SINAN\\httpwww.snis.gov.brdownloadssh_coleta\\Base_CNJ_SH_Municipios_Dados.xlsx", sheet="Dados_2010_a_2018")
#SNIS<-SNIS[,str_detect(colnames(SNIS),regex('^ano|_est|_mun|_2017|G06A|G06B|G12A|G12B|AG001', ignore_case = TRUE))]
SNIS<-SNIS[,str_detect(colnames(SNIS),regex('^ano|_est|_mun|G06A|G06B|G12A|G12B|AG001', ignore_case = TRUE))]
SNIS<-filter(SNIS,sgl_est=="BA"& ano_ref=="2015")%>%select(!(c("ano_ref","sgl_est","nom_est"))) # elimino coluna UF
colnames(SNIS)<-c("IBGE_6","mun","SNIS_G06A_2015","SNIS_G06B_2015","SNIS_G12A_2015","SNIS_G12B_2015","SNIS_AG001_2015")
SNIS$mun<-toupper(abjutils::rm_accent(SNIS$mun))

SNIS<-SNIS[,str_detect(colnames(SNIS),regex('^mun|SNIS_G12A_2015|SNIS_G12B_2015', ignore_case = TRUE))]

#SNIS[is.na(SNIS)] = 0
############################### IFDM x AUX_POP

aux_pop<-left_join(aux_pop,SNIS,by=c("mun"="mun"))

###############################

grupo.2<-"Barreiras"
grupo.3<-c("Feira de Santana","Camaçari")
grupo.5<-"Salvador"

aux_pop<-aux_pop%>%mutate(grupo.SES.2014 = case_when(regiao.de.saude %in% grupo.2 ~ '2',
                                                     regiao.de.saude %in% grupo.3 ~ '3',
                                                     regiao.de.saude %in% grupo.5 ~ '5'
                                    ,TRUE ~ '1'))

names(aux_pop)[104]<-"grupo_SES_CRI_2014"
####################################### ÍNDICES VETORIAIS
# MEDIA CICLO

aux_pop$mediaciclo22<-round((aux_pop$IND22_CICLO_2016+aux_pop$IND22_CICLO_2017+aux_pop$IND22_CICLO_2018+aux_pop$IND22_CICLO_2019)/4,2)

names(aux_pop)
###################################### INSERINDO BIOMA ("bioma_2014")
load("datasets_bioma.RData")

aux_pop<-left_join(aux_pop,bioma_selected,by=c("IBGE_7"="IBGE"))

###################################### BASE PARA ICC

var_interesse<-c("OBJECTID","mun","semi.arido","newtipology","dens_mean","percent_medio_ab","IFDM_geral_2016","SNIS_G12A_2015","SNIS_G12B_2015","mediaciclo22","bioma_2014","grupo_SES_CRI_2014", 
                 "ZK_incid_2016","ZK_incid_2017","ZK_incid_2018","ZK_incid_2019","chik_incid_2016","chik_incid_2017","chik_incid_2018","chik_incid_2019",
                 "dg_incid_2016","dg_incid_2017","dg_incid_2018","dg_incid_2019"
                 )
data_geral<-aux_pop[,var_interesse]

#######################################


#################################################
# Dataset multinivel municipal geral

data_multinivel_mun<-left_join(aux_pop,data_geral,by=c("mun"="Mun"))


#################################################

#ttadap
#coam


library(haven)
library(foreign)
names(data_geral) <- str_replace_all(names(data_geral), pattern = "\\.", replacement = "_") # to replace illegal character
write_dta(data_geral, "data_multinivel_mun.dta")

save(aux_pop,file="datasets_municipio.RData")


########################################### CODEBOOK
# biblioteca <sjPlot>
library(sjPlot)
view_df(data_geral, show.type = TRUE, show.values = FALSE, show.labels = FALSE,
        CSS = list(css.table = "border: 2px solid;",
                   css.tdata = "border: 1px solid;",
                   css.arc = "color:blue;"))

library(summarytools)

report.title = "-------- Sumário base Multinivel 22.09.2021 --------"
st_options(footnote = report.title)
sumario_SMS<-dfSummary(data_geral, style='multiline', graph.col = FALSE)
print(sumario_SMS, file = 'G://backup_maira//SINAN//analisebruno//sumario_datageral_22_09.html')


#######################################
view_df(data_multinivel_mun_long, show.type = TRUE, show.values = FALSE, show.labels = FALSE,
        CSS = list(css.table = "border: 2px solid;",
                   css.tdata = "border: 1px solid;",
                   css.arc = "color:blue;"))

report.title = "-------- Sumário base Multinivel 22.09.2021 --------"
st_options(footnote = report.title)
sumario_SMS<-dfSummary(data_multinivel_mun_long, style='multiline', graph.col = FALSE)
print(sumario_SMS, file = 'G://backup_maira//SINAN//analisebruno//sumario_data_multinivel_mun_long_22_09.html')

