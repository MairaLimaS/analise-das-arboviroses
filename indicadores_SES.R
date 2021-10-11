#carregar 
setwd("G:/backup_maira/SINAN/analisebruno")
#load("dataset_arboviroses_2016_2019.RData") # contêm datasets de todos casos suspeitos e confirmados e dataset de incidência. Dados separardos por arbo  
load("dataset_HLM_v2.RData") #dados com cobertura ab, incidencia e bioma

library(dplyr)
library(readxl)
library(utils)


# selecionamos
# TXMOINF	Taxa de mortalidade infantil
#TXBRUTAMORT	Taxa bruta de mortalidade 
#PBF	Percentual de pessoas inscritas no Cadastro Único que recebem Bolsa Família
#SNIS_PAGUA	Percentual da população urbana residente em domicílios ligados à rede de abastecimento de água
#SNIS_PESGOTO Percentual da população urbana residente em domicílios ligados à rede de esgotamento sanitário
#SNIS_PESGTRA	Percentual de esgoto tratado
#SNIS_PCOLSEl	Existência de coleta seletiva
#SNIS_CDI
#Percentual da população urbana atendida por serviços regulares de coleta de resíduos domiciliares
#PFOCOS	Concentração dos focos de calor
#PFLORA	Percentual de cobertura vegetal natural


dataset <- read_excel("G:\\SINAN\\httpsbasedosdados.org\\mapadesenvolvimentohumano\\Download Registros Administrativos\\DOWNLOAD REGISTRO ADMINISTRATIVO TOTAL 2012 A 2017.xlsx", sheet="MUNICÍPIO")%>%select(IBGE7,NOME,TXMOINF,TXBRUTAMORT,SNIS_PAGUA,SNIS_PESGOTO,SNIS_PESGTRA,SNIS_PCOLSEL,SNIS_CDI,PFOCOS,PFLORA)
names(dataset)<-c("IBGE7","mun","TXMOINF.2017","TXBRUTAMORT.2017","SNIS_PAGUA.2017","SNIS_PESGOTO.2017","SNIS_PESGTRA.2017","SNIS_PCOLSEL.2017","SNIS_CDI.2017","PFOCOS.2017","PFLORA.2017")

#dataset<-dataset[str_detect(as.character(dataset$IBGE7),pattern='^290'),]

########### DADOS REGIAO SAUDE
#Distribuicao das regioes de saude segundo os 5 grupos socioeconomicos
#Fonte: https://indicadores.resbr.net.br/view/index.php?uf=29&indicador=grupo_socio
#Grupo 1	
#Baixo desenvolvimento socioeconômico e baixa oferta de serviços	
#Grupo 2
#Médio/alto desenvolvimento socioeconômico e baixa oferta de serviços	Médio desenvolvimento socioeconômico e média/alta oferta de serviços	
#Grupo 3
#Médio desenvolvimento socioeconômico e média/alta oferta de serviços
#Grupo 4
#Alto desenvolvimento socioeconômico e média oferta de serviços	Alto desenvolvimento socioeconômico e alta oferta de serviços
#Grupo 5
#Alto desenvolvimento socioeconômico e alta oferta de serviços


#Barreiras Grupo 2
#Feira de Santana, Camaçari Grupo 3
# Salvador Grupo 5

regional<-read_excel("G:\\SINAN\\Nova pasta\\cod_regionalizacao_ba.xlsx")


grupo.2<-"Barreiras"
grupo.3<-c("Feira de Santana","Camaçari")
grupo.5<-"Salvador"

regional<-regional %>%  mutate(cod.regiao.saude= case_when(str_detect(Regiao.de.Saude, pattern = 'Alago') ~'1',
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


#regional<-regional %>% relocate(cod.regiao.saude, .after = CIR_IBGE)%>%relocate(grupo.SES.2014,.after=territorio.identidade)%>%arrange(mun)


#data_2017<-merge(dataset,regional,by.x = "IBGE7", by.y="IBGE_7",all.x = FALSE)
#data_2017<-data_2017[data_2017$ANO==2017,]

# Abro mão do dado coletado no atlas (data 2)
#SES<-read_excel("G:\\SINAN\\Nova pasta\\data_SES.xlsx", sheet="data")

##########################################
# abro dado  IFDM - Indice FIRJAN de Desenvolvimento Municipal - 
# estudo do Sistema FIRJAN que acompanha anualmente o desenvolvimento socioeconômico de todos os mais de 5 mil municípios brasileiros
# em três áreas de atuação: Emprego & renda, Educação e Saúde. Criado em 2008, ele é feito, 
# exclusivamente, com base em estatísticas públicas oficiais, disponibilizadas pelos ministérios do Trabalho, Educação e Saúde.
# o índice varia de 0 (mínimo) a 1 ponto (máximo) para classificar o nível de cada localidade em quatro categorias: 
#baixo (de 0 a 0,4), regular (0,4 a 0,6), moderado (de 0,6 a 0,8) e alto (0,8 a 1) desenvolvimento. 
#Ou seja, quanto mais próximo de 1, maior o desenvolvimento da localidade.

IFDM<-read_excel("G:\\SINAN\\httpswww_firjan.com.brifdmdownloads\\IFDM BA.xlsx", sheet="data")%>%arrange(mun)%>%select(!(UF)) # elimino coluna UF
IFDM$mun<-toupper(IFDM$mun) # municipio em capslock

############################## IPEA BAHIA BOLSA FAMILIA
# Dados coletados de http://www.ipeadata.gov.br/Default.aspx
# Programa Bolsa Família (PBF) - valor total dos benefícios (R$) em dezembro

IPEA_PBF<-data.table::fread("G:\\SINAN\\ipeadata\\ipeadata_PBF.csv",sep=",", quote = "",na.strings = "NA")%>%data.frame()
names(IPEA_PBF)<-c("UF","IBGE","micro.regiao","PBF_2011","PBF_2012","PBF_2013","PBF_2014","PBF_2016")

# dados agrupados por micro.regioes
IPEA_PBF<-IPEA_PBF%>%select("IBGE","micro.regiao","PBF_2016")

#############
#PIB<-data.table::fread("G:\\SINAN\\Nova pasta\\Produto Interno Bruto dos Municípios.csv",sep=";", na.strings = "NA")%>%data.frame()%>%select("Localidade", "X2017")
#PIB<-aggregate(. ~ Localidade, data=PIB,FUN=sum)%>%rename(PIB.2017=X2017)

########## PRODUTO INTERNO BRUTO MUNICIPAL
#PIB obtido no site do SEI
#https://www.sei.ba.gov.br/index.php?option=com_content&view=article&id=561&Itemid=335
#PIB_2017(R$ milhões)
#PIB_Per_Capita_2017 (R$1,00)
#seleciono dados sobre o ano 2017

PIB<-read_excel("G:\\SINAN\\httpswww.sei.ba.gov.brindex.phpoption=com_content&view=article&id=561&Itemid=308\\pib_2017.xls")
PIB<-PIB[,str_detect(colnames(PIB),regex('^Mun|_2017', ignore_case = TRUE))]
colnames(PIB)<-c("mun","PIB_2017","PIB_percapita_2017")
PIB$mun<-toupper(PIB$mun) # municipio em capslock

#PIB_regional<-aggregate(PIB$PIB_2017, by=list(municipio=PIB$mun), FUN=sum)


########## Sistema Nacional de Informações sobre Saneamento [municipal]
# SNIS obtido no site 
# http://www.snis.gov.br/downloads/
# seleciono dados sobre o ano 2017

# variaveis de interesse
# G06A	População urbana residente do(s) município(s) com abastecimento de água/Habitantes
# G06B	População urbana residente do(s) município(s) com esgotamento sanitário/	Habitantes
# G12A	População total residente do(s) município(s) com abastecimento de água, segundo o IBGE/	Habitantes
# G12B	População total residente do(s) município(s) com esgotamento sanitário, segundo o IBGE/	Habitantes
# AG001	População total atendida com abastecimento de água	Habitantes


SNIS<- read_excel("G:\\SINAN\\httpwww.snis.gov.brdownloadssh_coleta\\Base_CNJ_SH_Municipios_Dados.xlsx", sheet="Dados_2010_a_2018")
SNIS<-SNIS[,str_detect(colnames(SNIS),regex('^ano|_est|_mun|_2017|G06A|G06B|G12A|G12B|AG001', ignore_case = TRUE))]
SNIS<-filter(SNIS,sgl_est=="BA"& ano_ref=="2017")%>%select(!(c("ano_ref","sgl_est","nom_est"))) # elimino coluna UF
colnames(SNIS)<-c("IBGE_6","mun","SNIS_G06A_2017","SNIS_G06B_2017","SNIS_G12A_2017","SNIS_G12B_2017","SNIS_AG001_2017")
SNIS$mun<-toupper(SNIS$mun) # municipio em capslock

################## Agrupando dados
# 1 - IFDM

data_2017<-merge(regional,IFDM,by.x = "mun", by.y="mun",all.x = TRUE)

#2 - PIB

data_2017<-merge(data_2017,PIB,by.x = "mun", by.y="mun",all.x = TRUE)

# 3- SNIS
data_2017<-merge(data_2017,SNIS,by.x = "IBGE_6", by.y="IBGE_6",all.x = TRUE)

data_2017<-data_2017%>%select(!(c("mun.y")))


# 4 - dataset
#ibge<-data_2017$IBGE_7
#aux<-dataset[as.character(dataset$IBGE7)%in% ibge,]
#aux<-merge()

######################################## AGREGAR POR REGIAO SAUDE

data_2017<-data_2017%>%select(IBGE_6,IBGE_7,mun.x,Regiao.de.Saude,cod.regiao.saude,CIR_IBGE,everything())

# seleciono as variaveis de interesse

data_2017_final<-data_2017%>%select(!(c("IBGE_6","IBGE_7","CIR_IBGE","macrorregiao","territorio.identidade")))%>%rename(mun="mun.x")

var.IFDM<-colnames(data_2017_final)[str_detect(colnames(data_2017_final),regex('^IFDM_', ignore_case = TRUE))]

library(magrittr)

dta.IFDM<-data.table()

for (var in var.IFDM){ 
  aux<-""
  cod<-ifelse(str_detect(data_2017_final[, var], "ND"),NA,data_2017_final[, var])
  data_2017_final[, var]<-as.numeric(cod)
  aux<-data_2017_final%>%select(Regiao.de.Saude,var)
  aux<-aux%>%aggregate(. ~ Regiao.de.Saude, data = ., FUN =sum)
  colnames(aux)<-c("Regiao.de.Saude",paste0("CIR_",var))
  nome<-paste0("CIR_",var)
  dta.IFDM[,"Regiao.de.Saude"]<-aux[,"Regiao.de.Saude"]
  dta.IFDM[,nome]<-aux[,nome]
  rm(aux,nome)
}
rm(var.IFDM)

var.PIB<-colnames(data_2017_final)[str_detect(colnames(data_2017_final),regex('^PIB_', ignore_case = TRUE))]
dta.PIB<-data.table()

for (var in var.PIB){ 
  aux<-""
  cod<-ifelse(str_detect(data_2017_final[, var], "ND"),NA,data_2017_final[, var])
  data_2017_final[, var]<-as.numeric(cod)
  aux<-data_2017_final%>%select(Regiao.de.Saude,var)
  aux<-aux%>%aggregate(. ~ Regiao.de.Saude, data = ., FUN =sum)
  colnames(aux)<-c("Regiao.de.Saude",paste0("CIR_",var))
  nome<-paste0("CIR_",var)
  dta.PIB[,"Regiao.de.Saude"]<-aux[,"Regiao.de.Saude"]
  dta.PIB[,nome]<-aux[,nome]
  rm(aux,nome)
}
rm(var.PIB)

var.SNIS<-colnames(data_2017_final)[str_detect(colnames(data_2017_final),regex('^SNIS_', ignore_case = TRUE))]
dta.SNIS<-data.table()

for (var in var.SNIS){ 
  aux<-""
  cod<-ifelse(str_detect(data_2017_final[, var], "ND"),NA,data_2017_final[, var])
  data_2017_final[, var]<-as.numeric(cod)
  aux<-data_2017_final%>%select(Regiao.de.Saude,var)
  aux<-aux%>%aggregate(. ~ Regiao.de.Saude, data = ., FUN =sum)
  colnames(aux)<-c("Regiao.de.Saude",paste0("CIR_",var))
  nome<-paste0("CIR_",var)
  dta.SNIS[,"Regiao.de.Saude"]<-aux[,"Regiao.de.Saude"]
  dta.SNIS[,nome]<-aux[,nome]
  rm(aux,nome)
}
rm(var.SNIS)

# retiro dos datasets a regiao de saude
dta.2017<-data_2017_final[,2:4]%>%distinct()%>%arrange(Regiao.de.Saude)

dta.2017<-cbind(dta.2017,dta.IFDM[,-1],dta.PIB[,-1],dta.SNIS[,-1])

save(data_2017_final,data_2017, file="dataset_IFDM_PIB_SNIS.RData")
save(dta.2017,file="dataset_HLM_v4.RData")
