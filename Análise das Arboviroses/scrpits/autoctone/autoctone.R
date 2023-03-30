#aux_pop$mun<-gsub("DIAS D'AVILA", "DIAS D AVILA", aux_pop$mun)


ano<-2019    

############# dengue###########

base_arb = fe_dengue_conf %>% dplyr::filter(!is.na(TPAUTOCTO) & NU_ANO == ano) %>% 
      group_by(ID_MN_RESI, TPAUTOCTO) %>% count() %>% rename('casos' = n) %>% 
      inner_join(aux_pop %>% dplyr::select(IBGE_6, mun, paste0('pop.', ano)), by = c('ID_MN_RESI' = 'IBGE_6')) %>% 
      rename(pop = paste0('pop.', ano))
    
base_arb = base_arb %>%  
  mutate('incid_auto' = casos,
         TPAUTOCTO = case_when(
           TPAUTOCTO == 1 ~ "SIM",
           TPAUTOCTO == 2 ~ "NAO", 
           TPAUTOCTO == 3 ~ "IND")) %>% 
  dplyr::select(!c(casos, pop)) %>% 
  pivot_wider(names_from = TPAUTOCTO, values_from = incid_auto, values_fill = 0)

    base_arb = base_arb %>%  
      mutate('incid_auto' = round(100000 * casos/pop, 3),
             TPAUTOCTO = case_when(
               TPAUTOCTO == 1 ~ "SIM",
               TPAUTOCTO == 2 ~ "NAO", 
               TPAUTOCTO == 3 ~ "IND")) %>% 
      dplyr::select(!c(casos, pop)) %>% 
      pivot_wider(names_from = TPAUTOCTO, values_from = incid_auto, values_fill = 0)
    
  ###############################
    dg_base_arb_2016 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
    
    dg_base_arb_2017 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
    
    dg_base_arb_2018 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
    
    dg_base_arb_2019 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
    
    dg_autoctone<-rbind(dg_base_arb_2016,dg_base_arb_2017,dg_base_arb_2018,dg_base_arb_2019)
    names(dg_autoctone)<-c("mun","ano","dg_auto","dg_NAO","dg_IND")
    
    
    ################ ZIKA
ano=2019

    base_arb = fe_zika_conf %>% dplyr::filter(!is.na(TPAUTOCTO) & NU_ANO == ano) %>% 
      group_by(ID_MN_RESI, TPAUTOCTO) %>% count() %>% rename('casos' = n) %>% 
      inner_join(aux_pop %>% dplyr::select(IBGE_6, mun, paste0('pop.', ano)), by = c('ID_MN_RESI' = 'IBGE_6')) %>% 
      rename(pop = paste0('pop.', ano))
  
    base_arb = base_arb %>%  
      mutate('incid_auto' = casos,
             TPAUTOCTO = case_when(
               TPAUTOCTO == 1 ~ "SIM",
               TPAUTOCTO == 2 ~ "NAO", 
               TPAUTOCTO == 3 ~ "IND")) %>% 
      dplyr::select(!c(casos, pop)) %>% 
      pivot_wider(names_from = TPAUTOCTO, values_from = incid_auto, values_fill = 0)
    
        
    base_arb = base_arb %>%  
      mutate('incid_auto' = round(100000 * casos/pop, 3),
             TPAUTOCTO = case_when(
               TPAUTOCTO == 1 ~ "SIM",
               TPAUTOCTO == 2 ~ "NAO", 
               TPAUTOCTO == 3 ~ "IND")) %>% 
      dplyr::select(!c(casos, pop)) %>% 
      pivot_wider(names_from = TPAUTOCTO, values_from = incid_auto, values_fill = 0)
    
    
    zk_base_arb_2016 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
    
    zk_base_arb_2017 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
      
    zk_base_arb_2018 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
    
    zk_base_arb_2019 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
    
    zk_autoctone<-rbind(zk_base_arb_2016,zk_base_arb_2017,zk_base_arb_2018,zk_base_arb_2019)
    
    names(zk_autoctone)<-c("mun","ano","zk_auto","zk_NAO","zk_IND")
    
  ################ CHIK
    ano=2017
    
    base_arb = fe_chik_conf %>% dplyr::filter(!is.na(TPAUTOCTO) & NU_ANO == ano) %>% 
      group_by(ID_MN_RESI, TPAUTOCTO) %>% count() %>% rename('casos' = n) %>% 
      inner_join(aux_pop %>% dplyr::select(IBGE_6, mun, paste0('pop.', ano)), by = c('ID_MN_RESI' = 'IBGE_6')) %>% 
      rename(pop = paste0('pop.', ano))
    
    base_arb = base_arb %>%  
      mutate('incid_auto' = casos,
             TPAUTOCTO = case_when(
               TPAUTOCTO == 1 ~ "SIM",
               TPAUTOCTO == 2 ~ "NAO", 
               TPAUTOCTO == 3 ~ "IND")) %>% 
      dplyr::select(!c(casos, pop)) %>% 
      pivot_wider(names_from = TPAUTOCTO, values_from = incid_auto, values_fill = 0)
    
    base_arb = base_arb %>%  
      mutate('incid_auto' = round(100000 * casos/pop, 3),
             TPAUTOCTO = case_when(
               TPAUTOCTO == 1 ~ "SIM",
               TPAUTOCTO == 2 ~ "NAO", 
               TPAUTOCTO == 3 ~ "IND")) %>% 
      dplyr::select(!c(casos, pop)) %>% 
      pivot_wider(names_from = TPAUTOCTO, values_from = incid_auto, values_fill = 0)
    
    
    chik_base_arb_2016 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
    
    ano=2017
    chik_base_arb_2017 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
    
    ano=2018
    chik_base_arb_2018 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
    
    ano=2019
    chik_base_arb_2019 = base_arb %>% 
      as.data.frame() %>% 
      dplyr::select(!c(ID_MN_RESI))%>%
      mutate('ano' = ano)%>%
      dplyr::select(mun,ano,SIM,NAO,IND)
    
    chik_autoctone<-rbind(chik_base_arb_2016,chik_base_arb_2017,chik_base_arb_2018,chik_base_arb_2019)
    names(chik_autoctone)<-c("mun","ano","chik_auto","chik_NAO","chik_IND")
    
    save(zk_autoctone,chik_autoctone,dg_autoctone,file="/cloud/project/binomial/data_arbo_autoc_att_vcasos.RData")
