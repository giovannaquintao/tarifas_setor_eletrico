library(tidyverse)

rm(list=ls())
gc()

address<-"C:/Users/giova/OneDrive/raw_data/ibge/pof/pof_17_18"

########################### open ################################################
DOMICILIO <-
  read.fwf(paste0(address,"/DOMICILIO.txt"),
           , widths = c(2,4,1,9,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,2,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,14,14,1
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "V0201", "V0202",
                           "V0203", "V0204", "V0205", "V0206", "V0207",
                           "V0208", "V0209", "V02101", "V02102",
                           "V02103", "V02104", "V02105", "V02111",
                           "V02112", "V02113", "V0212", "V0213",
                           "V02141", "V02142", "V0215", "V02161",
                           "V02162", "V02163", "V02164", "V0217",
                           "V0219", "V0220", "V0221", "PESO",
                           "PESO_FINAL", "V6199")
           , dec="."
  )
MORADOR <-
  read.fwf(paste0(address,"/MORADOR.txt"),
           , widths = c(2,4,1,9,2,1,2,2,1,2,2,4,3,1,1,
                        1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,
                        2,1,2,14,14,10,1,20,20,20,20)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE",
                           "V0306", "V0401", "V04021", "V04022", "V04023",
                           "V0403", "V0404", "V0405", "V0406", "V0407",
                           "V0408", "V0409", "V0410", "V0411", "V0412",
                           "V0413", "V0414", "V0415", "V0416",
                           "V041711", "V041712", "V041721", "V041722",
                           "V041731", "V041732", "V041741", "V041742",
                           "V0418", "V0419", "V0420", "V0421", "V0422",
                           "V0423", "V0424", "V0425", "V0426", "V0427",
                           "V0428", "V0429", "V0430", "ANOS_ESTUDO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL",
                           "NIVEL_INSTRUCAO", "RENDA_DISP_PC","RENDA_MONET_PC",
                           "RENDA_NAO_MONET_PC","DEDUCAO_PC"   )
           , dec="."
  )

DESPESA_COLETIVA <-
  read.fwf(paste0(address,"/DESPESA_COLETIVA.txt"),
           , widths = c(2,4,1,9,2,1,2,2,7,2,4,10,2,2,1
                        ,10,1,12,10,10,1,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9002", "V9005", "V8000",
                           "V9010", "V9011", "V9012", "V1904",
                           "V1905", "DEFLATOR", "V8000_DEFLA",
                           "V1904_DEFLA", "COD_IMPUT_VALOR",
                           "COD_IMPUT_QUANTIDADE", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )



DESPESA_INDIVIDUAL <- 
  read.fwf(paste0(address,"/DESPESA_INDIVIDUAL.txt"),
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V9002", "V8000", "V9010", "V9011", "V9012",
                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )   



CADERNETA_COLETIVA <-
  read.fwf(paste0(address,"/CADERNETA_COLETIVA.txt"),
           , widths = c(2,4,1,9,2,1,2,3,7,2,10,12,10,1,2,14,14,10,
                        9,4,5,9,5
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9002", "V8000", "DEFLATOR",
                           "V8000_DEFLA", "COD_IMPUT_VALOR",
                           "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL",
                           "V9005", "V9007", "V9009", "QTD_FINAL","V9004")
           , dec="."
  )



ALUGUEL_ESTIMADO <-
  read.fwf(paste0(address,"/ALUGUEL_ESTIMADO.txt"),
           , widths = c(2,4,1,9,2,1,2,7,2,10,2,2,12,10,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "V9001", "V9002", "V8000", "V9010", "V9011",
                           "DEFLATOR", "V8000_DEFLA", "COD_IMPUT_VALOR",
                           "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL")
           , dec="."
  )


################### cleaning ##########################################################

MORADOR_2<-MORADOR %>%
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  filter(V0306==1) %>%
  mutate(sexo=ifelse(V0404==1, "Homem", "Mulher")) %>%
  mutate(raca=case_when(
    V0405==1 ~ "Branca",
    V0405%in%c(2,4) ~ "Negro",
    V0405==3 ~ "Amarela",
    V0405==5 ~ "Indígena",
    TRUE ~ "Sem declaração"
  )
  ) %>%
  select(id_dom,id_uc,sexo,raca,ANOS_ESTUDO,RENDA_DISP_PC,NIVEL_INSTRUCAO,RENDA_TOTAL,PESO,PESO_FINAL)



DOMICILIO_2<-DOMICILIO %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  mutate(rede_geral=ifelse(V02141==1, "Sim", "Não")) %>%
  mutate(outra_origem=ifelse(V02142==1, "Sim", "Não")) %>%
  mutate(energia_integral=ifelse(V0215==1, "Sim", "Não")) %>%
  mutate(loc_dom=case_when(
    TIPO_SITUACAO_REG==1 ~ "Urbano",
    TIPO_SITUACAO_REG==2 ~ "Rural"
  )) %>%
  select(id_dom, rede_geral, outra_origem, energia_integral,loc_dom)



DESPESA_COLETIVA_2 <- DESPESA_COLETIVA %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    V8000_DEFLA_anual = V8000_DEFLA * FATOR_ANUALIZACAO,
    V8000_DEFLA_anual  = ifelse( QUADRO==10|QUADRO==19,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO), 
                           (V8000_DEFLA*FATOR_ANUALIZACAO)
    )
  ) %>%
  group_by(id_uc) %>%
  summarise(
    despesa_coletiva = sum(V8000_DEFLA_anual, na.rm = TRUE),
    .groups = "drop"
  )




DESPESA_INDIVIDUAL_2 <- DESPESA_INDIVIDUAL %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    V8000_DEFLA_anual = V8000_DEFLA * FATOR_ANUALIZACAO,
    V8000_DEFLA_anual = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO), 
                           (V8000_DEFLA*FATOR_ANUALIZACAO)
    )
  ) %>%
  group_by(id_uc) %>%
  summarise(
    despesa_individual = sum(V8000_DEFLA_anual, na.rm = TRUE),
    .groups = "drop"
  )

habitacao <- DESPESA_COLETIVA %>%
  filter(QUADRO%in%c(6:19)) %>% 
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    V8000_DEFLA_anual = V8000_DEFLA * FATOR_ANUALIZACAO,
    V8000_DEFLA_anual  = ifelse( QUADRO==10|QUADRO==19,
                                 (V8000_DEFLA*V9011*FATOR_ANUALIZACAO), 
                                 (V8000_DEFLA*FATOR_ANUALIZACAO)
  )) %>%
  group_by(id_uc) %>%
  summarise(
    habitacao = sum(V8000_DEFLA_anual, na.rm = TRUE),
    .groups = "drop"
  )

names(CADERNETA_COLETIVA)
CADERNETA_COLETIVA_2 <- CADERNETA_COLETIVA %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    V8000_DEFLA_anual = V8000_DEFLA * FATOR_ANUALIZACAO
    ,V8000_DEFLA_anual = V8000_DEFLA * FATOR_ANUALIZACAO
  ) %>%
  group_by(id_uc) %>%
  summarise(
    caderneta_coletiva = sum(V8000_DEFLA_anual, na.rm = TRUE),
    .groups = "drop"
  )
ALUGUEL_ESTIMADO_2<-ALUGUEL_ESTIMADO %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    V8000_DEFLA_anual = V8000_DEFLA * FATOR_ANUALIZACAO
    ,V8000_DEFLA_anual = V8000_DEFLA *V9011*FATOR_ANUALIZACAO
  ) %>%
  group_by(id_uc) %>%
  summarise(
    aluguel = sum(V8000_DEFLA_anual, na.rm = TRUE),
    .groups = "drop"
  )
base_final <- MORADOR_2 %>%
  left_join(DOMICILIO_2, by = "id_dom") %>%
  left_join(DESPESA_COLETIVA_2, by = "id_uc") %>%
  left_join(DESPESA_INDIVIDUAL_2, by = "id_uc") %>%
  left_join(habitacao, by = "id_uc") %>%
  left_join(CADERNETA_COLETIVA_2, by = "id_uc") %>%
  left_join(ALUGUEL_ESTIMADO_2, by = "id_uc")


names(base_final)
base_final<-base_final %>% 
  rowwise() %>%
  mutate(gastos_habitacao=sum(habitacao,aluguel,na.rm=T),
         gastos_totais=sum(despesa_coletiva,caderneta_coletiva,
                           aluguel,
                           despesa_individual,
                           na.rm=T))


weighted.mean(base_final$gastos_habitacao_2,weight=base_final$PESO_FINAL,na.rm=T)/12
weighted.mean(base_final$gastos_totais,weight=base_final$PESO_FINAL,na.rm=T)/12

weighted.mean(base_final$aluguel,weight=base_final$PESO_FINAL,na.rm=T)/12


salario_minimo <- 998  # valor do salário mínimo na POF 2017-2018

base_final <- base_final %>%
  mutate(
    homem_ref = sexo == "Homem",
    mulher_ref = sexo == "Mulher",
    negra_ref = raca == "Negro",
    homem_negro_ref = sexo == "Homem" & raca == "Negro",
    mulher_negra_ref = sexo == "Mulher" & raca == "Negro",
    renda_pc_ate_05 = RENDA_DISP_PC <= 0.5 * salario_minimo,
    renda_pc_05a3 = RENDA_DISP_PC > 0.5 * salario_minimo & RENDA_DISP_PC <= 3 * salario_minimo,
    renda_pc_mais3 = RENDA_DISP_PC > 3 * salario_minimo,
    rural = loc_dom == "Rural",
    urbano = loc_dom == "Urbano"
  )


colunas_dummy <- c(
  "homem_ref", "mulher_ref", "negra_ref", "homem_negro_ref", "mulher_negra_ref",
  "renda_pc_ate_05", "renda_pc_05a3", "renda_pc_mais3", "rural", "urbano"
)

stats <- lapply(colunas_dummy, function(var) {
  dados <- base_final %>% filter(.data[[var]] == TRUE)
  media <- weighted.mean(dados$gastos_totais, w = dados$PESO_FINAL, na.rm = TRUE)
  data.frame(tipo = var, media_gastos = media)
}) %>% bind_rows()
