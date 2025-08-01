
library(tidyverse)
library(survey)
library(kableExtra)

rm(list=ls())
gc()
DOMICILIO <- readRDS("data/clean/DOMICILIO.rds")
MORADOR <- readRDS("data/clean/MORADOR.rds")
DESPESA_COLETIVA <- readRDS("data/clean/DESPESA_COLETIVA.rds")
DESPESA_INDIVIDUAL <- readRDS("data/clean/DESPESA_INDIVIDUAL.rds")
CADERNETA_COLETIVA <- readRDS("data/clean/CADERNETA_COLETIVA.rds")
ALUGUEL_ESTIMADO <- readRDS("data/clean/ALUGUEL_ESTIMADO.rds")
OUTROS_RENDIMENTOS <- readRDS("data/clean/OUTROS_RENDIMENTOS.rds")
RENDIMENTO_TRABALHO <- readRDS("data/clean/RENDIMENTO_TRABALHO.rds")

################### cleaning ##########################################################

MORADOR_2<-MORADOR %>%
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  filter(V0306==1) %>%
  mutate(sexo=ifelse(V0404==1, "Homem", "Mulher")) %>%
  mutate(raca=case_when(
    V0405==1 ~ "Branco",
    V0405%in%c(2,4) ~ "Negro",
    V0405==3 ~ "Amarela",
    V0405==5 ~ "Indígena",
    TRUE ~ "Sem declaração"
  )
  ) %>%
  select(id_dom,id_uc,sexo,raca,COD_UPA,ESTRATO_POF,
         ANOS_ESTUDO,RENDA_DISP_PC,NIVEL_INSTRUCAO,RENDA_TOTAL,PESO,PESO_FINAL)



DOMICILIO_2<-DOMICILIO %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  mutate(rede_geral=ifelse(V02141==1, "Sim", "Não")) %>%
  mutate(outra_origem=ifelse(V02142==1, "Sim", "Não")) %>%
  mutate(energia_integral=ifelse(V0215==1, "Sim", "Não")) %>%
  mutate(loc_dom=case_when(
    TIPO_SITUACAO_REG==1 ~ "Urbano",
    TIPO_SITUACAO_REG==2 ~ "Rural"
  )) %>%
  select(id_dom, UF,rede_geral, outra_origem, energia_integral,loc_dom)



DESPESA_COLETIVA_2 <- DESPESA_COLETIVA %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    V8000_DEFLA_anual = V8000_DEFLA * FATOR_ANUALIZACAO,
    V8000_DEFLA_anual  = ifelse( QUADRO==10|QUADRO==19,
                                 (V8000_DEFLA*V9011*FATOR_ANUALIZACAO), 
                                 (V8000_DEFLA*FATOR_ANUALIZACAO)
    ),
    inss_anual=(V1904_DEFLA*V9011*FATOR_ANUALIZACAO)) %>%
  group_by(id_uc) %>%
  summarise(
    despesa_coletiva = sum(V8000_DEFLA_anual, na.rm = TRUE),
    despesa_energia=sum(V8000_DEFLA_anual[V9001==600101],na.rm=T),
    despesa_gas=sum(V8000_DEFLA_anual[V9001==700101],na.rm=T),
    inss_anual = sum(inss_anual, na.rm = TRUE),
    quantidade_kws=sum(V9005,na.rm=T),
    quantidade_kws_NI=sum(V9005[COD_IMPUT_QUANTIDADE==0],na.rm=T),
    .groups = "drop"
  ) 


DESPESA_INDIVIDUAL_2 <- DESPESA_INDIVIDUAL %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    V8000_DEFLA_anual = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50,
                                (V8000_DEFLA*V9011*FATOR_ANUALIZACAO), 
                                (V8000_DEFLA*FATOR_ANUALIZACAO)
                                
    ),
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
    contas_casa = sum(V8000_DEFLA_anual, na.rm = TRUE),
    .groups = "drop"
  )


CADERNETA_COLETIVA_2 <- CADERNETA_COLETIVA %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    V8000_DEFLA_anual = V8000_DEFLA * FATOR_ANUALIZACAO
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
    ,V8000_DEFLA_anual = V8000_DEFLA *V9011*FATOR_ANUALIZACAO
  ) %>%
  group_by(id_uc) %>%
  summarise(
    aluguel = sum(V8000_DEFLA_anual, na.rm = TRUE),
    .groups = "drop"
  )



OUTROS_RENDIMENTOS_2<-OUTROS_RENDIMENTOS %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    ,   deducao_anual = ifelse( QUADRO==54,
                                (V8501_DEFLA*V9011*FATOR_ANUALIZACAO), 
                                (V8501_DEFLA*FATOR_ANUALIZACAO)
    ) 
  ) %>%
  group_by(id_uc) %>%
  summarise(
    deducao_anual = sum(deducao_anual, na.rm = TRUE),
    .groups = "drop"
  )


RENDIMENTO_TRABALHO_2 <- RENDIMENTO_TRABALHO %>%
  
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    ,  prev_pub_anual=(V531112_DEFLA*V9011*FATOR_ANUALIZACAO),
    imp_renda_anual=(V531122_DEFLA*V9011*FATOR_ANUALIZACAO),
    iss_anual=(V531132_DEFLA*V9011*FATOR_ANUALIZACAO)
  ) %>%
  group_by(id_uc) %>%
  summarise(
    prev_pub_anual = sum( prev_pub_anual, na.rm = TRUE),
    imp_renda_anual = sum( imp_renda_anual, na.rm = TRUE),
    iss_anual = sum( iss_anual, na.rm = TRUE),
    .groups = "drop"
  )

base_final <- MORADOR_2 %>%
  left_join(DOMICILIO_2, by = "id_dom") %>%
  left_join(DESPESA_COLETIVA_2, by = "id_uc") %>%
  left_join(DESPESA_INDIVIDUAL_2, by = "id_uc") %>%
  left_join(RENDIMENTO_TRABALHO_2, by = "id_uc") %>%
  left_join(OUTROS_RENDIMENTOS_2, by = "id_uc") %>%
  left_join(habitacao, by = "id_uc") %>%
  left_join(CADERNETA_COLETIVA_2, by = "id_uc") %>%
  left_join(ALUGUEL_ESTIMADO_2, by = "id_uc")


base_final <- base_final %>% 
  mutate(preco_kwh = despesa_energia/quantidade_kws) %>% 

  # 1. Transformar despesas anuais em mensais
  mutate(
    despesa_energia = despesa_energia / 12,
    contas_casa = contas_casa / 12,
    aluguel = aluguel / 12,
    despesa_coletiva = despesa_coletiva / 12,
    caderneta_coletiva = caderneta_coletiva / 12,
    despesa_individual = despesa_individual / 12,
    despesa_gas = despesa_gas / 12
  ) %>% 
  # 2. Somar os componentes já mensais
  rowwise() %>%
  mutate(
    gastos_habitacao = sum(contas_casa, aluguel, na.rm = TRUE),
    gastos_totais = sum(despesa_coletiva, caderneta_coletiva, aluguel, despesa_individual, na.rm = TRUE)
  ) %>% 
  ungroup()


salario_minimo <- 998 
# valor do salário mínimo na POF 2017-2018

base_final <- base_final %>%
  mutate(
    homem_ref = sexo == "Homem",
    mulher_ref = sexo == "Mulher",
    negra_ref = raca == "Negro",
    homem_negro_ref = sexo == "Homem" & raca == "Negro",
    mulher_negra_ref = sexo == "Mulher" & raca == "Negro",
    homem_branco_ref = sexo == "Homem" & raca == "Branco",
    mulher_branca_ref = sexo == "Mulher" & raca == "Branco",
    renda_pc_ate_05 = RENDA_DISP_PC <= 0.5 * salario_minimo,
    renda_pc_05a3 = RENDA_DISP_PC > 0.5 * salario_minimo & RENDA_DISP_PC <= 3 * salario_minimo,
    renda_pc_mais3 = RENDA_DISP_PC > 3 * salario_minimo,
    rural = loc_dom == "Rural",
    urbano = loc_dom == "Urbano"
  )

base_final <- base_final %>%
  mutate(
    mulher_negra_renda_media = mulher_negra_ref & renda_pc_05a3,
    homem_branco_renda_media = homem_branco_ref & renda_pc_05a3,
    homem_branco_renda_alta = homem_branco_ref &  renda_pc_mais3,
    mulher_branca_renda_alta = mulher_branca_ref &  renda_pc_mais3
  )




write_csv(base_final, "data/clean/base_final.csv")
