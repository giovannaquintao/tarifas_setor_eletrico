
library(tidyverse)
library(survey)

rm(list=ls())
gc()
DOMICILIO <- readRDS("data/clean/DOMICILIO.rds")
MORADOR <- readRDS("data/clean/MORADOR.rds")
DESPESA_COLETIVA <- readRDS("data/clean/DESPESA_COLETIVA.rds")
DESPESA_INDIVIDUAL <- readRDS("data/clean/DESPESA_INDIVIDUAL.rds")
CADERNETA_COLETIVA <- readRDS("data/clean/CADERNETA_COLETIVA.rds")
ALUGUEL_ESTIMADO <- readRDS("data/clean/ALUGUEL_ESTIMADO.rds")


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

names(DESPESA_COLETIVA)
table(DESPESA_COLETIVA$COD_IMPUT_QUANTIDADE)

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
    despesa_energia=sum(V8000_DEFLA_anual[V9001==600101],na.rm=T),
    quantidade_kws=sum(V9005,na.rm=T),
    quantidade_kws_NI=sum(V9005[COD_IMPUT_QUANTIDADE==0],na.rm=T),
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
  mutate(despesa_energia=despesa_energia/12) %>% 
  rowwise() %>%
  mutate(gastos_habitacao=sum(habitacao,aluguel,na.rm=T),
         gastos_totais=sum(despesa_coletiva,caderneta_coletiva,
                           aluguel,
                           despesa_individual,
                           na.rm=T)/12) %>% 
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





design <- svydesign(
  id = ~COD_UPA,
  strata = ~ESTRATO_POF,
  weights = ~PESO_FINAL,
  data = base_final,
  nest = TRUE
)


colunas_grupos <- c(
  "homem_ref", "mulher_ref", "homem_negro_ref", "mulher_negra_ref", 
  "homem_branco_ref", "mulher_branca_ref",
  "renda_pc_ate_05", "renda_pc_05a3", "renda_pc_mais3", "rural", "urbano"
)



################# regressao ##################

names(base_final)
table(is.na(base_final$quantidade_kws_NI))
base_final <- base_final %>%
  mutate(preco_kwh = despesa_energia/quantidade_kws)

weighted.mean(base_final$preco_kwh, w = base_final$PESO_FINAL, na.rm = TRUE)
weighted.mean(base_final$despesa_energia, w = base_final$PESO_FINAL, na.rm = TRUE)
weighted.mean(base_final$RENDA_DISP_PC, w = base_final$PESO_FINAL, na.rm = TRUE)
weighted.mean(base_final$quantidade_kws, w = base_final$PESO_FINAL, na.rm = TRUE)
names(DOMICILIO_2)
# Remover casos problemáticos
base_final2 <- base_final %>%
  filter(despesa_energia > 0, RENDA_DISP_PC > 0, preco_kwh > 0) %>% 
  filter(is.na(quantidade_kws_NI)==F&quantidade_kws_NI>0) %>% 
  # filter(renda_pc_05a3 | renda_pc_mais3) %>% 
 # filter(renda_pc_05a3) %>% 
  mutate(UF=as.factor(UF)) 
 # filter(quantidade_kws < 1000&quantidade_kws > 100) %>% 
#  filter(preco_kwh < 0.7)
# Definir o objeto de desenho amostral (survey design)
design <- svydesign(
  ids = ~COD_UPA,
  strata = ~ESTRATO_POF,
  weights = ~PESO_FINAL,
  data = base_final2,
  nest = TRUE
)
options(survey.lonely.psu = "adjust")
# Estimar o modelo com correção para o plano amostral
modelo_svy <- svyglm(
  log(quantidade_kws_NI) ~ log(preco_kwh)* log(RENDA_DISP_PC) + mulher_ref + negra_ref + rural
  + UF,
  design = design
)

# Ver os resultados
summary(modelo_svy)
