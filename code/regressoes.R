
library(tidyverse)
library(survey)

rm(list=ls())
gc()


base_final<-read_csv("data/clean/base_final.csv")


colunas_grupos <- c(
  "homem_ref", "mulher_ref", "homem_negro_ref", "mulher_negra_ref", 
  "homem_branco_ref", "mulher_branca_ref",
  "renda_pc_ate_05", "renda_pc_05a3", "renda_pc_mais3", "rural", "urbano"
)

names(base_final)

################# regressao ##################

base_final <- base_final %>%
  mutate(preco_kwh = despesa_energia/quantidade_kws)


# Remover casos problemáticos
base_final2 <- base_final %>%
  filter(despesa_energia > 0, RENDA_DISP_PC > 0, preco_kwh > 0) %>% 
  filter(is.na(quantidade_kws_NI)==F&quantidade_kws_NI>0) %>% 
  filter(renda_pc_05a3 | renda_pc_mais3) %>% 
  mutate(UF=as.factor(UF)) 



design <- svydesign(
  ids = ~COD_UPA,
  strata = ~ESTRATO_POF,
  weights = ~PESO_FINAL,
  data = base_final2,
  nest = TRUE
)


options(survey.lonely.psu = "adjust")


modelo_svy <- svyglm(
  log(quantidade_kws_NI) ~ 
    log(preco_kwh)*mulher_ref +
    log(preco_kwh)*negra_ref +
    log(preco_kwh)*rural +
    log(preco_kwh)*renda_pc_05a3 +
    log(preco_kwh)*renda_pc_mais3 +
    UF,
  design = design
)


summary(modelo_svy)


#aumenta o preco, reduz o consumo, 
