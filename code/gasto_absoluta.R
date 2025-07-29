
library(tidyverse)
library(survey)
library(kableExtra)

rm(list=ls())
gc()

options(survey.lonely.psu = "adjust")  # ou "certainty", "remove", "average"


base_final<-read_csv("data/clean/base_final.csv")


colunas_grupos <- c(
  "homem_ref", "mulher_ref", "homem_negro_ref", "mulher_negra_ref", 
  "homem_branco_ref", "mulher_branca_ref",
  "renda_pc_ate_05", "renda_pc_05a3", "renda_pc_mais3", "rural", "urbano"
)

design <- svydesign(
  id = ~COD_UPA,
  strata = ~ESTRATO_POF,
  weights = ~PESO_FINAL,
  data = base_final,
  nest = TRUE
)