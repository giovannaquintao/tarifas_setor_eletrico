#Calculate the spending per family
library(tidyverse)
library(survey)
library(kableExtra)

DOMICILIO <- readRDS("data/clean/DOMICILIO.rds")
MORADOR <- readRDS("data/clean/MORADOR.rds")
DESPESA_COLETIVA <- readRDS("data/clean/DESPESA_COLETIVA.rds")
DESPESA_INDIVIDUAL <- readRDS("data/clean/DESPESA_INDIVIDUAL.rds")
CADERNETA_COLETIVA <- readRDS("data/clean/CADERNETA_COLETIVA.rds")
ALUGUEL_ESTIMADO <- readRDS("data/clean/ALUGUEL_ESTIMADO.rds")
OUTROS_RENDIMENTOS <- readRDS("data/clean/OUTROS_RENDIMENTOS.rds")
RENDIMENTO_TRABALHO <- readRDS("data/clean/RENDIMENTO_TRABALHO.rds")


weighted.mean(
  x = DESPESA_COLETIVA$V9005[DESPESA_COLETIVA$COD_IMPUT_QUANTIDADE == 0],
  w = DESPESA_COLETIVA$PESO_FINAL[DESPESA_COLETIVA$COD_IMPUT_QUANTIDADE == 0],
  na.rm = TRUE
)


k<-DESPESA_COLETIVA %>% 
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>% 
  summarise(n=n_distinct(PESO_FINAL))

table(k$n,useNA = "always")
k<-MORADOR%>% 
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>% 
  summarise(n=n_distinct(PESO_FINAL))

table(k$n,useNA = "always")
k<-DESPESA_INDIVIDUAL%>% 
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>% 
  summarise(n=n_distinct(PESO_FINAL))

table(k$n,useNA = "always")


k<-MORADOR%>% 
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>% 
  summarise(n=n_distinct(RENDA_DISP_PC))

table(k$n,useNA = "always")

k<-MORADOR%>% 
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>% 
  summarise(n=n_distinct(RENDA_TOTAL))

table(k$n,useNA = "always")

