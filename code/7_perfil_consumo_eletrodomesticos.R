
library(tidyverse)
library(survey)
library(scales)
library(flextable)
library(officer)
library(dplyr)
library(purrr)
library(tibble)

rm(list=ls())


gc()



################# 1. Preparar Base de Dados ##################


options(survey.lonely.psu = "adjust")  


base_final<-read_csv("data/clean/base_final.csv")


design <- svydesign(
  id = ~COD_UPA,
  strata = ~ESTRATO_POF,
  weights = ~PESO_FINAL,
  data = base_final,
  nest = TRUE
)

# 2. Grupos
colunas_grupos <- c(
  "homem_ref", "mulher_ref", "homem_negro_ref", "mulher_negra_ref", 
  "homem_branco_ref", "mulher_branca_ref",
  "renda_pc_ate_05", "renda_pc_05a3", "renda_pc_mais3", "rural", "urbano",
  "homem_negro_renda_baixa", "homem_negro_renda_media", "homem_negro_renda_alta",
  "homem_branco_renda_baixa", "homem_branco_renda_media", "homem_branco_renda_alta",
  "mulher_negra_renda_baixa", "mulher_negra_renda_media", "mulher_negra_renda_alta",
  "mulher_branca_renda_baixa", "mulher_branca_renda_media", "mulher_branca_renda_alta"
)

names(base_final)
# Variáveis de bens a medir
vars_bens <- c(
  "ar_cond","geladeira","microondas","chuveiro",
  "lavar_roupa","lavar_louca","computadores","televisores"
)

stats <- map_dfr(colunas_grupos, function(g){
  # subset por grupo (dummies lógicas: TRUE/FALSE)
  subd <- subset(design, get(g) == TRUE)
  
  # svymean em todas as variáveis de uma vez
  fml <- as.formula(paste0("~", paste(vars_bens, collapse = " + ")))
  est <- svymean(fml, subd, na.rm = TRUE)
  
  tibble(
    grupo          = g,
    variavel       = names(coef(est)),
    media          = as.numeric(coef(est)),
    erro_padrao    = as.numeric(SE(est)),
    coef_var_perc  = 100 * erro_padrao / pmax(media, .Machine$double.eps)
  )
})


stats <- stats %>%
  mutate(
    categoria = case_when(
      str_detect(grupo, "negra_renda") ~ "Renda/Gênero/Raça",
      str_detect(grupo, "negro_renda") ~ "Renda/Gênero/Raça",
      str_detect(grupo, "branco_renda") ~ "Renda/Gênero/Raça",
      str_detect(grupo, "branca_renda") ~ "Renda/Gênero/Raça",
      grupo %in% c("rural", "urbano") ~ "Localidade",
      str_detect(grupo, "renda") ~ "Renda",
      TRUE ~ "Gênero/Raça"
    ),
    grupo_label = case_when(
      grupo == "homem_ref" ~ "Homem",
      grupo == "mulher_ref" ~ "Mulher",
      grupo == "homem_negro_ref" ~ "Homem negro",
      grupo == "mulher_negra_ref" ~ "Mulher negra",
      grupo == "homem_branco_ref" ~ "Homem branco",
      grupo == "mulher_branca_ref" ~ "Mulher branca",
      grupo == "renda_pc_ate_05" ~ "Até 0,5 SM per capita",
      grupo == "renda_pc_05a3" ~ "De 0,5 a 3 SM per capita",
      grupo == "renda_pc_mais3" ~ "Acima de 3 SM per capita",
      grupo == "rural" ~ "Zona rural",
      grupo == "urbano" ~ "Zona urbana",
      # ---- combinações sexo × raça × renda ----
      grupo == "homem_negro_renda_baixa"   ~ "Homem negro (renda baixa)",
      grupo == "homem_negro_renda_media"   ~ "Homem negro (renda média)",
      grupo == "homem_negro_renda_alta"    ~ "Homem negro (renda alta)",
      
      grupo == "homem_branco_renda_baixa"  ~ "Homem branco (renda baixa)",
      grupo == "homem_branco_renda_media"  ~ "Homem branco (renda média)",
      grupo == "homem_branco_renda_alta"   ~ "Homem branco (renda alta)",
      
      grupo == "mulher_negra_renda_baixa"  ~ "Mulher negra (renda baixa)",
      grupo == "mulher_negra_renda_media"  ~ "Mulher negra (renda média)",
      grupo == "mulher_negra_renda_alta"   ~ "Mulher negra (renda alta)",
      
      grupo == "mulher_branca_renda_baixa" ~ "Mulher branca (renda baixa)",
      grupo == "mulher_branca_renda_media" ~ "Mulher branca (renda média)",
      grupo == "mulher_branca_renda_alta"  ~ "Mulher branca (renda alta)",
      TRUE ~ grupo
    )
  )

names(stats)
stats<-stats %>% 
  select(categoria,grupo_label,variavel,media,coef_var_perc) 

stats2<-stats%>% 
  mutate(percentual=100*media) %>% 
  select(-media) %>% 

  filter(categoria=="Renda/Gênero/Raça") %>% 
  select(-categoria) %>% 
  pivot_wider(
    id_cols    = grupo_label,
    names_from = variavel,
    values_from = c(percentual, coef_var_perc),
    names_glue = "{.value}_{variavel}")
ord <- names(stats2) %>%
  {\(.) .[str_detect(., "^percentual_")]}() %>%
  str_remove("^percentual_")

# constrói a ordem intercalando média e CV(%) para cada variável
col_order <- c(
  "grupo_label",
  as.vector(rbind(paste0("percentual_", ord),
                  paste0("coef_var_perc_", ord)))
)

# reordena
stats2 <- stats2 %>% select(all_of(col_order))



# 2) Defina quais são "cozinha" (apenas as que existirem no stats2)
cozinha_candidatas <- c("geladeira", "microondas", "forno", "lavar_louca")
vars_cozinha <- intersect(cozinha_candidatas, vars_bens)

# 3) Outras variáveis = tudo que não é cozinha
vars_outros <- setdiff(vars_bens, vars_cozinha)
col_order_cozinha <- c(
  "grupo_label",
  as.vector(rbind(paste0("percentual_", vars_cozinha),
                  paste0("coef_var_perc_", vars_cozinha)))
)

col_order_outros <- c(
  "grupo_label",
  as.vector(rbind(paste0("percentual_", vars_outros),
                  paste0("coef_var_perc_", vars_outros)))
)

stats2_cozinha <- stats2 %>%
  select(any_of(col_order_cozinha)) %>%
  arrange(grupo_label)

stats2_outros <- stats2 %>%
  select(any_of(col_order_outros)) %>%
  arrange(grupo_label)
