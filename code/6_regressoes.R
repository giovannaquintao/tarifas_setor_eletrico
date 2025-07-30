
library(tidyverse)
library(survey)

rm(list=ls())
gc()


base_final<-read_csv("data/clean/base_final.csv")


# Grupos
colunas_grupos <- c(
   "rural", "urbano",
  "mulher_negra_renda_media", "homem_branco_renda_media", "homem_branco_renda_alta",  "mulher_branca_renda_alta"
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




names(base_final)
modelo_svy <- svyglm(
  log(quantidade_kws_NI) ~ 
    log(preco_kwh) + 
    log(preco_kwh):mulher_negra_renda_media +
    log(preco_kwh):homem_branco_renda_media +
    log(preco_kwh):homem_branco_renda_alta +
    log(preco_kwh):mulher_branca_renda_alta +
    log(preco_kwh):rural +RENDA_DISP_PC,
  design = design
)

summary(modelo_svy)

# Obtenha os coeficientes
coefs <- coef(modelo_svy)


# Grupo de referência:
# homem branco, renda baixa (até 0,5 SM), urbano
elas_base <- coefs["log(preco_kwh)"]

# Elasticidades por grupo (soma do coeficiente base + interação)
elas_mulher_negra_renda_media   <- elas_base + coefs["log(preco_kwh):mulher_negra_renda_mediaTRUE"]
elas_homem_branco_renda_media   <- elas_base + coefs["log(preco_kwh):homem_branco_renda_mediaTRUE"]
elas_homem_branco_renda_alta    <- elas_base + coefs["log(preco_kwh):homem_branco_renda_altaTRUE"]
elas_mulher_branca_renda_alta   <- elas_base + coefs["log(preco_kwh):mulher_branca_renda_altaTRUE"]
elas_rural                      <- elas_base + coefs["log(preco_kwh):ruralTRUE"]

# Criar data frame com as elasticidades
elasticidades <- data.frame(
  grupo = c(
    "Homem branco (renda baixa)",  # grupo de referência
    "Mulher negra (renda média)",
    "Homem branco (renda média)",
    "Homem branco (renda alta)",
    "Mulher branca (renda alta)",
    "Zona rural",
    "Zona urbana"  # mesma elasticidade do grupo de referência
  ),
  elasticidade_preco = c(
    elas_base,
    elas_mulher_negra_renda_media,
    elas_homem_branco_renda_media,
    elas_homem_branco_renda_alta,
    elas_mulher_branca_renda_alta,
    elas_rural,
    elas_base
  )
)


elasticidades

stats <- map_dfr(colunas_grupos, function(var) {
  subdesign <- subset(design, get(var) == TRUE & !is.na(quantidade_kws))
  resultado1 <- svymean(~quantidade_kws, subdesign, na.rm = TRUE)
  resultado2 <- svymean(~RENDA_DISP_PC, subdesign, na.rm = TRUE)
  resultado3 <- svymean(~gastos_totais, subdesign, na.rm = TRUE)
  tibble(
    grupo = var,
    media_kwh = as.numeric(coef(resultado1)),
    media_renda = as.numeric(coef(resultado2)),
    media_gastos = as.numeric(coef(resultado3))
  )
})


stats2 <- stats %>%
  mutate(
    categoria = case_when(
      str_detect(grupo, "mulher_negra_renda_media") ~ "Renda/Gênero/Raça",
      str_detect(grupo, "homem_branco_renda_media") ~ "Renda/Gênero/Raça",
      str_detect(grupo, "homem_branco_renda_alta") ~ "Renda/Gênero/Raça",
      str_detect(grupo, "mulher_branca_renda_alta") ~ "Renda/Gênero/Raça",
      grupo %in% c("rural", "urbano") ~ "Localidade",
      str_detect(grupo, "renda") ~ "Renda",
      TRUE ~ "Gênero/Raça"
    ),
    grupo_label = case_when(
      grupo == "rural" ~ "Zona rural",
      grupo == "urbano" ~ "Zona urbana",
      grupo == "mulher_negra_renda_media" ~ "Mulher negra (renda média)",
      grupo == "homem_branco_renda_media" ~ "Homem branco (renda média)",
      grupo == "homem_branco_renda_alta" ~ "Homem branco (renda alta)",
      grupo == "mulher_branca_renda_alta" ~ "Mulher branca (renda alta)",
      TRUE ~ grupo
    )
  ) %>% 
  select(-grupo) %>% 
  rename(grupo=grupo_label)



# Adiciona à sua tabela
stats2<-stats2 %>% 
  left_join(.,elasticidades,by="grupo") %>% 
  mutate(preco=0.70) %>% 
  mutate(preco_novo=preco+0.07877) %>% 
  mutate(aumento_preco=100*0.07877/0.70)


stats2<-stats2 %>% 
  mutate(pct_consumo_after=elasticidade_preco*aumento_preco) %>% 
  mutate(var_consumo=pct_consumo_after/100*media_kwh) %>% 
  mutate(gasto_antes=preco*media_kwh) %>% 
  mutate(gasto_depois=preco_novo*(media_kwh-var_consumo))


stats2<-stats2 %>% 
  mutate(dif=gasto_depois-gasto_antes) %>% 
  mutate(dif_renda=100*dif/media_renda) %>%
  mutate(dif_gastos=100*dif/media_gastos) %>% 
  mutate(pct_dif=100*(gasto_depois-gasto_antes)/gasto_antes)


#aumenta o preco, reduz o consumo, 

# Adicionar categorias e labels


elasticidade<-stats2 %>% 
  select(grupo,categoria,elasticidade_preco) %>% 
  mutate(elasticidade_preco=100*round(elasticidade_preco,4))

# Primeiro, transformamos para formato longo
df_long <- stats2 %>%
  select(categoria,grupo_label,grupo,dif_gastos,dif_renda,pct_dif) %>% 
  pivot_longer(cols = c(dif_renda,dif_gastos,pct_dif),
               names_to = "variavel", values_to = "media")


ggplot(df_long %>% filter(categoria == "Renda"&variavel=="dif_gastos") , 
       aes(x = grupo_label, y = media, fill = variavel)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~variavel, scales = "free_y") +
#  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

