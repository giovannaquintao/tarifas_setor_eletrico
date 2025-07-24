
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
                           na.rm=T)) %>% 
  mutate(gastos_totais=gastos_totais/12) 


###############################################################



salario_minimo <- 998  # valor do salário mínimo na POF 2017-2018

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

svymean(~gastos_totais, subset(design, mulher_ref == TRUE), na.rm = TRUE)
svymean(~gastos_totais, subset(design, homem_ref == TRUE), na.rm = TRUE)
svymean(~gastos_totais, subset(design,  homem_negro_ref == TRUE), na.rm = TRUE)
svymean(~gastos_totais, subset(design,  mulher_negra_ref == TRUE), na.rm = TRUE)

colunas_grupos <- c(
  "homem_ref", "mulher_ref", "homem_negro_ref", "mulher_negra_ref", 
  "homem_branco_ref", "mulher_branca_ref",
  "renda_pc_ate_05", "renda_pc_05a3", "renda_pc_mais3", "rural", "urbano"
)


library(tidyverse)

# Criar labels bonitos
stats <- stats %>%
  mutate(
    ic_lower = media_gastos - 1.96 * erro_padrao,
    ic_upper = media_gastos + 1.96 * erro_padrao,
    categoria = case_when(
      str_detect(grupo, "renda") ~ "Renda",
      grupo %in% c("rural", "urbano") ~ "Localidade",
      TRUE ~ "Gênero/Raça"
    ),
    grupo_label = case_when(
      grupo == "homem_ref" ~ "Homem ",
      grupo == "mulher_ref" ~ "Mulher ",
      grupo == "negra_ref" ~ "Pessoa negra ",
      grupo == "homem_negro_ref" ~ "Homem negro ",
      grupo == "mulher_negra_ref" ~ "Mulher negra ",
      grupo == "homem_branco_ref" ~ "Homem branco ",
      grupo == "mulher_branca_ref" ~ "Mulher branca ",
      grupo == "renda_pc_ate_05" ~ "Até 0,5 SM per capita",
      grupo == "renda_pc_05a3" ~ "De 0,5 a 3 SM per capita",
      grupo == "renda_pc_mais3" ~ "Acima de 3 SM per capita",
      grupo == "rural" ~ "Zona rural",
      grupo == "urbano" ~ "Zona urbana",
      TRUE ~ grupo
    )
  )

# Função para plotar com labels bonitos
plot_ic <- function(df, categoria_nome) {
  df %>%
    filter(categoria == categoria_nome) %>%
    ggplot(aes(x = reorder(grupo_label, media_gastos), y = media_gastos)) +
    geom_col(fill = "#4682B4") +
    geom_errorbar(aes(ymin = ic_lower, ymax = ic_upper), width = 0.2) +
    labs(
      title = paste("Gasto médio da família por grupo -", categoria_nome),
      x = NULL,
      y = "Gasto mensal da família (R$)"
    ) +
    theme_minimal(base_size = 13)
}

# Gerar os gráficos
plot_ic(stats, "Gênero/Raça")
plot_ic(stats, "Renda")
plot_ic(stats, "Localidade")



# Filtra apenas Gênero/Raça e organiza sexo e rótulo principal
genero_raca <- stats %>%
  filter(categoria == "Gênero/Raça") %>%
  mutate(
    sexo = case_when(
      str_detect(grupo, "homem") ~ "Homem",
      str_detect(grupo, "mulher") ~ "Mulher",
      TRUE ~ "Ambos"
    ),
    grupo_base = case_when(
      grupo %in% c("homem_branco_ref", "mulher_branca_ref") ~ "Branco",
      grupo %in% c("homem_negro_ref", "mulher_negra_ref", "negra_ref") ~ "Negro",
      grupo == "homem_ref" ~ "Homem (geral)",
      grupo == "mulher_ref" ~ "Mulher (geral)",
      TRUE ~ grupo
    )
  ) %>% 
  filter(!grupo%in%c("homem_ref","mulher_ref"))

# Criar gráfico com barras lado a lado e alpha por sexo
ggplot(genero_raca, aes(x = grupo_base, y = media_gastos, fill = sexo, alpha = sexo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = ic_lower, ymax = ic_upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  scale_alpha_manual(values = c("Mulher" = 1, "Homem" = 0.6, "Ambos" = 0.8)) +
  labs(
    title = "Gasto mensal por raça e sexo da pessoa de referência",
    x = NULL,
    y = "Gasto médio (R$)",
    fill = "Sexo",
    alpha = "Sexo"
  ) +
  theme_minimal(base_size = 13)
