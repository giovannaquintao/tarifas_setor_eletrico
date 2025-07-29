
library(tidyverse)
library(survey)
library(kableExtra)
library(flextable)
library(officer)
library(purrr)

rm(list=ls())
gc()


base_final<-read_csv("data/clean/base_final.csv")

names(base_final)
table(base_final$rede_geral)
table(base_final$outra_origem)



base_final<-base_final %>% 
mutate(rede_geral=ifelse(rede_geral=="Sim",1,0)) %>% 
mutate(energia_integral=ifelse(energia_integral=="Sim",1,0)) %>% 
mutate(outra_origem=ifelse(outra_origem=="Sim",1,0)) 
table(base_final$rede_geral)
table(base_final$outra_origem)

table(base_final$energia_integral)


options(survey.lonely.psu = "adjust")  # ou "certainty", "remove", "average"

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

resultado1 <- svymean(~rede_geral, design, na.rm = TRUE)
resultado2 <- svymean(~energia_integral, design, na.rm = TRUE)
resultado3 <- svymean(~outra_origem, design, na.rm = TRUE)


resultado1

resultado2

resultado3

stats <- map_dfr(colunas_grupos, function(var) {
  # subset com a condição do grupo == TRUE
  subdesign <- subset(design, get(var) == TRUE & !is.na(gastos_totais))
  
  # calcular média e erro padrão
  resultado1 <- svymean(~rede_geral, subdesign, na.rm = TRUE)
  resultado2 <- svymean(~energia_integral, subdesign, na.rm = TRUE)
  resultado3 <- svymean(~outra_origem, subdesign, na.rm = TRUE)
  
  
  media1 <- 100*coef(resultado1)
  erro1 <- 100*SE(resultado1)
  cv1    <- 100 * erro1 / media1
  
  
  media2 <- 100*coef(resultado2)
  erro2 <- 100*SE(resultado2)
  cv2   <- 100 * erro2 / media2
  
  
  media3 <- coef(resultado3)
  erro3 <- SE(resultado3)
  cv3   <- 100 * erro3 / media3
  
  tibble(
    grupo = var,
    media_rede_geral = as.numeric(media1),
    erro_padrao_g = as.numeric(erro1),
    coef_var_perc_g = as.numeric(cv1),
    media_energia_integral = as.numeric(media2),
    erro_padrao_ei = as.numeric(erro2),
    coef_var_perc_ei = as.numeric(cv2),
    media_outra_origem = as.numeric(media3),
    erro_padrao_oo = as.numeric(erro3),
    coef_var_perc_oo = as.numeric(cv3)
  )
})


stats <- stats %>%
  mutate(
    categoria = case_when(
      str_detect(grupo, "renda") ~ "Renda",
      grupo %in% c("rural", "urbano") ~ "Localidade",
      TRUE ~ "Gênero/Raça"
    ),
    grupo_label = case_when(
      grupo == "homem_ref" ~ "Homem ",
      grupo == "mulher_ref" ~ "Mulher ",
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

names(stats)

# 1. Preparar a base com nomes bonitos
df_tab <- stats %>%
  select(categoria, grupo_label, 
         media_rede_geral, coef_var_perc_g,
         media_energia_integral, coef_var_perc_ei,
         media_outra_origem, coef_var_perc_oo) %>%
  arrange(categoria) %>%
  rename(
    Categoria = categoria,
    Grupo = grupo_label,
    `Uso de rede geral (%)` = media_rede_geral,
    `CV rede geral (%)` = coef_var_perc_g,
    `Energia integral (%)` = media_energia_integral,
    `CV energia integral (%)` = coef_var_perc_ei,
    `Outra origem de energia (%)` = media_outra_origem,
    `CV outra origem (%)` = coef_var_perc_oo
  )

# 2. Adicionar linha de separação por categoria
df_fmt <- df_tab %>%
  group_by(Categoria) %>%
  group_split() %>%
  map_dfr(~{
    cat_name <- unique(.x$Categoria)
    separador <- tibble(
      Categoria = cat_name,
      Grupo = paste0("▸ ", cat_name),
      `Uso de rede geral (%)` = NA,
      `CV rede geral (%)` = NA,
      `Energia integral (%)` = NA,
      `CV energia integral (%)` = NA,
      `Outra origem de energia (%)` = NA,
      `CV outra origem (%)` = NA
    )
    bind_rows(separador, .x)
  }) %>%
  ungroup() %>%
  select(-Categoria) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), NA, round(., 2)))

# 3. Criar flextable
ft <- flextable(df_fmt) %>%
  bold(i = grepl("▸", df_fmt$Grupo), bold = TRUE) %>%
  align(align = "left", part = "all") %>%
  autofit() %>%
  set_table_properties(layout = "autofit")
ft
# 4. Criar e salvar documento Word
doc <- read_docx() %>%
  body_add_par("Tabela: Fontes de energia por grupo de famílias", style = "heading 1") %>%
  body_add_flextable(ft)
doc
print(doc, target = "output/tabela_fontes_energias.docx")



# 
# A energia elétrica utilizada neste domicílio é proveniente de rede geral?	1 – Sim
# 2 – Não
# A energia elétrica utilizada neste domicílio é proveniente de outra origem (gerador, placa solar, eólica, etc.)?	1 – Sim
# 2 – Não
# Com que frequência a energia elétrica, proveniente de rede geral, está habitualmente disponível para este domicílio?	Branco – Não Aplicável
# 1 – Diária, em tempo integral
# 2 – Diária, por algumas horas
# 3 – Outra frequência

