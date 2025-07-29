
library(tidyverse)
library(survey)
library(kableExtra)

rm(list=ls())
gc()


base_final<-read_csv("data/clean/base_final.csv")




design <- svydesign(
  id = ~COD_UPA,
  strata = ~ESTRATO_POF,
  weights = ~PESO_FINAL,
  data = base_final,
  nest = TRUE
)


# calcular média e erro padrão
resultado1 <- svymean(~gastos_totais, design, na.rm = TRUE)
resultado2 <- svymean(~RENDA_DISP_PC, design, na.rm = TRUE)

resultado1
resultado2
colunas_grupos <- c(
  "homem_ref", "mulher_ref", "homem_negro_ref", "mulher_negra_ref", 
  "homem_branco_ref", "mulher_branca_ref",
  "renda_pc_ate_05", "renda_pc_05a3", "renda_pc_mais3", "rural", "urbano"
)



###################### gasto medio R$ ######################

stats <- map_dfr(colunas_grupos, function(var) {
  # subset com a condição do grupo == TRUE
  subdesign <- subset(design, get(var) == TRUE & !is.na(gastos_totais))
  
  # calcular média e erro padrão
  resultado1 <- svymean(~gastos_totais, subdesign, na.rm = TRUE)
  resultado2 <- svymean(~RENDA_DISP_PC, subdesign, na.rm = TRUE)
  
  
  media1 <- coef(resultado1)
  erro1 <- SE(resultado1)
  cv1    <- 100 * erro1 / media1
  
  
  media2 <- coef(resultado2)
  erro2 <- SE(resultado2)
  cv2   <- 100 * erro2 / media2
  
  tibble(
    grupo = var,
    media_gastos = as.numeric(media1),
    erro_padrao_g = as.numeric(erro1),
    coef_var_perc_g = as.numeric(cv1),
    media_renda = as.numeric(media2),
    erro_padrao_r = as.numeric(erro2),
    coef_var_perc_r = as.numeric(cv2)
  )
})
# Criar labels bonitos
stats <- stats %>%
  mutate(
    ic_lower_g = media_gastos - 1.96 * erro_padrao_g,
    ic_upper_g = media_gastos + 1.96 * erro_padrao_g,
    ic_lower_r = media_renda - 1.96 * erro_padrao_r,
    ic_upper_r = media_renda + 1.96 * erro_padrao_r,
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
library(dplyr)
library(flextable)
library(officer)

# 1. Preparar a base com nomes bonitos
df_tab <- stats %>%
  select(categoria, grupo_label, 
         media_gastos, coef_var_perc_g, 
         media_renda, coef_var_perc_r) %>%
  arrange(categoria) %>%
  rename(
    Categoria = categoria,
    Grupo = grupo_label,
    `Gasto médio mensal (R$)` = media_gastos,
    `Coef. var. gasto (%)` = coef_var_perc_g,
    `Renda média mensal (R$)` = media_renda,
    `Coef. var. renda (%)` = coef_var_perc_r
  )

# 2. Adicionar linhas de separação visual entre categorias
df_fmt <- df_tab %>%
 # mutate(grupo_display = Grupo) %>%
  group_by(Categoria) %>%
  group_split() %>%
  purrr::map_dfr(~{
    cat_name <- unique(.x$Categoria)
    separador <- tibble(
      Categoria = cat_name,
      Grupo = paste0("▸ ", cat_name),
      `Gasto médio mensal (R$)` = NA,
      `Coef. var. gasto (%)` = NA,
      `Renda média mensal (R$)` = NA,
      `Coef. var. renda (%)` = NA,
    )
    bind_rows(separador, .x)
  }) %>%
  ungroup() %>%
  select(-Categoria) %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), NA, round(., 1)))

# 3. Criar flextable
ft <- flextable(df_fmt) %>%
  bold(i = grepl("▸", df_fmt$Grupo), bold = TRUE) %>%
  align(align = "left", part = "all") %>%
  autofit() 

# 4. Criar documento Word e adicionar tabela
doc <- read_docx() %>%
  body_add_par("Tabela: Comparações por grupo", style = "heading 1") %>%
  body_add_flextable(ft) %>%
  set_table_properties(layout = "autofit")

doc
# 5. Salvar
print(doc, target = "output/tabela_familias.docx")
