
library(tidyverse)
library(survey)
library(kableExtra)
library(flextable)
library(officer)
rm(list=ls())
gc()



base_final<-read_csv("data/clean/base_final.csv")


# 1. Ajuste de plano amostral
options(survey.lonely.psu = "adjust")

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
  "renda_pc_ate_05", "renda_pc_05a3", "renda_pc_mais3", "rural", "urbano"
)

# 3. Calcular média e erro padrão para quantidade_kws
stats <- map_dfr(colunas_grupos, function(var) {
  subdesign <- subset(design, get(var) == TRUE & !is.na(quantidade_kws))
  resultado <- svymean(~quantidade_kws, subdesign, na.rm = TRUE)
  
  tibble(
    grupo = var,
    media_kwh = as.numeric(coef(resultado)),
    erro_padrao = as.numeric(SE(resultado)),
    coef_var_perc = 100 * erro_padrao / media_kwh
  )
})

# 4. Rotulagem
stats <- stats %>%
  mutate(
    categoria = case_when(
      str_detect(grupo, "renda") ~ "Renda",
      grupo %in% c("rural", "urbano") ~ "Localidade",
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
      TRUE ~ grupo
    )
  )

# 5. Formatar tabela
df_fmt <- stats %>%
  arrange(categoria) %>%
  select(categoria, grupo_label, media_kwh, coef_var_perc) %>%
  rename(
    Categoria = categoria,
    Grupo = grupo_label,
    `Consumo médio de energia (kWh)` = media_kwh,
    `CV (%)` = coef_var_perc
  ) %>%
  group_by(Categoria) %>%
  group_split() %>%
  map_dfr(~{
    cat_name <- unique(.x$Categoria)
    separador <- tibble(
      Categoria = cat_name,
      Grupo = paste0("▸ ", cat_name),
      `Consumo médio de energia (kWh)` = NA,
      `CV (%)` = NA
    )
    bind_rows(separador, .x)
  }) %>%
  ungroup() %>%
  select(-Categoria) %>%
  mutate(across(where(is.numeric), ~ round(., 1)))

# 6. Flextable
ft <- flextable(df_fmt) %>%
  bold(i = grepl("▸", df_fmt$Grupo), bold = TRUE) %>%
  align(align = "left", part = "all") %>%
  autofit() %>%
  set_table_properties(layout = "autofit")

ft
# 7. Exportar para Word
doc <- read_docx() %>%
  body_add_par("Tabela: Consumo médio mensal de energia elétrica por grupo de famílias", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "output/tabela_consumo_kwh.docx")