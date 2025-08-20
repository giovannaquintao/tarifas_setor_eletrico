# Fazer tabela de caracterização das famílias

rm(list=ls())
gc()



library(tidyverse)
library(survey)
library(flextable)
library(officer)
library(purrr)



################### 1. Calculating average ################################

# Ler base
base_final <- read_csv("data/clean/base_final.csv")


# Plano amostral
design <- svydesign(
  id = ~COD_UPA,
  strata = ~ESTRATO_POF,
  weights = ~PESO_FINAL,
  data = base_final,
  nest = TRUE
)

names(base_final)

# Grupos
colunas_grupos <- c(
  "homem_ref", "mulher_ref", "homem_negro_ref", "mulher_negra_ref", 
  "homem_branco_ref", "mulher_branca_ref",
  "renda_pc_ate_05", "renda_pc_05a3", "renda_pc_mais3", "rural", "urbano",
  "mulher_negra_renda_media", "homem_branco_renda_media", "homem_branco_renda_alta",  "mulher_branca_renda_alta"
)

#checar numero de familias - cerca de 72 milhoes

n_familias   <- svytotal(~I(!is.na(gastos_totais)), design)  # ponderado
coef(n_familias)[2] /1000000



# Loop por grupo
stats <- map_dfr(colunas_grupos, function(var) {
  subdesign <- subset(design, get(var) == TRUE & !is.na(gastos_totais))
  
  media_gastos <- svymean(~gastos_totais, subdesign, na.rm = TRUE)
  media_renda  <- svymean(~RENDA_DISP_PC, subdesign, na.rm = TRUE)
  media_hab    <- svymean(~gastos_habitacao, subdesign, na.rm = TRUE)
  total_fam    <- svytotal(~I(!is.na(gastos_totais)), subdesign)  # ponderado
  
  tibble(
    grupo = var,
    media_gastos_g = as.numeric(coef(media_gastos)),
    coef_var_perc_g = as.numeric(100 * SE(media_gastos) / coef(media_gastos)),
    media_renda_g = as.numeric(coef(media_renda)),
    coef_var_perc_r = as.numeric(100 * SE(media_renda) / coef(media_renda)),
    media_gastos_hab = as.numeric(coef(media_hab)),
    coef_var_perc_hab = as.numeric(100 * SE(media_hab) / coef(media_hab)),
    n_familias = as.numeric(coef(total_fam)[[2]])
  )
}
)

# Adicionar categorias e labels
stats <- stats %>%
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
      grupo == "mulher_negra_renda_media" ~ "Mulher negra (renda média)",
      grupo == "homem_branco_renda_media" ~ "Homem branco (renda média)",
      grupo == "homem_branco_renda_alta" ~ "Homem branco (renda alta)",
      grupo == "mulher_branca_renda_alta" ~ "Mulher branca (renda alta)",
      TRUE ~ grupo
    )
  )



total_fam <- stats %>%
  filter(grupo_label %in% c("Homem", "Mulher")) %>%
  summarise(total = sum(n_familias, na.rm = TRUE)) %>%
  pull(total)


stats<-stats %>% 
  mutate(n_familias2=paste0(round(n_familias,0)," (",round(100*n_familias/total_fam,1),"%)"))

stats<-stats %>% 
  mutate(pct_hab=100*media_gastos_hab/media_gastos_g)%>% 
  mutate(pct_renda_pc=100*media_gastos_hab/media_renda_g)


################### 2. Creating Table ################################


# 1. Organizar base
df_tab <- stats %>%
  select(categoria, grupo_label, 
         media_renda_g, coef_var_perc_r,
         media_gastos_g, coef_var_perc_g,
         media_gastos_hab, coef_var_perc_hab,
         n_familias2) %>%
  arrange(categoria) %>%
  rename(
    Categoria = categoria,
    Grupo = grupo_label,
    `Gasto total (R$)` = media_gastos_g,
    `CV gasto (%)` = coef_var_perc_g,
    `Renda per capita (R$)` = media_renda_g,
    `CV renda (%)` = coef_var_perc_r,
    `Gasto com habitação (R$)` = media_gastos_hab,
    `CV habitação (%)` = coef_var_perc_hab,
    `Nº famílias (% sobre total)` = n_familias2
  )

# 2. Adicionar linhas de separação por categoria
df_fmt <- df_tab %>%
  group_by(Categoria) %>%
  group_split() %>%
  map_dfr(~{
    cat_name <- unique(.x$Categoria)
    separador <- tibble(
      Categoria = cat_name,
      Grupo = paste0("▸ ", cat_name),
      `Renda per capita (R$)` = NA,
      `CV renda (%)` = NA,
      `Gasto total (R$)` = NA,
      `CV gasto (%)` = NA,
      `Gasto com habitação (R$)` = NA,
      `CV habitação (%)` = NA,
      `Nº famílias (% sobre total)` = NA
    )
    bind_rows(separador, .x)
  }) %>%
  ungroup() %>%
  select(-Categoria) %>%
  mutate(across(where(is.numeric), ~round(., 1)))

# 3. Criar flextable
ft <- flextable(df_fmt) %>%
  bold(i = grepl("▸", df_fmt$Grupo), bold = TRUE) %>%
  align(align = "left", part = "all") %>%
  autofit() %>%
  set_table_properties(layout = "autofit")%>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all")%>%
  padding(padding = 0, part = "all") 


ft
# 4. Exportar para Word
doc <- read_docx() %>%
  body_add_par("Tabela: Despesas e renda por grupo de famílias", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "output/tabela_familias_completa.docx")



