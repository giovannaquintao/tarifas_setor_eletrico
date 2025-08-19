
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

design_complete <- svydesign(
  ids = ~COD_UPA,
  strata = ~ESTRATO_POF,
  weights = ~PESO_FINAL,
  data = base_final ,
  nest = TRUE
)



#remover familiar com menos de meio salario minimo per capita
base_final2 <- base_final %>%
  filter(despesa_energia > 0, RENDA_DISP_PC > 0, preco_kwh > 0) %>% 
  filter(is.na(quantidade_kws_NI)==F&quantidade_kws_NI>0) %>% 
  filter(renda_pc_05a3 | renda_pc_mais3) %>% 
  mutate(UF=as.factor(UF))


design <- svydesign(
  ids = ~COD_UPA,
  strata = ~ESTRATO_POF,
  weights = ~PESO_FINAL,
  data = base_final2 ,
  nest = TRUE
)

################# 2. Rodar modelo Geral ##################


svymean(~preco_kwh, design = design_complete, na.rm = TRUE)

modelo <- svyglm(
  log(quantidade_kws_NI) ~ log(preco_kwh)+log(RENDA_DISP_PC)+ANOS_ESTUDO+n_moradores+media_idade+cond_ocup+loc_dom,
  design = design
)

summary(modelo)

################# 3. Rodar modelos para grupos ##################


# para grupos sexo + renda

resultados1 <- purrr::map_dfr(c(
  "mulher_negra_renda_media",
  "homem_branco_renda_media",
  "homem_branco_renda_alta",
  "mulher_branca_renda_alta"
), function(grupo_nome) {
  
  # Filtrar o subgrupo
  sub_design <- subset(design, get(grupo_nome) == TRUE)
  
  # Rodar o modelo simples
  modelo <- svyglm(
    log(quantidade_kws_NI) ~ log(preco_kwh)+log(RENDA_DISP_PC)+ANOS_ESTUDO+n_moradores+media_idade+cond_ocup+loc_dom,
    design = sub_design
  )
  
  # Extrair elasticidade (coeficiente)
  coef_est <- coef(modelo)["log(preco_kwh)"]
  se_est   <- coef(summary(modelo))[, "Std. Error"]["log(preco_kwh)"]
  ic_low   <- coef_est - 1.96 * se_est
  ic_high  <- coef_est + 1.96 * se_est
  
  # Voltar como tibble
  tibble(
    grupo = grupo_nome,
    elasticidade = coef_est,
    se = se_est,
    ic_inf = ic_low,
    ic_sup = ic_high
  )
})



resultados2 <- purrr::map_dfr(c(
  "rural",
  "urbano"
), function(grupo_nome) {
  
  # Filtrar o subgrupo
  sub_design <- subset(design, get(grupo_nome) == TRUE)
  
  # Rodar o modelo simples
  modelo <- svyglm(
    log(quantidade_kws_NI) ~ log(preco_kwh)+log(RENDA_DISP_PC)+ANOS_ESTUDO+n_moradores+media_idade+cond_ocup+sexo+raca,
    design = sub_design
  )
  
  # Extrair elasticidade (coeficiente)
  coef_est <- coef(modelo)["log(preco_kwh)"]
  se_est   <- coef(summary(modelo))[, "Std. Error"]["log(preco_kwh)"]
  ic_low   <- coef_est - 1.96 * se_est
  ic_high  <- coef_est + 1.96 * se_est
  
  # Voltar como tibble
  tibble(
    grupo = grupo_nome,
    elasticidade = coef_est,
    se = se_est,
    ic_inf = ic_low,
    ic_sup = ic_high
  )
})



################# 4. Base de Dados e Grafico ##################



resultados <-  bind_rows(resultados1, resultados2) %>%
  mutate(
    grupo_label = case_when(
      grupo == "homem_branco_renda_baixa" ~ "Homem branco (renda baixa)",
      grupo == "mulher_negra_renda_media" ~ "Mulher negra (renda média)",
      grupo == "homem_branco_renda_media" ~ "Homem branco (renda média)",
      grupo == "mulher_branca_renda_alta" ~ "Mulher branca (renda alta)",
      grupo == "homem_branco_renda_alta"  ~ "Homem branco (renda alta)",
      grupo == "urbano" ~ "Zona urbana",
      grupo == "rural" ~ "Zona rural",
      TRUE ~ grupo
    ),
    grupo_label = factor(grupo_label, levels = c(
      "Homem branco (renda baixa)",
      "Mulher negra (renda média)",
      "Homem branco (renda média)",
      "Mulher branca (renda alta)",
      "Homem branco (renda alta)",
      "Zona urbana",
      "Zona rural"
    ))
  )




ggplot(resultados, aes(x = grupo_label, y = elasticidade)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  # Rótulos de elasticidade dentro das barras
  geom_text(
    aes(label = sprintf("%.2f", elasticidade)),
    vjust = 2, # 
    hjust = -2, # ajusta posição
    color = "black",
    fontface = "bold",
    size = 3.5
  ) +
  labs(
    title = "Elasticidade-preço da demanda por energia elétrica",
    x = NULL,
    y = "Elasticidade (log-log)"
  ) +
  theme_minimal(base_size = 12) +
  coord_flip()

ggsave("output/elasticidades.png", width = 8, height = 6)

################# 4. Tabelas com as Infos ##################


colunas_grupos <- c(
  "mulher_negra_renda_media",
  "homem_branco_renda_media",
  "homem_branco_renda_alta",
  "mulher_branca_renda_alta",
  "rural",
  "urbano"
)

stats <- map_dfr(colunas_grupos, function(var) {
  subdesign <- subset(design_complete, get(var) == TRUE & !is.na(quantidade_kws))
  resultado1 <- svymean(~quantidade_kws, subdesign, na.rm = TRUE)
  resultado2 <- svymean(~RENDA_DISP_PC, subdesign, na.rm = TRUE)
  resultado3 <- svymean(~gastos_habitacao, subdesign, na.rm = TRUE)
  total_fam    <- svytotal(~I(!is.na(gastos_totais)), subdesign)  # ponderado
  
  tibble(
    grupo = var,
    media_kwh = as.numeric(coef(resultado1)),
    media_renda = as.numeric(coef(resultado2)),
    media_gastos = as.numeric(coef(resultado3)),
    n_familias = as.numeric(coef(total_fam)[[2]])
  )
})


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
    )) %>% 
  left_join(.,resultados,by="grupo") 

names(stats)

###################### Impacto tarifario #####################################


p<-0.7

tarifa_vermelha_2<-0.07877

vermelha_2<-stats %>% 
  select(grupo,grupo_label,categoria,media_kwh,media_renda,media_gastos,media_contas,elasticidade,n_familias) %>% 
  mutate(preco=p) %>% 
  mutate(preco_novo=preco+tarifa_vermelha_2) %>% 
  mutate(pct_aumento_preco=100*tarifa_vermelha_2/p)%>% 
  mutate(pct_consumo_after=elasticidade*pct_aumento_preco)



vermelha_2<-vermelha_2 %>% 
  mutate(consumo_after=(1+pct_consumo_after/100)*media_kwh) %>% 
  mutate(gasto_antes=preco*media_kwh) %>% 
  mutate(gasto_depois=preco_novo*consumo_after)


vermelha_2<-vermelha_2 %>% 
  mutate(abs_dif=gasto_depois-gasto_antes) %>% 
  mutate(pct_dif_renda=abs_dif/media_renda) %>%
  mutate(pct_dif_gastos=abs_dif/media_gastos) %>% 
  mutate(pct_dif=(gasto_depois-gasto_antes)/gasto_antes)%>% 
  select(grupo,grupo_label,categoria,n_familias,starts_with("pct_dif"),abs_dif)


tarifa_vermelha_1<-0.04463


vermelha_1<-stats %>% 
  select(grupo,grupo_label,categoria,media_kwh,media_renda,media_gastos,media_contas,elasticidade) %>% 
  mutate(preco=p) %>% 
  mutate(preco_novo=preco+tarifa_vermelha_1) %>% 
  mutate(pct_aumento_preco=100*tarifa_vermelha_1/p)%>% 
  mutate(pct_consumo_after=elasticidade*pct_aumento_preco)



vermelha_1<-vermelha_1 %>% 
  mutate(consumo_after=(1+pct_consumo_after/100)*media_kwh) %>% 
  mutate(gasto_antes=preco*media_kwh) %>% 
  mutate(gasto_depois=preco_novo*consumo_after)


vermelha_1<-vermelha_1 %>% 
  mutate(abs_dif=gasto_depois-gasto_antes) %>% 
  mutate(pct_dif_renda=abs_dif/media_renda) %>%
  mutate(pct_dif_gastos=abs_dif/media_gastos) %>% 
  mutate(pct_dif=(gasto_depois-gasto_antes)/gasto_antes)%>% 
  select(grupo,starts_with("pct_dif"),abs_dif)




tarifa_amarela<-0.01885

amarela<-stats %>% 
  select(grupo,grupo_label,categoria,media_kwh,media_renda,media_gastos,media_contas,elasticidade) %>% 
  mutate(preco=p) %>% 
  mutate(preco_novo=preco+tarifa_amarela) %>% 
  mutate(pct_aumento_preco=100*tarifa_amarela/p)%>% 
  mutate(pct_consumo_after=elasticidade*pct_aumento_preco)



amarela<-amarela%>% 
  mutate(consumo_after=(1+pct_consumo_after/100)*media_kwh) %>% 
  mutate(gasto_antes=preco*media_kwh) %>% 
  mutate(gasto_depois=preco_novo*consumo_after)


amarela<-amarela %>% 
  mutate(abs_dif=gasto_depois-gasto_antes) %>% 
  mutate(pct_dif_renda=abs_dif/media_renda) %>%
  mutate(pct_dif_gastos=abs_dif/media_gastos) %>% 
  mutate(pct_dif=(gasto_depois-gasto_antes)/gasto_antes) %>% 
  select(grupo,starts_with("pct_dif"),abs_dif)




###################### Bind all #####################################

amarela_renamed <- amarela %>%
  rename_with(~ paste0(., "_amarela"), .cols = -grupo)

impacto <- vermelha_1 %>% 
  left_join(vermelha_2, by = "grupo", suffix = c("_vermelha_1", "_vermelha_2")) %>% 
  left_join(amarela_renamed, by = "grupo")


names(impacto)
impacto %>%
  ggplot(aes(x = grupo_label, y = pct_dif_amarela, fill = categoria)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = scales::percent(pct_dif_amarela, accuracy = 0.01)),
    vjust = -0.3,
    size = 4
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.01),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(values = c(
    "#B58900" ,  # amarelo queimado,
    "#F4B400" 
  )) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave("output/impact_amarela.png", width = 8, height = 6)

names(impacto)
impacto %>%
  ggplot(aes(x = grupo_label, y = pct_dif_vermelha_1, fill = categoria),alpha=0.4) +
  geom_col(position = position_dodge(width = 0.8),alpha=0.6) +
  geom_text(
    aes(label = scales::percent(pct_dif_vermelha_1, accuracy = 0.01)),
    vjust = -0.3,
    size = 4
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.01),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(values = c(
    "#CC0000",
    "#990000"  # vermelho forte# vermelho escuro
    
  )) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave("output/impact_vermelha_1.png", width = 8, height = 6)


impacto %>%
  ggplot(aes(x = grupo_label, y = pct_dif_vermelha_2, fill = categoria)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = scales::percent(pct_dif_vermelha_2, accuracy = 0.01)),
    vjust = -0.3,
    size = 4
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.01),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(values = c(
    "#CC0000",
    "#990000"  # vermelho forte# vermelho escuro
    
  )) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

ggsave("output/impact_vermelha_2.png", width = 8, height = 6)

##################### tabela impacto ####################################

table_impacto <- impacto %>% 
  select(grupo_label, categoria, starts_with("pct_dif_renda"), starts_with("pct_dif_gastos")) %>% 
  mutate(across(where(is.numeric), ~ . * 100))
# 1. Organizar a base
df_tab_impacto <- table_impacto %>% 
  select(
    categoria,
    grupo_label,
    pct_dif_gastos_amarela,
    pct_dif_renda_amarela,
    pct_dif_gastos_vermelha_1,
    pct_dif_renda_vermelha_1,
    pct_dif_gastos_vermelha_2,
    pct_dif_renda_vermelha_2,
  ) %>%
  rename(
    Categoria = categoria,
    Grupo = grupo_label,
    `Renda` = pct_dif_renda_vermelha_2,
    `Renda ` = pct_dif_renda_vermelha_1,
    `Renda  ` = pct_dif_renda_amarela,
    `Gastos totais` = pct_dif_gastos_vermelha_2,
    `Gastos totais ` = pct_dif_gastos_vermelha_1,
    `Gastos totais  ` = pct_dif_gastos_amarela
  )

# 2. Adicionar separadores por categoria
df_fmt_impacto <- df_tab_impacto %>%
  group_by(Categoria) %>%
  group_split() %>%
  map_dfr(~{
    cat_name <- unique(.x$Categoria)
    separador <- tibble(
      Categoria = cat_name,
      Grupo = paste0("▸ ", cat_name),
      `Renda` = NA,
      `Renda ` = NA,
      `Renda  ` = NA,
      `Gastos totais` = NA,
      `Gastos totais ` = NA,
      `Gastos totais  ` = NA
    )
    bind_rows(separador, .x)
  }) %>%
  ungroup() %>%
  select(-Categoria) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# 3. Criar labels de cabeçalho com superheaders descritivos
header_labels <- data.frame(
  col_keys = names(df_fmt_impacto),
  line2 = c("Grupo",
            "Bandeira Amarela", "Bandeira Amarela", 
            "Bandeira Vermelha I", "Bandeira Vermelha I",
            "Bandeira Vermelha II", "Bandeira Vermelha II"),
  line1 = c("", 
            "Renda (%)", "Renda (%)", "Renda (%)", 
            "Gastos totais (%)", "Gastos totais (%)", "Gastos totais (%)"),
  stringsAsFactors = FALSE
)

# 4. Criar flextable com superheaders
ft_impacto <- flextable(df_fmt_impacto) %>%
  set_header_df(mapping = header_labels, key = "col_keys") %>%
  merge_h(part = "header") %>%
  theme_booktabs() %>%
  bold(i = grepl("▸", df_fmt_impacto$Grupo), bold = TRUE) %>%
  align(align = "left", part = "all") %>%
  align(align = "center", part = "header") %>%
  autofit() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  padding(padding = 0, part = "all")

ft_impacto
# 5. Exportar para Word
doc <- read_docx() %>%
  body_add_par("Tabela: Impacto percentual da mudança de bandeira tarifária", style = "heading 1") %>%
  body_add_flextable(ft_impacto)

print(doc, target = "output/tabela_impacto_bandeira_completa.docx")


##################### absoluto familias #####################

absoluto <- impacto %>% 
  select(grupo_label, categoria,n_familias, starts_with("abs_dif"))

names(absoluto)
absoluto<-absoluto %>% 
  mutate(total_perdido=1*abs_dif_vermelha_1+1*abs_dif_vermelha_2+2*abs_dif_amarela) %>% 
  mutate(montante=(n_familias*total_perdido)/10^6) %>% 
  select(-starts_with("abs"))



library(dplyr)
library(flextable)
library(officer)

# Organizar tabela
df_tab <- absoluto %>%
  arrange(categoria) %>%
  rename(
    Categoria = categoria,
    Grupo = grupo_label,
    `Nº de famílias` = n_familias,
    `Gasto anual adicional (R$)` = total_perdido,
    `Gasto anual adicional total (milhões R$)` = montante
  )

# Adicionar linhas de separação por categoria
df_fmt <- df_tab %>%
  group_by(Categoria) %>%
  group_split() %>%
  purrr::map_dfr(~{
    cat_name <- unique(.x$Categoria)
    separador <- tibble(
      Categoria = cat_name,
      Grupo = paste0("▸ ", cat_name),
      `Nº de famílias` = NA,
      `Gasto anual adicional (R$)` = NA,
      `Gasto anual adicional total (milhões R$)` = NA
    )
    bind_rows(separador, .x)
  }) %>%
  ungroup() %>%
  select(-Categoria) %>%
  mutate(across(where(is.numeric), ~round(., 1)))

# Criar flextable
ft <- flextable(df_fmt) %>%
  bold(i = grepl("▸", df_fmt$Grupo), bold = TRUE) %>%
  align(align = "left", part = "all") %>%
  autofit() %>%
  set_table_properties(layout = "autofit") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  padding(padding = 0, part = "all")
ft
# Exportar para Word
doc <- read_docx() %>%
  body_add_par("Tabela: Impacto absoluto por grupo de famílias", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "output/tabela_impacto_absoluto.docx")
