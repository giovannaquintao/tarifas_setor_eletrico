
library(tidyverse)
library(survey)
library(kableExtra)

rm(list=ls())
gc()


base_final<-read_csv("data/clean/base_final.csv")

mean(base_final$RENDA_TOTAL)
names(base_final)
base_final<-base_final %>% 
  mutate(pct_energia_consumo=100*despesa_energia/gastos_totais,
         pct_energia_renda=100*despesa_energia/RENDA_TOTAL)

mean(base_final$pct_energia_renda, na.rm = TRUE)

design <- svydesign(
  id = ~COD_UPA,
  strata = ~ESTRATO_POF,
  weights = ~PESO_FINAL,
  data = base_final,
  nest = TRUE
)


# calcular média e erro padrão
resultado1 <- svymean(~pct_energia_consumo, design, na.rm = TRUE)
resultado2 <- svymean(~pct_energia_renda, design, na.rm = TRUE)

resultado1
resultado2
##################### gasto kws ######################

stats <- map_dfr(colunas_grupos, function(var) {
  # subset com a condição do grupo == TRUE
  subdesign <- subset(design, get(var) == TRUE & !is.na(gastos_totais))
  
  # calcular média e erro padrão
  resultado <- svymean(~quantidade_kws, subdesign, na.rm = TRUE)
  
  media <- coef(resultado)
  erro  <- SE(resultado)
  cv    <- 100 * erro / media
  
  tibble(
    grupo = var,
    media_gastos = as.numeric(media),
    erro_padrao = as.numeric(erro),
    coef_var_perc = as.numeric(cv)
  )
})
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
      #   grupo == "negra_ref" ~ "Pessoa negra ",
      grupo == "homem_negro_ref" ~ "Homem negro ",
      grupo == "mulher_negra_ref" ~ "Mulher negra ",
      grupo == "homem_branco_ref" ~ "Homem branco ",
      grupo == "mulher_branca_ref" ~ "Mulher branca ",
      grupo == "renda_pc_ate_05" ~ "Até 0,5 SM pc",
      grupo == "renda_pc_05a3" ~ "De 0,5 a 3 SM pc",
      grupo == "renda_pc_mais3" ~ "Acima de 3 SM pc",
      grupo == "rural" ~ "Zona rural",
      grupo == "urbano" ~ "Zona urbana",
      TRUE ~ grupo
    )
  )

# Gerar os gráficos

stats %>%
  filter(categoria == "Renda") %>%
  ggplot(aes(x = reorder(grupo_label, media_gastos), y = media_gastos)) +
  geom_col(fill = "#5ab4ac") +
  geom_errorbar(aes(ymin = ic_lower, ymax = ic_upper), width = 0.2) +
  labs(
    title = paste("Gasto Energia (kWh) por renda"),
    x = NULL,
    y = "Gasto Energia Mensal (kWh)"
  ) +
  theme_minimal(base_size = 13)


ggsave("output/renda_kWh.png")


stats %>%
  filter(categoria == "Localidade") %>%
  ggplot(aes(x = reorder(grupo_label, media_gastos), y = media_gastos)) +
  geom_col(fill = "#d8b365") +
  geom_errorbar(aes(ymin = ic_lower, ymax = ic_upper), width = 0.2) +
  labs(
    title = paste("Gasto Energia (kWh) por localidade"),
    x = NULL,
    y = "Gasto Energia Mensal (kWh)"
  ) +
  theme_minimal(base_size = 13)


ggsave("output/location_kWh.png")



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
  scale_fill_brewer(type="qual")+
  scale_alpha_manual(values = c("Mulher" = 1, "Homem" = 0.6, "Ambos" = 0.8)) +
  labs(
    title = "Gasto Energia (kWh) por raça e sexo",
    x = NULL,
    y = "Gasto Energia Mensal (kWh)",
    fill = "Sexo",
    alpha = "Sexo"
  ) +
  theme_minimal(base_size = 13)

ggsave("output/sex_kwh.png")
