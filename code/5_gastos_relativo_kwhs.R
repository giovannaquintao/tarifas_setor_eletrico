
library(tidyverse)
library(survey)
library(kableExtra)

rm(list=ls())
gc()


################### 1. Calculating average ################################

base_final<-read_csv("data/clean/base_final.csv")


#remove missings
base_final<-base_final %>% 
  filter(is.na(despesa_energia)==F&despesa_energia>0&RENDA_TOTAL>0) %>% 
  mutate(pct_energia_consumo=despesa_energia/gastos_habitacao,
         pct_energia_renda=despesa_energia/RENDA_TOTAL)


options(survey.lonely.psu = "adjust")  
# Grupos
colunas_grupos <- c(
 "homem_negro_ref", "mulher_negra_ref", 
  "homem_branco_ref", "mulher_branca_ref",
  "renda_pc_ate_05", "renda_pc_05a3", "renda_pc_mais3", "rural", "urbano",
  "mulher_negra_renda_media", "homem_branco_renda_media", "homem_branco_renda_alta",  "mulher_branca_renda_alta"
)

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



stats <- map_dfr(colunas_grupos, function(var) {
  # subset com a condição do grupo == TRUE
  subdesign <- subset(design, get(var) == TRUE)
  
  # calcular média e erro padrão
  resultado1 <- svymean(~pct_energia_consumo, subdesign, na.rm = TRUE)
  resultado2 <- svymean(~pct_energia_renda,subdesign, na.rm = TRUE)
  
  
  media1 <- coef(resultado1)
  erro1 <- SE(resultado1)
  cv1    <- 100 * erro1 / media1
  
  
  media2 <- coef(resultado2)
  erro2 <- SE(resultado2)
  cv2   <- 100 * erro2 / media2
  
  tibble(
    grupo = var,
    media_consumo = as.numeric(media1),
    erro_padrao_g = as.numeric(erro1),
    coef_var_perc_g = as.numeric(cv1),
    media_renda = as.numeric(media2),
    erro_padrao_r = as.numeric(erro2),
    coef_var_perc_r = as.numeric(cv2)
  )
  
  
})


################### 2. Generating Table  ################################
stats <- stats %>%
  mutate(
    ic_lower_g = media_consumo - 1.96 * erro_padrao_g,
    ic_upper_g = media_consumo + 1.96 * erro_padrao_g,
    ic_lower_r = media_renda - 1.96 * erro_padrao_r,
    ic_upper_r = media_renda + 1.96 * erro_padrao_r,
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
  ) %>% 
  select(grupo_label,categoria,starts_with("media"),starts_with("ic"))



df_long <- stats %>%
  pivot_longer(cols = c(media_consumo, media_renda),
               names_to = "variavel", values_to = "media") %>%
  mutate(ic_lower = ifelse(variavel == "media_consumo", ic_lower_g, ic_lower_r),
         ic_upper = ifelse(variavel == "media_consumo", ic_upper_g, ic_upper_r),
         variavel = recode(variavel,
                           "media_consumo" = "Gasto Energia sobre Gastos com Habitação (%)",
                           "media_renda" = "Gasto Energia sobre Renda Total (%)"))


################### 3. Generating Graphs ################################

k<-df_long %>% filter(categoria == "Renda") %>% 
  mutate(grupo_label = fct_reorder(grupo_label, media, .desc = TRUE))


ggplot(k)+ 
  aes(x = grupo_label, y = media, fill = variavel) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = ic_lower, ymax = ic_upper),
                position = position_dodge(width = 0.8), width = 0.2) +
  geom_text(aes(label = scales::percent(media, accuracy = 0.1)),
            vjust = 4,            # empurra o texto para dentro da barra
            color = "black" ,
            size = 3.6) +
  facet_wrap(~variavel, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



ggsave("output/renda_consumo_relativo.png", width = 8, height = 6)


k<-df_long %>% filter(categoria == "Localidade") %>% 
  mutate(grupo_label = fct_reorder(grupo_label, media, .desc = TRUE))



ggplot(k)+ 
  aes(x = grupo_label, y = media, fill = variavel) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = ic_lower, ymax = ic_upper),
                position = position_dodge(width = 0.8), width = 0.2) +
  facet_wrap(~variavel, scales = "free_y") +
  geom_text(aes(label = scales::percent(media, accuracy = 0.1)),
            vjust = 4,             # empurra o texto para dentro da barra
            color = "black" ,
            size = 3.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



ggsave("output/localidade_consumo_relativo.png", width = 8, height = 6)


genero_raca <- df_long %>%
  filter(categoria == "Gênero/Raça") %>%
  filter(!grupo_label%in%c("Homem ","Mulher ")) %>% 
  mutate(
    sexo = case_when(
      str_detect(grupo_label, "Homem") ~ "Homem",
      str_detect(grupo_label, "Mulher") ~ "Mulher",
      TRUE ~ "Ambos"
    ))

genero_raca
ggplot(genero_raca %>% 
         filter(categoria == "Gênero/Raça") %>% 
         mutate(grupo_label = fct_reorder(grupo_label, media, .desc = TRUE)), 
       aes(x = grupo_label, y = media, fill = variavel, alpha = sexo)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = ic_lower, ymax = ic_upper),
                position = position_dodge(width = 0.8), width = 0.2) +
  facet_wrap(~variavel, scales = "free_y") +
  geom_text(aes(label = scales::percent(media, accuracy = 0.1)),
            vjust = 5,           # empurra o texto para dentro da barra
            color = "black" ,
            size = 3.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set2") +
  scale_alpha_manual(values = c("Mulher" = 1, "Homem" = 0.6)) +
  labs(x = NULL, y = NULL, fill = NULL, alpha = "Sexo") +  # <- aqui define o nome da legenda do alpha
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



ggsave("output/genero_consumo_relativo.png", width = 8, height = 6)


genero_raca_renda <- df_long %>%
  filter(categoria == "Renda/Gênero/Raça") %>%
  filter(!grupo_label%in%c("Homem ","Mulher ")) %>% 
  mutate(
    sexo = case_when(
      str_detect(grupo_label, "Homem") ~ "Homem",
      str_detect(grupo_label, "Mulher") ~ "Mulher",
      TRUE ~ "Ambos"
    ))


ggplot(genero_raca_renda %>% 
         mutate(grupo_label = fct_reorder(grupo_label, media, .desc = TRUE)), 
       aes(x = grupo_label, y = media, fill = variavel, alpha = sexo)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = ic_lower, ymax = ic_upper),
                position = position_dodge(width = 0.8), width = 0.2) +
  facet_wrap(~variavel, scales = "free_y") +
  geom_text(aes(label = scales::percent(media, accuracy = 0.1)),
            vjust = 4,          # empurra o texto para dentro da barra
            color = "black" ,
            size = 3.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set2") +
  scale_alpha_manual(values = c("Mulher" = 1, "Homem" = 0.6)) +
  labs(x = NULL, y = NULL, fill = NULL, alpha = "Sexo") +  # <- aqui define o nome da legenda do alpha
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8),
        legend.position = "none")



ggsave("output/genero_raca_renda_consumo_relativo.png", width = 9, height = 6)


