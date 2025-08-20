#Calculate the spending per family
library(tidyverse)
library(survey)
library(kableExtra)

rm(list=ls())
gc()


DOMICILIO <- readRDS("data/clean/DOMICILIO.rds")
MORADOR <- readRDS("data/clean/MORADOR.rds")
DESPESA_COLETIVA <- readRDS("data/clean/DESPESA_COLETIVA.rds")
DESPESA_INDIVIDUAL <- readRDS("data/clean/DESPESA_INDIVIDUAL.rds")
CADERNETA_COLETIVA <- readRDS("data/clean/CADERNETA_COLETIVA.rds")
ALUGUEL_ESTIMADO <- readRDS("data/clean/ALUGUEL_ESTIMADO.rds")
OUTROS_RENDIMENTOS <- readRDS("data/clean/OUTROS_RENDIMENTOS.rds")
RENDIMENTO_TRABALHO <- readRDS("data/clean/RENDIMENTO_TRABALHO.rds")

################### 1. Morador ################################

# V0306	Condição na Unidade de Consumo.
# 	01 – Pessoa de referência da UC 

# V0404	Sexo
# 1 – Homem
# 2 – Mulher

# V0405	Cor ou raça	
# 1 – Branca
# 2 – Preta
# 3 – Amarela
# 4 – Parda
# 5 – Indígena
# 9 – Sem declaração



MORADOR_2<-MORADOR %>%
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  #filter only the reference person of the household
  filter(V0306==1) %>%
  #clean sexo and raca
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



# RENDA_TOTAL	 Valor em reais (R$), considerando os centavos, do rendimento bruto total mensal da Unidade de Consumo. 
# O rendimento total é obtido através do somatório dos rendimentos brutos monetários mensais de todos os moradores da Unidade de Consumo, obtidos através do trabalho,
# transferências e outras rendas, mais a parcela relativa aos rendimentos não monetários mensais do domicílio, acrescido da variação patrimonial, que compreende vendas de imóveis,
# recebimentos de heranças e o saldo positivo da movimentação financeira. 



#RENDA_DISP_PC	Renda disponível familiar per capita.
# É a divisão do total da renda disponível da Unidade de Consumo pelo total de moradores da Unidade de Consumo.
# Considerou-se como renda disponível a soma dos rendimentos monetários e não monetários menos impostos diretos, contribuições sociais, e outras deduções compulsórias ou quase compulsórias.
# A renda disponível familiar per capita pode ser negativa, pois as contas englobam deduções pesquisadas nos questionários POF 2, POF 4 e POF 5 (ver arquivo Renda_disponível_dedução_micro_v1). 
# Moradores com condição na família “empregado doméstico” e “parente de empregado doméstico” são excluídos de todas as etapas de construção da variável.
# 



################### 1b. Moradores do Dom ################################

# V0306	Condição na Unidade de Consumo.
# 	01 – Pessoa de referência da UC 

# V0404	Sexo
# 1 – Homem
# 2 – Mulher

# V0405	Cor ou raça	
# 1 – Branca
# 2 – Preta
# 3 – Amarela
# 4 – Parda
# 5 – Indígena
# 9 – Sem declaração



MORADOR_3<-MORADOR %>%
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>%
  summarise(n_moradores=n(),
            media_idade=mean(V0403,na.rm=T)) %>% 
  ungroup()


################### 2. Domicilio ################################

# V02141	A energia elétrica utilizada neste domicílio é proveniente de rede geral?	
# 1 – Sim
# 2 – Não
# V02142	A energia elétrica utilizada neste domicílio é proveniente de outra origem (gerador, placa solar, eólica, etc.)?	
# 1 – Sim
# 2 – Não
# V0215	Com que frequência a energia elétrica, proveniente de rede geral, está habitualmente disponível para este domicílio?	
#   Branco – Não Aplicável
# 1 – Diária, em tempo integral
# 2 – Diária, por algumas horas
# 3 – Outra frequência

# TIPO_SITUACAO_REG	Situação do Domicílio
# 1 – Urbano
# 2 – Rural 


# V0217	Este domicílio é:	
# 1 – Próprio de algum morador – já pago
# 2 – Próprio de algum morador – ainda pagando
# 3 – Alugado
# 4 – Cedido por empregador
# 5 – Cedido por familiar
# 6 – Cedido de outra forma
# 7 – Outra condição


# V0201	Este domicílio é do tipo:
# 1 – Casa
# 2 – Apartamento
# 3 – Habitação em casa de cômodos, cortiço ou cabeça de porco


DOMICILIO_2 <- DOMICILIO %>%
  mutate(id_dom = paste0(COD_UPA, "_", NUM_DOM)) %>%
  mutate(
    rede_geral       = ifelse(V02141 == 1, "Sim", "Não"),
    outra_origem     = ifelse(V02142 == 1, "Sim", "Não"),
    energia_integral = ifelse(V0215 == 1, "Sim", "Não"),
    loc_dom = case_when(
      TIPO_SITUACAO_REG == 1 ~ "Urbano",
      TIPO_SITUACAO_REG == 2 ~ "Rural"
    ),
    tipo_dom = case_when(
      V0201 == 1 ~ "Casa",
      V0201 == 2 ~ "Apartamento",
      V0201 == 3 ~ "Habitação em cômodos / cortiço / cabeça de porco",
      TRUE       ~ NA_character_
    ),
    cond_ocup = case_when(
      V0217 == 1 ~ "Próprio – já pago",
      V0217 == 2 ~ "Próprio – ainda pagando",
      V0217 == 3 ~ "Alugado",
      V0217 == 4 ~ "Cedido por empregador",
      V0217 == 5 ~ "Cedido por familiar",
      V0217 == 6 ~ "Cedido de outra forma",
      V0217 == 7 ~ "Outra condição",
      TRUE       ~ NA_character_
    )
  ) %>%
  select(id_dom, UF, rede_geral, outra_origem, energia_integral, loc_dom, tipo_dom, cond_ocup)

################### 3. Despesas Coletivas ################################

# 600101	ENERGIA ELETRICA (KWH)
# 700101	GAS DE BOTIJAO (COMBUSTIVEL DOMESTICO)

# V9005	Quantidade final consumida em kilowatt (kwh), após os procedimentos de imputação. 
# Aplicável apenas para o item Energia Elétrica (V9001=600101). Para os outros itens, este campo está em branco. 


# COD_IMPUT_QUANTIDADE	Indica se a quantidade consumida em kilowatt (kwh) foi imputada. 
# Aplicável apenas para o item Energia Elétrica (V9001=600101). Para os outros itens, este campo está em branco. 


#caclculando gastos com habitacao (despesa coletiva)
#condominio
#servicoes e taxas (energia, agua, esgoto, etc)
#manutencao do lar
#artigos de limpeza
#eletrodomesticos
#consertos artigos do lar

DESPESA_COLETIVA_2 <- DESPESA_COLETIVA %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    #despesa anualizada
    V8000_DEFLA_anual  = ifelse( QUADRO==10|QUADRO==19,
                                 (V8000_DEFLA*V9011*FATOR_ANUALIZACAO), 
                                 (V8000_DEFLA*FATOR_ANUALIZACAO)
    ),
    inss_anual=(V1904_DEFLA*V9011*FATOR_ANUALIZACAO)) %>% 
  group_by(id_uc) %>%
  summarise(
    despesa_coletiva = sum(V8000_DEFLA_anual, na.rm = TRUE),
    despesa_energia=sum(V8000_DEFLA_anual[V9001==600101],na.rm=T),
    despesa_gas=sum(V8000_DEFLA_anual[V9001==700101],na.rm=T),
    inss_anual = sum(inss_anual, na.rm = TRUE),
    quantidade_kws=sum(V9005,na.rm=T),
    quantidade_kws_NI=sum(V9005[COD_IMPUT_QUANTIDADE==0],na.rm=T),
    .groups = "drop"
  ) %>% 
  ungroup()




################### 4. Despesa Individual ################################

DESPESA_INDIVIDUAL_2 <- DESPESA_INDIVIDUAL %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    V8000_DEFLA_anual = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50,
                                (V8000_DEFLA*V9011*FATOR_ANUALIZACAO), 
                                (V8000_DEFLA*FATOR_ANUALIZACAO)
                                
    ),
  ) %>%
  group_by(id_uc) %>%
  summarise(
    despesa_individual = sum(V8000_DEFLA_anual, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ungroup()

################### 5. Caderneta Coletiva ################################

CADERNETA_COLETIVA_2 <- CADERNETA_COLETIVA %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    V8000_DEFLA_anual = V8000_DEFLA * FATOR_ANUALIZACAO
  ) %>%
  group_by(id_uc) %>%
  summarise(
    caderneta_coletiva = sum(V8000_DEFLA_anual, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ungroup()

################### 6. Aluguel Estimado ################################


ALUGUEL_ESTIMADO_2<-ALUGUEL_ESTIMADO %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    ,V8000_DEFLA_anual = V8000_DEFLA *V9011*FATOR_ANUALIZACAO
  ) %>%
  group_by(id_uc) %>%
  summarise(
    aluguel = sum(V8000_DEFLA_anual, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ungroup()

################### 7. Outros Rendimentos ################################


OUTROS_RENDIMENTOS_2<-OUTROS_RENDIMENTOS %>%
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    ,   deducao_anual = ifelse( QUADRO==54,
                                (V8501_DEFLA*V9011*FATOR_ANUALIZACAO), 
                                (V8501_DEFLA*FATOR_ANUALIZACAO)
    ) 
  ) %>%
  group_by(id_uc) %>%
  summarise(
    deducao_anual = sum(deducao_anual, na.rm = TRUE),
    .groups = "drop"
  )
################### 8. Rendimento do Trabalho ################################


RENDIMENTO_TRABALHO_2 <- RENDIMENTO_TRABALHO %>%
  
  mutate(
    id_dom = paste0(COD_UPA, "_", NUM_DOM),
    id_uc  = paste0(COD_UPA, "_", NUM_DOM, "_", NUM_UC),
    ,  prev_pub_anual=(V531112_DEFLA*V9011*FATOR_ANUALIZACAO),
    imp_renda_anual=(V531122_DEFLA*V9011*FATOR_ANUALIZACAO),
    iss_anual=(V531132_DEFLA*V9011*FATOR_ANUALIZACAO)
  ) %>%
  group_by(id_uc) %>%
  summarise(
    prev_pub_anual = sum( prev_pub_anual, na.rm = TRUE),
    imp_renda_anual = sum( imp_renda_anual, na.rm = TRUE),
    iss_anual = sum( iss_anual, na.rm = TRUE),
    .groups = "drop"
  )


################### 9. Bind All################################


base_final <- MORADOR_2 %>%
  left_join(DOMICILIO_2, by = "id_dom") %>%
  left_join(DESPESA_COLETIVA_2, by = "id_uc") %>%
  left_join(DESPESA_INDIVIDUAL_2, by = "id_uc") %>%
  left_join(RENDIMENTO_TRABALHO_2, by = "id_uc") %>%
  left_join(OUTROS_RENDIMENTOS_2, by = "id_uc") %>%
  left_join(CADERNETA_COLETIVA_2, by = "id_uc") %>%
  left_join(ALUGUEL_ESTIMADO_2, by = "id_uc") %>%
  left_join(MORADOR_3, by = "id_uc")





base_final <- base_final %>% 
  
  # 1. Transformar despesas anuais em mensais
  mutate(
    despesa_energia = despesa_energia / 12,
    aluguel = aluguel / 12,
    despesa_coletiva = despesa_coletiva / 12,
    caderneta_coletiva = caderneta_coletiva / 12,
    despesa_individual = despesa_individual / 12,
    despesa_gas = despesa_gas / 12
  ) %>% 
  # 2. Somar os componentes já mensais
  rowwise() %>%
  mutate(
    gastos_habitacao = sum(despesa_coletiva, aluguel, na.rm = TRUE),
    gastos_totais = sum(despesa_coletiva, caderneta_coletiva, aluguel, despesa_individual, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(preco_kwh = despesa_energia/quantidade_kws) 


salario_minimo <- 954
# valor do salário mínimo em 2018

#(https://www.ipeadata.gov.br/ExibeSerie.aspx?serid=1739471028)

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

base_final <- base_final %>%
  mutate(
    mulher_negra_renda_media = mulher_negra_ref & renda_pc_05a3,
    homem_branco_renda_media = homem_branco_ref & renda_pc_05a3,
    homem_branco_renda_alta = homem_branco_ref &  renda_pc_mais3,
    mulher_branca_renda_alta = mulher_branca_ref &  renda_pc_mais3
  )




write_csv(base_final, "data/clean/base_final.csv")