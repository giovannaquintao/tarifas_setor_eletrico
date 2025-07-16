library(tidyverse)

DESPESA_COLETIVA <- 
  read.fwf("data/raw/DESPESA_COLETIVA.txt"
           , widths = c(2,4,1,9,2,1,2,2,7,2,4,10,2,2,1
                        ,10,1,12,10,10,1,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9002", "V9005", "V8000",
                           "V9010", "V9011", "V9012", "V1904",
                           "V1905", "DEFLATOR", "V8000_DEFLA",
                           "V1904_DEFLA", "COD_IMPUT_VALOR",
                           "COD_IMPUT_QUANTIDADE", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )

table(DESPESA_COLETIVA$V9001)

DESPESA_COLETIVA <- DESPESA_COLETIVA %>%
  mutate(renda_grupo = ntile(RENDA_TOTAL, 3)) %>%
  mutate(renda_grupo = factor(renda_grupo,
                              levels = 1:3,
                              labels = c("Baixa", "Média", "Alta")))

k<-DESPESA_COLETIVA %>% 
  filter(V9001==600101)

library(survey)
options(survey.lonely.psu = "adjust")

despesa_design <- svydesign(id = ~COD_UPA,
                            strata = ~ESTRATO_POF,
                            weights = ~PESO_FINAL,
                            data = k,
                            nest = TRUE)

# 3. Calcular média e CV da variável V9001 por grupo de renda

resultado <- svyby(~V9005,
                   ~renda_grupo,
                   design = despesa_design,
                   FUN = svymean,
                   na.rm = TRUE,
                   vartype = c("cv")) # inclui o coeficiente de variação
resultado
