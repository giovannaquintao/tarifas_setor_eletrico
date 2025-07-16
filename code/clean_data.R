library(tidyverse)

library(tidyverse)

DOMICILIO <-
  read.fwf("data/raw/DOMICILIO.txt"
           , widths = c(2,4,1,9,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,2,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,14,14,1
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "V0201", "V0202",
                           "V0203", "V0204", "V0205", "V0206", "V0207",
                           "V0208", "V0209", "V02101", "V02102",
                           "V02103", "V02104", "V02105", "V02111",
                           "V02112", "V02113", "V0212", "V0213",
                           "V02141", "V02142", "V0215", "V02161",
                           "V02162", "V02163", "V02164", "V0217",
                           "V0219", "V0220", "V0221", "PESO",
                           "PESO_FINAL", "V6199")
           , dec="."
  )
MORADOR <-
  read.fwf("data/raw/MORADOR.txt"
           , widths = c(2,4,1,9,2,1,2,2,1,2,2,4,3,1,1,
                        1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,
                        2,1,2,14,14,10,1,20,20,20,20)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE",
                           "V0306", "V0401", "V04021", "V04022", "V04023",
                           "V0403", "V0404", "V0405", "V0406", "V0407",
                           "V0408", "V0409", "V0410", "V0411", "V0412",
                           "V0413", "V0414", "V0415", "V0416",
                           "V041711", "V041712", "V041721", "V041722",
                           "V041731", "V041732", "V041741", "V041742",
                           "V0418", "V0419", "V0420", "V0421", "V0422",
                           "V0423", "V0424", "V0425", "V0426", "V0427",
                           "V0428", "V0429", "V0430", "ANOS_ESTUDO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL",
                           "NIVEL_INSTRUCAO", "RENDA_DISP_PC","RENDA_MONET_PC",
                           "RENDA_NAO_MONET_PC","DEDUCAO_PC"   )
           , dec="."
  )
View(MORADOR)
k<-MORADOR %>%
  filter(NUM_UC==2)
View(k)
names(k)
View(k)
View(k)
k<-MORADOR %>%
  filter(COD_UPA==110014602&NUM_DOM==5)
View(k)
names(k)
k<-MORADOR %>%
  mutate(id=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  filter(V0306==1) %>%
  mutate(sexo=ifelse(V0404==1, "Homem", "Mulher")) %>%
  mutate(raca=case_when(
    V0405==1 ~ "Branca",
    V0405%in%c(2,4) ~ "Negros",
    V0405==3 ~ "Amarela",
    V0405==5 ~ "Indígena",
    TRUE ~ "Sem declaração"
  )
  ) %>%
  select(id,sexo,raca,ANOS_ESTUDO,RENDA_DISP_PC,NIVEL_INSTRUCAO,RENDA_TOTAL,RENDA_MONET_PC,RENDA_NAO_MONET_PC,DEDUCAO_PC,PESO,PESO_FINAL)
MORADOR_2<-MORADOR %>%
  mutate(id=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  filter(V0306==1) %>%
  mutate(sexo=ifelse(V0404==1, "Homem", "Mulher")) %>%
  mutate(raca=case_when(
    V0405==1 ~ "Branca",
    V0405%in%c(2,4) ~ "Negros",
    V0405==3 ~ "Amarela",
    V0405==5 ~ "Indígena",
    TRUE ~ "Sem declaração"
  )
  ) %>%
  select(id,sexo,raca,ANOS_ESTUDO,RENDA_DISP_PC,NIVEL_INSTRUCAO,RENDA_TOTAL,RENDA_MONET_PC,RENDA_NAO_MONET_PC,DEDUCAO_PC,PESO,PESO_FINAL)
View(MORADOR_2)
MORADOR_2<-MORADOR %>%
  mutate(id=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  filter(V0306==1) %>%
  mutate(sexo=ifelse(V0404==1, "Homem", "Mulher")) %>%
  mutate(raca=case_when(
    V0405==1 ~ "Branca",
    V0405%in%c(2,4) ~ "Negro",
    V0405==3 ~ "Amarela",
    V0405==5 ~ "Indígena",
    TRUE ~ "Sem declaração"
  )
  ) %>%
  select(id,sexo,raca,ANOS_ESTUDO,RENDA_DISP_PC,NIVEL_INSTRUCAO,RENDA_TOTAL,RENDA_MONET_PC,RENDA_NAO_MONET_PC,DEDUCAO_PC,PESO,PESO_FINAL)
View(MORADOR_2)
MORADOR_2<-MORADOR %>%
  mutate(id=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  filter(V0306==1) %>%
  mutate(sexo=ifelse(V0404==1, "Homem", "Mulher")) %>%
  mutate(raca=case_when(
    V0405==1 ~ "Branca",
    V0405%in%c(2,4) ~ "Negro",
    V0405==3 ~ "Amarela",
    V0405==5 ~ "Indígena",
    TRUE ~ "Sem declaração"
  )
  ) %>%
  select(id,sexo,raca,ANOS_ESTUDO,RENDA_DISP_PC,NIVEL_INSTRUCAO,RENDA_TOTAL,PESO,PESO_FINAL)
View(MORADOR_2)
MORADOR_2<-MORADOR %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  filter(V0306==1) %>%
  mutate(sexo=ifelse(V0404==1, "Homem", "Mulher")) %>%
  mutate(raca=case_when(
    V0405==1 ~ "Branca",
    V0405%in%c(2,4) ~ "Negro",
    V0405==3 ~ "Amarela",
    V0405==5 ~ "Indígena",
    TRUE ~ "Sem declaração"
  )
  ) %>%
  select(id_dom,sexo,raca,ANOS_ESTUDO,RENDA_DISP_PC,NIVEL_INSTRUCAO,RENDA_TOTAL,PESO,PESO_FINAL)
select(id_dom, rede_geral, outra_origem, energia_integral,loc_dom)
DOMICILIO<-DOMICILIO %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  mutate(rede_geral=ifelse(V0211==1, "Sim", "Não")) %>%
  mutate(outra_origem=ifelse(V0212==1, "Sim", "Não")) %>%
  mutate(energia_integral=ifelse(V0215==1, "Sim", "Não")) %>%
  mutate(loc_dom=case_when(
    TIPO_SITUACAO_REG==1 ~ "Urbano",
    TIPO_SITUACAO_REG==2 ~ "Rural"
  )) %>%
  select(id_dom, rede_geral, outra_origem, energia_integral,loc_dom)
DOMICILIO <-
  read.fwf("data/raw/DOMICILIO.txt"
           , widths = c(2,4,1,9,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,2,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,14,14,1
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "V0201", "V0202",
                           "V0203", "V0204", "V0205", "V0206", "V0207",
                           "V0208", "V0209", "V02101", "V02102",
                           "V02103", "V02104", "V02105", "V02111",
                           "V02112", "V02113", "V0212", "V0213",
                           "V02141", "V02142", "V0215", "V02161",
                           "V02162", "V02163", "V02164", "V0217",
                           "V0219", "V0220", "V0221", "PESO",
                           "PESO_FINAL", "V6199")
           , dec="."
  )
DOMICILIO_2<-DOMICILIO %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  mutate(rede_geral=ifelse(V0211==1, "Sim", "Não")) %>%
  mutate(outra_origem=ifelse(V0212==1, "Sim", "Não")) %>%
  mutate(energia_integral=ifelse(V0215==1, "Sim", "Não")) %>%
  mutate(loc_dom=case_when(
    TIPO_SITUACAO_REG==1 ~ "Urbano",
    TIPO_SITUACAO_REG==2 ~ "Rural"
  )) %>%
  select(id_dom, rede_geral, outra_origem, energia_integral,loc_dom)
names(DOMICILIO)
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
DESPESA_COLETIVA <-
  read.fwf("DESPESA_COLETIVA.txt"
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
names(DESPESA_COLETIVA)
table(despesa_coletiva$V9001)
table(DESPESA_COLETIVA$V9001)
x<-DESPESA_COLETIVA %>%
  filter(V9001==000101)
table(DESPESA_COLETIVA$QUADRO)
table(DESPESA_COLETIVA$V9005)
x<-DESPESA_COLETIVA %>%
  filter(V9001==600201)
x<-DESPESA_COLETIVA %>%
  filter(V9001==600201) %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>%
  mutate(despesa_coletiva= sum(V8000_DEFLA, na.rm = TRUE) )
tablle(x$despesa_coletiva)
table(x$despesa_coletiva)
mean(x$despesa_coletiva)
max(x$despesa_coletiva)
hist(x$despesa_coletiva)
x<-DESPESA_COLETIVA %>%
  filter(V9001==600201)
hist(x$V8000_DEFLA, breaks = 100, main = "Histograma de Despesa Coletiva (R$)", xlab = "Despesa Coletiva (R$)")
x<-DESPESA_COLETIVA %>%
  filter(V9001==600401)
mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>%
  mutate(despesa_coletiva= sum(V8000_DEFLA, na.rm = TRUE) )
hist(x$V8000_DEFLA, breaks = 100, main = "Histograma de Despesa Coletiva (R$)", xlab = "Despesa Coletiva (R$)")
mean(x$V8000_DEFLA, na.rm = TRUE)
percent_rank(x$V8000_DEFLA, na.rm = TRUE)
quantile(x$V8000_DEFLA, na.rm = TRUE)
DESPESA_COLETIVA_2 <- DESPESA_COLETIVA %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>%
  mutate(despesa_coletiva= sum(V8000_DEFLA, na.rm = TRUE)*FATOR_ANUALIZACAO )
CADERNETA_COLETIVA <-
  read.fwf("data/raw/CADERNETA_COLETIVA.txt"
           , widths = c(2,4,1,9,2,1,2,3,7,2,10,12,10,1,2,14,14,10,
                        9,4,5,9,5
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9002", "V8000", "DEFLATOR",
                           "V8000_DEFLA", "COD_IMPUT_VALOR",
                           "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL",
                           "V9005", "V9007", "V9009", "QTD_FINAL","V9004")
           , dec="."
  )
CADERNETA_COLETIVA_2 <- CADERNETA_COLETIVA %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>%
  mutate(caderneta_coletiva= sum(V8000_DEFLA, na.rm = TRUE)*FATOR_ANUALIZACAO ) %>%
  select(id_dom,id_uc,caderneta_coletiva)
ALUGUEL_ESTIMADO <-
  read.fwf("data/raw/ALUGUEL_ESTIMADO.txt"
           , widths = c(2,4,1,9,2,1,2,7,2,10,2,2,12,10,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "V9001", "V9002", "V8000", "V9010", "V9011",
                           "DEFLATOR", "V8000_DEFLA", "COD_IMPUT_VALOR",
                           "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL")
           , dec="."
  )
table(df$v8000_DEFLA)
table(ALUGUEL_ESTIMADO$v8000_DEFLA)
table(ALUGUEL_ESTIMADO$V8000_DEFLA)
hist(ALUGUEL_ESTIMADO$V8000_DEFLA, breaks = 100, main = "Histograma de Aluguel Estimado (R$)", xlab = "Aluguel Estimado (R$)")
quantile(ALUGUEL_ESTIMADO$V8000_DEFLA, na.rm = TRUE)
weighted.mean(ALUGUEL_ESTIMADO$V8000_DEFLA, ALUGUEL_ESTIMADO$PESO_FINAL, na.rm = TRUE)
ALUGUEL_ESTIMADO_2<-ALUGUEL_ESTIMADO %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>%
  mutate(aluguel_estimado= sum(V8000_DEFLA, na.rm = TRUE)*FATOR_ANUALIZACAO ) %>%
  select(id_dom,id_uc,aluguel_estimado)
DESPESA_INDIVIDUAL <-
  read.fwf("DESPESA_INDIVIDUAL.txt"
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V9002", "V8000", "V9010", "V9011", "V9012",
                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )
ALUGUEL_ESTIMADO_2<-ALUGUEL_ESTIMADO %>%
  mutate(id_dom=paste0(COD_UPA,"_",NUM_DOM)) %>%
  mutate(id_uc=paste0(COD_UPA,"_",NUM_DOM,"_",NUM_UC)) %>%
  group_by(id_uc) %>%
  mutate(aluguel_estimado= sum(V8000_DEFLA, na.rm = TRUE)*FATOR_ANUALIZACAO ) %>%
  select(id_dom,id_uc,aluguel_estimado,PESO_FINAL)
weighted.mean(ALUGUEL_ESTIMADO$V8000_DEFLA, ALUGUEL_ESTIMADO$PESO_FINAL, na.rm = TRUE)
weighted.mean(ALUGUEL_ESTIMADO_2$aluguel_estimado, ALUGUEL_ESTIMADO_2$PESO_FINAL, na.rm = TRUE)
DESPESA_INDIVIDUAL <-
  read.fwf("DESPESA_INDIVIDUAL.txt"
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V9002", "V8000", "V9010", "V9011", "V9012",
                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )
DESPESA_INDIVIDUAL <-
  read.fwf("data/raw/DESPESA_INDIVIDUAL.txt"
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V9002", "V8000", "V9010", "V9011", "V9012",
                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )
x<-DESPESA_COLETIVA %>%
  filter(V9001==600201) %>%
  mutate(agua_esgoto_anual= sum(V8000_DEFLA, na.rm = TRUE)*FATOR_ANUALIZACAO )
agua_esgoto_mensal<- weighted.mean(x$agua_esgoto_anual, x$PESO_FINAL, na.rm = TRUE)/12
agua_esgoto_mensal
table(x$agua_esgoto_anual)
x<-DESPESA_COLETIVA %>%
  filter(V9001==600201) %>%
  mutate(agua_esgoto_anual= V8000_DEFLA*FATOR_ANUALIZACAO )
table(x$agua_esgoto_anual)
agua_esgoto_mensal<- weighted.mean(x$agua_esgoto_anual, x$PESO_FINAL, na.rm = TRUE)/12
agua_esgoto_mensal
