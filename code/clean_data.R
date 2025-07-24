library(tidyverse)

rm(list=ls())
gc()

address<-"C:/Users/giova/OneDrive/raw_data/ibge/pof/pof_17_18"

########################### open ################################################
DOMICILIO <-
  read.fwf(paste0(address,"/DOMICILIO.txt"),
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
  read.fwf(paste0(address,"/MORADOR.txt"),
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

DESPESA_COLETIVA <-
  read.fwf(paste0(address,"/DESPESA_COLETIVA.txt"),
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



DESPESA_INDIVIDUAL <- 
  read.fwf(paste0(address,"/DESPESA_INDIVIDUAL.txt"),
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



CADERNETA_COLETIVA <-
  read.fwf(paste0(address,"/CADERNETA_COLETIVA.txt"),
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



ALUGUEL_ESTIMADO <-
  read.fwf(paste0(address,"/ALUGUEL_ESTIMADO.txt"),
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


saveRDS(DOMICILIO, "data/clean/DOMICILIO.rds")
saveRDS(MORADOR, "data/clean/MORADOR.rds")
saveRDS(DESPESA_COLETIVA, "data/clean/DESPESA_COLETIVA.rds")
saveRDS(DESPESA_INDIVIDUAL, "data/clean/DESPESA_INDIVIDUAL.rds")
saveRDS(CADERNETA_COLETIVA, "data/clean/CADERNETA_COLETIVA.rds")
saveRDS(ALUGUEL_ESTIMADO, "data/clean/ALUGUEL_ESTIMADO.rds")
