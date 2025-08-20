# Tarifas do Setor Elétrico — POF 2017-2018

Este repositório reúne análises, visualizações e scripts em **R** relacionados ao consumo de energia elétrica e tarifas no Brasil, com base nos dados da **Pesquisa de Orçamentos Familiares (POF) 2017-2018** do IBGE.

##  Conteúdo

- `code/` – Scripts fonte em R usados para cálculos, análises e visualizações.
- `output/` – Resultados gerados pelos scripts: tabelas, gráficos e relatórios.
- `.gitignore` – Para ignorar arquivos temporários e de configuração local.

##  Objetivo

- Estimar gastos com energia elétrica por faixa de renda, raça e sexo do chefe de domicílio
- Estimar o impacto das bandeiras tarifárias

##  Fonte dos dados

Todos os dados utilizados foram obtidos da **Pesquisa de Orçamentos Familiares (POF) 2017-2018**, realizada pelo IBGE. Os microdados estão disponíveis aqui:
https://www.ibge.gov.br/estatisticas/sociais/saude/24786-pesquisa-de-orcamentos-familiares-2.html?=&t=microdados

##  Como usar

1. **Pré-requisitos:**  
   - R (versão X ou superior)  
   - Pacotes R como `tidyverse`, `survey`, 

2. Faça o download dos microdados da POF 2017-2018 no site do IBGE e salve-os localmente .

3. Execute os scripts do diretório `code/` na ordem para gerar os outputs
