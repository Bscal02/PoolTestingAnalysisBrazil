# Pacotes usados ----

library(readxl)
library(tidyverse)
# library(lubridate)

# Dados do Pooling ----

dados_pool <- read_xlsx('Dados/Pool Testing Resultados - LabEst.xlsx') %>% 
  select(-c('#','...19','Prioridade?')) %>% 
  rename(Id_Pool = `Pool #`,
        `Data de Recebimento` = `Data de\r\nRecebimento`,
        `CT E Pool` =  `CT E\r\nPool`,
        `CT RP Pool` = `CT RP\r\nPool`,
        `CT E Indiv.` = `CT E\r\nIndiv.`,
        `CT RP Indiv.` = `CT RP\r\nIndiv.`,
        `Resultado Indiv.` = `Resultado\r\nIndiv.`)

## Idade --

dados_pool$Idade[dados_pool$Idade == '660'] <- '66'

dados_pool$Idade[dados_pool$Idade == 'F'] <- '41'

dados_pool <- dados_pool %>% 
  mutate(Idade = as.integer(Idade)) 

## Sexo --

dados_pool$Sexo[dados_pool$Sexo == 'f'] <- 'F'

dados_pool$Sexo[dados_pool$Sexo == 41] <- 'F'

dados_pool <- dados_pool %>% 
  mutate(Sexo = as.factor(Sexo))

## Data de Coleta --

dados_pool <- dados_pool %>% 
         mutate( data1 = as.Date(as.integer(`Data de Coleta`), origin = "1899-12-30"),
         data2 = as.Date(`Data de Coleta`,"%d/%m/%y"))

dados_pool$`Data de Coleta` <- dados_pool$data1

dados_pool$`Data de Coleta`[is.na(dados_pool$`Data de Coleta`)] <- 
  dados_pool$data2[is.na(dados_pool$`Data de Coleta`)]

dados_pool$`Data de Coleta`[32] <- as.Date('16/10/20',"%d/%m/%y")

dados_pool$`Data de Coleta`[dados_pool$`Data de Coleta` == '1921-01-19'] <- '2021-01-19'

dados_pool <- dados_pool %>% 
  select(-c(data1,data2))

## Id_Pool --

dados_pool <- dados_pool %>% 
  mutate(Id_Pool = as.integer(Id_Pool))

## Data de Recebimento --

dados_pool <- dados_pool %>% 
  mutate(`Data de Recebimento` = as.character(`Data de Recebimento`),
           `Data de Recebimento` = as.Date(`Data de Recebimento`,"%Y-%m-%d"))

## Data da Reação Pool --

dados_pool <- dados_pool %>% 
  mutate(
    `Data da Reação Pool` = as.integer(`Data da Reação Pool`),
    `Data da Reação Pool` = as.Date(`Data da Reação Pool`, origin = "1899-12-30"))

## CT E Pool --

dados_pool <- dados_pool %>% 
  mutate(`CT E Pool` = as.numeric(`CT E Pool`))

## CT RP Pool --

dados_pool <- dados_pool %>% 
  mutate(`CT RP Pool` = as.numeric(`CT RP Pool`))

## Resultado Pool --

dados_pool$`Resultado Pool` <- ifelse(dados_pool$`Resultado Pool` == '-',
                                      NA, dados_pool$`Resultado Pool`)

dados_pool <- dados_pool %>% 
  mutate(`Resultado Pool` = as.factor(`Resultado Pool`))

## ID Placa Indiv. --

dados_pool$`ID Placa Indiv.` <- ifelse(dados_pool$`ID Placa Indiv.` == '-',
                                      NA, dados_pool$`ID Placa Indiv.`)

## Data da Reação Indiv. --

dados_pool <- dados_pool %>% 
  mutate(
    `Data da Reação Indiv.` = as.integer(`Data da Reação Indiv.`),
    `Data da Reação Indiv.` = as.Date(`Data da Reação Indiv.`, origin = "1899-12-30"))

## CT E Indiv. --

dados_pool <- dados_pool %>% 
  mutate(`CT E Indiv.` = as.numeric(`CT E Indiv.`))

## CT RP Indiv. --

dados_pool <- dados_pool %>% 
  mutate(`CT RP Indiv.` = as.numeric(`CT RP Indiv.`))

## Resultado Indiv. --

dados_pool$`Resultado Indiv.` <- ifelse(dados_pool$`Resultado Indiv.` == '-',
                                       NA, dados_pool$`Resultado Indiv.`)

dados_pool <- dados_pool %>% 
  mutate(`Resultado Indiv.` = as.factor(`Resultado Indiv.`))

# Dados do questionário ----

dados_quest <- read_xlsx('Dados/Questionário - Pesquisa - Pool testing - LabEst.xlsx') %>% 
  select(-c("Nome", "Tomou vacina?", "Observação", "Mês","Nº atend.","#")) %>% 
  rename(semana = Sem., `N cômodos` = `Nº cômodos`, 
         Enfisema = `Enfisema DPOC`, DPOC = `...64`)

## semana 

dados_quest <- dados_quest %>% mutate(semana = as.factor(semana))

## Data 

dados_quest <- dados_quest %>% mutate(Data = as.Date(Data))

## Sexo 

dados_quest <- dados_quest %>% mutate(Sexo = as.factor(Sexo))

## Cq 

dados_quest <- dados_quest %>% mutate(Cq = as.numeric(Cq))

## Resultado 

dados_quest <- dados_quest %>% mutate(Resultado = as.factor(Resultado))

## Idade 

dados_quest <- dados_quest %>% mutate(Idade = as.integer(Idade))

## Etnia 

dados_quest <- dados_quest %>% 
  mutate(Etnia = case_when(
    Etnia == 'BRANCA' ~ 'Branca',
    Etnia == 'PARDA' ~ 'Parda',
    TRUE ~ Etnia)) %>% 
  mutate(Etnia = as.factor(Etnia))

## Situação de rua 

dados_quest <- dados_quest %>% 
  mutate(`Situação de rua` = case_when(
    `Situação de rua` == 'M' ~ 'N',
    TRUE ~ `Situação de rua`)) %>% 
  mutate(`Situação de rua` = as.factor(`Situação de rua`))

## Quantas pessoas moram na casa 

dados_quest <- dados_quest %>% 
  mutate(`Quantas pessoas moram na casa` = as.integer(`Quantas pessoas moram na casa`))
#+10, 400
#categorizar

## N cômodos 

dados_quest <- dados_quest %>% mutate(`N cômodos` = as.integer(`N cômodos`))

## Escolaridade 

dados_quest <- dados_quest %>% mutate(Escolaridade = as.factor(Escolaridade))

## Febre 

dados_quest <- dados_quest %>% 
  mutate(Febre = case_when(
    Febre == 'N' ~ 0,
    TRUE ~ `Qts dias...18`)) %>% 
  select(-`Qts dias...18`)

## Dor ocular 

dados_quest <- dados_quest %>% 
  mutate(`Dor ocular` = case_when(
    `Dor ocular` == 'N' ~ 0,
    TRUE ~ `Qts dias...20`)) %>% 
  select(-`Qts dias...20`)

## Cefalia 

dados_quest <- dados_quest %>% 
  mutate(Cefalia = case_when(
    Cefalia == 'N' ~ '0',
    TRUE ~ `Qts dias...22`)) %>% 
  mutate(Cefalia = as.integer(Cefalia)) %>% 
  select(-`Qts dias...22`)

## Dor de garganta 

dados_quest <- dados_quest %>% 
  mutate(`Dor de garganta` = case_when(
    `Dor de garganta` == 'N' ~ 0,
    TRUE ~ `Qts dias...24`)) %>% 
  select(-`Qts dias...24`)

## Tosse 

dados_quest <- dados_quest %>% 
  mutate(Tosse = case_when(
    Tosse == 'N' ~ 0,
    TRUE ~ `Qts dias...26`)) %>% 
  select(-`Qts dias...26`)

## Secreção 

dados_quest <- dados_quest %>% mutate(Secreção = as.factor(Secreção))

## Dific. de resp. 

dados_quest <- dados_quest %>% 
  mutate(`Dific. de resp.` = case_when(
    `Dific. de resp.` == 'N' ~ 0,
    TRUE ~ `Qts dias...29`)) %>% 
  select(-`Qts dias...29`)

## Anosmia 

dados_quest <- dados_quest %>% 
  mutate(Anosmia = case_when(
    Anosmia == 'N' ~ 0,
    TRUE ~ `Qts dias...31`)) %>% 
  select(-`Qts dias...31`)

## Coriza 

dados_quest <- dados_quest %>% 
  mutate(Coriza = case_when(
    Coriza == 'N' ~ 0,
    TRUE ~ `Qts dias...33`)) %>% 
  select(-`Qts dias...33`)

## Congestão nasal 

dados_quest <- dados_quest %>% 
  mutate(`Congestão nasal` = case_when(
    `Congestão nasal` == 'N' ~ '0',
    TRUE ~ `Qts dias...35`)) %>% 
    mutate(`Congestão nasal` = as.integer(`Congestão nasal`)) %>% 
  select(-`Qts dias...35`)
#5 meses

## Ageusia 

dados_quest <- dados_quest %>% 
  mutate(Ageusia = case_when(
    Ageusia == 'N' ~ 0,
    TRUE ~ `Qts dias...37`)) %>% 
  select(-`Qts dias...37`)

## Perda de apetite 

dados_quest <- dados_quest %>% 
  mutate(`Perda de apetite` = case_when(
    `Perda de apetite` == 'N' ~ '0',
    TRUE ~ `Qts dias...39`)) %>% 
    mutate(`Perda de apetite` = as.integer(`Perda de apetite`)) %>% 
  select(-`Qts dias...39`)
#1 mes

## Náusea 

dados_quest <- dados_quest %>% 
  mutate(Náusea = case_when(
    Náusea == 'N' ~ 0,
    TRUE ~ `Qts dias...41`)) %>% 
  select(-`Qts dias...41`)

## Vômito 

dados_quest <- dados_quest %>% 
  mutate(Vômito = case_when(
    Vômito == 'N' ~ 0,
    TRUE ~ `Qts dias...43`)) %>% 
  select(-`Qts dias...43`)

## Diarreia 

dados_quest <- dados_quest %>% 
  mutate(Diarreia = case_when(
    Diarreia == 'N' ~ 0,
    TRUE ~ `Qts dias...45`)) %>% 
  select(-`Qts dias...45`)

## Dor torácica 

dados_quest <- dados_quest %>% 
  mutate(`Dor torácica` = case_when(
    `Dor torácica` == 'N' ~ 0,
    TRUE ~ `Qts dias...47`)) %>% 
  select(-`Qts dias...47`)

## Taquicardia 

dados_quest <- dados_quest %>% 
  mutate(Taquicardia = case_when(
    Taquicardia == 'N' ~ 0,
    TRUE ~ `Qts dias...49`)) %>% 
  select(-`Qts dias...49`)

## Fadiga 

dados_quest <- dados_quest %>% 
  mutate(Fadiga = case_when(
    Fadiga == 'N' ~ '0',
    TRUE ~ `Qts dias...51`)) %>% 
    mutate(Fadiga = as.integer(Fadiga)) %>% 
  select(-`Qts dias...51`)
#8 meses

## Mialgia 

dados_quest <- dados_quest %>% 
  mutate(Mialgia = case_when(
    Mialgia == 'N' ~ 0,
    TRUE ~ `Qts dias...53`)) %>% 
  rename(`Local - Mialgia` = `Local...54`) %>% 
  select(-`Qts dias...53`)

## Artralgia 

dados_quest <- dados_quest %>% 
  mutate(Artralgia = case_when(
    Artralgia == 'N' ~ 0,
    TRUE ~ `Qts dias...56`)) %>% 
  rename(`Local - Artralgia` = `Local...57`) %>% 
  select(-`Qts dias...56`)

## HAS 

dados_quest <- dados_quest %>% mutate(HAS = as.factor(HAS))

## Outra cardiopatia 

dados_quest <- dados_quest %>% mutate(`Outra cardiopatia` = as.factor(`Outra cardiopatia`))

## DM 

dados_quest <- dados_quest %>% mutate(DM = as.factor(DM))

## Asma 

dados_quest <- dados_quest %>% mutate(Asma = as.factor(Asma))

## Bronquite 

dados_quest <- dados_quest %>% mutate(Bronquite = as.factor(Bronquite))

## Enfisema

dados_quest <- dados_quest %>% mutate(Enfisema = as.factor(Enfisema))

## DPOC

dados_quest <- dados_quest %>% mutate(DPOC = as.factor(DPOC))

## Câncer 

dados_quest <- dados_quest %>% mutate(Câncer = as.factor(Câncer))

## Doença crônica nos rins 

dados_quest <- dados_quest %>% mutate(`Doença crônica nos rins` = as.factor(`Doença crônica nos rins`))

## Outra doença crônica 

dados_quest <- dados_quest %>% mutate(`Outra doença crônica` = as.factor(`Outra doença crônica`))

## Outra doença crônica 

dados_quest <- dados_quest %>% mutate(`Outra doença crônica` = as.factor(`Outra doença crônica`))

## Qual 

dados_quest <- dados_quest %>% mutate(Qual = ifelse(Qual == '-',NA,Qual))

## Rotina de atividades nas últimas 2 semanas 

dados_quest <- dados_quest %>% 
  mutate(`Rotina de atividades nas últimas 2 semanas` = as.factor(`Rotina de atividades nas últimas 2 semanas`))

## Recebeu alguma visita nas últimas 2 semanas 

dados_quest <- dados_quest %>% 
  mutate(`Recebeu alguma visita nas últimas 2 semanas` = ifelse(`Recebeu alguma visita nas últimas 2 semanas` == 's','S',`Recebeu alguma visita nas últimas 2 semanas`)) %>% 
  mutate(`Recebeu alguma visita nas últimas 2 semanas` = as.factor(`Recebeu alguma visita nas últimas 2 semanas`))

## Conhece alguém que teve o diagnóstico de COVID-19 

dados_quest <- dados_quest %>% mutate(`Conhece alguém que teve o diagnóstico de COVID-19` = as.factor(`Conhece alguém que teve o diagnóstico de COVID-19`))

## Teve contato com alguém diagnósticada com COVID-19 

dados_quest <- dados_quest %>% mutate(`Teve contato com alguém diagnósticada com COVID-19` = as.factor(`Teve contato com alguém diagnósticada com COVID-19`))

## Usa máscara 

dados_quest <- dados_quest %>% mutate(`Usa máscara` = as.factor(`Usa máscara`))

## Alguma pessoas que mora na mesma casa testou positivo e participou da pesquisa 

dados_quest <- dados_quest %>% 
  mutate(`Alguma pessoas que mora na mesma casa testou positivo e participou da pesquisa` = ifelse(`Alguma pessoas que mora na mesma casa testou positivo e participou da pesquisa` == 'n','N',`Alguma pessoas que mora na mesma casa testou positivo e participou da pesquisa`)) %>% 
  mutate(`Alguma pessoas que mora na mesma casa testou positivo e participou da pesquisa` = as.factor(`Alguma pessoas que mora na mesma casa testou positivo e participou da pesquisa`))

# Linhas dúplicadas ----

## dados_pool

dados_pool %>% group_by(ID) %>% summarise(n = n()) %>% filter(n > 1)
#apenas duplicadas

dados_pool <- dados_pool %>% distinct(ID, .keep_all = T)

dados_quest %>% group_by(Código) %>% summarise(n = n()) %>% filter(n > 1)
# duas linhas diferentes com o mesmo Código
#retirei a linha baseada no que batia com a idade da outra tabela

dados_quest <- dados_quest %>% filter(!(Código == 'P-A1608' & Idade == 25))

# Juntando bases ----

base_completa <- left_join(dados_pool,dados_quest, by = c('ID' = 'Código')) 

base_completa <- base_completa[-which(base_completa$Idade.x != base_completa$Idade.y |
                                        base_completa$Sexo.x != base_completa$Sexo.y),] %>% 
  select(-c(Idade.y,Sexo.y)) %>% 
  rename(Idade = Idade.x, Sexo = Sexo.x)

write.csv(base_completa,'base_completa.csv', row.names = FALSE)

