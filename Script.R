# Pacotes usados ----

library(readxl)
library(tidyverse)
library(writexl)

# Dados do Pooling ----

# dados_pool <- read_xlsx('Dados/Pool Testing Resultados - LabEst.xlsx') %>% 
dados_pool <- read_xlsx('Dados/Resultados Pool Testing - Final - Correto.xlsx', sheet = 'Pesquisa') %>% 
  select(-c('#','Prioridade?',"Data de\r\nRecebimento","ID Placa Pool","Data da Reação Pool","CT E\r\nPool",
            "CT RP\r\nPool","ID Placa Indiv.","Data da Reação Indiv.","CT E\r\nIndiv.","CT RP\r\nIndiv.",
            "Data de Envio de Resultado")) %>% 
  rename(Id_Pool = `Pool #`,
         `Resultado Indiv.` = `Resultado\r\nIndiv.`)

## ID --

dados_pool %>% distinct(ID) %>% count() == dados_pool %>% count()

## Idade --

dados_pool$Idade[dados_pool$Idade == 'F'] <- '41'

dados_pool <- dados_pool %>% mutate(Idade = as.integer(Idade))

summary(dados_pool$Idade)

## Sexo --

summary(as.factor(dados_pool$Sexo))

dados_pool$Sexo[dados_pool$Sexo == 'f'] <- 'F'

dados_pool$Sexo[dados_pool$Sexo == 41] <- 'F'

summary(as.factor(dados_pool$Sexo))

## Data de Coleta --

dados_pool <- dados_pool %>% 
  mutate(data1 = as.Date(`Data de Coleta`,"%d/%m/%y"),
         data2 = as.Date(as.numeric(`Data de Coleta`), origin = "1899-12-30"))

dados_pool %>% mutate(teste = is.na(data1) + is.na(data2)) %>% pull(teste)

dados_pool <- dados_pool %>% 
  mutate(`Data de Coleta` = case_when(
    is.na(data1) ~ data2,
    T ~ data1)) %>% 
  select(-c(data1,data2))

summary(dados_pool$`Data de Coleta`)

## Id_Pool --

dados_pool$Id_Pool[is.na(as.integer(dados_pool$Id_Pool))]

dados_pool <- dados_pool %>% 
  filter(Id_Pool != '-') %>% 
  mutate(Id_Pool = as.integer(Id_Pool))

summary(dados_pool$Id_Pool)

## Resultado Pool --

summary(as.factor(dados_pool$`Resultado Pool`))

## Resultado Indiv. --

summary(as.factor(dados_pool$`Resultado Indiv.`))

dados_pool <- dados_pool %>% 
  mutate(`Resultado Indiv.` = ifelse(`Resultado Indiv.` == '-',
                                     NA, 
                                     `Resultado Indiv.`))

summary(as.factor(dados_pool$`Resultado Indiv.`))

## Tamanho do Pool --

summary(as.factor(dados_pool$`Tamanho do Pool`))

dados_pool <- dados_pool %>% 
  mutate(`Tamanho do Pool` = as.integer(`Tamanho do Pool`))

## Resultado Final --

summary(as.factor(dados_pool$`Resultado Final`))

dados_pool$`Resultado Final`[dados_pool$`Resultado Final` == '-' &
             dados_pool$`Resultado Pool` == 'Não Detectável'] = 'Não Detectável'

dados_pool <- dados_pool %>% filter(`Resultado Final` != '-')

summary(as.factor(dados_pool$`Resultado Final`))

## Avaliando coerência dos dados

# Resultados

dados_pool %>% distinct(`Resultado Pool`,`Resultado Indiv.`,`Resultado Final`)

dados_pool <- dados_pool %>% 
  mutate(`Resultado Indiv.` = ifelse(`Resultado Pool` == 'Não Detectável' &
                                       `Resultado Indiv.` == 'Não Detectável',
                                     NA, `Resultado Indiv.`))

dados_pool <- dados_pool %>% select(-`Resultado Final`)

# Tamanho do pool

dados_pool %>% distinct(Id_Pool,`Tamanho do Pool`) %>% count()
dados_pool %>% distinct(Id_Pool) %>% count()

dados_pool %>% group_by(Id_Pool) %>% summarise(n = n()) == dados_pool %>% distinct(Id_Pool,`Tamanho do Pool`)

dados_pool <- dados_pool %>% select(-`Tamanho do Pool`)

# Dados do questionário ----

dados_quest <- read_xlsx('Dados/Questionário - Pesquisa - Pool testing - LabEst.xlsx') %>% 
  select(-c("Nome","Cq" ,"Tomou vacina?", "Observação", "Mês","Nº atend.","#",
            "Local...54","Local...57")) %>% 
  rename(semana = Sem., `N cômodos` = `Nº cômodos`, 
         Enfisema = `Enfisema DPOC`, DPOC = `...64`)

## Semana

summary(as.integer(dados_quest$semana))

dados_quest <- dados_quest %>% mutate(semana = as.integer(semana))

## Data 

dados_quest <- dados_quest %>% mutate(Data = as.Date(Data))

summary(dados_quest$Data)

## Resultado 

summary(as.factor(dados_quest$Resultado))

## Idade 

dados_quest <- dados_quest %>% mutate(Idade = as.integer(Idade))

summary(dados_quest$Idade)

## Etnia 

dados_quest <- dados_quest %>% 
  mutate(Etnia = case_when(
    Etnia == 'BRANCA' ~ 'Branca',
    Etnia == 'PARDA' ~ 'Parda',
    TRUE ~ Etnia))

summary(as.factor(dados_quest$Etnia))

## Situação de rua 

dados_quest <- dados_quest %>% 
  mutate(`Situação de rua` = case_when(
    `Situação de rua` == 'M' ~ 'N',
    TRUE ~ `Situação de rua`))

summary(as.factor(dados_quest$`Situação de rua`))

## Quantas pessoas moram na casa 

summary(as.factor(dados_quest$`Quantas pessoas moram na casa`))

## N cômodos 

summary(as.factor(dados_quest$`N cômodos`))

dados_quest$`N cômodos`[dados_quest$`N cômodos` == '-'] = NA

summary(as.factor(dados_quest$`N cômodos`))

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

summary(as.factor(dados_quest$`Qts dias...22`))

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

summary(as.factor(dados_quest$Secreção))

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
  mutate(
    `Congestão nasal` = case_when(
    `Congestão nasal` == 'N' ~ '0',
    TRUE ~ `Qts dias...35`),
    `Congestão nasal` = case_when(
      `Congestão nasal` == '5 meses' ~ '150',
      T ~ `Congestão nasal`)) %>%
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
  mutate(
    `Perda de apetite` = case_when(
    `Perda de apetite` == 'N' ~ '0',
    TRUE ~ `Qts dias...39`),
    
    `Perda de apetite` = case_when(
      `Perda de apetite` == '1 mes' ~ '30',
      T ~ `Perda de apetite`)) %>%
  
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
  mutate(
    Fadiga = case_when(
    Fadiga == 'N' ~ '0',
    TRUE ~ `Qts dias...51`),
    
    Fadiga = case_when(
    Fadiga == '8 meses' ~ '240',
    T ~ Fadiga)) %>%
  
    mutate(Fadiga = as.integer(Fadiga)) %>% 
  select(-`Qts dias...51`)
#8 meses

## Mialgia 

dados_quest <- dados_quest %>% 
  mutate(Mialgia = case_when(
    Mialgia == 'N' ~ 0,
    TRUE ~ `Qts dias...53`)) %>% 
  #rename(`Local - Mialgia` = `Local...54`) %>% 
  select(-`Qts dias...53`)

## Artralgia 

dados_quest <- dados_quest %>% 
  mutate(Artralgia = case_when(
    Artralgia == 'N' ~ 0,
    TRUE ~ `Qts dias...56`)) %>% 
  #rename(`Local - Artralgia` = `Local...57`) %>% 
  select(-`Qts dias...56`)

## HAS 

summary(as.factor(dados_quest$HAS))

## Outra cardiopatia 

summary(as.factor(dados_quest$`Outra cardiopatia`))

## DM 

summary(as.factor(dados_quest$DM))

## Asma 

summary(as.factor(dados_quest$Asma))

## Bronquite 

summary(as.factor(dados_quest$Bronquite))

## Enfisema

summary(as.factor(dados_quest$Enfisema))

## DPOC

summary(as.factor(dados_quest$DPOC))

## Câncer 

summary(as.factor(dados_quest$Câncer))

## Doença crônica nos rins 

summary(as.factor(dados_quest$`Doença crônica nos rins`))

## Outra doença crônica

summary(as.factor(dados_quest$`Outra doença crônica`))

## Qual 

summary(as.factor(dados_quest$Qual))

dados_quest <- dados_quest %>% mutate(Qual = ifelse(Qual == '-',NA,Qual))

## Rotina de atividades nas últimas 2 semanas 

summary(as.factor(dados_quest$`Rotina de atividades nas últimas 2 semanas`))

## Recebeu alguma visita nas últimas 2 semanas 

summary(as.factor(dados_quest$`Recebeu alguma visita nas últimas 2 semanas`))

dados_quest <- dados_quest %>% 
  mutate(`Recebeu alguma visita nas últimas 2 semanas` = case_when(
    `Recebeu alguma visita nas últimas 2 semanas` == 's' ~ 'S',
    T ~ `Recebeu alguma visita nas últimas 2 semanas`))

## Conhece alguém que teve o diagnóstico de COVID-19

summary(as.factor(dados_quest$`Conhece alguém que teve o diagnóstico de COVID-19`))

## Teve contato com alguém diagnósticada com COVID-19 

summary(as.factor(dados_quest$`Teve contato com alguém diagnósticada com COVID-19`))

## Usa máscara 

summary(as.factor(dados_quest$`Usa máscara`))

## Alguma pessoas que mora na mesma casa testou positivo e participou da pesquisa 

summary(as.factor(dados_quest$`Alguma pessoas que mora na mesma casa testou positivo e participou da pesquisa`))

dados_quest <- dados_quest %>% 
  mutate(`Alguma pessoas que mora na mesma casa testou positivo e participou da pesquisa` = case_when(
    `Alguma pessoas que mora na mesma casa testou positivo e participou da pesquisa` == 'n' ~ 'N',
    T ~ `Alguma pessoas que mora na mesma casa testou positivo e participou da pesquisa`
  ))

# Linhas dúplicadas ----

## dados_pool

dados_pool %>% group_by(ID) %>% summarise(n = n()) %>% filter(n > 1)

## dados_quest

dados_quest %>% group_by(Código) %>% summarise(n = n()) %>% filter(n > 1)
# duas linhas diferentes com o mesmo Código

dados_quest$Código[1331] <- "P-A1607"

# Juntando bases ----

base_completa <- left_join(dados_pool,dados_quest, by = c('ID' = 'Código'))

Erro <- dados_pool %>% filter((dados_pool$ID %in% dados_quest$Código) == F)

write_xlsx(Erro, path = 'Dados/DadosApenasNoPool.xlsx', 
           col_names = TRUE)

Erro <- dados_quest %>% filter((dados_quest$Código %in% dados_pool$ID) == F)

write_xlsx(Erro, path = 'Dados/DadosApenasNoQuestionario.xlsx', 
           col_names = TRUE)

Erro <- base_completa %>% select(ID, Idade.x, Idade.y, Sexo.x, Sexo.y) %>% 
  filter(!(Idade.x == Idade.y & Sexo.x == Sexo.y)) %>% 
  rename(IdadePool = Idade.x, IdadeQuestionario = Idade.y,
         SexoPool = Sexo.x, SexoQuestionario = Sexo.y)

write_xlsx(Erro, path = 'Dados/DadosIncongruentes.xlsx', 
           col_names = TRUE)

base_completa <- base_completa[-which(base_completa$Idade.x != base_completa$Idade.y |
                                        base_completa$Sexo.x != base_completa$Sexo.y),] %>% 
  select(-c(Idade.y,Sexo.y)) %>% 
  rename(Idade = Idade.x, Sexo = Sexo.x)

write.csv(base_completa,'Dados/base_completa.csv', row.names = FALSE)
