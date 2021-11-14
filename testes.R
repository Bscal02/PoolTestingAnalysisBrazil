library(tidyverse)

base_completa <- read.csv('Dados/base_completa.csv')

base_testePlaca <- base_completa %>% group_by(Id_Pool) %>% 
  mutate(Resultado.Pool = ifelse(Resultado.Pool != 'Não Detectável' | 
                                   is.na(Resultado.Pool), 1,0)) %>% 
  dplyr::summarise(n = n(),
                   Resultado.Pool = sum(Resultado.Pool)) %>% 
  mutate(Resultado.Pool = ifelse(Resultado.Pool == 0,0,1)) %>% 
  group_by(n,Resultado.Pool) %>% 
  dplyr::summarise(Freq = n()) %>% 
  mutate(nTestes = ifelse(Resultado.Pool == 0,Freq,(1 + n)*Freq),
         testesind = n*Freq) %>% 
  group_by(n) %>% 
  dplyr::summarise(nTestes = sum(nTestes),
                   testesind = sum(testesind)) %>% 
mutate(razao = nTestes/testesind)



sum(base_testePlaca$nTestes)/sum(base_testePlaca$testesind) 



base_completa$Id_Pool