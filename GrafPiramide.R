library(tidyverse)
require(plyr)

base_completa <- read.csv('Dados/base_completa.csv')

base_completa %>% 
  mutate(Resultado.Pool = ifelse(Resultado.Pool != 'Não Detectável' |
                                   is.na(Resultado.Pool),
                                 'Positivo','Negativo')) %>% 
  group_by(Resultado.Pool) %>% summarise(n = n())


base_completa %>% group_by(Id_Pool) %>% summarise(n = n()) %>% 
  group_by(n) %>% summarise(N = n())


graph <- base_completa %>% select(Resultado.Pool,Idade) %>% 
  mutate(Resultado.Pool = as.factor(ifelse(Resultado.Pool != 'Não Detectável' |
                                   is.na(Resultado.Pool),
                                 'Indeterminado','Não Detectável')),
         Idade = as.integer(Idade)) %>% 
  filter(!(is.na(Idade)),!(is.na(Resultado.Pool)))

ggplot(data=graph,aes(x = Idade, fill = Resultado.Pool)) + 
  geom_bar(subset=.(g=="Indeterminado")) + 
  geom_bar(subset=.(g=="Não Detectável"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()+
  geom_hline(yintercept = 0)+
  labs(y = 'Frequência', fill = 'Resultado do Pool')+
  theme_bw()

ggplot(data=graph,aes(x=Idade,fill=Resultado.Pool)) + 
  geom_bar(data=subset(graph,Resultado.Pool=="Indeterminado")) + 
  geom_bar(data=subset(graph,Resultado.Pool=="Não Detectável"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()+
  geom_hline(yintercept = 0)+
  labs(y = 'Frequência', fill = 'Resultado do Pool')+
  theme_bw()








