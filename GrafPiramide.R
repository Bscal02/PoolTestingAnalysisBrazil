library(tidyverse)
library(plyr)

base_completa <- read.csv('Dados/base_completa.csv')

base_completa %>% 
  mutate(Resultado.Pool = ifelse(Resultado.Pool != 'Não Detectável' |
                                   is.na(Resultado.Pool),
                                 'Positivo','Negativo')) %>% 
  group_by(Resultado.Pool) %>% dplyr::summarise(n = n())


base_completa %>% group_by(Id_Pool) %>% dplyr::summarise(n = n()) %>% 
  group_by(n) %>% dplyr::summarise(N = n())


graph <- base_completa %>% select(Resultado.Pool,Idade,Sexo) %>% 
  mutate(Resultado.Pool = as.factor(ifelse(Resultado.Pool != 'Não Detectável' |
                                   is.na(Resultado.Pool),
                                 'Indeterminado/Positivo','Não Detectável')),
         Idade = as.integer(Idade),
         Total = 'Todos') %>% 
  filter(!(is.na(Idade)),!(is.na(Resultado.Pool)),!(is.na(Sexo)))

ggplot(data=graph,aes(x = Idade, fill = Resultado.Pool)) + 
  geom_bar() + 
  geom_bar(aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()+
  geom_hline(yintercept = 0)+
  labs(y = 'Frequência', fill = 'Resultado do Pool')+
  theme_bw()

ggplot(data=graph,aes(x=Idade,fill=Resultado.Pool)) + 
  geom_histogram(data=subset(graph,Sexo=="M"),binwidth = 2,aes(y=100*(..count..)/sum(..count..))) + 
  geom_histogram(data=subset(graph,Sexo=="F"),binwidth = 2,aes(y=100*(..count..)/sum(..count..)*(-1))) + 
  scale_fill_manual('Resultado do Pool', values = c("Indeterminado/Positivo" = "#0e0e0e", "Não Detectável" = "#5a5a5a"))+
  scale_y_continuous(labels=abs(seq(-8,8,by = 2)), breaks=seq(-8,8,by = 2), limits = c(-8,8))+
  labs(y = 'Frequência(%)')+
  coord_flip()+
  geom_hline(yintercept = 0)+
  theme_bw()+
  annotate("text", x=90, y=c(-3.5,3.5), label= c("Masculino","Feminino"),size = 7, colour = "black")




ggplot(data=graph,aes(x=Idade,fill=Resultado.Pool)) + 
  geom_histogram(data=subset(graph,Sexo=="M"),
                 binwidth = 2,
                 aes(y=..count.., fill = Total))+
  geom_histogram(data=subset(graph,Sexo=="F"),
                 binwidth = 2,
                 aes(y=(-1)*..count.., fill = Total))+
  geom_histogram(data=subset(graph,Sexo=="M" & Resultado.Pool == "Não Detectável"),
                 binwidth = 2,
                 aes(y=..count.., fill = Resultado.Pool))+
  geom_histogram(data=subset(graph,Sexo=="F" & Resultado.Pool == "Não Detectável"),
                 binwidth = 2,
                 aes(y=(-1)*..count.., fill = Resultado.Pool))+
  coord_flip()+ 
  scale_fill_manual('Resultado do Pool', values = c("Todos" = "#0e0e0e", "Não Detectável" = "#5a5a5a"))+
  geom_hline(yintercept = 0)+
  theme_bw()+
  labs(y = 'Frequência', title = 'Pirâmide etária dos resultados dos Pools')+
  scale_y_continuous(labels=abs(seq(-50,50,by = 20)), breaks=seq(-50,50,by = 20), limits = c(-55,55))+
  annotate("text", x=90, y=c(-40,40), label= c("Feminino","Masculino"),size = 7, colour = "black")













