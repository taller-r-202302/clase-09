## Eduard Martinez
## 18-10-2023

## intial setup
rm(list=ls())
require(pacman)
p_load(tidyverse,
       rio,
       data.table,
       janitor)

## load data
files <- list.files("input",recursive=T , full.names=T)
files

## Cabecera - Caracter
str_subset(files,"Cabecera - Car")
cg <- import_list(str_subset(files,"Cabecera - Car")) %>% 
      rbindlist(fill=T) %>% clean_names()

## Cabecera - Caracter
str_subset(files,"Cabecera - Ocu")
ocu <- import_list(str_subset(files,"Cabecera - Ocu")) %>% 
       rbindlist(fill=T) %>% clean_names()

## join
geih <- left_join(x=cg , y=ocu , by=c("directorio","secuencia_p","orden","hogar"))

## clean environment
rm(list=ls())
geih <- import("output/geih_2019_2021.rds")

## scatter plot
ggplot(data=geih , 
       mapping=aes(x=p6040 , y=inglabo , col=as.factor(p6020))) + 
geom_point() + 
theme_light() + 
labs(title = "Edad vs Inglabo" , x="Edad" , y="Ingreso") +
theme(plot.title = element_text(hjust = 0.5))

## Ingreso por sexo
db <- geih %>% 
group_by(p6020) %>%
summarise(mean_wage=mean(inglabo,na.rm=T)/1000000)

ggplot(data = db , aes(x=p6020 , y=mean_wage , fill=as.factor(p6020))) +
geom_bar(stat="identity") +
scale_fill_manual(values=c("1"="red" , "2"="blue"), 
                  labels=c("1"="Hombre" , "2"="Mujer")) + 
theme_light() + theme(axis.text.x=element_blank()) + 
labs(title = "Inglabo por sexo" , x="" , y="Ingreso" , fill="Sexo")

## Ingreso por sexo y tipo de contrato
geih <- geih %>% 
        mutate(genero=ifelse(p6020==1,"Hombre","Mujer"),
               rango=case_when(p6040<18~"Ninos",
                               p6040>=18 &p6040<=28~"Jovenes",
                               p6040>28~"Adultos"))
  

geih %>% 
group_by(genero,rango) %>%
summarise(mean_wage=mean(inglabo,na.rm=T)/1000000) %>%
ggplot(data=. , 
       aes(x=as.factor(genero) , y=mean_wage , fill=as.factor(genero))) +
geom_bar(position="dodge", stat="identity" , show.legend = F) +
facet_wrap(~as.factor(rango)) +
theme_light() 


