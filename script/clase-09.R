## Clase 09: 
## Eduard Martinez
## Update: 09-10-2023

## **[0.] Configuración inicial**

### **☑ Librerías**

## instalar/llamar pacman
require(pacman)

## usar la función p_load de pacman para instalar/llamar las librerías de la clase
p_load(tidyverse, ## manipular/limpiar conjuntos de datos.
       rio, ## para leer/escribir archivos desde diferentes formatos. 
       skimr, ## skim: describir un conjunto de datos
       janitor, ##  tabyl: frecuencias relativas
       data.table) ## rbindlist

##=====================================##
## **[1.] Visualizaciones** `ggplot()` ##
##=====================================##

## limpiar el entorno y leer el conjutno de datos
rm(list = ls())
geih <- import("output/geih_2019_2021.rds")
  
### **2.1 Grafico base:** "mapping" y "aes"
ggplot(data = geih , mapping = aes(x=p6040 , y=inglabo))

### **2.2 Adicionar geometría**
ggplot(data = geih , mapping = aes(x=p6040 , y=inglabo)) +
geom_point(color="red")

#### puede guardar la gráfica dentro de un objeto: 
p <- ggplot(data = geih , mapping = aes(x=p6040 , y=inglabo)) + 
     geom_point(color="red")

p <- ggplot(data = geih , 
            mapping = aes(x=p6040 , y=inglabo , 
                          group=as.factor(p6020) , color=as.factor(p6020))) + 
     geom_point()
p

#### añadir atributos a este objeto:
p <- p + scale_color_manual(values = c("2"="green" , "1"="yellow"),
                       label = c("2"="Mujer" , "1"="Hombre") , name ="Sexo") 
p  

#### density chart:
density <- subset(geih , p6040>=18 & inglabo <1e+07 & is.na(p6450)==F) %>%
           ggplot(data=. , 
                  mapping = aes(inglabo , group=as.factor(p6450) , fill=as.factor(p6450))) +
            geom_density(na.rm=T , alpha=0.3)
density

### **2.3** `theme()`
density + theme_light()

density + theme_light() +
labs(x = "Ingresos" , 
     y = "Densidad" , 
     title = "Distribución de los ingresos (menores a 1e+7)",
     subtitle = "(por tipo de contrato)") + 
scale_fill_discrete(label = c("1"="Verbal" , "2"="Escrito" , "9"="NS/NR") , name ="Tipo de contrato") 

### **2.4** `group_by()` `+` `ggplot()` 
pea <- geih %>% 
       subset(fuerza==1)

## summarize data
pea %>%
summarise(deso = sum(desocupado,na.rm=T),
          pea = sum(fuerza,na.rm=T)) %>%
mutate(tasa_deso = deso/pea*100)

## tasa desocupacion por sexo
tasa_sexo <- pea %>%
             group_by(p6020) %>%
             summarise(deso = sum(desocupado,na.rm=T),
                       pea = sum(fuerza,na.rm=T)) %>%
             mutate(tasa_deso = deso/pea*100)
tasa_sexo

ggplot(data=tasa_sexo, 
       aes(x=as.factor(p6020), y=tasa_deso , 
           fill=as.factor(p6020) , group=as.factor(p6020))) + 
geom_bar(stat = "identity") + theme_test()
  
## tasa desocupacion por sexo y urbano rural
tasa_sexo_urb <- pea %>%
                 group_by(p6020,urbano) %>%
                 summarise(deso = sum(desocupado,na.rm=T),
                           pea = sum(fuerza,na.rm=T)) %>%
                 mutate(tasa_deso = deso/pea*100)
tasa_sexo_urb

ggplot(tasa_sexo_urb, aes(fill=as.factor(p6020), y=tasa_deso, x=as.factor(urbano))) + 
geom_bar(position="dodge", stat="identity")

## ingreso promedio por departamento y sexo
ing_sexo_depto <- pea %>%
                  group_by(p6020,dpto.x,urbano) %>%
                  summarise(ing_mean = mean(inglabo,na.rm=T)) %>%
                  mutate(name_urbano=ifelse(urbano==1,"Urbano","Rural"))
ing_sexo_depto

ggplot(data = ing_sexo_depto , aes(fill=as.factor(p6020), y=ing_mean, x=name_urbano)) + 
geom_bar(position="dodge", stat="identity") +
facet_wrap(~dpto.x) +
scale_fill_discrete(label = c("1"="Hombre" , "2"="Mujer") , name ="Sexo") + theme_light()

## ingreso promedio por sexo y tipo contrato para departamento magdalena
ing_sexo_depto47 <- pea %>% 
                    subset(dpto.x==47) %>%
                    group_by(p6020,urbano,p6450) %>%
                    summarise(ing_mean = mean(inglabo,na.rm=T)) %>%
                    mutate(name_urbano=ifelse(urbano==1,"Urbano","Rural"),
                           name_p6450=case_when(p6450==1~"Verbal",
                                                p6450==2~"Escrito",
                                                p6450==9~"NS/NR",
                                                p6450==NA~"NA"))
ing_sexo_depto47

ggplot(data = ing_sexo_depto47 , aes(fill=as.factor(p6020), y=ing_mean, x=name_urbano)) + 
geom_bar(position="dodge", stat="identity") +
facet_wrap(~name_p6450) +
scale_fill_discrete(label = c("1"="Hombre" , "2"="Mujer") , name ="Sexo") + theme_light()





