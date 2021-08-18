library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(formattable)
library(readxl)
library(bibliometrix)
library(stringr)
library(tm)
library(stringdist)

# Este programa es para la union de de las bases de datos de coffe by products con
# la busqueda de WOS y Scopus


# ----------------- SE CARGAN LOS DATOS DE BUSQUEDA EN WOS Y SCOPUS ------------

coffe_wos_1 <- 
  convert2df(here("data",
                  "/raw data wos/Coffe Husk WOS 29.txt"), 
             dbsource = "wos", 
             format = "plaintext")

coffe_wos_2 <- 
  convert2df(here("data", 
                  "raw data wos/Coffee Pulp WOS 26.txt"),
             dbsource = "wos", 
             format = "plaintext")

coffe_wos_3 <- 
  convert2df(here("data", 
                  "raw data wos/Coffee Silverskin WOS 26.txt"),
             dbsource = "wos", 
             format = "plaintext")

coffe_wos_4 <- 
  convert2df(here("data",
                  "raw data wos/Spent coffee grounds WOS 57.txt"), 
             dbsource = "wos", 
             format = "plaintext")

coffe_scopus_1 <- 
  convert2df(here("data",
                  "raw data scopus/Coffee Husk Scopus 21.bib"), 
             dbsource = "scopus", 
             format = "bibtex")

coffe_scopus_2 <- convert2df(here("data",
                                  "raw data scopus/Coffee Pulp Scopus 33.bib"),
                             dbsource = "scopus", 
                             format = "bibtex")

coffe_scopus_3 <- 
  convert2df(here("data",
             "raw data scopus/Coffee Silverskin Scopus 34.bib"),
             dbsource = "scopus", format = "bibtex")

coffe_scopus_4 <- 
  convert2df(here("data", 
                  "raw data scopus/Spent coffee grounds Scopus 47.bib"),
                  dbsource = "scopus", format = "bibtex")

# Union de las bases de datos 
coffe_scopus_wos <- mergeDbSources(coffe_wos_1,coffe_wos_2,coffe_wos_3,coffe_wos_4,
                                   coffe_scopus_1,coffe_scopus_2,coffe_scopus_3,coffe_scopus_4,
                                   remove.duplicated=TRUE)


rm(coffe_wos_1,coffe_wos_2,coffe_wos_3,coffe_wos_4,
   coffe_scopus_1,coffe_scopus_2,coffe_scopus_3,coffe_scopus_4) # Liberar memoria 

coffe_scopus_wos$TI <- str_to_lower(coffe_scopus_wos$TI)
coffe_scopus_wos$TI <- gsub('-','',coffe_scopus_wos$TI)
coffe_scopus_wos$TI <- gsub(':','',coffe_scopus_wos$TI)
coffe_scopus_wos$TI <- gsub('\\.','',coffe_scopus_wos$TI)

# ----------------- SE CARGA LA BASE DE DATOS  COFFE BY PRODUCTS ---------------

datos       <- read.csv(here("output",
                             "mendeley_tags_cate.csv"))
datos$title <- str_to_lower(datos$title)  
datos$title <- gsub('-','',datos$title)
datos$title <- gsub(':','',datos$title)
datos$title <- gsub('\\.','',datos$title)
datos$title <- gsub('\\,','',datos$title)
datos$autores <- NA

# -- SE EXTRAE DE LOS DATOS DE WOS Y SCOPUS LOS QUE ESTAN EN LA BASE DE DATOS --
idx1 <- c()
idx2 <- which(coffe_scopus_wos$DI %in% datos$doi)
for (i in (1:length(coffe_scopus_wos$AU))){
  pos <- which(stringsim(coffe_scopus_wos$TI[i],datos$title) > 0.6)
  
  if (length(pos) > 0){
    idx1 <- c(i,idx1)
    datos$autores[pos] <- coffe_scopus_wos$AU[i]
  }
} 
idx          <- union(idx1,idx2)
datos_select <- coffe_scopus_wos[idx,]
datos_unidos <- inner_join(datos_select,datos[!is.na(datos$autores),],by = c('AU' = 'autores'))

# -- ANALISIS BIBLIOMETRICO ----------------------------------------------------
Resultados <- bibliometrix::biblioAnalysis(datos_unidos, sep = ';')
S          <- summary(Resultados)
plot(x = Resultados, k = 10, pause = FALSE)
indices <- Hindex(datos_unidos, field = "author", elements= unique(unlist(strsplit(datos_unidos$AU,split = ';'))), sep = ";", years = 10)

# Citation network
NetMatrix <- biblioNetwork(datos_unidos, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", 
                type = "fruchterman", size=T,
                remove.multiple=FALSE, 
                labelsize=0.7,edgesize = 5,
                cluster = 'louvain')


# COuntry cientific colaboration 
M <- metaTagExtraction(datos_unidos, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")


# key word co-ocurrences
NetMatrix <- biblioNetwork(datos_unidos, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)


# Tabla de journals por producto 
datos_unidos <- inner_join(datos_select,datos,by = c('AU' = 'autores')) %>%
  separate_rows(byproduct,sep = ',')
datos_unidos$byproduct <- gsub(" ","",datos_unidos$byproduct)

journal_product <- datos_unidos %>%
  group_by(JI,byproduct) %>%
  count()


# Autores mas productivos
datos_autores <- datos_unidos %>%
  separate_rows(AU, sep = ";")

autores_mas_prod <- datos_autores %>%
  group_by(AU) %>% 
  count() %>%
  arrange(desc(n)) %>% 
  head(10)


# Autores mas productivos por producto 

autores_producto <- datos_autores %>%
  group_by(AU,byproduct) %>% 
  count() %>%
  arrange(desc(n)) %>%
  group_by(byproduct) %>%
  top_n(n = 5)


# Autores por categoria 
autores_categoria <- datos_autores %>%
  group_by(AU,category) %>% 
  count() %>%
  arrange(desc(n)) %>%
  group_by(category) %>%
  top_n(n = 5)






