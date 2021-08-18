library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(formattable)
library(readxl)

archivo       <- "datos_cafe.csv"
mendeley_tags <- read.csv(file = archivo)

mendeley_tags_year  <- 
  mendeley_tags %>% 
  group_by(year,tag,byproduct) %>% 
  count(tag, sort = TRUE)

mendeley_tags_count <- mendeley_tags_year %>% 
                       group_by(tag) %>% 
                       count(tag, sort = TRUE) %>%
                       filter(n > 1)

tags <- c("Cosmetics","Food", "Nutraceuticals","Feed","Alcoholic beverages",
          "Edible mushrooms","Organic acids","Carotenoids","Biopolymers",
          "Bioethanol","Biogas","Ensymes", "Biosurfactants","Biomethane",
          "Biohydrogen","Lipids","Bioadsorbents","Fertilizers","Composting",
          "Biocatalyst","Biodiesel","Bio-oil","Solid fuels","Syngas",
          "Construction materials","Nanocomposites","Textiles","Inks and paint",
          "Yarn","Paper","Pigments")



ggplot(data = mendeley_tags_year %>% 
              filter(tag %in% mendeley_tags_count$tag), # corregir
              mapping = aes(x = year, y = n, color = tag)) +
  geom_point() + 
  facet_wrap(vars(byproduct)) +
  geom_smooth(span = 1, se = FALSE)










