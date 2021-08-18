library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(formattable)
library(readxl)

archivo       <- "datos_cafe.csv"
mendeley_tags <- read.csv(file = archivo)

mendeley_tags_year  <- mendeley_tags %>% group_by(year,tag,byproduct) %>% count(tag, sort = TRUE)

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



mendeley_tags_year_husk %>% 
  ggplot(mapping = aes(x = year, y = n, color = tag)) +
  geom_point() + 
  facet_wrap(vars(byproduct)) +
  geom_line()


mendeley_tags_year_husk  <- 
  mendeley_tags %>% 
  filter(byproduct == "Husk") %>% 
  group_by(year,tag) %>% 
  count(tag, sort = TRUE)

mendeley_tags_year_husk  %>% 
  ggplot(mapping = aes(x = year , y = n, color = tag, group = 1)) +
  geom_point() +
  geom_line() + 
  geom_smooth(span = 1, se = FALSE)


# Grafico por producto y por a?o para husk
mendeley_tags_year_husk %>% 
  arrange(year,tag) %>% 
  mutate(tag = factor(tag)) %>% 
  ggplot(aes(x = tag, y = year, size = n, color= tag)) +
  geom_point(alpha = 1) +
  scale_size(name="Papers") +
  xlab("Byproducts Applications") +
  ggtitle("Husk applications") +
  theme(axis.text.x = element_text(face = "bold", 
                                   size = 10, angle = 45,vjust = 0.5),
        axis.text.y = element_text(face = "bold", 
                                   size = 10, angle = 45,hjust = 0)) +
  #scale_y_continuous(breaks = seq(2003,2020)) + 
  guides(color = FALSE)



####

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(formattable)
library(readxl)

archivo       <- "datos_cafe.csv"
mendeley_tags <- read.csv(file = archivo)

mendeley_tags_year  <- mendeley_tags %>% group_by(year,tag,byproduct) %>% count(tag, sort = TRUE)

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
         filter(tag %in% mendeley_tags_count$tag), 
       mapping = aes(x = year, y = n, color = tag)) +
  geom_point() + 
  facet_wrap(vars(byproduct)) +
  geom_line()


mendeley_tags_year_husk  <- 
  mendeley_tags %>% 
  filter(byproduct == "Husk") %>% 
  group_by(year,tag) %>% 
  count(tag, sort = TRUE)

mendeley_tags_year_husk  %>% 
  ggplot(mapping = aes(x = year , y = n, color = tag, group = 1)) +
  geom_point() +
  geom_line() + 
  geom_smooth(span = 1, se = FALSE)


# Grafico por producto y por año para husk
mendeley_tags_year_husk %>% 
  arrange(year,tag) %>% 
  mutate(tag = factor(tag)) %>% 
  ggplot(aes(x = tag, y = year, size = n, color= tag)) +
  geom_point(alpha = 1) +
  scale_size(name="Papers") +
  xlab("Byproducts Applications") +
  ggtitle("Husk applications") +
  theme(axis.text.x = element_text(face = "bold", 
                                   size = 10, angle = 45,vjust = 0.5),
        axis.text.y = element_text(face = "bold", 
                                   size = 10, angle = 45,hjust = 0)) +
  scale_y_continuous(breaks = seq(2003,2020)) + 
  guides(color = FALSE)
