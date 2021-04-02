######################################
#### Data Visualization Challenge ###
#### Heidi Steiner ##################
#### steiner@pharmacy.arizona.edu ###
####### MAR 26 2020 ##################
######################################

## Libraries 
library(tidyverse)
theme_set(theme_minimal())
library(ggalluvial)
library(Polychrome)
library(readxl)
library(cowplot)

## functions
`%!in%` = negate(`%in%`)

## load data 
# Affirmative Data
Table17 <- read_excel("~/Downloads/Table17.xls", 
                      skip = 4)

table17d <- read_excel("~/Downloads/table17d.xlsx", 
                       skip = 3)

fy2019_table17d <- read_excel("~/Downloads/fy2019_table17d.xlsx", 
                              skip = 3)

# All entry data
table13 = read_excel("~/Downloads/yrbk_2019_rfa_excel_final (1)/fy2019_table13.xlsx",
                     skip = 3) 

presidents = presidential %>% 
  add_row(name = "Trump", start = as.Date("2017-01-20"),
        end = as.Date("2021-01-20"), party = "Republican")



## create data

t17 = Table17[-c(161:165),] %>% 
  filter(`Region and country of nationality` %!in% c("All nationalities", "Europe", "Asia", "Africa", "Oceania", "North America", "Caribbean" ,"Central America", "South America"),
         !is.na(`Region and country of nationality`)) %>% 
  dplyr::select(`Region and country of nationality`:`2000`)

t17_2 = table17d[- c(1:11, 116:121), ] 

data1 = full_join(t17, t17_2) %>% 
  filter(!grepl("Serbia and", `Region and country of nationality`)) %>% 
  rbind(c("Serbia and Montenegro", 519, 448, 726, 514, NA,NA, NA, 58, 26, 30, 65, 52, 37, 18)) %>% 
  select(-"2010")

t17_3 = fy2019_table17d[-c(1:11, 116:122),] %>% 
  filter(!grepl("Serbia", `Region and country of nationality`)) %>% 
  rbind(c("Serbia and Montenegro", 9, 11, 23, 17, 19, 10, 4, 11, 45, 43))


data = full_join(data1, t17_3) %>%
  na_if("D") %>% 
  na_if("-") %>% 
  na_if("X") %>% 
  mutate(
    across(everything(), ~replace_na(.x, 1))
  ) %>% 
  pivot_longer(cols = c("1997":"2019"), names_to = "year", values_to = "refugees") %>% 
  select(country.etc = `Region and country of nationality`,
         year,
         refugees) %>% 
  mutate(refugees = as.numeric(refugees))


vector = unique(data$country.etc)
target2 = vector[seq(from = 1, to = length(vector), by=2)]

top_countries = data %>% 
  arrange(desc(refugees)) %>% 
  group_by(year) %>% 
  top_n(10)
  
plotdat = data %>% 
  filter(country.etc %in% top_countries$country.etc) %>% 
  mutate(grp = if_else(country.etc %in% target2, 2, 1),
         scaled = if_else(grp == 1, refugees*-1, refugees))

totals_affirmative = data %>% 
  group_by(year) %>% 
  summarise(affirmatives = sum(refugees)) %>% 
  arrange(desc(year ))

t13 = table13 %>% 
  mutate(year = as.numeric(Year)) %>% 
  filter(Year %in% 1997:2019) %>% 
  select(-Year) %>% 
  arrange(desc(year)) %>% 
  cbind(totals_affirmative$affirmatives) %>% 
  mutate(affirmatives = totals_affirmative$affirmatives,
         denied = Number - affirmatives,
         denied = if_else(denied < 0 , 0, denied)) %>% 
  dplyr::select(-`totals_affirmative$affirmatives`) %>% 
  pivot_longer(cols=c("denied", "affirmatives")) 


## colors 
set.seed(3)

P30 <- createPalette(30, c("#FF0000", "#00FF00", "#0000FF"))
P30 <- as.vector(t(matrix(P30, ncol=3)))


## alluvial plot 
ggall = ggplot(data = plotdat,
       aes(x = as.numeric(year), y = scaled, alluvium = country.etc)) +
  geom_alluvium(aes(fill = country.etc, colour = country.etc),
                alpha = .75, decreasing = FALSE) +
  theme(legend.position = "none",
        axis.title = element_blank(), 
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = P30) +
  scale_fill_manual(values = P30) +
  scale_x_continuous(breaks = seq(from = 1997, to = 2019, by = 2)) 
  # guides(fill = guide_legend(ncol = 3),
  #        col = guide_legend(ncol = 3))

ggall 

## bar plot
ggbar = ggplot(data = t13,
               aes(x = as.numeric(year),y = value,  fill = name)) + 
  # geom_vline(aes(xintercept = 2001),
  #            color = "gray40") + 
  # geom_vline(aes(xintercept = 2009),
  #            color = "gray40") +
  # geom_vline(aes(xintercept = 2017),
  #            color = "gray40") + 
  geom_bar(stat = "identity",) +
  scale_fill_manual(values = c("gray", "gray40")) +
  scale_x_continuous(breaks = seq(from = 1997, to = 2019, by =2 )) +
  ylim(0, 100000) + 
  theme(axis.title = element_blank(),
        axis.text.y  = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 15) , 
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "gray"),
        axis.ticks.x = element_line(colour = "gray")) +
  geom_text(aes(x = year, y = Number ,  label = formatC(Number, format="f", big.mark=",", digits = 0)),color = "gray40", vjust = -.5)

ggbar


## cowplot
gg = cowplot::plot_grid(ggall,
                   ggbar, 
                   ncol = 1,
                   rel_heights   = c(3, 1),
                   vjust = -1000
                      )

gg
## save plot

png("USasylum.png", width = 2000, height = 900,
    bg = "transparent")
print(gg)
dev.off()

