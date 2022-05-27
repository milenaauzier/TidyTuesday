library(tidyverse)
library(janitor)
library(ggdist)
library(MetBrewer)
library(showtext)
library(ggrepel)
library(ggtext)

df <- readxl::read_xlsx("C:/Users/milen/Desktop/Trabalhos/Trab Deise/Dados/SDR.xlsx", sheet = "Data for Trends")


df2 <- select(df, Country, Year, sdg1_wpc)


df4 <- df2 %>% 
      filter(Country %in% c("Brazil", "Argentina", "Chile", 
                            "Paraguay", "Bolivia", "Peru", "Ecuador")) %>%
      na.omit()
df4


font <- "Barlow Condensed"
font_add_google(family=font, font)
bg <- "#006778"
txt_col <- "grey95"
theme_set(theme_minimal(base_family = font))
showtext_auto(enable = TRUE)


gpov <- 
  df4 %>% 
  ggplot(aes(x=Year, y=sdg1_wpc, group=Country)) +
  geom_line(aes(color=Country)) +
  geom_point(data=df4 %>% filter(Year=="2021"),
             aes(color=Country)
  )+
  geom_text_repel(data=df4 %>% filter(Year=="2021"),
                  aes(label=Country, color=Country),
                  hjust=-.4,
                  min.segment.length = Inf,
                  family=font, 
                  size = 6, alpha = 4) +
  scale_color_manual(values = c("seagreen1", "coral1", "gold1", 
                                "azure3", "lightblue1", "pink1", "yellow")) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous (limits = c(0,10), breaks = c(seq(0, 10, 2))) +
  xlab("Year") +
  ylab("Poverty Rate(%)") +
  labs(title = "Poverty Rate in Latin America ",
       subtitle = "Poverty headcount ratio at $1.90/day",
       caption ="Milena Auzier | Data: Sustainable Development Report"
  ) +
  coord_cartesian(clip = 'off') +
  theme_minimal(base_family = font) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color="darkorange", size=14),
    axis.title = element_text(color="darkorange", size=14),
    plot.title = element_text(size=25, hjust=0, color="darkorange",face = "bold", margin = margin(0,0,0,0)),
    plot.subtitle = element_text(size=22, hjust=0, color="darkorange",face = "bold", margin = margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5, color="darkorange", size = 10),
    legend.position = "none",
    plot.background = element_rect(fill = "#000033", color = "#000033"),
    plot.margin = margin(30, 30, 30, 30))

 gpov

