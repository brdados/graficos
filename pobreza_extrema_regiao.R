# ---
# POBREZA EXTREMA POR REGIAO
# ---


# Pacotes
library(tidyverse)
library(httr)


# Grafico - % de extremamente pobres baseado em necessidades caloricas (Ipeadata)
p <- "http://ipeadata2-homologa.ipea.gov.br/api/v1/AnoValors(SERCODIGO='PMA0',NIVNOME='Regi%C3%B5es')?$top=100&$skip=0&$count=true&ANOINICIAL=1976&ANOFINAL=2014" %>%
  GET() %>%
  content() %>%
  .$value %>%
  map(bind_rows) %>%
  bind_rows() %>%
  gather("ano", "percentagem", -SERCODIGO, -TERCODIGO, -TERNOME, -NIVNOME) %>%
  mutate(ano = substr(ano, 2, 5) %>% as.numeric) %>%
  mutate(regiao = gsub("Região ", "", TERNOME)) %>%
  mutate(percentagem = as.numeric(percentagem)) %>%
  ggplot(aes(x = ano, y = I(percentagem / 100), color = regiao)) +
  geom_line(size = .98, alpha = 0.9) +
  scale_x_continuous(breaks = seq(1977, 2014, by = 4)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent, expand = c(0, 0)) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme_minimal(base_size = 16) +
  theme(axis.text = element_text(color = "black"),
        legend.text = element_text(),
        legend.position = "top",
        plot.subtitle = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        axis.line.x = element_line()) +
  labs(color = NULL, x = NULL, y = "Extremamente Pobres (%)",
       caption = "\nFonte: Ipeadata (goo.gl/5qETah)",
       title = "Extremamente Pobres por Região, 1976-2014",
       subtitle = "As linhas indicam a percentagem por região em cada ano")
       

# Exporta (a extrema pobreza caiu 76% no nordeste de 76 pra ca)
Cairo::CairoPNG(filename = "extremamente_pobres_76_2014.png", width = 9, height = 7, dpi = 72, units = "in")
p
dev.off()

