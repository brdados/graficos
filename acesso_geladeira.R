## Acesso a geladeira por classes
## Brasil em Dados

# Se os pacotes necessários não estiverem já instalados, fazer a instalação
if (! "readxl" %in% installed.packages()) install.packages("readxl")
if (! "tidyr" %in% installed.packages()) install.packages("tidyr")
if (! "dplyr" %in% installed.packages()) install.packages("dplyr")
if (! "ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (! "directlabels" %in% installed.packages()) install.packages("directlabels")

# Carrega os pacotes
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(directlabels)

### carregando o tema BR Dados com algumas modificações
tema_br_dados <- function(tam.fonte = 13, fonte = "sans", angle.x = 360, leg_pos = "bottom"){
  theme_minimal(base_size = tam.fonte, base_family = fonte) + 
    theme(axis.text = element_text(color = "black"),
          legend.text = element_text(),
          legend.position = "none",
          plot.subtitle = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          axis.line.x = element_line(),
          plot.caption = element_text(size = 10)
    )
}


# Acesso a geladeira - lê os dados
bd = read_xlsx("brasilemdados_geladeira.xlsx", skip = 2, n_max = 20, sheet = 2) %>% 
  select(Ano, DE, C, AB)

# Faz o gráfico
g = bd %>% gather("Classe", "Perc", -Ano) %>%
  mutate(Percentual = Perc * 100) %>% 
  mutate(classe = case_when(
    Classe == "AB" ~ "Classes AB",
    Classe == "C" ~ "Classe C",
    Classe == "DE" ~ "Classes DE"
  )) %>% 
  ggplot(aes(x = Ano, y = Percentual, color = classe, group = classe)) + 
  geom_line(size = .65) + tema_br_dados() + # gráficos de linhas + tema BRDADOS
  ylim(0, 100) +                            # Limites do y
  scale_x_continuous(breaks = seq(from=1995, to=2015, by=2)) +  # define os breaks do eixo x
  scale_color_brewer(palette = "Dark2") +                       # Paleta de cores
  geom_label(aes(x = 1998, y = 18, label = "Governo\nFHC"), fill = "white", color = "black", 
             label.r = unit(0, "lines"), label.size = 0.1) +    # Coloca caixa
  geom_vline(xintercept=2003, size=0.98, colour="#8c510a", 
             alpha=0.9, linetype="dashed") +                    # Coloca linha vertical
  geom_label(aes(x = 2007, y = 18, label = "Governo\nLula"), fill = "white", color = "black", 
             label.r = unit(0, "lines"), label.size = 0.1) +
  geom_vline(xintercept=2011, size=0.98, colour="#8c510a", 
             alpha=0.9, linetype="dashed") +
  geom_label(aes(x = 2013.5, y = 18, label = "Governo\nDilma"), fill = "white", color = "black", 
             label.r = unit(0, "lines"), label.size = 0.1) +
  labs(title = "Posse de geladeira por classes de renda",
       subtitle = "Menos desigualdade no acesso a bens básicos",
       caption = paste0("Fonte: PNAD (IBGE - goo.gl/ZQCPHX)\n",
                        "Classe A-B: 1+ SM per cap. / ",
                        "Classe C: 0,5-1 SM per cap. / ",
                        "Classe D-E: 0,5- SM per cap."),
       y = "% de domicílios com geladeira",
       x = "") +        # Coloca título, subtítulo, nota, e label dos eixos x e y
  geom_dl(aes(label = classe), method = list(dl.trans(x = x + 2, y = y-.2), "first.points")) # Coloca label nas linhas

# Salva o gráfico
ggsave(file="acesso_geladeira.png", plot=g, dpi=300, width=6.89, height=4.88)
