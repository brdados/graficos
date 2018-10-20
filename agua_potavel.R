# DOMICÍLIOS COM ÁGUA POTÁVEL ----

library(here)
library(brdados)
library(ipeaData)
library(ggplot2)
library(dplyr)
library(Cairo)

here() # dir de trabalho

# Carregar dados
bd <- ipeadata("SPAGUA", type = "tibble")

# Gráfico Brasil
p <- bd %>%
        filter(NIVNOME == "Brasil") %>% 
        rename(ano = ANO) %>%
        mutate(cob_agua = VALVALOR * 100) %>% 
        ggplot(aes(x = ano, y = cob_agua)) +
        geom_line(size = .98, alpha = 0.9) +
        scale_x_continuous(breaks = seq(1981, 2014, by = 3)) +
        scale_y_continuous(breaks = seq(20, 100, by = 20), limits = c(0, 100)) +
        labs(color = NULL, x = NULL, y = "Acesso a água potável (%)",
             caption = "\nFonte: Ipeadata (goo.gl/PkFfMw)",
             title = "Acesso a Água Potável",
             subtitle = "Percentual de domicílios com água potável na rede geral") +
        tema_brdados() +
        geom_vline(xintercept = 1988, 
                   size = 0.98, 
                   colour = "#8c510a", 
                   alpha = 0.9, 
                   linetype = "dashed") +
        geom_vline(xintercept = 1990,
                   size = 0.98, 
                   colour = "#8c510a", 
                   alpha = 0.9, 
                   linetype = "dashed") +
        geom_vline(xintercept = 2007, 
                   size = 0.98, 
                   colour = "#8c510a", 
                   alpha = 0.9, 
                   linetype = "dashed") +
        annotate("text", x = 1985.9, y = 97, label = "Constituição", parse = FALSE) +
        annotate("text", x = 1991.8, y = 97, label = "Lei do SUS", parse = FALSE) +
        annotate("text", x = 2010.3, y = 92, label = "Diretrizes Nacionais \npara o Saneamento \nBásico", parse = FALSE)
        
# Exportação
ggsave("agua_potavel.png", p, 
       width = 10, 
       height = 7, 
       units = "in", 
       type = "cairo-png")

