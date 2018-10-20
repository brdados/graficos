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
p_br <- bd %>%
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
             subtitle = "Percentual de domicílios com água potável") +
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
        annotate("text", x = 1985.9, y = 5, label = "Constituição", parse = FALSE) +
        annotate("text", x = 1991.9, y = 5, label = "Lei do SUS", parse = FALSE) +
        annotate("text", x = 2010.3, y = 7, label = "Diretrizes Nacionais \npara o Saneamento \nBásico", parse = FALSE)
 
# Gráfico Regiões
p_reg <- bd %>%
        filter(NIVNOME == "Regiões") %>% 
        rename(ano = ANO) %>%
        mutate(cob_agua = VALVALOR * 100) %>% 
        ggplot(aes(x = ano, y = cob_agua,  group = TERNOME, color = TERNOME)) +
        geom_line(size = .98, alpha = 0.9) +
        scale_x_continuous(breaks = seq(1981, 2014, by = 3)) +
        scale_y_continuous(breaks = seq(20, 100, by = 20), limits = c(0, 100)) +
        labs(color = NULL, x = NULL, y = "Acesso a água potável (%)",
             caption = "\nFonte: Ipeadata (goo.gl/PkFfMw)",
             title = "Acesso a Água Potável",
             subtitle = "Percentual de domicílios com água potável") +
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
        annotate("text", x = 1985.9, y = 5, label = "Constituição", parse = FALSE) +
        annotate("text", x = 1991.9, y = 5, label = "Lei do SUS", parse = FALSE) +
        annotate("text", x = 2010.3, y = 7, label = "Diretrizes Nacionais \npara o Saneamento \nBásico", parse = FALSE)
       
# Exportação
ggsave("agua_potavel_br.png", p_br, 
       width = 10, 
       height = 7, 
       units = "in", 
       type = "cairo-png")

ggsave("agua_potavel_reg.png", p_reg, 
       width = 10, 
       height = 7, 
       units = "in", 
       type = "cairo-png")

