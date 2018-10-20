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
        scale_y_continuous(breaks = seq(10, 90, by = 20)) +
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
        geom_vline(xintercept = 1995,
                   size = 0.98, 
                   colour = "#8c510a", 
                   alpha = 0.9, 
                   linetype = "dashed") +
        geom_vline(xintercept = 2003, 
                   size = 0.98, 
                   colour = "#8c510a", 
                   alpha = 0.9, 
                   linetype = "dashed") +
        geom_vline(xintercept = 2011, 
                   size = 0.98, 
                   colour = "#8c510a", 
                   alpha = 0.9, 
                   linetype = "dashed") +
        annotate("text", x = 1990.3, y = 2.6, label = "bold(Constituição)", parse = TRUE) +
        annotate("text", x = 1996, y = 2.6, label = "bold(FHC)", parse = TRUE) +
        annotate("text", x = 2004, y = 2.6, label = "bold(Lula)", parse = TRUE) +
        annotate("text", x = 2012.2, y = 2.6, label = "bold(Dilma)", parse = TRUE)

# Exportação
ggsave(plot = p, "agua_potavel.png", type = "cairo-png")
