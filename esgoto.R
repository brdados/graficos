# DOMICÍLIOS COM ESGOTO ----

library(here)
library(brdados)
library(ipeaData)
library(ggplot2)
library(dplyr)
library(Cairo)

here() # dir de trabalho

# Carregar dados
bd <- ipeadata("SPESCO", type = "tibble")

# Gráfico Brasil
p <- bd %>%
        filter(NIVNOME == "Brasil") %>% 
        rename(ano = ANO) %>%
        mutate(cob_esgoto = VALVALOR * 100) %>% 
        ggplot(aes(x = ano, y = cob_esgoto)) +
        geom_line(size = .98, alpha = 0.9) +
        scale_x_continuous(breaks = seq(1981, 2014, by = 3)) +
        scale_y_continuous(breaks = seq(20, 100, by = 20), limits = c(0, 100)) +
        labs(color = NULL, x = NULL, y = "Presença de esgoto (%)",
             caption = "\nFonte: Ipeadata (goo.gl/Awat5Z)",
             title = "Cobertura de Esgoto",
             subtitle = "Percentual de domicílios com cobertura adequada de esgoto") +
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
ggsave(plot = p, "esgoto.png", type = "cairo-png")

