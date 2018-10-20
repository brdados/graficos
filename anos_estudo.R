# ANOS DE ESTUDO ----

library(here)
library(brdados)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Cairo)

here() # dir de trabalho

# Carregar dados
bd <- read_excel("anos_estudo_ipeaDATA.xlsx", skip = 2)

# Gráfico Brasil ----
p_br <- bd %>% 
        ggplot(aes(x = Ano, y = Brasil)) +
        geom_line(size = .98, alpha = 0.9) +
        scale_x_continuous(breaks = seq(1976, 2015, by = 3)) +
        labs(color = NULL, x = NULL, y = "Anos de Estudo (Média)",
             caption = "\nFonte: Elaboração própria a partir de dados da PNAD - IBGE",
             title = "Anos de Estudo",
             subtitle = "Média de anos de estudo da população brasileira com mais de 14 anos") +
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
        annotate("text", x = 1990.4, y = 8.2, label = "bold(Constituição)", parse = TRUE) +
        annotate("text", x = 1996, y = 8.2, label = "bold(FHC)", parse = TRUE) +
        annotate("text", x = 2004, y = 8.2, label = "bold(Lula)", parse = TRUE) +
        annotate("text", x = 2012.2, y = 8.2, label = "bold(Dilma)", parse = TRUE)

# Gráfico Regiões ----
p_reg <- bd %>% 
        gather("Região", "Anos_estudo", 3:7) %>% 
        ggplot(aes(x = Ano, y = Anos_estudo, group = Região, color = Região)) +
        geom_line(size = .98, alpha = 0.9) +
        scale_x_continuous(breaks = seq(1976, 2015, by = 3)) +
        labs(color = NULL, x = NULL, y = "Anos de Estudo (Média)",
             caption = "\nFonte: Elaboração própria a partir de dados da PNAD - IBGE",
             title = "Anos de Estudo",
             subtitle = "Média de anos de estudo da população brasileira com mais de 14 anos") +
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
        annotate("text", x = 1990.5, y = 1, label = "bold(Constituição)", parse = TRUE) +
        annotate("text", x = 1996, y = 1, label = "bold(FHC)", parse = TRUE) +
        annotate("text", x = 2004, y = 1, label = "bold(Lula)", parse = TRUE) +
        annotate("text", x = 2012.2, y = 1, label = "bold(Dilma)", parse = TRUE)

# Exportação
ggsave(plot = p_br, "anos_estudo_br.png", type = "cairo-png")
ggsave(plot = p_reg, "anos_estudo_reg.png", type = "cairo-png")