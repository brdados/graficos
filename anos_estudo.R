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

# Gráfico Sexo ----
p_sex <- bd %>% 
        gather("Sexo", "Anos_estudo", 8:9) %>% 
        ggplot(aes(x = Ano, y = Anos_estudo, group = Sexo, color = Sexo)) +
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
        geom_vline(xintercept = 1996,
                   size = 0.98, 
                   colour = "#8c510a", 
                   alpha = 0.9, 
                   linetype = "dashed") +
        geom_vline(xintercept = 2006, 
                   size = 0.98, 
                   colour = "#8c510a", 
                   alpha = 0.9, 
                   linetype = "dashed") +
        annotate("text", x = 1990.5, y = 1, label = "bold(Constituição)", parse = TRUE) +
        annotate("text", x = 1997.8, y = 1, label = "bold(FUNDEF)", parse = TRUE) +
        annotate("text", x = 2007.8, y = 1, label = "bold(FUNDEB)", parse = TRUE)

# Exportação
ggsave("anos_estudo_br.png", p_br, 
       width = 10, 
       height = 7, 
       units = "in", 
       type = "cairo-png")

ggsave("anos_estudo_reg.png", p_reg, 
       width = 10, 
       height = 7, 
       units = "in", 
       type = "cairo-png")

ggsave("anos_estudo_sexo.png", p_sex, 
       width = 10, 
       height = 7, 
       units = "in", 
       type = "cairo-png")
