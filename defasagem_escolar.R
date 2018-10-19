# DEFASAGEM ESCOLAR - 10 A 14 ANOS ----

library(here)
library(brdados)
library(ipeaData)
library(ggplot2)
library(dplyr)
library(Cairo)

here() # dir de trabalho

# Carrega a base de dados pela API do IPEA. A chamada não está conectada com
# a análise para que os dados não sejam baixados toda vez que for feita alguma
# alteração
bd <- ipeadata("DE1014P", type = "tibble")

# Gráfico Brasil
p <- bd %>%
  filter(NIVNOME == "Brasil") %>% 
  rename(ano = ANO,
         defasagem = VALVALOR) %>% 
  #mutate(ano = as.numeric(ANO)) %>% 
  ggplot(aes(x = ano, y = defasagem)) +
  geom_line(size = .98, alpha = 0.9) +
  scale_x_continuous(breaks = seq(1981, 2014, by = 3)) +
  labs(color = NULL, x = NULL, y = "Defasagem (em anos)",
       caption = "\nFonte: Ipeadata (goo.gl/a2hTsF)",
       title = "Defasagem Escolar Média - 10 a 14 anos",
       subtitle = "Diferença entre a escolaridade esperada para  uma criança e a atingida") +
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
  annotate("text", x = 1996.2, y = 2.6, label = "bold(FHC)", parse = TRUE) +
  annotate("text", x = 2004.2, y = 2.6, label = "bold(Lula)", parse = TRUE) +
  annotate("text", x = 2012.5, y = 2.6, label = "bold(Dilma)", parse = TRUE)

# Exportação
ggsave(plot = p, "defasagem_escolar.png", type = "cairo-png")
  
  

