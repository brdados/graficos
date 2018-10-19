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
  labs(color = NULL, x = "Ano", y = "Defasagem (em anos)",
       caption = "\nFonte: Ipeadata (goo.gl/a2hTsF)",
       title = "Defasagem Escolar Média - 10 a 14 anos",
       subtitle = "Diferença entre a escolaridade esperada para  uma criança e a atingida") +
  tema_brdados() +
  geom_rect(aes(xmin = 1981, xmax = 1988, ymin = -Inf, ymax = Inf), alpha = .005) +
  geom_rect(aes(xmin = 1995, xmax = 2002, ymin = -Inf, ymax = Inf), alpha = .005) +
  geom_rect(aes(xmin = 2011, xmax = 2014, ymin = -Inf, ymax = Inf), alpha = .005) +
  annotate("text", x = 1984.5, y = .9, label = "bold(Pré-Constituição)", parse = TRUE) +
  annotate("text", x = 1998.5, y = 2.6, label = "bold(FHC)", parse = TRUE) +
  annotate("text", x = 2006.5, y = 2.6, label = "bold(Lula)", parse = TRUE) +
  annotate("text", x = 2012.5, y = 2.6, label = "bold(Dilma)", parse = TRUE)

# Exportação
ggsave(plot = p, "defasagem_escolar.png", type = "cairo-png")
  
  

