#### GRAFICO SALARIO MINIMO ####

library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(scales)

### carregando tema BR Dados

tema_br_dados <- function (tam.fonte = 16, fonte = "sans", angle.x = 360, leg_pos = "bottom"){
  
  theme_minimal(base_size = 16, base_family = fonte) + 
    theme(axis.text = element_text(color = "black"),
          legend.text = element_text(),
          legend.position = "top",
          plot.subtitle = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          axis.line.x = element_line()
    )
}

### Puxando dados do arquivo IPEA (https://goo.gl/96BZwC)

dados.df <- fread("ipeadata[14-10-2018-06-49].csv", encoding="UTF-8")

### Ajustando data frame

dados.df[,3] <- NULL
dados.df$SMR <- dados.df[,2]
dados.df[,2] <- NULL
dados.df <- dados.df %>%
  separate(Data, c("Ano", "Mes")) %>%
  mutate(Ano = as.numeric(Ano),
         Mes = as.numeric(Mes)) %>%
  filter(Ano %in% (1958:2018), Mes=="9")

### Criando ggplot e ajustando elementos do gráfico

p <- ggplot(dados.df, aes(x=Ano, y=SMR)) + 
  geom_line(size=1.1, alpha=0.9, colour="#00441b") +
  tema_br_dados() +
  scale_x_continuous(breaks = seq(1958, 2018, by = 5)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  labs(color = NULL, x = NULL, y = "Salário Mínimo Real (R$)",
       caption = "\nFonte: Ipeadata (https://goo.gl/96BZwC)",
       title = "Salário Mínimo Real, 1958-2018",
       subtitle = "Crescimento constante ao longo dos últimos 20 anos") +
  geom_vline(xintercept=1964, size=0.98, colour="#8c510a", 
             alpha=0.9, linetype="dashed") +
  geom_vline(xintercept=1985, size=0.98, colour="#8c510a",
             alpha=0.9, linetype="dashed") +
  geom_vline(xintercept=1994, size=0.98, colour="#8c510a", 
             alpha=0.9, linetype="dashed") +
  geom_vline(xintercept=2003, size=0.98, colour="#8c510a",
             alpha=0.9, linetype="dashed")

p

### Exportando para formato .png

ggsave(p, file= "plot.png", dpi=300, width=6.89, height=4.88)