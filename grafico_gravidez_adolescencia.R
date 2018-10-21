# contruir série temporal de gravidez na adolescencia a partir de dados do data.sus

# pacotes necessarios
#install.packages("read.dbc")
#install.packages("downloader")
#devtools::install_github("brdados/brdados")
library(data.table)
library(ggplot2)
library(brdados)

# carregando funcao que coleta os dados de nacimento infantil direto do ftp do datasus
devtools::source_url("https://raw.githubusercontent.com/silvadenisson/mortalidadeInfantil/master/sinasc.dn.R")

dados <- NULL
for(i in 1996:2016){
  tabela <- sinasc.dn(i) 
  
  tabela <- as.data.frame(table(tabela$IDADEMAE, tabela$CODMUNRES))
  tabela$Ano <-  i
  
  dados <- rbind(dados, tabela)
  print(i)
}


dados <- data.table(dados)
dados$Var1 <- as.numeric(as.character(dados$Var1))

dt <- dados[, .(amenor15 = sum(Freq[Var1 < 15]),
                a1519 = sum(Freq[Var1 >14 & Var1 < 20]),
                amior19 = sum(Freq[Var1 > 19])), by = Ano]

# faixa etaria de 15 a 19 escolhida usando o padrao da Organizacao Mundial da Saude
# http://apps.who.int/gho/data/node.wrapper.imr?x-id=4669

# calculando a taxa
# populacao baixada http://tabnet.datasus.gov.br/cgi/deftohtm.exe?novapop/cnv/popbr.def
# serie 2000 a 2015
idade <- read.csv2("idade_faixa_19.csv", stringsAsFactors = F)

dt <- merge(dt, idade[, c(1, 3)], by = "Ano", all.x = T)

# anterior a 2000 foi baixada direto do ftp do dadasus
# ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POP

for(i in 96:99){
  idade9 <- read.csv2(paste0("POPBR", i, "/POPBR", i, ".csv"), sep = ",", stringsAsFactors = F) 
  idade9 <- data.table(idade9)
  id <- idade9[, .(total = sum(POPULACAO[FXETARIA %in% c("1515", "1616", "1717", "1818", "1919")])),
               by = list(SEXO, ANO)]
  dt$Feminino[dt$Ano == id$ANO[1]] <- id$total[2]
}


# grafico
ggplot(dt, aes(Ano, taxa)) + geom_line() + 
  labs(title = "Taxa de Gravidez na Adolescência entre 15 a 19 anos (a cada 1.000)",
                                                 caption = "Fonte: DATASUS", y = "Taxa") +
  scale_x_continuous(breaks = 1996:2016) + 
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175), limits = c(0, 200)) +
  geom_vline(xintercept = 1998, linetype = 3) +
  annotate("text", label = "Orçamento próprio para o PSF", x = 1999.4, y = 170, size = 3) +
  geom_vline(xintercept = 1999, linetype = 3) +
  annotate("text", label = "1.º Pacto da Atenção Básica", x = 2000.7, y = 90, size = 3) +
  tema_brdados()
 
ggsave("n_nascimento_adolescencia.png", width = 11.49, height = 7.44)
