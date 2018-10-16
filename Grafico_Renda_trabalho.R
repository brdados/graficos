rm(list=ls())
#devtools::install_github("antrologos/harmonizePNAD")

library(data.table)
library(tidyverse)
library(Hmisc)
library(harmonizePNAD)
library(imputeTS)

setwd("E:/dropbox-ro/dropbox/rogerio/bancos_dados/pnads/")
anos <- c(1976:1979, 1981:1990, 1992, 1993, 1995:1999, 2001:2009, 2011:2015)


pnad_stacked <- NULL
for(ano_i in anos){
        print(ano_i)
        
        
        # Assume-se que os arquivos estejam sempre guardados em pastas nomeadas como "PNAD XXXX", e os
        # nomes dos arquivos de pessoas tenham a seguinte estrutura: "pnad.pes_XXXX.csv"
        Data <- fread(paste0("pnad ", ano_i,"/pnad.pes_",ano_i, ".csv"), colClasses = "numeric")
        
        Data <- mutate_all(Data, as.numeric)
        gc(); Sys.sleep(.2);gc()
        
        # As variáveis sobre a situação de residência (rural/urbano) não estão disponíveis diretamente 
        # nos bancos de pessoas para os anos de 1976, 1978 e 1979. Devem ser importadas dos bancos de 
        # domicílios.
        # Assume-se que os arquivos dos bancos de dados de domicílios estejam nas mesmas pastas que os 
        # arquivos de pessoas e que seus nomes tenham a seguinte estrutura: "pnad.dom_XXXX.csv"
        
        if(ano_i %in% c(1976,1978,1979)){
                
                if(ano_i == 1976){
                        rural_var = "v1004"
                }else{
                        rural_var = "v1152"
                }
                
                # A variável domicilioid já deve existir previamente. 
                Data_dom <- fread(paste0("pnad ", ano_i,"/pnad.dom_",ano_i, ".csv"),
                                  select = c("domicilioid", rural_var),
                                  colClasses = "numeric")
                Data <- merge(x = Data, y = Data_dom, by = "domicilioid",
                              all.x = T, all.y = F, sort = F)
                
                rm(Data_dom); gc()
        }
        
        originalVarNames <- names(Data)
        
        # A partir deste passo, utilizamos as funções do pacote harmonizePNAD para construir um
        # conjunto de variáveis harmonizadas e comparáveis longitudinalmente -- a partir das quais
        # serão elaborados os indicadores do IPEAData
        Data <- prepare_to_harmonize(Data, type = "pnad", year = ano_i)
        
        Data <- Data %>%
                build_identification_year() %>%
                build_identification_wgt() %>%
                
                build_geography_regionMCA() %>%
                build_geography_ruralUrban() %>%
                
                build_work_econActivity() %>%
                build_work_occupationalStatus() %>%
                build_work_hoursWorkedMainJob() %>%
                build_work_hoursWorkedAllJobs() %>%
                adjust_work_hoursWorkedMainJob() %>% # PARA CONSTRUIR AS SÉRIES QUE INCIAM ANTES DE 1992, É NECESSÁRIO ATIVAR ESSA LINHA
                
                build_income_earningsMainJob() %>%
                build_income_earningsAllJobs()
        
        gc(); Sys.sleep(.2);gc()
        
        # Removendo as variáveis originais - mantém-se apenas as variáveis harmonizadas
        Data <- Data %>% select(-originalVarNames)
        gc(); Sys.sleep(.2);gc()
        
        # Empilhandos as PNADs dos diferentes anos
        pnad_stacked <- bind_rows(pnad_stacked, Data)
        gc(); Sys.sleep(.2);gc()
}


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


### Sumarizando dados

renda <- pnad_stacked %>%
        group_by(year) %>%
        summarise(renda_todos_trab_median = wtd.quantile(earningsAllJobs, fweight, probs = .5),
                  renda_todos_trab_mean   = wtd.mean(earningsAllJobs, fweight)) %>%
        bind_rows(tibble(year = c(1980, 1991, 1994, 2000, 2010), renda_todos_trab = as.numeric(c(NA, NA, NA, NA, NA)))) %>%
        arrange(year) %>%
        mutate(renda_todos_trab_median_input = na.interpolation(renda_todos_trab_median), 
               renda_todos_trab_mean_input   = na.interpolation(renda_todos_trab_mean))


p_mean <- renda %>%
        ggplot(aes(x=year, y = renda_todos_trab_mean_input)) +
        geom_line(lwd = 1.2, alpha = .5, col = "dark blue") + 
        tema_br_dados() +
        scale_x_continuous(breaks = seq(1976, 2015, by = 5)) +
        scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE), limits = c(1000, 2500)) +
        labs(color = NULL, x = NULL, y = "Renda de todos os trabalhos (R$)",
             caption = paste0("\nFonte: PNAD (IBGE), 1976-2015. Elaboração Própria",
                             "\nValores deflacionados pelo INPC e moedas convertidas para Reais"),
             title = "Evolução da renda média do trabalho, 1976-2015",
             subtitle = "Estabilização econômica e avanços democráticos") +
        geom_vline(xintercept=1986, size=0.98, colour="#8c510a",
                   alpha=0.9, linetype="dashed") +
        geom_vline(xintercept=1994, size=0.98, colour="#8c510a", 
                   alpha=0.9, linetype="dashed") +
        geom_vline(xintercept=2003, size=0.98, colour="#8c510a",
                   alpha=0.9, linetype="dashed") +
        geom_vline(xintercept=2011, size=0.98, colour="#8c510a",
           alpha=0.9, linetype="dashed") + 
        geom_label(aes(x = 1986, y = 2250, label = "Plano Cruzado"), fill = "white", color = "black", label.r = unit(0, "lines"), label.size = 0.1) + 
        geom_label(aes(x = 1995, y = 2000, label = "Governo\nFHC"), fill = "white", color = "black", label.r = unit(0, "lines"), label.size = 0.1) + 
        geom_label(aes(x = 2003, y = 1750, label = "Governo\nLula"), fill = "white", color = "black", label.r = unit(0, "lines"), label.size = 0.1) +
        geom_label(aes(x = 2011, y = 2000, label = "Governo\nDilma"), fill = "white", color = "black", label.r = unit(0, "lines"), label.size = 0.1)


p_median <- renda %>%
        ggplot(aes(x=year, y = renda_todos_trab_median_input)) +
        geom_line(lwd = 1.2, alpha = .5, col = "dark blue") + 
        tema_br_dados() +
        scale_x_continuous(breaks = seq(1976, 2015, by = 3)) +
        scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE), limits = c(500, 1500)) +
        labs(color = NULL, x = NULL, y = "Renda de todos os trabalhos (R$)",
             caption = paste0("\nFonte: PNAD (IBGE), 1976-2015. Elaboração Própria",
                              "\nValores deflacionados pelo INPC e moedas convertidas para Reais"),
             title = "Evolução da renda do trabalho (mediana), 1976-2015",
             subtitle = "Estabilização econômica e avanços democráticos") +
        geom_vline(xintercept=1986, size=0.98, colour="#8c510a",
                   alpha=0.9, linetype="dashed") +
        geom_vline(xintercept=1995, size=0.98, colour="#8c510a", 
                   alpha=0.9, linetype="dashed") +
        geom_vline(xintercept=2003, size=0.98, colour="#8c510a",
                   alpha=0.9, linetype="dashed") +
        geom_vline(xintercept=2011, size=0.98, colour="#8c510a",
                   alpha=0.9, linetype="dashed") + 
        geom_label(aes(x = 1986, y = 1250, label = "Plano Cruzado"), fill = "white", color = "black", label.r = unit(0, "lines"), label.size = 0.1) + 
        geom_label(aes(x = 1994, y = 1000, label = "Governo\nFHC"), fill = "white", color = "black", label.r = unit(0, "lines"), label.size = 0.1) + 
        geom_label(aes(x = 2003, y = 1000, label = "Governo\nLula"), fill = "white", color = "black", label.r = unit(0, "lines"), label.size = 0.1) +
        geom_label(aes(x = 2011, y = 1250, label = "Governo\nDilma"), fill = "white", color = "black", label.r = unit(0, "lines"), label.size = 0.1)

                       
### Exportando para formato .png
ggsave(p_median, file= "plot.png", dpi=300, width=6.89, height=4.88)