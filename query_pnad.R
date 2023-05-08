library(tidyverse)

pnad03 <- readRDS("PNAD/pnad-2003.rds") %>% as_tibble()
pnad06 <- readRDS("PNAD/pnad-2006.rds") %>% as_tibble()
pnad09 <- readRDS("PNAD/pnad-2009.rds") %>% as_tibble()

pnad03 <- pnad03 %>% 
  select(ano = v0101, uf, n_controle = v0102, n_serie = v0103, sexo = v0302, idade = v8005,
         n_familia = v0403, raca = v0404, mae_viva = v0405, nasceu_mun = v0501, nasceu_uf = v0502,
         le_escreve = v0601, frequenta_escola = v0602, trabalhou_semana = v9001) %>% 
  mutate(id_domicilio = paste(ano, n_controle, n_serie, sep = ""),
         nasceu_uf = replace_na(nasceu_uf, 0),
         frequenta_escola = replace_na(frequenta_escola, 0),
         trabalhou_semana = replace_na(trabalhou_semana, 0),
         across(c(sexo, raca, mae_viva, nasceu_mun, nasceu_uf, le_escreve, frequenta_escola, trabalhou_semana),
                as_factor),
         sexo = fct_recode(sexo, "M" = "2", "F" = "4"),
         raca = fct_collapse(raca, "bra" = c("2", "6"), "pre" = c("4", "8", "0"), "nd" = "9"),
         mae_viva = fct_recode(mae_viva, "S" = "1", "N" = "3", "ns" = "5", "nd" = "9"),
         nasceu_mun = fct_recode(nasceu_mun, "S" = "1", "N" = "3", "nd" = "0"),
         nasceu_uf = fct_recode(nasceu_uf, "S" = "2", "N" = "4", "nd" = "0"),
         le_escreve = fct_recode(le_escreve, "S" = "1", "N" = "3", "nd" = "0"),
         frequenta_escola = fct_recode(frequenta_escola, "S" = "2", "N" = "4", "nd" = "0"),
         trabalhou_semana = fct_recode(trabalhou_semana, "S" = "1", "N" = "3", "nd" = "0"))
  

pnad06 <- pnad06 %>% 
  select(ano = v0101, uf, n_controle = v0102, n_serie = v0103, sexo = v0302, idade = v8005,
         n_familia = v0403, raca = v0404, mae_viva = v0405, nasceu_mun = v0501, nasceu_uf = v0502,
         le_escreve = v0601, frequenta_escola = v0602, trabalhou_semana = v9001) %>% 
  mutate(id_domicilio = paste(ano, n_controle, n_serie, sep = ""),
         nasceu_uf = replace_na(nasceu_uf, 0),
         frequenta_escola = replace_na(frequenta_escola, 0),
         trabalhou_semana = replace_na(trabalhou_semana, 0),
         across(c(sexo, raca, mae_viva, nasceu_mun, nasceu_uf, le_escreve, frequenta_escola, trabalhou_semana),
                as_factor),
         sexo = fct_recode(sexo, "M" = "2", "F" = "4"),
         raca = fct_collapse(raca, "bra" = c("2", "6"), "pre" = c("4", "8", "0"), "nd" = "9"),
         mae_viva = fct_recode(mae_viva, "S" = "1", "N" = "3", "ns" = "5", "nd" = "9"),
         nasceu_mun = fct_recode(nasceu_mun, "S" = "1", "N" = "3", "nd" = "0"),
         nasceu_uf = fct_recode(nasceu_uf, "S" = "2", "N" = "4", "nd" = "0"),
         le_escreve = fct_recode(le_escreve, "S" = "1", "N" = "3", "nd" = "0"),
         frequenta_escola = fct_recode(frequenta_escola, "S" = "2", "N" = "4", "nd" = "0"),
         trabalhou_semana = fct_recode(trabalhou_semana, "S" = "1", "N" = "3", "nd" = "0"))

pnad09 <- pnad09 %>% 
  select(ano = v0101, uf, n_controle = v0102, n_serie = v0103, sexo = v0302, idade = v8005,
         n_familia = v0403, raca = v0404, mae_viva = v0405, nasceu_mun = v0501, nasceu_uf = v0502,
         le_escreve = v0601, frequenta_escola = v0602, trabalhou_semana = v9001) %>% 
  mutate(id_domicilio = paste(ano, n_controle, n_serie, sep = ""),
         nasceu_uf = replace_na(nasceu_uf, 0),
         frequenta_escola = replace_na(frequenta_escola, 0),
         trabalhou_semana = replace_na(trabalhou_semana, 0),
         across(c(sexo, raca, mae_viva, nasceu_mun, nasceu_uf, le_escreve, frequenta_escola, trabalhou_semana),
                as_factor),
         sexo = fct_recode(sexo, "M" = "2", "F" = "4"),
         raca = fct_collapse(raca, "bra" = c("2", "6"), "pre" = c("4", "8", "0"), "nd" = "9"),
         mae_viva = fct_recode(mae_viva, "S" = "1", "N" = "3", "ns" = "5"),
         nasceu_mun = fct_recode(nasceu_mun, "S" = "1", "N" = "3", "nd" = "0"),
         nasceu_uf = fct_recode(nasceu_uf, "S" = "2", "N" = "4", "nd" = "0"),
         le_escreve = fct_recode(le_escreve, "S" = "1", "N" = "3", "nd" = "0"),
         frequenta_escola = fct_recode(frequenta_escola, "S" = "2", "N" = "4", "nd" = "0"),
         trabalhou_semana = fct_recode(trabalhou_semana, "S" = "1", "N" = "3", "nd" = "0"))

pnad <- pnad03 %>% bind_rows(pnad06, pnad09)
save(pnad, file = "pnad.RData")


dom03 <- readRDS("PNAD/dom2003.rds") %>% as_tibble()
dom06 <- readRDS("PNAD/dom2006.rds") %>% as_tibble()
dom09 <- readRDS("PNAD/dom2009.rds") %>% as_tibble()

dom03 <- dom03 %>% 
  select(ano = v0101, uf, n_controle = v0102, n_serie = v0103, total_moradores = v0105,
         estrato = v4602, peso = v4611, renda_mensal = v4614,
         UPA = v4615, STRAT = v4617, PSU = v4618) %>% 
  mutate(id_domicilio = paste(ano, n_controle, n_serie, sep = ""))

dom06 <- dom06 %>% 
  select(ano = v0101, uf, n_controle = v0102, n_serie = v0103, total_moradores = v0105,
         estrato = v4602, peso = v4611, renda_mensal = v4614,
         UPA, STRAT = v4617, PSU = v4618) %>% 
  mutate(id_domicilio = paste(ano, n_controle, n_serie, sep = ""))

dom09 <- dom09 %>% 
  select(ano = v0101, uf, n_controle = v0102, n_serie = v0103, total_moradores = v0105,
         estrato = v4602, peso = v4611, renda_mensal = v4614,
         UPA, STRAT = v4617, PSU = v4618) %>% 
  mutate(id_domicilio = paste(ano, n_controle, n_serie, sep = ""))

dom <- dom03 %>% bind_rows(dom06, dom09)
save(dom, file = "dom.RData")

