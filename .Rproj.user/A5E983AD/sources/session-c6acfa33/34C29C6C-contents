library(basedosdados)
library(tidyverse)

set_billing_id("insper-385811")

query <- bdplyr("br_ibge_pnad.microdados_compatibilizados_domicilio")
query <- query %>% 
  filter(ano %in% c(2003, 2006, 2009))

pnad_d <- query %>% bd_collect(.)
save(pnad_d, file = "pnad_d.RData")

query <- bdplyr("br_ibge_pnad.microdados_compatibilizados_pessoa")
query <- query %>% 
  filter(ano %in% c(2003, 2006, 2009))

pnad_p <- query %>% bd_collect(.)
save(pnad_p, file = "pnad_p.RData")
