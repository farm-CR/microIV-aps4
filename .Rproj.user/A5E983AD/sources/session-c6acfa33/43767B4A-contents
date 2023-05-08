library(tidyverse)
library(survey)
library(MatchIt)

load("pnad_d.RData")
load("pnad_p.RData")

pnad_d <- pnad_d %>% 
  filter(!is.na(renda_mensal_domiciliar_compativel_1992_deflacionado)) %>% 
  mutate(renda_per_capita = renda_mensal_domiciliar_compativel_1992_deflacionado / total_pessoas) %>% 
  slice_min(order_by = renda_per_capita, prop = 0.2)

domicilios <- pnad_p %>% 
  semi_join(pnad_d, by = "id_domicilio") %>% 
  filter(idade %in% c(14, 15, 16, 17)) %>%
  mutate(g_idade = ifelse(idade == 14 | idade == 15, "BVJ", "nBVJ")) %>% 
  group_by(id_domicilio, numero_familia, ano, g_idade) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = g_idade, values_from = n) %>% 
  mutate(controle = nBVJ == 2 & is.na(BVJ),
         tratamento = BVJ == 1 & nBVJ == 1) %>% 
  filter(controle | tratamento) %>% 
  select(id_domicilio, ano, numero_familia, tratamento)

pnad_p <- pnad_p %>% 
  filter(idade %in% 14:17) %>% 
  inner_join(domicilios, by = c("id_domicilio", "ano", "numero_familia")) %>% 
  inner_join(pnad_d, by = c("id_domicilio", "ano"))
  # select(id_domicilio, ano, numero_familia, tratamento, id_regiao = id_regiao.x, 
  #        id_uf = id_uf.x, sexo, idade, raca_cor, frequenta_escola, renda_per_capita, tinha_outro_trabalho,
  #        peso_amostral) %>% 

pnad_p %>% 
  group_by(total_pessoas) %>% 
  count()

svy <- svydesign(id = ~1,
                 strata = ~id_uf.x,
                 weights = ~peso_amostral, 
                 data = pnad_p %>% filter(ano == 2009))

psm <- svyglm(as.integer(frequenta_escola) ~ sexo + raca_cor + trabalhou_semana +
                renda_per_capita + total_pessoas,
              design = svy,
              family = binomial(link = "logit"))
psm <- glm(as.integer(frequenta_escola) ~ sexo + raca_cor + trabalhou_semana +
             renda_per_capita + total_pessoas,
           family = binomial(link = "logit"),
           data = pnad_p %>% filter(ano == 2009))

psm <- glm(as.integer(tratamento) ~ sexo + raca_cor + trabalhou_semana +
             renda_per_capita + total_pessoas,
           family = binomial(link = "logit"),
           data = pnad_p %>% filter(ano == 2009))

summary(psm)
pscore <- predict(psm, type="response")

ggplot(pnad_p %>% filter(ano == 2009), mapping = aes(pscore)) + 
  geom_density() +
  facet_wrap(~tratamento) +
  xlab("p")

match_ATT <- matchit(as.integer(tratamento) ~ as.factor(sexo) + as.factor(raca_cor) + 
                       trabalhou_semana + total_pessoas,
                     data=pnad_p %>% filter(ano == 2009),
                     link="logit",
                     method="full",
                     discard="both")
summary(match_ATT)

plot(match_ATT,type="density",interactive=FALSE)


# pnad_d <- read.fwf(file = "PNAD/DOM2006.txt", widths = dicdom2009$tamanho, fileEncoding="latin1")
