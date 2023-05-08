library(tidyverse)
library(survey)
library(MatchIt)

load("pnad.RData")
load("dom.RData")

dom <- dom %>% 
  filter(!is.na(renda_mensal)) %>% 
  mutate(renda_per_capita = renda_mensal / total_moradores) %>% 
  slice_min(order_by = renda_per_capita, prop = 0.2)

domicilios <- pnad %>% 
  filter(ano == 2006 | ano == 2009) %>% 
  semi_join(dom, by = "id_domicilio") %>% 
  filter(idade %in% c(14, 15, 16, 17)) %>%
  mutate(g_idade = ifelse(idade == 14 | idade == 15, "BVJ", "nBVJ")) %>% 
  group_by(id_domicilio, n_familia, ano, g_idade) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = g_idade, values_from = n) %>% 
  mutate(controle = nBVJ == 2 & is.na(BVJ),
         tratamento = BVJ == 1 & nBVJ == 1) %>% 
  filter(controle | tratamento) %>% 
  select(id_domicilio, ano, n_familia, tratamento)

pnad <- pnad %>% 
  filter(idade %in% 14:17) %>% 
  inner_join(domicilios, by = c("id_domicilio", "ano", "n_familia")) %>% 
  inner_join(dom, by = c("id_domicilio", "ano", "uf"))

pnad %>% 
  group_by(tratamento, ano) %>% 
  count()

svy <- svydesign(id = ~1,
                 strata = ~STRAT,
                 weights = ~peso, 
                 data = pnad %>% filter(ano == 2009))

psm <- svyglm(as.integer(frequenta_escola) ~ sexo + raca,
              design = svy,
              family = binomial(link = "logit"))
psm <- glm(as.integer(frequenta_escola) ~ sexo + raca_cor + trabalhou_semana +
             renda_per_capita + total_pessoas,
           family = binomial(link = "logit"),
           data = pnad_p %>% filter(ano == 2009))

psm <- glm(as.integer(tratamento) ~ sexo + raca + mae_viva + nasceu_mun + 
             nasceu_uf + le_escreve + trabalhou_semana + total_moradores,
           family = binomial(link = "logit"),
           data = pnad %>% filter(ano == 2009))
summary(psm)
pscore <- predict(psm, type="response")

ggplot(pnad %>% filter(ano == 2009), mapping = aes(pscore)) + 
  geom_density() +
  facet_wrap(~tratamento) +
  xlab("p")

match_ATT <- matchit(as.integer(tratamento) ~ sexo + raca + mae_viva + nasceu_mun + 
                       nasceu_uf + le_escreve + trabalhou_semana + total_moradores,
                     data = pnad %>% filter(ano == 2009),
                     link = "logit",
                     method = "full",
                     discard = "both")
summary(match_ATT)

plot(match_ATT,type="density",interactive=FALSE)

