library(tidyverse)

dados <- readxl::read_excel("exemplos_de_aula//BD_CIS0684.xlsx")

dados |>
  filter(
    sexo == "Feminino"
  ) |>
  t_test(idade1 ~ NULL, mu = 41)

# teste de comparacao de grupos -------------------------------------------

# H_0: os homens e mulheres tem a mesma idade
# M, H. M-H = 0

# media amostral das mulheres - media amostral dos homens
# eu faco medias dos desvio padrao das populações e
# minha "estatistica" fica:

# (media amostral das mulheres - media amostral dos homens)
# / (sobre)
# raiz(desvio padrao das mulheres + desvio padrao dos homens)/raiz(2)

dados |>
  t_test(idade1 ~ sexo, alternative = "less")

# nao temos evidência para rejeitar a hipotese de que as idades sao iguais

dados |>
  prop_test(p32c ~ sexo, success = "Sim")

dados |>
  t_test(p32c ~ sexo)
# isso aqui dá erro porque eu preciso de um desvio padrao no teste t, que é estimado

# fazendo graficos --------------------------------------------------------

dados_brutos <- dados |>
  group_by(sexo) |>
  sample_n(4) |>
  specify(idade1 ~ sexo)

estatistica <- dados_brutos |>
  calculate(stat = "t")

dados_brutos |>
  assume("t") |>
  visualise() +
  shade_p_value(estatistica, direction = "right")


# -------------------------------------------------------------------------

# Teste chi quadrado de independência

dados_brutos <- dados |>
  filter(idade1 < 45) |>
  specify(p32c ~ idade, success = "Sim") |>
  hypothesise(null = "independence")

dados |> count(idade)

dados |>
  count(idade, p32c) |>
  spread(p32c, n)

glm(factor(p32c) ~ idade, data = dados, family = "binomial") |>
  summary()

estatistica_chi_quadrado <- dados_brutos |>
  calculate(stat = "Chisq")

dados_brutos |>
  assume("Chisq") |>
  visualise() +
  shade_p_value(estatistica_chi_quadrado, direction = "right")

dados |>
  prop_test(p32c ~ idade, success = "Sim")

chisq_test(dados, p32c ~ idade)

# mais testes -------------------------------------------------------------

# prop_test
# sempre vai ser para comparar proporcoes entre grupos ou proporcoes
# em proporcoes de uma coluna com um valor fixo

# t_test
# sempre vai ser para comparar valores numeros entr grupos ou
# a media/sd/mediana de uma coluna com um valor fixo

# pra outros casos:
# hypothetize pra definir a hipótese nula +
# assume pra definir a estatistica (algumas hipoteses só funcionam com certas 'assume') +
# calculate da estatistica e plota o gráfico


dados |>
  specify(p32c ~ idade*sexo, success = "Sim") |>
  hypothesise(null = "independence")

dados |>
  prop_test(p32c ~ NULL, z = 0, p = 0.1)
