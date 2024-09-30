
# Na ultima eleição a candidata recebeu 53% dos votos

# 4 anos depois, fizemos uma pesquisa e em uma amostra de tamanho 10000
# ela recebeu 51% das intenções de voto. Essa queda
# é motivo pra preocupação ou pode ser um "acaso"

prop.test(
  n = 10000,
  x = 5100,
  p = 0.53)

x_barra = 5100/10000
desv_pad_x_barra = sqrt(x_barra*(1-x_barra))/sqrt(10000)

# valor p deu muito pequeno, o que quer dizer que não tinha muito espaço
# pra ser pior o resultado do que foi. é motivo pra se preocupar o 53 caiu sim...

library(infer)
library(tidyverse)

tabela_exemplo <- tibble(
  voto = c(rep("candidata A", 5100), rep("candidato B", 4900))
)

prop_test(
  tabela_exemplo,
  voto ~ NULL,
  p = 0.53
)

dados_brutos <- tabela_exemplo |>
  specify(response = voto, success = "candidata A")

sampling_dist <- tabela_exemplo |>
  specify(response = voto, success = "candidata A") |>
  assume("z")

estatistica_do_teste <- dados_brutos |>
  hypothesise(null = "point", p = 0.52) |>
  calculate(stat = "z")

sampling_dist |>
  visualise() +
  shade_p_value(estatistica_do_teste, direction = "left")
