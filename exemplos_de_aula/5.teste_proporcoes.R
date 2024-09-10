# pesquisa eleitoral

# candidato A ou candidato B

# fazemos uma pesquisa eleitoral

# sorteamos aleatoriamente 3000 pessoas da cidade que votam

# e dá que 47% vota no candidato A e 53% no candidato B

# será que o candidato A tem chance de ganhar com esse 47%?

observado = 100*0.47
tamanho_amostra = 100
p = 0.5

library(infer)
library(tidyverse)

prop.test(
  x = observado,
  n = tamanho_amostra,
  p = 0.50
)
