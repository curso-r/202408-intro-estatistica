library(infer)
library(tidyverse)

dados <- readxl::read_excel("exemplos_de_aula//BD_CIS0684.xlsx")

# vamos estimar a distribuição populacional da idade das mulheres.
# "estimar distribuição" é chutar um histograma
idade_mulheres <- dados |>
  filter(sexo == "Feminino") |>
  with(idade1)

n_mulheres <-dados |>
  filter(sexo == "Feminino") |>
  nrow()

hist(idade_mulheres)

media_amostral <- mean(idade_mulheres)
desvio_padrao_amostral <- sd(idade_mulheres)

# vou chutar o erro da distribuição amostral da média populacional
erro_media_amostral <-desvio_padrao_amostral/sqrt(n_mulheres)

media_amostral + 2*erro_media_amostral
media_amostral - 2*erro_media_amostral

# intervalo de confiança no infer

sampling_dist <- dados |>
  filter(sexo == "Feminino") |>
  specify(response = idade1) |>
  assume("t")
# aplique o teorema central do limite
# trocando desvio padrao populacional por
# desvio padrao amostral (que dá t)

sampling_dist |>
  visualise()

media_amostral <- dados |>
  filter(sexo == "Feminino") |>
  specify(response = idade1) |>
  calculate(stat = "mean")

ci <- get_confidence_interval(
  sampling_dist,
  level = .99,
  point_estimate = media_amostral)

sampling_dist |>
  visualise() +
  shade_confidence_interval(ci)

# eu "roubei" pra fazer essa distribuição...

# o "erro_media_amostral" tem um nome técnico que é "erro padrão"
# eu "roubei" quando eu fiz o desenho ao lado, porque eu troquei
# a média populacional pela amostral.

# mas existem resultados estatísticos que me garantem que
# a distribuição X_barra / desvio_padrao_amostral é aproximadamente t com n-1
# graus de liberdades. uma t com mais do que 30 graus de liberdade é aproximadamente
# normal

# estimar proporções é um caso particular de estimar médias:
#exemplo:
quem_gosta_de_sorvete <- c(1, 1, 0, 1, 0)

mean(quem_gosta_de_sorvete)*100

(3/5)*100

sqrt(sum((quem_gosta_de_sorvete-mean(quem_gosta_de_sorvete))^2/5))

prop <- mean(quem_gosta_de_sorvete)

sqrt(prop*(1-prop))

######

# intervalo para proporções

sampling_dist <- dados |>
  filter(sexo == "Feminino") |>
  specify(response = p32c, success = "Sim") |>
  assume("z")

prop_amostral <- dados |>
  filter(sexo == "Feminino") |>
  specify(response = p32c, success = "Sim") |>
  calculate(stat = "prop")

sqrt(.27*(1-.27))/sqrt(1089)

ci <- get_confidence_interval(
  sampling_dist,
  level = .99,
  point_estimate = prop_amostral)

sampling_dist |>
  visualise() +
  shade_confidence_interval(ci)

