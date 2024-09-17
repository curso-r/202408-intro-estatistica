
# teste de hipotese sai de graça dos graficos de intervalo de conf --------

# tenho por hipotese que 30% das mulheres deveriam ter respondido
# "sim" no ultimo grafico

dados |>
  filter(
    sexo == "Feminino"
  ) |>
  prop_test(p32c ~ NULL, p = 0.3, success = "Sim")

dados_brutos <- dados |>
  filter(sexo == "Feminino") |>
  specify(response = p32c, success = "Sim")

sampling_dist <- dados |>
  filter(sexo == "Feminino") |>
  specify(response = p32c, success = "Sim") |>
  assume("z")

estatistica_do_teste <- dados_brutos |>
  hypothesise(null = "point", p = 0.30) |>
  calculate(stat = "z")

sampling_dist |>
  visualise() +
  shade_p_value(estatistica_do_teste, direction = "left")
