# Dados -------------------------------------------------------------------

dados_rolagens <- readRDS("dados/dados_rolagens.rds")

# Fazendo contagens -------------------------------------------------------

X <- dados_rolagens$resultado

table(X)
# é o jeito de contar do R básico

contagens <- dados_rolagens |>
  count(resultado)

# histograma
dados_rolagens |>
  ggplot(aes(x = resultado)) +
  geom_bar()

# média
mean(dados_rolagens$resultado)

#amplitude
max(dados_rolagens$resultado)-min(dados_rolagens$resultado)

#desvio absoluto medio
mean(abs(dados_rolagens$resultado-3.51))

# simulacoes --------------------------------------------------------------

NN <- 100

infinitos_dados <- sample(
  1:6,
  size = NN,
  replace = TRUE,
  prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))

hist(infinitos_dados)

mean(infinitos_dados)

max(infinitos_dados)-min(infinitos_dados)

mean(abs(infinitos_dados - mean(infinitos_dados)))

# dois dados --------------------------------------------------------------

BB <- 100000

dado1 <- sample(
  1:6,
  size = BB,
  replace = TRUE,
  prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))

dado2 <- sample(
  1:6,
  size = BB,
  replace = TRUE,
  prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))

infinitas_somas <- dado1+dado2

hist(infinitas_somas)

mean(infinitas_somas)

max(infinitas_somas)-min(infinitas_somas)

mean(abs(infinitas_somas - mean(infinitas_somas)))


# dados forjados ----------------------------------------------------------

BB <- 10000000

dado1_forjado <- sample(
  1:6,
  size = BB,
  replace = TRUE,
  prob = c(1/6, 1/6, 1/6, 1/6, 1/6-1/7, 1/6+1/7))

dado2_forjado <- sample(
  1:6,
  size = BB,
  replace = TRUE,
  prob = c(1/6, 1/6, 1/6, 1/6, 1/6-1/10, 1/6+1/10))

infinitas_somas_forjada <- dado1_forjado+dado2_forjado

hist(infinitas_somas_forjada)

# distribuicao normal -----------------------------------------------------

dados_temperatura <- readRDS("dados/chuvas_A701.rds")

dados_temperatura |>
  ggplot(aes(x = `Tair_mean (c)`)) +
  geom_histogram(
    bins = 7,
    fill = "royalblue",
    color = "white"
  )

media_temperatura <- mean(dados_temperatura$`Tair_mean (c)`)
desv_pad_temperatura <- sd(dados_temperatura$`Tair_mean (c)`)

media_temperatura
desv_pad_temperatura

# o que é esperado de um histograma de variavel quantitativa conti --------

NN <- 8000000

amostra_infinita_de_temperatura <- rnorm(
  NN,
  media_temperatura,
  desv_pad_temperatura)

hist(amostra_infinita_de_temperatura, breaks = "FD")

mean(amostra_infinita_de_temperatura)
sd(amostra_infinita_de_temperatura)

# comparando o observado com uma normal teorica razoavel ------------------

hist(dados_temperatura$`Tair_mean (c)`, freq = FALSE)

dados_ordenados <- sort(dados_temperatura$`Tair_mean (c)`)

# codigo que vai plotar a normal:
lines(
  dados_ordenados,
  dnorm(dados_ordenados, media_temperatura, desv_pad_temperatura),
  col = 'red')

