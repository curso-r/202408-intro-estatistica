# Como construi a base ----------------------------------------------------

# dado_vermelho_x10 <- sample(1:6, size = 10, replace = TRUE)
#
# dado_branco_x10  <- sample(1:6, size = 10, replace = TRUE)
#
# amostra_soma <- dado_vermelho_x10 + dado_branco_x10

# isso é um exemplo ^

# Respondendo as perguntas ------------------------------------------------

minha_amostra <- readxl::read_excel("dados/dados_do_meu_amigo.xlsx")

simulacoes <- readxl::read_excel(
  "dados/simulacao_1000_amostras_tamanho_10.xlsx",
  sheet = "soma_dos_dados")

# quantos % das amostras não vejo 6?

simulacoes |>
  mutate(
    nenhum_6 = (lancamento_1 != 6) &
      (lancamento_2 != 6) &
      (lancamento_3 != 6) &
      (lancamento_4 != 6) &
      (lancamento_5 != 6) &
      (lancamento_6 != 6) &
      (lancamento_7 != 6) &
      (lancamento_8 != 6) &
      (lancamento_9 != 6) &
      (lancamento_10 != 6)
  ) |>
  count(nenhum_6)

238/(238+762)
# 23%

# ou seja, é comum ver pelo menos 1 número 6


# media -------------------------------------------------------------------

mean(unlist(minha_amostra[1,1:10]))

simulacoes |>
  mutate(
    media_amostral = (lancamento_1 + lancamento_2 + lancamento_3 + lancamento_4 +
      lancamento_5 + lancamento_6 + lancamento_7 + lancamento_8 + lancamento_9 + lancamento_10)/10
  ) |>
  ggplot(aes(x = media_amostral)) +
  geom_histogram(bins = 12) +
  geom_vline(xintercept = 5.4, color = "red", size = 2, linetype = 2)


# amplitude ---------------------------------------------------------------

amplitude_casa_do_meu_amigo <-
minha_amostra |>
  with(
    pmax(lancamento_1,lancamento_2,lancamento_3,lancamento_4,
         lancamento_5,lancamento_6,lancamento_7,lancamento_8,lancamento_9,lancamento_10)
  )

simulacoes |>
  mutate(
    amplitude_amostral = pmax(lancamento_1,lancamento_2,lancamento_3,lancamento_4,
                        lancamento_5,lancamento_6,lancamento_7,lancamento_8,lancamento_9,lancamento_10)
  ) |>
  ggplot(aes(x = amplitude_amostral)) +
  geom_histogram(bins = 12) +
  geom_vline(xintercept = amplitude_casa_do_meu_amigo, color = "red", size = 2, linetype = 2)

# desvio absoluto

MEDIA_lancamentos <- mean(unlist(minha_amostra[1,1:10]))

t(minha_amostra[, -11]) |>
  as_tibble() |>
  set_names("lancamento") |>
  mutate(
    MEDIA_LANCAMENTOS = MEDIA_lancamentos,
    DESVIO = lancamento-MEDIA_lancamentos,
    DESVIO_ABSOLUTO = abs(DESVIO)
  ) |>
  summarise(
    DESVIO_ABSOLUTO_MEDIO = mean(DESVIO_ABSOLUTO)
  )

simulacoes |>
  mutate(
    media_amostral = (lancamento_1 + lancamento_2 + lancamento_3 + lancamento_4 +
                        lancamento_5 + lancamento_6 + lancamento_7 + lancamento_8 + lancamento_9 + lancamento_10)/10,
    desvio_absoluto_medio = (abs(lancamento_1-media_amostral) +
      abs(lancamento_2-media_amostral) +
      abs(lancamento_3-media_amostral) +
      abs(lancamento_4-media_amostral) +
      abs(lancamento_5-media_amostral) +
      abs(lancamento_6-media_amostral) +
      abs(lancamento_7-media_amostral) +
      abs(lancamento_8-media_amostral) +
      abs(lancamento_9-media_amostral) +
      abs(lancamento_10-media_amostral))/10
  ) |>
  ggplot(aes(x = desvio_absoluto_medio)) +
  geom_histogram(bins = 11) +
  geom_vline(xintercept = 2.08, color = "red", size = 2, linetype = 2)

# teste que deu que o dado do meu amigo está como esperado
# pela distribuição amostral


# organizando a conclusao -------------------------------------------------

# fizemos tres comparacoes:

# 1)

# media observada na casa do meu amigo

5.4

# vs

# distribuicao amostral da média em amostras de tamanho 10

# conclusão: 5.4 está muito distante do 7, que seria esperado, e
# também é um dos menores valores que ainda é possível observar numa amostra
# de tamanho 10

# 2)

# amplitude observada na casa do meu amigo

9

# vs

# distribuição amostral da amplitude em amostras de tamanho 10

# conclusão: a amplitude 9 é uma das menos provaveis, que seriam 10, 11 ou 12.

# mas ainda é mais provavel do que 8 ou 7 não é muito improvavelm dar 9, mas é um pouco

# 3)

# desvio absoluto medio observado na casa do meu amigo

2.08

# vs

# distribuição amostral do desvio absoluto médio em amostras de tamanho 10

# conclusão: 2.08 está muito próximo do numero 2, que é a tendencia central
# de todas as amostras. a maior parte das amostra tem desvio absoluto médio
# perdo de 2.

# conclusão final: dois testes indicaram que os resultados do meu amigo (do lançamento)
# estão abaixo das tendencias das amostras simuladas. o teste que deu compatibilidade
# entre a amostra e a distribuição amostral conclui que a variabilidade ao redor da média
# aparentemente é a mesma, mas nao a média.

# VEREDITO: teria que ser "passado" nos tres testes pra dizer que os dados do meu amigo
# sao honestos. os dois primeiros testes nos fazem concluir que tem ALGO diferente

# pontos de reforco: a distribuicao amostral é "teorica" ------------------

# logo, o histograma "esperado", "teorico" deveria ser uma curva
# intuicao 1: se eu aumentar as simulacoes de 1000 pra 10000 ou 1000000

# faz 1 amostra de tamanho 10

dado_vermelho_x10 <- sample(1:6, size = 10, replace = TRUE)

dado_branco_x10  <- sample(1:6, size = 10, replace = TRUE)

amostra_soma <- dado_vermelho_x10 + dado_branco_x10

# faz BB:

BB <- 1000000

amostra_medias <- numeric(length = BB)

for(repeticao in 1:BB){

  print(repeticao)
  dado_vermelho_x10 <- sample(1:6, size = 10, replace = TRUE)

  dado_branco_x10  <- sample(1:6, size = 10, replace = TRUE)

  amostra_soma <- dado_vermelho_x10 + dado_branco_x10

  amostra_medias[repeticao] <- sum(amostra_soma)/10
}

amostra_medias

hist(amostra_medias, breaks = 100)
# desenho super preciso da distribuição amostral
# a info é a mesma... a media amostral de 10 lancamentos
# provavelmente fica entre 5 e 9, bem perto de 7. se sair disso
# tem algo errado

# intuicao 2: o tamanho da amostra que eu efetivamente observei, 10,
# deve influenciar o desenho da distribuicao amostral. a "sombra"


BB <- 100000

amostra_medias <- numeric(length = BB)

for(repeticao in 1:BB){

  print(repeticao)
  dado_vermelho_x10 <- sample(1:6, size = 3, replace = TRUE)

  dado_branco_x10  <- sample(1:6, size = 3, replace = TRUE)

  amostra_soma <- dado_vermelho_x10 + dado_branco_x10

  amostra_medias[repeticao] <- sum(amostra_soma)/3
}

#amostra_medias

hist(amostra_medias, breaks = 100)

# 3a intuicao. o histograma teorico da distribuição normal (ou gaussiana)
# aparentemente está aparecendo aqui

hist(rnorm(100000), breaks = 100)


