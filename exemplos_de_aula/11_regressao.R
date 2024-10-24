### Qual é o cenário "atual" que a gente analisou

# tenho uma base dados qualquer:

dados <- readxl::read_excel("exemplos_de_aula//BD_CIS0684.xlsx")

# e tenho colunas:

dados$idade1
hist(dados$idade1)

mean(dados$idade1)
sd(dados$idade1)

# ideia estatística

amostras <- 2000

media_amostral_possivel = numeric(amostras)
for(amostra in 1:amostras){
  idade1_possivel = round(runif(n = 1000)*80)

  media_amostral_possivel[amostra] = mean(idade1_possivel)
}
hist(media_amostral_possivel, breaks = 100)

### qual é o cenário de hoje?

plot(mtcars$wt, mtcars$mpg)

# primeira pergunta: como os dados foram gerados?

# HIPÓTESE da regressão é que os dados foram gerados da seguinte maneira:

# FIXADO O PESO WT:

X = sample(mtcars$wt, replace = TRUE, size = 30)
beta = -15
alfa = 60
Y = alfa+beta*X + rnorm(30, sd = 10)
# Y = 60 - 15 * X + erro(media = 0, desv_pad = 10)

plot(X,Y)
plot(mtcars$wt, mtcars$mpg)

modelo <- lm(Y ~ X)
summary(modelo)

#####

modelo_consumo_wt <- lm(mpg ~ wt, data = mtcars)
summary(modelo_consumo_wt)

# ele produziu estimativa para b0, mas eu nao acho que faça sentido

modelo_consumo_wt <- lm(mpg ~ wt-1, data = mtcars)
summary(modelo_consumo_wt)
# de fato ficou até com menos sentido, o beta do wt deveria ser sempre negativo...

# vamos voltar para o outro:

modelo_consumo_wt <- lm(mpg ~ wt, data = mtcars)
summary(modelo_consumo_wt)

#### teste 1: por hipótese y_i - alfa-beta*x deveria ter distribuicao normal
### (teorica). Se eu fizesse um histograma deveria passar no teste

hist(residuals(modelo_consumo_wt))

### teste 2: os residuos nao deveriam ter nada a ver com o x

plot(mtcars$wt, residuals(modelo_consumo_wt))

### teste 3: nenhum resíduo deveria ser muuuuito maior que o outro
# estamos esperando que todos os residuos estejam a mais ou menos 3 desvio padrao
# do 0

max(abs(residuals(modelo_consumo_wt))/sd(residuals(modelo_consumo_wt)))

# faz essa conta ^ se der muito maior do que 3, tem algo errado
# (outliers!)

### o R ajuda a "resumir" essa decisão/discussão nos plots básicos

plot(modelo_consumo_wt)

### refazendo o modelo sem o Chrysler Imperial

library(tidyverse)

mtcars_novo <- mtcars |>
  tibble::rownames_to_column()

modelo_sem_chrysler <- lm(
  mpg ~ wt,
  data = mtcars_novo |>
    filter(
      !(rowname %in% c("Chrysler Imperial"))
    )
)

plot(modelo_sem_chrysler)

summary(modelo_sem_chrysler)

# é normal que o resultado de um modelo de regressão seja identico a
# um teste t/teste qui quadrado:

library(tidyverse)
library(infer)

dados <- readxl::read_excel("exemplos_de_aula//BD_CIS0684.xlsx")

dados |>
  filter(
    sexo == "Feminino"
  ) |>
  t_test(idade1 ~ NULL, mu = 0)

lm(idade1 ~ 1, data= dados |> filter(sexo == "Feminino")) |> summary()

# por que deu igual?

# meu modelo de regressao era
# idade1_observado_media = media(intercepto) + erro(0, desvio_padrao(populacional))
# idade1_observado_media = normal(media(populacional), desvio_padrao(populacional))

# meu modelo de amostragem era:
# idade1_observado_media = x_barra de uma distribuição populacional que eu nao qual é
# mas pelo CLT:
# idade1_observado_media = normal(media(populacional), desvio_padrao(populacional))

lm(mpg ~ wt + disp, data = mtcars) |>
  plot()

modelo_consumo_wt |>
  summary()
