# Elaborar o intervalo de confiança para a média (Z) para a variável “sales”;

ceo <- read.table("/cloud/project/ceo.txt", header = TRUE, sep = "", na.strings = "NA", dec = ".", strip.white = TRUE)
# Salvar conjunto de dados no formato R

save ("ceo", file = "/cloud/project/ceo")


# Carregar conjunto de dados salvo no formato do R

load("/cloud/project/ceo")

library(carData)

library(datasets)

#install.packages("BSDA")

library(BSDA)

sd <- sd(ceo$sales)

sd

x <- ceo$sales

x

z.test(x, y = NULL, alternative = "two.sided", mu = 0, sigma.x = sd,
       sigma.y = NULL, conf.level = 0.95)
# -----------------------------------------------------------------------------
# Elaborar o intervalo de confiança para a média (t) para a variável “profits”;

install.packages("stats")
View(ceo)
names (ceo)

library(stats)

profits <- ceo$profits

t.test(profits, y = NULL,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, data=ceo)
# -----------------------------------------------------------------------------
# Elaborar o intervalo de confiança para a variância para a variável “salary”;
var <- var(ceo$salary)

n <- sum (ceo$n)

#obtendo os valores de chi quadrado

chiinf <- qchisq(0.025, df=5633)
chisup <- qchisq(0.975, df=5633)

superior <- ((n-1)*var)/chiinf
inferior <- ((n-1)*var)/chisup

inferior
superior

# -----------------------------------------------------------------------------
# Fazer o teste para a diferença entre duas médias (Z) para as variáveis “sales”e “salary”;
library(BSDA)

# calculando os desvios padr?o de x e y

sdx <- sd(ceo$sales)

sdy <- sd(ceo$salary)

# Criando os objetos com as vari?veis em an?lise
sales <- ceo$sales

salary <- ceo$salary

z.test(sales, salary, alternative = "two.sided", mu = 0, sigma.x = sdx,
       sigma.y = sdy, conf.level = 0.95)
# -----------------------------------------------------------------------------
# Fazer o teste para a diferença entre duas médias (t) para as variáveis “sales”e “profits”;
library(stats)

sales <- ceo$sales

profits <- ceo$profits

t.test(sales, profits,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)
# -----------------------------------------------------------------------------
# Fazer o teste para a diferença entre variâncias (F) para as variáveis “sales”e “mktval”;
library(stats)

sales <- ceo$sales

mktval <- ceo$mktval

var.test(sales, mktval, alternative = "two.sided", conf.level = 0.95)
# -----------------------------------------------------------------------------
# Fazer o teste de normalidade de Kolmogorov-Smirnov para as variáveis “sales”e “salary”;
library(RcmdrMisc)

sales <- ceo$sales

salary <- ceo$salary

normalityTest(~sales, test="lillie.test", data=ceo)

normalityTest(~salary, test="lillie.test", data=ceo)
# -----------------------------------------------------------------------------
# Fazer o teste de independência/equivalência ou igualdade entre amostras(t)para as variáveis “salary”e “sales”;
library(ggpubr)

sales <- ceo$sales

salary <- ceo$salary

t.test(sales, salary, alternative = "two.sided", var.equal = TRUE)

# -----------------------------------------------------------------------------
# Fazer o teste de Wilcoxon-Mann-Whitney para amostras independentes para as variáveis “salary”e “sales”;
library(stats)

sales <- ceo$sales

salary <- ceo$salary

wilcox.test(sales, salary, alternative = "two.sided") 
# -----------------------------------------------------------------------------
# Elaborar a matriz de correlação parcial para as seguintes variáveis:husearns; earns; huseduc; educ; husexp; exper.
salarios <- read.table("/cloud/project/cps91.txt", header = TRUE, sep = "", na.strings = "NA", dec = ".", strip.white = TRUE)

cor(salarios[,c("earns","huseduc","educ", "husexp", "exper")], use="complete")

# -----------------------------------------------------------------------------
# Estimar um modelo preliminar por MQO a partir do seguinte modelo:𝑓𝑎𝑚𝑖𝑛𝑐=𝛽0+𝛽1ℎ𝑢𝑠𝑎𝑔𝑒+𝛽2ℎ𝑢𝑠𝑒𝑎𝑟𝑛𝑠+𝛽3ℎ𝑢𝑠𝑒𝑑𝑢𝑐+𝛽4ℎ𝑢𝑠𝑏𝑙𝑐𝑘+𝛽5ℎ𝑢𝑠ℎ𝑖𝑠𝑝+𝛽6ℎ𝑢𝑠ℎ𝑟𝑠+𝛽7𝑒𝑎𝑟𝑛𝑠+𝛽8𝑎𝑔𝑒+𝛽9ℎ𝑖𝑠𝑝𝑎𝑛𝑖𝑐+𝛽10𝑏𝑙𝑎𝑐𝑘+𝛽11𝑒𝑑𝑢𝑐+𝛽12ℎ𝑢𝑠𝑒𝑥𝑝+𝛽12𝑒𝑥𝑝𝑒𝑟+𝛽13ℎ𝑜𝑢𝑟𝑠+𝛽14𝑖𝑛𝑙𝑓+
View(salarios)
names(salarios)
#número de eventos
salarios$ObsNumber <- 1:5634

save ("salarios", file = "/cloud/project/salarios")


# Estimando um modelo preliminar

resultados <- lm (faminc~husage+husearns+huseduc+husblck+hushisp+hushrs+earns+hispanic+black+educ+husexp+hours+inlf, data=salarios)
summary (resultados)
# -----------------------------------------------------------------------------
# Fazer os testes de “t”para os coeficientes (parâmetros calculados) no modelo estimado acima e interpretá-los.



