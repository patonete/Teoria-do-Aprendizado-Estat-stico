df <- read.csv("dados.csv")

print('As medidas foram feitas usando os dados da colune referente a lesao corporal dolosa')

#Media
print('Media')
mean(df$`lesao.corporal.dolosa`, na.rm = TRUE)

#Mediana
print('Mediana')
median(df$`lesao.corporal.dolosa`, na.rm = TRUE)

#Moda
print('Moda')
temp_tab <- table(df$`lesao.corporal.dolosa`)
names(temp_tab)[which.max(temp_tab)]

# Histograma simples
print('Histograma')
hist(df$`lesao.corporal.dolosa`,
     main = "Histograma de Lesões corporais dolosas",
     xlab = "Ocorrências",
     ylab = "Frequência",
     col = "lightgreen",
     breaks = 10)

#Media aparada
print('Media aparada (25 e 50%)')
mean(df$`lesao.corporal.dolosa`, na.rm = TRUE, trim = 0.25)

mean(df$`lesao.corporal.dolosa`, na.rm = TRUE, trim = 0.5)

#Media geometrica
print('Media geométrica')
exp(mean(log(df$`lesao.corporal.dolosa`)))

# Desvios em relacao a media
print('Desvios em relacao a media')
df$`lesao.corporal.dolosa` - mean(df$`lesao.corporal.dolosa`)

# Desvios em relacao a mediana
print('Desvios em relacao a mediana')
df$`lesao.corporal.dolosa` - median(df$`lesao.corporal.dolosa`)

# Desvio padrao
print('Desvio padrao')
sd(df$`lesao.corporal.dolosa`)

# Variancia
print('Variancia')
var(df$`lesao.corporal.dolosa`)

# Desvio absoluto medio
print('Desvio absoluto medio')
mean(abs(df$`lesao.corporal.dolosa` - mean(df$`lesao.corporal.dolosa`)))

# MAD
print('MAD')
mad(df$`lesao.corporal.dolosa`)

# Amplitude
print('Amplitude')
diff(range(df$`lesao.corporal.dolosa`))

# Percentil
print('Percentil 25')
quantile(df$`lesao.corporal.dolosa`, probs = 0.25)

print('Percentil 90')
quantile(df$`lesao corporal dolosa`, probs = 0.9)

# Amplitude Interquartilica
print('Amplitude Interquartilica')
IQR(df$`lesao.corporal.dolosa`)

dados <- df$`lesao.corporal.dolosa`
# 50 valores normais em torno de 10, mais dois outliers: 25 e 30
# Boxplot
boxplot(dados,
        main = "Boxplot Lesao corporal dolosa",
        ylab = "Valores",
        col = "lightblue",
        border = "darkblue",
        notch = F)

# Tabela de frequencia
print('Tabela de frequencia')
prop.table(table(df$`lesao.corporal.dolosa`))

# Tabela de frequencia
print('Tabela de frequencia %')
round(prop.table(table(df$`lesao.corporal.dolosa`)) * 100, 2)

# Estimativa densidade
print('Estimativa densidade')

df$`lesao corporal dolosa` <- rnorm(120)
plot(density(df$`lesao.corporal.dolosa`))

# Distribuicao normal
library(fitdistrplus)
teste <- fitdist(df$`total.de.roubo...outros`, "norm")
plot(teste)
teste

# Teste de correlação (pega as 10 maiores correlações da tabela)
# 1. Filtrar apenas colunas numéricas
df_num <- df[, sapply(df, is.numeric)]

# 2. Calcular a matriz de correlação
matriz_cor <- cor(df_num, use = "pairwise.complete.obs")

# 3. Remover a diagonal e a parte repetida (espelhada)
matriz_cor[lower.tri(matriz_cor, diag = TRUE)] <- NA

# 4. Transformar em formato de lista (long format)
tabela_cor <- as.data.frame(as.table(matriz_cor))

# 5. Limpar os NAs e renomear
tabela_cor <- na.omit(tabela_cor)
colnames(tabela_cor) <- c("Variavel_1", "Variavel_2", "Correlacao")

# 6. Obter as 10 maiores correlações (em valor absoluto)
top_10 <- head(tabela_cor[order(abs(tabela_cor$Correlacao), decreasing = TRUE), ], 10)

# Resultado
print(top_10)

#Relacao entre duas colunas com alta correlacao (Lesao corporal dolosa e furtos, no caso)
plot(df$lesao.corporal.dolosa, df$furto...outros,
main = "Relacao entre lesao corporal dolosa e furtos",
xlab = "Lesao corporal dolosa", ylab = "Furtos",
pch = 14, # alterar formato
col = "blue", # cor dos pontos
cex = 1.3) # tamanho dos pontos


#tapply + Distribuicao de roubos por cidades com boxplot
summary_roubos_cidades <-tapply(df$roubo...outros,df$cidade,summary)
summary_roubos_cidades

boxplot(df$roubo...outros ~ df$cidade,data = df,
col="darkblue", #cordepreenchimento
border="grey", #cordasbordas
main="Distribuição de roubos por cidade",
xlab="Cidades",
ylab="N de Roubos")


#alpha e beta - Função lm

# Garante que os dados estão em ordem cronológica
df <- df[order(df$Data), ]

# Cria o índice numérico (1 para o janeiro, 2 para fevereiro, etc.)
df$mes_indice <- 1:nrow(df)

y <- df$furto...outros
x <- df$mes_indice
  
fun_lm <- lm( 
  formula = y ~ x, 
  data = df, 
  subset = x >= 50, 
  weights = 1/x, 
  na.action = na.omit
  )

fun_lm

plot(x, y, main = "Tabela teste", pch = 19, col = "darkgray", xlab = "x", ylab = "y")
