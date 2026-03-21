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