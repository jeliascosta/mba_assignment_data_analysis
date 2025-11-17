################################################################################
# TRABALHO FINAL
################################################################################

# ------------------------------------------------------------------------------
# 0 - Carregando pacotes
# ------------------------------------------------------------------------------
# Pacotes utilizados para leitura, manipulação, visualização e testes estatísticos

library(readxl)     # Leitura de arquivos Excel (.xlsx)
library(dplyr)      # Manipulação e transformação de dados
library(ggplot2)    # Visualização de dados
library(ggpubr)     # Gráficos estatísticos (ex: QQ-plots)
library(patchwork)  # Combinação de gráficos
library(naniar)     # Visualização de dados faltantes
library(visdat)     # Visualização estrutural da base
library(gtsummary)  # Tabelas descritivas formatadas
library(epiR)       # Intervalos de confiança e análises epidemiológicas
library(DescTools)  # Testes estatísticos adicionais
library(vcd)        # Associação entre variáveis categóricas
library(corrplot)   # Visualização de matrizes de correlação
library(PerformanceAnalytics) # Gráficos de correlação e dispersão

# ------------------------------------------------------------------------------
# 1 - Importando e tratando a base de dados
# ------------------------------------------------------------------------------
# Pergunta orientadora: "Como estão organizadas e codificadas as variáveis da base?"

# Leitura da base
base <- read_excel(path = "Base_trabalho.xlsx")

# Convertendo variáveis categóricas para fatores
base <- base |> 
  mutate(
    sexo = factor(x = sexo,
                  levels = c(0, 1), 
                  labels = c("Feminino", "Masculino")),
    filhos = factor(x = filhos, 
                    levels = c(0, 1), 
                    labels = c("Não", "Sim")),
    escolaridade = factor(x = escolaridade, 
                          levels = c(1, 2, 3), 
                          labels = c("Fundamental", "Médio", "Superior")),
    casado = factor(x = casado, 
                    levels = c(0, 1), 
                    labels = c("Não", "Sim")),
    reincidente = factor(x = reincidente, 
                         levels = c(0, 1), 
                         labels = c("Não", "Sim"))
  )

# Verificando medidas da base, que apresentam valores coerentes
summary(base)

# ------------------------------------------------------------------------------
# 2 - Analisando dados faltantes
# ------------------------------------------------------------------------------
# Pergunta orientadora: "Existem variáveis com proporção relevante de valores ausentes?"

# Visualização gráfica dos dados faltantes, mostrando 100% de presença
vis_miss(x = base)

# Visualização gráfica dos dados faltantes, mostrando 0% de ausência
gg_miss_var(x = base,
            show_pct = TRUE)
# ------------------------------------------------------------------------------
# QUESTÃO 01 - Podemos afirmar que o score médio de periculosidade é superior a 170 pontos?
# ------------------------------------------------------------------------------

# Assumindo nível de significância de 5%

# Histograma do score de periculosidade com curva de densidade Normal sobreposta
media <- mean(x = base$score_periculosidade, na.rm = TRUE)
desvio <- sd(x = base$score_periculosidade, na.rm = TRUE)

hist_score <- ggplot(data = base, 
                     mapping = aes(x = base$score_periculosidade)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 10) +
  stat_function(fun = dnorm, 
                args = list(mean = media, 
                            sd = desvio), 
                colour = "red") +
  labs(y = "Densidade", x = "Score Periculosidade")

# A visualização do histograma sugere uma distribuição ~normal
hist_score

# QQ-plot para avaliação visual da normalidade, mostrando praticamente todos os
# pontos na faixa que indica tendência de normalidade
ggqqplot(data = base$score_periculosidade)

# Visualização da distribuição da score de periculosidade, indicando possível
# distribuição ~normal, dada a simetria em relação a mediana
ggplot(data = base, 
       mapping = aes(y = score_periculosidade)) +
  geom_boxplot() +
  labs(y = "Score de periculosidade", x = "") +
  theme_minimal()

# Estimativa pontual igual a 174.5205, reforçando a possibilidade da média populacional
# ser maior que 170
media

# Teste de Shapiro-Wilk para normalidade
# H0: O score de periculosidade segue uma distribuição aproximadamente normal
# H1: O score de periculosidade não segue uma distribuição aproximadamente normal
shapiro.test(x = base$score_periculosidade)

# Conclusão:
# Como p-valor 0.7738 > 0.05 nível de significância não rejeitamos H0, logo assumimos
# que os scores de periculosidade possuem distribuição aproximadamente normal

# Teste t para uma média, no caso, 170
# H0: μ = 170
# H1: μ > 170
t.test(x = base$score_periculosidade, 
       mu = 170, 
       alternative = "greater")

# Conclusão:
# p-valor = 0.006845 < 0.05 → Rejeitamos H0

# O score de periculosidade médio da população é superior a 170

# Tendo em vista a conclusão acima, calculamos o intervalo de confiança de 95%
epi.conf(dat = base$score_periculosidade, 
         ctype = "mean.single", 
         conf.level = 0.95)

# Conclusão:
# O intervalo de confiança de 95% é [170.9367 178.1044]


# -----------------------------------------------------------------------------------------
# QUESTÃO 02 - O tempo médio de prisão entre homens e mulheres é igual?
# -----------------------------------------------------------------------------------------

# Separação dos grupos
baseH <- base |> filter(sexo == "Masculino")
baseM <- base |> filter(sexo == "Feminino")

# Teste de normalidade por grupo

#Graficamente
qq1 = ggqqplot(baseH$tempo_preso)
qq2 = ggqqplot(baseM$tempo_preso)
qq1 + qq2

#Teste de normalidade para a população 1 (nível de significância de 5%)
#H0: O tempo médio de prisão de homens segue uma distribuição aproximadamente normal
#H1: O tempo médio de prisão de homens não segue uma distribuição aproximadamente normal
shapiro.test(x = baseH$tempo_preso)

#Conclusão:
#p-valor = 0.8863 > 0,05 (nivel de sig) não rejeita H0 -> Tempo médio de prisão de homens tem distribuição normal

#Teste de normalidade para a população 2 (nível de significância de 5%)
#H0: O tempo médio de prisão de mulheres segue uma distribuição aproximadamente normal
#H1: O tempo médio de prisão de mulheres não segue uma distribuição aproximadamente normal
shapiro.test(x = baseM$tempo_preso)

#Conclusão:
#p-valor = 0.09781 > 0,05 (nivel de sig) não rejeita H0 -> O tempo médio de prisão de mulheres tem distribuição normal

#Analise descritiva
base |> 
  group_by(sexo) |> 
  summarise(n = n(),
            media = mean(x = tempo_preso, na.rm = TRUE),
            desvio = sd(x = tempo_preso, na.rm = TRUE))

ggplot(data = base,
       mapping = aes(x = sexo,
                     y = tempo_preso)) +
  geom_boxplot() +
  theme_minimal()

# Teste de igualdade das variâncias
# H0: σ²_H = σ²_M
# H1: σ²_H ≠ σ²_M
VarTest(x = baseH$tempo_preso, 
        y = baseM$tempo_preso, 
        alternative = "two.sided", 
        ratio = 1, 
        conf.level = 0.95)

#VarTest - Realiza teste de comparação de variâncias entre duas populações (teste F).
## x - vetor numérico da primeira amostra.
## y - vetor numérico da segunda amostra.
## alternative - define a hipótese alternativa: "two.sided" (variâncias diferentes),
##               "less" (variância de x < variância de y),
##               "greater" (variância de x > variância de y).
## ratio - valor da razão de variâncias sob H0 (default = 1).
## conf.level - nível de confiança do intervalo (ex: 0.95 para 95%).

# Conclusão:
# p-valor = 0.7134 > 0,05 → Não há evidência suficiente para rejeitar H0. Logo, aceita a hipótese de que as variâncias são iguais

# Teste t para médias independentes
# H0: μ_H = μ_M
# H1: μ_H ≠ μ_M
t.test(x = baseH$tempo_preso, 
       y = baseM$tempo_preso,  
       var.equal = TRUE, 
       alternative = "two.sided")

#t.test - Realiza o teste t de Student para comparação de médias entre duas amostras independentes.
## x - vetor numérico da primeira amostra.
## y - vetor numérico da segunda amostra.
## var.equal - indica se as variâncias das duas amostras devem ser consideradas iguais (TRUE) ou não (FALSE).
## alternative - define a hipótese alternativa:
##               "two.sided" (diferença em ambos os lados),
##               "less" (x < y),
##               "greater" (x > y).

# Conclusão:
# p-valor = 0.2072 > 0,05 → Aceitamos H0.
# O tempo médio de prisão entre homens e mulheres pode ser considerado igual.

# -----------------------------------------------------------------------------------------
# QUESTÃO 03 - Existe correlação entre o tempo de prisão e o score de periculosidade
# -----------------------------------------------------------------------------------------

# geom_point(): cria gráfico de dispersão entre duas variáveis numéricas
base |>
  ggplot(mapping = aes(x = tempo_preso,
                       y = score_periculosidade)) +
  geom_point() +
  labs(x = "Tempo de prisão",
       y = "Score de Periculosidade")

# Teste de normalidade por grupo

#Graficamente
qq1 = ggqqplot(base$tempo_preso)
qq2 = ggqqplot(base$score_periculosidade)
qq1 + qq2

shapiro.test(x = base$tempo_preso)
shapiro.test(x = base$score_periculosidade)

# Correlação de Pearson
# H0: ρ = 0  (Não existe correlação linear positiva entre tempo de prisão e score de periculosidade)
# H1: ρ > 0  (Existe correlação linear positiva entre tempo de prisão e score de periculosidade)
cor.test(x = base$tempo_preso, 
         y = base$score_periculosidade,
         alternative = "greater")

#cor.test - Realiza o teste de correlação entre duas variáveis contínuas.
## x - vetor numérico da primeira variável.
## y - vetor numérico da segunda variável.
## alternative - define a hipótese alternativa:
##               "two.sided" (correlação diferente de 0),
##               "less" (correlação negativa),
##               "greater" (correlação positiva).

# Conclusão:
# p-valor = 2.2e-16 < 0,05 → Rejeitamos H0.
# Existe correlação linear positiva entre tempo de prisão e score de periculosidade.
# cor: coeficiente de correlação entre as variáveis: 0.8037558


# -----------------------------------------------------------------------------------------
# QUESTÃO 04 - Há relação entre o sexo e a reincidência dos indivíduos?
# -----------------------------------------------------------------------------------------

# Assumindo nível de significância de 5%

# Tabela: Sexo e reincidência (frequências + percentuais por coluna)
tabela <- base |> 
  select(sexo, reincidente) |> 
  tbl_summary(
    by = sexo,
    statistic = all_categorical() ~ "{n} ({p}%)",
    percent = "column", #row ou cell ou column
    label = list(reincidente ~ "Reincidente?")
  ) |> 
  modify_header(label ~ "**Variável**") |> 
  bold_labels()

# Visualizando a tabela, observamos percentuais próximos de reincidentes e não
# reincidentes ao variar o sexo, sugerindo independência entre as variáveis
tabela

# Gráfico de barras empilhadas com proporções mostra percentuais próximos de reincidentes e não
# reincidentes ao variar o sexo, sugerindo independência entre as variáveis
base |> 
  ggplot(mapping = aes(x =sexo,
                       fill = reincidente)) +
  geom_bar(position = "fill") +
  labs(x = "Sexo",
       y = "Porcentagem",
       fill = "Reincidente") +
  scale_y_continuous(label = scales::percent) +
  theme_classic()

# Tabela de contingência
tab <- table(base$sexo, base$reincidente)

# V de Cramer = 0.034, um valor próximo de zero, sugerindo independência 
# entre sexo e reincidência
assocstats(tab)

# Teste qui-quadrado, tendo em vista que são duas variáveis qualitativas
# H0: Sexo e reincidência são independentes
# H1: Há associação entre sexo e reincidência
chisq.test(x = tab, 
           correct = FALSE)

# Conclusão:
# p-valor = 0.6345 > 0.05 → Não rejeitamos H0
# Sexo e reincidência são independentes

# -----------------------------------------------------------------------------------------
# QUESTÃO 05 - Ajuste um modelo de regressão para o score de periculosidade,
#considerando todas as variáveis explicativas disponíveis. Avalie as suposições
#de normalidade e homoscedasticidade dos resíduos e interprete os coeficientes
#estimados. Qual o percentual de explicação do modelo? Utilize um nível de
#significância de 5%.
# -----------------------------------------------------------------------------------------
# Pergunta orientadora: "Quais fatores explicam o score de periculosidade?"

# Seleciona apenas variáveis numéricas
base_numerica <- base |> 
  select(where(is.numeric))

## Calcula coeficientes de correlação com o método pearson
# entre variáveis numéricas, ignorando casos incompletos
matriz_cor <- cor(base_numerica,
                  use = "complete.obs", 
                  method = "pearson")

# Visualização da matriz de correlação
corrplot(corr = matriz_cor, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45)

# Correlograma com gráficos de dispersão
chart.Correlation(R = base_numerica, 
                  histogram = TRUE, 
                  pch = 19)

# Conclusão: excetuando score_periculosidade [variável dependente] vs tempo_preso,
# não há variáveis numéricas com correlação forte entre si.
# À priori, não descartaremos nenhuma.

# Ajuste do modelo: score de periculosidade ~ idade + escolaridade + reincidência + filhos + sexo + tempo preso + casado
fit.model <- lm(formula = score_periculosidade ~ idade + escolaridade + reincidente + 
                  filhos + sexo + tempo_preso + casado, data = base)

# Avaliação dos pressupostos do modelo
# Pergunta orientadora: "Os resíduos satisfazem as condições de homocedasticidade e normalidade?"

# Teste de homocedasticidade (Breusch-Pagan)
# H0: os resíduos são homocedásticos
# H1: os resíduos são heterocedásticos
bptest(fit.model)

# Interpretação: Como o p-valor (0.6296) > nível de significânica (0.05), não rejeitamos H0, ou seja, 
# os resíduos satisfazem as condições de homocedasticidade.

# Teste de normalidade dos resíduos (Shapiro-Wilk)
# H0: os resíduos são normalmente distribuídos
# H1: os resíduos não são normalmente distribuídos
shapiro.test(fit.model$residuals)

# Interpretação: Como o p-valor (0.2289) > nível de significânica (0.05), não rejeitamos H0, ou seja, 
# os resíduos satisfazem as condições de normalidade.

# Gráficos de diagnóstico do modelo
par(mfrow = c(1, 2))        # define layout com 2 gráficos lado a lado
plot(fit.model, which = 1)  # resíduos vs valores ajustados
plot(fit.model, which = 2)  # gráfico Q-Q dos resíduos
par(mfrow = c(1, 1))        # retorna ao layout padrão

# Resumo do modelo ajustado
# Estimativas dos coeficientes, R², p-valores e diagnóstico do modelo.
summary(fit.model)

# Interpretação: 
# p-valores dos Betas de intercepto (β0), escolaridade, reincidente, sexo e tempo_preso são pequenos,
# ou seja, rejeitamos H0 (βi = 0) para cada um. Logo, estes βi são diferentes de 0, ou seja,
# as variáveis associadas ajudam a explicar o o score_periculosidade.
# p-valores dos β de idade, filhos e casado são grandes,
# ou seja, não rejeitamos H0. Logo, estes coeficientes β são igual a 0, sugerindo 
# que as variáveis associadas não impactam|ajudam a explicar o score_periculosidade.
# Reparamos que o p-valor de βcasadoSim não foi tão maior assim que a significância (~2x).

# Em relação às variáveis que ajudam a explicar o score_periculosidade:
# β0 (intercepto) ~ 38.8 | média do score_periculosidade quando todas as variáveis explicativas são zero.
# βescolaridadeMédio ~ -6.3 | possuir escolaridade nível médio reduz 6,3 unidades no score de periculosidade,
# em relação à categoria de referência (escolaridade nível fundamental).
# βescolaridadeSuperior ~ -11.3 | possuir escolaridade nível superior reduz 11,3 unidades no score de periculosidade,
# em relação à categoria de referência (escolaridade nível fundamental).
# βreincidenteSim ~ 15.8 | ser reincidente aumenta 15,8 unidades no score de periculosidade.
# βsexoMasculino  ~ 19.1 | ser do sexo masculino aumenta 19,1 unidades no score de periculosidade.
# βtempo_preso ~ 2 | aumento de 1 mês do tempo preso aumenta 2 unidades no score de periculosidade.

# Como o valor do R² ajustado é de 0.8355, significa dizer que 83.55% da variação 
# no score de periculosidade é explicada pelas variáveis independentes.

# ------------------------------------------------------------------------------
# QUESTÃO 06 - Seleção de variáveis explicativas  a partir do modelo ajustado,  
# checagens dos pressupostos e interpretação dos resultados.
# ------------------------------------------------------------------------------

# Seleção automática de variáveis pelo critério AIC
step(fit.model)

# Interpretação: Aplicando o critério AIC para a seleção de variáveis a partir do modelo ajustado, 
# concluiu-se que as variáveis "idade" e "filhos" não contribuiam significativmente para a explicação do
# score de periculosidade. A remoção dessas variáveis contribuiu para um valor de AIC menor, que se traduz
# em um maior equilíbrio entre ajuste e complexidade quando comparado ao modelo inicial com todas as variáveis.

# Ajuste novo modelo após a seleção de variáveis
best.model <- lm(formula = score_periculosidade ~ escolaridade + reincidente + 
                   sexo + tempo_preso + casado, data = base)

# Avaliação dos pressupostos para o novo modelo

# Teste de homocedasticidade (Breusch-Pagan)
# H0: os resíduos são homocedásticos
# H1: os resíduos são heterocedásticos
bptest(best.model)

# Interpretação: Como o p-valor (0.4843) > nível de significânica (0.05), não rejeitamos H0, ou seja, 
# os resíduos satisfazem as condições de homocedasticidade.

# Teste de normalidade dos resíduos (Shapiro-Wilk)
# H0: os resíduos são normalmente distribuídos
# H1: os resíduos não são normalmente distribuídos
shapiro.test(best.model$residuals)

# Interpretação: Como o p-valor (0.2037) > nível de significânica (0.05), não rejeitamos H0, ou seja, 
# os resíduos satisfazem as condições de normalidade.

# Gráficos de diagnóstico do modelo
par(mfrow = c(1, 2))
plot(best.model, which = 1)
plot(best.model, which = 2)
par(mfrow = c(1, 1))

# Resumo do novo modelo
# Estimativas dos coeficientes, R², p-valores e diagnóstico do modelo.
summary(object = best.model)

# Interpretação:
# β0 (intercepto) ~ 38.8 | média do score_periculosidade quando todas as variáveis explicativas são zero.
# βescolaridadeMédio ~ -6.4 | possuir escolaridade nível médio reduz 6,4 unidades no score de periculosidade,
# em relação à categoria de referência (escolaridade nível fundamental).
# βescolaridadeSuperior ~ -11.2 | possuir escolaridade nível superior reduz 11,2 unidades no score de periculosidade,
# em relação à categoria de referência (escolaridade nível fundamental).
# βreincidenteSim ~ 15.8 | ser reincidente aumenta 15,8 unidades no score de periculosidade.
# βsexoMasculino  ~ 19.2 | ser do sexo masculino aumenta 19,2 unidades no score de periculosidade.
# βtempo_preso ~ 2 | aumento de 1 mês do tempo preso aumenta 2 unidades no score de periculosidade.
# βcasadoSim ~ 2.4 | ser casado aumenta 2,4 unidades no score de periculosidade.

# Como o valor do R² ajustado é de 0.8367, significa dizer que 83.67% da variação 
# no score de periculosidade é explicada pelas variáveis independentes.