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

# QQ-plot para avaliação visual da normalidade, mostrando praticamente todos os
# pontos na faixa que indica tendência de normalidade
ggqqplot(data = base$score_periculosidade)

# Teste de Shapiro-Wilk para normalidade
# H0: O score de periculosidade segue uma distribuição aproximadamente normal
# H1: O score de periculosidade não segue uma distribuição aproximadamente normal
shapiro.test(x = base$score_periculosidade)

# Conclusão:
# Como p-valor 0.7738 > 0.05 nível de significância não rejeitamos H0, logo assumimos
# que os scores de periculosidade possuem distribuição aproximadamente normal

# Visualização da distribuição da score de periculosidade, indicando possível
# distribuição ~normal, dada a simetria em relação a mediana
ggplot(data = base, 
       mapping = aes(y = score_periculosidade)) +
  geom_boxplot() +
  labs(y = "Score de periculosidade", x = "") +
  theme_minimal()

# Estimativa pontual igual a 174.5205, reforçando a possibilidade da média populacional
# ser maior que 170
mean(base$score_periculosidade, na.rm = TRUE)

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
# PERGUNTA 04 - Há relação entre o sexo e a reincidência dos indivíduos?
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

# Interpretação: Como o valor do Adjusted R-squared é de 0.8355, significa dizer que 83.55% da variação 
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

# Interpretação: Como o valor do Adjusted R-squared é de 0.8367, significa dizer que 83.67% da variação 
# no score de periculosidade é explicada pelas variáveis independentes.
