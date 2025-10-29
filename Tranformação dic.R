# ========================= #
# SCRIPT DE TRANSFORMAÇÕES - DIC
# ========================= #
library(dplyr)
library(car)

# ========================= #
# 1. Importar dados  
# ========================= #
aula <- read.table("clipboard", dec = ",", header = TRUE)
colnames(aula) <- c("Tratamento", "Valor")

aula$Tratamento <- as.factor(aula$Tratamento)
aula$Valor <- as.numeric(as.character(aula$Valor))

# ========================= #
# 2. Criar transformações
# ========================= #
aula <- aula %>%
  mutate(
    log_Valor = ifelse(Valor > 0, log(Valor), NA),
    sqrt_Valor = sqrt(Valor),
    inv_Valor = 1 / Valor,
    arcsin_Valor = asin(sqrt(Valor / 100))
  )

# ========================= #
# 3. Testes de normalidade e homogeneidade
# ========================= #
variaveis_transformadas <- c("Valor", "log_Valor", "sqrt_Valor", "inv_Valor", "arcsin_Valor")

# Criar modelo ANOVA para calcular os resíduos
modelo <- aov(Valor ~ Tratamento, data = aula)
residuos <- residuals(modelo)

# P-valor original do teste de Shapiro sobre os resíduos
shapiro_original <- shapiro.test(residuos)$p.value

resumo_transform <- data.frame(
  Transformacao = character(),
  Shapiro_W = numeric(),
  Shapiro_p = numeric(),
  Shapiro_ok = character(),
  Levene_F = numeric(),
  Levene_p = numeric(),
  Levene_ok = character(),
  Pvalor_Original = numeric(),
  Necessario_Transformar = character(),
  stringsAsFactors = FALSE
)

for(v in variaveis_transformadas){
  # Teste de normalidade
  shapiro <- shapiro.test(aula[[v]])
  shapiro_ok <- ifelse(shapiro$p.value >= 0.05, "Sim", "Não")
  
  # Teste de homogeneidade
  levene <- leveneTest(aula[[v]] ~ aula$Tratamento)
  levene_ok <- ifelse(levene$`Pr(>F)`[1] >= 0.05, "Sim", "Não")
  
  # Decisão final
  necessario <- ifelse(shapiro_ok == "Não" | levene_ok == "Não", "Sim", "Não")
  
  # Adicionar na tabela de resumo
  resumo_transform <- rbind(resumo_transform, data.frame(
    Transformacao = v,
    Shapiro_W = round(shapiro$statistic, 4),
    Shapiro_p = round(shapiro$p.value, 4),
    Shapiro_ok = shapiro_ok,
    Levene_F = round(levene$`F value`[1], 4),
    Levene_p = round(levene$`Pr(>F)`[1], 4),
    Levene_ok = levene_ok,
    Pvalor_Original = round(shapiro_original, 4),  # p-valor dos resíduos
    Necessario_Transformar = necessario
  ))
}

# ========================= #
# 4. Criar tabela com dados originais e transformados
# ========================= #
tabela_completa <- aula %>%
  select(Tratamento, Valor, log_Valor, sqrt_Valor, inv_Valor, arcsin_Valor)

# ========================= #
# 5. Mostrar resultados
# ========================= #
cat("\n===== Resumo Transformações =====\n")
print(resumo_transform)

cat("\n===== Tabela com Dados Originais e Transformados =====\n")
print(tabela_completa)

