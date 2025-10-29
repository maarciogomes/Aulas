# ========================= #
# SCRIPT DE TRANSFORMAÇÕES - DBC (Delineamento em Bloco Casualizado)
# Teste de normalidade nos resíduos de cada transformação
# ========================= #
library(dplyr)

# ========================= #
# 1. Importar dados  
# ========================= #
aula <- read.table("clipboard", dec = ",", header = TRUE)
colnames(aula) <- c("Tratamento", "Bloco", "Valor")

# Converter variáveis para fatores e numéricas
aula$Tratamento <- as.factor(aula$Tratamento)
aula$Bloco <- as.factor(aula$Bloco)
aula$Valor <- as.numeric(as.character(aula$Valor))

# ========================= #
# 2. Criar transformações
# ========================= #
aula <- aula %>%
  mutate(
    log_Valor = ifelse(Valor > 0, log(Valor), NA),
    sqrt_Valor = sqrt(Valor),
    inv_Valor = ifelse(Valor != 0, 1 / Valor, NA),
    arcsin_Valor = ifelse(Valor <= 100, asin(sqrt(Valor / 100)), NA)
  )

# ========================= #
# 3. Testes de normalidade nos resíduos
# ========================= #
variaveis_transformadas <- c("Valor", "log_Valor", "sqrt_Valor", "inv_Valor", "arcsin_Valor")

resumo_transform <- data.frame(
  Transformacao = character(),
  Shapiro_W = numeric(),
  Shapiro_p = numeric(),
  Shapiro_ok = character(),
  Necessario_Transformar = character(),
  stringsAsFactors = FALSE
)

for(v in variaveis_transformadas){
  # Criar modelo ANOVA para cada transformação
  modelo <- aov(as.formula(paste(v, "~ Bloco + Tratamento")), data = aula)
  
  # Resíduos do modelo
  residuos <- residuals(modelo)
  
  # Teste de Shapiro-Wilk nos resíduos
  shapiro <- shapiro.test(residuos)
  shapiro_ok <- ifelse(shapiro$p.value >= 0.05, "Sim", "Não")
  necessario <- ifelse(shapiro_ok == "Não", "Sim", "Não")
  
  # Adicionar resultados ao resumo
  resumo_transform <- rbind(resumo_transform, data.frame(
    Transformacao = v,
    Shapiro_W = round(shapiro$statistic, 4),
    Shapiro_p = round(shapiro$p.value, 4),
    Shapiro_ok = shapiro_ok,
    Necessario_Transformar = necessario
  ))
}

# ========================= #
# 4. Criar tabela com dados originais e transformados
# ========================= #
tabela_completa <- aula %>%
  select(Bloco, Tratamento, Valor, log_Valor, sqrt_Valor, inv_Valor, arcsin_Valor)

# ========================= #
# 5. Mostrar resultados
# ========================= #
cat("\n===== Resumo Transformações (Shapiro-Wilk nos resíduos) =====\n")
print(resumo_transform)

cat("\n===== Tabela com Dados Originais e Transformados =====\n")
print(tabela_completa)

