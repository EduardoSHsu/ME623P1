# PROJETO 1: PLANEJAMENTO EXPERIMENTAL (ME623)

# Lista de pacotes necessários
pacotes <- c("mixexp", "ggplot2", "ggtern", "dplyr")

# Instalação e carregamento automático
for (p in pacotes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# Data frame contendo as proporções (0-100%), as categorias funcionais do delineamento e os resultados de cTnI (Média e SD).

dados_mistura <- data.frame(
  Ponto = factor(1:10),
  Categoria = factor(c("Puro", "Puro", "Puro", 
                       "Binário", "Binário", "Binário", 
                       "Centroide", 
                       "Axial", "Axial", "Axial"),
                     levels = c("Puro", "Binário", "Centroide", "Axial")),
  Sal_A = c(100, 0, 0, 50, 50, 0, 33.33, 66.67, 16.67, 16.67),
  Sal_B = c(0, 100, 0, 50, 0, 50, 33.33, 16.67, 66.67, 16.67),
  DSS   = c(0, 0, 100, 0, 50, 50, 33.33, 16.67, 16.67, 66.67),
  cTnI_Media = c(1.12, 1.13, 1.24, 1.21, 1.12, 1.29, 1.21, 0.72, 1.11, 1.01),
  cTnI_SD    = c(0.01, 0.03, 0.01, 0.02, 0.03, 0.01, 0.13, 0.11, 0.05, 0.01)
)

# Exibição da Matriz de Planejamento e Resultados no Console (Tabela 1) 
cat("\n--- Matriz de Planejamento Simplex-Centroide Aumentado (Tabela 1) ---\n")
print(dados_mistura)

# Gráfico Ternário Básico: Mostra a distribuição dos 10 pontos no Simplex.
DesignPoints(dados_mistura[, 3:5] / 100) 

# 1. Gráfico Ternário de Bolhas: Mapeia o nível de cTnI no triângulo.
grafico_ternario <- ggtern(data = dados_mistura, 
                           aes(x = Sal_B, y = Sal_A, z = DSS)) +
  geom_point(aes(size = cTnI_Media, fill = cTnI_Media), 
             shape = 21, color = "black", alpha = 0.8) +
  geom_text(aes(label = Ponto), vjust = 2.5, size = 4) +
  scale_size_continuous(range = c(5, 15), name = "cTnI (ng/mL)") +
  scale_fill_gradient(low = "darkgreen", high = "red", name = "cTnI (ng/mL)") +
  theme_bw() +
  theme_showarrows() +
  labs(Tarrow = "Sal-A (%)", Larrow = "Sal-B (%)", Rarrow = "DSS (%)") +
  theme(legend.position = "right")

print(grafico_ternario)

# 2. Gráfico de Barras Agrupado: Compara eficácia entre categorias funcionais.
cores_categorias <- c("Puro" = "#4477AA", "Binário" = "#66CCEE", 
                      "Centroide" = "#228833", "Axial" = "#CCBB44")

grafico_barras <- ggplot(dados_mistura, aes(x = reorder(Ponto, cTnI_Media), 
                                            y = cTnI_Media, fill = Categoria)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = cTnI_Media - cTnI_SD, ymax = cTnI_Media + cTnI_SD), 
                width = 0.2, linewidth = 0.7) +
  scale_fill_manual(values = cores_categorias) +
  theme_classic() +
  labs(x = "Ponto Experimental (Ordenado por eficácia)",
       y = "cTnI (ng/mL)") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11))

print(grafico_barras)