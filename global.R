## Baixar e Carregar Pacotes ##
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(lubridate)

DATA_FILE  <- "lancamentos.rds"
FIXOS_FILE <- "fixos.rds"

carregar_dados <- function() {
  if (file.exists(DATA_FILE)) {
    df <- readRDS(DATA_FILE)
    if (!"origem" %in% names(df)) df$origem <- "manual"
    if (!"divisao" %in% names(df)) df$divisao <- 0
    df
  } else {
    tibble(
      id = integer(), data = as.Date(character()),
      descricao = character(), categoria = character(),
      subcategoria = character(), tipo = character(),
      cartao = character(), vencimento = as.Date(character()),
      valor = numeric(), origem = character(),
      divisao = numeric()
    )
  }
}

carregar_fixos <- function() {
  if (file.exists(FIXOS_FILE)) {
    df <- readRDS(FIXOS_FILE)
    if (!"divisao" %in% names(df)) df$divisao <- 0
    df
  } else {
    tibble(
      id = integer(), descricao = character(),
      categoria = character(), subcategoria = character(),
      tipo = character(), cartao = character(),
      dia = integer(), ate_mes = as.Date(character()),
      valor = numeric(), divisao = numeric()
    )
  }
}

salvar_dados <- function(df) saveRDS(df, DATA_FILE)
salvar_fixos <- function(df) saveRDS(df, FIXOS_FILE)

CATEGORIAS   <- c("Basico", "Extras")
SUBCATEGORIAS <- c("Moradia","Alimentacao","Transporte","Saude","Educacao",
                   "Lazer","Comprinhas","Assinaturas","Investimento","Outros")
receitas <- c("Receita Fixa", "Receita Eventual")

CORES_SUBCAT <- c(
  Moradia="#1565C0", Alimentacao="#2E7D32", Transporte="#F57F17",
  Saude="#AD1457", Educacao="#6A1B9A", Lazer="#00838F",
  Comprinhas="#4E342E", Assinaturas="#37474F",
  Investimento="#0D47A1", Outros="#757575"
)

tema_app <- theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill="#f5f7fa", color=NA),
    panel.background = element_rect(fill="#f5f7fa", color=NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color="#e0e4ea"),
    plot.title = element_text(face="bold", size=13, color="#1a1a2e"),
    axis.text = element_text(color="#555", size=10),
    legend.background = element_rect(fill="#f5f7fa", color=NA)
  )

fmt_brl <- function(x) {
  paste0("R$ ", formatC(x, format="f", digits=2, big.mark=".", decimal.mark=","))
}
