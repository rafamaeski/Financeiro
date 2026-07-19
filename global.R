## Baixar e Carregar Pacotes ##
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(lubridate)
library(DBI)
library(RPostgres)

DATA_FILE  <- "lancamentos.rds"
FIXOS_FILE <- "fixos.rds"

## ── Persistencia: Supabase/Postgres (producao) ou .rds local (dev) ──────
# No Connect Cloud, configure estas variaveis em Settings > Environment Variables:
#   SUPABASE_HOST, SUPABASE_PORT, SUPABASE_DB, SUPABASE_USER, SUPABASE_PASSWORD
# Se essas variaveis nao existirem (ex: rodando localmente com runApp()),
# o app usa os arquivos .rds locais automaticamente.

USA_SUPABASE <- nzchar(Sys.getenv("SUPABASE_HOST")) && nzchar(Sys.getenv("SUPABASE_PASSWORD"))

conectar_db <- function() {
  dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("SUPABASE_HOST"),
    port     = as.integer(Sys.getenv("SUPABASE_PORT", "5432")),
    dbname   = Sys.getenv("SUPABASE_DB", "postgres"),
    user     = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASSWORD")
  )
}

carregar_dados <- function() {
  if (USA_SUPABASE) {
    df <- tryCatch({
      con <- conectar_db()
      on.exit(dbDisconnect(con))
      dbGetQuery(con, "SELECT * FROM lancamentos ORDER BY id")
    }, error = function(e) NULL)
    
    if (is.null(df) || nrow(df) == 0) {
      tibble(
        id = integer(), data = as.Date(character()),
        descricao = character(), categoria = character(),
        subcategoria = character(), tipo = character(),
        cartao = character(), vencimento = as.Date(character()),
        valor = numeric(), origem = character(),
        divisao = numeric()
      )
    } else {
      df <- as_tibble(df)
      df$data       <- as.Date(df$data)
      df$vencimento <- as.Date(df$vencimento)
      if (!"origem" %in% names(df)) df$origem <- "manual"
      if (!"divisao" %in% names(df)) df$divisao <- 0
      df
    }
  } else if (file.exists(DATA_FILE)) {
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
  if (USA_SUPABASE) {
    df <- tryCatch({
      con <- conectar_db()
      on.exit(dbDisconnect(con))
      dbGetQuery(con, "SELECT * FROM fixos ORDER BY id")
    }, error = function(e) NULL)
    
    if (is.null(df) || nrow(df) == 0) {
      tibble(
        id = integer(), descricao = character(),
        categoria = character(), subcategoria = character(),
        tipo = character(), cartao = character(),
        dia = integer(), ate_mes = as.Date(character()),
        valor = numeric(), divisao = numeric()
      )
    } else {
      df <- as_tibble(df)
      df$ate_mes <- as.Date(df$ate_mes)
      if (!"divisao" %in% names(df)) df$divisao <- 0
      df
    }
  } else if (file.exists(FIXOS_FILE)) {
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

salvar_dados <- function(df) {
  if (USA_SUPABASE) {
    con <- conectar_db()
    on.exit(dbDisconnect(con))
    dbExecute(con, "DELETE FROM lancamentos")
    if (nrow(df) > 0) dbAppendTable(con, "lancamentos", df)
  } else {
    saveRDS(df, DATA_FILE)
  }
}

salvar_fixos <- function(df) {
  if (USA_SUPABASE) {
    con <- conectar_db()
    on.exit(dbDisconnect(con))
    dbExecute(con, "DELETE FROM fixos")
    if (nrow(df) > 0) dbAppendTable(con, "fixos", df)
  } else {
    saveRDS(df, FIXOS_FILE)
  }
}

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