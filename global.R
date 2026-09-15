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
library(shinymanager)

## ── Login do app ──────────────────────────────────────────────────────
# No Connect Cloud, configure em Settings > Environment Variables:
#   APP_USER     -> nome de usuario para login
#   APP_PASSWORD -> senha para login
# Se essas variaveis nao existirem (ex: rodando localmente com runApp()),
# usa um usuario/senha padrao soh para nao travar o desenvolvimento local.
CREDENCIAIS <- data.frame(
  user     = Sys.getenv("APP_USER", "admin"),
  password = Sys.getenv("APP_PASSWORD", "admin"),
  stringsAsFactors = FALSE
)

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
        dia = integer(), mes_inicio = as.Date(character()),
        ate_mes = as.Date(character()),
        valor = numeric(), divisao = numeric()
      )
    } else {
      df <- as_tibble(df)
      df$ate_mes <- as.Date(df$ate_mes)
      if (!"divisao" %in% names(df)) df$divisao <- 0
      if (!"mes_inicio" %in% names(df)) df$mes_inicio <- as.Date("2000-01-01")
      df$mes_inicio <- as.Date(df$mes_inicio)
      df
    }
  } else if (file.exists(FIXOS_FILE)) {
    df <- readRDS(FIXOS_FILE)
    if (!"divisao" %in% names(df)) df$divisao <- 0
    if (!"mes_inicio" %in% names(df)) df$mes_inicio <- as.Date("2000-01-01")
    df
  } else {
    tibble(
      id = integer(), descricao = character(),
      categoria = character(), subcategoria = character(),
      tipo = character(), cartao = character(),
      dia = integer(), mes_inicio = as.Date(character()),
      ate_mes = as.Date(character()),
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

## ── Investimentos ────────────────────────────────────────────────────
INVEST_FILE <- "investimentos.rds"

carregar_investimentos <- function() {
  if (USA_SUPABASE) {
    df <- tryCatch({
      con <- conectar_db()
      on.exit(dbDisconnect(con))
      dbGetQuery(con, "SELECT * FROM investimentos ORDER BY id")
    }, error = function(e) NULL)

    if (is.null(df) || nrow(df) == 0) {
      tibble(
        id = integer(), data = as.Date(character()),
        descricao = character(), tipo = character(),
        valor_aportado = numeric(), valor_atual = numeric()
      )
    } else {
      df <- as_tibble(df)
      df$data <- as.Date(df$data)
      df
    }
  } else if (file.exists(INVEST_FILE)) {
    readRDS(INVEST_FILE)
  } else {
    tibble(
      id = integer(), data = as.Date(character()),
      descricao = character(), tipo = character(),
      valor_aportado = numeric(), valor_atual = numeric()
    )
  }
}

salvar_investimentos <- function(df) {
  if (USA_SUPABASE) {
    con <- conectar_db()
    on.exit(dbDisconnect(con))
    dbExecute(con, "DELETE FROM investimentos")
    if (nrow(df) > 0) dbAppendTable(con, "investimentos", df)
  } else {
    saveRDS(df, INVEST_FILE)
  }
}

CATEGORIAS   <- c("Basico", "Extras")
SUBCATEGORIAS <- c("Moradia","Alimentacao","Transporte","Saude","Educacao",
                   "Lazer","Comprinhas","Assinaturas","Investimento","Outros")
receitas <- c("Receita Fixa", "Receita Eventual")

## Paleta principal do app, em versoes mais fortes/saturadas das cores base
## (#90d7ff, #c9f9ff, #bfd0e0, #b8b3be), evitando tons escuros ou neon.
CORES_SUBCAT <- c(
  Moradia="#2ba9e0", Alimentacao="#3fae7c", Transporte="#8a5bc2",
  Saude="#c9507a", Educacao="#5b7fa6", Lazer="#35c4d9",
  Comprinhas="#c98a4f", Assinaturas="#3f5f7a",
  Investimento="#7a6f8a", Outros="#6f6f78"
)

# Verde e vermelho fortes (nem escuros, nem neon) para Receita x Despesa
COR_RECEITA <- "#3fae6a"
COR_DESPESA <- "#d1495b"

# Cores para o grafico de distribuicao de investimentos por tipo
CORES_INVEST <- c(
  "Renda Fixa"="#2ba9e0", "Fundos"="#35c4d9", "Acoes"="#5b7fa6",
  "Cripto"="#8a5bc2", "Outros"="#7a6f8a"
)

tema_app <- theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill="#f4fbfd", color=NA),
    panel.background = element_rect(fill="#f4fbfd", color=NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color="#dce9ee"),
    plot.title = element_text(face="bold", size=13, color="#2c3e50"),
    axis.text = element_text(color="#5a6b73", size=10),
    legend.background = element_rect(fill="#f4fbfd", color=NA)
  )

fmt_brl <- function(x) {
  paste0("R$ ", formatC(x, format="f", digits=2, big.mark=".", decimal.mark=","))
}
