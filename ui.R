
ui <- page_navbar(
  title = tags$b("Controle Financeiro"),
  theme = bs_theme(bootswatch="flatly", primary="#1565C0",
                   base_font=font_google("Inter"), heading_font=font_google("Inter")),
  bg = "#1565C0", inverse = TRUE,
  
  nav_panel("Lancar", icon = icon("plus-circle"),
            br(),
            layout_columns(col_widths = c(5, 7),
                           card(card_header("Novo lancamento"),
                                card_body(
                                  dateInput("data", "Data", value=Sys.Date(), format="dd/mm/yyyy", language="pt-BR"),
                                  textInput("descricao", "Descricao", placeholder="Ex: Supermercado Extra"),
                                  selectInput("tipo", "Tipo",
                                              choices=c("Debito","Credito","Receita Fixa","Receita Eventual")),
                                  conditionalPanel("input.tipo == 'Debito' || input.tipo == 'Credito'",
                                                   selectInput("categoria", "Categoria", choices=CATEGORIAS),
                                                   uiOutput("subcategoria_ui")),
                                  conditionalPanel("input.tipo == 'Credito'",
                                                   selectInput("cartao", "Tipo de Credito", choices=c("Cartao","Outros")),
                                                   div(class="alert alert-info p-2 mb-2", style="font-size:.85rem;",
                                                       "Informe o vencimento da fatura em que esta compra caira."),
                                                   dateInput("vencimento", "Vencimento da fatura",
                                                             value=ceiling_date(Sys.Date(),"month"),
                                                             format="dd/mm/yyyy", language="pt-BR", min=Sys.Date())),
                                  conditionalPanel("input.tipo == 'Debito' || input.tipo == 'Credito'",
                                                   checkboxInput("dividir", "Dividir com namorada?", value=FALSE),
                                                   conditionalPanel("input.dividir == true",
                                                                    sliderInput("divisao_pct", "% que ela paga",
                                                                                min=5, max=100, value=50, step=5, post="%")
                                                   )),
                                  numericInput("valor", "Valor (R$)", value=NULL, min=0.01, step=0.01),
                                  actionButton("adicionar", "Adicionar lancamento",
                                               class="btn-primary w-100 mt-2", icon=icon("check"))
                                )
                           ),
                           card(
                             card_header(layout_columns(col_widths=c(8,4),
                                                        "Ultimos lancamentos",
                                                        div(style="text-align:right;",
                                                            actionButton("limpar_sel", "Excluir selecionado",
                                                                         class="btn-outline-danger btn-sm")))),
                             card_body(DTOutput("tabela_recente"))
                           )
            )
  ),
  
  nav_panel("Fixos", icon = icon("repeat"),
            br(),
            layout_columns(col_widths = c(4, 8),
                           card(card_header("Cadastrar lancamento fixo"),
                                card_body(
                                  textInput("fixo_descricao", "Descricao",
                                            placeholder="Ex: Salario, Aluguel..."),
                                  selectInput("fixo_tipo", "Tipo",
                                              choices=c("Debito","Receita Fixa","Receita Eventual")),
                                  conditionalPanel("input.fixo_tipo == 'Debito'",
                                                   selectInput("fixo_categoria", "Categoria", choices=CATEGORIAS),
                                                   uiOutput("fixo_subcategoria_ui")),
                                  conditionalPanel("input.fixo_tipo == 'Debito'",
                                                   checkboxInput("fixo_dividir", "Dividir com namorada?", value=FALSE),
                                                   conditionalPanel("input.fixo_dividir == true",
                                                                    sliderInput("fixo_divisao_pct", "% que ela paga",
                                                                                min=5, max=100, value=50, step=5, post="%")
                                                   )),
                                  numericInput("fixo_dia", "Dia do mes que cai",
                                               value=1, min=1, max=28, step=1),
                                  dateInput("fixo_ate", "Repetir ate o mes de",
                                            value=floor_date(Sys.Date()+years(1),"month"),
                                            format="mm/yyyy", language="pt-BR", min=Sys.Date()),
                                  numericInput("fixo_valor", "Valor (R$)", value=NULL, min=0.01, step=0.01),
                                  actionButton("adicionar_fixo", "Salvar lancamento fixo",
                                               class="btn-success w-100 mt-2", icon=icon("check"))
                                )
                           ),
                           card(
                             card_header(layout_columns(col_widths=c(8,4),
                                                        "Lancamentos fixos cadastrados",
                                                        div(style="text-align:right;",
                                                            actionButton("excluir_fixo", "Excluir selecionado",
                                                                         class="btn-outline-danger btn-sm")))),
                             card_body(DTOutput("tabela_fixos"))
                           )
            )
  ),
  
  nav_panel("Relatorio", icon = icon("chart-pie"),
            br(),
            layout_columns(col_widths = c(3, 9),
                           card(card_header("Filtros"),
                                card_body(
                                  selectInput("filtro_mes", "Mes",
                                              choices=format(seq(floor_date(Sys.Date()-365,"month"),
                                                                 ceiling_date(Sys.Date(),"month"),
                                                                 by="month"), "%m/%Y"),
                                              selected=format(Sys.Date(),"%m/%Y")),
                                  hr(),
                                  uiOutput("resumo_lateral")
                                )
                           ),
                           div(
                             layout_columns(col_widths=c(6,6),
                                            card(card_header("Entrada x Saida"),
                                                 card_body(plotOutput("graf_fluxo",   height="280px"))),
                                            card(card_header("Gastos por Categoria"),
                                                 card_body(plotOutput("graf_cat",     height="280px")))
                             ),
                             br(),
                             card(card_header("Historico mensal"),
                                  card_body(plotOutput("graf_historico", height="240px")))
                           )
            )
  ),
  
  nav_panel("Contas divididas", icon = icon("people-arrows"),
            br(),
            layout_columns(col_widths = c(4, 8),
                           card(
                             card_header("Contas Divididas"),
                             card_body(
                               selectInput("credito_venc", "Mes de referencia",
                                           choices  = c("Todos os meses",
                                                        format(seq(floor_date(Sys.Date()-365,"month"),
                                                                   ceiling_date(Sys.Date()+365,"month"),
                                                                   by="month"), "%m/%Y")),
                                           selected = format(Sys.Date(), "%m/%Y")),
                               hr(),
                               uiOutput("resumo_fatura")
                             )
                           ),
                           card(
                             card_header("Lancamentos divididos com a namorada"),
                             card_body(DTOutput("tabela_credito"))
                           )
            )
  )
)
