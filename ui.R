## UI #######################################
ui_app <- page_navbar(
  title = tags$b("Meu Dinheirin"),
  theme = bs_theme(bootswatch="flatly",
                   primary="#3f5f7a", secondary="#7a6f8a",
                   success="#3fae6a", danger="#d1495b", info="#4fc3d9",
                   base_font=font_google("Inter"), heading_font=font_google("Inter")),
  bg = "#3f5f7a", inverse = TRUE,
  header = tagList(
    tags$style(HTML("
      .navbar .nav-link.active,
      .navbar .nav-link.show {
        color: #0d3350 !important;
        font-weight: 700;
      }
      .compacto .form-group { margin-bottom: 0 !important; }
    ")),
    # Copia as colunas marcadas da tabela "tabela_credito", na ordem em que ela
    # esta ordenada/filtrada na tela. Roda no navegador (dentro do clique), o que
    # evita bloqueio de area de transferencia que ocorre com copia via servidor.
    tags$script(HTML(r"---(
      (function() {
        if (window.__copiarDivisaoInit) return;
        window.__copiarDivisaoInit = true;

        function decodificar(s) {
          if (s === null || s === undefined) return '';
          var t = document.createElement('textarea');
          t.innerHTML = String(s);
          return t.value;
        }
        function limpar(s) {
          return decodificar(s).replace(/[\t\r\n]+/g, ' ').trim();
        }
        function copiarTexto(texto) {
          if (navigator.clipboard && window.isSecureContext) {
            return navigator.clipboard.writeText(texto);
          }
          return new Promise(function(resolve, reject) {
            var ta = document.createElement('textarea');
            ta.value = texto;
            ta.style.position = 'fixed';
            ta.style.opacity = '0';
            document.body.appendChild(ta);
            ta.focus();
            ta.select();
            var ok = false;
            try { ok = document.execCommand('copy'); } catch (e) {}
            document.body.removeChild(ta);
            if (ok) { resolve(); } else { reject(); }
          });
        }
        function avisar(ok, linhas, colunas, motivo) {
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('copia_resultado',
              {ok: ok, linhas: linhas, colunas: colunas, motivo: motivo || '', t: Date.now()},
              {priority: 'event'});
          }
        }

        $(document).on('click', '#copiar_divisao', function() {
          var el = $('#tabela_credito table.dataTable');
          if (!el.length) { avisar(false, 0, 0, 'sem_tabela'); return; }

          var tabela = el.DataTable();
          var titulos = tabela.columns().header().toArray().map(function(th) { return limpar($(th).text()); });
          if (titulos.length === 1 && titulos[0] === 'Mensagem') { avisar(false, 0, 0, 'sem_dados'); return; }

          var escolhidas = $('#cols_copiar input:checked').map(function() { return limpar(this.value); }).get();
          var idx = [];
          titulos.forEach(function(t, i) { if (escolhidas.indexOf(t) !== -1) idx.push(i); });
          if (!idx.length) { avisar(false, 0, 0, 'sem_colunas'); return; }

          // ordem 'current' = ordenacao atual da tabela; search 'applied' = respeita a busca/filtro
          var linhas = tabela.rows({order: 'current', search: 'applied'}).data().toArray();
          if (!linhas.length) { avisar(false, 0, idx.length, 'sem_dados'); return; }

          var saida = [];
          if ($('#copiar_cabecalho').is(':checked')) {
            saida.push(idx.map(function(i) { return titulos[i]; }).join('\t'));
          }
          linhas.forEach(function(l) {
            saida.push(idx.map(function(i) { return limpar(l[i]); }).join('\t'));
          });

          copiarTexto(saida.join('\n'))
            .then(function() { avisar(true, linhas.length, idx.length); })
            .catch(function() { avisar(false, linhas.length, idx.length, 'clipboard'); });
        });
      })();
    )---"))
  ),

  nav_panel("Lancar", icon = icon("plus-circle"),
            br(),
            layout_columns(col_widths = c(3, 9),
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
                                                             format="dd/mm/yyyy", language="pt-BR", min=Sys.Date()),
                                                   numericInput("parcelas", "Numero de parcelas",
                                                                value=1, min=1, max=48, step=1)),
                                  conditionalPanel("input.tipo == 'Debito' || input.tipo == 'Credito'",
                                                   checkboxInput("dividir", "Dividir com namorada?", value=FALSE),
                                                   conditionalPanel("input.dividir == true",
                                                                    sliderInput("divisao_pct", "% que ela paga",
                                                                                min=5, max=100, value=50, step=5, post="%")
                                                   )),
                                  conditionalPanel("input.tipo == 'Credito' && input.parcelas > 1",
                                                   div(class="alert alert-info p-2 mb-2", style="font-size:.85rem;",
                                                       "Informe o valor TOTAL da compra — o app divide pelas parcelas automaticamente.")),
                                  numericInput("valor", "Valor (R$)", value=NULL, min=0.01, step=0.01),
                                  actionButton("adicionar", "Adicionar lancamento",
                                               class="btn-primary w-100 mt-2", icon=icon("check"))
                                )
                           ),
                           card(
                             card_header(layout_columns(col_widths=c(6,3,3),
                                                        "Ultimos lancamentos",
                                                        div(style="text-align:right;",
                                                            actionButton("editar_sel", "Editar",
                                                                         class="btn-outline-primary btn-sm")),
                                                        div(style="text-align:right;",
                                                            actionButton("limpar_sel", "Excluir",
                                                                         class="btn-outline-danger btn-sm")))),
                             card_body(DTOutput("tabela_recente"))
                           )
            )
  ),

  nav_panel("Fixos", icon = icon("repeat"),
            br(),
            layout_columns(col_widths = c(3, 9),
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
                             card_header(layout_columns(col_widths=c(6,3,3),
                                                        "Lancamentos fixos cadastrados",
                                                        div(style="text-align:right;",
                                                            actionButton("editar_fixo", "Editar",
                                                                         class="btn-outline-primary btn-sm")),
                                                        div(style="text-align:right;",
                                                            actionButton("excluir_fixo", "Excluir",
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
                                                                 ceiling_date(Sys.Date()+365,"month"),
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

  nav_panel("Divisão de bens", icon = icon("people-arrows"),
            br(),
            layout_columns(col_widths = c(3, 9),
                           card(
                             card_header("Divisão de bens"),
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
                             card_body(
                               checkboxGroupInput("cols_copiar", "Colunas para copiar",
                                                  choices = COLUNAS_DIVISAO,
                                                  selected = COLUNAS_DIVISAO,
                                                  inline = TRUE),
                               div(class = "compacto d-flex align-items-center gap-3 mb-3",
                                   actionButton("copiar_divisao", "Copiar",
                                                icon = icon("copy"), class = "btn-primary btn-sm"),
                                   checkboxInput("copiar_cabecalho", "Incluir cabecalho", value = TRUE)),
                               DTOutput("tabela_credito")
                             )
                           )
            )
  ),

  nav_panel("Investimentos", icon = icon("chart-line"),
            br(),
            layout_columns(col_widths = c(3, 9),
                           card(card_header("Novo investimento"),
                                card_body(
                                  dateInput("invest_data", "Data do aporte", value=Sys.Date(),
                                            format="dd/mm/yyyy", language="pt-BR"),
                                  textInput("invest_descricao", "Descricao",
                                            placeholder="Ex: Tesouro Selic, ETF, etc."),
                                  selectInput("invest_tipo", "Tipo",
                                              choices=c("Renda Fixa","Fundos","Acoes","Cripto","Outros")),
                                  numericInput("invest_aportado", "Valor aportado (R$)",
                                               value=NULL, min=0.01, step=0.01),
                                  numericInput("invest_rentabilidade", "Rentabilidade (% ao ano)",
                                               value=NULL, min=-100, max=1000, step=0.1),
                                  div(class="alert alert-info p-2 mb-2", style="font-size:.85rem;",
                                      "Com a rentabilidade preenchida, o valor estimado para hoje e calculado automaticamente (juros compostos pelos dias desde o aporte)."),
                                  numericInput("invest_atual", "Valor atual - manual (R$)",
                                               value=NULL, min=0.01, step=0.01),
                                  div(class="alert alert-warning p-2 mb-2", style="font-size:.85rem;",
                                      "Preencha 'Valor atual' so se quiser sobrescrever a estimativa com um valor real (ex: extrato da corretora). Deixe em branco para usar a estimativa pela rentabilidade."),
                                  actionButton("adicionar_investimento", "Adicionar investimento",
                                               class="btn-primary w-100 mt-2", icon=icon("check"))
                                )
                           ),
                           div(
                             card(card_header("Resumo — Distribuicao por tipo"),
                                  card_body(plotOutput("graf_investimentos", height="420px"))),
                             br(),
                             card(
                               card_header(layout_columns(col_widths=c(5,2,3,2),
                                                          "Investimentos cadastrados",
                                                          div(style="text-align:right;",
                                                              actionButton("editar_investimento", "Editar",
                                                                           class="btn-outline-primary btn-sm")),
                                                          div(style="text-align:right;",
                                                              actionButton("resgatar_investimento", "Resgatar",
                                                                           class="btn-outline-warning btn-sm")),
                                                          div(style="text-align:right;",
                                                              actionButton("excluir_investimento", "Excluir",
                                                                           class="btn-outline-danger btn-sm")))),
                               card_body(DTOutput("tabela_investimentos"))
                             )
                           )
            )
  )
)

# Envolve a interface com a tela de login do shinymanager
ui <- secure_app(ui_app, language = "pt")
