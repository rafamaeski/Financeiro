## SERVER ##########################################
server <- function(input, output, session) {
  
  rv <- reactiveValues(df = carregar_dados(), fixos = carregar_fixos())
  
  # Subcategoria lancamento normal
  output$subcategoria_ui <- renderUI({
    req(input$categoria)
    choices <- if (input$categoria == "Basico") SUBCATEGORIAS[1:5] else SUBCATEGORIAS[-c(1:5)]
    selectInput("subcategoria", "Subcategoria", choices=choices)
  })
  
  # Subcategoria fixo
  output$fixo_subcategoria_ui <- renderUI({
    req(input$fixo_categoria)
    choices <- if (input$fixo_categoria == "Basico") SUBCATEGORIAS[1:5] else SUBCATEGORIAS[-c(1:5)]
    selectInput("fixo_subcategoria", "Subcategoria", choices=choices)
  })
  
  # Gera os lancamentos fixos (se ainda nao existirem) para um dado mes
  gerar_fixos_do_mes <- function(mes_ano) {
    if (nrow(rv$fixos) == 0) return(invisible())
    
    fixos_validos <- rv$fixos %>%
      filter(floor_date(ate_mes, "month") >= mes_ano)
    
    if (nrow(fixos_validos) == 0) return(invisible())
    
    ja_gerados <- rv$df %>%
      filter(origem == "fixo",
             floor_date(vencimento, "month") == mes_ano) %>%
      pull(descricao)
    
    novos <- fixos_validos %>%
      filter(!descricao %in% ja_gerados) %>%
      mutate(
        id           = seq(max(c(rv$df$id, 0L)) + 1L, length.out=n()),
        data         = as.Date(mes_ano) + (as.integer(dia) - 1L),
        vencimento   = as.Date(mes_ano) + (as.integer(dia) - 1L),
        cartao       = "-",
        origem       = "fixo"
      ) %>%
      select(id, data, descricao, categoria, subcategoria,
             tipo, cartao, vencimento, valor, origem, divisao)
    
    if (nrow(novos) == 0) return(invisible())
    
    rv$df <- bind_rows(rv$df, novos)
    salvar_dados(rv$df)
    showNotification(
      paste0(nrow(novos), " lancamento(s) fixo(s) gerado(s) para ", format(mes_ano, "%m/%Y")),
      type="message", duration=4
    )
  }
  
  # Gerar fixos ao mudar o mes (Relatorio)
  observeEvent(input$filtro_mes, {
    mes_ano <- as.Date(paste0("01/", input$filtro_mes), format="%d/%m/%Y")
    gerar_fixos_do_mes(mes_ano)
  })
  
  # Gerar fixos ao mudar o mes (Contas divididas), garante que fixos
  # divididos ja estejam presentes mesmo sem passar pelo Relatorio
  observeEvent(input$credito_venc, {
    req(input$credito_venc != "Todos os meses")
    mes_ano <- as.Date(paste0("01/", input$credito_venc), format="%d/%m/%Y")
    gerar_fixos_do_mes(mes_ano)
  })
  
  # Adicionar lancamento manual
  observeEvent(input$adicionar, {
    req(input$descricao, input$valor)
    if (is.na(input$valor) || input$valor <= 0) {
      showNotification("Informe um valor maior que zero.", type="warning"); return()
    }
    novo <- tibble(
      id           = if (nrow(rv$df)==0) 1L else max(rv$df$id)+1L,
      data         = as.Date(input$data),
      descricao    = trimws(input$descricao),
      categoria    = if (input$tipo %in% receitas) "-" else input$categoria,
      subcategoria = if (input$tipo %in% receitas) "Receita" else input$subcategoria,
      tipo         = input$tipo,
      cartao       = if (input$tipo=="Credito") input$cartao else "-",
      vencimento   = as.Date(if (input$tipo=="Credito") input$vencimento else input$data),
      valor        = as.numeric(input$valor),
      origem       = "manual",
      divisao      = if (input$tipo %in% c("Debito","Credito") && isTRUE(input$dividir)) input$divisao_pct else 0
    )
    rv$df <- bind_rows(rv$df, novo)
    salvar_dados(rv$df)
    showNotification(paste0("'", novo$descricao, "' adicionado!"), type="message", duration=3)
    updateTextInput(session, "descricao", value="")
    updateNumericInput(session, "valor", value=NA)
    updateDateInput(session, "data", value=Sys.Date())
  })
  
  # Adicionar fixo
  observeEvent(input$adicionar_fixo, {
    req(input$fixo_descricao, input$fixo_valor, input$fixo_dia, input$fixo_ate)
    if (is.na(input$fixo_valor) || input$fixo_valor <= 0) {
      showNotification("Informe um valor maior que zero.", type="warning"); return()
    }
    novo_fixo <- tibble(
      id           = if (nrow(rv$fixos)==0) 1L else max(rv$fixos$id)+1L,
      descricao    = trimws(input$fixo_descricao),
      categoria    = if (input$fixo_tipo %in% receitas) "-" else input$fixo_categoria,
      subcategoria = if (input$fixo_tipo %in% receitas) "Receita" else
        if (is.null(input$fixo_subcategoria)) "Outros" else input$fixo_subcategoria,
      tipo         = input$fixo_tipo,
      cartao       = "-",
      dia          = as.integer(input$fixo_dia),
      ate_mes      = as.Date(floor_date(input$fixo_ate, "month")),
      valor        = as.numeric(input$fixo_valor),
      divisao      = if (input$fixo_tipo == "Debito" && isTRUE(input$fixo_dividir)) input$fixo_divisao_pct else 0
    )
    rv$fixos <- bind_rows(rv$fixos, novo_fixo)
    salvar_fixos(rv$fixos)
    showNotification(paste0("'", novo_fixo$descricao, "' salvo como fixo!"),
                     type="message", duration=3)
    updateTextInput(session, "fixo_descricao", value="")
    updateNumericInput(session, "fixo_valor", value=NA)
  })
  
  # Excluir fixo
  observeEvent(input$excluir_fixo, {
    sel <- input$tabela_fixos_rows_selected
    if (is.null(sel) || length(sel)==0) {
      showNotification("Selecione uma linha para excluir.", type="warning"); return()
    }
    ids_excluir <- rv$fixos %>% slice(sel) %>% pull(id)
    rv$fixos <- filter(rv$fixos, !id %in% ids_excluir)
    salvar_fixos(rv$fixos)
    showNotification("Fixo excluido.", type="message")
  })
  
  # Excluir lancamento
  observeEvent(input$limpar_sel, {
    sel <- input$tabela_recente_rows_selected
    if (is.null(sel) || length(sel)==0) {
      showNotification("Selecione uma linha para excluir.", type="warning"); return()
    }
    ids_excluir <- rv$df %>% arrange(desc(data), desc(id)) %>% slice(sel) %>% pull(id)
    rv$df <- filter(rv$df, !id %in% ids_excluir)
    salvar_dados(rv$df)
    showNotification("Lancamento excluido.", type="message")
  })
  
  # Tabela recente
  output$tabela_recente <- renderDT({
    rv$df %>%
      arrange(desc(data), desc(id)) %>%
      head(50) %>%
      mutate(
        Data      = format(data, "%d/%m/%Y"),
        Vencimento = if_else(tipo=="Credito", format(vencimento,"%d/%m/%Y"), "-"),
        Origem    = if_else(origem=="fixo", "Auto", "Manual"),
        Divisao   = if_else(divisao > 0, paste0(divisao, "%"), "-"),
        Valor     = fmt_brl(valor)
      ) %>%
      select(Data, Descricao=descricao, Subcategoria=subcategoria,
             Tipo=tipo, Origem, `Valor (R$)`=Valor, Vencimento, `% Dela`=Divisao) %>%
      datatable(selection="single", rownames=FALSE,
                options=list(dom="tp", pageLength=10,
                             language=list(
                               paginate=list(previous="Ant", `next`="Pro"),
                               info="Mostrando _START_ a _END_ de _TOTAL_")))
  })
  
  # Tabela fixos
  output$tabela_fixos <- renderDT({
    rv$fixos %>%
      mutate(`Dia do mes`=dia, `Ate`=format(ate_mes,"%m/%Y"), Valor=fmt_brl(valor),
             `% Dela`=if_else(divisao > 0, paste0(divisao, "%"), "-")) %>%
      select(Descricao=descricao, Subcategoria=subcategoria,
             Tipo=tipo, `Dia do mes`, `Ate`, Valor, `% Dela`) %>%
      datatable(selection="single", rownames=FALSE,
                options=list(dom="t", pageLength=20))
  })
  
  # Dados do mes
  dados_mes <- reactive({
    req(input$filtro_mes)
    mes_ano <- as.Date(paste0("01/", input$filtro_mes), format="%d/%m/%Y")
    rv$df %>%
      filter(floor_date(vencimento,"month") == mes_ano) %>%
      mutate(
        # parte que voce efetivamente paga (desconta o que e dividido/reembolsado)
        valor_pessoal = if_else(tipo %in% c("Debito","Credito"),
                                valor * (1 - divisao/100),
                                valor)
      )
  })
  
  # Resumo lateral
  output$resumo_lateral <- renderUI({
    df  <- dados_mes()
    rec <- sum(df$valor[df$tipo %in% receitas], na.rm=TRUE)
    des <- sum(df$valor_pessoal[df$tipo %in% c("Debito","Credito")], na.rm=TRUE)
    sal <- rec - des
    kpi <- function(label, val, cor) {
      div(class="mb-3",
          tags$small(class="text-muted", label),
          tags$h5(class=paste("fw-bold", cor), fmt_brl(val)))
    }
    tagList(
      kpi("Receitas", rec, "text-success"),
      kpi("Despesas", des, "text-danger"),
      kpi("Saldo", sal, if(sal>=0) "text-success" else "text-danger"),
      hr(),
      tags$small(class="text-muted", paste0(nrow(df)," lancamentos no mes"))
    )
  })
  
  # Grafico fluxo
  output$graf_fluxo <- renderPlot({
    df <- dados_mes()
    if (nrow(df)==0) return(NULL)
    tibble(
      Tipo  = c("Receita","Despesa"),
      Total = c(
        sum(df$valor[df$tipo %in% receitas], na.rm=TRUE),
        sum(df$valor_pessoal[df$tipo %in% c("Debito","Credito")], na.rm=TRUE)
      )
    ) %>%
      ggplot(aes(x=Tipo, y=Total, fill=Tipo)) +
      geom_col(width=0.5, show.legend=FALSE) +
      geom_text(aes(label=fmt_brl(Total)), vjust=-0.4,
                fontface="bold", size=3.8, color="#333") +
      scale_fill_manual(values=c(Receita="#2E7D32", Despesa="#C62828")) +
      scale_y_continuous(expand=expansion(mult=c(0,.18)),
                         labels=label_number(big.mark=".", decimal.mark=",")) +
      labs(x=NULL, y="R$", title=paste("Fluxo -", input$filtro_mes)) +
      tema_app
  })
  
  # Grafico categorias
  output$graf_cat <- renderPlot({
    df <- dados_mes() %>% filter(tipo %in% c("Debito","Credito"))
    if (nrow(df)==0) return(NULL)
    df %>%
      group_by(subcategoria) %>%
      summarise(total=sum(valor_pessoal), .groups="drop") %>%
      arrange(total) %>%
      mutate(subcategoria=factor(subcategoria, levels=subcategoria),
             pct=total/sum(total)) %>%
      ggplot(aes(x=subcategoria, y=total, fill=subcategoria)) +
      geom_col(width=0.65, show.legend=FALSE) +
      geom_text(aes(label=paste0(fmt_brl(total),"\n(",
                                 scales::percent(pct,accuracy=1),")")),
                hjust=-0.05, size=2.9, color="#333", lineheight=1.1) +
      scale_fill_manual(values=CORES_SUBCAT) +
      scale_y_continuous(expand=expansion(mult=c(0,.35))) +
      coord_flip() +
      labs(x=NULL, y="R$", title="Despesas por Categoria") +
      tema_app
  })
  
  # Grafico historico
  output$graf_historico <- renderPlot({
    df <- rv$df
    if (nrow(df)==0) return(NULL)
    df %>%
      mutate(
        mes = floor_date(vencimento,"month"),
        valor_pessoal = if_else(tipo %in% c("Debito","Credito"),
                                valor * (1 - divisao/100),
                                valor)
      ) %>%
      group_by(mes, tipo_grupo=if_else(tipo %in% receitas,"Receita","Despesa")) %>%
      summarise(total=sum(valor_pessoal), .groups="drop") %>%
      ggplot(aes(x=mes, y=total, color=tipo_grupo, group=tipo_grupo)) +
      geom_line(linewidth=1.2) +
      geom_point(size=3) +
      scale_color_manual(values=c(Receita="#2E7D32", Despesa="#C62828"), name=NULL) +
      scale_x_date(date_breaks="1 month", date_labels="%b/%y") +
      scale_y_continuous(labels=label_number(big.mark=".", decimal.mark=",")) +
      labs(x=NULL, y="R$", title="Historico Mensal - Receitas vs Despesas") +
      tema_app + theme(legend.position="top")
  })
  
  # ── Dados do mes selecionado: todas as contas divididas (Debito e Credito) ──
  # Debito filtra pela data da compra; Credito filtra pelo vencimento da fatura
  # (mesmo criterio usado nos relatorios/graficos)
  dados_fatura <- reactive({
    req(input$credito_venc)
    
    base <- rv$df %>%
      filter(tipo %in% c("Debito","Credito"), divisao > 0) %>%
      mutate(mes_ref = if_else(tipo == "Debito",
                               floor_date(data, "month"),
                               floor_date(vencimento, "month")))
    
    if (input$credito_venc == "Todos os meses") {
      base
    } else {
      mes_ano <- as.Date(paste0("01/", input$credito_venc), format="%d/%m/%Y")
      base %>% filter(mes_ref == mes_ano)
    }
  })
  
  # ── Resumo das contas divididas ───────────────────────────
  output$resumo_fatura <- renderUI({
    df <- dados_fatura()
    
    total_dividido <- sum(df$valor, na.rm=TRUE)
    total_ela       <- sum(df$valor * df$divisao / 100, na.rm=TRUE)
    total_seu        <- total_dividido - total_ela
    n_divididas      <- nrow(df)
    
    kpi <- function(label, val, cor, subtexto=NULL) {
      div(class="mb-3",
          tags$small(class="text-muted", label),
          tags$h5(class=paste("fw-bold", cor), fmt_brl(val)),
          if (!is.null(subtexto))
            tags$small(class="text-muted", subtexto)
      )
    }
    
    tagList(
      kpi("Total dividido",   total_dividido, "text-dark"),
      kpi("Sua parte",        total_seu,      "text-primary"),
      hr(),
      div(class="card border-warning mb-2",
          div(class="card-body p-3",
              tags$p(class="text-muted small mb-1", "Namorada te deve"),
              tags$h4(class="fw-bold text-warning mb-0", fmt_brl(total_ela)),
              tags$small(class="text-muted",
                         paste0(n_divididas, " conta(s) dividida(s)"))
          )
      ),
      hr(),
      tags$small(class="text-muted",
                 paste0(nrow(df), " lancamento(s) dividido(s) no mes"))
    )
  })
  
  # ── Tabela de contas divididas ─────────────────────────────
  output$tabela_credito <- renderDT({
    df <- dados_fatura()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensagem="Nenhuma conta dividida neste mes."),
                       rownames=FALSE, options=list(dom="t")))
    }
    
    df %>%
      arrange(data) %>%
      mutate(
        Data        = format(data, "%d/%m/%Y"),
        Vencimento  = if_else(tipo=="Credito", format(vencimento,"%d/%m/%Y"), "-"),
        `Valor (R$)`= fmt_brl(valor),
        `% Dela`    = paste0(divisao, "%"),
        `Ela paga`  = fmt_brl(valor * divisao / 100),
        `Voce paga` = fmt_brl(valor * (1 - divisao/100))
      ) %>%
      select(Data, Descricao=descricao, Tipo=tipo, Subcategoria=subcategoria,
             `Valor (R$)`, `% Dela`, `Ela paga`, `Voce paga`, Vencimento) %>%
      datatable(
        rownames = FALSE,
        options  = list(dom="tp", pageLength=20,
                        language=list(
                          paginate=list(previous="Ant", `next`="Pro"),
                          info="Mostrando _START_ a _END_ de _TOTAL_")),
        class = "stripe hover"
      )
  })
}