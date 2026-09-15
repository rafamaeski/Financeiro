## SERVER ##########################################
server <- function(input, output, session) {

  # Verifica login antes de liberar qualquer coisa do app
  res_auth <- secure_server(check_credentials = check_credentials(CREDENCIAIS))

  rv <- reactiveValues(df = carregar_dados(), fixos = carregar_fixos(),
                        investimentos = carregar_investimentos())

  editando_id       <- reactiveVal(NULL)
  editando_fixo_id  <- reactiveVal(NULL)
  editando_invest_id <- reactiveVal(NULL)

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
      filter(floor_date(ate_mes, "month") >= mes_ano,
             floor_date(mes_inicio, "month") <= mes_ano)
    
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

    n_parcelas <- if (input$tipo == "Credito" && !is.null(input$parcelas)) {
      max(1L, as.integer(input$parcelas))
    } else {
      1L
    }

    id_inicial <- if (nrow(rv$df)==0) 1L else max(rv$df$id)+1L

    # Divide o valor total pelas parcelas, ajustando centavos na ultima
    # parcela para que a soma bata exatamente com o valor informado.
    valores_parcela <- if (n_parcelas > 1) {
      base <- round(input$valor / n_parcelas, 2)
      valores <- rep(base, n_parcelas)
      valores[n_parcelas] <- round(input$valor - base * (n_parcelas - 1), 2)
      valores
    } else {
      input$valor
    }

    novo <- tibble(
      id           = seq(id_inicial, length.out = n_parcelas),
      data         = as.Date(input$data),
      descricao    = if (n_parcelas > 1) {
        paste0(trimws(input$descricao), " (", seq_len(n_parcelas), "/", n_parcelas, ")")
      } else {
        trimws(input$descricao)
      },
      categoria    = if (input$tipo %in% receitas) "-" else input$categoria,
      subcategoria = if (input$tipo %in% receitas) "Receita" else input$subcategoria,
      tipo         = input$tipo,
      cartao       = if (input$tipo=="Credito") input$cartao else "-",
      vencimento   = if (input$tipo=="Credito") {
        as.Date(input$vencimento) %m+% months(0:(n_parcelas-1))
      } else {
        as.Date(input$data)
      },
      valor        = as.numeric(valores_parcela),
      origem       = "manual",
      divisao      = if (input$tipo %in% c("Debito","Credito") && isTRUE(input$dividir)) input$divisao_pct else 0
    )
    rv$df <- bind_rows(rv$df, novo)
    salvar_dados(rv$df)
    msg <- if (n_parcelas > 1) {
      paste0("'", trimws(input$descricao), "' adicionado em ", n_parcelas,
             "x de ", fmt_brl(valores_parcela[1]), "!")
    } else {
      paste0("'", trimws(input$descricao), "' adicionado!")
    }
    showNotification(msg, type="message", duration=3)
    updateTextInput(session, "descricao", value="")
    updateNumericInput(session, "valor", value=NA)
    updateNumericInput(session, "parcelas", value=1)
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
      mes_inicio   = as.Date(floor_date(Sys.Date(), "month")),
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

  # Editar fixo: abre modal preenchido com os dados da linha selecionada
  observeEvent(input$editar_fixo, {
    sel <- input$tabela_fixos_rows_selected
    if (is.null(sel) || length(sel)==0) {
      showNotification("Selecione uma linha para editar.", type="warning"); return()
    }
    linha <- rv$fixos %>% slice(sel)
    editando_fixo_id(linha$id)

    showModal(modalDialog(
      title = "Editar lancamento fixo",
      textInput("edit_fixo_descricao", "Descricao", value = linha$descricao),
      selectInput("edit_fixo_tipo", "Tipo",
                  choices = c("Debito","Receita Fixa","Receita Eventual"),
                  selected = linha$tipo),
      conditionalPanel("input.edit_fixo_tipo == 'Debito'",
        selectInput("edit_fixo_categoria", "Categoria", choices=CATEGORIAS,
                    selected = if (linha$categoria %in% CATEGORIAS) linha$categoria else CATEGORIAS[1]),
        uiOutput("edit_fixo_subcategoria_ui")),
      conditionalPanel("input.edit_fixo_tipo == 'Debito'",
        checkboxInput("edit_fixo_dividir", "Dividir com namorada?", value = linha$divisao > 0),
        conditionalPanel("input.edit_fixo_dividir == true",
          sliderInput("edit_fixo_divisao_pct", "% que ela paga", min=5, max=100, step=5,
                      value = if (linha$divisao > 0) linha$divisao else 50, post="%"))),
      numericInput("edit_fixo_dia", "Dia do mes que cai", value = linha$dia, min=1, max=28, step=1),
      dateInput("edit_fixo_ate", "Repetir ate o mes de", value = linha$ate_mes,
                format="mm/yyyy", language="pt-BR"),
      numericInput("edit_fixo_valor", "Valor (R$)", value = linha$valor, min=0.01, step=0.01),
      div(class="alert alert-warning p-2 mt-2", style="font-size:.85rem;",
          "Isso altera a regra do fixo dai pra frente. Lancamentos ja gerados em meses anteriores nao mudam."),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("salvar_edicao_fixo", "Salvar", class="btn-primary")
      )
    ))
  })

  output$edit_fixo_subcategoria_ui <- renderUI({
    req(input$edit_fixo_categoria)
    choices <- if (input$edit_fixo_categoria == "Basico") SUBCATEGORIAS[1:5] else SUBCATEGORIAS[-c(1:5)]
    sel <- isolate({
      linha <- rv$fixos %>% filter(id == editando_fixo_id())
      if (nrow(linha) > 0 && linha$subcategoria[1] %in% choices) linha$subcategoria[1] else choices[1]
    })
    selectInput("edit_fixo_subcategoria", "Subcategoria", choices=choices, selected=sel)
  })

  observeEvent(input$salvar_edicao_fixo, {
    req(editando_fixo_id())
    id_alvo <- editando_fixo_id()
    if (is.na(input$edit_fixo_valor) || input$edit_fixo_valor <= 0) {
      showNotification("Informe um valor maior que zero.", type="warning"); return()
    }
    rv$fixos <- rv$fixos %>%
      mutate(
        descricao = if_else(id == id_alvo, trimws(input$edit_fixo_descricao), descricao),
        categoria = if_else(id == id_alvo,
                             if (input$edit_fixo_tipo %in% receitas) "-" else input$edit_fixo_categoria,
                             categoria),
        subcategoria = if_else(id == id_alvo,
                                if (input$edit_fixo_tipo %in% receitas) "Receita" else
                                  if (is.null(input$edit_fixo_subcategoria)) "Outros" else input$edit_fixo_subcategoria,
                                subcategoria),
        tipo = if_else(id == id_alvo, input$edit_fixo_tipo, tipo),
        dia = if_else(id == id_alvo, as.integer(input$edit_fixo_dia), dia),
        ate_mes = if_else(id == id_alvo, as.Date(floor_date(input$edit_fixo_ate, "month")), ate_mes),
        valor = if_else(id == id_alvo, as.numeric(input$edit_fixo_valor), valor),
        divisao = if_else(id == id_alvo,
                           if (input$edit_fixo_tipo == "Debito" && isTRUE(input$edit_fixo_dividir)) input$edit_fixo_divisao_pct else 0,
                           divisao)
      )
    salvar_fixos(rv$fixos)
    removeModal()
    showNotification("Fixo atualizado!", type="message")
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

  # Editar lancamento: abre modal preenchido com os dados da linha selecionada
  observeEvent(input$editar_sel, {
    sel <- input$tabela_recente_rows_selected
    if (is.null(sel) || length(sel)==0) {
      showNotification("Selecione uma linha para editar.", type="warning"); return()
    }
    linha <- rv$df %>% arrange(desc(data), desc(id)) %>% slice(sel)
    editando_id(linha$id)

    showModal(modalDialog(
      title = "Editar lancamento",
      dateInput("edit_data", "Data", value = linha$data, format="dd/mm/yyyy", language="pt-BR"),
      textInput("edit_descricao", "Descricao", value = linha$descricao),
      selectInput("edit_tipo", "Tipo",
                  choices = c("Debito","Credito","Receita Fixa","Receita Eventual"),
                  selected = linha$tipo),
      conditionalPanel("input.edit_tipo == 'Debito' || input.edit_tipo == 'Credito'",
        selectInput("edit_categoria", "Categoria", choices=CATEGORIAS,
                    selected = if (linha$categoria %in% CATEGORIAS) linha$categoria else CATEGORIAS[1]),
        uiOutput("edit_subcategoria_ui")),
      conditionalPanel("input.edit_tipo == 'Credito'",
        selectInput("edit_cartao", "Tipo de Credito", choices=c("Cartao","Outros"),
                    selected = if (linha$cartao %in% c("Cartao","Outros")) linha$cartao else "Cartao"),
        dateInput("edit_vencimento", "Vencimento da fatura", value = linha$vencimento,
                  format="dd/mm/yyyy", language="pt-BR")),
      conditionalPanel("input.edit_tipo == 'Debito' || input.edit_tipo == 'Credito'",
        checkboxInput("edit_dividir", "Dividir com namorada?", value = linha$divisao > 0),
        conditionalPanel("input.edit_dividir == true",
          sliderInput("edit_divisao_pct", "% que ela paga", min=5, max=100, step=5,
                      value = if (linha$divisao > 0) linha$divisao else 50, post="%"))),
      numericInput("edit_valor", "Valor (R$)", value = linha$valor, min=0.01, step=0.01),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("salvar_edicao", "Salvar", class="btn-primary")
      )
    ))
  })

  output$edit_subcategoria_ui <- renderUI({
    req(input$edit_categoria)
    choices <- if (input$edit_categoria == "Basico") SUBCATEGORIAS[1:5] else SUBCATEGORIAS[-c(1:5)]
    sel <- isolate({
      linha <- rv$df %>% filter(id == editando_id())
      if (nrow(linha) > 0 && linha$subcategoria[1] %in% choices) linha$subcategoria[1] else choices[1]
    })
    selectInput("edit_subcategoria", "Subcategoria", choices=choices, selected=sel)
  })

  observeEvent(input$salvar_edicao, {
    req(editando_id())
    id_alvo <- editando_id()
    if (is.na(input$edit_valor) || input$edit_valor <= 0) {
      showNotification("Informe um valor maior que zero.", type="warning"); return()
    }
    rv$df <- rv$df %>%
      mutate(
        data = if_else(id == id_alvo, as.Date(input$edit_data), data),
        descricao = if_else(id == id_alvo, trimws(input$edit_descricao), descricao),
        categoria = if_else(id == id_alvo,
                             if (input$edit_tipo %in% receitas) "-" else input$edit_categoria,
                             categoria),
        subcategoria = if_else(id == id_alvo,
                                if (input$edit_tipo %in% receitas) "Receita" else input$edit_subcategoria,
                                subcategoria),
        tipo = if_else(id == id_alvo, input$edit_tipo, tipo),
        cartao = if_else(id == id_alvo,
                          if (input$edit_tipo == "Credito") input$edit_cartao else "-",
                          cartao),
        vencimento = if_else(id == id_alvo,
                              if (input$edit_tipo == "Credito") as.Date(input$edit_vencimento) else as.Date(input$edit_data),
                              vencimento),
        valor = if_else(id == id_alvo, as.numeric(input$edit_valor), valor),
        divisao = if_else(id == id_alvo,
                           if (input$edit_tipo %in% c("Debito","Credito") && isTRUE(input$edit_dividir)) input$edit_divisao_pct else 0,
                           divisao)
      )
    salvar_dados(rv$df)
    removeModal()
    showNotification("Lancamento atualizado!", type="message")
  })
  
  # Tabela recente
  output$tabela_recente <- renderDT({
    rv$df %>%
      arrange(desc(data), desc(id)) %>%
      mutate(
        DataOrd   = as.numeric(data),
        Data      = format(data, "%d/%m/%Y"),
        Vencimento = if_else(tipo=="Credito", format(vencimento,"%d/%m/%Y"), "-"),
        Origem    = if_else(origem=="fixo", "Auto", "Manual"),
        Divisao   = if_else(divisao > 0, paste0(divisao, "%"), "-"),
        Valor     = fmt_brl(valor)
      ) %>%
      select(Data, Descricao=descricao, Subcategoria=subcategoria,
             Tipo=tipo, Origem, `Valor (R$)`=Valor, Vencimento, `% Dela`=Divisao, DataOrd) %>%
      datatable(selection="single", rownames=FALSE,
                options=list(dom="ftp", pageLength=10,
                             columnDefs=list(
                               list(visible=FALSE, targets=8),
                               list(orderData=8, targets=0)
                             ),
                             language=list(
                               search="Buscar:",
                               searchPlaceholder="Descricao, categoria, etc.",
                               paginate=list(previous="Ant", `next`="Pro"),
                               info="Mostrando _START_ a _END_ de _TOTAL_")))
  })
  
  # Tabela fixos
  output$tabela_fixos <- renderDT({
    rv$fixos %>%
      mutate(`Dia do mes`=dia, `Desde`=format(mes_inicio,"%m/%Y"),
             `Ate`=format(ate_mes,"%m/%Y"), Valor=fmt_brl(valor),
             `% Dela`=if_else(divisao > 0, paste0(divisao, "%"), "-")) %>%
      select(Descricao=descricao, Subcategoria=subcategoria,
             Tipo=tipo, `Dia do mes`, `Desde`, `Ate`, Valor, `% Dela`) %>%
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
    sal_mes <- rec - des

    # Saldo acumulado: soma receitas - despesas de todos os meses ate o mes selecionado (inclusive)
    mes_ano <- as.Date(paste0("01/", input$filtro_mes), format="%d/%m/%Y")
    historico <- rv$df %>%
      filter(floor_date(vencimento, "month") <= mes_ano) %>%
      mutate(
        valor_pessoal = if_else(tipo %in% c("Debito","Credito"),
                                 valor * (1 - divisao/100),
                                 valor)
      )
    rec_acum <- sum(historico$valor[historico$tipo %in% receitas], na.rm=TRUE)
    des_acum <- sum(historico$valor_pessoal[historico$tipo %in% c("Debito","Credito")], na.rm=TRUE)
    sal_acum <- rec_acum - des_acum

    kpi <- function(label, val, cor, subtexto=NULL) {
      div(class="mb-3",
          tags$small(class="text-muted", label),
          tags$h5(class=paste("fw-bold", cor), fmt_brl(val)),
          if (!is.null(subtexto)) tags$small(class="text-muted", subtexto))
    }
    tagList(
      kpi("Receitas do mes", rec, "text-success"),
      kpi("Despesas do mes", des, "text-danger"),
      kpi("Saldo do mes", sal_mes, if(sal_mes>=0) "text-success" else "text-danger"),
      hr(),
      kpi("Saldo acumulado", sal_acum, if(sal_acum>=0) "text-success" else "text-danger",
          paste0("Somando todos os meses ate ", input$filtro_mes)),
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
      scale_fill_manual(values=c(Receita=COR_RECEITA, Despesa=COR_DESPESA)) +
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
      scale_color_manual(values=c(Receita=COR_RECEITA, Despesa=COR_DESPESA), name=NULL) +
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

  # ── Investimentos ──────────────────────────────────────────

  observeEvent(input$adicionar_investimento, {
    req(input$invest_descricao, input$invest_aportado)
    if (is.na(input$invest_aportado) || input$invest_aportado <= 0) {
      showNotification("Informe um valor aportado maior que zero.", type="warning"); return()
    }
    valor_atual <- if (is.na(input$invest_atual) || input$invest_atual <= 0) {
      input$invest_aportado
    } else {
      input$invest_atual
    }

    novo_invest <- tibble(
      id = if (nrow(rv$investimentos)==0) 1L else max(rv$investimentos$id)+1L,
      data = as.Date(input$invest_data),
      descricao = trimws(input$invest_descricao),
      tipo = input$invest_tipo,
      valor_aportado = as.numeric(input$invest_aportado),
      valor_atual = as.numeric(valor_atual)
    )
    rv$investimentos <- bind_rows(rv$investimentos, novo_invest)
    salvar_investimentos(rv$investimentos)
    showNotification(paste0("'", novo_invest$descricao, "' adicionado aos investimentos!"),
                      type="message", duration=3)
    updateTextInput(session, "invest_descricao", value="")
    updateNumericInput(session, "invest_aportado", value=NA)
    updateNumericInput(session, "invest_atual", value=NA)
  })

  observeEvent(input$excluir_investimento, {
    sel <- input$tabela_investimentos_rows_selected
    if (is.null(sel) || length(sel)==0) {
      showNotification("Selecione uma linha para excluir.", type="warning"); return()
    }
    ids_excluir <- rv$investimentos %>% arrange(desc(data)) %>% slice(sel) %>% pull(id)
    rv$investimentos <- filter(rv$investimentos, !id %in% ids_excluir)
    salvar_investimentos(rv$investimentos)
    showNotification("Investimento excluido.", type="message")
  })

  observeEvent(input$editar_investimento, {
    sel <- input$tabela_investimentos_rows_selected
    if (is.null(sel) || length(sel)==0) {
      showNotification("Selecione uma linha para editar.", type="warning"); return()
    }
    linha <- rv$investimentos %>% arrange(desc(data)) %>% slice(sel)
    editando_invest_id(linha$id)

    showModal(modalDialog(
      title = "Editar investimento",
      dateInput("edit_invest_data", "Data do aporte", value = linha$data,
                format="dd/mm/yyyy", language="pt-BR"),
      textInput("edit_invest_descricao", "Descricao", value = linha$descricao),
      selectInput("edit_invest_tipo", "Tipo",
                  choices = c("Renda Fixa","Fundos","Acoes","Cripto","Outros"),
                  selected = linha$tipo),
      numericInput("edit_invest_aportado", "Valor aportado (R$)",
                   value = linha$valor_aportado, min=0.01, step=0.01),
      numericInput("edit_invest_atual", "Valor atual (R$)",
                   value = linha$valor_atual, min=0.01, step=0.01),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("salvar_edicao_investimento", "Salvar", class="btn-primary")
      )
    ))
  })

  observeEvent(input$salvar_edicao_investimento, {
    req(editando_invest_id())
    id_alvo <- editando_invest_id()
    if (is.na(input$edit_invest_aportado) || input$edit_invest_aportado <= 0) {
      showNotification("Informe um valor aportado maior que zero.", type="warning"); return()
    }
    rv$investimentos <- rv$investimentos %>%
      mutate(
        data = if_else(id == id_alvo, as.Date(input$edit_invest_data), data),
        descricao = if_else(id == id_alvo, trimws(input$edit_invest_descricao), descricao),
        tipo = if_else(id == id_alvo, input$edit_invest_tipo, tipo),
        valor_aportado = if_else(id == id_alvo, as.numeric(input$edit_invest_aportado), valor_aportado),
        valor_atual = if_else(id == id_alvo, as.numeric(input$edit_invest_atual), valor_atual)
      )
    salvar_investimentos(rv$investimentos)
    removeModal()
    showNotification("Investimento atualizado!", type="message")
  })

  output$tabela_investimentos <- renderDT({
    rv$investimentos %>%
      arrange(desc(data)) %>%
      mutate(
        Data = format(data, "%d/%m/%Y"),
        `Aportado (R$)` = fmt_brl(valor_aportado),
        `Atual (R$)` = fmt_brl(valor_atual),
        Rentabilidade = paste0(round((valor_atual/valor_aportado - 1) * 100, 1), "%")
      ) %>%
      select(Data, Descricao=descricao, Tipo=tipo,
             `Aportado (R$)`, `Atual (R$)`, Rentabilidade) %>%
      datatable(selection="single", rownames=FALSE,
                options=list(dom="tp", pageLength=15,
                             language=list(
                               paginate=list(previous="Ant", `next`="Pro"),
                               info="Mostrando _START_ a _END_ de _TOTAL_")))
  })

  output$graf_investimentos <- renderPlot({
    df <- rv$investimentos
    if (nrow(df) == 0) return(NULL)

    df %>%
      group_by(tipo) %>%
      summarise(total = sum(valor_aportado), .groups="drop") %>%
      mutate(pct = total / sum(total)) %>%
      ggplot(aes(x="", y=total, fill=tipo)) +
      geom_col(width=1, color="white") +
      coord_polar("y") +
      geom_text(aes(label=paste0(scales::percent(pct, accuracy=1))),
                position=position_stack(vjust=0.5), size=4.5, color="#2c3e50", fontface="bold") +
      scale_fill_manual(values=CORES_INVEST, name="Tipo") +
      labs(title=paste0("Total investido: ", fmt_brl(sum(df$valor_aportado, na.rm=TRUE)))) +
      theme_void() +
      theme(
        plot.title = element_text(face="bold", size=13, color="#2c3e50", hjust=0.5),
        legend.position = "bottom",
        legend.title = element_text(face="bold")
      )
  })
}