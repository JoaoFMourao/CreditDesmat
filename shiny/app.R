# =============================================================================
# shiny/app.R  -- Dashboard CreditDesmat
# -----------------------------------------------------------------------------
# Dashboard simples para explorar a base agregada gerada em
# r2c/4_municipal_long_output.R.
#
# Funcionalidade
# --------------
# * Eixo X fixo em ano_safra (serie temporal)
# * Eixo Y: valor de credito a precos de dez/2025 (default), credito nominal,
#           numero de operacoes ou desmat (proxy com sobreposicao)
# * Modo: ABSOLUTO ou PERCENTUAL DO TOTAL DA SAFRA
# * Filtros: UF, municipio, bioma, status_car, faixa_mf, tipo_pessoa,
#            fonte de recurso, programa, subprograma, modalidade,
#            programa_fonte
# * Quebra de linhas: escolha qual dimensao gera multiplas series no grafico
#                     (ex: uma linha por status_car ou por bioma)
#
# Como rodar
# ----------
#   setwd("<repo>")
#   shiny::runApp("shiny")
#
# Input
# -----
# Le diretamente do CSV gerado pelo passo 4:
#   <root>/output/long/credit_long_municipal.csv
# (root padrao: C:/Users/<USER>/Documents/baseMCR/dados;
#  pode ser sobrescrito via variavel de ambiente CREDITDESMAT_ROOT)
# =============================================================================

library(shiny)
library(tidyverse)
library(data.table)
library(scales)
library(bslib)

# ---- localizacao do CSV --------------------------------------------------
root_default <- file.path("C:/Users", Sys.getenv("USERNAME"),
                          "Documents", "baseMCR/dados")
root <- Sys.getenv("CREDITDESMAT_ROOT", unset = root_default)

csv_path <- file.path(root, "output", "long", "credit_long_municipal.csv")

if (!file.exists(csv_path)) {
  stop(
    "Arquivo nao encontrado: ", csv_path,
    "\nRode primeiro o script r2c/4_municipal_long_output.R ou ajuste a ",
    "variavel de ambiente CREDITDESMAT_ROOT."
  )
}

dat <- fread(csv_path)

# ---- helpers de UI -------------------------------------------------------
filter_choices <- function(col) {
  v <- sort(unique(dat[[col]]))
  v <- v[!is.na(v)]
  v
}

metric_choices <- c(
  "Credito real (R$ dez/2025)" = "vl_parc_credito_real",
  "Credito nominal (R$)"        = "vl_parc_credito",
  "Numero de operacoes"         = "n_ops",
  "Desmat - proxy c/ sobrep. (ha)" = "desmat_ha_proxy"
)

dim_choices <- c(
  "Sem quebra (uma linha)" = "_none_",
  "Status CAR"             = "status_car",
  "Bioma dominante"        = "biome_dominante",
  "Faixa de modulos fiscais" = "faixa_mf",
  "Tipo de pessoa"         = "cd_tipo_pessoa",
  "Fonte de recurso"       = "cd_fonte_recurso",
  "Programa"               = "cd_programa",
  "Subprograma"            = "cd_subprograma",
  "Modalidade"             = "cd_modalidade",
  "Programa/Fonte"         = "programa_fonte"
)

filter_panel <- function() {
  tagList(
    selectizeInput("status_car", "Status CAR",
                   choices = filter_choices("status_car"),
                   selected = NULL, multiple = TRUE),
    selectizeInput("biome", "Bioma dominante",
                   choices = filter_choices("biome_dominante"),
                   selected = NULL, multiple = TRUE),
    selectizeInput("faixa_mf", "Faixa MF",
                   choices = filter_choices("faixa_mf"),
                   selected = NULL, multiple = TRUE),
    selectizeInput("tipo_pessoa", "Tipo de pessoa (F/J)",
                   choices = filter_choices("cd_tipo_pessoa"),
                   selected = NULL, multiple = TRUE),
    selectizeInput("fonte", "Fonte de recurso",
                   choices = filter_choices("cd_fonte_recurso"),
                   selected = NULL, multiple = TRUE),
    selectizeInput("programa", "Programa",
                   choices = filter_choices("cd_programa"),
                   selected = NULL, multiple = TRUE),
    selectizeInput("subprograma", "Subprograma",
                   choices = filter_choices("cd_subprograma"),
                   selected = NULL, multiple = TRUE),
    selectizeInput("modalidade", "Modalidade",
                   choices = filter_choices("cd_modalidade"),
                   selected = NULL, multiple = TRUE),
    selectizeInput("municipio", "Municipio (codigo IBGE)",
                   choices = filter_choices("cd_municipio_ibge_cc"),
                   selected = NULL, multiple = TRUE,
                   options = list(maxOptions = 6000))
  )
}

# ---- UI ------------------------------------------------------------------
ui <- page_sidebar(
  title = "CreditDesmat - Dashboard",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 320,
    helpText("Filtros aplicados ao output municipal long"),
    selectInput("metric", "Variavel (eixo Y)",
                choices = metric_choices,
                selected = "vl_parc_credito_real"),
    radioButtons("mode", "Modo",
                 choices = c("Absoluto" = "abs",
                             "Percentual do total da safra" = "pct"),
                 selected = "abs"),
    selectInput("dim", "Quebra de linhas",
                choices = dim_choices, selected = "status_car"),
    hr(),
    filter_panel()
  ),

  layout_columns(
    col_widths = c(12),
    card(
      card_header("Serie temporal"),
      plotOutput("plot_main", height = "480px")
    )
  ),

  layout_columns(
    col_widths = c(12),
    card(
      card_header("Tabela agregada"),
      tableOutput("tbl")
    )
  )
)

# ---- SERVER --------------------------------------------------------------
server <- function(input, output, session) {

  filtered <- reactive({
    d <- dat

    if (length(input$status_car))   d <- d[status_car        %in% input$status_car]
    if (length(input$biome))        d <- d[biome_dominante   %in% input$biome]
    if (length(input$faixa_mf))     d <- d[faixa_mf          %in% input$faixa_mf]
    if (length(input$tipo_pessoa))  d <- d[cd_tipo_pessoa    %in% input$tipo_pessoa]
    if (length(input$fonte))        d <- d[cd_fonte_recurso  %in% input$fonte]
    if (length(input$programa))     d <- d[cd_programa       %in% input$programa]
    if (length(input$subprograma))  d <- d[cd_subprograma    %in% input$subprograma]
    if (length(input$modalidade))   d <- d[cd_modalidade     %in% input$modalidade]
    if (length(input$municipio))    d <- d[cd_municipio_ibge_cc %in% input$municipio]

    d
  })

  agg <- reactive({
    d <- filtered()
    metric <- input$metric

    if (input$dim == "_none_") {
      out <- d[, .(value = sum(get(metric), na.rm = TRUE)), by = .(ano_safra)]
      out[, serie := "Total"]
    } else {
      out <- d[, .(value = sum(get(metric), na.rm = TRUE)),
               by = c("ano_safra", input$dim)]
      setnames(out, input$dim, "serie")
      out[, serie := ifelse(is.na(serie) | serie == "", "(NA)", as.character(serie))]
    }

    if (input$mode == "pct") {
      tot <- out[, .(tot = sum(value, na.rm = TRUE)), by = .(ano_safra)]
      out <- merge(out, tot, by = "ano_safra")
      out[, value := ifelse(tot == 0, 0, 100 * value / tot)]
      out[, tot := NULL]
    }

    out[order(ano_safra, serie)]
  })

  output$plot_main <- renderPlot({
    out <- agg()
    if (!nrow(out)) {
      return(ggplot() + labs(title = "Sem dados para os filtros aplicados"))
    }

    is_pct <- input$mode == "pct"
    metric_label <- names(metric_choices)[metric_choices == input$metric]
    y_lab <- if (is_pct) paste0(metric_label, " (% do total)") else metric_label

    g <- ggplot(out,
                aes(x = ano_safra, y = value, color = serie, group = serie)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      labs(x = "Ano safra", y = y_lab, color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")

    if (is_pct) {
      g <- g + scale_y_continuous(labels = label_percent(scale = 1))
    } else if (input$metric %in% c("vl_parc_credito_real", "vl_parc_credito")) {
      g <- g + scale_y_continuous(
        labels = label_number(scale = 1e-9, suffix = " bi", accuracy = 0.1)
      )
    } else {
      g <- g + scale_y_continuous(labels = label_number(big.mark = "."))
    }

    g
  })

  output$tbl <- renderTable({
    out <- agg()
    if (!nrow(out)) return(NULL)
    out_wide <- out %>%
      pivot_wider(names_from = ano_safra, values_from = value)
    out_wide
  })
}

shinyApp(ui, server)
