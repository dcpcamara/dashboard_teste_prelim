library(shiny)
library(tidyr)
library(dplyr)
library(readr)
library(plotly)
library(rio)
library(MetBrewer)
library(leaflet)
library(sf)


# Auxiliary functions -------------------------------------------------------------------------

# Plotly for Country --------------------------------------------------------------------------

plot_grafico_artigo <- function(df, palette) {
  
  quants <- rev(unique(df$quantile))
  colors <- palette[seq_along(quants)]
  
  max_valor <- max(df$maxvalues[is.finite(df$maxvalues)], df$cases, na.rm = TRUE)
  df_max_round <- ceiling(max_valor / 50) * 50
  df_max_round2 <- df_max_round + (df_max_round * 0.1)
  df2 <- df |> mutate(maxvalues = ifelse(is.infinite(maxvalues), df_max_round2, maxvalues))
  
  fig <- plot_ly()
  for(i in seq_along(quants)) {
    fig <- fig |> 
      add_trace(
        data = df2 |> filter(quantile == quants[i]),
        type = 'scatter', 
        mode = 'lines',
        x = ~date, 
        y = ~maxvalues,
        text = ~message,
        fill = 'tozeroy',
        line = list(color = colors[i]),
        fillcolor = colors[i],
        name = quants[i],
        hovertemplate = paste0("<b>", quants[i],"</b>: %{text}<extra></extra>")
      )
  }
  
  fig |>
    add_trace(
      data = df2,
      type = 'scatter', mode = 'lines',
      x = ~date, y = ~cases,
      line = list(color = "black"),
      name = "Cases",
      hovertemplate = "<b>Cases</b>: %{y:,.0f}<extra></extra>"
    ) |>
    layout(
      hovermode = "x unified",
      xaxis = list(title = "Date", showgrid = FALSE, zeroline = FALSE),
      yaxis = list(title = "Dengue cases", showgrid = FALSE, zeroline = FALSE),
      legend = list(itemclick = FALSE, itemdoubleclick = FALSE)
    )
}


# Plotly for UF -------------------------------------------------------------------------------

plot_grafico_artigo_uf <- function(df, UF, palette) {
  
  quants <- rev(unique(df$quantile))
  colors <- palette[seq_along(quants)]
  
  max_valor <- df |> filter(uf == UF, is.finite(df$maxvalues)) |> select(maxvalues, cases) |> max(na.rm = TRUE)
  df_max_round <- ceiling(max_valor / 50) * 50
  df_max_round2 <- df_max_round + (df_max_round * 0.1)
  df2 <- df |> filter(uf == UF) |> mutate(maxvalues = ifelse(is.infinite(maxvalues), df_max_round2, maxvalues))
  
  fig <- plot_ly()
  for(i in seq_along(quants)) {
    fig <- fig |> 
      add_trace(
        data = df2 |> filter(quantile == quants[i]),
        type = 'scatter', 
        mode = 'lines',
        x = ~date, 
        y = ~maxvalues,
        text = ~message,
        fill = 'tozeroy',
        line = list(color = colors[i]),
        fillcolor = colors[i],
        name = quants[i],
        hovertemplate = paste0("<b>", quants[i],"</b>: %{text}<extra></extra>")
      )
  }
  
  fig |>
    add_trace(
      data = df2,
      type = 'scatter', mode = 'lines',
      x = ~date, y = ~cases,
      line = list(color = "black"),
      name = "Cases",
      hovertemplate = "<b>Cases</b>: %{y:,.0f}<extra></extra>"
    ) |>
    layout(
      hovermode = "x unified",
      xaxis = list(title = "Date", showgrid = FALSE, zeroline = FALSE),
      yaxis = list(title = "Dengue cases", showgrid = FALSE, zeroline = FALSE),
      legend = list(itemclick = FALSE, itemdoubleclick = FALSE)
    )
}


# Plotly for HD -------------------------------------------------------------------------------

plot_grafico_artigo_macro_por_uf <- function(df, UF, palette, height_per_plot = 300) {
  
  df_uf <- df |> filter(uf == UF, macroregional != "")
  regs <- sort(unique(df_uf$macroregional))
  n_regs <- length(regs)
  
  plots <- vector("list", n_regs)
  for (j in seq_along(regs)) {
    reg_j <- regs[j]
    df_reg <- df_uf |> filter(macroregional == reg_j)
    
    quants <- rev(unique(df_reg$quantile))
    colors <- palette[seq_along(quants)]
    
    max_valor <- df_reg |>
      filter(is.finite(maxvalues)) |>
      summarise(m = max(maxvalues, cases, na.rm = TRUE)) |>
      pull(m)
    df_max_rnd2 <- ceiling(max_valor / 50) * 50 * 1.1
    
    df2 <- df_reg |>
      mutate(maxvalues = ifelse(is.infinite(maxvalues), df_max_rnd2, maxvalues))
    
    fig_i <- plot_ly()
    for (i in seq_along(quants)) {
      fig_i <- fig_i |>
        add_trace(
          data = df2 |> filter(quantile == quants[i]),
          type = 'scatter',
          mode = 'lines',
          x = ~date,
          y = ~maxvalues,
          fill = 'tozeroy',
          line = list(color = colors[i]),
          fillcolor = colors[i],
          name = quants[i],
          hovertemplate= paste0("<b>", quants[i], "</b>: %{y:,.0f}<extra></extra>")
        )
    }
    
    fig_i <- fig_i |>
      add_trace(
        data = df2,
        type = 'scatter',
        mode = 'lines',
        x = ~date,
        y = ~cases,
        line = list(color = "black"),
        name = "Cases",
        hovertemplate= "<b>Cases</b>: %{y:,.0f}<extra></extra>"
      ) |>
      layout(
        margin = list(t = 20, b = 20, l = 20, r = 20),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, title = NULL),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, title = NULL),
        showlegend = FALSE
      )
    
    plots[[j]] <- fig_i
  }
  
  ncol <- min(2, n_regs)
  nrow <- ceiling(n_regs / ncol)
  
  p <- subplot(
    plotlist = plots,
    nrows = nrow,
    margin = 0.02,
    shareX = TRUE,
    shareY = FALSE,
    titleX = FALSE,
    titleY = FALSE
  )
  
  title_hd <- lapply(seq_along(regs), function(i) {
    coli <- (i - 1) %% ncol
    rowi <- floor((i - 1) / ncol)
    xc <- (coli + 0.5) / ncol
    y1 <- 1 - (rowi / nrow)
    list(
      x = xc,
      y = y1 - 0.01,
      xref = "paper",
      yref = "paper",
      text = regs[i],
      showarrow = FALSE,
      xanchor = "center",
      yanchor = "bottom",
      font = list(size = 12)
    )
  })
  
  total_height <- height_per_plot * nrow
  
  p |>
    layout(
      height = total_height,
      hovermode = "x unified",
      annotations = title_hd
    )
}


# Hover message for plotly --------------------------------------------------------------------

message_hover <- function(df){
  df |> 
    mutate(
      message = case_when(
        quantile == "Below the median,\ntypical" ~ paste0("<", format(round(maxvalues), 
                                                                      nsmall=0, 
                                                                      big.mark=",")),
        quantile == "Moderately high,\nfairly typical" ~ paste0(">=", format(round(minvalues), 
                                                                             nsmall=0, 
                                                                             big.mark=","),
                                                                " and <", format(round(maxvalues), 
                                                                                 nsmall=0, 
                                                                                 big.mark=",")),
        quantile == "Fairly high,\natypical" ~ paste0(">=", format(round(minvalues), 
                                                                   nsmall=0, 
                                                                   big.mark=","),
                                                      " and <", format(round(maxvalues), 
                                                                       nsmall=0, 
                                                                       big.mark=",")),
        quantile == "Exceptionally high,\nvery atypical" ~ paste0(">=", format(round(minvalues), 
                                                                               nsmall=0, 
                                                                               big.mark=","))
      )
    )
}


# Reading data --------------------------------------------------------------------------------

observed <- import("data/observed.csv.gz")
df.prob.22_23 <- import(file = "samples/macro.prob.22_23.csv.gz")
df.prob.23_24 <- import(file = "samples/macro.prob.23_24.csv.gz")
df.prob.24_25 <- import(file = "samples/macro.prob.24_25.csv.gz")
shape <- read_sf("data/shape_macro_simp.gpkg")
ufshape <- read_sf("data/shape_uf_simp.gpkg")
uf_labels <- readRDS("data/uf_labels.rds")


# Datasets for Brazil, UFs and Health Regions -------------------------------------------------

t1br <- import("data/t1br.csv.gz") |> 
  message_hover()
t2br <- import("data/t2br.csv.gz") |> 
  message_hover()
t3br <- import("data/t3br.csv.gz") |> 
  message_hover()

t1uf <- import("data/t1uf.csv.gz") |> 
  message_hover()
t2uf <- import("data/t2uf.csv.gz") |> 
  message_hover()
t3uf <- import("data/t3uf.csv.gz") |> 
  message_hover()

t1hd <- import("data/t1hd.csv.gz") |> 
  message_hover()
t2hd <- import("data/t2hd.csv.gz") |> 
  message_hover()
t3hd <- import("data/t3hd.csv.gz") |> 
  message_hover()


# Color palettes ------------------------------------------------------------------------------

region_colors <- c(
  "North" = "#3CAACF",
  "Northeast" = "#D4EC88",
  "Mid-West" = "#66C07E",
  "Southeast" = "#A383D9",
  "South" = "#DC8E4B"
)

# pal <- met.brewer('Hiroshige', 5)[1:3]
# pal2 <- c(met.brewer('Hiroshige', 7)[1], pal)
pal <- colorFactor(palette = region_colors, domain = shape$region_en)
pal3 <- c("#EE9188","#F4AD7F","#F9D894","#C4E7E9")



# UI ------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    # garante que o dropdown do input select fique por cima do controle de zoom do leaflet
    tags$style(HTML("
      .selectize-dropdown {
        z-index: 10000 !important;
      }
    "))
  ),
  
  titlePanel("A statistical model for forecasting probabilistic epidemic bands for dengue cases in Brazil"),
  
  tags$h5(
    "Available on ",
    tags$a(
      "https://doi.org/10.1101/2025.06.12.25329525",
      href = "https://doi.org/10.1101/2025.06.12.25329525",
      target = "_blank"
    )
  ),
  
  tabsetPanel(
    # 1) Publication
    tabPanel("Publication",
             tags$h3("A statistical model for forecasting probabilistic epidemic bands for dengue cases in Brazil"),
             tags$p(
               "Authors: ",
               "Laís Picinini Freitas", tags$sup("1"), ", ",
               "Danielle Andreza da Cruz Ferreira", tags$sup("2"), ", ",
               "Raquel Martins Lana", tags$sup("3"), ", ",
               "Daniel Cardoso Portela Câmara", tags$sup("1"), ", ",
               "Tatiana P. Portella", tags$sup("1"), ", ",
               "Marilia Sá Carvalho", tags$sup("1"), ", ",
               "Ayrton Sena Gouveia", tags$sup("4"), ", ",
               "Iasmim Ferreira de Almeida", tags$sup("4"), ", ",
               "Eduardo Correa Araujo", tags$sup("5"), ", ",
               "Luã Bida Vacaro", tags$sup("5"), ", ",
               "Fabiana Ganem", tags$sup("5"), ", ",
               "Oswaldo Gonçalves Cruz", tags$sup("1"), ", ",
               "Flávio Codeço Coelho", tags$sup("5"), ", ",
               "Claudia Torres Codeço", tags$sup("1"), ", ",
               "Luiz Max Carvalho", tags$sup("5"), " and ",
               "Leonardo Soares Bastosa", tags$sup("1")
             ),
             tags$p(tags$sup("1"), "Scientific Computing Program, Oswaldo Cruz Foundation, Rio de Janeiro, RJ, Brazil"),
             tags$p(tags$sup("2"), "Federal University of Minas Gerais, Belo Horizonte, MG, Brazil"),
             tags$p(tags$sup("3"), "Barcelona Supercomputing Center (BSC), Barcelona, Spain"),
             tags$p(tags$sup("4"), "Graduate Program in Epidemiology in Public Health, Sergio Arouca National School of Public Health, Oswaldo Cruz Foundation, Rio de Janeiro, RJ, Brazil"),
             tags$p(tags$sup("5"), "Applied Mathematics School, Getulio Vargas Foundation, Rio de Janeiro, RJ, Brazil"),
             tags$p(
               "Available on ",
               tags$a(
                 href = "https://doi.org/10.1101/2025.06.12.25329525",
                 target = "_blank",
                 "https://doi.org/10.1101/2025.06.12.25329525"
               )
             )
    ),
    
    # 2) Brazil
    tabPanel("Brazil",
             fluidRow(
               column(6, plotlyOutput("plotBR1", width = "100%", height = "600px")),
               column(6, plotlyOutput("plotBR2", width = "100%", height = "600px")),
               column(6, plotlyOutput("plotBR3", width = "100%", height = "600px"))
             )
    ),
    
    # 3) Federal Units (states)
    tabPanel("Federal Units (states)",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput(
                   inputId = "uf",
                   label = "Choose the Federation Unit:",
                   choices = sort(unique(observed$uf)),
                   selected = sort(unique(observed$uf))[1]
                 ),
                 leafletOutput("mapInsertUF", width = "100%", height = "300px")
               ),
               mainPanel(
                 width = 9,
                 plotlyOutput("plotUF1", width = "100%", height = "600px"),
                 plotlyOutput("plotUF2", width = "100%", height = "600px"),
                 plotlyOutput("plotUF3", width = "100%", height = "600px")
               )
             )
    ),
    
    # 4) Health Districts
    tabPanel("Health Districts",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput(
                   inputId = "uf_macro",
                   label = "Choose the Federation Unit:",
                   choices = sort(unique(observed$uf)),
                   selected = sort(unique(observed$uf))[1]
                 ),
                 radioButtons(
                   inputId = "season_macro",
                   label = "Choose the time period:",
                   choices = c(
                     "2022–2023" = "22_23",
                     "2023–2024" = "23_24",
                     "2024–2025" = "24_25"
                   ),
                   selected = "22_23",
                   inline = TRUE
                 ),
                 leafletOutput("mapInsertMacro", width = "100%", height = "300px")
               ),
               mainPanel(
                 width = 9,
                 uiOutput("ui_macro_plot")
               )
             )
    )
    
  ), # end tabsetPanel
  
  tags$hr(),
  tags$footer(
    style = "width:100%;text-align:center;font-size:0.8em;color:#666;padding:5px 0;",
    "Source: Picinini Freitas, L. et al (2025). ",
    tags$a(
      href = "https://doi.org/10.1101/2025.06.12.25329525",
      target = "_blank",
      "https://doi.org/10.1101/2025.06.12.25329525"
    )
  )
)



# SERVER --------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # 1) Brasil
  output$plotBR1 <- renderPlotly({
    plot_grafico_artigo(t1br, pal3)
  })
  
  output$plotBR2 <- renderPlotly({
    plot_grafico_artigo(t2br, pal3)
  })
  
  output$plotBR3 <- renderPlotly({
    plot_grafico_artigo(t3br, pal3)
  })
  
  # 2) UF
  output$plotUF1 <- renderPlotly({
    uf <- input$uf
    plot_grafico_artigo_uf(t1uf, uf, pal3)
  })
  
  output$plotUF2 <- renderPlotly({
    uf <- input$uf
    plot_grafico_artigo_uf(t2uf, uf, pal3)
  })
  
  output$plotUF3 <- renderPlotly({
    uf <- input$uf
    plot_grafico_artigo_uf(t3uf, uf, pal3)
  })
  
  output$mapInsertUF <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(data = shape,
                  color = "white",
                  weight = 1,
                  fillColor = ~pal(region_en),
                  fillOpacity = 0.7,
                  label = ~paste("Health district:", macroregional, "/", uf_name)
      ) |>
      addPolygons(data = ufshape,
                  fill = FALSE,
                  color = "black",
                  weight = 2
      ) |>
      addLabelOnlyMarkers(data = uf_labels,
                          lng = ~X, lat = ~Y,
                          label = ~abbrev_state,
                          labelOptions = labelOptions(
                            noHide = TRUE,
                            direction = "center",
                            textOnly = TRUE,
                            style = list(
                              "color" = "black",
                              "font-size" = "12px",
                              "font-weight" = "bold"
                            )
                          ))
  })
  
  # 3) Macrorregião
  output$plotMacro1 <- renderPlotly({
    req(input$uf_macro)
    plot_grafico_artigo_macro_por_uf(t1hd, input$uf_macro, pal3, height_per_plot = 400)
  })
  output$plotMacro2 <- renderPlotly({
    req(input$uf_macro)
    plot_grafico_artigo_macro_por_uf(t2hd, input$uf_macro, pal3, height_per_plot = 400)
  })
  output$plotMacro3 <- renderPlotly({
    req(input$uf_macro)
    plot_grafico_artigo_macro_por_uf(t3hd, input$uf_macro, pal3, height_per_plot = 400)
  })
  
  output$ui_macro_plot <- renderUI({
    req(input$season_macro)
    
    plot_id <- switch(
      input$season_macro,
      "22_23" = "plotMacro1",
      "23_24" = "plotMacro2",
      "24_25" = "plotMacro3"
    )
    
    plotlyOutput(plot_id, height = "600px")
  })
  
  output$mapInsertMacro <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        data = shape,
        color = "white",
        weight = 1,
        fillColor = ~pal(region_en),
        fillOpacity = 0.7,
        label = ~paste("Health district:", macroregional, "/", uf_name)
      ) |>
      addPolygons(
        data = ufshape,
        fill = FALSE,
        color = "black",
        weight = 2
      ) |>
      addLabelOnlyMarkers(
        data = uf_labels,
        lng = ~X,
        lat = ~Y,
        label = ~abbrev_state,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "color" = "black",
            "font-size" = "12px",
            "font-weight" = "bold"
          )
        )
      )
  })
}



# RUN APP -------------------------------------------------------------------------------------

shinyApp(ui, server)
