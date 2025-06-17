library(shiny)
library(tidyverse)
library(plotly)
library(vroom)
library(MetBrewer)


# Auxiliary functions -------------------------------------------------------------------------

source("graf_fun.r")


# Palette -------------------------------------------------------------------------------------

pal <- met.brewer('Hiroshige', 5)[1:3]
pal2 <- c(met.brewer('Hiroshige', 7)[1], pal)


# Reading data --------------------------------------------------------------------------------

observed <- vroom::vroom("data/observed.csv.gz")
df.prob.22_23 <- read_csv(file = "samples/macro.prob.22_23.csv.gz")
df.prob.23_24 <- read_csv(file = "samples/macro.prob.23_24.csv.gz")


# Datasets for Brazil, UFs and Health Regions -------------------------------------------------

t1br <- vroom::vroom("data/t1br.csv.gz")
t2br <- vroom::vroom("data/t2br.csv.gz")

t1uf <- vroom::vroom("data/t1uf.csv.gz")
t2uf <- vroom::vroom("data/t2uf.csv.gz")

t1hd <- vroom::vroom("data/t1hd.csv.gz")
t2hd <- vroom::vroom("data/t2hd.csv.gz")


# UI ------------------------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Dashboard - Freitas et al. 2025"),
  
  tabsetPanel(
    
    tabPanel("Brasil",
             column(
               width = 6,
               plotlyOutput("plotBR1", height = "600px")
             ),
             column(
               width = 6,
               plotlyOutput("plotBR2", height = "600px")
             )
    ),
    
    tabPanel("UF",
             selectInput(
               inputId = "uf", 
               label = "Escolha a UF:", 
               choices = sort(unique(observed$uf)), 
               selected = sort(unique(observed$uf))[1]
             ),
             fluidRow(
               column(width = 6,
                      plotlyOutput("plotUF1", height = "600px")
               ),
               column(width = 6,
                      plotlyOutput("plotUF2", height = "600px")
               )
             )
    ),
    
    tabPanel("Macrorregião",
             selectInput(
               inputId = "uf_macro",
               label   = "Escolha a UF:",
               choices = sort(unique(observed$uf)),
               selected = sort(unique(observed$uf))[1]
             ),
             plotlyOutput("plotMacro", height = "600px")
    )
  )
)


# SERVER --------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # 1) Brasil
  output$plotBR1 <- renderPlotly({
    plot_grafico_artigo(t1br, pal2)
  })
  
  output$plotBR2 <- renderPlotly({
    plot_grafico_artigo(t2br, pal2)
  })
  
    # 2) UF
  output$plotUF1 <- renderPlotly({
    uf <- input$uf
    plot_grafico_artigo_uf(t1uf, uf, pal2)
  })
  
  output$plotUF2 <- renderPlotly({
    uf <- input$uf
    plot_grafico_artigo_uf(t2uf, uf, pal2)
  })
  
  # 3) Macrorregião
  output$plotMacro <- renderPlotly({
    uf_macro <- input$uf_macro
    plot_grafico_artigo_macro_por_uf(t1hd, uf_macro, pal2)
  })
  
}


# RUN APP -------------------------------------------------------------------------------------

shinyApp(ui, server)
