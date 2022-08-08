


library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(shiny)
library(tidyr)



ubs <- readr::read_rds(here::here("ubs_completa_final.rds"))
municipios <- geobr::read_municipality()

ui <- navbarPage(
  title = "Unidades Básicas de Saúde - UBS",
  theme = shinythemes::shinytheme("united"),
  tabPanel(
    title = "Leia-me",
    h2("Informações Gerais"),
    HTML("<p>O objetivo desse dashboard é servir como trabalho de conclusão do curso
          de Dashboards da <a href='https://curso-r.com/'>Curso-R</a>. O conjunto de dados
          escolhido é um que trata sobre as Unidades Básicas de Saúde (UBS) distribuídas pelo
          Brasil. Esse material se encontra disponível no site
          <a href='https://dados.gov.br/dataset/unidades-basicas-de-saude-ubs'>
          dados.gov.br</a> do governo federal.<p>"),
    HTML("<p>A ideia é poder visualizar a localização das UBS ao redor do Brasil, utilizar um
          buscador com os dados coletados, filtrando por Estado e Município e verificar
          quais municípios em cada Estado possuem mais unidades de atendimento. Cada uma dessas
          atividades podem ser feitas nas abas 'Mapa UBS', 'Busca UBS' e 'Por Estado', respectivamente.</p>")
  ),
  tabPanel(
    title = "Mapa UBS",
    leafletOutput(outputId = "mapa", height = 570)
  ),
  tabPanel(
    title = "Busca UBS",
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = "estado_1",
          label = "Escolha um Estado:",
          choices = sort(unique(ubs$nome_uf)),
        )
      ),
      column(
        width = 4,
        selectInput(
          inputId = "municipio_1",
          label = "Escolha um Município:",
          choices = sort(unique(ubs$nome_municipio)),
        )
      )
    ),
    dataTableOutput(outputId = "dados")
  ),
  tabPanel(
    title = "Por Estado",
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = "estado_2",
          label = "Escolha um Estado:",
          choices = sort(unique(municipios$abbrev_state)),
        )
      ),
    ),
    fluidRow(
      column(
        width = 6,
        plotOutput(outputId = "grafico_estado")
      ),
      column(
        offset = 1,
        width = 4,
        h4("Os 10 municípios com mais"),
        h4("UBS no Estado:"),
        tableOutput(outputId = "tabela_1")
      )
    )
  ),
)




server <- function(input, output, session) {


  observe({

    filter_1 <- ubs |>
      filter(nome_uf == input$estado_1) |>
      pull(nome_municipio) |>
      unique() |>
      sort()

    updateSelectInput(
      session = session,
      inputId = "municipio_1",
      choices = filter_1
    )

  })


  output$mapa <- renderLeaflet({

    ubs |>
      leaflet() |>
      addTiles() |>
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = "red",
        radius = 4,
        label = ~nome,
        clusterOptions = leaflet::markerClusterOptions()
      )

  })


  output$dados <- DT::renderDataTable({

    DT::datatable(
      ubs |>
        filter(nome_uf == input$estado_1) |>
        filter(nome_municipio == input$municipio_1) |>
        select(nome, nome_uf, nome_mesorregiao, nome_municipio, bairro, logradouro),
      filter = "top",
      colnames = c("Nome da UBS", "Estado", "Mesorregião", "Município", "Bairro", "Endereço")
    )

  })


  output$grafico_estado <- renderPlot({

    tmp <- ubs |>
      count(municipio_completo)|>
      rename(code_muni = municipio_completo) |>
      mutate(code_muni = as.numeric(code_muni))

    municipios <- left_join(municipios, tmp)

    municipios <- municipios |>
      mutate(
        n = replace_na(n, 0),
        `Qtde UBS` = n,
        n = NULL
      )

    municipios |>
      filter(abbrev_state == input$estado_2) |>
      ggplot() +
      geom_sf(aes(fill = `Qtde UBS`)) +
      scale_fill_gradientn(colors = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")) +
      theme_void()

  })


  output$tabela_1 <- renderTable({

    ubs |>
      filter(abbrev_state == input$estado_2) |>
      count(nome_municipio)|>
      arrange(desc(n)) |>
      mutate(`Município` = nome_municipio, `Qtde UBS` = n, nome_municipio = NULL, n = NULL) |>
      head(10)

  })


}


shinyApp(ui, server)



