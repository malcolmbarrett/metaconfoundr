#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(metaconfoundr)
library(ggplot2)
library(dplyr)
library(rlang)
library(markdown)


wrap_labeller <- function(x) stringr::str_wrap(x, 10)

is_csv <- function(x) tolower(tools::file_ext("x.CSV")) == "csv"

scq_text <- 'Sociodemographics = `Maternal age` & `Race/ethnicity` & `Marital status`,
    Socioeconomics = `SES category` | Insurance & Education,
    \"Reproductive Hx\" = `Prior pregnancy outcome`'
# Define UI
ui <-
  div(
    fluidPage(
      titlePanel("Visualize Confounder Control in Meta-Analyses"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h2("Upload data"),
          fileInput("data_file", "Upload data file"),
          downloadButton("download_example", "Download example data"),
          hr(),
          h2("Plot"),
          selectInput(
            "type",
            label = "Plot Type",
            choices = c("Heatmap" = "hm", "Traffic light" = "tl"),
            selected = 1
          ),
          conditionalPanel(
            condition = "input.type === 'tl'",
            numericInput(
              "tl_size",
              label = "Circle Size",
              value = 12
            )
          ),
          selectInput(
            "pal",
            label = "Color Palette",
            choices = c("Viridis", "Cochrane"),
            selected = 1
          ),
          checkboxInput(
            "group_domain",
            label = "Group by Construct"
          ),
          checkboxInput(
            "symbols",
            label = "Use Cochrane-style Symbols"
          ),
          checkboxInput(
            "dodge",
            label = "Dodge x-axis labels",
            value = TRUE
          ),
          checkboxInput(
            "wrap_labels",
            label = "Wrap x-axis labels",
            value = TRUE
          ),
          checkboxInput(
            "use_robins_labels",
            label = "Use ROBINS-style labels"
          ),
          hr(),
          h2("Summary"),
          checkboxInput(
            "summary_plot",
            label = "Summary plot"
          ),
          conditionalPanel(
            condition = "input.summary_plot",
            textAreaInput(
              "summary_logic",
              label = "Summarize confounder control logic (see about)",
              value = scq_text,
              height = "150px"
            )
          ),
          hr(),
          h2("Download plot"),
          selectInput("fig_ext", "File type", choices = c("png", "pdf", "jpg", "tiff")),
          splitLayout(
            numericInput("fig_height", "Height", value = 9.5),
            numericInput("fig_width", "Width", value = 17),
            numericInput("fig_dpi", "DPI", value = 320)
          ),
          downloadButton("download_plot", "Download plot")
        ),

        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              "Plot",
              plotOutput("metaconfoundr_plot", height = "600px")
            ),
            tabPanel(
              "About",
              includeMarkdown("about.md")
            )
          )
        )
      )
    ),
    style = "max-width:1500px;margin:auto;"
  )

# Define server logic
server <- function(input, output) {
  mc_data <- reactive({
    if (is.null(input$data_file)) {
      .df <- ipi
    } else if (requireNamespace("rio", quietly = TRUE)) {
      .df <- rio::import(input$data_file$datapath)
    } else if (requireNamespace("readr", quietly = TRUE) && is_csv(input$data_file$datapath)) {
      .df <- readr::read_csv(input$data_file$datapath)
    } else if (is_csv(input$data_file$datapath)) {
      .df <- read.csv(input$data_file$datapath, header = TRUE)
    } else {
      stop("Can't read data file")
    }

    .df <- metaconfoundr(.df)

    if (input$summary_plot) {
      req(input$summary_logic)
      .df <- summarize_control_quality(
        .df,
        !!input$summary_logic
      )
    }


    if (input$wrap_labels) {
      .df <- mutate(.df, variable = stringr::str_wrap(variable, 10))
    }

    .df
  })

  output$download_example <- downloadHandler(
    filename = "ipi.csv",
    content = function(file) {
      write.csv(ipi, file)
    }
  )

  build_metaconfoundr_plot <- reactive({
    if (input$type == "tl") {
      p <- mc_trafficlight(mc_data(), size = input$tl_size) +
        theme_mc()
    } else {
      p <- mc_heatmap(mc_data()) +
        theme_mc()
    }

    if (input$pal == "Cochrane" && !input$use_robins_labels) {
      p <- p +
        scale_fill_cochrane() +
        scale_color_cochrane()
    }

    if (input$pal == "Cochrane" && input$use_robins_labels) {
      p <- p +
        scale_fill_cochrane(labels = label_robins()) +
        scale_color_cochrane(labels = label_robins())
    }

    if (input$pal != "Cochrane" && input$use_robins_labels) {
      p <- p +
        scale_fill_ordinal(labels = label_robins()) +
        scale_color_ordinal(labels = label_robins())
    }

    if (input$symbols && !input$use_robins_labels) {
      p <- p +
        geom_cochrane() +
        scale_shape_cochrane()
    }

    if (input$symbols && input$use_robins_labels) {
      p <- p +
        geom_cochrane() +
        scale_shape_cochrane(labels = label_robins())
    }

    if (input$dodge) {
      p <- p + guides(x = guide_axis(n.dodge = 2))
    }

    if (input$group_domain) {
      p <- p + facet_grid(
        . ~ construct,
        scales = "free_x",
        space = "free_x",
        labeller = as_labeller(wrap_labeller)
      )
    }

    p
  })

  output$metaconfoundr_plot <- renderPlot({
    build_metaconfoundr_plot()
  })


  output$download_plot <- downloadHandler(
    filename = paste0("metaconfoundr_plot.", input$fig_ext),
    content = function(file) {
      ggsave(
        file,
        build_metaconfoundr_plot(),
        height = input$fig_height,
        width = input$fig_width,
        dpi = input$fig_dpi
      )
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
