library(shiny)
library(ggplot2)
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(gridExtra)

cur_x_axis <- "pc1"
cur_y_axis <- "pc2"

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  navbarPage(
    title = "許廷瑋的PCA作業",
    tabPanel("PCA",
             titlePanel("PCA"),
             sidebarLayout(

               # Sidebar panel for inputs ----
               sidebarPanel(
                 radioButtons("pca_x_radio", h3("X axis"),
                              c("pc1", "pc2", "pc3", "pc4"),
                              selected = cur_x_axis),
                 radioButtons("pca_y_radio", h3("Y axis"),
                              c("pc1", "pc2", "pc3", "pc4"),
                              selected = cur_y_axis),
                 checkboxInput("show_pca_arrow", "show arrows", value=TRUE),
                 checkboxInput("show_pca_point", "show points", value=TRUE),
                 checkboxInput("show_pca_ellipse", "show ellipses", value=TRUE)
               ),

               # Main panel for displaying outputs ----
               mainPanel(

                 plotOutput(outputId = "distPcaPlot", width = "800px", height = "700px")

               )
             )),
    tabPanel("CA",
             titlePanel("CA"),
             sidebarLayout(

               # Sidebar panel for inputs ----
               sidebarPanel(
                 sliderInput(inputId = "point_num",
                             label = "Number of points:",
                             min = 6,
                             max = nrow(iris),
                             value = nrow(iris)),
                 checkboxInput("show_ca_arrow", "show arrows", value=FALSE),
                 checkboxInput("show_ca_point", "show points", value=TRUE),
                 checkboxInput("show_ca_number", "show numbers", value=TRUE)
               ),

               # Main panel for displaying outputs ----
               mainPanel(

                 plotOutput(outputId = "distCaPlot", width = "800px", height = "700px")

               )
             )),
    tabPanel("作者",
             titlePanel("作者資訊"),
             fluidRow(
               column(width=4,
                      h4("姓名: 許廷瑋"),
                      h4("學號: 111971021"),
                      h4("系級: 資科專一"))
             ))
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  data(iris)

  ## PCA
  update_pca_plot <- function() {
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]

    pca <- prcomp(log.ir, center=TRUE, scale.=TRUE)
    x_axis <- switch(cur_x_axis,
                     "pc1"=1,
                     "pc2"=2,
                     "pc3"=3,
                     "pc4"=4)
    y_axis <- switch(cur_y_axis,
                     "pc1"=1,
                     "pc2"=2,
                     "pc3"=3,
                     "pc4"=4)
    if (input$show_pca_point == TRUE) {
      point_alpha <- 1
    } else {
      point_alpha <- 0
    }
    p1 <- ggbiplot(pca, choices=c(x_axis, y_axis), obs.scale=1, var.scale=1,
                   groups=iris$Species,
                   var.axes=input$show_pca_arrow,
                   ellipse=input$show_pca_ellipse,
                   alpha=point_alpha) +
      scale_color_discrete(name = "Species", labels = c("setosa", "versicolor", "virginica")) +
      scale_color_manual(values=c("setosa"="blue", "versicolor" = "orange", "virginica" = "green"))

    # Add grid lines
    p1 <- p1 + theme(panel.grid.major=element_line(linetype = "dashed"),
                     aspect.ratio=1)
    # Display plot
    return(p1)
  }

  output$distPcaPlot <- renderPlot({
    update_pca_plot()
  })

  observeEvent(input$pca_x_radio, {
    if(input$pca_x_radio == cur_y_axis) {
      updateRadioButtons(session, "pca_y_radio", selected = cur_x_axis)
      cur_y_axis <<- cur_x_axis
    }
    cur_x_axis <<- input$pca_x_radio
    output$distPcaPlot <- renderPlot({
      update_pca_plot()
    })
  })

  observeEvent(input$pca_y_radio, {
    if(input$pca_y_radio == cur_x_axis) {
      updateRadioButtons(session, "pca_x_radio", selected = cur_y_axis)
      cur_x_axis <<- cur_y_axis
    }
    cur_y_axis <<- input$pca_y_radio
    output$distPcaPlot <- renderPlot({
      update_pca_plot()
    })
  })

  ## CA
  update_ca_plot <- function() {
    if (input$show_ca_point == TRUE) {
      alpha <- 1
    } else {
      alpha <- 0
    }
    if (input$show_ca_arrow == TRUE) {
      arrows <- c(TRUE, TRUE)
    } else {
      arrows <- c(FALSE, FALSE)
    }
    if (input$show_ca_number == TRUE) {
      geom <- c("point", "text")
    } else {
      geom <- c("point")
    }

    res.ca <- CA(subset(iris[1:input$point_num, 1:4]))
    p2 <- fviz_ca_biplot(res.ca, repel=TRUE, alpha=alpha, geom=geom, arrows=arrows, title="")
    return(p2)
  }

  output$distCaPlot <- renderPlot({
    update_ca_plot()
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)