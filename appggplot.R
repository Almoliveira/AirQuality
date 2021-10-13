library(datasets)
library(tidyr)
library(tibble)
library(shiny)
library(ggplot2)

aq.no.missing <- drop_na(airquality)

options <- c("Ozone (parts per billion)" = "Ozone",
             "Solar (Langleys)" = "Solar.R",
             "Wind (MPH)" = "Wind",
             "Temperature (F)" = "Temp")

df.options <- data.frame(options)

df.lv <- rownames_to_column(df.options)

colnames(df.lv) <- c("label","value")

ui <- fluidPage(
  selectInput("X", "x Variable:", options),
  selectInput("Y", "Y Variable:", options),
  plotOutput("scatter")
)

server <- function(input, output) {
  selections <- reactive({
    aq.no.missing[, c(input$X, input$Y)]
  })
  
  output$scatter <- renderPlot({
    
    x_column <- selections()[,1]
    y_column <- selections()[,2]
    
    correlation <- cor(x_column, y_column)
    regression <- lm(y_column ~ x_column)
    intercept <- regression$coefficients[1]
    slope <- regression$coefficients[2]
    
    X_Label <- df.lv$label[which(df.lv$value == input$X)]
    Y_Label <- df.lv$label[which(df.lv$value == input$Y)] 
    
    ggplot(selections(), aes(x = x_column, y = y_column )) +
      geom_point(size =3) +
      labs(x = X_Label, y = Y_Label,
           title = paste(Y_Label, "vs", X_Label,
            "\n r = ", round(correlation, 3),
            "Y' =",round(intercept,3), "+", round(slope,3), "X")) +
      theme(axis.title.x = element_text(size=18),
            axis.text.x = element_text(size=17),
            axis.title.y = element_text(size=18),
            axis.text.y = element_text(size=18),
            plot.title = element_text(hjust = 0.5, size=20)
            ) +
      geom_smooth(method="lm", col="black")
  })
}

shinyApp(ui = ui, server = server)
