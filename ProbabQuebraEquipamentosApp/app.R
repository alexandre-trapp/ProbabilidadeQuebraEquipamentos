library(shiny)

ui <- fluidPage(

    titlePanel("Probabilidade de falha em equipamentos"),
    fluidRow(
        column(6,
               radioButtons("Opcao", label = "Selecione o cálculo", choices = list("Prob. Exata"=1, "Menos que"=2, "Mais que"=3), selected = 1)
               ),
        column(6,
               numericInput("Ocorrencia", "Ocorrência atual:", value = 2, min = 1, max = 99),
               actionButton("Processar","Processar")
               )
    ),
    fluidRow(
        column(12,
               plotOutput("Graf")
               )
    )
)

server <- function(input, output) {

    observeEvent(input$Processar,{
        
        lamb = input$Ocorrencia
        
        tipo = input$Opcao
        
        inic = lamb -2
        fim = lamb +2
        
        exata = 1
        menosQue = 2
        maisQue = 3
        
        if (tipo == exata) {
            
            x = dpois(inic:fim, lambda = lamb)
            tit = "Probabilidade de ocorrência"
        }
        else if (tipo == menosQue) {
            
            x = ppois(inic:fim, lambda = lamb)
            tit = "Probabilidade de ocorrência Menor que"
        }
        else if (tipo == maisQue) {
            
            x = ppois(inic:fim, lambda = lamb, lower.tail = F)
            tit = "Probabilidade de ocorrência Maior que"
        }
        
        z = as.character(round(x, 4))
        y = as.character(inic:fim)
        
        lab = paste(y, "Prob:", z)
        
        output$Graf = renderPlot({
            
            barplot(x, names.arg = lab, col = gray.colors(5), main=tit)
            box()
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
