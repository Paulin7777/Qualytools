library(rsconnect)
library(shiny)
library(readxl)
library(nortest)
library(qcc)

# Função para validar e obter variáveis numéricas
get_numeric_vars <- function(df, selected_vars) {
  num_vars <- selected_vars[sapply(df[selected_vars], is.numeric)]
  validate(need(length(num_vars) > 0, "Please select at least one numeric variable."))
  return(num_vars)
}

# Atualizar entradas selectize
update_selectize_inputs <- function(session, df) {
  updateSelectizeInput(session, "variables", choices = names(df), server = TRUE)
  updateSelectizeInput(session, "pareto_vars", choices = names(df), server = TRUE)
  updateSelectizeInput(session, "scatter_x", choices = names(df), server = TRUE)
  updateSelectizeInput(session, "scatter_y", choices = names(df), server = TRUE)
  updateSelectizeInput(session, "control_variable", choices = names(df), server = TRUE)
}

options(shiny.maxRequestSize = 200*1024^2)

# Define UI
ui <- fluidPage(
  titlePanel(
    windowTitle = "QualyTools",
    title = div(
      tags$img(src = "logo.PNG", height = "70px", style = "margin-right:10px;"), 
      "Quality Control Tools")
    ),
  
  # Layout principal
  sidebarLayout(
    sidebarPanel(
      # Primeira seção: Carregar Dados
      tags$div(style = "border: 2px solid #007BFF; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
               fileInput("file", "Upload file", accept = c(".csv", ".xlsx")),
               radioButtons("separator", "Separator (for CSV):",
                            choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Histogram and Normality'",
        selectizeInput("variables", "Select variables for histogram:", choices = NULL, multiple = TRUE),
        tags$p("NOTE: You can concatenate the variables."),
        sliderInput("Classes", "Number of classes:", min = 1, max = 50, value = 12),
        numericInput("lic", "Lower Specification Limit (LSL):", value = NULL),
        numericInput("lsc", "Specification Limit (SL):", value = NULL),
        numericInput("ls", "Upper Specification Limit (USL):", value = NULL),
        checkboxInput("include_normal", "Include normality curve", value = TRUE),
        checkboxInput("include_tolerance", "Include specification limits", value = TRUE)
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Data", tableOutput("file_contents")),
                  tabPanel("Histogram and Normality",
                           verbatimTextOutput("concat_summary"),
                           plotOutput("histPlot"),
                           plotOutput("qqPlot"),
                           verbatimTextOutput("lillieforsTest"),
                           verbatimTextOutput("shapirotest")
                  ),
                  tabPanel("Scatter",
                           selectizeInput("scatter_x", "Select variable for X axis:", choices = NULL),
                           selectizeInput("scatter_y", "Select variable for Y axis:", choices = NULL),
                           plotOutput("scatterPlot"),
                           verbatimTextOutput("spearmantest")
                  ),
                  tabPanel("Pareto Chart",
                           selectizeInput("pareto_vars", "Select variable for Pareto chart:", choices = NULL),
                           plotOutput("paretoPlot")
                  ),
                  tabPanel("Control Chart",
                           selectizeInput("control_variable", "Select variable for control chart:", choices = NULL),
                           numericInput("subgroup_size", "Subgroup Size:", value = 5, min = 2, step = 1),
                           selectizeInput("chart_type", 
                                          "Select Control Chart type:", 
                                          choices = c("Mean-Deviation" = "S", "Mean-Range" = "R"),
                                          selected = "S"),
                           plotOutput("xbar_chart"),
                           plotOutput("control_chart")
                  ),
                  tabPanel("Capability Analysis",  
                           selectizeInput("capability_variable", "Select variable for capability analysis:", choices = NULL),
                           numericInput("lie", "Lower Specification Limit (LSL):", value = NULL),
                           numericInput("target", "Target Value:", value = NA, step = 0.01),
                           numericInput("les", "Upper Specification Limit (USL):", value = NULL),
                           numericInput("subgroup_size_capability", "Subgroup Size:", value = 5, min = 2, step = 1),
                           verbatimTextOutput("capability_summary")  
                  ),
                  
                  tabPanel("About",
                           h3("About QualyTools"),
                           
                           p("QualyTools is an open-source application designed to support quality management through accessible and practical statistical tools."),
                           
                           p(HTML("It is available online, and its source code is openly available for contributions and improvements at the 
                <a href='https://github.com/Paulin7777/Qualytools' target='_blank'>QualyTools GitHub repository</a>.")),
                           
                           p("This app is licensed under the Creative Commons Attribution-NonCommercial 4.0 International License."),
                           
                           tags$ul(
                             tags$li("Attribution: You must give appropriate credit, provide a link to the license, and indicate if changes were made."),
                             tags$li("NonCommercial: You may not use the material for commercial purposes."),
                             tags$li("No Additional Restrictions: You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.")
                           ),
                           
                           tags$a(href = "https://creativecommons.org/licenses/by-nc/4.0/legalcode", 
                                  "Read the Full License (Click Here)", 
                                  target = "_blank"),
                           
                           tags$code("https://creativecommons.org/licenses/by-nc/4.0/legalcode")
                  )
                  
                  
                  
                   
      )
    )
  )
)  

# Define server logic
server <- function(input, output, session) {
  
  # Função para atualizar os inputs dinamicamente
  update_selectize_inputs <- function(session, df) {
    updateSelectizeInput(session, "variables", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "scatter_x", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "scatter_y", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "pareto_vars", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "control_variable", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "capability_variable", choices = names(df), server = TRUE)
  }
  
  # Carregar arquivo
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    tryCatch({
      df <- switch(ext,
                   "csv" = read.csv(input$file$datapath, sep = input$separator),
                   "xlsx" = read_excel(input$file$datapath),
                   stop("Unsupported file format."))
      update_selectize_inputs(session, df)
      return(df)
    }, error = function(e) {
      showNotification("Error loading the file. Check the format and separator.", type = "error")
      return(NULL)
    })
  })
  
  # Exibir conteúdo do arquivo
  output$file_contents <- renderTable({
    data()
  }) 
  
  # Resumo: Número total de observações
  output$concat_summary <- renderPrint({
    df <- data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    paste("Total number of observations:", length(concatenated_values))
  })
  
  # Histograma com curva de normalidade 
  output$histPlot <- renderPlot({
    df <- data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    
    xlim_values <- range(c(input$lic, input$ls, concatenated_values), na.rm = TRUE)
    bins <- seq(min(concatenated_values), max(concatenated_values), length.out = input$Classes + 1)
    
    hist_data <- hist(concatenated_values, breaks = bins, col = 'lightblue', border = 'grey',
                      xlab = "Data", main = "Histogram of Selected Variables",
                      ylab = "Frequency", freq = TRUE, xlim = xlim_values)
    
    if (input$include_normal) {
      x <- seq(min(concatenated_values), max(concatenated_values), length.out = 100)
      y <- dnorm(x, mean = mean(concatenated_values), sd = sd(concatenated_values))
      y <- y * length(concatenated_values) * diff(hist_data$breaks)[1]
      lines(x, y, col = "black", lwd = 2)
    }
    
    if (input$include_tolerance) {
      if (!is.null(input$lic)) abline(v = input$lic, col = "red", lwd = 2, lty = 2)
      if (!is.null(input$lsc)) abline(v = input$lsc, col = "green", lwd = 2, lty = 2)
      if (!is.null(input$ls)) abline(v = input$ls, col = "red", lwd = 2, lty = 4)
    }
  })
  
  # QQ Plot
  output$qqPlot <- renderPlot({
    df <- data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    
    qqnorm(concatenated_values, main = "QQ Plot of Selected Variables", pch = 19, col = "lightblue")
    qqline(concatenated_values, col = "red", lwd = 2)
  })
  
  # Testes estatísticos
  output$lillieforsTest <- renderPrint({
    df <- data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    lillie.test(concatenated_values)
  })
  
  output$shapirotest <- renderPrint({
    df <- data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    shapiro.test(concatenated_values)
  })
  
  # Gráfico de dispersão
  output$scatterPlot <- renderPlot({
    df <- data()
    req(df, input$scatter_x, input$scatter_y)
    validate(need(input$scatter_x %in% names(df), "Select a valid variable for the X-axis."))
    validate(need(input$scatter_y %in% names(df), "Select a valid variable for the Y-axis."))
    
    plot(df[[input$scatter_x]], df[[input$scatter_y]],
         main = "Scatter Plot",
         xlab = input$scatter_x,
         ylab = input$scatter_y,
         col = "blue", pch = 19)
    
    model <- lm(df[[input$scatter_y]] ~ df[[input$scatter_x]])
    abline(model, col = "red", lwd = 2)
  })
  
  output$spearmantest <- renderPrint({
    df <- data()
    req(df, input$scatter_x, input$scatter_y)
    x <- df[[input$scatter_x]]
    y <- df[[input$scatter_y]]
    cor.test(x, y, method = "spearman")
  })
  
  #PARETO
  output$paretoPlot <- renderPlot({
    df <- data()
    req(df, input$pareto_vars)
    
    selected_var <- input$pareto_vars
    validate(need(selected_var %in% names(df), "The selected variable does not exist in the dataset."))
    
    # Contagem e ordenação decrescente
    freq_table <- table(df[[selected_var]])
    freq_table <- freq_table[order(freq_table, decreasing = TRUE)]
    
    # Cálculo da frequência acumulada
    cum_freq <- cumsum(freq_table)
    
    # Definição dos limites do eixo y
    y1 <- c(0, max(cum_freq) * 1.04)  # 4% extra no topo
    y2 <- c(0, 104)  # Eixo percentual acumulado
    
    # Armazena as configurações atuais do gráfico e ajusta margens
    oldpar <- par(mar = c(5, 4, 4, 4) + 0.1)
    
    # Criação do barplot
    bp <- barplot(freq_table, ylim = y1, ylab = "Frequency", col = "steelblue", las = 2, cex.names = 0.8)
    
    # Adiciona a linha do percentual acumulado
    points(bp[, 1], cum_freq, type = "b", col = "red", lwd = 2, pch = 19)
    
    # Adiciona o eixo secundário (percentual acumulado)
    y2lab <- pretty(c(0, 100))  # Define intervalos "bonitos" para o eixo
    y2at <- y2lab / 100 * max(cum_freq)  # Ajusta para a escala dos dados
    axis(4, at = y2at, labels = paste0(y2lab, "%"))
    mtext("Cumulative Percentage (%)", side = 4, line = 3)
    
    # Restaura os parâmetros gráficos originais
    par(oldpar)
  })
  
  
  
  # Função para validar e processar os dados
  validate_and_process <- function(data, variable_name, subgroup_size) {
    variable <- data[[variable_name]]
    
    # Validar se a variável é numérica
    validate(need(is.numeric(variable), "The selected variable must be numeric."))
    
    # Validar tamanho dos dados
    validate(need(length(variable) >= subgroup_size, "The amount of data must be greater than or equal to the size of the subgroup."))
    
    # Ajustar tamanho dos dados para ser divisível pelo tamanho do subgrupo
    trimmed_variable <- head(variable, floor(length(variable) / subgroup_size) * subgroup_size)
    
    # Criar matriz de subgrupos
    subgroups <- matrix(trimmed_variable, ncol = subgroup_size, byrow = TRUE)
    
    return(subgroups)
  }
  
  # Função para gerar e exibir gráficos de controle
  generate_control_chart <- function(subgroups, chart_type) {
    if (chart_type == "xbar") {
      qcc_chart <- qcc(data = subgroups, type = "xbar")
    } else if (chart_type == "S") {
      qcc_chart <- qcc(data = subgroups, type = "S")
    } else if (chart_type == "R") {
      qcc_chart <- qcc(data = subgroups, type = "R")
    } else {
      stop("Invalid chart type.")
    }
    
    return(qcc_chart)
  }
  
  # Gerar a Carta de Controle X-bar
  output$xbar_chart <- renderPlot({
    req(data(), input$control_variable, input$subgroup_size)
    
    # Obter dados e criar subgrupos
    subgroups <- validate_and_process(data(), input$control_variable, as.numeric(input$subgroup_size))
    
    # Gerar carta X-bar
    qcc_xbar <- generate_control_chart(subgroups, "xbar")
    
    # Plotar a carta X-bar
    plot(qcc_xbar)
  })
  
  # Gerar a Carta de Controle S ou R
  output$control_chart <- renderPlot({
    req(data(), input$control_variable, input$subgroup_size, input$chart_type)
    
    # Obter dados e criar subgrupos
    subgroups <- validate_and_process(data(), input$control_variable, as.numeric(input$subgroup_size))
    
    # Gerar carta de controle complementar
    qcc_control <- generate_control_chart(subgroups, input$chart_type)
    
    # Plotar o gráfico de controle complementar
    plot(qcc_control)
  })
  
  # Função auxiliar para criar subgrupos
  create_subgroups <- function(data, variable_name, subgroup_size) {
    variable <- data[[variable_name]]
    
    # Validações simples
    if (!is.numeric(variable)) stop("The selected variable must be numeric.")
    if (length(variable) < subgroup_size) stop("The amount of data must be greater than or equal to the size of the subgroup.")
    
    trimmed_variable <- head(variable, floor(length(variable) / subgroup_size) * subgroup_size)
    subgroups <- matrix(trimmed_variable, ncol = subgroup_size, byrow = TRUE)
    
    if (nrow(subgroups) <= 1) stop("More than one subgroup is required to perform the analysis.")
    
    return(subgroups)
  }
  
  # Análise de Capacidade
  output$capability_summary <- renderPrint({
    req(data(), input$capability_variable, input$lie, input$les, input$subgroup_size_capability, input$target)
    
    # Validação dos limites e target
    if (input$lie >= input$les) stop("The lower limit (LIE) must be less than the upper limit (LES).")
    if (is.na(input$target)) stop("Please, define the target value.")
    if (input$target < input$lie || input$target > input$les) stop("The target value must be between the lower limit (LIE) and the upper limit (LES).")
    # Criar subgrupos
    subgroups <- create_subgroups(data(), input$capability_variable, as.numeric(input$subgroup_size_capability))
    
    # Análise de capacidade
    qcc_obj <- qcc(subgroups, type = "xbar", nsigmas = 3, plot = FALSE)
    capability_analysis <- qcc::process.capability(qcc_obj, spec.limits = c(input$lie, input$les), target = input$target)
    
    # Imprimir toda a análise de capacidade
    print(capability_analysis)
  })
  
  
  
}
# Run app
shinyApp(ui, server)
