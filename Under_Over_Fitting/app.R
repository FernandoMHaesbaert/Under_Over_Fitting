library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# Funções para simulação Monte Carlo de viés e variância
simulate_bias_variance <- function(n_sim = 100, n_points = 50, noise_sd = 2, max_degree = 15) {
      set.seed(123)
      
      # Função verdadeira cúbica
      true_function <- function(x) x^3 - 0.1*x^2 + 0.1*x + 1
      
      # Grid de graus
      degrees <- 1:max_degree
      
      # Resultados
      results <- matrix(NA, nrow = max_degree, ncol = 6)
      colnames(results) <- c("degree", "bias_squared", "variance", "mse", "train_error", "test_error")
      
      for (deg in degrees) {
            # Armazenar predições para calcular viés e variância
            predictions_matrix <- matrix(NA, nrow = n_sim, ncol = n_points)
            train_errors <- numeric(n_sim)
            test_errors <- numeric(n_sim)
            
            for (sim in 1:n_sim) {
                  # Gerar dados
                  x_train <- seq(-3, 3, length.out = n_points)
                  y_true <- true_function(x_train)
                  y_train <- y_true + rnorm(n_points, 0, noise_sd)
                  
                  # Dividir em treino e teste
                  train_idx <- sample(n_points, floor(0.7 * n_points))
                  x_test <- x_train[-train_idx]
                  y_test <- y_true[-train_idx]
                  
                  # Ajustar modelo
                  formula <- as.formula(paste("y_train ~ poly(x_train,", 
                                              deg, ", raw = TRUE)"))
                  model <- lm(formula)
                  
                  # Predições
                  pred_train <- predict(model)
                  pred_test <- predict(model, newdata = data.frame(x_train = x_test))
                  
                  # Armazenar erros
                  train_errors[sim] <- mean((y_train - pred_train)^2)
                  test_errors[sim] <- mean((y_test - pred_test)^2)
                  
                  # Predições no grid completo para cálculo de viés/variância
                  pred_full <- predict(model, newdata = data.frame(x_train = x_train))
                  predictions_matrix[sim, ] <- pred_full
            }
            
            # Calcular viés e variância
            pred_mean <- rowMeans(predictions_matrix)
            bias_squared <- mean((pred_mean - true_function(x_train))^2)
            variance <- mean(apply(predictions_matrix, 2, var))
            mse <- bias_squared + variance
            
            results[deg, ] <- c(deg, bias_squared, variance, mse, 
                                mean(train_errors), mean(test_errors))
      }
      
      as.data.frame(results)
}

# Função para gerar dados
generate_data <- function(n_points = 50, noise_sd = 2, seed = 123) {
      set.seed(seed)
      x <- seq(-3, 3, length.out = n_points)
      y_true <- x^3 - 0.1*x^2 + 0.1*x + 1
      y <- y_true + rnorm(n_points, 0, noise_sd)
      data.frame(x = x, y = y, y_true = y_true)
}

# Função para ajustar modelos
fit_models <- function(data) {
      degrees <- 1:15
      models <- map(degrees, function(deg) {
            formula <- as.formula(paste("y ~ poly(x,", deg, ", raw = TRUE)"))
            lm(formula, data = data)
      })
      names(models) <- paste0("degree_", 1:15)
      models
}

# Função para calcular erros
calculate_errors <- function(data, models) {
      set.seed(123)
      train_idx <- sample(nrow(data), floor(0.7 * nrow(data)))
      train_data <- data[train_idx, ]
      test_data <- data[-train_idx, ]
      
      errors <- map_dfr(names(models), function(model_name) {
            model <- models[[model_name]]
            degree <- as.numeric(gsub("degree_", "", model_name))
            
            pred_train <- predict(model, newdata = train_data)
            pred_test <- predict(model, newdata = test_data)
            
            mse_train <- mean((train_data$y - pred_train)^2)
            mse_test <- mean((test_data$y - pred_test)^2)
            
            tibble(
                  degree = degree,
                  mse_train = mse_train,
                  mse_test = mse_test
            )
      })
      
      errors
}

# Interface do usuário
ui <- fluidPage(
      tags$head(
            tags$style(HTML("
      .well {
        background-color: #f5f5f5;
        border: 1px solid #ddd;
      }
      .metric-box {
        padding: 10px;
        margin: 5px 0;
        border-radius: 5px;
      }
      .underfit { background-color: #ffeaa7; }
      .good { background-color: #55efc4; }
      .overfit { background-color: #ff7675; }
      .interpretation-box {
        background-color: #f8f9fa;
        border-left: 4px solid #007bff;
        padding: 15px;
        margin: 10px 0;
      }
    "))
      ),
      
      titlePanel("Demonstração de Underfitting e Overfitting: Equilíbrio Viés-Variância"),
      
      sidebarLayout(
            sidebarPanel(
                  h4("Parâmetros do Modelo"),
                  
                  sliderInput("degree", "Grau do Polinômio:", 
                              min = 1, max = 15, value = 3, step = 1),
                  
                  sliderInput("n_points", "Número de Pontos:", 
                              min = 20, max = 100, value = 50, step = 5),
                  
                  sliderInput("noise", "Nível de Ruído:", 
                              min = 1, max = 20, value = 4, step = 1),
                  
                  actionButton("generate", "Gerar Novos Dados", class = "btn-primary"),
                  
                  hr(),
                  
                  h4("Análise do Modelo"),
                  
                  div(id = "metrics_panel",
                      uiOutput("metrics")
                  ),
                  
                  hr(),
                  
                  h4("Interpretação do Equilíbrio"),
                  div(id = "interpretation",
                      textOutput("interp_text")
                  ),
                  
                  conditionalPanel(
                        condition = "input.degree <= 2",
                        div(class = "interpretation-box",
                            h5("🎯 Underfitting Detectado"),
                            p("O modelo é muito simples (alto viés). Aumente o grau do polinômio para capturar melhor o padrão cúbico dos dados.")
                        )
                  ),
                  
                  conditionalPanel(
                        condition = "input.degree >= 10",
                        div(class = "interpretation-box",
                            h5("⚠️ Overfitting Detectado"),
                            p("O modelo é muito complexo (alta variância). Reduza o grau do polinômio para evitar ajuste excessivo ao ruído.")
                        )
                  ),
                  
                  conditionalPanel(
                        condition = "input.degree > 2 && input.degree < 10",
                        div(class = "interpretation-box",
                            h5("✅ Bom Equilíbrio"),
                            p("O modelo apresenta equilíbrio adequado entre viés e variância.")
                        )
                  )
            ),
            
            mainPanel(
                  tabsetPanel(
                        tabPanel("Visualização Principal",
                                 plotOutput("main_plot", height = "500px"),
                                 h4("📊 Como interpretar:"),
                                 div(class = "row",
                                     div(class = "col-md-6",
                                         p("• Pontos pretos: dados observados com ruído"),
                                         p("• Linha vermelha: função verdadeira (cúbica)")),
                                     div(class = "col-md-6",
                                         p("• Linha azul: modelo ajustado"),
                                         p("• Área cinza: intervalo de confiança 95%")))
                        ),
                        
                        tabPanel("Equilíbrio Viés-Variância",
                                 plotOutput("bias_variance_plot", height = "500px"),
                                 h4("📈 Componentes do Erro:"),
                                 div(class = "row",
                                     div(class = "col-md-6",
                                         p("• Viés²: erro devido a suposições incorretas"),
                                         p("• Variância: erro devido a sensibilidade aos dados")),
                                     div(class = "col-md-6",
                                         p("• Erro Total: Viés² + Variância + Erro Irredutível"),
                                         p("• Ponto ideal: mínimo da curva de erro total"))),
                                 br(),
                                 h4("🎯 Zonas de Operação:"),
                                 div(class = "row",
                                     div(class = "col-md-4", 
                                         div(class = "metric-box underfit",
                                             h5("Underfitting"),
                                             p("Complexidade baixa"),
                                             p("Alto Viés, Baixa Variância"))
                                     ),
                                     div(class = "col-md-4", 
                                         div(class = "metric-box good",
                                             h5("Ponto Ideal"),
                                             p("Complexidade ótima"),
                                             p("Equilíbrio Viés-Variância"))
                                     ),
                                     div(class = "col-md-4", 
                                         div(class = "metric-box overfit",
                                             h5("Overfitting"),
                                             p("Complexidade alta"),
                                             p("Baixo Viés, Alta Variância"))
                                     ))
                        ),
                        
                        tabPanel("Erros de Treino vs Teste",
                                 plotOutput("error_plot", height = "400px"),
                                 p("Este gráfico mostra como os erros de treino e teste variam com o grau do polinômio.")),
                        
                        tabPanel("Múltiplos Graus",
                                 plotOutput("multi_plot", height = "500px"),
                                 p("Comparação visual de diferentes graus polinomiais."))
                  )
            )
      )
)

# Lógica do servidor
server <- function(input, output, session) {
      
      # Reactive values
      values <- reactiveValues(
            data = NULL,
            models = list(),
            errors = NULL,
            bias_variance_data = NULL
      )
      
      # Inicializar com simulação de viés-variância
      observe({
            values$bias_variance_data <- simulate_bias_variance(
                  n_sim = 50,  # Reduzido para performance
                  n_points = input$n_points,
                  noise_sd = input$noise,
                  max_degree = 15
            )
      })
      
      # Atualizar dados e modelos
      update_data <- function() {
            values$data <- generate_data(input$n_points, input$noise)
            values$models <- fit_models(values$data)
            values$errors <- calculate_errors(values$data, values$models)
            values$bias_variance_data <- simulate_bias_variance(
                  n_sim = 50,
                  n_points = input$n_points,
                  noise_sd = input$noise,
                  max_degree = 15
            )
      }
      
      # Inicializar
      observe({
            update_data()
      })
      
      # Atualizar quando o botão for clicado
      observeEvent(input$generate, {
            update_data()
      })
      
      # Plot principal
      output$main_plot <- renderPlot({
            req(values$data, values$models)
            
            data <- values$data
            model <- values$models[[paste0("degree_", input$degree)]]
            
            # Criar grid para predições suaves
            x_grid <- seq(min(data$x), max(data$x), length.out = 200)
            pred <- predict(model, newdata = data.frame(x = x_grid), 
                            interval = "confidence", level = 0.95)
            
            plot_data <- data.frame(
                  x = x_grid,
                  y_fit = pred[, "fit"],
                  y_lower = pred[, "lwr"],
                  y_upper = pred[, "upr"]
            )
            
            ggplot() +
                  geom_point(data = data, aes(x = x, y = y), 
                             color = "black", size = 2, alpha = 0.7) +
                  geom_line(aes(x = x, y = y_true), 
                            data = data, color = "red", size = 1.2, linetype = "dashed") +
                  geom_line(aes(x = x, y = y_fit), 
                            data = plot_data, color = "blue", size = 1.2) +
                  geom_ribbon(aes(x = x, ymin = y_lower, ymax = y_upper), 
                              data = plot_data, alpha = 0.2, fill = "blue") +
                  labs(title = paste("Regressão Polinomial de Grau", input$degree),
                       subtitle = "Pontos = dados observados, Linha vermelha = função verdadeira, Linha azul = modelo ajustado",
                       x = "x", y = "y") +
                  theme_minimal() +
                  theme(plot.title = element_text(size = 14, face = "bold"))
      })
      
      # Plot de Bias-Variance Trade-off
      output$bias_variance_plot <- renderPlot({
            req(values$bias_variance_data)
            
            bv_data <- values$bias_variance_data
            
            # Criar dados para visualização
            plot_data <- bv_data %>%
                  select(degree, bias_squared, variance, mse) %>%
                  pivot_longer(cols = c(bias_squared, variance, mse), 
                               names_to = "component", values_to = "error")
            
            # Adicionar erro irredutível (ruído)
            noise_level <- input$noise^2
            plot_data <- plot_data %>%
                  mutate(
                        component = case_when(
                              component == "bias_squared" ~ "Viés²",
                              component == "variance" ~ "Variância",
                              component == "mse" ~ "Erro Total (MSE)"
                        ),
                        error = error + ifelse(component == "Erro Total (MSE)", 
                                               noise_level, 0)
                  )
            
            # Encontrar ponto ótimo
            optimal_degree <- bv_data$degree[which.min(bv_data$mse + noise_level)]
            
            ggplot(plot_data, aes(x = degree, y = error, color = component)) +
                  geom_line(size = 1.2) +
                  geom_point(size = 2) +
                  geom_vline(xintercept = input$degree, linetype = "dashed", 
                             alpha = 0.7, color = "purple") +
                  geom_vline(xintercept = optimal_degree, linetype = "dotted", 
                             alpha = 0.8, color = "green", size = 1) +
                  annotate("text", x = optimal_degree, 
                           y = max(plot_data$error) * 0.9, 
                           label = paste("Ótimo:", optimal_degree), 
                           hjust = -0.1, color = "green", fontface = "bold") +
                  scale_color_manual(values = c("Viés²" = "#e74c3c", 
                                                "Variância" = "#3498db", 
                                                "Erro Total (MSE)" = "#2c3e50")) +
                  labs(title = "Equilíbrio entre Viés e Variância (Bias-Variance Trade-off)",
                       subtitle = "Linha pontilhada verde = grau ótimo, Linha tracejada roxa = grau atual",
                       x = "Complexidade do Modelo (Grau do Polinômio)",
                       y = "Erro",
                       color = "Componente do Erro") +
                  theme_minimal() +
                  theme(
                        plot.title = element_text(size = 20, face = "bold"),
                        # Aumentar fonte do subtitulo
                        plot.subtitle = element_text(size = 16),
                        # Aumentar fonte dos eixos
                        axis.title = element_text(size = 16),
                        # Aumentar fonte dos rótulos dos eixos
                        axis.text = element_text(size = 16),
                        legend.position = "bottom",
                        panel.grid.minor = element_blank()
                  )
      })
      
      # Plot de erros
      output$error_plot <- renderPlot({
            req(values$errors)
            
            errors <- values$errors
            
            ggplot(errors, aes(x = degree)) +
                  geom_line(aes(y = mse_train, color = "Treino"), size = 1.2) +
                  geom_line(aes(y = mse_test, color = "Teste"), size = 1.2) +
                  geom_vline(xintercept = input$degree, linetype = "dashed", alpha = 0.7) +
                  scale_color_manual(values = c("Treino" = "blue", "Teste" = "red")) +
                  labs(title = "Erro Quadrático Médio vs Grau do Polinômio",
                       x = "Grau do Polinômio", y = "MSE",
                       color = "Conjunto") +
                  theme_minimal() +
                  theme(legend.position = "bottom")
      })
      
      # Plot múltiplos graus
      output$multi_plot <- renderPlot({
            req(values$data, values$models)
            
            data <- values$data
            selected_degrees <- c(1, 2, 3, 5, 8, 12)
            
            plots_data <- map_dfr(selected_degrees, function(deg) {
                  model <- values$models[[paste0("degree_", deg)]]
                  x_grid <- seq(min(data$x), max(data$x), length.out = 100)
                  pred <- predict(model, newdata = data.frame(x = x_grid))
                  
                  data.frame(
                        x = x_grid,
                        y = pred,
                        degree = factor(paste("Grau", deg))
                  )
            })
            
            ggplot() +
                  geom_point(data = data, aes(x = x, y = y), 
                             color = "black", size = 1.5, alpha = 0.6) +
                  geom_line(aes(x = x, y = y, color = degree), 
                            data = plots_data, size = 1) +
                  labs(title = "Comparação de Diferentes Graus Polinomiais",
                       x = "x", y = "y", color = "Modelo") +
                  theme_minimal() +
                  theme(legend.position = "bottom")
      })
      
      # Métricas
      output$metrics <- renderUI({
            req(values$errors, values$models, values$data, values$bias_variance_data)
            
            current_errors <- values$errors[values$errors$degree == input$degree, ]
            model <- values$models[[paste0("degree_", input$degree)]]
            current_bv <- values$bias_variance_data[values$bias_variance_data$degree == input$degree, ]
            
            # Calcular R²
            r_squared <- summary(model)$r.squared
            
            # Determinar status
            if (input$degree <= 2) {
                  status <- "underfit"
                  status_text <- "Underfitting"
                  icon <- "⚠️"
            } else if (input$degree >= 10) {
                  status <- "overfit"
                  status_text <- "Overfitting"
                  icon <- "⚠️"
            } else {
                  status <- "good"
                  status_text <- "Bom Ajuste"
                  icon <- "✅"
            }
            
            tagList(
                  div(class = paste("metric-box", status),
                      h5(paste(icon, status_text)),
                      p(paste("MSE Treino:", round(current_errors$mse_train, 3))),
                      p(paste("MSE Teste:", round(current_errors$mse_test, 3))),
                      p(paste("R²:", round(r_squared, 3))),
                      hr(),
                      h5("Componentes do Erro:"),
                      p(paste("Viés²:", round(current_bv$bias_squared, 3))),
                      p(paste("Variância:", round(current_bv$variance, 3))),
                      p(paste("MSE Total:", round(current_bv$mse, 3)))
                  )
            )
      })
      
      # Texto de interpretação
      output$interp_text <- renderText({
            req(input$degree, values$bias_variance_data)
            
            bv_data <- values$bias_variance_data[values$bias_variance_data$degree == input$degree, ]
            
            if (input$degree <= 2) {
                  paste("🎯 Underfitting: O modelo é muito simples (alto viés =", round(bv_data$bias_squared, 3), 
                        "). Aumente o grau do polinômio para capturar melhor o padrão cúbico dos dados.")
            } else if (input$degree >= 10) {
                  paste("⚠️ Overfitting: O modelo é muito complexo (alta variância =", round(bv_data$variance, 3), 
                        "). Reduza o grau do polinômio para evitar ajuste excessivo ao ruído.")
            } else {
                  paste("✅ Bom Equilíbrio: O modelo apresenta equilíbrio adequado entre viés (", round(bv_data$bias_squared, 3), 
                        ") e variância (", round(bv_data$variance, 3), ").")
            }
      })
}

# Executar o aplicativo
shinyApp(ui = ui, server = server)