# Load required library
if(!require(shiny)) install.packages("shiny")
library(shiny)

# --- 1. STATIC DATA GENERATION ---
# Generated OUTSIDE the server function so it never recalculates.
set.seed(42)
n_trials <- 300
static_eff_pt <- runif(n_trials, 0, 1)
# Hidden true parameters for the simulation
true_prob <- 0.25 + (0.95 - 0.25) * pnorm(static_eff_pt, mean = 0.35, sd = 0.08)
static_success <- rbinom(n_trials, 1, true_prob)

# Pre-calculate the visual jitter so the dots don't wiggle during slider updates
y_jitter <- ifelse(static_success == 1, 
                   1 - runif(n_trials, 0, 0.05), 
                   runif(n_trials, 0, 0.05))
point_colors <- ifelse(static_success == 1, "black", "darkred")

# --- 2. USER INTERFACE ---
ui <- fluidPage(
  titlePanel("Speed-Accuracy Tradeoff: PDF & CDF Fitter"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Model Parameters"),
      sliderInput("mu", "Threshold (mu) - Center of Gaussian:", 
                  min = 0.1, max = 0.8, value = 0.4, step = 0.01),
      
      sliderInput("sigma", "Slope/Noise (sigma) - Width of Gaussian:", 
                  min = 0.01, max = 0.3, value = 0.1, step = 0.01),
      
      sliderInput("gamma", "Guess Rate (gamma) - Lower asymptote:", 
                  min = 0.0, max = 0.5, value = 0.25, step = 0.01),
      
      sliderInput("asym", "Lapse Rate (asym) - Upper asymptote:", 
                  min = 0.5, max = 1.0, value = 0.95, step = 0.01),
      
      hr(),
      h4("Model Fit"),
      h3(textOutput("llText"), style = "color: blue;"),
      p("Maximize the Log-Likelihood (get it closest to 0) to find the best fit.")
    ),
    
    mainPanel(
      # Increased height to comfortably fit both plots
      plotOutput("dualPlots", height = "700px")
    )
  )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output) {
  
  # Calculate Log-Likelihood reactively
  current_ll <- reactive({
    preds <- input$gamma + (input$asym - input$gamma) * pnorm(static_eff_pt, input$mu, input$sigma)
    # Constrain to prevent log(0) crashes
    preds <- pmin(pmax(preds, 1e-10), 1 - 1e-10)
    sum(static_success * log(preds) + (1 - static_success) * log(1 - preds))
  })
  
  output$llText <- renderText({
    paste("Log-Likelihood:", round(current_ll(), 2))
  })
  
  # Render using base R graphics for maximum speed
  output$dualPlots <- renderPlot({
    
    # Set up a 2-row layout with adjusted margins
    par(mfrow = c(2, 1), mar = c(4, 5, 3, 2) + 0.1)
    
    # X-axis sequence for drawing smooth curves
    x_seq <- seq(0, 1, length.out = 200)
    
    # ---------------------------------------------------------
    # PLOT 1: Raw Data and Gaussian PDF
    # ---------------------------------------------------------
    # Draw empty plot frame
    plot(NULL, xlim = c(0, 1), ylim = c(-0.1, 1.1),
         xlab = "", ylab = "Trial Outcome / Density", 
         main = "Raw Data & Gaussian PDF (Decision Engine)",
         cex.main = 1.5, cex.lab = 1.2)
    
    # Add horizontal guide lines
    abline(h = c(0, 1), col = "gray80", lty = 2)
    
    # Add the pre-jittered static points
    points(static_eff_pt, y_jitter, col = point_colors, pch = 16, cex = 1.2)
    
    # Calculate the PDF (Gaussian) and scale it so its maximum height is exactly 1.0
    raw_pdf <- dnorm(x_seq, input$mu, input$sigma)
    scaled_pdf <- raw_pdf / max(raw_pdf)
    
    # Draw the Gaussian curve
    lines(x_seq, scaled_pdf, col = "dodgerblue", lwd = 3)
    
    # Draw a vertical line denoting the exact center (mu)
    abline(v = input$mu, col = "dodgerblue", lty = 2, lwd = 2)
    
    # ---------------------------------------------------------
    # PLOT 2: Psychometric CDF
    # ---------------------------------------------------------
    # Calculate the cumulative distribution function
    cdf_y <- input$gamma + (input$asym - input$gamma) * pnorm(x_seq, input$mu, input$sigma)
    
    # Draw empty plot frame
    plot(NULL, xlim = c(0, 1), ylim = c(-0.1, 1.1),
         xlab = "Preparation Time (eff_pt)", ylab = "Probability of Success", 
         main = "Resultant Psychometric CDF (Speed-Accuracy Curve)",
         cex.main = 1.5, cex.lab = 1.2)
    
    # Add horizontal guide lines for the asymptotes
    abline(h = c(input$gamma, input$asym), col = "gray60", lty = 3, lwd = 2)
    
    # Draw the CDF curve
    lines(x_seq, cdf_y, col = "purple", lwd = 3)
    
    # Draw a vertical line to show where mu maps onto the CDF
    abline(v = input$mu, col = "dodgerblue", lty = 2, lwd = 2)
    
  })
}

# --- 4. RUN APP ---
shinyApp(ui = ui, server = server)