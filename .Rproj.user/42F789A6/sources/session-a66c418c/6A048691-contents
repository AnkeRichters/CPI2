library(shiny)
library(shinyWidgets)

# -----------------------
# Shrunk betas from model
# -----------------------

# GECHECKT
betas <- list(
  age_lt50       =  0.30712033,   # age.cat<50
  age_ge65       =  0.15134142,   # age.cat65+  (ref: 50-64)
  kps90          =  0.20930986,   # KPS = 90  (ref: KPS 100)
  kps_lt90       =  0.30108799,   # KPS < 90
  pr_neph        = -0.17050378,   # prior nephrectomy
  met_liver      =  0.15554129,   # liver metastasis
  met_lung       =  0.11317516,   # lung metastasis
  met_bone       =  0.19377888,   # bone metastasis
  met_lymph      =  0.11857024,   # lymph node metastasis
  met_lung_bone  =  0.24207091, # interaction: lung AND bone metastasis
  met_liver_bone =  0.28761072, # interaction: lung AND bone metastasis
  cal_cat1       =  0.09805855,   # calcium < 2.3 mmol/L
  cal_cat3       =  0.45278468,   # calcium > 2.6 mmol/L  (ref: 2.3-2.6)
  log_alp        =  0.12542496,   # log(ALP) (continuous)
  alb            = -0.03415469,   # albumin (continuous)
  ldh_100        =  0.070039607,   # LDH / 100 (continuous)
  anc            = -0.01457190,   # ANC (continuous)
  log_lym        = -0.46817435,   # log(lymphocytes) (continuous)
  wbc            =  0.07099350    # WBC (continuous)
)

cumbashaz36 <- 0.6137185

# -----------------------
# Helper function for form rows
# -----------------------
formRow <- function(label, input) {
  fluidRow(
    column(7, strong(label)),
    column(4, input)
  )
}

# -----------------------
# UI
# -----------------------
ui <- fluidPage(
  
  tags$style(HTML("
    .shiny-input-radiogroup .btn {
      background-color: darkgrey;
      color: white;
      border-color: darkgrey;
    }
    .shiny-input-radiogroup .btn.active {
      background-color: #30abce;
      color: white;
      border-color: #30abce;
    }
    #calculate {
      background-color: #263d62;
      color: white;
      border-color: #263d62;
    }
    .info-link {
      color: #999;
      cursor: pointer;
      font-size: 11px;
      text-decoration: underline;
    }
    .info-link:hover {
      color: #E77428;
    }
    .modal-backdrop-custom {
      display: none;
      position: fixed;
      top: 0; left: 0;
      width: 100%; height: 100%;
      background: rgba(0,0,0,0.5);
      z-index: 9998;
    }
    .modal-custom {
      display: none;
      position: fixed;
      top: 50%; left: 50%;
      transform: translate(-50%, -50%);
      background: white;
      padding: 20px;
      border-radius: 8px;
      z-index: 9999;
      min-width: 400px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.3);
    }
    .modal-close {
      float: right;
      cursor: pointer;
      font-size: 18px;
      font-weight: bold;
      color: #666;
      background: none;
      border: none;
      line-height: 1;
    }
    .modal-close:hover { color: #000; }
  ")),
  # JavaScript to handle open/close
  tags$script(HTML("
    function openFigureModal() {
      document.getElementById('figureModal').style.display = 'block';
      document.getElementById('modalBackdrop').style.display = 'block';
    }
    function closeFigureModal() {
      document.getElementById('figureModal').style.display = 'none';
      document.getElementById('modalBackdrop').style.display = 'none';
    }
  ")),
  
  # Backdrop
  tags$div(id = "modalBackdrop", class = "modal-backdrop-custom",
           onclick = "closeFigureModal()"
  ),
  
  # Modal
  tags$div(id = "figureModal", class = "modal-custom",
           tags$button("✕", class = "modal-close", onclick = "closeFigureModal()"),
           tags$h4("Performance status scales"),
           tags$img(src = "Performance_status_scales.jpg", stile="width:100%"),
           plotOutput("popupPlot", width = "500px", height = "300px")
  ),
  
  
  
  titlePanel("Clinical Prognostic Index in the Checkpoint Inhibitor era (CPI2)"),
  
  fluidRow(
    style = "display: flex; align-items: flex-start; gap: 10px;",
    
    # ---------- Left box ----------
    column(
      width = 8,
      wellPanel(
        style = "border: 1px solid #ccc; padding: 15px; background-color: #f9f9f9;",
        
        formRow(
          "Age (years)",
          numericInput("age", NULL, value = 65, min = 0, width = "100px")
        ),
        formRow(
          "Karnofsky performance status",
          radioGroupButtons(
            "karnofsky",
            choices = c("100", "90", "<90"),
            selected = "100"
          )
        ),
        tags$span("Click for ECOG", class = "info-link",
                  onclick = "openFigureModal()"
                  ),
        formRow(
          "Previous nephrectomy",
          radioGroupButtons(
            "pr_neph",
            choices = c("No", "Yes"),
            selected = "No"
          )
        ),
        formRow(
          "Liver metastasis",
          radioGroupButtons(
            "liver_met",
            choices = c("No", "Yes"),
            selected = "No"
          )
        ),
        formRow(
          "Lung metastasis",
          radioGroupButtons(
            "lung_met",
            choices = c("No", "Yes"),
            selected = "No"
          )
        ),
        formRow(
          "Bone metastasis",
          radioGroupButtons(
            "bone_met",
            choices = c("No", "Yes"),
            selected = "No"
          )
        ),
        formRow(
          "Lymph node metastasis",
          radioGroupButtons(
            "lymph_met",
            choices = c("No", "Yes"),
            selected = "No"
          )
        ),
        formRow(
          "Calcium (mmol/L)",
          numericInput("calcium", NULL, value = 2.4, min = 0, width = "100px")
        ),
        formRow(
          "ALP (U/L)",
          numericInput("alp", NULL, value = 100, min = 0, width = "100px")
        ),
        formRow(
          "Albumin (g/L)",
          numericInput("albumin", NULL, value = 40, min = 0, width = "100px")
        ),
        formRow(
          "LDH (U/L)",
          numericInput("ldh", NULL, value = 190, min = 0, width = "100px")
        ),
        formRow(
          "ANC (10\u2079 c/L)",
          numericInput("anc", NULL, value = 4.3, min = 0, width = "100px")
        ),
        formRow(
          "Lymphocytes (10\u2079 c/L)",
          numericInput("lymphocytes", NULL, value = 1.6, min = 0, width = "100px")
        ),
        formRow(
          "WBC (10\u2079 c/L)",
          numericInput("wbc", NULL, value = 7.20, min = 0, width = "100px")
        ),
        
        br(),
        actionButton("calculate", "Calculate", class = "btn-primary")
      )
    ),
    
    # ---------- Right box ----------
    column(
      width = 4,
      wellPanel(
        style = "border: 1px solid #ccc; padding: 15px; background-color: #f9f9f9;",
        tags$h3("Prognosis", style = "margin-top: 0; margin-bottom: 20px;"),
        uiOutput("result")
      )
    )
  )
)

# -----------------------
# Server
# -----------------------
server <- function(input, output, session) {
  
  output$result <- renderUI({
    input$calculate
    
    isolate({
      
      # --- Age category (ref: 50-64) ---
      b_age <- ifelse(input$age < 50,  betas$age_lt50,
                      ifelse(input$age >= 65, betas$age_ge65, 0))
      
      # --- KPS (ref: 100) ---
      b_kps <- ifelse(input$karnofsky == "90",  betas$kps90,
                      ifelse(input$karnofsky == "<90", betas$kps_lt90, 0))
      
      # --- Calcium category (ref: 2.3-2.6) ---
      b_cal <- ifelse(input$calcium < 2.3,  betas$cal_cat1,
                      ifelse(input$calcium > 2.6,  betas$cal_cat3, 0))
      
      # --- Continuous variables ---
      b_alb     <- betas$alb     * input$albumin
      b_anc     <- betas$anc     * input$anc
      b_ldh     <- betas$ldh_100 * (input$ldh / 100)
      b_log_alp <- betas$log_alp * log(input$alp)
      b_log_lym <- betas$log_lym * log(input$lymphocytes)
      b_wbc     <- betas$wbc     * input$wbc
      
      # --- Metastasis (binary) ---
      b_liver <- ifelse(input$liver_met == "Yes", betas$met_liver, 0)
      b_lymph <- ifelse(input$lymph_met == "Yes", betas$met_lymph, 0)
      b_lung  <- ifelse(input$lung_met == "Yes", betas$met_lung, 0)
      b_bone  <- ifelse(input$bone_met == "Yes", betas$met_bone, 0)
      # Lung and bone: main effects + interaction term
      b_lung_bone  <- ifelse(input$lung_met == "Yes"  & input$bone_met == "Yes", betas$met_lung_bone, 0)
      b_liver_bone <- ifelse(input$liver_met == "Yes" & input$bone_met == "Yes", betas$met_liver_bone, 0)
      
      # --- Nephrectomy (binary) ---
      b_neph  <- ifelse(input$pr_neph   == "Yes", betas$pr_neph,   0)
      
      # --- Linear predictor (prognostic index) ---
      PI <- b_age + b_kps + b_neph + 
        b_liver + b_lung + b_bone + b_lymph + b_lung_bone + b_liver_bone +
        b_cal + b_log_alp + b_alb + b_ldh + b_anc + b_log_lym + b_wbc
         
        
      # --- Predicted probability of death by 36 months ---
      prob <- 1-exp(-(exp(PI) * (cumbashaz36)))
      prob_pct <- round(prob * 100, 1)
      
      # --- Risk group (optional: adapt thresholds to your model) ---
      risk_group <- ifelse(prob_pct < 40, "CPI2 Favorable Prognosis",
                           ifelse(prob_pct < 57, "CPI2 Intermediate Prognosis", "CPI2 Poor Prognosis"))
      
      HTML(paste0(
        "<b>Predicted probability<br>of mortality at 36 months:</b>",
        "<br>",
        "<span style='font-size:1.2em; color:#E77428;'>", prob_pct, "%</span>",
        "<br><br>",
        "<b>Risk group:</b> ","<br>", 
        "<span style='font-size:1.2em; color:#E77428;'>", risk_group, "</span>"
      ))
    })
  })
}

# -----------------------
# Run
# -----------------------
shinyApp(ui, server)