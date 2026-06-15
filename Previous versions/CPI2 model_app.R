library(shiny)
library(shinyWidgets)

# -----------------------
# Helper function for form rows
# -----------------------
formRow <- function(label, input) {
  fluidRow(
    column(7, strong(label)),  # left 2/3 for label
    column(4, input)           # right 1/3 for input
  )
}

# -----------------------
# UI
# -----------------------
ui <- fluidPage(
  
  # ---- Global CSS for toggle colors ----
  tags$style(HTML("
    /* All radioGroupButtons in the app */
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
    background-color: #30abce;
    color: white;
    border-color: #30abce
    }
  ")),
  
  titlePanel("Clinical Prognostic Index in the Checkpoint Inhibitor era (CPI2)"),
  
  fluidRow(
    style = "display: flex; align-items: flex-start; gap: 10px;",  # align headers
    
    # ---------- Left box ----------
    column(
      width = 8,
      wellPanel(
        style = "border: 1px solid #ccc; padding: 15px; background-color: #f9f9f9;",
        tags$h3("Prognostic factors", style = "margin-top: 0; margin-bottom: 20px;"),
        
        formRow(
          "Age (years)",
          numericInput("age", NULL, value = 75, min = 0, width = "100px")
        ),
        formRow(
          "Sex",
          radioGroupButtons(
            "sex",
            choices = c("Male", "Female"),
            selected = "Male"
          )
        ),
        formRow(
          "Karnofsky performance status",
          radioGroupButtons(
            "karnofsky",
            choices = c("100", "90", "80"),
            selected = "90"
          )
        ),
        formRow(
          "Lung metastasis",
          radioGroupButtons(
            "lung_met",
            choices = c("No","Yes"),
            selected = "Yes"
          )
        ),
        formRow(
          "Liver metastasis",
          radioGroupButtons(
            "liver_met",
            choices = c("No","Yes"),
            selected = "No"
          )
        ),
        formRow(
          "Bone metastasis",
          radioGroupButtons(
            "bone_met",
            choices = c("No","Yes"),
            selected = "No"
          )
        ),
        formRow(
          "Calcium (mmol/L)",
          numericInput("calcium1", NULL, value = 2.4, width = "100px")
        ),
        formRow(
          "ALT (U/L)",
          numericInput("alt", NULL, value = 26, width = "100px")
        ),
        formRow(
          "ALP (U/L)",
          numericInput("alp", NULL, value = 100, width = "100px")
        ),
        formRow(
          "Lymphocytes (10*9 c/L)",
          numericInput("lymphocytes", NULL, value = 1.2, width = "100px")
        ),
        formRow(
          "ANC (10*9 c/L)",
          numericInput("anc", NULL, value = 3.3, width = "100px")
        ),
        formRow(
          "Platelets (10*9 c/L)",
          numericInput("platelets", NULL, value = 279, width = "100px")
        ),
        formRow(
          "LDH (U/L)",
          numericInput("ldh", NULL, value = 413, width = "100px")
        ),
        formRow(
          "WBC (10*9 c/L)",
          numericInput("wbc", NULL, value = 5.8, width = "100px")
        ),
        
        br(),
        actionButton("calculate", "Calculate", class = "btn-primary")
      )
    ),
    
    # ---------- Right box ----------
    column(
      width = 4,
      # Right box
      wellPanel(
        style = "border: 1px solid #ccc; padding: 15px; background-color: #f1f1f1;",
        tags$h3("Prognosis", style = "margin-top: 0; margin-bottom: 20px;"),
        uiOutput("result")   # <- must be uiOutput, not verbatimTextOutput
      )
      
    )
  )
)

# -----------------------
# Server
# -----------------------
server <- function(input, output, session) {
  
  output$result <- renderUI({
    input$calculate  # make reactive to button click
    
    # Use HTML() to output bold text
    HTML("<b>Predicted probability<br>of mortality at 36 months:<br></b>46% (CPI2 intermediate group)")
  })
}



# -----------------------
# Run the app
# -----------------------
shinyApp(ui, server)
