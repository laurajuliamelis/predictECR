# SHINY APP: Predict ECR - UPDATED

## Load libraries
library(shiny)
library(pROC)
library(ggplot2)
library(fastshap)
library(shapviz)
library(rms)
library(bslib)
library(thematic)
library(shinydashboard)
library(bsicons)

## Setup theming
bs_global_set(bs_theme(bootswatch = "flatly", base_font = font_google("Roboto")))
thematic::thematic_shiny()

## Load model and normalization parameters
load("data/model_objects.Rdata") # includes rms.clin, cutoff.clin, res.pca, wcst_peps

## UI
ui <- navbarPage(
  title = "FarmaPRED-PEP: Early Clinical Recovery (ECR) Predictor",
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Roboto"), heading_font = font_google("Poppins")),
  
  tabPanel("Predictive Tool",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 tags$h3("Patient Inputs", style = "font-weight: bold; color: #3b5773;"),
                 numericInput("DUP", "Duration of untreated psychosis (DUP)", value = 0, min = 0, max = 800),
                 numericInput("DTP", "Days of treated psychosis (DTP)", value = 0, min = 0, max = 400),
                 sliderInput("EEAG", "Global Assessment of Functioning (GAF) scale", value = 10, min = 1, max = 100, step = 1),
                 numericInput("PAS_infancia", "PAS (childhood)", value = 0, step = 0.1),
                 numericInput("PAS_adolescencia", "PAS (early adolescence)", value = 0, step = 0.1),
                 numericInput("Educacion", "Education (years)", value = 0, step = 1),
                 numericInput("CI_estimado", "Estimated premorbid IQ", value = 100, step = 1),
                 sliderInput("Insight", "Insight (1 = good, 7 = poor)", min = 1, max = 7, value = 1, step = 1),
                 numericInput("Perseveratives", "WCST: Perseverative responses", value = 0, min = 0, step = 1),
                 actionButton("predict", "Enter", class = "btn btn-primary")
               ),
               mainPanel(
                 tags$strong("WARNING: Tool for research use only", style = "color: #db3d16; font-weight: bold; text-align:center;vertical-align: middle"),
                 tags$h2("Welcome to the ECR Predictor", style = "color: #3498db; font-weight: bold;"),
                 tags$p("This tools is created to easily estimate the probability of early clinical recovery (ECR). More details can be found in the publication: ",
                        tags$a(href = "https://www.google.com/", target = "_blank", "TO DO")),
                 p(strong("Please, use the left panel to enter your patient characteristics and view the predicted response class.")),
                 
                 tags$h2("Results", style = "color: #2980b9; font-weight: bold;"),
                 # LAYOUT: CONTINU
                 # tags$h4("1. Prediction"),
                 # fluidRow(
                 #   column(7, uiOutput("class_result")),
                 #   column(5, uiOutput("prob_result"))
                 # ),
                 
                 # tags$h4("2. Performance of predicted ECR probability in your patient"),
                 # tags$p("This density plot helps interpret the estimated probability of Early Clinical Recovery (ECR) for your patient by comparing it with the distribution of predicted probabilities from the reference dataset. The plot shows the probability densities for patients classified as ECR and non-ECR by the model. Your patient's predicted probability is represented by a vertical line. If this line falls closer to the ECR curve, it suggests a higher similarity to those who responded early. This visual comparison provides contextual insight into the model’s prediction."),
                 # plotOutput("density_plot"),
                 # 
                 # tags$h4("3. Variable contribution to the predicted probability"),
                 # tags$p("The SHAP force plot visually breaks down how each individual predictor contributes to the estimated probability of Early Onset Response (ECR) for your patient. Variables shown in green push the prediction higher (towards ECR), while those in red lower it (towards non-ECR). The size of each bar reflects the strength of that variable’s influence. This allows you to understand not just the outcome, but why the model reached that conclusion—highlighting which patient-specific features are most relevant to the prediction."),
                 # plotOutput("force_plot")
                 
                 # LAYOUT: TABS
                 # navset_card_tab( 
                 #   nav_panel("Prediction", 
                 #             fluidRow(
                 #               column(7, uiOutput("class_result")),
                 #               column(5, uiOutput("prob_result"))
                 #             ),), 
                 #   nav_panel("Performance of predicted ECR probability in your patient",
                 #             tags$p("This density plot helps interpret the estimated probability of Early Clinical Recovery (ECR) for your patient by comparing it with the distribution of predicted probabilities from the reference dataset. The plot shows the probability densities for patients classified as ECR and non-ECR by the model. Your patient's predicted probability is represented by a vertical line. If this line falls closer to the ECR curve, it suggests a higher similarity to those who responded early. This visual comparison provides contextual insight into the model’s prediction."),
                 #             plotOutput("density_plot")), 
                 #   nav_panel("Variable contribution", 
                 #             tags$p("The SHAP force plot visually breaks down how each individual predictor contributes to the estimated probability of Early Onset Response (ECR) for your patient. Variables shown in green push the prediction higher (towards ECR), while those in red lower it (towards non-ECR). The size of each bar reflects the strength of that variable’s influence. This allows you to understand not just the outcome, but why the model reached that conclusion—highlighting which patient-specific features are most relevant to the prediction."),
                 #             plotOutput("force_plot"))
                 # )
                 
                 # LAYOUT: ACCORDION
                 accordion(  
                   accordion_panel( 
                     title = "Prediction", 
                     icon = bsicons::bs_icon("1-circle-fill"),
                     fluidRow(
                       column(7, uiOutput("class_result")),
                       column(5, uiOutput("prob_result")))
                   ),  
                   accordion_panel(
                     title = "Performance of predicted ECR probability in your patient",
                     icon = bsicons::bs_icon("2-circle-fill"),
                     tags$p("This density plot helps interpret the estimated probability of Early Clinical Recovery (ECR) for your patient by comparing it with the distribution of predicted probabilities from the reference dataset. The plot shows the probability densities for patients classified as ECR and non-ECR by the model. Your patient's predicted probability is represented by a vertical line. If this line falls closer to the ECR curve, it suggests a higher similarity to those who responded early. This visual comparison provides contextual insight into the model’s prediction."),
                     plotOutput("density_plot")
                   ),  
                   accordion_panel(
                     title = "Variable contribution to the predicted probability",
                     icon = bsicons::bs_icon("3-circle-fill"),
                     tags$p("The SHAP force plot visually breaks down how each individual predictor contributes to the estimated probability of Early Onset Response (ECR) for your patient. Variables shown in green push the prediction higher (towards ECR), while those in red lower it (towards non-ECR). The size of each bar reflects the strength of that variable’s influence. This allows you to understand not just the outcome, but why the model reached that conclusion—highlighting which patient-specific features are most relevant to the prediction."),
                     plotOutput("force_plot")
                   ),  
                   id = "acc",  
                   open = c("Prediction", "Performance of predicted ECR probability in your patient", "Variable contribution to the predicted probability")
                 ) 
               )
             )
           )
  ),
  
  tabPanel("Details",
           fluidPage(
             tags$h2("PEPS Sample", style = "color: #2980b9; font-weight: bold;"),
             tags$p("The dataset used to develop the model comprised individuals from the PEPs cohort (N=335) from the study “Genotype-Phenotype Interaction and Environment. Application to a Predictive Model in First Psychotic Episodes” (PI08/0208), which can be found at ",
                    tags$a(href = "https://doi.org/10.1016/j.rpsm.2012.11.001", target = "_blank", "https://doi.org/10.1016/j.rpsm.2012.11.001"), ". Participants were recruited between April 2009 and April 2011 across 16 clinical centres throughout Spain."),
             tags$p("The inclusion criteria of the patients were:"),
             tags$ul(
               tags$li("Age between 7 and 35 years at the time of the first evaluation."),
               tags$li("Presence of psychotic symptoms lasting less than 12 months"),
               tags$li("Speaking Spanish correctly"),
               tags$li("Signing the informed consent")
             ),
             tags$p("The exclusion criteria for the patients were: mental retardation according to the criteria of the DSM-IV60 (which includes, in addition to an intellectual coefficient below 70, functional problems), a history of head trauma with loss of consciousness and organic disease with mental repercussions."),
             tags$ul(
               tags$li("Mental retardation according to the criteria of the DSM-IV60 (which includes, in addition to an intellectual coefficient below 70, functional problems)"),
               tags$li("History of head trauma with loss of consciousness"),
               tags$li("Organic disease with mental repercussions"),
             ),
             tags$h2("Model-building details", style = "color: #2980b9; font-weight: bold;"),
             tags$p("The outcome for this study was early clinical recovery (ECR), defined as achieving both symptomatic and functional remission one year after study inclusion. The predictor variables were selected using penalised logistic regression using the least absolute shrinkage and selection operator (LASSO) and missing data were handled using Multiple Imputation by Chained Equations (MICE). The predictive tool was developed using the logistic regression model on the entire development dataset (PEPs cohort) and predicted probabilities were computed using the inverse logit of the linear predictor. Model performance was evaluated on across three complementary dimensions: discrimination (optimism-corrected AUC), calibration (calibration plot and slope curve estimation), and clinical utility (decision curve analysis). The full analysis code is publicly available in the GitHub repository ", tags$a(href = "https://github.com/laurajuliamelis/ECR-prediction-models", target = "_blank", "https://github.com/laurajuliamelis/ECR-prediction-models"), "."),
             tags$p("The six predictive variables in the model are the following:"),
             tags$ul(
               tags$li("Duration of untreated psychosis (DUP) in days,"),
               tags$li("Duration of treated psychosis (DTP) in days,"),
               tags$li("Global Assessment of Functioning (GAF) score,"),
               tags$li("Cognitive reserve*, "),
               tags$li("Insight (from PANSS item G12) and"),
               tags$li("Executive function: Wisconsin Card Sorting Test (WCST) - perseverative responses")
             ),
             tags$p("*Cognitive reserve was summarised  via Principal Component Analysis (PCA) on the standardised varianles estimated IQ, education (years), and premorbid adjustment scores (childhood plus early adolescence). Also, executive function values were normalized to z-scores ussing the values from a control population in the PEPS cohort.")
           )
  ),
  
  tabPanel("Credits and Funding",
           br(),
           tags$h3("About the FarmaPRED-PEP Project", style = "color: #3498db; font-weight: bold;"),
           tags$p("This application was developed within the FarmaPRED-PEP project. The FarmaPRED-PEP project aims to develop predictive models of early treatment response to antipsychotics in patients experiencing a first episode of psychosis (FEP). This work integrates clinical, neurocognitive, and environmental data to provide clinicians with accessible tools to assist in early decision-making."),
           tags$p("The project was supported by the Instituto de Salud Carlos III (ISCIII), co-funded by the European Union, and carried out by a multidisciplinary team from the University of Barcelona, IDIBAPS, and CIBERSAM."),
           tags$p("This tool is intended solely for research use and should not replace clinical judgment."),
           
           tags$h4("Contact us", style = "color: #3498db; font-weight: bold;"),
           tags$p("For questions, comments, or feedback regarding this tool or the publication, please contact us at", tags$a(href = "mailto:sergimash@ub.edu?subject=FarmaPRED-PEP%20Tool", target = "_blank", "sergimash@ub.edu"), "and", tags$a(href = "mailto:laurajulia@ub.edu?subject=FarmaPRED-PEP%20Tool", target = "_blank", "laurajulia@ub.edu"), ". If you are interested in collaborating with us, do not hesitate to reach out!")
  ),
  
  tags$footer(
    tags$div(
      fluidRow(
        column(4, img(src = "logo_ub.png", height = "60px")),
        column(4, img(src = "logo_idibaps.png", height = "60px")),
        column(4, img(src = "logo_isciii.png", height = "60px"))
      ),
      br(),
      HTML("Code available at <a href='https://github.com/laurajuliamelis/predictECR' target='_blank'><i class='bi bi-github'></i> GitHub</a>"),
      actionButton("github_btn", label = NULL, icon = icon("github"),
                   onclick = "window.open('https://github.com/laurajuliamelis/predictECR', '_blank')",
                   style = "background-color: transparent; border: none; color: #333; font-size: 20px; margin-top: 10px;"),
      style = "width: 100%; color: black; text-align: center;"
    )
  )
)

## SERVER
server <- function(input, output, session) {
  observeEvent(input$predict, {
    wcst_z <- (input$Perseveratives - wcst_peps$mean) / wcst_peps$sd
    
    newdata <- data.frame(
      EstimacionCI2meses = input$CI_estimado,
      añoseducación = input$Educacion,
      PAS_infancia_adolescencia = (input$PAS_infancia + input$PAS_adolescencia) / 2
    )
    vars <- c("EstimacionCI2meses","añoseducación","PAS_infancia_adolescencia")
    for (i in vars) {
      newdata[[i]] <- (newdata[[i]] - scale_params[i,"Media"]) / scale_params[i,"SD"]
    }
    cognitive_reserve <- predict(res.pca, newdata = newdata)[[1]]
    
    new_patient <- data.frame(
      DUP = input$DUP,
      DTP = input$DTP,
      functionality = input$EEAG,
      cognitive_reserve = cognitive_reserve,
      insight = input$Insight,
      executive_function = wcst_z
    )
    
    prob <- predict(rms.clin, newdata = new_patient, type = "fitted")
    
    output$class_result <- renderUI({
      value_box(title = "Patient class",
                value = if (prob > cutoff.clin) "ECR" else "Non-ECR",
                theme = value_box_theme(bg = if (prob > cutoff.clin) {"#00a86b"} else {"#D22B2B"}, fg = "#FFFFFF"),
                showcase = if (prob > cutoff.clin) bsicons::bs_icon("person-fill-check") else bsicons::bs_icon("person-fill-x"),
                showcase_layout = "left center",
                full_screen = FALSE, fill = TRUE)
    })
    
    output$prob_result <- renderUI({
      value_box(title = "Probability of ECR", value = paste0(round(prob * 100, 2), "%"),
                theme = value_box_theme(bg = "#f4f4f4", fg = "#989898"),
                showcase = bsicons::bs_icon("calculator"), showcase_layout = "top right",
                full_screen = FALSE, fill = TRUE)
    })
    
    data.clin.imp$PredResponse <- factor(data.clin.imp$PredResponse, levels = c(0, 1), labels = c("Non-ECR", "ECR"))
    
    output$density_plot <- renderPlot({
      ggplot(data.clin.imp, aes(x = Prediction, fill = PredResponse, color = PredResponse)) +
        geom_density(alpha = 0.2) +
        geom_vline(aes(xintercept = prob, color = "Patient"), linetype = "solid", linewidth = 1) +
        annotate("text", x = prob + 0.04, y = 3.8, label = paste0("p=", round(prob, 3)),
                 vjust = -0.5, hjust = 0.5, color = "coral", size = 4) +
        labs(x = "Probability", fill = "Class", y = "density") +
        scale_color_manual(name = NULL, values = c("Patient" = "coral")) +
        guides(color = guide_legend(override.aes = list(linetype = "solid"))) +
        theme_classic() + theme(axis.text = element_text(size = 12),
                                axis.title = element_text(size = 14, face = "bold"),
                                legend.text = element_text(size = 12),
                                legend.title = element_text(size = 14, face = "bold"))
    })
    
    set.seed(123)
    ex.clin <- explain(rms.clin, X = X, pred_wrapper = pfun, newdata = new_patient, nsim = 500, adjust = TRUE, shap_only = F)
    shv <- shapviz::shapviz(ex.clin)
    
    output$force_plot <- renderPlot({
      set.seed(123)
      sv_force(shv, row_id = 1, fill_colors = c("#9ce991", "#e9919c"),
               contrast = FALSE,
               bar_label_size = 5,
               show_annotation = TRUE,
               annotation_size = 4) +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold"))
    })
  })
}

## Run the app
shinyApp(ui = ui, server = server)
