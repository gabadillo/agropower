output_main_panel <- mainPanel(
  width = 8,
  uiOutput("header_logos"), 
  tabsetPanel(
    id = "main_tabs",
    
    # --- MAIN TAB 1: Input Review ---
    tabPanel("Input Review & Summary",
             icon = icon("clipboard-list"),
             h3("Review of Calculated Parameters"),
             helpText("The parameters below are calculated based on your input and will be used for the simulation."),
             hr(),
             
             # Output where the processed parameters are shown
             verbatimTextOutput("input_summary")
    ),
    
    # --- MAIN TAB 2: Power Results (Containing Nested Tabs) ---
    tabPanel("Results", 
             icon = icon("chart-bar"),
             h3("Power Analysis Results"),
             
             # NESTED TABS START HERE
             tabsetPanel(
               id = "results_tabs",
               
               tabPanel("Summary Metrics", icon = icon("table"),
                        
                        # OUTPUT - RANKING 
                        fluidRow(
                          column(12,
                                 wellPanel(style = "border-top: 5px solid #2c3e50; background-color: rgba(255,255,255,0.95); border-radius: 8px; padding: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.05);",
                                           h4("Control Gene Ranking", align = "center", style = "font-weight: bold; color: #2c3e50;"),
                                           p("Ideally, the control gene would rank first in every simulation as having the strongest estimated effect.
This table shows how often it reached the top spot, as well as its mean and median rank across all iterations.", align = "center", style="color: #666;"),
                                           div(style = "width: 100%; margin-top: 15px;", 
                                               DT::DTOutput("display_ranking"))
                                 )
                          )
                        ),
                        
                        # SUBTLE SPACER
                        tags$div(style = "height: 10px;"),
                        
                        # ROW 2: Side-by-Side (Power and Coverage)
                        fluidRow(
                          column(6,
                                 wellPanel(style = "border-top: 5px solid #3498db; background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); min-height: 220px;",
                                           h4("Control is found significant", style = "color: #3498db; font-weight: bold;"),
                                           p("The control gene should always be detected as significantly different from zero.
However, there is still a risk of overestimating or underestimating its true biological effect.", style="color: #666; font-size: 0.9em;"),
                                           div(style = "width: 100%; margin-top: 10px;", 
                                               DT::DTOutput("display_significant"))
                                 )
                          ),
                          column(6,
                                 wellPanel(style = "border-top: 5px solid #9b59b6; background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); min-height: 220px;",
                                           h4("Control is found non-significant", style = "color: #9b59b6; font-weight: bold;"),
                                           p("Even when the test fails to detect the effect, we can track if the true value was at least captured by the 95% CI.", style="color: #666; font-size: 0.9em;"),
                                           div(style = "width: 100%; margin-top: 10px;", 
                                               DT::DTOutput("display_nonsignificant"))
                                 )
                          )
                        ),
                        
                        # ROW 3: Empirical Alpha (The Full Width Footer)
                        fluidRow(
                          column(12,
                                 wellPanel(style = "border-top: 5px solid black; background-color: white; padding: 25px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.05);",
                                           h4("Empirical Alpha - False Positive Rate", style = "color: black; font-weight: bold; margin-top: 0;"),
                                           p("This metric represents the overall Type I Error rate calculated from genes with no true biological effect. At the (\\alpha) = 0.05 level, we expect a 5% of false positives.", 
                                             style="color: #666; font-size: 0.9em;"),
                                           
                                           # Center the alpha box within the full-width card
                                           div(style = "width: 100%; display: flex; justify-content: center;",
                                               uiOutput("empirical_alpha_box"))
                                 )
                          )
                        )
               ),
               
               # NESTED TAB 2: Detailed Browser (Plots/Interactive View) 
               tabPanel("Summary Plots",
                        icon = icon("chart-bar"),
                        
                        wellPanel(style = "border-top: 5px solid black; background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); min-height: 220px;",
                                  h4("Control Estimates across Iterations", style = "color: black; font-weight: bold;"),
                                  p("The plot above shows the estimate and the 95% CI for each iteration. Iterations are ordered by estimated effect. The green line represents the true value", style="color: #666; font-size: 0.9em;"),
                                  div(style = "width: 100%; margin-top: 10px;", 
                                      plotOutput("detailed_plot"))
                        ),
               ), # END OUTPUT MIDDLE SUBTAB
               
               tabPanel(
                 "Simulation Browser",
                 icon = icon("table-columns"), 
                 value = "Simulation Browser",
                 fluidRow( 
                   column(3,
                     selectInput(
                       inputId = "sim_browser_select",
                       label = tags$strong("Select Simulation Run:"),
                       choices = c("Run simulation first" = ""))),
                   column(12,
                          tags$head(
                            tags$style(HTML("
                         /* Active (Selected) States */
                         .btn-check-C.active { background-color: #FF4500 !important; border-color: #FF4500 !important; color: white !important; }
                         .btn-check-E.active { background-color: #2E8B57 !important; border-color: #2E8B57 !important; color: white !important; }
                         .btn-check-G.active { background-color: #1E90FF !important; border-color: #1E90FF !important; color: white !important; }
                         .btn-check-X.active { background-color: #7FFFD4 !important; border-color: #7FFFD4 !important; color: black !important; }
                         .btn-check-B.active { background-color: #FF8C00 !important; border-color: #FF8C00 !important; color: white !important; }
                         .btn-check-R.active { background-color: #808080 !important; border-color: #808080 !important; color: white !important; }
                         
                         /* Inactive (Unselected) States */
                         .btn-check-C, .btn-check-E, .btn-check-G, .btn-check-X, .btn-check-B, .btn-check-R { 
                            opacity: 0.5; font-weight: bold; margin-bottom: 5px; 
                         }
                         .btn-check-C:hover, .btn-check-E:hover, .btn-check-G:hover, .btn-check-X:hover, .btn-check-B:hover, .btn-check-R:hover { 
                            opacity: 0.9; 
                         }
                       "))
                          ),
                          checkboxGroupButtons(
                            inputId = "sim_browser_layers",
                            label = tags$strong("Layers to Display:"),
                            choices = c(
                              "Gene Effect" = "C",
                              "Environment" = "E",
                              "Genotype" = "G",
                              "GxE Interaction" = "X",
                              "Block" = "B",
                              "Residual" = "R"
                            ),
                            selected = c("C","E","G","X","B","R"),
                            justified = TRUE,
                            individual = TRUE,
                            #checkIcon = list(yes = icon("layer-group")),
                            status = c("check-C", "check-E", "check-G", "check-X", "check-B", "check-R")
                          )
                   )
                 ),
                 fluidRow(
                   column(12,
                          plotOutput("simulation_browser_plot", height = "600px")
                   )
                 )
               ) 
             ) 
    ) 
  ) 
)