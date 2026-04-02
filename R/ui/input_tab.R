input_sidebar_panel <- sidebarPanel(
  width = 4,
  h3("Design & Parameters Input"),
  
  tabsetPanel(
    id = "input_tabs",
    
    # BEGIN INPUT - EXPERIMENTAL DESIGN
    tabPanel("Design",
             icon = icon("flask"),
             hr(),
             numericInput("g_count", label = "Number of Genotypes - (G)", 
                          value = input_defaults$G, min = 2, max = 100),
             numericInput("e_count", label = "Number of Environments - (E)", 
                          value = input_defaults$E, min = 2, max = 50),
             numericInput("b_count", label = "Number of Blocks per (E) - (B)", 
                          value = input_defaults$B, min = 2, max = 20)
    ),
    
    # BEGIN INPUT - VARIANCES
    tabPanel("Variances",
             icon = icon("calculator"),
             hr(),
             sliderInput("var_g", label = "Genetic Variance - (Vg)",
                         min = 0, max = 1, step = 0.01, value = input_defaults$VAR_G),
             sliderInput("var_e", label = "Environmental Variance - (Ve)",
                         min = 0, max = 1, step = 0.01, value = input_defaults$VAR_E),
             sliderInput("var_b", label = "Block Variance - (Vb)",
                         min = 0, max = 1, step = 0.01, value = input_defaults$VAR_B),
             sliderInput("var_gxe", label = "G(\\times)E Variance - (Vx)",
                         min = 0, max = 1, step = 0.01, value = input_defaults$VAR_GXE),
             sliderInput("var_res", label = "Residual Variance - (Vr)",
                         min = 0.01, max = 1, step = 0.01, value = input_defaults$VAR_RES)
    ),
    
    # BEGIN INPUT - GENES
    tabPanel("Treatments",
             icon = icon("dna"),
             hr(),
             numericInput("n_total", label = "Total Candidate Genes - (N)", 
                          value = input_defaults$N_GENES, min = 5, max = 100),
             sliderInput(
               inputId = 'beta', 
               label = 'Control Gene Effect Size - (S)',
               min = 0, max = 2, step = 0.05, 
               value = input_defaults$BETA
             )
    ),
    
    
    # BEGIN INPUT - ITERATIONS
    tabPanel("Simulation",
             icon = icon("cogs"),
             hr(),
             numericInput("nIter", label = "Number of Iterations - (M)", 
                          value = input_defaults$N_ITER, min = 10, max = 10000, step = 10),
             numericInput("seed", label = "Random seed - (P)", 
                          value = input_defaults$SEED)
    )
  ), # End tabsetPanel
  
  hr(),
  actionButton("run_simulation", "Run Power Analysis", icon = icon("bolt"), 
               class = "btn-success btn-lg"),
    tags$head(
      tags$script(HTML('
        Shiny.addCustomMessageHandler("updateLog", function(data) {
          var el = document.getElementById(data.target);
          if (el) {
            if (data.action === "addLog") {
              // This adds the new message on a new line without deleting the old ones
              el.insertAdjacentHTML("beforeend", "<div>" + data.content + "</div>");
              
              // Auto-scroll to the bottom so the latest step is always visible
              el.scrollTop = el.scrollHeight;
            } else if (data.action === "clearLogs") {
              el.innerHTML = "";
            }
          }
        });
      '))
    ),
  tags$div(
    id = "logs", 
    style = "
      margin-top: 15px; 
      padding: 12px; 
      background-color: #343a40;   /* Dark Slate Gray */
      color: #f8f9fa;              /* Off-white text */
      border: 1px solid #212529; 
      border-radius: 6px; 
      box-shadow: inset 0 2px 5px rgba(0,0,0,0.4); 
      height: 140px; 
      overflow-y: auto;
      font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
      font-size: 12px;
      line-height: 1.6;
      white-space: pre-wrap;    
    ",
    "" 
  )
)
