source('R/utils/functions.R')

server <- function(input, output, session) {
  
  r_values <- reactiveValues(
    results = NULL, # display
    sim_ids = NULL, # dropdown for simulation browser
    gene_ids = NULL, # dropdown for control display
    sim_path = NULL # folder with the current data
  )
  
  # --- 4.2 Reactive Input Parameter Generation ---
  processed_params <- reactive({
    tryCatch({
    
      params <- generate_design_params(
        g_count = input$g_count,
        e_count = input$e_count,
        b_count = input$b_count,
        n_total = input$n_total,
        beta = input$beta,
        var_g = input$var_g,
        var_e = input$var_e,
        var_b = input$var_b,
        var_gxe = input$var_gxe, 
        var_res = input$var_res,
        nIter = input$nIter,
        seed = input$seed
      )
      
      return(params)
      
    }, error = function(e) {
      showNotification(
        paste("Input Error:", e$message),
        type = "error",
        duration = 5
      )
      return(NULL)
    })
  })
  
  # --- 4.3 Output: Input Summary Tab ---
  
  output$right_aligned_logos <- renderUI({
    tags$div(
      # 1. FLOAT: Pushes the whole group to the right
      # 2. MARGIN-TOP: Pulls the group up into the titlePanel's vertical space.
      style = "float: right; margin-top: -80px; margin-right: 15px;", 
      
      # Logo 1 (CTC) - Use margin-right to space it from the other logo
      tags$img(
        src = "logo_CTC.png", 
        height = "130px", 
        style = "margin-right: 10px;"
      ),
      
      # Logo 2 (IFAS)
      tags$img(
        src = "IFAS-RGB.png", 
        height = "70px"
      )
    )
  })
  
  # SERVER-SIDE CODE (within the server function)
  output$input_summary <- renderPrint({
    params <- processed_params()
    req(params) 
    
    cat("--- DESIGN ---\n")
    print(data.frame(
      Var = c('Number of genotypes             ',
              'Number of environments          ',
              'Number of blocks per environment'),
      Symbol = c('N_g', 'N_E', 'N_B'),
      Value = unname(unlist(params$Design))
    ))
    
    cat("\n--- VARIANCES ---\n")
    print(data.frame(
      Var = c('Genetic variance      ',
              'Environmental variance',
              'Block variance        ',
              'GxE variance          ',
              'Residual variance     ',
              'Total variance        '),
      Symbol = c("s2_g", "s2_E", "s2_B", "s2_gxE", "s2_res", "s2_y"),
      Value = unname(unlist(params$Variances))
    ))
    
    cat("\n--- TREATMENT ---\n")
    print(data.frame(
      Var = c('Number of treatments        ',
              'Positive control effect size'),
      Symbol = c('N_C', 'S'),
      Value = unname(unlist(params$Genetic))
    ))
    
    cat("\n--- SIMULATION ---\n")
    print(data.frame(
      Var = c('Number of iterations',
              'Random seed         '),
      Symbol = c('M', 'P'),
      Value = unname(unlist(params$Simulation))
    ))
  })
  
  observeEvent(input$run_simulation, {
    
    # 1. Require that the essential parameters list is available
    req(processed_params())
    # 2. Extract parameters and show a status message while processing
    params <- processed_params()
    nIter = params$Simulation$Iterations
    seed = params$Simulation$Seed
    #save(params, file = 'PARAMS.RData') # debugging
    logging(session, sprintf("Running %s iterations \n with seed %s", nIter, seed), t = 2)
    # Create the directory R/tmp
    sim_dir <- file.path("R", "tmp")
    if (dir.exists(sim_dir)) {
      unlink(sim_dir, recursive = TRUE)
    }
    dir.create(sim_dir, recursive = TRUE)
    # loop over iterations using map
    # STEP 1. Simulate data
    logging(session, "Step 1/3: Simulating data...")
    
    set.seed(seed)
    path = purrr::map(1:params$Simulation$Iterations, ~run_simulation(params, ., sim_dir))
    
    # STEP 2. Fitting mixed model 
    logging(session, "Step 2/3: Fitting mixed model...")
    sim_ids = purrr::map(path,                    ~fit_simulation(params, .))

    # STEP 3. Summarizing metrics
    logging(session, "Step 3/3: Summarizing metrics...", t = 1)
    results = aggregate_results(params, sim_dir)

    logging(session, "Done!")

    r_values$gene_ids = names(results$plots)
    r_values$sim_path = sim_dir 
    r_values$results = results
    r_values$sim_ids = sim_ids
    
    display_names <- paste("Simulation", sim_ids)
    updateSelectInput(session, "sim_browser_select", 
                      choices = setNames(sim_ids, display_names),
                      selected = sim_ids[1])
    updateTabsetPanel(session, "results_tabs", selected = "Summary Metrics")
  })
  
  results <- reactive({
    req(r_values$results)
    return(r_values$results)
  })
  
  # Renders the table for the "Summary Metrics" tab
  output$display_ranking <- DT::renderDT({
    req(results()) 
    display <- results()$ranking
    DT::datatable(display,
                  rownames = FALSE, width = "100%",
                  options = list(dom = 't', autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })
  
  output$display_significant <- DT::renderDT({
    req(results()) 
    display <- results()$power.significant
    DT::datatable(display,
                  rownames = FALSE, width = "100%",
                  options = list(dom = 't', autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })
  
  output$display_nonsignificant <- DT::renderDT({
    req(results()) 
    display <- results()$power.nonsignificant
    DT::datatable(display,
                  rownames = FALSE, width = "100%",
                  options = list(dom = 't', autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })
  
  output$empirical_alpha_box <- renderUI({
    req(results()) 
    
    empirical_alpha <- results()$significance
    target_alpha = 0.05
    threshold = 0.5
    lower_threshold = target_alpha * (1-threshold) 
    upper_threshold = target_alpha * (1+threshold)
    is_controlled = empirical_alpha >= lower_threshold && empirical_alpha <= upper_threshold
    
    box_color = if (is_controlled) "#1B9E77" else "#D95F02" # Green for good, Orange for warning/failure
    box_icon = if (is_controlled) icon("check-circle") else icon("exclamation-triangle")
    
    status_message <- ""
    
    if (is_controlled) {
      status_message <- sprintf("Result (%.4f) is well-controlled and close to the target α = %.2f.", empirical_alpha, target_alpha)
    } else if (empirical_alpha > upper_threshold) {
      status_message <- sprintf("Warning: Empirical Alpha (%.4f) is too high. The Type I Error rate exceeds the tolerance (above %.4f), indicating too many False Positives.", empirical_alpha, upper_threshold)
    } else { # empirical_alpha < lower_threshold
      status_message <- sprintf("Warning: Empirical Alpha (%.4f) is too low. The model is overly conservative (below %.4f) and may be suffering from low Power.", empirical_alpha, lower_threshold)
    }
    
    shiny::tagList(
      div(
        style = paste0(
          "padding: 10px; margin-bottom: 15px; border: 1px solid #ddd; ",
          "border-left: 5px solid ", box_color, ";",
          "background-color: #ffffff; border-radius: 4px; box-shadow: 0 1px 1px rgba(0,0,0,0.05);"
        ),
        
        div(
          style = "display: flex; align-items: center;",
          
          # Icon on the left
          span(box_icon, style = paste0("font-size: 1.5em; color: ", box_color, "; margin-right: 15px;")),
          
          # Content on the right
          div(
            p(
              style = "font-size: 1.2em; margin-bottom: 2px;",
              strong("Empirical Alpha:"),
              span(
                style = "font-weight: bold;",
                sprintf(" %.4f", empirical_alpha)
              )
            ),
            p(
              style = "margin: 0; font-size: 0.9em; color: #666;",
              status_message
            )
          )
        )
      )
    )
  })
  
  observe({
    # CRITICAL: Wait for the gene IDs to be populated after the sim runs
    req(r_values$gene_ids) 
    
    # Update the selectInput element named "selected_gene"
    updateSelectInput(
      session, 
      "selected_gene", 
      label = "Select Gene to View Detailed Plot:",
      choices = r_values$gene_ids,
      # Set the first gene as the default selection
      selected = r_values$gene_ids[1] 
    )
  })
  
  output$detailed_plot <- renderPlot({
    selected_gene <- input$selected_gene
    return(results()$plots[[1]])
  })
  
  # Reactive expression to generate the browser plot when the selection changes
  sim_browser_plot_react <- reactive({
    
    # 1. Require selection and the reactive path/params to be ready
    req(input$sim_browser_select)
    req(r_values$sim_path) # Require the path stored in the reactive value
    params <- processed_params()
    
    selected_sim_id <- input$sim_browser_select
    active_layers <- input$sim_browser_layers 
    logic_layers = c('C','E', 'G', 'X', 'B', 'R') %in% active_layers
    # 2. Call the plot generation function
    if (selected_sim_id != "") {
      
      # Use the user's correct reactive value for the file path: rv$sim_path
      browser_sim(
        sim = selected_sim_id,
        path = r_values$sim_path, 
        params = params,
        layers = logic_layers
      )
      
    } else {
      # Placeholder plot when nothing is selected
      ggplot() + 
        geom_text(aes(x=0, y=0, label="Select a simulation run to view the effect map.")) + 
        theme_void()
    }
  })
  
  # Render the plot for the Simulation Browser tab
  output$simulation_browser_plot <- renderPlot({
    sim_browser_plot_react()
  })
  
}



