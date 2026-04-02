
# --- Source UI Component Files ---
source("R/ui/input_defaults.R")     # Defines 'input_defaults'
source("R/ui/input_tab.R")          # Defines 'input_sidebar_panel'
source("R/ui/output_tab.R")         # Defines 'output_main_panel'

ui <- fluidPage(
  #setBackgroundColor(color = "lightblue"),
  tags$head(
    tags$script(
      type = "text/x-mathjax-config",
      "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});"
    )
  ),
  
  # 2. LINK THE EXTERNAL CSS FILE HERE
  tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles.css"),
  
  withMathJax(), # latex display
  titlePanel(
    list(
      tags$img(src = "logo.png", height = "60px", style = "margin-right: 15px;"),
      tags$span("agroPower", style = "font-size: 48px; font-weight: bold; color: white;")
    )
  ),
  uiOutput("right_aligned_logos"),

  # 3. Sidebar Layout (The main content area)
  sidebarLayout(
    input_sidebar_panel,  
    output_main_panel
  )
)
