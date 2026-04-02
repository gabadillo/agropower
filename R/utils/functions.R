library(shiny)
library(dplyr) 
library(shinyWidgets)
library(tidyverse)
library(DT)

paddingZeros = function(n){
  digits = function(x) floor(log10(x))+1
  zeros = do.call('c', map(digits(max(n)) - digits(1:n), ~ paste(rep(0,.), collapse = '')))
  return(paste0(zeros, 1:n))
}

generate_design_params = function(g_count, e_count, b_count, 
                                  n_total, beta,
                                  var_g, var_e, var_b, var_gxe, var_res, 
                                  nIter, seed) {
  
  k_controls = 1
  if (k_controls > n_total) {
    stop("The number of control genes (K) cannot exceed the total genes (N).")
  }
  if (any(c(g_count, e_count, b_count, n_total, nIter) < 1)) {
    stop("Counts must be positive integers.")
  }
  if (any(c(var_g, var_e, var_b, var_gxe, var_res) < 0)) {
    stop("Variance components must be non-negative.")
  }
  
  gene_names = 'Control_beta'
  beta_list_elements = setNames(beta, gene_names)
  
  experimental_design = list(
    Genotypes = g_count,
    Environments = e_count,
    Blocks_per_E = b_count
  )
  
  genetic_model = c(
    list(
      Total_Genes = n_total,
      Beta_Control = beta
    )
  )
  
  variance_components = list(
    G = var_g,
    E = var_e,
    Block = var_b,
    GxE = var_gxe,
    Residual = var_res,
    Total_Phenotypic_Var = var_g + var_e + var_b + var_gxe + var_res
  )
  
  simulation_params = list(
    Iterations = nIter,
    Seed = seed
  )
  
  return(list(
    Design = experimental_design,
    Genetic = genetic_model,
    Variances = variance_components,
    Simulation = simulation_params
  ))
}


run_simulation = function(params, iteration, sim_dir) {
  
  nGID    = params$Design$Genotypes
  nEID    = params$Design$Environments
  nBlock  = params$Design$Blocks_per_E
  nGenes  = params$Genetic$Total_Genes
  nControls = 1

  sGID = diag(nGID) * params$Variances$G
  dfGID = data.frame(
    GID  = paste0('G', paddingZeros(nGID)),
    Gval = t(mvtnorm::rmvnorm(1, sigma = sGID))
  )
  
  sEID = diag(nEID) * params$Variances$E
  dfEID = data.frame(
    EID  = paste0('E', paddingZeros(nEID)),
    Eval = t(mvtnorm::rmvnorm(1, sigma = sEID))
  )
  
  sBlock = diag(nBlock) * params$Variances$Block
  sBlock = kronecker(sEID, sBlock)
  dfBlock = data.frame(Block = paste0('B', paddingZeros(nBlock)))
  dfBlock = cross_join(dfEID, dfBlock) %>% dplyr::select(-Eval) %>% 
    mutate(Bval = as.vector(t(mvtnorm::rmvnorm(1, sigma = sBlock))))
  
  sGXE = kronecker(sEID, sGID)
  dfGXE = cross_join(dfEID, dfGID) %>%  
    mutate(GXEval = as.vector(t(mvtnorm::rmvnorm(1, sigma = sGXE))))
  
  # scale phenotype and add treatment
  true_sd = 1
  true_mu = 0
  # Gene effect
  beta = params$Genetic$Beta_Control
  dfGenes = data.frame(
    GeneID = paste0('Gene', paddingZeros(nGenes)),
    Effect = c(beta*true_sd, rep(0, nGenes - nControls))
  )
  
  df = dfGXE %>% 
    right_join(dfBlock, relationship = "many-to-many", by = "EID") %>% 
    mutate(GeneID = sample(dfGenes$GeneID, n(), replace = TRUE)) %>%  
    left_join(dfGenes, by = "GeneID") %>% 
    mutate(Rval = rnorm(n(), sd = sqrt(params$Variances$Residual)))

  v_total = sum(apply(df[,c('Gval', 'Eval', 'GXEval', 'Bval', 'Rval')], 2, var))
  k = true_sd / sqrt(v_total)
  
  df = df %>%
    mutate(
      Gval = Gval * k,
      Eval = Eval * k,
      GXEval = GXEval * k,
      Bval = Bval * k,
      Rval = Rval * k,
      y = Gval + Eval + GXEval + Bval + Rval + Effect + true_mu
    )

  # save file
  max_M = params$Simulation$Iterations
  padded_run_number = paddingZeros(max_M)[iteration] 
  file_name = paste0("sim_", padded_run_number, ".csv")
  file_path = file.path(sim_dir, file_name)
  
  write_csv(df, file_path)
  return(file_path)
}


fit_simulation = function(params, file, alpha = 0.05) {
  
  trueBetas = params$Genetic$Beta_Control
  nControls = 1
  nZeros = floor(log10(params$Genetic$Total_Genes))
  za2 = qnorm(1 - alpha/2)
  
  sim = str_match(file, 'sim_(.+).csv')[,2]
  df = read_csv(file, show_col_types = FALSE)
  
  fm = lmerTest::lmer(y ~ -1 + GeneID + (1|GID) + (1|EID) + (1 | EID:Block) + (1 | GID:EID), data = df, REML = TRUE)
  b = coef(summary(fm))
  b[,1] = scale(b[,1], scale = FALSE)
  ranks = rank(-b[,1])
  mean(df$y)
  out_control_list = list()
  i = 1
    geneID = sprintf('GeneIDGene%s%s', paste(rep('0',nZeros), collapse = ''), i) 
    # Safety Check: Handle cases where the control gene is missing
    if (! geneID %in% rownames(b)){
      add = matrix(c(0, 0, 0, 0, 1), nrow = 1)
      rownames(add) = geneID
      colnames(add) = colnames(b)
      b = rbind(add, b)
      b = b[sort(rownames(b)),]
      
      ranks = c(ranks, length(ranks)+1)
      names(ranks[length(ranks)]) = geneID
      ranks = rank(-b[,1])
      ranks[1] = 0
    }

    curr = data.frame(sim = sim, geneID = geneID, 
                      bHat = b[geneID,1], SE = b[geneID,2], rank = ranks[geneID])
    curr$covered = as.numeric(trueBetas[i] < curr$bHat + za2*curr$SE && trueBetas[i] > curr$bHat - za2*curr$SE) 
    curr$significant = as.numeric(curr$bHat - za2*curr$SE > 0 | curr$bHat + za2*curr$SE < 0)
    out_control_list[[i]] = curr
  df_controls_sim = bind_rows(out_control_list)
  
  # FALSE POSITIVES
  H0 = nrow(b) - nControls
  H1 = sum(b[-c(1:nControls), 5] < alpha)
  
  df_alpha_sim = data.frame(sim = sim, H0 = H0, H1 = H1, alpha = H1/H0)
  
  file_path = gsub('sim', 'out', file)
  write_csv(df_controls_sim, file_path)
  file_path = gsub('sim', 'alp', file)
  write_csv(df_alpha_sim, file_path)
  return(sim)
}


# Function to aggregate raw simulation results, calculate summary metrics
# and generate a estimated effects plot
aggregate_results = function(params, sim_dir, alpha = 0.05){
  
  path = dir(sim_dir, full.names = TRUE)
  out = lapply(path[grepl('out', path)], function(p) read_csv(p, col_types = cols(sim = "c"))) %>% bind_rows()
  alp = lapply(path[grepl('alp', path)], function(p) read_csv(p, col_types = cols(sim = "c"))) %>% bind_rows()
  
  beta = params$Genetic$Beta_Control
  gene = unique(out$geneID)
  geneTrue = data.frame(gene = gene, beta = beta)
  
  totalGenes = params$Genetic$Total_Genes
  za2 = qnorm(1 - alpha/2)

  nIter = nrow(out)
  out.ranking = out %>% 
    summarize(
      n.Top1 = sum(rank == 1),
      Mean.Rank = round(mean(rank), 2),
      Median.Rank = quantile(rank, 0.5))
  
  out.significant = out %>% 
    filter(significant == 1) %>% 
    mutate(class = ifelse(covered == 1, 'Correct', ifelse(bHat > beta, 'Overestimated', 'Underestimated'))) %>%
    mutate(class = factor(class, levels = c('Correct', 'Overestimated', 'Underestimated')))
  
  out2.significant = as.data.frame.list(table(out.significant$class))
  
  out.nonsignificant = out %>% 
    filter(significant == 0) %>% 
    mutate(class = ifelse(covered, 'Covering', 'Non-covering')) %>%
    mutate(class = factor(class, levels = c('Covering', 'Non-covering')))
  
  out2.nonsignificant = as.data.frame.list(table(out.nonsignificant$class))
  
  out = rbind(out.significant, out.nonsignificant)
  sim_factor = out %>% arrange(-bHat) %>% pull(sim)
  out$sim = factor(out$sim, levels = sim_factor)
  
  plot_list = list()
  plotID = 'Control Gene'
  #za2 = 1 # uncomment for error bars representing SE instead of 95% CI
  
  ghost_data = data.frame(
    sim = factor(levels(out$sim)[1], levels = levels(out$sim)), bHat = 0, 
    class = factor(
      c('Significant (Correct)', 'Significant (Overestimation)', 'Significant (Underestimation)', 
        'Non-significant (covering true value)', 'Non-significant (not covering true value)'),
      levels = c('Significant (Correct)', 'Significant (Overestimation)', 'Significant (Underestimation)', 
                 'Non-significant (covering true value)', 'Non-significant (not covering true value)')
    )
  )
  
  plot_list[[plotID]] = 
    out %>%
    mutate(class = factor(class, 
                          levels = c('Correct', 'Overestimated', 'Underestimated', 'Covering', 'Non-covering'),
                          labels = c('Significant (Correct)', 'Significant (Overestimation)', 'Significant (Underestimation)', 
                                     'Non-significant (covering true value)', 'Non-significant (not covering true value)'))) %>% 
    ggplot(aes(x = sim, y = bHat, color = class)) +
    # ghost layer
    geom_point(data = ghost_data, aes(x = sim, y = bHat, color = class), alpha = 0, show.legend = TRUE) + 
  geom_point(size = 3, aes(shape = rank == 0)) +
    geom_text(aes(y = (bHat + za2*SE + 0.15), label = ifelse(rank == 0, '', rank)), size = 5, show.legend = FALSE) +
    geom_errorbar(aes(ymin = bHat - za2*SE, ymax = bHat + za2*SE), width = 0) +
    geom_hline(yintercept = beta, color = '#1B9E77') +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    labs(x = 'Simulation ID',
         y = sprintf('Estimated effect (%s%% CI)', 100*(1-alpha)),
         color = '') +
    scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
    ggpubr::theme_pubclean() +
    theme(legend.position = 'top',
          legend.background = element_rect(fill = "#F7F7F7", color = "grey50", linewidth = 0.5),
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 18, face = 'bold'),
          axis.title = element_text(size = 12, face = 'bold')) +
    scale_color_manual(
      values = c('Significant (Correct)' = '#102542', 
                 'Significant (Overestimation)' = '#FCB8B1', 
                 'Significant (Underestimation)' = '#891306',
                 'Non-significant (covering true value)' = '#98c1d9',
                 'Non-significant (not covering true value)' = '#f87060'),
      drop = FALSE
    ) +
    scale_shape_manual(values = c(19, 4))+
    guides(shape = 'none',
      color = guide_legend(
      nrow = 2, 
      byrow = TRUE,
      override.aes = list(alpha = 1, size = 4)))
  
  # Calculate the overall empirical alpha (Type I Error Rate) across all simulations
  # Alpha = Total False Positives (H1) / Total True Nulls (H0)
  significance = sum(alp$H1) / sum(alp$H0)
  
  # here summary
  summary = data.frame(
  nGenotypes = params$Design$Genotypes, 
  nEnvironments = params$Design$Environments,
  nBlocks = params$Design$Blocks_per_E,
  nCandidateGenes = params$Genetic$Total_Genes, 
  controlEffectSize = params$Genetic$Beta_Control, 
  varG = params$Variances$G, 
  varE = params$Variances$E, 
  varB = params$Variances$Block, 
  varGxE = params$Variances$GxE, 
  varRes = params$Variances$Residual, 
  varTotal = params$Variances$Total_Phenotypic_Var,
  nIter = params$Simulation$Iterations,
  seed = params$Simulation$Seed,
  Type = rep(c('Ranking', 'Control is significant', 'Control is not significant', 'Non-control genes'), c(3,3,2,1)),
  Metric = c(colnames(out.ranking), colnames(out2.significant), colnames(out2.nonsignificant), 'Alpha'),
  Value = unlist(c(out.ranking, out2.significant, out2.nonsignificant, significance))
  )

  if (!file.exists('output')){
    dir.create('output', recursive = TRUE, showWarnings = FALSE)
  }
  
  nIter = params$Simulation$Iterations
  seed = params$Simulation$Seed
  
  write_csv(summary, file = sprintf('output/summary_%s_%s.csv', nIter, seed))
  ggsave(plot_list, file = sprintf('output/estimates_%s_%s.png', nIter, seed), width = 12, height = 6)
  
  out = list(ranking = out.ranking, power.significant = out2.significant, power.nonsignificant = out2.nonsignificant,
             significance = significance, plots = plot_list)
  #save(out, file = 'out.RData') #debuggin
  return(out)
}

browser_sim = function(sim, path, params, layers = rep(T, 6)){
  pheno = read_csv(sprintf('%s/sim_%s.csv', path, sim))  
  nZeros = nchar(gsub('Gene','',pheno$GeneID[1])) - 1
  controls = paste0('Gene',strrep('0',nZeros),1)
  pheno$Gene = ifelse(pheno$GeneID %in% controls, 'Control', 'Other gene')
  effects = cbind(as.matrix(pheno[, c('Effect', 'Eval', 'Gval', 'GXEval', 'Bval', 'Rval')]), mu = 0, placeholder = 0)
  pheno$Display = apply(effects[,c(layers, TRUE, TRUE)], 1, sum)
  plot = pheno %>% 
    
    ggplot(aes(y = Block, x = GID, fill = Display))+
    geom_tile(color = 'white')+
    geom_point(aes(color = Gene), size = 4, show.legend = FALSE)+
    facet_wrap(~EID)+
    scale_fill_viridis_c(option = 'D')+
    theme_minimal()+
    theme(legend.position = 'top',
          legend.background = element_rect(fill = "#F7F7F7", color = "grey50", size = 0.5),
          plot.title = element_text(size = 18, face = 'bold'),
          axis.title = element_text(size = 12, face = 'bold'))+
    scale_color_manual(values = c('Control' = 'red', 'Other gene' = 'transparent'))+
    guides(fill = guide_colorbar(direction = "vertical", position = 'right', barheight = 10))+
    labs(fill = 'Phenotype', title = sprintf('Simulation %s', sim), x = '', y = '',
         subtitle = 'Plants with the control gene are denoted with red')+
    theme(legend.title = element_text(face = 'bold'))
  return(plot)
}


logging = function(session, message = "", t = 0.01, target = "logs") {
  if (message == "") {
    action = "clearLogs"
    content = NULL
  } else {
    action = "addLog"
    content = message
  }
  message_data = list(
    target = target,
    action = action,
    content = content
  )
  session$sendCustomMessage("updateLog", message_data)
  Sys.sleep(t)
}


aggregate_results_lite = function(params, sim_dir, alpha = 0.05){
  
  path = dir(sim_dir, full.names = TRUE)
  out = lapply(path[grepl('out', path)], function(p) read_csv(p, col_types = cols(sim = "c"))) %>% bind_rows()
  alp = lapply(path[grepl('alp', path)], function(p) read_csv(p, col_types = cols(sim = "c"))) %>% bind_rows()
  
  beta = params$Genetic$Beta_Control
  gene = unique(out$geneID)
  geneTrue = data.frame(gene = gene, beta = beta)
  
  totalGenes = params$Genetic$Total_Genes
  za2 = qnorm(1 - alpha/2)
  
  nIter = nrow(out)
  out.ranking = out %>% 
    summarize(
      n.Top1 = sum(rank == 1),
      Mean.Rank = round(mean(rank), 2),
      Median.Rank = quantile(rank, 0.5))
  
  out.significant = out %>% 
    filter(significant == 1) %>% 
    mutate(class = ifelse(covered == 1, 'Correct', ifelse(bHat > beta, 'Overestimated', 'Underestimated'))) %>%
    mutate(class = factor(class, levels = c('Correct', 'Overestimated', 'Underestimated')))
  
  out2.significant = as.data.frame.list(table(out.significant$class))
  
  out.nonsignificant = out %>% 
    filter(significant == 0) %>% 
    mutate(class = ifelse(covered, 'Covering', 'Non-covering')) %>%
    mutate(class = factor(class, levels = c('Covering', 'Non-covering')))
  
  out2.nonsignificant = as.data.frame.list(table(out.nonsignificant$class))
  
  out = rbind(out.significant, out.nonsignificant)
  sim_factor = out %>% arrange(-bHat) %>% pull(sim)
  out$sim = factor(out$sim, levels = sim_factor)
  
  
  # Calculate the overall empirical alpha (Type I Error Rate) across all simulations
  # Alpha = Total False Positives (H1) / Total True Nulls (H0)
  significance = sum(alp$H1) / sum(alp$H0)
  
  # here summary
  summary = data.frame(
    nGenotypes = params$Design$Genotypes, 
    nEnvironments = params$Design$Environments,
    nBlocks = params$Design$Blocks_per_E,
    nCandidateGenes = params$Genetic$Total_Genes, 
    controlEffectSize = params$Genetic$Beta_Control, 
    varG = params$Variances$G, 
    varE = params$Variances$E, 
    varB = params$Variances$Block, 
    varGxE = params$Variances$GxE, 
    varRes = params$Variances$Residual, 
    varTotal = params$Variances$Total_Phenotypic_Var,
    nIter = params$Simulation$Iterations,
    seed = params$Simulation$Seed,
    Type = rep(c('Ranking', 'Control is significant', 'Control is not significant', 'Non-control genes'), c(3,3,2,1)),
    Metric = c(colnames(out.ranking), colnames(out2.significant), colnames(out2.nonsignificant), 'Alpha'),
    Value = unlist(c(out.ranking, out2.significant, out2.nonsignificant, significance))
  )
  
  return(summary)
}
