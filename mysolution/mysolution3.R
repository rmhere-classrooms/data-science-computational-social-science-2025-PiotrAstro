library(shiny)
library(bslib)
library(igraph)

dfGraph <- read.csv2(
  url("https://bergplace.org/share/out.radoslaw_email_email"),
  skip = 2,
  sep = " ",
  header = FALSE
)[, 1:2]

names(dfGraph) <- c("from", "to")

# graf skierowany i uproszczony

g <- graph.data.frame(dfGraph, directed = TRUE)
g <- simplify(g)

# vertices - powinno być 167
vcount(g)
# edges - powinno być 5783
ecount(g)

# budowanie wag
edge_counts <- as.data.frame(table(dfGraph$from, dfGraph$to))
names(edge_counts) <- c("from", "to", "count_edge")
total_counts <- aggregate(count_edge ~ from, data = edge_counts, FUN = sum)
names(total_counts) <- c("from", "count_total")

edge_counts <- merge(edge_counts, total_counts, by = "from")
edge_counts$weight <- edge_counts$count_edge / edge_counts$count_total

E(g)$weight <- 0
for(i in 1:nrow(edge_counts)) {
  from_node <- as.character(edge_counts$from[i])
  to_node <- as.character(edge_counts$to[i])
  weight <- edge_counts$weight[i]
  
  edge_id <- get_edge_ids(g, c(from_node, to_node))
  E(g)$weight[edge_id] <- weight
}

# -------------------------------------------------------------
# funkcje


# Postanowiłem jako 5 miarę użyć pagerank
# Po histogramie widziałem, że nie jest to do końca graf tego typu, ale i tak postanowiłem spróbować
select_initial_nodes <- function(g, method, percentage = 0.05) {
  n_nodes <- ceiling(vcount(g) * percentage)
  
  if(method == "outdegree") {
    nodes <- order(degree(g, mode = "out"), decreasing = TRUE)[1:n_nodes]
    
  } else if(method == "betweenness") {
    nodes <- order(betweenness(g, directed = TRUE), decreasing = TRUE)[1:n_nodes]
    
  } else if(method == "closeness") {
    nodes <- order(closeness(g, mode = "out"), decreasing = TRUE)[1:n_nodes]
    
  } else if(method == "random") {
    nodes <- sample(1:vcount(g), n_nodes)
    
  } else if(method == "pagerank") {
    nodes <- order(page_rank(g)$vector, decreasing = TRUE)[1:n_nodes]
    
  } else {
    stop("Unknown method: ", method)
  }
  
  return(nodes)
}




run_simulation <- function(g, initially_activated_ids, wij_percent = 100, max_iter=50) {
  wij_ratio <- wij_percent / 100
  n_v <- vcount(g)
  
  edge_indices <- ends(g, E(g), names = FALSE) 
  all_edges_numeric <- data.frame(
    from = edge_indices[, 1],
    to = edge_indices[, 2],
    weight = E(g)$weight
  )
  
  activated <- rep(FALSE, n_v)
  activated[initially_activated_ids] <- TRUE
  
  iteration_activated <- c(length(initially_activated_ids))
  activating_nodes <- initially_activated_ids
  
  for (iter in 1:max_iter) {
    if (length(activating_nodes) == 0) {
      break
    }
    
    potential_edges_idx <- which(all_edges_numeric$from %in% activating_nodes)
    potential_to_nodes <- all_edges_numeric$to[potential_edges_idx]
    target_is_inactive_idx <- which(!activated[potential_to_nodes])
    
    final_edges_idx <- potential_edges_idx[target_is_inactive_idx]
    
    if (length(final_edges_idx) == 0) {
      break
    }
    

    weights <- all_edges_numeric$weight[final_edges_idx] * wij_ratio
    success_activation <- runif(length(final_edges_idx)) < weights
    successful_target_nodes <- all_edges_numeric$to[final_edges_idx[success_activation]]
    
    newly_activated <- unique(successful_target_nodes)
    activated[newly_activated] <- TRUE
    
    activating_nodes <- newly_activated
    iteration_activated <- c(iteration_activated, length(newly_activated))
  }
  
  return(iteration_activated)
}

run_experiments <- function(g, method, n_experiments = 100, wij_percent = 100, max_iter=50) {
  results <- list()
  
  for(i in 1:n_experiments) {
    initial_nodes <- select_initial_nodes(g, method, percentage = 0.05)
    result <- run_simulation(g, initial_nodes, wij_percent, max_iter)
    results[[i]] <- result
  }
  
  max_len <- max(sapply(results, length))  # ← UŻYJ max_len zamiast max_iter
  averaged <- numeric(max_len)
  
  for(iter in 1:max_len) {  # ← max_len, nie max_iter!
    values <- sapply(results, function(x) {
      if(iter <= length(x)) x[iter] else 0
    })
    averaged[iter] <- mean(values)
  }
  
  return(averaged)
}


# ---------------------------------------------------------------------------
# aplikacja

ui <- page_sidebar(
  title = "Piotr Zatwarnicki - Rozprzestrzenianie się informacji w sieci e-mail",
  
  sidebar = sidebar(
    sliderInput(
      inputId = "weight_mult",
      label = "Mnożnik prawdopodobieństwa aktywacji (% wij):",
      min = 10,
      max = 200,
      value = 100,
      step = 10,
      post = "%"
    ),
    
    sliderInput(
      inputId = "max_iter",
      label = "Liczba iteracji:",
      min = 1,
      max = 50,
      value = 10,
      step = 1
    ),
    
    actionButton(
      inputId = "run_sim",
      label = "Uruchom symulację",
      class = "btn-primary",
      width = "100%"
    ),
    
    hr(),
    
    helpText("Każda metoda: 100 eksperymentów, wyniki uśrednione"),
    helpText("5% węzłów początkowych")
  ),
  
  plotOutput(outputId = "cascadePlot", height = "600px")
)


server <- function(input, output) {
  
  simulation_results <- eventReactive(input$run_sim, {
    
    weight_mult <- input$weight_mult / 100
    max_iter <- input$max_iter
    
    withProgress(message = 'Uruchamianie symulacji...', value = 0, {
      
      methods <- c("outdegree", "betweenness", "closeness", "random", "pagerank")
      method_names <- c("Out-degree", "Betweenness", "Closeness", "Random", "PageRank")
      
      results <- list()
      
      for(i in 1:length(methods)) {
        incProgress(1/length(methods), detail = paste("Metoda:", method_names[i]))
        
        results[[method_names[i]]] <- run_experiments(
          g, 
          methods[i], 
          n_experiments = 100,
          wij_percent = input$weight_mult,
          max_iter = max_iter
        )
      }
      
      results
    })
  })
  
  output$cascadePlot <- renderPlot({
    
    results <- simulation_results()
    
    max_len <- max(sapply(results, length))
    max_y <- max(sapply(results, max))
    
    plot(NULL, 
         xlim = c(0, max_len - 1), 
         ylim = c(0, max_y),
         xlab = "Numer iteracji", 
         ylab = "Liczba nowo aktywowanych węzłów",
         main = "Proces dyfuzji informacji w sieci e-mail",
         cex.main = 1.3, 
         cex.lab = 1.1,
         las = 1)
    
    colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#ff7f00", "#984ea3")
    
    for(i in 1:length(results)) {
      x_vals <- 0:(length(results[[i]]) - 1)
      lines(x_vals, results[[i]], 
            col = colors[i], 
            lwd = 2.5, 
            type = "b", 
            pch = 19,
            cex = 0.8)
    }
    
    legend("topright", 
           legend = names(results),
           col = colors,
           lwd = 2.5,
           pch = 19,
           cex = 1.0,
           bg = "white")
    
    grid(col = "gray80", lty = "dotted")
    
    mtext(paste("Parametry: wij × ", input$weight_mult, "%, max iter = ", input$max_iter, sep = ""),
          side = 3, line = 0.3, cex = 0.8, col = "gray40")
  })
}

# -------------------------------------------------------------------------------
# uruchomienie

shinyApp(ui = ui, server = server)