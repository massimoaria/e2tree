#' Interactive E2Tree Plot for R Graphics Device
#'
#' @description
#' Displays an E2Tree as an interactive plot in the R graphics device.
#' Click on nodes to see detailed information in the console.
#' Right-click or press ESC to exit interactive mode.
#'
#' @param fit An e2tree object
#' @param data The training data used to build the tree
#' @param ensemble The ensemble model (randomForest or ranger)
#' @param main Plot title (default: "E2Tree - Click on nodes (ESC to exit)")
#' @param ... Additional arguments passed to rpart.plot
#'
#' @return Invisibly returns the rpart object
#'
#' @details
#' This function converts the e2tree object to an rpart object and displays it
#' using rpart.plot. You can then click on any node to see:
#' - Node ID and type (terminal/internal)
#' - Number of observations
#' - Prediction and probability/purity
#' - Decision path to reach the node
#' - Class distribution (for classification)
#' - Split rule (for internal nodes)
#' - Observations in the node (for terminal nodes)
#'
#' @examples
#' \dontrun{
#' # After creating an e2tree object
#' plot_e2tree_click(tree, training, ensemble)
#' 
#' # Click on nodes to explore
#' # Press ESC or right-click to exit
#' }
#'
#' @export
plot_e2tree_click <- function(fit, data, ensemble, 
                               main = "E2Tree - Click on nodes (ESC to exit)",
                               ...) {
  

  # Check required packages
  if (!requireNamespace("rpart.plot", quietly = TRUE)) {
    stop("Package 'rpart.plot' is required. Install with: install.packages('rpart.plot')")
  }
  
  # Validate inputs
  if (!inherits(fit, "e2tree")) {
    stop("'fit' must be an e2tree object")
  }
  
  # Convert e2tree to rpart
  rpart_obj <- rpart2Tree(fit, ensemble)
  
  # Get tree info
  tree_info <- fit$tree
  
  # Determine task type
  is_classification <- !is.null(attr(fit, "ylevels"))
  response_var <- all.vars(fit$terms)[1]
  
  # Get class levels and colors
  if (is_classification) {
    class_levels <- attr(fit, "ylevels")
    n_classes <- length(class_levels)
    if (n_classes <= 3) {
      class_colors <- c("#e74c3c", "#3498db", "#27ae60")[1:n_classes]
    } else {
      class_colors <- rainbow(n_classes)
    }
  }
  
  # =========================================================================
  # Helper function to format path
  # =========================================================================
  format_path <- function(path) {
    if (is.na(path) || path == "") return("Root node")
    
    # Split by " & "
    conditions <- strsplit(path, " & ")[[1]]
    
    formatted <- sapply(conditions, function(cond) {
      cond <- trimws(cond)
      # Handle negation
      if (startsWith(cond, "!")) {
        cond <- substring(cond, 2)
        if (grepl("<=", cond)) {
          cond <- gsub("<=", ">", cond)
        } else if (grepl("%in%", cond)) {
          cond <- gsub("%in%", "NOT IN", cond)
        }
      } else {
        if (grepl("%in%", cond)) {
          cond <- gsub("%in%", "IN", cond)
        }
      }
      # Format c() values
      if (grepl("c\\(", cond)) {
        cond <- gsub("c\\(", "{", cond)
        cond <- gsub("\\)", "}", cond)
      }
      cond
    })
    
    paste(formatted, collapse = "\n  AND ")
  }
  
  # =========================================================================
  # Get node positions from rpart.plot
  # =========================================================================
  get_node_positions <- function(rpart_obj) {
    # Get frame info
    frame <- rpart_obj$frame
    nodes <- as.integer(row.names(frame))
    n_nodes <- length(nodes)
    
    # Calculate depth for each node
    get_depth <- function(node) {
      d <- 0
      n <- node
      while (n > 1) {
        n <- floor(n / 2)
        d <- d + 1
      }
      d
    }
    
    depths <- sapply(nodes, get_depth)
    max_depth <- max(depths)
    
    # Calculate x positions based on binary tree structure
    # Nodes at each level are positioned based on their path
    x_pos <- numeric(n_nodes)
    y_pos <- numeric(n_nodes)
    
    for (i in seq_along(nodes)) {
      node <- nodes[i]
      depth <- depths[i]
      
      # Y position based on depth (inverted, root at top)
      y_pos[i] <- 1 - depth / (max_depth + 1)
      
      # X position based on binary path
      if (node == 1) {
        x_pos[i] <- 0.5
      } else {
        # Trace path from root
        path <- integer(0)
        n <- node
        while (n > 1) {
          path <- c(n %% 2, path)  # 0 = left, 1 = right
          n <- floor(n / 2)
        }
        
        # Calculate x position
        x <- 0.5
        width <- 0.25
        for (dir in path) {
          if (dir == 0) {
            x <- x - width
          } else {
            x <- x + width
          }
          width <- width / 2
        }
        x_pos[i] <- x
      }
    }
    
    data.frame(
      node = nodes,
      x = x_pos,
      y = y_pos,
      depth = depths,
      is_terminal = frame$var == "<leaf>",
      n = frame$n
    )
  }
  
  # =========================================================================
  # Print node information
  # =========================================================================
  print_node_info <- function(node_id) {
    
    # Find node in tree_info
    node_idx <- which(tree_info$node == node_id)
    
    if (length(node_idx) == 0) {
      cat("\n  Node not found in tree structure\n")
      return(invisible(NULL))
    }
    
    node_row <- tree_info[node_idx, ]
    
    # Header
    cat("\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    if (node_row$terminal) {
      cat(sprintf("  TERMINAL NODE #%d\n", node_id))
    } else {
      cat(sprintf("  INTERNAL NODE #%d\n", node_id))
    }
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
    
    # Basic info
    cat(sprintf("  Observations:    %d\n", node_row$n))
    cat(sprintf("  Prediction:      %s\n", node_row$pred))
    
    if (is_classification) {
      cat(sprintf("  Purity:          %.1f%%\n", node_row$prob * 100))
    }
    
    # Path
    cat("\n  PATH TO NODE:\n")
    cat("  ", format_path(node_row$path), "\n")
    
    # Split rule for internal nodes
    if (!node_row$terminal && !is.na(node_row$splitLabel)) {
      cat("\n  SPLIT RULE:\n")
      split_label <- node_row$splitLabel
      if (grepl("<=", split_label)) {
        parts <- strsplit(split_label, "<=")[[1]]
        cat(sprintf("    If %s <= %s -> LEFT child (node %d)\n", 
                    trimws(parts[1]), trimws(parts[2]), node_id * 2))
        cat(sprintf("    If %s >  %s -> RIGHT child (node %d)\n", 
                    trimws(parts[1]), trimws(parts[2]), node_id * 2 + 1))
      } else if (grepl("%in%", split_label)) {
        cat(sprintf("    %s\n", split_label))
        cat(sprintf("    TRUE  -> LEFT child (node %d)\n", node_id * 2))
        cat(sprintf("    FALSE -> RIGHT child (node %d)\n", node_id * 2 + 1))
      }
    }
    
    # Class distribution for classification
    if (is_classification && !is.null(node_row$obs) && length(node_row$obs[[1]]) > 0) {
      obs_indices <- node_row$obs[[1]]
      if (length(obs_indices) > 0 && max(obs_indices) <= nrow(data)) {
        node_data <- data[obs_indices, ]
        class_table <- table(factor(node_data[[response_var]], levels = class_levels))
        
        cat("\n  CLASS DISTRIBUTION:\n")
        for (cls in names(class_table)) {
          pct <- round(class_table[cls] / sum(class_table) * 100, 1)
          bar_width <- round(pct / 5)
          bar <- paste(rep("#", bar_width), collapse = "")
          cat(sprintf("    %-15s %4d (%5.1f%%) %s\n", cls, class_table[cls], pct, bar))
        }
      }
    }
    
    # Show observations for terminal nodes
    if (node_row$terminal && !is.null(node_row$obs) && length(node_row$obs[[1]]) > 0) {
      obs_indices <- node_row$obs[[1]]
      cat(sprintf("\n  OBSERVATION IDs: (%d total)\n", length(obs_indices)))
      
      if (length(obs_indices) <= 20) {
        cat("    ", paste(obs_indices, collapse = ", "), "\n")
      } else {
        cat("    ", paste(head(obs_indices, 10), collapse = ", "), 
            ", ..., ", 
            paste(tail(obs_indices, 5), collapse = ", "), "\n")
      }
    }
    
    cat("\n")
    cat(paste(rep("-", 60), collapse = ""), "\n")
    
    invisible(NULL)
  }
  
  # =========================================================================
  # Main plotting and interaction loop
  # =========================================================================
  
  # Set up the plot
  # Use a clean device
  if (dev.cur() > 1) {
    # Keep current device
  } else {
    dev.new()
  }
  
  # Plot the tree with rpart.plot
  rpart.plot::rpart.plot(
    rpart_obj,
    main = main,
    type = 4,
    extra = if(is_classification) 104 else 101,
    under = TRUE,
    faclen = 0,
    cex = 0.9,
    box.palette = if(is_classification) "auto" else "Blues",
    shadow.col = "gray",
    nn = TRUE,  # Show node numbers
    ...
  )
  
  # Get node positions
  node_pos <- get_node_positions(rpart_obj)
  
  # Get plot coordinates
  usr <- par("usr")
  
  # Scale node positions to plot coordinates
  node_pos$x_plot <- usr[1] + node_pos$x * (usr[2] - usr[1])
  node_pos$y_plot <- usr[3] + node_pos$y * (usr[4] - usr[3])
  
  # Instructions
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("  E2TREE INTERACTIVE PLOT\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("\n")
  cat("  Click on any node to see detailed information.\n")
  cat("  Press ESC or right-click to exit interactive mode.\n")
  cat("\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  # Interactive loop
  repeat {
    # Get click location
    click <- tryCatch({
      locator(1)
    }, error = function(e) {
      NULL
    })
    
    # Exit if no click (ESC pressed or window closed)
    if (is.null(click) || length(click$x) == 0) {
      cat("\n  Exiting interactive mode.\n\n")
      break
    }
    
    # Find closest node
    distances <- sqrt((node_pos$x_plot - click$x)^2 + (node_pos$y_plot - click$y)^2)
    closest_idx <- which.min(distances)
    closest_node <- node_pos$node[closest_idx]
    
    # Check if click is reasonably close to a node
    # (within 10% of plot width)
    threshold <- (usr[2] - usr[1]) * 0.1
    if (distances[closest_idx] > threshold) {
      cat("\n  Click closer to a node.\n")
      next
    }
    
    # Print node information
    print_node_info(closest_node)
    
    # Highlight the selected node briefly
    points(node_pos$x_plot[closest_idx], node_pos$y_plot[closest_idx], 
           pch = 1, cex = 4, col = "red", lwd = 3)
    
    # Redraw after a moment to remove highlight
    Sys.sleep(0.3)
    rpart.plot::rpart.plot(
      rpart_obj,
      main = main,
      type = 4,
      extra = if(is_classification) 104 else 101,
      under = TRUE,
      faclen = 0,
      cex = 0.9,
      box.palette = if(is_classification) "auto" else "Blues",
      shadow.col = "gray",
      nn = TRUE,
      ...
    )
  }
  
  invisible(rpart_obj)
}


#' Quick E2Tree Plot (Non-Interactive)
#'
#' @description
#' Displays an E2Tree as a static plot using rpart.plot.
#' For interactive exploration, use plot_e2tree_click().
#'
#' @param fit An e2tree object
#' @param ensemble The ensemble model (randomForest or ranger)
#' @param main Plot title
#' @param ... Additional arguments passed to rpart.plot
#'
#' @return Invisibly returns the rpart object
#'
#' @export
plot_e2tree <- function(fit, ensemble, main = "E2Tree", ...) {
  
  if (!requireNamespace("rpart.plot", quietly = TRUE)) {
    stop("Package 'rpart.plot' is required. Install with: install.packages('rpart.plot')")
  }
  
  if (!inherits(fit, "e2tree")) {
    stop("'fit' must be an e2tree object")
  }
  
  rpart_obj <- rpart2Tree(fit, ensemble)
  is_classification <- !is.null(attr(fit, "ylevels"))
  
  rpart.plot::rpart.plot(
    rpart_obj,
    main = main,
    type = 4,
    extra = if(is_classification) 104 else 101,
    under = TRUE,
    faclen = 0,
    cex = 0.9,
    box.palette = if(is_classification) "auto" else "Blues",
    shadow.col = "gray",
    nn = TRUE,
    ...
  )
  
  invisible(rpart_obj)
}


#' Print E2Tree Summary
#'
#' @description
#' Prints a comprehensive summary of an E2Tree model including
#' all decision rules, variable importance, and node statistics.
#'
#' @param fit An e2tree object
#' @param data The training data
#'
#' @export
print_e2tree_summary <- function(fit, data) {
  
  if (!inherits(fit, "e2tree")) {
    stop("'fit' must be an e2tree object")
  }
  
  tree_info <- fit$tree
  is_classification <- !is.null(attr(fit, "ylevels"))
  response_var <- all.vars(fit$terms)[1]
  
  if (is_classification) {
    class_levels <- attr(fit, "ylevels")
  }
  
  # Header
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("                    E2TREE MODEL SUMMARY\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  # Model info
  cat("MODEL INFORMATION\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  cat(sprintf("  Task:              %s\n", if(is_classification) "Classification" else "Regression"))
  cat(sprintf("  Response:          %s\n", response_var))
  cat(sprintf("  Observations:      %d\n", nrow(data)))
  cat(sprintf("  Total Nodes:       %d\n", nrow(tree_info)))
  cat(sprintf("  Terminal Nodes:    %d\n", sum(tree_info$terminal)))
  cat(sprintf("  Max Depth:         %d\n", max(sapply(tree_info$node, function(n) {
    d <- 0; while(n > 1) { n <- floor(n/2); d <- d + 1 }; d
  }))))
  
  # Variable importance
  if (!is.null(fit$varimp) && !is.null(fit$varimp$vimp)) {
    cat("\n\nVARIABLE IMPORTANCE\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    vimp <- fit$varimp$vimp
    if (is.data.frame(vimp) && ncol(vimp) >= 2) {
      vimp <- vimp[order(vimp[[2]], decreasing = TRUE), ]
      max_imp <- max(vimp[[2]])
      for (i in 1:nrow(vimp)) {
        bar_width <- round(vimp[[2]][i] / max_imp * 20)
        bar <- paste(rep("#", bar_width), collapse = "")
        cat(sprintf("  %-20s %8.4f  %s\n", vimp[[1]][i], vimp[[2]][i], bar))
      }
    }
  }
  
  # Decision rules
  cat("\n\nDECISION RULES\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  terminal_nodes <- tree_info[tree_info$terminal, ]
  
  for (i in 1:nrow(terminal_nodes)) {
    node <- terminal_nodes[i, ]
    
    cat(sprintf("\nRule %d (Node %d):\n", i, node$node))
    
    # Format path
    if (!is.na(node$path) && node$path != "") {
      conditions <- strsplit(node$path, " & ")[[1]]
      cat("  IF:\n")
      for (j in seq_along(conditions)) {
        cond <- trimws(conditions[j])
        # Handle negation
        if (startsWith(cond, "!")) {
          cond <- substring(cond, 2)
          if (grepl("<=", cond)) cond <- gsub("<=", ">", cond)
          else if (grepl("%in%", cond)) cond <- gsub("%in%", "NOT IN", cond)
        } else {
          if (grepl("%in%", cond)) cond <- gsub("%in%", "IN", cond)
        }
        if (grepl("c\\(", cond)) {
          cond <- gsub("c\\(", "{", cond)
          cond <- gsub("\\)", "}", cond)
        }
        if (j == 1) {
          cat(sprintf("       %s\n", cond))
        } else {
          cat(sprintf("   AND %s\n", cond))
        }
      }
    } else {
      cat("  IF: (root - all observations)\n")
    }
    
    cat(sprintf("  THEN: %s", node$pred))
    if (is_classification) {
      cat(sprintf(" (purity: %.1f%%, n=%d)", node$prob * 100, node$n))
    } else {
      cat(sprintf(" (n=%d)", node$n))
    }
    cat("\n")
  }
  
  cat("\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  invisible(fit)
}

