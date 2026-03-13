#' Interactive E2Tree Plot with visNetwork
#'
#' @description
#' Displays an E2Tree as an interactive network plot using visNetwork.
#' Features: drag nodes anywhere, zoom, pan, click for details.
#' Starts with hierarchical layout, then you can freely move nodes.
#'
#' @param fit An e2tree object
#' @param data The training data used to build the tree
#' @param ensemble The ensemble model (randomForest or ranger)
#' @param width Width of the widget (default: "100%")
#' @param height Height of the widget (default: "100%")
#' @param direction Layout direction: "UD" (top-down), "DU" (bottom-up), 
#'                  "LR" (left-right), "RL" (right-left)
#' @param node_spacing Spacing between nodes at same level (default: 200)
#' @param level_separation Spacing between levels (default: 200)
#' @param colors Named vector of colors for classes, or NULL for auto
#' @param show_percent Show percentage in nodes (default: TRUE)
#' @param show_prob Show class probabilities in nodes (default: TRUE)
#' @param show_n Show observation count in nodes (default: TRUE)
#' @param font_size Font size for node labels (default: 14)
#' @param edge_font_size Font size for edge labels (default: 12)
#' @param split_label_style How to display split information:
#'   \itemize{
#'     \item "rpart" - Variable name in node, threshold on edges (like rpart.plot)
#'     \item "full" - Full split rule on edges (variable + condition)
#'     \item "threshold" - Only threshold values on edges (< 47, >= 47)
#'     \item "yesno" - Simple yes/no on edges
#'     \item "none" - No labels on edges (hover for details)
#'     \item "innode" - Full split rule displayed IN the node (above stats)
#'   }
#' @param max_label_length Maximum characters for edge labels before truncating (default: 50)
#' @param details_on When to show node details:
#'   \itemize{
#'     \item "hover" - Show on mouse hover (default, but may cover other nodes)
#'     \item "click" - Show only on click (avoids covering highlighted nodes)
#'     \item "none" - No tooltips (use for cleaner visualization)
#'   }
#' @param navigation_buttons Show navigation buttons (default: FALSE)
#' @param free_drag If TRUE, nodes can be dragged in ALL directions (horizontal, 
#'                  vertical, diagonal). If FALSE (default), nodes can only be 
#'                  moved horizontally within their level.
#'
#' @return A visNetwork htmlwidget object
#'
#' @examples
#' \dontrun{
#' # Basic usage - hierarchical layout, horizontal drag only
#' plot_e2tree_vis(tree, training, ensemble)
#' 
#' # Enable free dragging in all directions
#' plot_e2tree_vis(tree, training, ensemble, free_drag = TRUE)
#' 
#' # Split rule shown directly in the node
#' plot_e2tree_vis(tree, training, ensemble, split_label_style = "innode")
#' }
#'
#' @export
plot_e2tree_vis <- function(fit, data, ensemble,
                            width = "100%",
                            height = "100%",
                            direction = "UD",
                            node_spacing = 200,
                            level_separation = 200,
                            colors = NULL,
                            show_percent = TRUE,
                            show_prob = TRUE,
                            show_n = TRUE,
                            font_size = 14,
                            edge_font_size = 12,
                            split_label_style = "rpart",
                            max_label_length = 50,
                            details_on = "hover",
                            navigation_buttons = FALSE,
                            free_drag = FALSE) {
  
  # Check required packages
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package 'visNetwork' is required. Install with: install.packages('visNetwork')")
  }
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install with: install.packages('jsonlite')")
  }
  
  if (!inherits(fit, "e2tree")) {
    stop("'fit' must be an e2tree object")
  }
  
  # Validate split_label_style
  valid_styles <- c("rpart", "full", "threshold", "yesno", "none", "innode")
  if (!split_label_style %in% valid_styles) {
    stop("split_label_style must be one of: ", paste(valid_styles, collapse = ", "))
  }
  
  # Validate details_on
  valid_details <- c("hover", "click", "none")
  if (!details_on %in% valid_details) {
    stop("details_on must be one of: ", paste(valid_details, collapse = ", "))
  }
  
  # Get tree info
  tree_info <- fit$tree
  
  # Determine task type
  is_classification <- !is.null(attr(fit, "ylevels"))
  response_var <- all.vars(fit$terms)[1]
  
  # Get class levels
  if (is_classification) {
    class_levels <- attr(fit, "ylevels")
    n_classes <- length(class_levels)
    
    # Default colors (rpart.plot style)
    if (is.null(colors)) {
      if (n_classes == 2) {
        colors <- c("#5B9F5B", "#6BAED6")
        names(colors) <- class_levels
      } else if (n_classes <= 8) {
        palette <- c("#5B9F5B", "#6BAED6", "#FDB863", "#B2ABD2", 
                     "#E66101", "#5E3C99", "#1B9E77", "#D95F02")
        colors <- palette[1:n_classes]
        names(colors) <- class_levels
      } else {
        colors <- rainbow(n_classes)
        names(colors) <- class_levels
      }
    }
  } else {
    colors <- NULL
  }
  
  # =========================================================================
  # Helper functions
  # =========================================================================
  
  calc_depth <- function(node_id) {
    depth <- 0
    n <- node_id
    while (n > 1) {
      n <- floor(n / 2)
      depth <- depth + 1
    }
    depth
  }
  
  # Get variable name from split
  get_split_variable <- function(split_label) {
    if (is.na(split_label) || split_label == "") return("")
    
    if (grepl("<=", split_label)) {
      parts <- strsplit(split_label, "<=")[[1]]
      return(trimws(parts[1]))
    } else if (grepl("%in%", split_label)) {
      parts <- strsplit(split_label, "%in%")[[1]]
      return(trimws(parts[1]))
    }
    return("")
  }
  
  # Get threshold/values from split
  get_split_condition <- function(split_label, is_left) {
    if (is.na(split_label) || split_label == "") return("")
    
    if (grepl("<=", split_label)) {
      parts <- strsplit(split_label, "<=")[[1]]
      threshold <- trimws(parts[2])
      if (is_left) {
        return(paste0("< ", threshold))
      } else {
        return(paste0("\u2265 ", threshold))
      }
    } else if (grepl("%in%", split_label)) {
      parts <- strsplit(split_label, "%in%")[[1]]
      values <- trimws(parts[2])
      values <- gsub("c\\(|\\)", "", values)
      values <- gsub("\"", "", values)
      if (is_left) {
        return(paste0("= ", values))
      } else {
        return(paste0("\u2260 ", values))
      }
    }
    return("")
  }
  
  # Get full condition for tooltip
  get_full_condition <- function(split_label, is_left) {
    if (is.na(split_label) || split_label == "") return("")
    
    var_name <- get_split_variable(split_label)
    
    if (grepl("<=", split_label)) {
      parts <- strsplit(split_label, "<=")[[1]]
      threshold <- trimws(parts[2])
      if (is_left) {
        return(paste0(var_name, " < ", threshold))
      } else {
        return(paste0(var_name, " \u2265 ", threshold))
      }
    } else if (grepl("%in%", split_label)) {
      parts <- strsplit(split_label, "%in%")[[1]]
      values <- trimws(parts[2])
      values <- gsub("c\\(", "{", values)
      values <- gsub("\\)", "}", values)
      values <- gsub("\"", "", values)
      if (is_left) {
        return(paste0(var_name, " IN ", values))
      } else {
        return(paste0(var_name, " NOT IN ", values))
      }
    }
    return("")
  }
  
  # Format edge label based on style
  format_edge_label <- function(split_label, is_left, style, max_len) {
    if (is.na(split_label) || split_label == "") return("")
    
    if (style == "none") {
      return("")
    }
    
    if (style == "yesno") {
      return(if (is_left) "yes" else "no")
    }
    
    # For "innode" style, use simple yes/no on edges since split is in node
    if (style == "innode") {
      return(if (is_left) "yes" else "no")
    }
    
    var_name <- get_split_variable(split_label)
    condition <- get_split_condition(split_label, is_left)
    
    if (style == "threshold" || style == "rpart") {
      # Just the condition (< 47, >= 47, or category values)
      label <- condition
    } else if (style == "full") {
      # Full: variable + condition
      label <- paste0(var_name, " ", condition)
    } else {
      label <- condition
    }
    
    # Truncate if too long
    if (nchar(label) > max_len) {
      label <- paste0(substr(label, 1, max_len - 3), "...")
    }
    
    return(label)
  }
  
  # Format path for tooltip (HTML)
  format_path_html <- function(path) {
    if (is.na(path) || path == "") return("<b>Root node</b>")
    
    conditions <- strsplit(path, " & ")[[1]]
    
    formatted <- sapply(conditions, function(cond) {
      cond <- trimws(cond)
      if (startsWith(cond, "!")) {
        cond <- substring(cond, 2)
        if (grepl("<=", cond)) cond <- gsub("<=", " > ", cond)
        else if (grepl("%in%", cond)) cond <- gsub("%in%", " NOT IN ", cond)
      } else {
        if (grepl("<=", cond)) cond <- gsub("<=", " \u2264 ", cond)
        if (grepl("%in%", cond)) cond <- gsub("%in%", " IN ", cond)
      }
      cond <- gsub("c\\(", "{", cond)
      cond <- gsub("\\)", "}", cond)
      cond <- gsub("\"", "", cond)
      cond
    })
    
    paste0("<b>Path:</b><br>", paste(formatted, collapse = "<br>AND "))
  }
  
  # Format split rule for tooltip
  format_split_html <- function(split_label) {
    if (is.na(split_label) || split_label == "") return("")
    
    split_display <- split_label
    split_display <- gsub("<=", " \u2264 ", split_display)
    split_display <- gsub("%in%", " IN ", split_display)
    split_display <- gsub("c\\(", "{", split_display)
    split_display <- gsub("\\)", "}", split_display)
    split_display <- gsub("\"", "", split_display)
    
    return(split_display)
  }
  
  # Format split rule for display IN the node (compact version)
  format_split_for_node <- function(split_label, max_chars = 25) {
    if (is.na(split_label) || split_label == "") return("")
    
    if (grepl("<=", split_label)) {
      parts <- strsplit(split_label, "<=")[[1]]
      var_name <- trimws(parts[1])
      threshold <- trimws(parts[2])
      result <- paste0(var_name, " \u2264 ", threshold)
    } else if (grepl("%in%", split_label)) {
      parts <- strsplit(split_label, "%in%")[[1]]
      var_name <- trimws(parts[1])
      values <- trimws(parts[2])
      values <- gsub("c\\(|\\)", "", values)
      values <- gsub("\"", "", values)
      # Truncate values if too long
      if (nchar(values) > max_chars - nchar(var_name) - 4) {
        values <- paste0(substr(values, 1, max_chars - nchar(var_name) - 7), "...")
      }
      result <- paste0(var_name, " = ", values)
    } else {
      result <- split_label
    }
    
    # Final truncation if still too long
    if (nchar(result) > max_chars + 10) {
      result <- paste0(substr(result, 1, max_chars + 7), "...")
    }
    
    return(result)
  }
  
  # =========================================================================
  # Build nodes dataframe
  # =========================================================================
  
  total_n <- tree_info$n[tree_info$node == 1]
  max_depth <- max(sapply(tree_info$node, calc_depth))
  
  # Function to calculate node position for free_drag mode
  calc_node_position <- function(node_id, node_spacing, level_separation, direction) {
    depth <- calc_depth(node_id)
    
    if (node_id == 1) {
      x_pos <- 0
      y_pos <- 0
    } else {
      # Trace path from root: 0 = left, 1 = right
      path <- c()
      n <- node_id
      while (n > 1) {
        path <- c(n %% 2, path)
        n <- floor(n / 2)
      }
      
      # Calculate x position following the path
      # Base width depends on tree depth for good spacing
      base_width <- node_spacing * (2 ^ (max_depth - 1))
      x_pos <- 0
      width <- base_width
      for (dir in path) {
        if (dir == 0) {
          x_pos <- x_pos - width
        } else {
          x_pos <- x_pos + width
        }
        width <- width / 2
      }
      
      y_pos <- depth * level_separation
    }
    
    # Adjust based on direction
    if (direction == "DU") {
      y_pos <- -y_pos
    } else if (direction == "LR") {
      tmp <- x_pos
      x_pos <- y_pos
      y_pos <- tmp
    } else if (direction == "RL") {
      tmp <- x_pos
      x_pos <- -y_pos
      y_pos <- tmp
    }
    
    list(x = x_pos, y = y_pos)
  }
  
  nodes_list <- lapply(seq_len(nrow(tree_info)), function(i) {
    row <- tree_info[i, ]
    node_id <- row$node
    depth <- calc_depth(node_id)
    
    # Calculate position for free_drag mode
    pos <- calc_node_position(node_id, node_spacing, level_separation, direction)
    
    # Calculate percentage
    pct <- round(row$n / total_n * 100)
    
    # Build label
    if (is_classification) {
      main_label <- as.character(row$pred)
      
      # For rpart style: add split variable name in internal nodes
      if (split_label_style == "rpart" && !row$terminal && !is.na(row$splitLabel)) {
        split_var <- get_split_variable(row$splitLabel)
        main_label <- paste0(main_label, "\n", split_var)
      }
      
      # For innode style: add full split rule in internal nodes
      if (split_label_style == "innode" && !row$terminal && !is.na(row$splitLabel)) {
        split_rule <- format_split_for_node(row$splitLabel, max_chars = 30)
        main_label <- paste0(main_label, "\n", split_rule)
      }
      
      # Secondary info - each on separate line
      info_lines <- c()
      if (show_prob) {
        prob_str <- sprintf("%.2f  %.2f", 1 - row$prob, row$prob)
        info_lines <- c(info_lines, prob_str)
      }
      if (show_percent) {
        info_lines <- c(info_lines, paste0(pct, "%"))
      }
      if (show_n) {
        info_lines <- c(info_lines, paste0("n=", row$n))
      }
      
      if (length(info_lines) > 0) {
        label <- paste0(main_label, "\n", paste(info_lines, collapse = "\n"))
      } else {
        label <- main_label
      }
      
      bg_color <- colors[as.character(row$pred)]
      if (is.null(bg_color) || is.na(bg_color)) bg_color <- "#cccccc"
      
    } else {
      # Regression
      main_label <- sprintf("%.2f", row$pred)
      
      if (split_label_style == "rpart" && !row$terminal && !is.na(row$splitLabel)) {
        split_var <- get_split_variable(row$splitLabel)
        main_label <- paste0(main_label, "\n", split_var)
      }
      
      # For innode style: add full split rule in internal nodes
      if (split_label_style == "innode" && !row$terminal && !is.na(row$splitLabel)) {
        split_rule <- format_split_for_node(row$splitLabel, max_chars = 30)
        main_label <- paste0(main_label, "\n", split_rule)
      }
      
      # Secondary info - each on separate line
      info_lines <- c()
      if (show_percent) info_lines <- c(info_lines, paste0(pct, "%"))
      if (show_n) info_lines <- c(info_lines, paste0("n=", row$n))
      
      if (length(info_lines) > 0) {
        label <- paste0(main_label, "\n", paste(info_lines, collapse = "\n"))
      } else {
        label <- main_label
      }
      
      bg_color <- "#6BAED6"
    }
    
    # Shape
    shape <- if (row$terminal) "box" else "ellipse"
    border_color <- if (row$terminal) "#333333" else "#666666"
    border_width <- if (row$terminal) 2 else 1
    
    # Build tooltip
    tooltip <- paste0(
      "<div style='padding: 10px; font-family: Arial, sans-serif; max-width: 500px;'>",
      "<h4 style='margin: 0 0 10px 0;'>Node ", node_id, 
      if (row$terminal) " (Terminal)" else " (Internal)", "</h4>",
      "<table style='border-collapse: collapse;'>",
      "<tr><td style='padding: 3px 10px 3px 0;'><b>Prediction:</b></td><td>", row$pred, "</td></tr>",
      "<tr><td style='padding: 3px 10px 3px 0;'><b>Observations:</b></td><td>", row$n, " (", pct, "%)</td></tr>"
    )
    
    if (is_classification) {
      tooltip <- paste0(tooltip,
                        "<tr><td style='padding: 3px 10px 3px 0;'><b>Accuracy:</b></td><td>", 
                        round(row$prob * 100, 1), "%</td></tr>"
      )
    }
    
    tooltip <- paste0(tooltip,
                      "<tr><td style='padding: 3px 10px 3px 0;'><b>Depth:</b></td><td>", depth, "</td></tr>",
                      "</table>",
                      "<hr style='margin: 10px 0;'>",
                      format_path_html(row$path)
    )
    
    # Add full split rule in tooltip
    if (!row$terminal && !is.na(row$splitLabel)) {
      tooltip <- paste0(tooltip,
                        "<hr style='margin: 10px 0;'>",
                        "<b>Split Rule:</b><br>",
                        "<span style='font-family: monospace; background: #f0f0f0; padding: 2px 5px; word-wrap: break-word;'>",
                        format_split_html(row$splitLabel),
                        "</span>"
      )
    }
    
    tooltip <- paste0(tooltip, "</div>")
    
    list(
      id = node_id,
      label = label,
      level = depth,
      x = pos$x,
      y = pos$y,
      shape = shape,
      color.background = bg_color,
      color.border = border_color,
      borderWidth = border_width,
      title = tooltip,
      is_terminal = row$terminal,
      split_label = if (!is.na(row$splitLabel)) row$splitLabel else ""
    )
  })
  
  nodes_df <- data.frame(
    id = sapply(nodes_list, function(x) x$id),
    label = sapply(nodes_list, function(x) x$label),
    level = sapply(nodes_list, function(x) x$level),
    shape = sapply(nodes_list, function(x) x$shape),
    color.background = sapply(nodes_list, function(x) x$color.background),
    color.border = sapply(nodes_list, function(x) x$color.border),
    color.highlight.background = "#FFD700",
    color.highlight.border = "#FFA500",
    borderWidth = sapply(nodes_list, function(x) x$borderWidth),
    font.size = font_size,
    font.color = "#000000",
    shadow = TRUE,
    stringsAsFactors = FALSE
  )
  
  # Add x, y coordinates only for free_drag mode
  if (free_drag) {
    nodes_df$x <- sapply(nodes_list, function(x) x$x)
    nodes_df$y <- sapply(nodes_list, function(x) x$y)
  }
  
  # Add tooltips only if details_on is not "none"
  if (details_on != "none") {
    nodes_df$title <- sapply(nodes_list, function(x) x$title)
  }
  
  # =========================================================================
  # Build edges dataframe
  # =========================================================================
  
  edges_list <- list()
  
  for (i in seq_len(nrow(tree_info))) {
    row <- tree_info[i, ]
    node_id <- row$node
    
    left_child <- node_id * 2
    right_child <- node_id * 2 + 1
    
    left_exists <- left_child %in% tree_info$node
    right_exists <- right_child %in% tree_info$node
    
    if (left_exists || right_exists) {
      split_label <- row$splitLabel
      
      if (left_exists) {
        edge_label <- format_edge_label(split_label, is_left = TRUE, 
                                        style = split_label_style, max_len = max_label_length)
        
        # Edge tooltip with FULL condition (never truncated)
        full_condition <- get_full_condition(split_label, is_left = TRUE)
        edge_tooltip <- paste0(
          "<div style='padding: 8px; font-family: Arial; max-width: 400px; word-wrap: break-word;'>",
          "<b>Condition:</b><br>",
          "<span style='font-family: monospace; background: #e8f4e8; padding: 3px 6px;'>",
          full_condition,
          "</span>",
          "</div>"
        )
        
        edges_list <- c(edges_list, list(data.frame(
          from = node_id,
          to = left_child,
          label = edge_label,
          title = edge_tooltip,
          arrows = "to",
          color = "#666666",
          width = 1.5,
          font.size = edge_font_size,
          font.color = "#333333",
          font.align = "middle",
          stringsAsFactors = FALSE
        )))
      }
      
      if (right_exists) {
        edge_label <- format_edge_label(split_label, is_left = FALSE, 
                                        style = split_label_style, max_len = max_label_length)
        
        # Edge tooltip with FULL condition (never truncated)
        full_condition <- get_full_condition(split_label, is_left = FALSE)
        edge_tooltip <- paste0(
          "<div style='padding: 8px; font-family: Arial; max-width: 400px; word-wrap: break-word;'>",
          "<b>Condition:</b><br>",
          "<span style='font-family: monospace; background: #f4e8e8; padding: 3px 6px;'>",
          full_condition,
          "</span>",
          "</div>"
        )
        
        edges_list <- c(edges_list, list(data.frame(
          from = node_id,
          to = right_child,
          label = edge_label,
          title = edge_tooltip,
          arrows = "to",
          color = "#666666",
          width = 1.5,
          font.size = edge_font_size,
          font.color = "#333333",
          font.align = "middle",
          stringsAsFactors = FALSE
        )))
      }
    }
  }
  
  if (length(edges_list) > 0) {
    edges_df <- do.call(rbind, edges_list)
  } else {
    edges_df <- data.frame(from = integer(), to = integer(), stringsAsFactors = FALSE)
  }
  
  # =========================================================================
  # Create visNetwork
  # =========================================================================
  
  vis <- visNetwork::visNetwork(
    nodes = nodes_df,
    edges = edges_df,
    width = width,
    height = height
  )
  
  # Layout: hierarchical (fixed vertical) or free (using pre-calculated x,y)
  if (!free_drag) {
    # Standard hierarchical layout - nodes can only move horizontally
    vis <- visNetwork::visHierarchicalLayout(
      vis,
      direction = direction,
      levelSeparation = level_separation,
      nodeSpacing = node_spacing,
      sortMethod = "directed",
      shakeTowards = "roots"
    )
  }
  # If free_drag = TRUE, nodes use x,y coordinates and can be dragged anywhere
  
  # Physics disabled - allows dragging without snapping back
  vis <- visNetwork::visPhysics(vis, enabled = FALSE)
  
  # Interaction - depends on details_on setting
  if (details_on == "hover") {
    vis <- visNetwork::visInteraction(
      vis,
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE,
      hover = TRUE,
      hoverConnectedEdges = TRUE,
      tooltipDelay = 100,
      tooltipStay = 500,
      navigationButtons = navigation_buttons
    )
  } else {
    # For "click" or "none" - disable hover tooltips
    vis <- visNetwork::visInteraction(
      vis,
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE,
      hover = FALSE,
      hoverConnectedEdges = TRUE,
      tooltipDelay = 0,
      navigationButtons = navigation_buttons
    )
  }
  
  # Options - remove node selection dropdown to save space
  vis <- visNetwork::visOptions(
    vis,
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    nodesIdSelection = FALSE
  )
  
  # Add click event for details_on = "click"
  if (details_on == "click") {
    # Store node info in a data attribute for JavaScript access
    node_info_json <- sapply(nodes_list, function(x) {
      jsonlite::toJSON(list(
        id = x$id,
        title = x$title
      ), auto_unbox = TRUE)
    })
    
    vis <- visNetwork::visEvents(
      vis,
      click = sprintf("function(params) {
        if (params.nodes.length > 0) {
          var nodeId = params.nodes[0];
          var nodeInfo = %s;
          var info = nodeInfo[nodeId];
          if (info && info.title) {
            // Create a popup div
            var popup = document.getElementById('e2tree-popup');
            if (!popup) {
              popup = document.createElement('div');
              popup.id = 'e2tree-popup';
              popup.style.cssText = 'position:fixed; top:10px; right:10px; max-width:400px; max-height:80vh; overflow-y:auto; background:white; border:2px solid #333; border-radius:8px; box-shadow:0 4px 20px rgba(0,0,0,0.3); z-index:9999; padding:0;';
              document.body.appendChild(popup);
            }
            // Add close button and content
            popup.innerHTML = '<div style=\"display:flex; justify-content:space-between; align-items:center; background:#f0f0f0; padding:8px 12px; border-bottom:1px solid #ddd;\"><span style=\"font-weight:bold;\">Node Details</span><button onclick=\"this.parentElement.parentElement.style.display=\\'none\\'\" style=\"border:none; background:#e74c3c; color:white; border-radius:4px; padding:4px 10px; cursor:pointer;\">Close</button></div><div style=\"padding:10px;\">' + info.title + '</div>';
            popup.style.display = 'block';
          }
        }
      }", jsonlite::toJSON(setNames(lapply(nodes_list, function(x) list(id = x$id, title = x$title)), sapply(nodes_list, function(x) x$id))))
    )
  }
  
  # Export
  vis <- visNetwork::visExport(vis, type = "png", name = "e2tree_plot")
  
  return(vis)
}


#' Save E2Tree visNetwork Plot to HTML
#'
#' @param vis A visNetwork object from plot_e2tree_vis()
#' @param file Output file path (should end with .html)
#' @param selfcontained Include all dependencies in single file
#'
#' @export
save_e2tree_html <- function(vis, file = "e2tree_plot.html", selfcontained = TRUE) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Package 'htmlwidgets' is required.")
  }
  
  htmlwidgets::saveWidget(vis, file = file, selfcontained = selfcontained)
  message("Plot saved to: ", normalizePath(file))
}
