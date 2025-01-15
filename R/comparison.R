# Define a function to process heatmaps and perform Mantel test
# The comparison is between the heatmap of the matrix O obtained from the RF output and the heatmap of the matrix O estimated by E2Tree
comparison <- function(train, tree, D, is_classification = TRUE) {
  # Get the number of observations in the training data
  n <- nrow(train)
  
  # Extract the tree structure from the tree object
  df <- tree$tree
  
  # Identify terminal nodes in the tree
  terminal_nodes <- df$node[df$terminal]
  
  # Initialize a matrix to store probabilities
  Ps <- matrix(0, n, n)
  
  # Populate the probability matrix Ps based on classification or regression
  for (i in terminal_nodes) {
    # Extract observations corresponding to the current terminal node
    obs <- eval(parse(text = df$obs[df$node == i]))
    
    # Populate Ps using the appropriate column based on the task type
    if (is_classification) {
      # Assign the probability of the current terminal node to the respective cells
      Ps[obs, obs] <- df$prob[df$node == i]
    } else {
      Ps[obs, obs] <- df$Wt[df$node == i]
    }
  }

  # Set diagonal elements of Ps to 1
  diag(Ps) <- 1
  
  # Assign row and column names to the Ps matrix
  rownames(Ps) <- 1:nrow(Ps)
  colnames(Ps) <- 1:ncol(Ps)
  
  # Use the provided O matrix
  D_exp <- D
  
  # Perform hierarchical clustering on the O matrix
  clusD <- hclust(as.dist(D_exp))
  
  # Extract the order of observations based on clustering
  order <- clusD$order
  
  # Reorder the Ps matrix based on the clustering order
  Ps_ord <- Ps[order, order]
  
  # Update row and column names to reflect the new order
  rownames(Ps_ord) <- order
  colnames(Ps_ord) <- order
  
  # Create a black-and-white color palette for the heatmaps
  bw_palette <- colorRampPalette(c("white", "black"))(100)
  
  # Generate a heatmap of the matrix O estimated by E2Tree
  heatmap(
    sqrt(Ps_ord), 
    Rowv = NA, 
    Colv = NA, 
    scale = "none", 
    col = bw_palette, 
    main = "E2Tree Heatmap"
  )
  
  # Generate a heatmap of the matrix O obtained from the RF output matrix
  heatmap(
    sqrt(1 - D_exp[order, order]), 
    Rowv = NA, 
    Colv = NA, 
    scale = "none", 
    col = bw_palette, 
    main = "Random Forest Heatmap"
  )
  
  # Perform Mantel test between the two
  mantel_test <- ape::mantel.test(
    Ps_ord, 
    1 - D_exp[order, order], 
    graph = TRUE, 
    main = "Mantel test"
  )
  
  # Return only the Mantel test result (graph included in mantel.test)
  return(mantel_test)
}
