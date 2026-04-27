require(ggplot2, quietly = TRUE)
require(topicmodels, quietly = TRUE)
require(tibble, quietly = TRUE)
require(tidyr, quietly = TRUE)

plot_topic_words <- function(tm, topic.nr = 1, n.words = 10, xlims = NULL) {
  tm_estimates <- posterior(tm)
  phi_topic <- tm_estimates$terms[topic.nr, ]
  top_idx <- order(phi_topic, decreasing = TRUE)[1:n.words]
  
  plot_data <- data.frame(
    term = names(phi_topic)[top_idx],
    prob = phi_topic[top_idx]
  )
  
  if (is.null(xlims)) {
    xlims <- c(0.0, max(plot_data$prob) * 1.2)
  }
  
  ggplot(plot_data, aes(x = reorder(term, prob), y = prob)) +
    geom_col() +
    ylim(xlims) +
    coord_flip() +
    labs(
      x = NULL,
      y = "Pr(term | topic)",
      title = paste("Top terms for topic", topic.nr)
    )
  
}


plot_heatmap <- function(x, what = "topic") {
  ord <- hclust( dist(x, method = "euclidean"), method = "ward.D" )$order
  x <- tibble::rownames_to_column(as.data.frame(x[ord, ]), var = "rowname")
  x$rowname <- factor(x$rowname, levels = x$rowname)
  x <- tidyr::pivot_longer(x, -rowname, names_to = "col", values_to = "prob")
  if (!is.null(what) && what == "topic")
    x$col <- factor(x$col, levels = sort(unique(as.integer(x$col))), ordered = TRUE)
  
  p <- ggplot(x, aes(y=rowname, x=col, fill=prob)) +
    geom_tile() +
    scale_fill_gradient2(
      low=scales::muted("blue"), 
      high=scales::muted("red")
    ) + 
    ylab(NULL) +
    theme(legend.position = "none")
  
  if (!is.null(what))
    p <- p + labs(x = what)
  
  p
}
