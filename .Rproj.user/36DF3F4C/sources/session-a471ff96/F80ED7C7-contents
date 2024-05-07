# creating a function for a one way bar plot

barchart <- function(df, x, title) {
  require(ggplot2)
  require(plotly)
  
  # Calculate frequency count of categories
  freq_count <- table(df[[x]])
  
  # Convert frequency count to a data frame
  df_freq <- as.data.frame(freq_count)
  colnames(df_freq) <- c("Categories", "Frequency")
  
  # Create ggplot object
  p <- ggplot(df_freq, aes_string(x = "Categories", y = "Frequency")) +
    geom_bar(fill = "red", alpha = 0.7, stat = "identity") +
    geom_text(aes(label = Frequency), vjust = -0.5) + 
    labs(title = title,
         x = "Categories",
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p)
}




# creating a fucntion  for a two way barchart 

barchart_2_way <- function(df, x, y, title) {
  require(ggplot2)
  
  # Calculate frequency count of categories
  
  freq_count <- table(df[[x]], df[[y]])
  
  # Convert frequency count to a data frame
  
  df_freq <- as.data.frame(freq_count)
  colnames(df_freq) <- c(x, y, "Frequency")
  
  # Create ggplot object
  p <- ggplot(df_freq, aes_string(x = x, y = "Frequency", fill = y)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
    labs(title = title,
         x = x,
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  ggplotly(p)
}