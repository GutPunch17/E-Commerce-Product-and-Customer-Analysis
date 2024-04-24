# count of products by primary categories
count_category <- function(df, cat1 = 'All', show_count = FALSE){
  if(cat1 == 'All'){
    result <- df %>%
      group_by(category1) %>%
      dplyr::summarize(category_count = n()) %>%
      arrange(desc(category_count))
  }else{result <- df %>%
    filter(category1 == cat1) %>%
    group_by(category2) %>%
    dplyr::summarize(category_count = n()) %>%
    arrange(desc(category_count)) %>%
    head(10)
  }
  
  if(cat1 == 'All'){
    plot <- ggplot(result, aes(x = reorder(category1, -category_count),
               y = category_count,
               fill = category_count)) +
      labs(x = 'Main Categories', y = 'Count of Products', title = paste("Top Categories"), fill = "Product Count")
  }else{
    plot <- ggplot(result, aes(x = reorder(category2, -category_count), 
               y = category_count,
               fill = category_count)) +
               labs(x = cat1, y = 'Count of Subcategories', title = paste("Top Subcategories in", cat1), fill = "Product Count")
      
  }
  
  # plot <- ggplot(result, aes(x = reorder(category2, -category_count), 
  #              y = category_count,
  #              fill = category_count))+
    
  plot <- plot +  geom_bar(stat = 'identity') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1, size = 13),
          axis.text.y = element_text(size = 13),
          axis.title = element_text(size = 15),
          plot.margin = unit(c(0.5, 0.5, 0.75, 0.75), "inches"),
          plot.title = element_text(size = 17, face = 'bold'),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
  if(show_count){
    plot <- plot + geom_text(aes(label = category_count), vjust = -0.5)
  }
  return(plot)
}


# count of ratings by primary categories
count_rating_bubble <- function(df, cat1, show_values = TRUE) {
  if(cat1 == 'All'){
    summarized_df <- df %>%
      group_by(category1) %>%
      dplyr::summarize(mean_rating_count = round(mean(rating_count, na.rm = TRUE), 2)) %>%
      arrange(desc(mean_rating_count))
  }else{summarized_df <- df %>%
    filter(category1 == cat1) %>%
    group_by(category2) %>%
    dplyr::summarize(mean_rating_count = round(mean(rating_count, na.rm = TRUE), 2)) %>%
    arrange(desc(mean_rating_count))
  }
  
  if(cat1 == 'All'){
    cat = 'category1'
  }else{
    cat = 'category2'
  }
  
  # determine to show value or not
  if (show_values) {
    summarized_df <- summarized_df %>%
      mutate(label = paste(!!sym(cat), mean_rating_count, sep = "\n"))
  } else {
    summarized_df <- summarized_df %>%
      mutate(label = !!sym(cat))
  }  
  
  # Calculate circle packing layout
  packing <- circleProgressiveLayout(summarized_df$mean_rating_count / 2)
  dat.gg <- circleLayoutVertices(packing)
  new_df <- cbind(summarized_df, packing)
  
  result <- ggplot(data = dat.gg) +
    geom_polygon(aes(x, y, group = id, fill = factor(id)), colour = "white", show.legend = FALSE) +
    scale_y_reverse() +
    coord_equal() +
    scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 1) +
    geom_text(data = new_df, aes(x, y, label = label), size = 5) +  
    labs(title = paste('Most Popular Subcategories in', cat1)) +
    theme_void() +
    theme(plot.title = element_text(size = 17, face = 'bold'))
  
  return(result)
}



# percentage of high rating products (rating>=4.3)
high_rating_pct <- function(df, cat1, show_pct = FALSE){
  if(cat1 == 'All'){
    result <- df
    cat = 'category1'
  }else{
    result <- df %>% 
    filter(category1 == cat1)
    cat = 'category2'
  }
  
  result <- result %>%
    mutate(high_rating = ifelse(rating >= 4.3, 1, 0)) %>%
    group_by(!!sym(cat)) %>%
    filter(n() > 1) %>%
    dplyr::summarize(high_rating_percentage = round(mean(high_rating, na.rm = TRUE), 4)) %>%
    arrange(desc(high_rating_percentage))
  
  plot <- ggplot(result, aes(x = reorder(!!sym(cat), -high_rating_percentage), y = high_rating_percentage, fill = high_rating_percentage)) +
    geom_bar(stat = 'identity') +
    labs(x = cat1, y = 'High Rating Percentages', title = paste('Most highly rated Subcategories in', cat1), fill = 'Percentages')
  if(cat1 == 'All'){
    plot <- plot +
      labs(x = 'Main Categories', y = 'High Rating Percentages', title = paste('Most highly rated Main Categories'), fill = 'Percentages')
  }
  plot <- plot+
    theme_bw() +
    theme(axis.text.x = element_text(angle  = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 13),
          axis.title = element_text(size = 15),
          plot.margin = unit(c(0.5, 0.5, 0.75, 0.75), "inches"),
          plot.title = element_text(size = 17, face = 'bold'),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
  
  if (show_pct) {
    plot <- plot + geom_text(aes(label = paste0(round(high_rating_percentage * 100, 2), "%")), vjust = -0.5)
  }
  return(plot)
}




# word cloud for high rating products
review_word_cloud <- function(df, cat1, word_num = 150){
  if(cat1 == 'All'){
    result <- df
  }else{
    result <- df %>%
      filter(category1 == cat1) 
  }
  corpus <- Corpus(VectorSource(result$review_content))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords('en'))
  corpus <- tm_map(corpus,FUN = stripWhitespace)
  corpus <- tm_map(corpus,FUN = stemDocument)
  
  wc <- wordcloud(words = corpus, max.words = word_num, scale = c(5, 0.5), random.order = FALSE, rot.per = 0.35, user.r.layout = FALSE, colors = brewer.pal(8, 'Dark2'))
  
  return(wc)
}



# sentiment distribution of reviews with different ratings
high_rating_sentiment <- function(df, cat1 = NULL, rating_range = c(2,5), afn){
  if(cat1 == 'All'){
    result <- df
  }else{
    result <- df %>% filter(category1 == cat1)
  }
  # return(rating_range[1])
  result <- result %>%
    filter(rating >= rating_range[1] & rating <= rating_range[2]) %>%
    group_by(product_id) %>%
    unnest_tokens(output = word, input = review_content) %>%
    inner_join(afn) %>%
    dplyr::summarize(review_sentiment = mean(value, na.rm = TRUE)) %>%
    ggplot(aes(x = review_sentiment, fill = review_sentiment > 0)) +
    geom_histogram(binwidth = 0.05) +
    scale_x_continuous(breaks=seq(-5,5,1)) +
    # labs(x = 'Sentiment Score', y = 'Count', title = paste('Sentiment Distribution of', cat1)) +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, size = 15),
          axis.text.y = element_text(size = 13),
          axis.title = element_text(size = 15),
          plot.margin = unit(c(0.5, 0.5, 0.75, 0.75), "inches"),
          plot.title = element_text(size = 17, face = 'bold'),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)) +
    guides(fill = guide_legend(title = "Positive/Negative Sentiment"))
  
  if(cat1 == 'All'){
    result <- result + labs(x = 'Sentiment Score', y = 'Count', title = paste('Sentiment Distribution of All Products'))
  }else{
    result <- result + labs(x = 'Sentiment Score', y = 'Count', title = paste('Sentiment Distribution of', cat1))
  }
  return(result)
}


# discount percentage by category
discount_pct <- function(df, cat1, show_pct = TRUE){
  if(cat1 == 'All'){
    result <- df
    cat <- 'category1'
  }else{
    result <- df %>%
      filter(category1 == cat1)
    cat <- 'category2'
  }
  
  result <- result %>%
    group_by(!!sym(cat)) %>%
    dplyr::summarize(mean_pct = mean(discount_percentage, na.rm = TRUE)) %>%
    arrange(desc(mean_pct))
  
  plot <- ggplot(result, aes(x = reorder(!!sym(cat), -mean_pct), y = mean_pct, fill = mean_pct)) +
    geom_bar(stat = 'identity') +
    labs(x = cat1, y = 'Average Discount Percentages', title = paste('Average Discount Percentages in', cat1), fill = 'Percentages') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 13),
          axis.title = element_text(size = 15),
          plot.margin = unit(c(0.5, 0.5, 0.75, 0.75), "inches"),
          plot.title = element_text(size = 17, face = 'bold'),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
  if(cat1 == 'All'){
    plot <- plot + 
      labs(x = 'Main Categories', y = 'Average Discount Percentages', title = paste('Average Discount Percentages'), fill = 'Percentages')
  }
  
  if (show_pct) {
    plot <- plot + geom_text(aes(label = paste0(round(mean_pct * 100, 2), "%")), vjust = -0.5)
  }
  return(plot)
}



# discount percentage vs rating
discount_rating <- function(df, cat1, show_line = TRUE){
  if(cat1 == 'All'){
    result <- df
    cat <- 'category1'
  }else{
    result <- df %>%
      filter(category1 == cat1)
    cat <- 'category2'
  }
  
  result <- result %>%
    group_by(!!sym(cat)) %>%
    dplyr::summarize(mean_pct = mean(discount_percentage, na.rm = TRUE), mean_rating = mean(rating, na.rm = TRUE)) %>%
    arrange(desc(mean_pct))
  
  plot <- ggplot(result, aes(x = mean_rating, y = mean_pct)) +
    geom_point() +
    # geom_smooth(method = "lm", se = FALSE, color = "#00AFBB") +
    labs(x = 'Mean Rating', y = 'Mean Discount Percent', title = paste('Discount Rate vs. Averate Rating'), fill = 'Percentages') +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 15),
          plot.margin = unit(c(0.5, 0.5, 0.75, 0.75), "inches"),
          plot.title = element_text(size = 17, face = 'bold'))
  
  if (show_line) {
    plot <- plot + geom_smooth(method = "lm", se = FALSE, color = "#00AFBB")
  }
  return(plot)
}



# geographical distribution of customers
geographics <- function(df, show_state = FALSE, show_value = FALSE){
  states_map <- map_data("state") %>% mutate(region = str_to_title(region))
  state_map <- data.frame(region = state.name, abbreviation = state.abb)
  # return(state_map)
  result <- df %>%
    group_by(state) %>%
    dplyr::summarize(occurrence = n()) %>%
    rename('region' = 'state') %>%
    left_join(state_map, by = "region")
  
  if(show_state & !show_value){
    result <- result %>% mutate(label = paste(abbreviation))
  }else if(!show_state & show_value){
    result <- result %>% mutate(label = paste(occurrence))
  }else if(show_state & show_value){
    result <- result %>% mutate(label = paste(abbreviation, occurrence, sep = "\n")) 
  }else{
    result <- result %>% mutate(label = '')
  }
  
  center_points <- states_map %>%
    group_by(region) %>%
    dplyr::summarize(center_long = mean(long, na.rm = TRUE),
              center_lat = mean(lat, na.rm = TRUE)) %>%
    inner_join(result, by = "region")
  
  merged_data <- inner_join(states_map, result, by = "region")
  
  plot <- ggplot() +
    geom_polygon(data = merged_data, aes(x = long, y = lat, group = group, fill = occurrence), color = "white") +
    # scale_fill_gradient(low = "lightblue", high = "darkblue") +
    scale_fill_gradientn(colors = brewer.pal(9, "Oranges")) +
    labs(fill = "Occurrence") +
    geom_text(data = center_points, aes(x = center_long, y = center_lat, label = label), size = 3, check_overlap = FALSE, colour = 'black', size = 5) +
    theme_void() +
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
  
  return(plot)
}




# city distribution
city_bubble <- function(df, stt, show_values = TRUE) {
  if(stt != 'All'){
    summarized_df <- df %>%
      filter(state == stt) %>%
      group_by(city) %>%
      dplyr::summarize(ct = n()) %>%
      arrange(desc(ct)) %>%
      head(10)
    reg <- 'city'
  }else{
    summarized_df <- df %>%
      group_by(state) %>%
      dplyr::summarize(ct = n()) %>%
      arrange(desc(ct)) %>%
      head(10) 
    reg <- 'state'
  }
  
  if (show_values) {
    summarized_df <- summarized_df %>%
      mutate(label = paste(!!sym(reg), ct, sep = "\n"))
  } else {
    summarized_df <- summarized_df %>%
      mutate(label = !!sym(reg))
  }  
  
  # Calculate circle packing layout
  packing <- circleProgressiveLayout(summarized_df$ct / 2)
  dat.gg <- circleLayoutVertices(packing)
  new_df <- cbind(summarized_df, packing)
  
  result <- ggplot(data = dat.gg) +
    geom_polygon(aes(x, y, group = id, fill = factor(id)), colour = "white", show.legend = FALSE) +
    scale_y_reverse() +
    coord_equal() +
    scale_fill_viridis(discrete = TRUE, option = 'G', begin = 0, end = 0.8) +
    geom_text(data = new_df, aes(x, y, label = label), color = 'white', size = 5) +
    theme_void() +
    theme(plot.title = element_text(size = 17, face = 'bold'))
  if(stt != 'All'){
    result <- result + labs(title = paste('Number of Customers by City in', stt))
  }else{
    result <- result + labs(title = paste('Number of Customers by States'))
  }
  return(result)
}



# Gender distribution
gender_dist <- function(df, stt, show_gender = FALSE, show_pct = FALSE){
  if(stt != 'All'){
    df <- df %>%
      filter(state == stt)
  }
  result <- df %>%
    group_by(gender) %>%
    dplyr::summarize(ct = n()) %>%
    mutate(pct = 100* ct / sum(ct),
           label = paste(round(pct, 1), "%"))
  result2 <- result %>%
    mutate(csum = rev(cumsum(rev(pct))),
           pos = pct/2 + lead(csum, 1),
           pos = if_else(is.na(pos), pct/2, pos))

  plot <- ggplot(result, aes(x = "" , y = pct, fill = fct_inorder(gender))) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Pastel2") +
    guides(fill = guide_legend(title = "Gender")) +
    theme_void() +
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))

  if(show_pct & show_gender){
    plot <- plot + geom_label_repel(data = result2,
                                    aes(y = pos, label = paste(gender, '-', round(pct, 1), "%")),
                                    size = 4.5, nudge_x = 1, show.legend = FALSE)
  }else if(show_gender & !show_pct){
    plot <- plot + geom_label_repel(data = result2,
                                    aes(y = pos, label = paste(gender)),
                                    size = 4.5, nudge_x = 1, show.legend = FALSE)
  }else if(!show_gender & show_pct){
    plot <- plot + geom_label_repel(data = result2,
                                    aes(y = pos, label = paste(round(pct, 1), "%")),
                                    size = 4.5, nudge_x = 1, show.legend = FALSE)
  }
  return(plot)
}









































