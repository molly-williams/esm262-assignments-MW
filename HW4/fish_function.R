#' Fish function
#' @title Fish Revenue Calculation Function 
#' @description This function determines most frequently caught fish and total revenue by location, and the total revenue for all values.
#' @param l count and type of fish at each location
#' @param p fish price by type
#' @param graph_result if TRUE, output a graphical representation of total fisheries revenue for each site based on type and fish catch count. Defaule = FALSE
#' @return most frequently caught fish species, total revenue by location and in total, total fisheries revenue sum for all location, graph if graph = TRUE
#'
#' @author Molly Williams and Mario Colon 

calc_fish_revenue = function(l, p, graph_result = TRUE, total_revenue = TRUE) {

  ##Max Frequency 
  
  #subset each location 
  
  hawaii_freq <- fish_catch %>% 
    select(species, hawaii_catch) %>% 
    filter(hawaii_catch == max(hawaii_catch)) %>% 
    melt(id = "species") %>% 
    rename(location = variable, catch = value)
  
  alaska_freq <- fish_catch %>% 
    select(species, alaska_catch) %>% 
    filter(alaska_catch == max(alaska_catch)) %>% 
    melt(id = "species") %>% 
    rename(location = variable, catch = value)
  
  california_freq <- fish_catch %>% 
    select(species, california_catch) %>% 
    filter(california_catch == max(california_catch)) %>% 
    melt(id = "species") %>% 
    rename(location = variable, catch = value)
  
  #create dataframe for highest caught fish
  max_freq <- rbind(hawaii_freq, alaska_freq, california_freq)
  
  
  ##Total Revenue for each location 
  
  fish_revenue <- data.frame(fish = fish_catch$species,
                             Hawaii = fish_catch[,2] * fish_prices[,2],
                             Alaska = fish_catch[,3] * fish_prices[,2], 
                             California = fish_catch[,4] * fish_prices[,2])
  
  # Expand into tidy data
  fish_revenue <- fish_revenue %>% 
    melt(id = "fish") %>% 
    rename(catch = value, location = variable)
  
  #sum each location 
  rev <- fish_revenue %>% 
    group_by(location) %>% 
    summarise(total_rev = sum(catch))
  
  rev = as.data.frame(rev)
  
  ##Total Fisheries Revenue Sum 
  
  total_fish_rev = sum(rev$total_rev)
  
  ##Optional Graph 
  
  rev_graph = as.data.frame(rev)
  
  final_graph = ggplot(rev_graph, aes(x=location, y = total_rev)) +
    geom_bar(stat = "identity", fill = "cyan", col = "purple") +
    theme_classic() +
    ylab("Total Revenue") +
    xlab("Catch Location") +
    labs(title = "Total Revenue by Catch Location",
         subtitle = "Total overall revenue = $52,700")
  
  
  ##Returns
  
  
  # with graph
    
  if(graph_result == TRUE) return(list("Most_Frequent_Fish" = as.matrix(max_freq), "Per_Site_Revenue" = as.matrix(rev),
                                       "Total_Fisheries_Revenue" = as.character(total_fish_rev), "Total_Fisheries_Graph" = final_graph))
                    
  
  # without graph 
  
  if(graph_result == FALSE) return(list("Most_Frequent_Fish" = as.matrix(max_freq), "Per_Site_Revenue" = as.matrix(rev), 
                                       "Total_Fisheries_Revenue" = as.character(total_fish_rev)))

  }


