frec <- function(pteo, repe) {
    
    result <- c()
    
    for (i in 1:repe) {
        
        sim_bin <- rbinom(i, 1, prob = pteo)
        prob_cara <- sum(sim_bin) / length(sim_bin)
        result <- round(c(result, prob_cara), 6)
    }
    
    x <- c(1:repe)
    data <- data.frame(x, result)
    plot_ly(data, x = ~x, y = ~result, type = 'scatter', mode = 'lines') %>%
    layout(xaxis = list(title = "cantidad de experimentos"), 
           yaxis = list(title = "frec. relativa de cara"))
}



