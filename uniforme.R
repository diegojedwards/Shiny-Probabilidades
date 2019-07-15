uniforme <- function(n, tipo.var, intervalos, table) {
    
    uni <- c()
    
    if (tipo.var == "x_raya"){
        
        for (i in 1:1000) {
            
            uni <- c(uni, mean(runif(n, 0, 1)))
        }
    } else {
        
        for (i in 1:1000) {
            
            uni <- c(uni, sum(runif(n, 0, 1)))
        }
    }
    uni <- data.frame(uni)
    
    if(table == TRUE) {
        
        tabla.frec <- with(uni, graph.freq(uni, plot=FALSE))
        #colnames(tabla.frec) <- c("lim. inferior", "lim. superior", "MC", "f", "fr%", "F", "Fr%")
        datatable(table.freq(tabla.frec))
    
    } else {
        
        ggplot(uni, aes(x = uni)) + geom_histogram(bins = intervalos, colour = "darkgreen", fill = "#009999") +
            stat_function( 
                fun = function(x, media, desvio, n, bw){ 
                    dnorm(x = x, mean = media, sd = desvio) * n * bw
                }, 
                args = c(media = mean(uni$uni), desvio = sd(uni$uni), n = 1000, 
                         bw = (max(uni$uni) - min(uni$uni)) / intervalos)) +
            labs(y = "probabilidad", x = switch(tipo.var,
                                                x_raya = "x_raya",
                                                x_suma = "t")) + 
            ggtitle(switch(tipo.var,
                           x_raya = "Distribucion de X_raya",
                           x_suma = "Distribucion de T"))  + theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))
    }
    
    
}