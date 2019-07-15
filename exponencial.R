exponencial <- function(n, tipo.var, intervalos, alfa, grafico, parametros) {
    
    exp <- c()
    
    if (tipo.var == "x_raya"){
        for (i in 1:1000) {
            exp <- c(exp, mean(rexp(n, rate = alfa)))
        }
        } else {
            for (i in 1:1000) {
                exp <- c(exp, sum(rexp(n, rate = alfa)))
                # print(last(exp))
            }
        }
    
    if (parametros == "media") {return(mean(exp))
        
        } else if (parametros == "desvio") {return(sd(exp))
        
            } else {
            
                exp1 <- data.frame(exp)
                
                if(grafico == TRUE) {
                    
                    ggplot(exp1, aes(x = exp)) +  geom_histogram(bins = intervalos, colour = "darkgreen", fill = "#009999") +
                        stat_function( 
                            fun = function(x, media, desvio, n, bw){ 
                                dnorm(x = x, mean = media, sd = desvio) * n * bw
                            }, 
                            args = c(media = mean(exp1$exp), desvio = sd(exp1$exp), n = 1000, 
                                     bw = (max(exp1$exp) - min(exp1$exp)) / intervalos)) +
                        labs(y = "probabilidad", x = switch(tipo.var,
                                                            x_raya = "x_raya",
                                                            x_suma = "t")) + 
                        ggtitle(switch(tipo.var,
                                       x_raya = "Distribucion de X_raya",
                                       x_suma = "Distribucion de T"))  + theme_bw() +
                        theme(plot.title = element_text(hjust = 0.5)) 
                    
                } else {
                    
                    # browser()
                    grafico <- ggplot(exp1, aes(x = exp)) +  geom_histogram(bins = intervalos, colour = "darkgreen", fill = "#009999") 
                    pg <- ggplot_build(grafico) 
                    
                    intervalos <- paste0("(", round(pg$data[[1]][, 4], 2), ",", round(pg$data[[1]][, 5], 2), "]")
                    
                    tabla <- data.frame(Intervalos = intervalos, PM = round(pg$data[[1]][, 3], 2), f = pg$data[[1]][, 1]) %>%
                        mutate(fr = round(f / sum(f), 4))
                    
                    frecAcumu <- tabla[1, 3]
                    frecAcRelat <- tabla[1, 4]
                    
                    for (i in 2 : dim(tabla)[1]) {
                        
                        frecAcumu <- c(frecAcumu, last(frecAcumu) + tabla[i, 3])
                        frecAcRelat <- c(frecAcRelat, last(frecAcRelat) + tabla[i, 4])
                    }
                    
                    tabla$F.abs. <- frecAcumu
                    tabla$F.relat <- frecAcRelat
                    names(tabla)[5:6] <- c("F", "Fr")
                    
                    datatable(tabla)
        }
    }
} 

    

