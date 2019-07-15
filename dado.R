dado <- function(nvar, imp, tipo.var, tabla){
    
    tablas <- list()
    
    tabla1 <- as.data.frame(permutations(6, nvar, repeats.allowed = TRUE))
    
    colnames(tabla1) <- paste0("x", 1:nvar)
    
    # colnames(tabla1) <- switch(nvar,
    #                            "1" = c("x1", "x2"),
    #                            "2" = c("x1", "x2"),
    #                            "3" = c("x1", "x2", "x3"),
    #                            "4" = c("x1", "x2", "x3", "x4"),
    #                            "5" = c("x1", "x2", "x3", "x4", "x5" ))
    
    tablas[[1]] <- tabla1
    
    variable <- tipo.var
    tablas[[1]][variable] <- switch(tipo.var, 
                                    x_raya = round(apply(tablas[[1]], 1, mean), digits = 2),
                                    x_suma = round(apply(tablas[[1]], 1, sum), digits = 2))
    
    
    
    dist_prob <- as.data.frame(table(tablas[[1]][variable]))
    dist_prob$prob <- round((1/6)^ nvar * dist_prob$Freq, 4)
    tablas[[2]] <- dist_prob[c("Var1", "prob")]
    
    names(tablas[[2]]) <- switch(tipo.var,
                                 x_raya = c("x_raya", "P(X_raya = x)"),
                                 x_suma = c("t", "P(T = t)"))
    #names(tablas[[2]]) <- c("x_raya", "P(X_raya = x)")
 
    if(imp == "tabla"){
        
       tabla.final <- switch(tabla,
                             permut = tablas[[1]],
                             prob = tablas[[2]])
       
       datatable(tabla.final)

    } else {
        
       # if(tipo.var == "x_raya") {
           
           # browser()
           ggplot(tablas[[2]], aes(x = tablas[[2]][[1]], y = tablas[[2]][[2]])) + 
               geom_bar(stat = "identity", width = 0.1, colour = "darkgreen", fill = "#009999") +
               labs(y = "probabilidad", x = switch(tipo.var,
                                                   x_raya = "x_raya",
                                                   x_suma = "t")) + 
               ggtitle(switch(tipo.var,
                              x_raya = "Distribucion de X_raya",
                              x_suma = "Distribucion de T"))  + theme_bw() +
               theme(plot.title = element_text(hjust = 0.5))
           
       # } 
       #  else {
       #     ggplot(tablas[[2]], aes(x = t, y = tablas[[2]][[2]])) + 
       #         geom_bar(stat = "identity", width = 0.1, colour = "darkgreen", fill = "#009999") +
       #         labs(y = "probabilidad") + theme_bw()
       # }
    }
}

