
binomial <- function(n, defec, tipo.graf, p) {
    
    prob <- c(0, seq(0.01, 1, by = 0.01))
    x <- 0 : n
    
    t.curvas <- data.frame(prob.exito = prob, prob.acum = pbinom(defec, n, prob))
    t.hist <- data.frame(X = x, prob.puntual = dbinom(x, n, p))
    
    if(tipo.graf == "max.defec") {
        plot_ly(t.curvas, x = ~prob.exito, y = ~prob.acum, type = 'scatter', mode = 'lines')
        #ggplot(t.curvas, aes(prob.exito, prob.acum)) + geom_line(colour = "red")
        
    } else {
            plot_ly(t.hist, x = ~X, y = ~prob.puntual, type = 'bar') %>%
            add_lines(x = defec + 0.5)
            #ggplot(t.hist, aes(x = X, y = prob.puntual)) +  geom_bar(stat = "identity") 
    }
}