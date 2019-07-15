
ruleta <- function(n) {
    
    a <- str_pad(sample(1:36, n, replace = T), 2, pad = "0")
    
    b <- data.frame(numero = names(table(a)))
    print(table(a))
    b <- cbind(b, probabilidad = matrix(table(a)) / length(a))
    
    print(b)
    plot_ly(b, x = ~numero, y = ~probabilidad, type = 'bar')
}

