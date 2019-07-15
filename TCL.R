require(ggplot2)

#------Simulación del dado-------------------

dado <- function(nvar){
    
    matriz <- as.data.frame(permutations(6, nvar, repeats.allowed = TRUE))
    matriz$suma <- apply(matriz, 1, sum)
    dist_prob <- as.data.frame(table(matriz$suma))
    dist_prob$prob <- (1/6)^nvar * dist_prob$Freq
    
    ggplot(dist_prob, aes(x=Var1, y = prob)) + geom_bar(stat = "identity", width = 0.5, fill = "blue")
}

dist_dado <- data.frame(X_1 = rep(1 : 6, c(2*3, 2*3, 2*3, 2*3, 2*3, 2*3)), 
                        X_2 = rep(1 : 6, 6), prob = rep(prob*prob, 6))

dist_dado$Y <- apply(dist_dado[,c(1, 2)], 1, sum)

dist <- as.data.frame(table(dist_dado$Y))
dist$prob <- ((1/6)^2)*dist$Freq

ggplot(dist, aes(x=Var1, y = prob)) + geom_bar(stat = "identity", width = 0.5, fill = "blue")


#----------------Dist. Exponencial------------

}
exp <- c()

for (i in 1:1000) {
    
    exp <- c(exp, sum(rexp(30, rate = 1)))
   
}
exp <- data.frame(exp)
hist(exp)
ggplot(exp, aes(x = exp)) + geom_histogram(bins = 10, fill = "blue")


