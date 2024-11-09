kwantyle <- zad1(20, 20, 0, 1, 0, 1)
cw <- kwantyle[1]
cab <- kwantyle[2]
cl <- kwantyle[3]
cks <- kwantyle[4]

zad2 <- function(fun, m, n, mi_1 = 0, sigma_1 = 1, mi_2, sigma_2){
  N <- 10000
  W <- matrix(0, 1, N)
  AB <- matrix(0, 1, N)
  L <- matrix(0, 1, N)
  KS <- matrix(0, 1, N)
  
  czestosci_W <- matrix(0, 1, N)
  czestosci_AB <- matrix(0, 1, N)
  czestosci_L <- matrix(0, 1, N)
  czestosci_KS <- matrix(0, 1, N)
  
  for(i in 1:N){
    X <- fun(m, mi_1, sigma_1)
    Y <- fun(n, mi_2, sigma_2)
    
    W[,i] <- stat_W(X,Y)
    AB[,i] <- stat_AB(X,Y)
    L[,i] <- stat_L(X,Y)
    KS[,i] <- stat_KS(X,Y)
    
    if(W[,i] > cw) czestosci_W[,i] <- 1 else czestosci_W[,i] <- 0
    if(AB[,i] > cab) czestosci_AB[,i] <- 1 else czestosci_AB[,i] <- 0
    if(L[,i] > cl) czestosci_L[,i] <- 1 else czestosci_L[,i] <- 0
    if(KS[,i] > cks) czestosci_KS[,i] <- 1 else czestosci_KS[,i] <- 0
  }
  
  moc_W <- mean(czestosci_W)
  moc_AB <- mean(czestosci_AB)
  moc_L <- mean(czestosci_L)
  moc_KS <- mean(czestosci_KS)
  
  return(c(moc_W, moc_AB, moc_L, moc_KS))
}

wykres <- function(fun, m, n, mi_1 = rep(0, times = 7), sigma_1 = rep(1, times = 7), mi_2, sigma_2, tytul, osx = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4), nosx, ile, l_x = 0.2, l_y = 0.95){
  moce <- matrix(0, nrow = 7, ncol = 4)
  for(i in 1:7){
    moce[i,] <- zad2(fun, m, n, mi_1[i], sigma_1[i], mi_2[i], sigma_2[i])
  }
  
  plot(osx, moce[,1], ylim = c(0, 1), type="o", pch=15, lty=1, lwd=1, col="indianred", xlab = nosx, ylab = "Moc testu")
  title(main = tytul, cex.main = 1.3, font.main = 3)
  lines(osx, moce[,2], type="o", pch=16, lty=1, lwd=1, col="dodgerblue1")
  lines(osx, moce[,3], type="o", pch=17, lty=1, lwd=1, col="olivedrab")
  lines(osx, moce[,4], type="o", pch=10, lty=1, lwd=1, col="sandybrown")
  legend(l_x, l_y, c(ile ,expression(W), expression(AB), expression(L), expression(KS)), pch=c(20, 15, 16, 17, 10), col = c("white","indianred", "dodgerblue1", "olivedrab", "sandybrown"), cex=0.8, box.lty=0)
}

wykres_4 <- function(fun, m, n, mi_1 = rep(0, times = 7), sigma_1 = rep(1, times = 7), mi_2, sigma_2, tytul, osx = seq(0, 3, by = 0.5), ile, l_x, l_y){
  moce <- matrix(0, nrow = 7, ncol = 4)
  for(i in 1:7){
    moce[i,] <- zad2(fun, m, n, mi_1[i], sigma_1[i], mi_2[i], sigma_2[i])
  }
  
  plot(seq(0, 3, by = 0.5), moce[,1], ylim = c(0, 1), type="o", pch=15, lty=1, lwd=1, col="indianred", ylab = "Moc testu", xaxt = "n", yaxt = "n", xlab = "")
  title(xlab = bquote("("~mu[2]~","~sigma[2]~")"), mgp = c(4, 1, 0), cex.lab=1.2)
  axis(1, at = seq(0, 3, by = 0.5), las = 2,
       labels = c(expression("("*0.2*","*1.0*")"), 
                  expression("("*0.4*","*1.5*")"), 
                  expression("("*0.6*","*2.0*")"), 
                  expression("("*0.8*","*2.5*")"), 
                  expression("("*1.0*","*3.0*")"), 
                  expression("("*1.2*","*3.5*")"), 
                  expression("("*1.4*","*4.0*")")))
  axis(2, seq(0, 1, by = 0.2))
  title(main = tytul, cex.main = 1.3, font.main = 3)
  lines(seq(0, 3, by = 0.5), moce[,2], type="o", pch=16, lty=1, lwd=1, col="dodgerblue1")
  lines(seq(0, 3, by = 0.5), moce[,3], type="o", pch=17, lty=1, lwd=1, col="olivedrab")
  lines(seq(0, 3, by = 0.5), moce[,4], type="o", pch=10, lty=1, lwd=1, col="sandybrown")
  legend(0.5, 1, c(ile, expression(W), expression(AB), expression(L), expression(KS)), pch=c(20, 15, 16, 17, 10), col = c("white","indianred", "dodgerblue1", "olivedrab", "sandybrown"), cex=0.8, box.lty=0)
}

# a)
par(mfrow = c(2,3))
wykres(rnorm, 20, 20, mi_2 = seq(0.2, 1.4, by = 0.2), sigma_2 = rep(1, times = 7), tytul = expression(N(0, 1)/N(mu[2], 1)), nosx = expression(mu[2]), ile = expression("n = 20"))

# b)
wykres(rlogis, 20, 20, mi_2 =  seq(0.2, 1.4, by = 0.2), sigma_2 = rep(1, times = 7), tytul = expression(L(0, 1)/L(mu[2], 1)), nosx = expression(mu[2]), ile = expression("n = 20"))

# c)
wykres(rcauchy, 20, 20, mi_2 = seq(0, 3, by = 0.5), sigma_2 = rep(1, times = 7), tytul = expression(C(0, 1)/C(mu[2], 1)), nosx = expression(mu[2]), ile = expression("n = 20"), osx = seq(0, 3, by = 0.5), l_x = 0.1, l_y = 0.95)

# a)
wykres(rnorm, 50, 50, mi_2 = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4), sigma_2 = rep(1, times = 7), tytul = expression(N(0, 1)/N(mu[2], 1)), nosx = expression(mu[2]), ile = expression("n = 50"), l_x = 1.0, l_y = 0.75)

# b)
wykres(rlogis, 50, 50, mi_2 =  c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4), sigma_2 = rep(1, times = 7), tytul = expression(L(0, 1)/L(mu[2], 1)), nosx = expression(mu[2]), ile = expression("n = 50"), l_x = 1.0, l_y = 0.75)

# c)
wykres(rcauchy, 50, 50, mi_2 = c(0, 0.5, 1, 1.5, 2, 2.5, 3), sigma_2 = rep(1, times = 7), tytul = expression(C(0, 1)/C(mu[2], 1)), nosx = expression(mu[2]), ile = expression("n = 50"), l_x = 2.0, l_y = 0.85, osx = seq(0, 3, by = 0.5))





#zad3


par(mfrow = c(2,3))
# a)
wykres(rnorm, 20, 20, mi_2 = rep(0, times = 7), sigma_2 = seq(1, 4, by = 0.5), osx = seq(from = 1, to = 4, by = 0.5), tytul = expression(N(0,1)/N(0, sigma[2])), nosx = expression(sigma[2]), ile = expression("n = 20"), l_x = 1.0, l_y = 1)

# b)
wykres(rlogis, 20, 20, mi_2 = rep(0, times = 7), sigma_2 = seq(1, 4, by = 0.5), osx = seq(from = 1, to = 4, by = 0.5), tytul = expression(L(0,1)/L(0, sigma[2])), nosx = expression(sigma[2]), ile = expression("n = 20"), l_x = 1.0, l_y = 1)

# c)
wykres(rcauchy, 20, 20, mi_2 = rep(0, times = 7), sigma_2 = 1:7, osx = 1:7, tytul = expression(C(0,1)/C(0, sigma[2])), nosx = expression(sigma[2]), ile = expression("n = 20"), l_x = 1.0, l_y = 1)

# a)
wykres(rnorm, 50, 50, mi_2 = rep(0, times = 7), sigma_2 = seq(1, 4, by = 0.5), osx = seq(from = 1, to = 4, by = 0.5), tytul = expression(N(0,1)/N(0, sigma[2])), nosx = expression(sigma[2]), ile = expression("n = 50"), l_x = 3.0, l_y = 0.8)

# b)
wykres(rlogis, 50, 50, mi_2 = rep(0, times = 7), sigma_2 = seq(1, 4, by = 0.5), osx = seq(from = 1, to = 4, by = 0.5), tytul = expression(L(0,1)/L(0, sigma[2])), nosx = expression(sigma[2]), ile = expression("n = 50"), l_x = 3.0, l_y = 0.8)

# c)
wykres(rcauchy, 50, 50, mi_2 = rep(0, times = 7), sigma_2 = 1:7, osx = 1:7, tytul = expression(C(0,1)/C(0, sigma[2])), nosx = expression(sigma[2]), ile = expression("n = 50"), l_x = 5.0, l_y = 0.8)

# zad 4


par(mfrow = c(2,3))
# a)
wykres_4(rnorm, 20, 20, mi_2 = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4), sigma_2 = seq(1, 4, by = 0.5), tytul = expression(N(0,1)/N(mu[2], sigma[2])), ile = expression("n = 20"))

# b)
wykres_4(rlogis, 20, 20, mi_2 =c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4), sigma_2 = seq(1, 4, by = 0.5), tytul = expression(L(0,1)/L(mu[2], sigma[2])), ile = expression("n = 20"))

# c)
wykres_4(rcauchy, 20, 20, mi_2 = seq(0, 3, by = 0.5), sigma_2 = 1:7, tytul = expression(C(0,1)/C(mu[2], sigma[2])), ile = expression("n = 20"))

# a)
wykres_4(rnorm, 50, 50, mi_2 = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4), sigma_2 = seq(1, 4, by = 0.5), tytul = expression(N(0,1)/N(mu[2], sigma[2])), ile = expression("n = 20"))

# b)
wykres_4(rlogis, 50, 50, mi_2 =c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4), sigma_2 = seq(1, 4, by = 0.5), tytul = expression(L(0,1)/L(mu[2], sigma[2])), ile = expression("n = 20"))

# c)
wykres_4(rcauchy, 50, 50, mi_2 = seq(0, 3, by = 0.5), sigma_2 = 1:7, tytul = expression(C(0,1)/C(mu[2], sigma[2])), ile = expression("n = 20"))

