library(raster)

draw = 1

files <- list.files('/Users/aucarter/Dropbox/DAVE_ANALYSIS/', full.names = T)
draw.files <- grep(paste0("\\.", draw, "\\."), files, value = T)
l <- lapply(draw.files, raster)


saveRDS(l, "/Users/aucarter/Data/UGA_MAP_1.rds")

l <- readRDS("/Users/aucarter/Data/UGA_MAP_1.rds")
M <- do.call(cbind, lapply(l, values))

PAR = list(A = 0.1, rho = 0, Q = 0.95, In = 5, Cn = 2, dt = 10)
Tx = rep(0, ncol(M))
    # c(rep(0.01, 250), seq(0.01, 0.315, length.out = (ncol(M) - 250)))


AR = PR2AR(X = M[10000,], Tx = Tx, PAR = PAR, Bfn = makeBdrugs_age, Xinterval = 30)
plot(1980 + (1:(ncol(M) - 1)) / 12 , AR, ty = 'l', ylim = c(0, 1), xlab = "Year", ylab = "Proportion")
lines(x = 1980 + (1:(ncol(M) - 1)) / 12 , M[10000,-1], col = "red")
lines(x = 1980 + (1:(ncol(M) - 1)) / 12 , Tx[-1], col = "gray")
legend("topright", legend = c("PfPR", "AR", "Tx"), fill = c("red", "black", "gray"))



#######################################

smoothK.i = function(i, x, t, alpha=1, beta=2, norm=1){
    if(length(norm)==1) norm = rep(norm, length(x))
    sum(x*exp(-alpha*((t-t[i])/beta)^2))/norm[i]
}

smoothK = function(x, t, alpha=3, beta=90){
    norm = sapply(1:length(t), smoothK.i, x=x*0+1, t=t, alpha=alpha, beta=beta, norm = 1)
    vals = sapply(1:length(t), smoothK.i, x=x, t=t, alpha=alpha, beta=beta, norm=norm)
    list(vals = vals, norm=norm)
}

smoothX = smoothK(X, 1:length(X), alpha = 120, beta = 20)$vals

plot(smoothX, type = "l")
lines(1:length(X), X, col = "red")


#######################################
library(ggplot2)
theme_set(theme_classic())

dt <- data.table(MAP = X, Year = seq(1980, 2018, length.out = length(X)))
dt[, Smoothed := smoothK(MAP, .I,  alpha = 2, beta = 2.5)$vals, by = .I]
dt <- melt(dt, id.vars = "Year", value.name = "PfPR", variable.name = "Source")

gg <- ggplot(dt[Year >= 1995], aes(x = Year, y = PfPR, color = Source)) +
        geom_vline(xintercept = 1995:2018, alpha = 0.1) +
        geom_line() +
        ggtitle(paste("Smoothed PfPR\nalpha =", 2, "; beta =", 2.5))

gg
#######################################




X = M[10000,1:13]
Tx = rep(0, 12)
# rep(0.05, ncol(M))
PAR$rho = Tx[1]

AR = PR2AR(X = M[10000,1:13], Tx = Tx, PAR = PAR, Bfn = makeBdrugs_age, Xinterval = 30)
plot(1:12 , AR, ty = 'l', ylim = c(0, 1))
lines(x = 1:13 , M[10000,1:13], col = "red")
lines(x = 1980 + (1:(ncol(M) - 1)) / 12 , Tx[-1], col = "gray")



PR2AReq(X = X[1], Tx = 0.0, PAR, makeBdrugs_age)$A




#######################################



xmax = ncol(M)
plot(((1:ncol(M)) / 12 + 1980)[1:xmax], M[9000,][1:xmax], ty = "l", ylim = c(0, 1),col=alpha(rgb(0,0,0), 0.05))

for(i in 1:1000) {
    idx <- sample(1:nrow(M), 1)
    if(all(!is.na(M[idx,]))) {
        lines(((1:ncol(M)) / 12 + 1980)[1:xmax], M[idx,][1:xmax],col=alpha(rgb(0,0,0), 0.05))
    }
}

change <- M[,2:ncol(M)] / M[, 1:(ncol(M) - 1)]

range(change, na.rm = T)

plot(density(M[,2:ncol(M)] / M[, 1:(ncol(M) - 1)], na.rm = T))
