# source the crypto data

Sys.setenv(TZ = "UTC")
library(xts)

md <- new.env()
symbols <- c("LTCUSD", "BTCUSD", "ETHUSD", "BCHUSD")

# load the data
for(sym in symbols){
  fname <- paste0("data/", sym, ".RData")
  load(fname, envir = md)
}

# Calculate the basics... simple mid, weighted mid, and bid ask spread
for(sym in symbols){
  x <- md[[sym]]
  x$MP <- 0.5 * (x$Bid.Price + x$Ask.Price)
  x$WMP <- (x$Bid.Price * x$Ask.Size + x$Ask.Price * x$Bid.Size) / (x$Bid.Size + x$Ask.Size)
  x$BAS <- x$Ask.Price - x$Bid.Price
  md[[sym]] <- x
  rm(x)
}

# Define some functions that we will use
vwap <- function(x, price.col = "Price", volume.col = "Volume"){
  z <- na.omit(x[,c(price.col, volume.col)])
  colnames(z) <- c("price", "volume")
  as.numeric(sum(z$price * z$volume)/sum(z$volume))
}

trade.imbalance <- function(x){
  x$trade.on <- ifelse(x$Price <= x$Bid.Price, -1, 
                       ifelse(x$Price >= x$Ask.Price, 1, 0))
  sum(x[x$trade.on > 0, "Volume"], na.rm = TRUE) - sum(x[x$trade.on < 0, "Volume"], na.rm = TRUE)
}

volume.endpoints <- function (x, k = 1) {
  x <- try.xts(x)
  z <- coredata(na.fill(x$Volume, 0))
  sum.vol <- 0
  v.ep <- 0
  ne0 <- which(z != 0)
  for (i in ne0) {
    sum.vol <- sum.vol + z[i]
    if (sum.vol >= k) {
      v.ep <- c(v.ep, i)
      sum.vol <- 0
    }
  }
  if (last(v.ep) != nrow(x)) 
    v.ep <- c(v.ep, nrow(x))
  v.ep
}

# Create volume bars
k <- c("LTCUSD" = 500, "BTCUSD" = 15, "ETHUSD" = 150, "BCHUSD" = 40)
volume.bars <- new.env()
for(sym in symbols){
  x <- md[[sym]]
  ve <- volume.endpoints(x, k = k[sym])
  last.price <- period.apply(x, ve, function(x) xts::last(na.locf(x$Price)))
  volume <- period.apply(x, ve, function(x) sum(x$Volume, na.rm = TRUE))
  vwap.price <- period.apply(x, ve, vwap, price.col = "Price")
  ti <- period.apply(x, ve, trade.imbalance)
  x.vb <- cbind(last.price, volume, vwap.price, ti)
  colnames(x.vb) <- c("Last.Price", "Volume", "VWAP", "Trade.Imbalance")
  volume.bars[[sym]] <- x.vb
  rm(x, x.vb, ti)
}
