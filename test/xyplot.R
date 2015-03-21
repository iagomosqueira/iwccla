temp <- scan("XY", 
  what = list(character(), double(), double(), double(), double(), double()))

x <- sapply(temp[2:6], "[", which(temp[[1]] == "X"))
y <- sapply(temp[2:6], "[", which(temp[[1]] == "Y"))

low <- rbind(data.frame(variable = "X", x), data.frame(variable = "Y", y))
colnames(low) <- c("variable", "try.1", "try.2", "x.1", "x.2", "ss")


temp <- scan("XY_0402.txt", 
  what = list(character(), double(), double(), double(), double(), double()))

x <- sapply(temp[2:6], "[", which(temp[[1]] == "X"))
y <- sapply(temp[2:6], "[", which(temp[[1]] == "Y"))

high <- rbind(data.frame(variable = "X", x), data.frame(variable = "Y", y))
colnames(high) <- c("variable", "try.1", "try.2", "x.1", "x.2", "ss")

par(oma = c(7,4,3,3), mfcol = c(2,2), mar = c(0,0,0,0), xpd = TRUE)
rbPal <- colorRampPalette(c("red", "blue"), alpha = TRUE)
cuts <- cut(c(high$ss, low$ss), breaks = 20)
cuts1 <- cuts[1:length(high$ss)]
cuts2 <- cuts[(length(high$ss) + 1):length(cuts)]
ylim <- range(cbind(high[,c("x.1","x.2")],low[,c("x.1","x.2")]))
with(high, 
  plot(try.1, x.1, las = 1, xaxt = "n", ylab = "", ylim = ylim,
  col = rbPal(20)[as.numeric(cuts1)]))
  mtext(side = 2, "Parameter estimates", outer = TRUE, line = 1.5)
with(low, 
  plot(try.1, x.1, las = 1, ylab = "", ylim = ylim,
  col = rbPal(20)[as.numeric(cuts2)]))
  mtext(side = 1, "initial guess for parameter 1", outer = FALSE, line = 3)


with(high, 
  plot(try.2, x.2, xaxt = "n", yaxt = "n", ylim = ylim,
  col = rbPal(20)[as.numeric(cuts1)]))
  mtext(side = 4, "MSYR = 0.2", line = 1)
with(low, 
  plot(try.2, x.2, las = 1, ylab = "", ylim = ylim, yaxt = "n",
  col = rbPal(20)[as.numeric(cuts2)]))
  mtext(side = 1, "initial guess for parameter 2", outer = FALSE, line = 3)
  mtext(side = 4, "MSYR = 0.1", line = 1)
legend("topleft", legend = levels(cuts), col = rbPal(20), pch = 1, bty = "n", ncol = 2)


head(low)
aggregate(x.1~variable, data = low, range)
table(low$try.1)
unique((low$ss[low$variable == "Y"])
log(low[low$variable == "Y",][which(low$ss[low$variable == "Y"] < 0.19),]$x.2)
