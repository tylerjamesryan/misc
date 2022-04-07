
dat <- read.csv("data/emotional_intelligence.csv")

plot_reg <- function(x, y, b0, b1, show_resid = FALSE, ...) {
  ss <- round(sum((y - (b0 + b1*x))^2),2)
  plot(
    x = x,
    y = y,
    ...,
    main = paste0("Sum of Squared Residuals = ", ss)
  )
  abline(a = b0, b = b1)
  if (show_resid) {
    ypred <- b0 + b1*x
    segments(x, y, x, ypred)
  }
}


plot_reg(
  x = dat$emotional_intelligence, 
  y = dat$wisdom, 
  b0 = 1.24, 
  b1 = 0.02, 
  show_resid = TRUE)








