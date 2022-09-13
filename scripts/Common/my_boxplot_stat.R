my_boxplot_stat <- function (x, quant = c(0.02,0.98), do.conf = TRUE, do.out = TRUE)
  {
    nna <- !is.na(x)
    n <- sum(nna)
    stats <- stats::fivenum(x, na.rm = TRUE)
    iqr <- diff(stats[c(2, 4)])
    stats[c(1, 5)] <- quantile(x,quant,na.rm=TRUE)  # PF
    if (any(quant<0 | quant>1))
      stop("'quant' must have two elements in the interval [0,1]")
    out <- x < stats[1] | x > stats[5]
    conf <- if (do.conf)
      stats[3] + c(-1.58, 1.58) * iqr/sqrt(n)
    list(stats = stats, n = n, conf = conf, out = x[out & nna])
  }
