my_boxplot <-
  function (x, ..., quant = c(0.02,0.98), width = NULL, varwidth = FALSE, 
            notch = FALSE, outline = TRUE, names, plot = TRUE, border = par("fg"), 
            col = NULL, log = "", pars = list(boxwex = 0.8, staplewex = 0.5, 
                                              outwex = 0.5), horizontal = FALSE, add = FALSE, at = NULL) 
  {
    args <- list(x, ...)
    namedargs <- if (!is.null(attributes(args)$names)) 
      attributes(args)$names != ""
    else rep(FALSE, length.out = length(args))
    pars <- c(args[namedargs], pars)
    groups <- if (is.list(x)) 
      x
    else args[!namedargs]
    if (0 == (n <- length(groups))) 
      stop("invalid first argument")
    if (length(class(groups))) 
      groups <- unclass(groups)
    if (!missing(names)) 
      attr(groups, "names") <- names
    else {
      if (is.null(attr(groups, "names"))) 
        attr(groups, "names") <- 1:n
      names <- attr(groups, "names")
    }
    for (i in 1:n) groups[i] <- list(my_boxplot_stat(groups[[i]], 
                                                       quant))
    stats <- matrix(0, nrow = 5, ncol = n)
    conf <- matrix(0, nrow = 2, ncol = n)
    ng <- out <- group <- numeric(0)
    ct <- 1
    for (i in groups) {
      stats[, ct] <- i$stats
      conf[, ct] <- i$conf
      ng <- c(ng, i$n)
      if ((lo <- length(i$out))) {
        out <- c(out, i$out)
        group <- c(group, rep.int(ct, lo))
      }
      ct <- ct + 1
    }
    z <- list(stats = stats, n = ng, conf = conf, out = out, 
              group = group, names = names)
    if (plot) {
      bxp(z, width, varwidth = varwidth, notch = notch, log = log, 
          border = border, boxfill = col, pars = pars, outline = outline, 
          horizontal = horizontal, add = add, at = at)
      invisible(z)
    }
    else z
  }