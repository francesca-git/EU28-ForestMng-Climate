library(stringr)
  
  lnorm_parametrized = function(data) {
    
    lower2.5 = range(data)[1]
    upper97.5 = range(data)[2]
    
    # lower2.5 = lower + (upper - lower)*0.025
    # upper97.5 = lower + (upper - lower)*0.975
    
    
    mug = sqrt(lower2.5*upper97.5)
    sigmag = (upper97.5/lower2.5)^(1/4)
    
    mulog = log(mug)
    sigmalog = log(sigmag)
    
    result <- list("meanlog" = mulog, "sdlog" = sigmalog, "mug" = mug, "sigmag" = sigmag, "CI 95%" = c(lower2.5, upper97.5))
    
    return(result)
    
  }

  
  lnorm_parametrized_68 = function(data) {
    
    lower68 = range(data)[1]
    upper68 = range(data)[2]
    
    # lower2.5 = lower + (upper - lower)*0.025
    # upper97.5 = lower + (upper - lower)*0.975
    
    
    mug = sqrt(lower68*upper68)
    sigmag = (upper68/lower68)^(1/2)
    
    mulog = log(mug)
    sigmalog = log(sigmag)
    
    result <- list("meanlog" = mulog, "sdlog" = sigmalog, "mug" = mug, "sigmag" = sigmag, "CI 68%" = c(lower68, upper68))
    
    return(result)
    
  }
  
  
  ##### LOG-NORMALITY TEST
  
  chi2test_lnorm = function(input_data, data_breaks, mulog, sigmalog) {
    ndata = length(input_data)
    measured <- hist(input_data, breaks = data_breaks, plot = F)$counts
    expected <- diff(plnorm(q = data_breaks, meanlog = mulog, sdlog = sigmalog))*ndata
    chi2_res <- sum((measured-expected)^2/expected)
    chi2_pvalue <- 1-pchisq(q = chi2_res, df = length(data_breaks)-2)
    results = list("chi2" = chi2_res, "pvalue" = chi2_pvalue)
    return(results)
  }
  
  
  lnorm_test = function(input_data, labels, QQ, hist_breaks) {
    
    if (length(input_data) <= 1) {
      
      plot.new()
      title(main = paste0(main = paste0(labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(input_data))))
      plot.new()
      title(main = "Q-Q plot of lognormal fit against data")
      
      results = list("Par of fit" = NA, "chi2 - pvalues (data)" = NA, "ks - pvalues (data)" = NA, 
                     "shapiro-wilk - pvalues" = NA, "Par of fit (range)" = NA, "CI 95% (range)" = NA)
      print(paste0("Not enought input data - ", labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(ratio)))
      
      return(results)
      
    } else {
      
      mulog = mean(log(input_data))
      sigmalog = sd(log(input_data))
      ndata = length(input_data)
      
      prob = seq(0, 1, length = 12)[2:11]
      
      ##### fit
      
      fit_params = fitdistr(input_data, "lognormal")
      fit <- dlnorm(seq(from = 0, to = 3, length.out = 300)[2:299], fit_params$estimate["meanlog"], fit_params$estimate["sdlog"])
      
      ##### test of parametrized fit using the range
      
      distr_range = lnorm_parametrized(input_data)
      fit_range_params = list("meanlog" = distr_range[[1]], "sdlog" = distr_range[[2]])
      fit_range <- dlnorm(seq(from = 0, to = 3, length.out = 300)[2:299], fit_range_params[[1]], fit_range_params[[2]])
      
      #### histogram
      
      
      if (missing(hist_breaks)) {
        hist_temp <- hist(input_data, plot = F)
      } else {
        hist_temp <- hist(input_data, breaks = hist_breaks, plot = F)
      }
      
      hist_ylim = c(0, max(max(fit), max(hist_temp$density), max(fit_range)))
      
      if (missing(hist_breaks)) {
        hist(input_data, freq = F, xlab = "Ratio Xe/Xc", ylim = hist_ylim, main = paste0(labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(input_data)))
      } else {
        hist(input_data, freq = F, xlab = "Ratio Xe/Xc", ylim = hist_ylim, main = paste0(labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(input_data)), breaks = hist_breaks)
      }
      
      
      if (min(hist_temp$breaks) == 0) {
        data_breaks = c(hist_temp$breaks[1:(length(hist_temp$breaks)-1)], hist_temp$breaks[length(hist_temp$breaks)-1]+1000)
      } else {
        data_breaks = c(0,hist_temp$breaks[2:(length(hist_temp$breaks)-1)], hist_temp$breaks[length(hist_temp$breaks)-1]+1000)
      }
      
      
      #### saphiro-wilk
      
      sw_res <- shapiro.test(log(input_data))
      sw_pvalue <- sw_res$p.value
      
      #### test of the goodness of fit - distribution from data
      
      #### chi2 
      
      chi2 = chi2test_lnorm(input_data, data_breaks, mulog, sigmalog)
      chi2_res = chi2[[1]]
      chi2_pvalue = chi2[[2]]
      #writeLines(sprintf("Significance of rejecting null hypothesis (distribution is lognormal) by chi2-test would be: %g %%.", 100*(1-chi2_pvalue)))
      
      #### ks
      
      ks_res <- ks.test(x = input_data, y = plnorm, meanlog = mulog, sdlog = sigmalog, exact = T)
      ks_pvalue <- ks_res$p.value
      
      #### test of the goodness of fit - distribution from range
      
      #### chi2 
      
      chi2_r = chi2test_lnorm(input_data, data_breaks,  fit_range_params[[1]],  fit_range_params[[2]])
      chi2_res_r = chi2_r[[1]]
      chi2_pvalue_r = chi2_r[[2]]
      #print(paste0("chi2 result (from range): ", round(chi2_res_r, digits = 4)))
      #print(100*(1-chi2_pvalue_r))
      #writeLines(sprintf("Significance of rejecting null hypothesis (distribution is lognormal) by chi2-test (from range) would be: %g %%.", 100*(1-chi2_pvalue_r)))
      
      #### ks
      
      #ks_res_r <- ks.test(x = input_data, y = plnorm, meanlog = fit_range_params[[1]], sdlog = fit_range_params[[2]], exact = T)
      #ks_pvalue_r <- ks_res_r$p.value
      
      #print(paste0("ks result (from range): ", round(ks_res_r$statistic, digits = 4)))
      #print(100*(1-ks_pvalue_r))
      #writeLines(sprintf("Significance of rejecting null hypothesis (distribution is lognormal) by ks-test (from range) would be: %g %%.", 100*(1-ks_pvalue_r)))
      
      
      #### Curve of the fit
      
      curve(dlnorm(x, meanlog = mean(log(input_data)), sdlog = sd(log(input_data))), col="red", lwd=2, add = T)
      
      #### Curve of the fit estimated from the range
      
      curve(dlnorm(x, meanlog = fit_range_params[[1]], sdlog = fit_range_params[[2]]), col="blue", lwd=1.5, add = T)
      
      legend("topright", legend = c(paste0("p.val (chi2 test): ", round(chi2_pvalue, digits = 4)),
                                    paste0("p.val (chi2 test): ", round(chi2_pvalue_r, digits = 4)),
                                    paste0("p.val (s-w test of lnorm): ", round(sw_pvalue, digits = 4))), 
             col = c("red","blue", "white"), lty=1, cex=1, box.lty=0)
      
      #### QQ plot
      
      if(QQ == TRUE) {
        
        quants_theor <- qlnorm(prob, fit_params$estimate["meanlog"], fit_params$estimate["sdlog"])
        quants_theor_r <- qlnorm(prob, fit_params[[1]], fit_params[[2]])
        quants_temp <- quantile(input_data, prob)
        quants <- unname(quants_temp)
        
        plot(quants_theor, quants, main = "Q-Q plot of lognormal fit against data", xlab = "Theoretical quantiles", ylab = "Sample data", 
             col = "red", ylim = c(min(min(quants_theor), min(quants), min(quants_theor_r)), max(max(quants_theor), max(quants), max(quants_theor_r))))
        points(quants_theor_r, quants, col = "blue")
        abline(0,1, col = "black")
        legend("bottomright", legend = c("from data", "parametrized"), col = c("red","blue"), lty=1, cex=1, box.lty=0)
        
      }
      
      
      results = list("Par of fit" = fit_params, "chi2 - pvalues (data)" = chi2_pvalue, "ks - pvalues (data)" = ks_pvalue, 
                     "shapiro-wilk - pvalues" = sw_pvalue, "Par of fit (range)" = fit_range_params, "CI 95% (range)" = distr_range[[5]], 
                     "chi2 - pvalues (range)" = chi2_pvalue_r)
      
      return(results)
      
    }
    
  } 
  
  
  lnorm_test_sep = function(input_data, labels, QQ, Plot, hist_breaks) {

      if (length(input_data) <= 2) {
        
        if (Plot == TRUE) {
          plot.new()
          title(main = paste0(main = paste0(labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(input_data))))
          
          if (QQ == TRUE) {
            plot.new()
            title(main = "Q-Q plot of lognormal fit against data")
          }
      } 
      
      results = list("Par of fit" = NA, "chi2 - pvalues" = NA, "shapiro-wilk - pvalues" = NA)
      print(paste0("Not enought input data - ", labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(ratio)))
      
      return(results)
      
    } else {
      
      mulog = mean(log(input_data))
      sigmalog = sd(log(input_data))
      ndata = length(input_data)
      
      prob = seq(0, 1, length = 12)[2:11]
      
      if (missing(hist_breaks)) {
        hist_temp <- hist(input_data, plot = F)
      } else {
        hist_temp <- hist(input_data, breaks = hist_breaks, plot = F)
      }
      
      ##### fit
      if (length(input_data) > 5) {
        
        fit_params = fitdistr(input_data, "lognormal")
        fit_params = list("meanlog" = unname(fit_params[[1]][[1]][1]), "sdlog" = unname(fit_params[[1]][[2]][1]))
        fit <- dlnorm(seq(from = 0, to = 3, length.out = 300)[2:299], fit_params[["meanlog"]], fit_params[["sdlog"]])
        
      } else {
        
        ##### test of parametrized fit using the range
        
        distr_range = lnorm_parametrized_68(input_data)
        fit_params = list("meanlog" = distr_range[[1]], "sdlog" = distr_range[[2]])
        fit <- dlnorm(seq(from = 0, to = 3, length.out = 300)[2:299],  fit_params[["meanlog"]], fit_params[["sdlog"]])
        
      }
      
      #### histogram
      hist_ylim = c(0, max(max(fit), max(hist_temp$density)))
      
      
      if (missing(hist_breaks)) {
        hist(input_data, freq = F, xlab = "Ratio Xe/Xc", ylim = hist_ylim, main = paste0(labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(input_data)), plot = Plot)
      } else {
        hist(input_data, freq = F, xlab = "Ratio Xe/Xc", ylim = hist_ylim, main = paste0(labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(input_data)), breaks = hist_breaks, plot = Plot)
      }
      
      if (min(hist_temp$breaks) == 0) {
        data_breaks = c(hist_temp$breaks[1:(length(hist_temp$breaks)-1)], hist_temp$breaks[length(hist_temp$breaks)-1]+1000)
      } else {
        data_breaks = c(0,hist_temp$breaks[2:(length(hist_temp$breaks)-1)], hist_temp$breaks[length(hist_temp$breaks)-1]+1000)
      }
      
      
      #### saphiro-wilk
      
      sw_res <- shapiro.test(log(input_data))
      sw_pvalue <- sw_res$p.value
      
      #### test of the goodness of fit 
      
      #### chi2 
      
      chi2 = chi2test_lnorm(input_data, data_breaks, fit_params[[1]],  fit_params[[2]])
      chi2_res = chi2[[1]]
      chi2_pvalue = chi2[[2]]
      
      #### Curve of the fit
      if (Plot == TRUE) {
      
        if (length(input_data)>5) {
          curve(dlnorm(x, meanlog = mean(log(input_data)), sdlog = sd(log(input_data))), col="red", lwd=2, add = T)
          legend("topright", legend = c(paste0("p.val (chi2 test): ", round(chi2_pvalue, digits = 4)),
                                        paste0("p.val (s-w test of lnorm): ", round(sw_pvalue, digits = 4))), 
                 col = c("red", "white"), lty=1, cex=1, box.lty=0)
          
        } else {
          #### Curve of the fit estimated from the range
          
          curve(dlnorm(x, meanlog = fit_params[[1]], sdlog = fit_params[[2]]), col="blue", lwd=2, add = T)
          legend("topright", legend = c(paste0("p.val (chi2 test): ", round(chi2_pvalue, digits = 4)),
                                        paste0("p.val (s-w test of lnorm): ", round(sw_pvalue, digits = 4))), 
                 col = c("blue", "white"), lty=1, cex=1, box.lty=0)
        }
        #### QQ plot
        
        if(QQ == TRUE) {
          
          quants_theor <- qlnorm(prob, fit_params[[1]], fit_params[[2]])
          quants_temp <- quantile(input_data, prob)
          quants <- unname(quants_temp)
          
          plot(quants_theor, quants, main = "Q-Q plot of lognormal fit against data", xlab = "Theoretical quantiles", ylab = "Sample data", 
               col = "red", ylim = c(min(min(quants_theor), min(quants)), max(max(quants_theor), max(quants))))
          abline(0,1, col = "black")
          
        }
        
      }
      
      results = list("Par of fit" = fit_params, "chi2 - pvalues (data)" = chi2_pvalue,
                     "shapiro-wilk - pvalues" = sw_pvalue)
      
      return(results)
      
    }
    
  } 
  
  
  ##### NORMALITY TEST
  
  norm_parametrized_68 = function(data) {
    
    lower68 = range(data)[1]
    upper68 = range(data)[2]
    
    # lower2.5 = lower + (upper - lower)*0.025
    # upper97.5 = lower + (upper - lower)*0.975
    
    
    mu = (lower68+upper68)/2
    sigma = (upper68-lower68)/2
    
    result <- list("mean" = mu, "sd" = sigma, "CI 68%" = c(lower68, upper68))
    
    return(result)
    
  }
  
  
  chi2test_norm = function(input_data, data_breaks, mu, sigma) {
    ndata = length(input_data)
    measured <- hist(input_data, breaks = data_breaks, plot = F)$counts
    expected <- diff(pnorm(q = data_breaks, mean = mu, sd = sigma))*ndata
    chi2_res <- sum((measured-expected)^2/expected)
    chi2_pvalue <- 1-pchisq(q = chi2_res, df = length(data_breaks)-2)
    results = list("chi2" = chi2_res, "pvalue" = chi2_pvalue)
    return(results)
  }
  
  
  norm_test_sep = function(input_data, labels, QQ, Plot, hist_breaks) {
    
    if (length(input_data) <= 2) {
      
      if (Plot == TRUE) {
      plot.new()
      title(main = paste0(main = paste0(labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(input_data))))
      
        if (QQ == TRUE) {
          plot.new()
          title(main = "Q-Q plot of lognormal fit against data")
        }
      }
      
      results = list("Par of fit" = NA, "chi2 - pvalues" = NA, "shapiro-wilk - pvalues" = NA)
      print(paste0("Not enought input data - ", labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(ratio)))
      
      return(results)
      
    } else {
      
      mu = mean(log(input_data))
      sigma = sd(log(input_data))
      ndata = length(input_data)
      
      prob = seq(0, 1, length = 12)[2:11]
      
      if (missing(hist_breaks)) {
        hist_temp <- hist(input_data, plot = F)
      } else {
        hist_temp <- hist(input_data, breaks = hist_breaks, plot = F)
      }
      
      ##### fit
      if (length(input_data) > 5) {
        
        fit_params = fitdistr(input_data, "normal")
        fit_params = list("mean" = unname(fit_params[[1]][[1]][1]), "sd" = unname(fit_params[[1]][[2]][1]))
        fit <- dnorm(seq(from = 0, to = 3, length.out = 300)[2:299], fit_params[["mean"]], fit_params[["sd"]])
        
      } else {
        
        ##### test of parametrized fit using the range
        
        distr_range = norm_parametrized_68(input_data)
        fit_params = list("mean" = distr_range[[1]], "sd" = distr_range[[2]])
        fit <- dnorm(seq(from = 0, to = 3, length.out = 300)[2:299],  fit_params[["mean"]], fit_params[["sd"]])
        
      }
      
      #### histogram
      hist_ylim = c(0, max(max(fit), max(hist_temp$density)))
      
      
      if (missing(hist_breaks)) {
        hist(input_data, freq = F, xlab = "Ratio Xe/Xc", ylim = hist_ylim, main = paste0(labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(input_data)), plot = Plot)
      } else {
        hist(input_data, freq = F, xlab = "Ratio Xe/Xc", ylim = hist_ylim, main = paste0(labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(input_data)), breaks = hist_breaks, plot = Plot)
      }
      
      if (min(hist_temp$breaks) == 0) {
        data_breaks = c(hist_temp$breaks[1:(length(hist_temp$breaks)-1)], hist_temp$breaks[length(hist_temp$breaks)-1]+1000)
      } else {
        data_breaks = c(0,hist_temp$breaks[2:(length(hist_temp$breaks)-1)], hist_temp$breaks[length(hist_temp$breaks)-1]+1000)
      }
      
      
      #### saphiro-wilk
      
      sw_res <- shapiro.test(input_data)
      sw_pvalue <- sw_res$p.value
      
      #### test of the goodness of fit 
      
      #### chi2 
      
      chi2 = chi2test_norm(input_data, data_breaks, fit_params[[1]],  fit_params[[2]])
      chi2_res = chi2[[1]]
      chi2_pvalue = chi2[[2]]
      
      #### Curve of the fit
      if (Plot == TRUE) {
        if (length(input_data)>5) {
          curve(dnorm(x, mean = fit_params[[1]], sd = fit_params[[2]]), col="green4", lwd=2, add = T)
          legend("topright", legend = c(paste0("p.val (chi2 test): ", round(chi2_pvalue, digits = 4)),
                                        paste0("p.val (s-w test of lnorm): ", round(sw_pvalue, digits = 4))), 
                                        col = c("green4", "white"), lty=1, cex=1, box.lty=0)
          
        } else {
          #### Curve of the fit estimated from the range
          
          curve(dnorm(x, mean = fit_params[[1]], sd = fit_params[[2]]), col="blue", lwd=2, add = T)
          legend("topright", legend = c(paste0("p.val (chi2 test): ", round(chi2_pvalue, digits = 4)),
                                        paste0("p.val (s-w test of lnorm): ", round(sw_pvalue, digits = 4))), 
                                        col = c("blue", "white"), lty=1, cex=1, box.lty=0)
        }
        #### QQ plot
        
        if(QQ == TRUE) {
          
          qqnorm(y=input_data,datax=T, distribution = qnorm(prob, mean = fit_params[[1]], sd = fit_params[[2]])*ndata, main = "Q-Q plot of normal fit against data", xlab = "Theoretical quantiles", ylab = "Sample data")
          abline(0,1, col = "black")
          
        }
        
      }
      results = list("Par of fit" = fit_params, "chi2 - pvalues (data)" = chi2_pvalue,
                     "shapiro-wilk - pvalues" = sw_pvalue)
      
      return(results)
      
    }
    
  } 
  
  
  
  fit_ratio = function(input_data, data_index, BM, LU, TX, fit_shape) {
    
    groups <- vector(mode = "list", length = length(input_data))
    
    for (i in (1:length(input_data))) {
      
      groups[[i]] = 1 - input_data[[i]]  
      
    }
    
    for (i in (1:length(groups))) {
      
      data = groups[[i]]
      
      #if(length(ratio) > 5) {
      
      #data = data[data != 0]
      labels <- vector(mode = "list", length = 3)
      labels[[1]] = BM[data_index[[i]][1]]
      print(paste0("BM: ", labels[[1]]))
      labels[[2]] = LU[data_index[[i]][3]]
      print(paste0("LU: ", labels[[2]]))
      labels[[3]] = TX[data_index[[i]][2]]
      print(paste0("TX: ", labels[[3]]))
      
      if (length(groups[[i]]) <= 2) {
        
        print(paste0("Not enought input data - ", labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(ratio)))
        
      } else {
        
        if (fit_shape == "log-norm") {
          
          fit = lnorm_test_sep(input_data, labels, QQ = FALSE)
          
        } else if (fit_shape == "norm") {
          
          fit = norm_test_sep(input_data, labels, QQ = FALSE)  
          
        }
        
      }
      #   }
      #   
      #   else {print(paste0("Not enought input data - ", labels[[1]], " - ", labels[[2]], " - ", labels[[3]], ". Data points: ", length(ratio)))}
      #   
    }
    
    return(fit)
    
  }
  
  
  
  
  gen_lnorm_distr = function(input_data, BM, LU, TX, n) {
    # 
    BM = biomes
    TX = taxa
    LU = land_use_type
    n = 1000
    input_data = CFloc_group_blu

    
    groups <- vector(mode = "list", length = length(input_data))
    names(groups) <- names(input_data)
    
    for (i in (1:length(input_data))) {
      
      groups[[i]] = 1 - input_data[[i]]  
      groups[[i]][groups[[i]] == 0] <- 0.4*10^(-8)
      
    }
    
    nLU = length(LU)
    nTX = length(TX)
    nBM = length(BM)

    mean <- (unname(unlist(lapply(lapply(groups, log), mean))))
    sd <- (unname(unlist(lapply(lapply(groups, log), sd))))
    
    distr = data.frame(matrix(NA, nrow=nBM*nLU*nTX, ncol=5+n))
      colnames(distr)[1:5] <- c("Biome_ID","Land_use_type","Taxa_used", "meanlog", "sdlog")

    mean <- unname(unlist(lapply(lapply(groups, log), mean)))
    sd <- unname(unlist(lapply(lapply(groups, log), sd)))
    cases <- names(groups)

    # Split dates using " - "
    split_names <- sapply(cases, function(x) str_split(x, pattern = fixed("_")))

    distr[,"meanlog"] <- mean
    distr[,"sdlog"] <- sd

    for (i in (1:length(groups))) {

        distr$Biome_ID[i] = split_names[[i]][3]
        distr$Land_use_type[i] = split_names[[i]][1]
        distr$Taxa_used[i] = split_names[[i]][2]
        #print(paste0("i: ", i))
        temp <- rlnorm(n, distr$meanlog[i], distr$sdlog[i])
        distr[i, 6:length(distr)] = temp

      }


    return(distr)
    
  }
  
  
 