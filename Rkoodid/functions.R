require(dplyr)

aidsEstgroup <- function (priceNames, 
                          shareNames, totExpName, 
                          group = NULL, #gruppeeriv tunnus
                          time = NULL, #aasta tunnus
                          data, 
                          method = "LA", priceIndex = "Ls", 
                          pxBase = 1, hom = TRUE, sym = TRUE, shifterNames = NULL, 
                          instNames = NULL, estMethod = ifelse(is.null(instNames), 
                                                               "SUR", "3SLS"), ILmaxiter = 50, ILtol = 1e-05, alpha0 = 0, 
                          restrict.regMat = FALSE, ...) 
{
  if (length(priceNames) != length(shareNames)) {
    stop("arguments 'priceNames' and 'shareNames' must have the same length")
  }
  nGoods <- length(priceNames)
  #nShifter  an optional vector of strings containing the names of the demand shifters
  #Nt kui on mikroandmed, siis võiks siia panna mingid muutujad.
  nShifter <- length(shifterNames)
  #see võtab meetodi. aga miks alates neljandast tähest kuni lõpuni? kas mingi vana asi?
  #see võtaks nagu hinna arvutamise meetodi. kas varem olid hindamismeetod ja hinnainfo koos meetodi all?
  #!!paistab, et vanasti kirjutati meetod nii: "LA:SL" või "IL:Ls" jne.
  extractPx <- function(method) {
    px <- substr(method, 4, nchar(method))
    return(px)
  }
  #Mis on MK meetod? Kirjutab tulemustes ikkagi "Estimation Method: 'Iterated Linear Least Squares Estimator' (IL)"
  #Jääb mulje, et MK on lihtsalt mingi vana IL, sest kogu aeg käivad koos sellega.
  
  #Kui ei kirjuta meetodi alla ühe nendest kolmest, siis ta eeldab, et panid meetodi alla hinnaindeksi info?
  if (!method %in% c("LA", "IL", "MK")) {
    #jääb mulje, et siin on ka mingid vanad jäägid, et kasutab nelja esimest tähte?
    #kontrollib, kas meetod on pikalt kirjutatud ja kas esimsed kaks tähte on ikkagi õiged.
    #!!paistab, et vanasti kirjutati meetod nii: "LA:SL" või "IL:Ls" jne.
    if (nchar(method) >= 4 && substr(method, 3, 3) == ":" && 
        substr(method, 1, 2) %in% c("LA", "IL", "MK")) {
      priceIndex <- extractPx(method)
      warning("using price index specified in argument 'method',", 
              " ignoring argument 'priceIndex'")
      method <- substr(method, 1, 2)
    }
    else {
      stop("argument 'method' must be either", " 'LA' (for 'Linear Approximation') or", 
           " 'IL' (for 'Iterated Linear Least Squares')")
    }
  }
  if (!(priceIndex %in% c("S", "SL", "P", "L", "Ls", "T"))) {
    stop("argument 'priceIndex' that specifies the price index must be either", 
         " 'S' (Stone index), 'SL' (Stone index with lagges shares),", 
         " 'P' (Paasche index), 'L' (Laspeyres index),", " 'Ls' (Laspeyres index, simplified), or", 
         " 'T' (Tornqvist index)")
  }
  if (sym && !hom) {
    hom <- TRUE
    warning("symmetry implies homogeneity: imposing additionally homogeniety")
  }
  #puuduvad väärtused lähevad välja
  allVarNames <- c(priceNames, shareNames, totExpName, instNames, 
                   shifterNames)
  if (sum(is.na(data[, allVarNames])) > 0) {
    warning("there are some NAs in the data,", " all observations (rows) with NAs are excluded from the analysis")
    data <- data[!is.na(rowSums(data[, allVarNames])), ]
  }
  nObs <- nrow(data)
  
  #Siin hakkab viitaeg sisse tulema! Seda pead käsitsi muutma
  #jätab esimese vaatluse välja, kui on viitajaga Stone
  if (is.null(group)) {
    if (priceIndex == "SL") {
      #kui ei ole gruppeerivat tunnust on viitajaga stone'i indeks, siis jäta vaid esimene vaatlus välja
      sample <- c(2:nObs)
    } else {
      sample <- c(1:nObs)
    }
  } else {  #kui aga on gruppeeriv tunnus, siis jäta välja iga grupi esimene vaatlus
    if (priceIndex == "SL") {
      require(dplyr)
      data$group = data[[group]]
      data$time = data[[time]]
      temp = data %>% arrange(group, year) %>% 
        mutate(rownumber = row_number()) %>% 
        group_by(group) %>% 
        filter(row_number()>1) %>% 
        ungroup() %>% 
        select(rownumber)
      sample= temp$rownumber
      rm(temp)
      
    } else {
      sample <- c(1:nObs)
    }    
  }
  
    
  # sample <- if (priceIndex == "SL") 
  #   c(2:nObs)
  # else c(1:nObs)
  
  result <- list()
  result$call <- match.call()
  wMeans <- numeric(nGoods)
  pMeans <- numeric(nGoods)
  #kulutuste keskmine
  #selle peaksid tegema siis igale kvintiilile eraldi!
  
  if (is.null(group)) {
    #kui pole gruppi, lihtne keskmine - see on aga vaid üks arv!
    xtMean <- mean(data[[totExpName]][sample])
    } else {
    #kui on grupp, siis iga grupi keskmine
      data$totExpName = data[[totExpName]]
      temp = data[sample,] %>% 
        group_by(group) %>% 
        summarise(xtMean = mean(totExpName))
      xtMean = temp$xtMean # see on aga iga grupi jaoks arv!
    }
  
  #xtMean <- mean(data[[totExpName]][sample])
  #Osakaalude keskmine, selle peaks ka tegema igale kvintiilile eraldi. 
  #Aga kuidas?
  for (i in seq(nGoods)) {
      if (is.null(group)) {
      wMeans[i] <- mean(data[[shareNames[i]]][sample])
      pMeans[i] <- mean(data[[priceNames[i]]][sample])
      } else {
      #praegu tegemata
        wMeans[i] <- mean(data[[shareNames[i]]][sample])
        pMeans[i] <- mean(data[[priceNames[i]]][sample])  
      }
  }
    # for (i in seq(nGoods)) {
    #   wMeans[i] <- mean(data[[shareNames[i]]][sample])
    #   pMeans[i] <- mean(data[[priceNames[i]]][sample])
    # }
    #See on eraldi funktsioon, mis näib tegevat hinnaindeksi võttes sisendiks indeksi tüübi, hinnad ja osakaalud
  lnp <- aidsPx(priceIndex, priceNames, shareNames = shareNames, 
                data = data, base = pxBase) #mis on pxBase?
  
  #siin tehake valmis kogukulutused ja log reaalsed kogukulutused	
  sysData <- data.frame(xt = data[[totExpName]], lxtr = (log(data[[totExpName]]) - 
                                                           lnp))
  #lisatakse osakaalud ja log hinnad
  for (i in 1:nGoods) {
    #kas nii saabki lisada andmetabelile juurde veerge lihtsalt?
    #osakaalud
    sysData[[paste("w", i, sep = "")]] <- data[[shareNames[i]]]
    #log hinnad
    sysData[[paste("lp", i, sep = "")]] <- log(data[[priceNames[i]]])
  }
  if (is.null(instNames)) {
    ivFormula <- NULL
  }
  #nii kui on olemas instrumendid, siis kasutab vaikimisi 3SLSi. (Peabki kasutama, sest üks kitsendus sees)
  else {
    estMethod <- "3SLS"
    ivFormula <- "~"
    #lisame instrumendid
    for (i in 1:length(instNames)) {
      sysData[[paste("i", i, sep = "")]] <- data[[instNames[i]]]
      #paneme valemile järjest otsa instrumendid
      ivFormula <- paste(ivFormula, " + i", as.character(i), 
                         sep = "")
    }
    ivFormula <- as.formula(ivFormula)
  }
  if (is.null(shifterNames)) {
    shifterFormula <- NULL
  }
  else {
    for (i in 1:length(shifterNames)) {
      #paneme juurde shifterite nimed andmetesse
      sysData[[paste("s", i, sep = "")]] <- data[[shifterNames[i]]]
    }
  }
  #kitsenduste loetelu! Uuri seda funktsiooni
  restr <- .aidsRestr(nGoods = nGoods, hom = hom, sym = sym, 
                      restrict.regMat = restrict.regMat, nShifter = nShifter)
  #võrrandite loetelu. uuri seda funktsiooni
  system <- .aidsSystem(nGoods = nGoods, nShifter = nShifter)
  #erinevus järgmises sõltub sellest, kuidas on kitsenduste maatriks ette antud
  #siin on ette antud restrict.regMat
  if (restrict.regMat) {
    #tavaline süsteemi hindamine. seda peaks siis kohandama paneelandmete jaoks.
    est <- systemfit(system, estMethod, data = sysData, restrict.regMat = restr, 
                     inst = ivFormula, ...)
  }
  #siin on ette antud restrict.matrix
  else {
    est <- systemfit(system, estMethod, data = sysData, restrict.matrix = restr, 
                     inst = ivFormula, ...)
  }
  #Nüüd hakkab iga meetodi puhul erinev asi pihta, kuidas koefitsiente võetakse?
  #kui on tegemist linear approximation-ga, siis lõpetab siin.
  if (method == "LA") { 
    # The 'Linear Approximate AIDS' (LA)
    #vaata seda aidsCoef funktsiooni
    result$coef <- .aidsCoef(coef(est), nGoods = nGoods, 
                             nShifter = nShifter, cov = vcov(est), priceNames = priceNames, 
                             shareNames = shareNames, shifterNames = shifterNames, 
                             df = df.residual(est))
    #see peaks olema siis hinnatud osakaalud
    result$wFitted <- aidsCalc(priceNames, totExpName, data = data, 
                               coef = result$coef, priceIndex = lnp)$shares
    iter <- est$iter
  }
  #kui meetod on midagi muud? 
  #MK - 
  #IL - 'Iterative Linear Least Squares Estimator' (IL) 
  #s´iis ta ikkagi hindab eelmises sammus LA ära ja võtab selle koefitsiendid aluseks.
  else if (method %in% c("MK", "IL")) {
    b <- coef(est)
    bd <- b
    iter <- est$iter
    ILiter <- 1
    while (((t(bd) %*% bd)/(t(b) %*% b))^0.5 > ILtol && ILiter < 
           ILmaxiter) {
      ILiter <- ILiter + 1
      bl <- b
      #uus reaalsete kulutuste väärtus
      #Mis on TL? Mingi hinnaindeks. Järsku translog, sest on ju IL meetod!
      sysData$lxtr <- log(data[[totExpName]]) - aidsPx("TL", 
                                                       priceNames, shareNames = shareNames, data = data, 
                                                       coef = .aidsCoef(coef(est), nGoods = nGoods, 
                                                                        nShifter = nShifter, alpha0 = alpha0))
      if (restrict.regMat) {
        est <- systemfit(system, estMethod, data = sysData, 
                         restrict.regMat = restr, inst = ivFormula, 
                         ...)
      }
      else {
        est <- systemfit(system, estMethod, data = sysData, 
                         restrict.matrix = restr, inst = ivFormula, 
                         ...)
      }
      iter <- c(iter, est$iter)
      weightNewCoef <- 1
      b <- weightNewCoef * coef(est) + (1 - weightNewCoef) * 
        b
      bd <- b - bl
    }
    #see vist on viimase tsükli järel
    sysData$lxtr <- log(data[[totExpName]]) - aidsPx("TL", 
                                                     priceNames, data = data, coef = .aidsCoef(coef(est), 
                                                                                               nGoods = nGoods, nShifter = nShifter, alpha0 = alpha0))
    Gmat <- cbind(rep(1, nObs), sysData$lxtr)
    for (i in 1:(nGoods)) {
      Gmat <- cbind(Gmat, sysData[[paste("lp", i, sep = "")]])
    }
    if (nShifter > 0) {
      for (i in 1:nShifter) {
        Gmat <- cbind(Gmat, sysData[[paste("s", i, sep = "")]])
      }
    }
    if (FALSE) {
      for (i in 1:(nGoods - 1)) {
        print(fitted(est$eq[[i]]) - Gmat %*% coef(est)[((i - 
                                                           1) * (nGoods + 2) + 1):(i * (nGoods + 2))])
      }
    }
    jacobian <- .aidsJacobian(coef(est), priceNames, totExpName, 
                              data = data, shifterNames = shifterNames, alpha0 = alpha0)
    if (hom) {
      modRegMat <- .aidsRestr(nGoods = nGoods, nShifter = nShifter, 
                              hom = hom, sym = sym, restrict.regMat = TRUE)
    }
    else {
      modRegMat <- diag((nGoods - 1) * (nGoods + 2 + nShifter))
    }
    Jmat <- crossprod(modRegMat, (diag(nGoods - 1) %x% t(Gmat))) %*% 
      jacobian
    JmatInv <- modRegMat %*% solve(Jmat, t(modRegMat))
    bcov <- JmatInv %*% (est$residCov %x% crossprod(Gmat)) %*% 
      t(JmatInv)
    result$coef <- .aidsCoef(coef(est), nGoods = nGoods, 
                             nShifter = nShifter, cov = bcov, priceNames = priceNames, 
                             shareNames = shareNames, shifterNames = shifterNames, 
                             df = df.residual(est))
    result$coef$alpha0 <- alpha0
    result$wFitted <- aidsCalc(priceNames, totExpName, data = data, 
                               coef = result$coef, priceIndex = "TL")$shares
    result$ILiter <- ILiter
  }
  names(result$wFitted) <- paste("wFitted", as.character(1:nGoods), 
                                 sep = "")
  result$wResid <- data.frame(matrix(NA, nrow = nObs, ncol = nGoods))
  names(result$wResid) <- paste("wResid", as.character(1:nGoods), 
                                sep = "")
  result$qObs <- data.frame(matrix(NA, nrow = nObs, ncol = nGoods))
  names(result$qObs) <- paste("qObs", as.character(1:nGoods), 
                              sep = "")
  result$qFitted <- data.frame(matrix(NA, nrow = nObs, ncol = nGoods))
  names(result$qFitted) <- paste("qFitted", as.character(1:nGoods), 
                                 sep = "")
  result$qResid <- data.frame(matrix(NA, nrow = nObs, ncol = nGoods))
  names(result$qResid) <- paste("qResid", as.character(1:nGoods), 
                                sep = "")
  for (i in 1:nGoods) {
    result$wResid[, i] <- data[[shareNames[i]]] - result$wFitted[, 
                                                                 i]
    result$qObs[, i] <- data[[shareNames[i]]] * data[[totExpName]]/data[[priceNames[i]]]
    result$qFitted[, i] <- result$wFitted[i] * data[[totExpName]]/data[[priceNames[i]]]
    result$qResid[, i] <- result$qObs[, i] - result$qFitted[, 
                                                            i]
  }
  result$r2 <- numeric(nGoods)
  for (i in 1:(nGoods - 1)) {
    result$r2[i] <- summary(est$eq[[i]])$r.squared
  }
  result$r2[nGoods] <- rSquared(data[sample, shareNames[nGoods]], 
                                result$wResid[sample, nGoods])
  names(result$r2) <- shareNames
  result$r2q <- numeric(nGoods)
  for (i in 1:nGoods) {
    result$r2q[i] <- rSquared(result$qObs[sample, i], result$qResid[sample, 
                                                                    i])
  }
  names(result$r2q) <- paste("q_", shareNames, sep = "")
  result$iter <- iter
  result$est <- est
  result$method <- method
  result$priceIndex <- priceIndex
  result$lnp <- lnp
  result$wMeans <- wMeans
  result$pMeans <- pMeans
  result$xtMean <- xtMean
  result$shareNames <- shareNames
  result$priceNames <- priceNames
  result$totExpName <- totExpName
  result$basePrices <- attributes(result$lnp)$basePrices
  result$baseShares <- attributes(result$lnp)$baseShares
  class(result) <- "aidsEst"
  return(result)
}