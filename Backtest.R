# ---------------------------------------------------------------------------- #
#  Backtest Trabalho de Conclusão de Curso
#  Curso: Ciências Econômicoas
#  Autor: Eduardo Mayer de Figueiredo
#  Semestre: 2022/02
# ---------------------------------------------------------------------------- #

# Pacotes utilizados

  library(readxl)
  library(PerformanceAnalytics)
  library(tidyverse)
  library(xts)
  library(lubridate)
  library(pbapply)
  library(quantmod)
  library(tinytex)
  library(lmtest)
  library(stats)
  library(estimatr)


# ******************************************************************************
# Importação das Bases de dados Economática 
# ******************************************************************************

  load("DB.RData")
  
# Formatação XTS
  xts <- lapply(Data_Base[c("Price",  "Benchmarks", "Liquidez", "PriceToBook", "PriceToEarnings", "EarningsYeald", "MktCap")], 
                function(x) xts(x[-1], order.by = as.Date(as.matrix(x[1]))))
 
# Bases calculadas  
  xts[[8]] <- Return.calculate(xts$Price)[-1]                                                   # Calculo de Retornos Mensais
  xts[[9]] <- apply.monthly(replace(xts$Liquidez, is.na(xts$Liquidez), 0), mean, na.rm = FALSE) # Calculo da média mensal do volume($) diário
  index(xts[[9]]) <- index(xts[[8]]) 
  xts[[9]][xts[[9]]==0] <- NA                                                                   # Retorna valores nulos para NA
  
  names(xts) <- c("Price",  "Benchmarks", "Liquidez", "PriceToBook", "PriceToEarnings", "EarningsYeald", "MktCap", "Returns", "Volume Médio")
  
  # Cálculo Momentum

  load("ROC.RData")
  
# ******************************************************************************  
# Funções Utilizadas
# ******************************************************************************  
  source("tabela.estatisticas.R")
  
  riskpremium <- function(x, rf) {x - rf}

  FilterQuantile <- function(Dados,nq, nquantil){
    
    # Dados = xts/data frame com os dados base para o flitro
    # n = quantil de corte. Seleção considera quantil iguais ou acima do valor indicado
    # nquantil = número de quantis a serem considerados
    
    #REMOVER - lag.Dados <- lag.xts(Dados, k = 1, na.pad = FALSE)
    x <- t(apply(Dados, 1, function(x) ntile(as.vector(x), nquantil)))
    colnames(x) <- names(Dados)
    x[x<nq] <- NA
    x[!is.na(x)] <- 1
    return(as.matrix(x))  
  }
  
  ValueQuantile <- function(Dados, nquantil){ # Retorna uma lista de matrizes para cada quantil considerado
    
    # Dados = xts/data frame com os dados base para o flitro
    # nquantil = número de quantis a serem considerados
    
    Dados <- Dados**-1  # inversão dos múltiplos para a análise de quantil
    list <- NULL
    for( q in 1:nquantil){  
      x <- t(apply(Dados, 1, function(z) ntile(as.vector(z), nquantil)))
      colnames(x) <- names(Dados)
      x[x!=q] <- NA
      x[x==q] <- 1
      list[[q]] <- x
    }
    return(list)
  }
  
  MomentumQuantil <- function(Dados, QualityIndex = NULL, nquantil){
    
    # Dados = xts/data frame com os dados base para o flitro
    # nquantil = número de quantis a serem considerados
    if(is.null(QualityIndex)){# Momentum Genérico
    list <- NULL
      for( i in 1:nquantil){  
        x <- t(apply(Dados, 1, function(x) ntile(as.vector(x), nquantil)))
        colnames(x) <- names(Dados)
        x[x!=i] <- NA
        x[x==i] <- 1
        list[[i]] <- x
      }
    }else{ # Momentum Quality
      list <- NULL
      for( i in 1:nquantil){  
        x <- t(apply(Dados, 1, function(x) ntile(as.vector(x), nquantil)))
        colnames(x) <- names(Dados)
        x[x!=i] <- NA
        x[x==i] <- 1
        
        QualityIndex <- QualityIndex[,names(Dados)]
        y <- as.matrix(QualityIndex)*x
        y <- t(apply(y, 1, function(x) ntile(as.vector(x), 3)))
        colnames(y) <- names(Dados)
        y[y!=1] <- NA
        y[y==1] <- 1
        list[[i]] <- y
        
      }
    }
    return(list)
  }
  
  BacktestMomentum <- function(returns, Liquidity = NULL, MktCap = NULL, Factor,  Vol = NULL,  holding_periods){
    # OBS: base mensal
    # n = holding periods 
    # returns = df com o retorno mensal de todo universo de ativos
    # Liquidity = xts de Volume médio diário 
    # MktCap = xts de Market Cap 
    # Factor = xts do múltiplo relativo ao Fator
    
    #ql <- 6
    #qmk <- 4
    
    # 1 Liquidez
    # Filtro Volume Negociado
    if(!is.null(Liquidity)){
      filter <- FilterQuantile(Liquidity, ql, 10) # matriz para filtro relativo de volume negociado
      col.order <- colnames(filter)
    }  
    
    # Filtro Market Cap
    if(!is.null(MktCap)){     # Se informado dados para filtro de Market Cap
      if(is.null(Liquidity)){ # Filtro apenas de Market Cap
        filter <- FilterQuantile(MktCap, qmk, 10) 
        col.order <- colnames(filter)
      }else{                  # Filtro de Market Cap após filtro de negociabilidade
        x <- as.matrix(MktCap[, col.order])*filter
        filter <- FilterQuantile(x, qmk, 10)
      }
    }
    
    # Rank Factor
    if(is.null(Liquidity) & is.null(MktCap)){
      rank.list <- MomentumQuantil(Factor, 5, QualityIndex = Vol) # Ranking Sem filtros adicionais
      col.order <- colnames(Factor)
    }else{
      x <- as.matrix(Factor[, col.order])*filter # Ranking após filtros adicionais
      rank.list <- MomentumQuantil(as.xts(x), 5, QualityIndex = Vol)
    }
    
    # Lag de Ranking Momentum Genérico
    lag.rank.list <- lapply(rank.list, function(x) lag.xts(as.xts(x), k = 1, na.pad = FALSE))
    
    # 3 Returns
    stats <- NULL
    for(i in 1:length(lag.rank.list)){
      portf.assets <- NULL
      portf.return <- NULL
      rank <- lag.rank.list[[i]]
      k <- 1
      
      for(k in 1:length(holding_periods)){
      n <- holding_periods[k] 
      
      ret <- returns[-1,colnames(rank)]
      wallet.date <- head(endpoints(rank, on = "months", k = n)+1, n = ceiling(nrow(rank)/n))
      wallet.rank <- rank[wallet.date,]
      ret.rank <- NULL
      j <- 1
      for(j in 1:nrow(ret)){
        
        x <- ret[j,]
        p <- ceiling(j/n)
        y <- wallet.rank[p,]
        ret.row <- as.matrix(x)*as.matrix(y)
        ret.rank <- rbind(ret.rank, ret.row)
      }
      
      vec.ret <- as.xts(rowMeans(ret.rank, na.rm = TRUE))
      colnames(vec.ret) <- paste("H",k)
      portf.return <- cbind(portf.return, vec.ret)
      n.assets <- apply(ret.rank, 1, function(x) (length(na.omit(x))))
      assets <- apply(ret.rank, 1, function(x) (row.names(as.data.frame(na.omit(x)))))
      portf.assets[[k]] <- assets
      }
    stats[[i]] <- list( returns = portf.return, portfolios = portf.assets)
    
    }
    
    names(stats) <- c("Q1", "Q2", "Q3", "Q4", "Q5")
    return(stats)
  }
  
  BacktestValue <- function(returns, Liquidity = NULL, MktCap = NULL, Factor,  n){
    # OBS: base mensal
    # n = holding periods 
    # returns = df com o retorno mensal de todo universo de ativos
    # Liquidity = xts de Volume médio diário 
    # MktCap = xts de Market Cap 
    # Factor = xts do múltiplo relativo ao Fator
    
    #ql <- 8
    #qmk <- 4
    
    # 1 Liquidez
      # Filtro Volume Negociado
      if(!is.null(Liquidity)){
        filter <- FilterQuantile(Liquidity, ql, 10) # matriz para filtro relativo de volume negociado
        col.order <- colnames(filter)
      }  
      
      # Filtro Market Cap
      if(!is.null(MktCap)){     # Se informado dados para filtro de Market Cap
        if(is.null(Liquidity)){ # Filtro apenas de Market Cap
          filter <- FilterQuantile(MktCap, qmk, 10) 
          col.order <- colnames(filter)
        }else{                  # Filtro de Market Cap após filtro de negociabilidade
          x <- as.matrix(MktCap[, col.order])*filter
          filter <- FilterQuantile(x, qmk, 10)
        }
      }
      
    # Rank Factor
    if(is.null(Liquidity) & is.null(MktCap)){
      rank.list <- ValueQuantile(Factor, 5)      # Ranking Sem filtros adicionais
      col.order <- colnames(Factor)
    }else{
      f <- as.matrix(Factor[, col.order])*filter # Ranking após filtros adicionais
      rank.list <- ValueQuantile(as.xts(f), 5)
    }
    
    # Lag de Ranking
    lag.rank.list <- lapply(rank.list, function(y) lag.xts(as.xts(y), k = 1, na.pad = FALSE))
    
    # 3 Returns
    stats <- NULL
    for(i in 1:length(lag.rank.list)){
      
      rank <- lag.rank.list[[i]]
      ret <- returns[-1,colnames(rank)]
      wallet.date <- head(endpoints(rank, on = "months", k = n)+1, n = ceiling(nrow(rank)/n))
      wallet.rank <- rank[wallet.date,]
      ret.rank <- NULL
      
      for(j in 1:nrow(ret)){
        
        x <- ret[j,]
        p <- ceiling(j/n)
        y <- wallet.rank[p,]
        ret.row <- as.matrix(x)*as.matrix(y)
        ret.rank <- rbind(ret.rank, ret.row)
      }
      
      vec.ret <- as.xts(rowMeans(ret.rank, na.rm = TRUE))
      n.assets <- apply(ret.rank, 1, function(x) (length(na.omit(x))))
      assets <- apply(ret.rank, 1, function(x) (row.names(as.data.frame(na.omit(x)))))
      stats[[i]] <- list(mat = ret.rank, return = vec.ret, assets = assets, nassets = n.assets,rank = rank)
    }
  
    return(stats)
  }

# ******************************************************************************  
# Construção das carteiras
# ******************************************************************************  

# Parâmetros da Estratégia -----------------------------------------------------
  date.start <- "1998-03-31" # Retornos a partir do mês seguinte
  date.end <-  "2022-12-31"
  
  xts <- lapply(xts, function(x) x[paste(date.start, "/", date.end, sep ="")])          # Definição do período analisado
  roc.mom <- lapply(roc.mom, function(x) x[paste(date.start, "/", date.end, sep ="")])  # Definição do período analisado

  ql <- 8     # Filtro Decil por Volume negociado
  qmk <- NULL # Filtro Decil por Market Cap


# VALUE INVESTING --------------------------------------------------------------
  PE <- BacktestValue(Factor = xts$PriceToEarnings,
                        Liquidity = xts$`Volume Médio`,
                        MktCap = NULL,
                        returns = xts$Returns,
                        n = 12)
  
  PB <- BacktestValue(Factor = xts$PriceToBook,
                        Liquidity = xts$`Volume Médio`,
                        MktCap = NULL,
                        returns = xts$Returns,
                        n = 12)
  
  EY <- BacktestValue(Factor = xts$EarningsYeald,
                           Liquidity = xts$`Volume Médio`,
                           MktCap = NULL,
                           returns = xts$Returns,
                           n = 12)
  
# MOMENTUM ---------------------------------------------------------------------
  
  holding.periods <- c(1:12)
  MOM<- lapply(roc.mom, function(x) BacktestMomentum(Factor = x,
                                                     Liquidity = xts$`Volume Médio`,
                                                     MktCap = NULL,
                                                     returns = xts$Returns,
                                                     Vol = NULL,        
                                                     holding_periods = c(1:12)))

  names(MOM) <- c("Formation:1", "Formation:2", "Formation:3", "Formation:4", "Formation:5", "Formation:6",
                  "Formation:7", "Formation:8", "Formation:9", "Formation:10", "Formation:11", "Formation:12")

# ******************************************************************************    
# Estatísticas de Retorno
# ******************************************************************************    
  
  #Formatação da base de retornos das estratégias de momentum
  Momentum <- NULL
  for(q in 1:5){
    quantil <- NULL
    for(i in 1:length(MOM)){
      quantil[[i]] <- MOM[[i]][[q]][["returns"]]
      colnames(quantil[[i]]) <- c(paste(i,"/",1), paste(i,"/",2), paste(i,"/",3), paste(i,"/",4), paste(i,"/",5), paste(i,"/",6),
                                 paste(i,"/",7), paste(i,"/",8), paste(i,"/",9), paste(i,"/",10), paste(i,"/",11), paste(i,"/",12))
    }
    Momentum[[q]] <- quantil
    names(Momentum[[q]]) <- c("Formation:1", "Formation:2", "Formation:3", "Formation:4", "Formation:5", "Formation:6", "Formation:7", "Formation:8", "Formation:9", "Formation:10", "Formation:11", "Formation:12")
  }
  names(Momentum) <- c("Q1", "Q2", "Q3", "Q4", "Q5")
  
  # Formatação da base de retornos dos Benchmarks
  xts$Benchmarks <- xts$Benchmarks[paste(start(PE[[1]][["return"]]), "/", end(PE[[1]][["return"]]), sep ="")]
  index(xts$Benchmarks) <- index(PE[[1]][["return"]])

# CAPM -------------------------------------------------------------------------
  
  retorno <- log(Momentum$Q5$`Formation:6`$`6 / 1`+1)
  bm <- log(xts$Benchmarks$`IBXX<XBSP>`[paste(start(retorno),"/", end(retorno), sep = "")]+1)
  rf <- log(xts$Benchmarks$`CDI Acumulado<BraNa>`[paste(start(retorno),"/", end(retorno), sep = "")]+1)
  
  # Tabelas de Alphas
  CAPM <- NULL
  CAPM_summary <- function(r, bm, rf){
    
    y <- riskpremium(r, rf)
    x <- riskpremium(bm, rf)
    fit <- lm(y ~ x)
    summary <- summary.lm(fit)
    
    if(bptest(fit)$p.value>0.05){
      
      alfa <- summary[["coefficients"]][1,]
      beta <- summary[["coefficients"]][2,]
      r.quadrado <- summary[["r.squared"]]
      adj.r.quadrado <- summary[["adj.r.squared"]]
      
    }else{
      rfit<- lm_robust(y ~ x, se_type = "stata")
      summary <- summary(rfit)
      alfa <- summary[["coefficients"]][1,c(1:4)]
      beta <- summary[["coefficients"]][2,c(1:4)]
      r.quadrado <- summary[["r.squared"]]
      adj.r.quadrado <- summary[["adj.r.squared"]]
    }
    Stats <- list(alfa = alfa, beta = beta, r.quadrado = r.quadrado, adj.r.quadrado = adj.r.quadrado)
    return(Stats)
  }
  CAPM_alpha <- function(r, bm, rf){
    
    fit <- lm(riskpremium(r, rf) ~ riskpremium(bm, rf))
    summary <- summary.lm(fit)
    
    if(as.vector(bptest(fit)$p.value)>0.05){
      alfa <- summary[["coefficients"]][1,]
    }else{
      rfit<- lm_robust(riskpremium(r, rf) ~ riskpremium(bm, rf), se_type = "stata")
      summary <- summary(rfit)
      alfa <- summary[["coefficients"]][1,c(1:4)]
    }
    return(alfa)
  }
  CAPM_beta <- function(r, bm, rf){
    
    fit <- lm(riskpremium(r, rf) ~ riskpremium(bm, rf))
    summary <- summary.lm(fit)
    
    if(as.vector(bptest(fit)$p.value)>0.05){
      beta <- summary[["coefficients"]][2,]
    }else{
      rfit<- lm_robust(riskpremium(r, rf) ~ riskpremium(bm, rf), se_type = "stata")
      summary <- summary(rfit)
      beta <- summary[["coefficients"]][2,c(1:4)]
    }
    return(beta)
  }
  
  for(i in 1:5){
    CAPM$PE[[i]] <-  CAPM_summary(r = log(1+PE[[i]][["return"]]), bm = bm, rf = rf)
    CAPM$BM[[i]] <- CAPM_summary(r = log(1+PB[[i]][["return"]]), bm = bm, rf = rf)
    CAPM$EY[[i]] <- CAPM_summary(r = log(1+EY[[i]][["return"]]), bm = bm, rf = rf)
  }
  
  for(i in 1:5){
    CAPM$MOM[[i]] <- list(alpha = do.call(cbind, lapply(Momentum[[i]], function(y) apply(y, 2, function(x) CAPM_alpha(r = log(x+1), bm = bm, rf = rf)))),
                          beta = do.call(cbind, lapply(Momentum[[i]], function(y) apply(y, 2, function(x) CAPM_beta(r = log(x+1), bm = bm, rf = rf)))))   
  }
  
  ## Modelo de regressão CAPM
  fit <- lm((riskpremium(retorno, rf)) ~ (riskpremium(bm, rf)))
  summary(fit)
  
  #H0 = There is Homoscedasticity
  bptest(fit)

# Estatísiticas básicas --------------------------------------------------------
  BasicStat <- NULL
  
  for (i in 1:5){
    BasicStat$PE[[i]] <- mean.geometric(PE[[i]][["return"]])
    BasicStat$PB[[i]] <- mean.geometric(PB[[i]][["return"]])
    BasicStat$EY[[i]] <- mean.geometric(EY[[i]][["return"]])
  } # Geo mean
  for (i in 1:5){
    BasicStat$PE[[i]] <- sum(log(PE[[i]][["return"]]+1))/nrow(PE[[i]][["return"]])
    BasicStat$PB[[i]] <- sum(log(PB[[i]][["return"]]+1))/nrow(PB[[i]][["return"]])
    BasicStat$EY[[i]] <- sum(log(EY[[i]][["return"]]+1))/nrow(EY[[i]][["return"]])
  } # Log mean
  
# Tabelas-----------------------------------------------------------------------
  tabelas.Momentum <- NULL
  tabelas.Momentum$alpha <- t(matrix(CAPM[["MOM"]][[4]][["alpha"]][1,], nrow = 12, ncol = 12))
  colnames(tabelas.Momentum$alpha) <-c("Holding:1", "Holding:2", "Holding:3", "Holding:4", "Holding:5", "Holding:6", "Holding:7", "Holding:8", "Holding:9", "Holding:10", "Holding:11", "Holding:12")
  rownames(tabelas.Momentum$alpha) <- c("Formation:1", "Formation:2", "Formation:3", "Formation:4", "Formation:5", "Formation:6", "Formation:7", "Formation:8", "Formation:9", "Formation:10", "Formation:11", "Formation:12")
  
  tabelas.Momentum$alpha.pvalue <- t(matrix(CAPM[["MOM"]][[4]][["alpha"]][4,], nrow = 12, ncol = 12))
  colnames(tabelas.Momentum$alpha.pvalue) <-c("Holding:1", "Holding:2", "Holding:3", "Holding:4", "Holding:5", "Holding:6", "Holding:7", "Holding:8", "Holding:9", "Holding:10", "Holding:11", "Holding:12")
  rownames(tabelas.Momentum$alpha.pvalue) <- c("Formation:1", "Formation:2", "Formation:3", "Formation:4", "Formation:5", "Formation:6", "Formation:7", "Formation:8", "Formation:9", "Formation:10", "Formation:11", "Formation:12")
  
  tabelas.Value <- NULL
  tabelas.Value$PE <- as.data.frame(CAPM$PE)
  tabelas.Value$BM <- as.data.frame(CAPM$BM)
  tabelas.Value$EY <- as.data.frame(CAPM$EY)
  
  tabelas.Value$means <- t(cbind(as.matrix(BasicStat[["PE"]]),
                                 as.matrix(BasicStat[["PB"]]),
                                 as.matrix(BasicStat[["EY"]])))
  rownames(tabelas.Value$means) <- c("PE", "PB", "EY")
  colnames(tabelas.Value$means) <- c(1:5)
  
# Download de tabelas ---------------------------------------------------------- 
  write.csv2(as.data.frame(tabelas.Momentum$alpha), file = "MomentumAlpha.csv")
  write.csv2(as.data.frame(tabelas.Momentum$alpha.pvalue), file = "MomentumAlpha_pval.csv")
  write.csv2(tabelas.Value$PE, file = "capm_PE.csv")
  write.csv2(tabelas.Value$BM, file = "capm_BM.csv")
  write.csv2(tabelas.Value$EY, file = "capm_EY.csv")
  write.csv(as.data.frame(tabelas.Value$means), file = "ValueMeans.csv")
  
  
  