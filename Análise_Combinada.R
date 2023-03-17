
# Estratégia combinada-----------------------------------------------------------------------
  Main_Portfolios <- list(returns = NULL, log.returns = NULL, index = NULL, log.index = NULL) 
  Main_Portfolios[["returns"]] <- as.xts(merge(PE[[5]][["return"]],
                                               PB[[5]][["return"]],
                                               EY[[5]][["return"]],
                                               Momentum[["Q5"]][["Formation:4"]][,"4 / 4"], # 4
                                               Momentum[["Q5"]][["Formation:5"]][,"5 / 4"], # 6
                                               xts$Benchmarks$`IBXX<XBSP>`[paste(start(PE[[5]][["return"]]),"/", end(PE[[5]][["return"]]), sep = "")]))
  names(Main_Portfolios[["returns"]]) <- c("PE", "PB", "EY", "MOM 4:4","MOM 5:4" ,"IBrX 100")
  
  Main_Portfolios[["log.returns"]] <- as.xts(apply(Main_Portfolios[["returns"]], 2, function(x) log(x+1)))
  Main_Portfolios[["index"]] <- as.xts(apply(Main_Portfolios[["returns"]], 2, function(x) cumprod(x+1)))
  Main_Portfolios[["log.index"]] <- as.xts(apply(Main_Portfolios[["index"]], 2, function(x) log(x)))

  # Tabelas de Estatísticas   
    cbind.data.frame(
      tabela.estatisticas(Main_Portfolios[["returns"]][,1], xts$Benchmarks$`CDI Acumulado<BraNa>`),
      tabela.estatisticas(Main_Portfolios[["returns"]][,2], xts$Benchmarks$`CDI Acumulado<BraNa>`),
      tabela.estatisticas(Main_Portfolios[["returns"]][,3], xts$Benchmarks$`CDI Acumulado<BraNa>`),
      tabela.estatisticas(Main_Portfolios[["returns"]][,4], xts$Benchmarks$`CDI Acumulado<BraNa>`),
      tabela.estatisticas(Main_Portfolios[["returns"]][,5], xts$Benchmarks$`CDI Acumulado<BraNa>`),
      tabela.estatisticas(Main_Portfolios[["returns"]][,6], xts$Benchmarks$`CDI Acumulado<BraNa>`)
    )

# Gráfico de índices------------------------------------------------------------
  dev.new()

  plot.xts(merge(Main_Portfolios$log.index[,1], Main_Portfolios$log.index[,2], Main_Portfolios$log.index[,5]), 
       main = NA,
       col =  c("#000066", "#3399FF", "#6666FF", "black"),
       lty=c(rep(1, 5),2), 
       lwd=rep(2, 6),
       grid.ticks.lty = 3)
  addLegend("topleft",
            legend.names = c("Earnings to Price", "Book to Market", "Earnings Yield", "IBrX 100"),
            lty=c(rep(1, 5),2), lwd=rep(2, 6),
            col = c("#000066", "#3399FF", "#6666FF", "black"))
  
  
  # Momentum
    plot(merge(Main_Portfolios[["log.index"]][,4],
               Main_Portfolios[["log.index"]][,5],
               Main_Portfolios[["log.index"]][,6]),
         main = NA,
         col =  c("#006666", "#6666FF", "#CC0000"),
         lty=c(rep(1, 5),2), 
         lwd=rep(2, 6),
         grid.ticks.lty = 3)
    addLegend("topleft",
              legend.names = c("Momentum 4/4","Momentum 5/4", "IBrX 100"),
              lty=c(rep(1, 5),2), lwd=rep(2, 6),
              col = c("#006666", "#6666FF", "#CC0000"))
  
    plot(Drawdowns(merge(Main_Portfolios[["returns"]][,4],
                         Main_Portfolios[["returns"]][,6])),
         main = NA,
         grid.ticks.lty = 3,
         lwd=c(2,1),
         col = c("#006666", "#CC0000"))
    addLegend("bottomright",
              legend.names = c("Momentum 4/4", "IBrX 100"),
              lty=c(rep(1, 5),2), lwd=c(2,1),
              col = c("#006666", "#CC0000"))
    
  # Value Investing
    plot(merge(Main_Portfolios[["log.index"]][,1],
               Main_Portfolios[["log.index"]][,2],
               Main_Portfolios[["log.index"]][,3],
               Main_Portfolios[["log.index"]][,6]),
         main = NA,
         col =  c("#000066", "#3399FF", "#6666FF", "#CC0000"),
         lty=c(rep(1, 5),2), 
         lwd=rep(2, 6),
         grid.ticks.lty = 3)
    addLegend("topleft",
              legend.names = c("Earnings to Price", "Book to Market", "EBIT to Enterprise Value",  "IBrX 100"),
              lty=c(rep(1, 5),2), lwd=rep(2, 6),
              col = c("#000066", "#3399FF", "#6666FF", "#CC0000"))

  
# Tabela de Correlação ---------------------------------------------------------
  tabelas.Combo <- NULL
  tabelas.Combo$cor <- cor(Main_Portfolios[["log.returns"]])
  # Correlação Momentum X Value
    tabelas.Combo$cor2 <- t(tail(t(cor(  merge(Momentum[["Q5"]][["Formation:1"]][,12],Momentum[["Q5"]][["Formation:2"]][,12],
                                              Momentum[["Q5"]][["Formation:3"]][,12],Momentum[["Q5"]][["Formation:4"]][,12],
                                              Momentum[["Q5"]][["Formation:5"]][,12],Momentum[["Q5"]][["Formation:6"]][,12],
                                              Momentum[["Q5"]][["Formation:7"]][,12],Momentum[["Q5"]][["Formation:8"]][,12],
                                              Momentum[["Q5"]][["Formation:9"]][,12],Momentum[["Q5"]][["Formation:10"]][,12],
                                              Momentum[["Q5"]][["Formation:11"]][,12],Momentum[["Q5"]][["Formation:12"]][,12],
                                              Main_Portfolios$log.returns$PE, Main_Portfolios$log.returns$PB, Main_Portfolios$log.returns$EY))),3))[c(1:12),]
    

# Portfólios combinados ------------------------------------------------------------------------
  
Combo <- NULL
  Combo$ret <- merge(Return.portfolio(merge(PE[[5]][["return"]], Momentum[["Q5"]][["Formation:4"]][,"4 / 4"]), wealth.index = F, weights = c(0.5,0.5), rebalance_on = "months"),
                     Return.portfolio(merge(PB[[5]][["return"]], Momentum[["Q5"]][["Formation:4"]][,"4 / 4"]), wealth.index = F, weights = c(0.5,0.5), rebalance_on = "months"),
                     Return.portfolio(merge(EY[[5]][["return"]], Momentum[["Q5"]][["Formation:4"]][,"4 / 4"]), wealth.index = F, weights = c(0.5,0.5), rebalance_on = "months"))
  names(Combo$ret) <- c("PE/MOM4", "PB/MOM4", "EY/MOM4")

  Combo$index <- as.xts(apply(Combo$ret, 2, function(x) cumprod(x+1)))
  Combo$log.ret <- as.xts(apply(Combo$ret, 2, function(x) log(x+1)))
  Combo$log.index <- as.xts(apply(Combo$index, 2, log))

  
  plot(merge(Combo$log.index$`PE/MOM4`,
             Main_Portfolios$log.index$PE,
             Main_Portfolios$log.index$`MOM 4:4`,
             Main_Portfolios$log.index$`IBrX 100`),
       main = NA,
       col =  c("#3300ff", "#3399FF", "#006666", "#CC0000"),
       lty=c(rep(1, 3),2), 
       lwd=rep(2, 5),
       grid.ticks.lty = 3)
  addLegend("topleft",
            legend.names = c("Earnings to Price & Mommentum", "Earnings to Price", "Momentum","IBrX 100"),
            lty=c(1, 1), lwd=c(2, 1),
            col = c("#3300ff", "#3399FF", "#006666", "#CC0000"))
  
  
  plot(merge(Combo$log.index$`PB/MOM4`,
             Main_Portfolios$log.index$PB,
             Main_Portfolios$log.index$`MOM 4:4`,
             Main_Portfolios$log.index$`IBrX 100`),
       main = NA,
       col =  c("#3300ff", "#3399FF", "#006666", "#CC0000"),
       lty=c(rep(1, 3),2), 
       lwd=rep(2, 5),
       grid.ticks.lty = 3)
  addLegend("topleft", on=1,
            legend.names = c("Book to Market & Mommentum", "Book to Market", "Momentum","IBrX 100"),
            lty=c(1, 1), lwd=c(2, 1),
            col = c("#3300ff", "#3399FF", "#006666", "#CC0000"))
  
  
  plot(merge(Combo$log.index$`EY/MOM4`,
             Main_Portfolios$log.index$EY,
             Main_Portfolios$log.index$`MOM 4:4`,
             Main_Portfolios$log.index$`IBrX 100`),
       main = NA,
       col =  c("#3300ff", "#3399FF", "#006666", "#CC0000"),
       lty=c(rep(1, 3),2), 
       lwd=rep(2, 5),
       grid.ticks.lty = 3)
  addLegend("topleft", on=1,
            legend.names = c("EBIT to EV & Mommentum", "EBIT to EV", "Momentum","IBrX 100"),
            lty=c(1, 1), lwd=c(2, 1),
            col = c("#3300ff", "#3399FF", "#006666", "#CC0000"))
  

  
  
# Tabela de estatísticas -------------------------------------------------------
  (cbind.data.frame(
    tabela.estatisticas(Main_Portfolios$log.returns$EY, rf = rf),
    tabela.estatisticas(Main_Portfolios$log.returns$PE, rf = rf),
    tabela.estatisticas(Main_Portfolios$log.returns$`MOM 4:4`, rf = rf),
    tabela.estatisticas(Combo$log.ret$`PE/MOM4`, rf = rf),
    tabela.estatisticas(Combo$log.ret$`EY/MOM4`, rf = rf)
  ))
  
  Combo$geo.mean <- apply(Combo$ret, 2, mean.geometric)
  Combo$log.mean <- apply(Combo$log.ret, 2, function(x) sum(x)/length(x))
  
# CAPM -------------------------------------------------------------------------
  
  CAPM$Combo <- apply(Combo$log.ret, 2, function(x) CAPM_summary(r = x, bm, rf))
  tabelas.Combo$capm <- as.data.frame(CAPM$Combo)
  
# Download de tabelas  
  write.csv2(tabelas.Combo$capm, file = "capm_Combo.csv")
  write.csv2(tabelas.Combo$cor, file = "cor.csv")
  write.csv2(tabelas.Combo$cor2, file = "cor2.csv")
  write.csv2(cbind(as.data.frame(Combo$geo.mean),as.data.frame(Combo$log.mean)),file = "mean_Combo.csv")
  
  