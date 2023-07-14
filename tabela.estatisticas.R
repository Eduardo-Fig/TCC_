
tabela.estatisticas <- function(r, rf){
  # Parâmetros utilizados
  # r = retorno do portfolio (vetor de retornos mensais)
  # bm = índice de mercado (vetor de retornos mensais)
  # rf = taxa livre de risco (vetor de retornos mensais)
  
  # CAGR
  # Desvio Padrão
  # Sharpe Ratio
  # Worst DrawDown
  # Worst month Return
 
  rf <- rf[paste(start(r), "/", end(r), sep ="")]
  rf <- as.numeric(mean(rf))
  
  
  mean <- mean(r)
  sd <- sd(r)
  sr <- (mean-rf)/sd
  
  wdd <- as.matrix(min(Drawdowns(r)))
  names(wdd) <- colnames(mean)

  
  
  x <- as.data.frame(rbind(mean, sd, sr, wdd))
  row.names(x) <- c("Retorno Médio",
                    "Desvio Padrão",
                    "Sharpe", 
                    "Worst DrawDown")
  
  
  
  return(x)
  
}
