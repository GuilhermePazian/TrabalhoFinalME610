tbl_anova_intens_dor <- function(fit_model,x = 4){
  anova <- anova(fit_model)
  anova <- cbind(c("Intesidade da Dor","Erro"),anova)
  row.names(anova) <- NULL
  names(anova) <- c("Fonte de variação","gl","Soma de quadrados", "Quadrados Médios","Estatística F","P-valor")
  row.name <- FALSE
  library(xtable)
  options(xtable.comment = FALSE)
  return(xtable(anova,digits = x))
}

tabelaproporcao <- function(dados){ apply(dados[,11:33], 2, function(x){Contagem = sum(x)
n = length(x)
prop = Contagem/n
return(c(Contagem,n,prop))})}