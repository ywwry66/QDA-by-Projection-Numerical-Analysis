library(tables)
draw_table <- function(x){
  x$n=as.factor(x$n)
  t=tabular(n*(Heading(pe)*Prediction.Error+ci)*Heading()*mean ~ Methods,x)
  print(t)
  latex(t)
}