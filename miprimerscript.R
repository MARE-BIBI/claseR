library(tidyverse)

Fibo <- function(n){
  if(n<=0){
    "no esta permitido"
  }
      else if(n==1 | n==2) {
        resultado=1
        resultado
      }
            else {
            resultado = Fibo(n-1)+ Fibo(n-2)
            resultado
            }
}

Fibo(n=7)
