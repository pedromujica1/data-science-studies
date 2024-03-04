#vetores ou listas em R

x <- c(2,8,3)
y <- c(7,69,52)
#SOMA RECICLA VALORES QUANDO A TAMANHO DO VETOR É MAIOR
#SOMA RESPECTIVOS VALORES DO VETOR
print(x+y)
#COMPARA VETORES
print(x>y)

x+c(1,2,3)
num = 9
#IF EM R
if (num> 0){print('negative number not found') }
count = 0
#FOR em R
for (variable in y) {
  count = count+variable 
  print(count)
  
}
#FUNÇOES EM R
pow <- function(x,y) {
  #function to print x to power of y
  result <- x^y
  print(paste(x,"elevado a",y," resulta em: ",result))
}
pow(10,2)




