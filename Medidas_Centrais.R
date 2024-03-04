idades <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t){
  #Metódo sum faz a soma de notas dentro de um vetor
  soma_idades <- sum(c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t))
  med_idade <- soma_idades /20
  cat("A média das notas é ",med_idade, "|")
  
  lista_idades = c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
  tamanho_lista =  length(lista_idades)
  #ORDENANDO AS NOTAS
  idades_ordenadas = sort(lista_idades)
  if(tamanho_lista %% 2 == 0 ){
    #FUNÇÃO MEAN FAZ A MEDIA
    #FAZ A MEDIA DAS DUAS POSICÔES NO MEIO
    #POSICOES NO MEIO: NOTAS_ORDENADAS[10] E NOTAS_ORDENADAS[11]
    mediana = mean(idades_ordenadas[(tamanho_lista/2)+ 0:1 ])
    cat("A médiana das notas é ",mediana, "|")
  }else{
    #RETIRA O UNICO VALOR DO MEIO QUANDO O TAMANHO FOR IMPAR
    mediana = mean(idades_ordenadas[(tamanho_lista+1) /2])
    cat("A médiana das notas é ",mediana, "|")
    
  }
  #para 50% menores e maiores notas
  #FATIAMENTO, COMEÇA NO 0 E VAI ATE O METADE DA LISta
  menores_idades_media <- mean(idades_ordenadas[0:(tamanho_lista/2)])
  #começa no meio e vai até o final
  maiores_idades_media <- mean(idades_ordenadas[((tamanho_lista/2)+1):tamanho_lista ])
  
  #mostrando na tela
  cat("MEDIA DOS 50% MENORES NOTAS: ",menores_idades_media, "|")
  cat("MEIDIA DOS 50% MAIORES NOTAS: " ,maiores_idades_media, "|")

}
notas <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t){
  #Metódo sum faz a soma de notas dentro de um vetor
  soma_notas <- sum(c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t))
  med_notas <- soma_notas /20
  cat("A média das notas é ",med_notas, "|")
  
  lista_notas = c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
  tamanho_lista =  length(lista_notas)
  #ORDENANDO AS NOTAS
  notas_ordenadas = sort(lista_notas)
  if(tamanho_lista %% 2 == 0 ){
    #FUNÇÃO MEAN FAZ A MEDIA
    #FAZ A MEDIA DAS DUAS POSICÔES NO MEIO
    #POSICOES NO MEIO: NOTAS_ORDENADAS[10] E NOTAS_ORDENADAS[11]
    mediana = mean(notas_ordenadas[(tamanho_lista/2)+ 0:1 ])
    cat("A médiana das notas é ",mediana, "|")
  }else{
    #RETIRA O UNICO VALOR DO MEIO QUANDO O TAMANHO FOR IMPAR
    mediana = mean(notas_ordenadas[(tamanho_lista+1) /2])
    cat("A médiana das notas é ",mediana, "|")
    
  }
  #para 50% menores e maiores notas
  #FATIAMENTO, COMEÇA NO 0 E VAI ATE O METADE DA LISta
  menores_notas_media <- mean(notas_ordenadas[0:(tamanho_lista/2)])
  #começa no meio e vai até o final
  maiores_notas_media <- mean(notas_ordenadas[((tamanho_lista/2)+1):tamanho_lista ])
  
  #mostrando na tela
  cat("MEDIA DOS 50% MENORES NOTAS: ", menores_notas_media, "|")
  cat("MEIDIA DOS 50% MAIORES NOTAS: " ,maiores_notas_media, "|")
  
}
#TESTANDO FUNÇÕS
notas(8,7,7,7,7,8,5,2,4,4,10,10,10,10,4,6,7,7,7,8)
idades(10,11,12,9,10,11,10,10,10,10,11,11,10,9,10,10,11,11,10,10)

#Trazendo vetores para fora das funções
notas_vetor = c(8,7,7,7,7,8,5,2,4,4,10,10,10,10,4,6,7,7,7,8)
idades_vetor = c(10,11,12,9,10,11,10,10,10,10,11,11,10,9,10,10,11,11,10,10)

#Colocando dados em dataframe
df = data.frame(notas_vetor,idades_vetor)

#ordenando
library(dplyr)
df_ordenado_idades = df[order(df$idades_vetor), ]

#OBTENDO MEDIA DAS NOTAS PARA 50% MENORES IDADES
#HEAD PEGA AS PRIMEIRAS LINHAS DO DATAFRAME
#NROW PEGA AS LINHAS DO DATA FRAME PARA FORMAR O NOVO DATAFRAME
#NROW DIVIDIO POR /2 POIS SO É NECESSARIO PEGAR AS 50% MENORES IDADES
df_menor = head(df_ordenado_idades,nrow(df_ordenado_idades)/2 )
#USAR O DF MENOR PARA CALCULAR NOTA MEDIA DOS 50% ALUNOS COM AS MENORES IDADES
media_menor_idade_notas = mean(df_menor$notas_vetor)
print(media_menor_idade_notas)

#OBTENDO A MEDIA DAS NOTAS PARA AS 50% MAIORES IDADES
#TAIL PEGA O FINAL DO DATAFRAME, OU SEJA O FINAL DA METADE DO DATRAFRAME
df_maior = tail(df_ordenado_idades,nrow(df_ordenado_idades)/2 )
#USAR O DF MENOR PARA CALCULAR NOTA MEDIA DOS 50% ALUNOS COM AS MENORES IDADES
media_maior_idade_notas = mean(df_maior$notas_vetor)
print(media_maior_idade_notas)

#Há relação entre idade e nota?
diferenca_percentual_notas = ((8.5/5.3)-1)*100
diferenca_percentual_idades = ((10.8/9.8)-1)*100
cat("DIFERENÇA PERCENTUAL ENTRE AS MAIORES E MENORES NOTAS É ",diferenca_percentual_notas)
cat("DIFERENÇA PERCENTUAL ENTRE AS MAIORES E MENORES NOTAS É ",diferenca_percentual_idades)

#CALCULANDO COEFICIENTE DE VARIAÇÂO
#SD CALCULA DESVIO PADRAO
cv_notas = (sd(notas_vetor) / mean(notas_vetor)) * 100
cv_idades = (sd(idades_vetor) / mean(idades_vetor)) * 100
cat("O coeficiente de variação das notas é:", cv_notas, "|")
cat("O coeficiente de variação das idades é:", cv_idades)

#Medida avançada
#COORELAÇÂO POSSIVEL que pode ser positiva mas não é garantia 
cor(idades_vetor,notas_vetor, method = "pearson")


