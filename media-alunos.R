idade <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t){
  #Metódo sum faz a soma de notas dentro de um vetor
  soma_idade <- sum(c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t))
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
notas(8,7,7,7,7,8,5,2,4,4,10,10,10,10,4,6,7,7,7,8)

  
  
}