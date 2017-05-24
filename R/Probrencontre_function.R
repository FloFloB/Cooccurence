#' Calculate probability of appearance
#'
#' This function allows you to calculate the probability ofappearance of aword.
#' @param N,n,f
#' @keywords appearance
#' @export
#' @examples
#' Probrencontre()


# Fonction pour calculer la probabilit? d'apparition du mot
Probrencontre<-function(N,n,f){
  # N nombre total de mots dans le corpus
  # n nombre moyen de mots dans un paragraphe ou dans une page
  # f fr?quence du mot consid?r? dans le corpus
  # k absence de ce mot dans le paragraphe ou la page
  # Calcul de la probabilit? d'absence du mot selon la formule hyperg?om?trique, ici k=0
  ##prob=(factorial(N-f)*factorial(N-n))/(factorial(N-f-n)*factorial(N)) ne marche pas
  prob=exp(lgamma(N-f+1)+lgamma(N-n+1)-lgamma(N-f-n+1)-lgamma(N+1))

  # Calcul de la probabili? de pr?sence du mot
  prob= 1-prob
  return(prob)
}
