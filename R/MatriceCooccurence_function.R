#' Fonction pour le calcul de la matrice de cooccurence
#'
#' This function allows you to calculate the cooccurrence matrix.
#' @param mots,presencemot .
#' @keywords cooccurrence matrix
#' @export
#' @examples
#' MatriceCooccurence()



# Fonction pour le calcul de la matrice de cooccurence
MatriceCooccurence<-function(mots,presencemot){
  x<-matrix(nrow = length(mots),ncol=length(mots))
  rownames(x) <- mots
  colnames(x) <- mots

  for(i in 1:length(mots)){
    for(j in 1:length(mots)){
      if(i==j){
        x[i,j]=0
      }else{
        x[i,j]=presencemot[i]*presencemot[j]
      }
    }
  }
  return(x)
}
