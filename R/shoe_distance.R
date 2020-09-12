#' @name shoe_distance
#' @export
#'
#' @title  Distance between two mesh objects
#'
#' @description this function returns a distance metric berween two mesh objects
#'
#' @param mesh1 mesh object
#' @param mesh2 mesh object to be compared to the first mesh object
#' @param sign If FALSE take absolute value of the distances
#' @param conf If TRUE returns 95% CI of the distances, if false returns mean
#'
#'
#' @return a distance metric
#'
#'
#' @importFrom Rvcg vcgClostKD




shoe_distance<-function(mesh1, mesh2, sign=TRUE, conf=TRUE){

  promesh <- Rvcg::vcgClostKD(mesh1,mesh2,sign=sign,threads = 1)
  closest <- promesh$vb[1:3,]
  distances<- promesh$quality

  if(conf=TRUE){
    low <- quantile(distances,probs=0.05,na.rm = TRUE)
    upper<-quantile(distances, probs=.95, na.rm = TRUE)
    confidence<-c(low, upper)
    return(confidence)
  }

  else{
    dists<-mean(distances)
    return(dists)
  }


}
