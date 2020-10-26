#' @name distance_shoe
#' @export
#'
#' @title  Find the distance between two aligned shoe mesh objects
#'
#' @description This function finds the distance between two mesh shoe objects, that have already been aligned.
#'
#' @param mesh1 mesh object
#' @param mesh2 mesh object
#'
#'
#' @return a list with the numeric distance metric and the distribution of distances between points
#'
#'
#' @import Rvcg
#'

distance_shoe<-function(mesh1, mesh2){
  promesh <- Rvcg::vcgClostKD(mesh1,mesh2,sign=T,threads = 1)
  promesh2 <- Rvcg::vcgClostKD(mesh2,mesh2,sign=T,threads = 1)
  differences<-c(sqrt((promesh$quality)^2),sqrt((promesh2$quality)^2))
  d<-mean(differences)
  return(list(d,differences))
}

a<-distance_shoe(test12[[1]],test12[[2]])






