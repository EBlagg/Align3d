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
#' @return a list with the numeric distance metric, the difference from scan 1 to scan 2 and the differences from scan 2 to scan 1
#'
#' @import Rvcg
#'

distance_shoe<-function(mesh1, mesh2){
  promesh <- Rvcg::vcgClostKD(mesh1,mesh2,sign=T,threads = 1)
  promesh2 <- Rvcg::vcgClostKD(mesh2,mesh2,sign=T,threads = 1)
  differences<-c(sqrt((promesh$quality)^2),sqrt((promesh2$quality)^2))
  differences_1<-promesh$quality
  differences_2<-promesh2$quality
  d<-mean(differences)
  return(list(d,differences_1, differences_2))
}








