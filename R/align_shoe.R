#' @name align_shoe
#' @export
#'
#' @title  Align two shoe mesh objects
#'
#' @description This function aligns two mesh shoe objects.
#'
#' @param mesh1 mesh object to be used as base for second mesh
#' @param mesh2 mesh object to be aligned to mesh1
#'
#'
#' @return a list of two triangle mesh objects, the original mesh 1 and the aligned mesh 2
#'
#'
#' @import
#'
#'




align_shoe<-function(meshobject1,meshobject2, itter=100){
  #centering the mesh objects
  center3d <- function(mesh, method=barycenter) {
    bary <- method(mesh) %>% colMeans()
    translate3d(mesh, -bary[1], -bary[2], -bary[3])
  }
  mesh1<-center3d(meshobject1)
  mesh2<-center3d(meshobject2)

  #making the shoes into verticies for pca
  shoe1<-shoe_coord(mesh1)
  shoe2<-shoe_coord(mesh2)

  #Computing itital pca alignment
  pca1<-prcomp(shoe1, center = TRUE,scale. = FALSE)
  pca2<-prcomp(shoe2, center = TRUE,scale. = FALSE)
  #applying the PCA
  shoetran1<-transform3d(mesh1, matrix = rotationMatrix(matrix = pca1$rotation))
  shoetran2<-transform3d(mesh2, matrix = rotationMatrix(matrix = pca2$rotation))
  #put in check of det of pca and then use 8 to flip everything back

  #creating a rotation matrix (this will be from sin/cos)
  rotations <- list(
    rotationMatrix(matrix=diag(c(1,1,1))),
    rotationMatrix(matrix=diag(c(1,-1,-1))),
    rotationMatrix(matrix=diag(c(-1,1,-1))),
    rotationMatrix(matrix=diag(c(-1,-1,1)))
  )

  #Apply the rotations to the second scan then measure the distance from the first
  if(det(pca1$rotation)<0){
    shoetran1<-transform3d(shoetran1, matrix = rotationMatrix(matrix = diag(c(1,1,-1))))

  }

  if(det(pca2$rotation)<0)  {
    shoetran2<-transform3d(shoetran2, matrix = rotationMatrix(matrix = diag(c(1,1,-1))))
  }




  # Applying the rotation matrices to the second shoe
  distances<-1:4
  for(i in 1:4){
    shoe_transformed<-transform3d(shoetran2, matrix = rotations[[i]])
    promesh <- Rvcg::vcgClostKD(shoetran1,shoe_transformed,sign=T,threads = 1)
    closest <- promesh$vb[1:3,]
    distances[i]<- mean(abs(promesh$quality))
  }

  rot<-which.min((distances))
  shoe_transformed<-transform3d(shoetran2, matrix = rotations[[rot]])
  icpshoe<-icp(shoe_transformed,shoetran1, iterations = itter)[[1]]

  promesh <- Rvcg::vcgClostKD(shoetran1,icpshoe,sign=T,threads = 1)
  closest <- promesh$vb[1:3,]
  d<- mean(promesh$quality)
  mesh1<-shoetran1
  meshalign<-icpshoe
  return(list(mesh1, meshalign, distances,rot, d))


}











