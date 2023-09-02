diffsignicorr3D <- function(eno,mat1,mat2,mat3) #n nb echantillon mat deux matrice 2D de correlation   
{
  
  
  diffsigni <- mat1*NA
  print( length(mat1[1,]))
  for(i in 1:length(mat1[,1]))
  {
    for(j in 1:length(mat1[1,]))
    {
      #print(mat1[i,j]) #&&is.na(mat2[i,j]))                         
      if(!(is.na(mat1[i,j])&&is.na(mat2[i,j])&&is.na(mat3[i,j])))
      {
        aux <- paired.r(mat1[i,j],mat2[i,j],mat3[i,j],n=eno[i,j],twotailed=FALSE)
        diffsigni[i,j]=aux$p
      }
    }
  }
  return(diffsigni)
  
  return(diffsigni)
}