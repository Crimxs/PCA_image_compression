#####################       Functions for Image Compression

install.packages("magick")


pca <- function(cur_wd,new_wd,reso)

  {
    

  
  library(magick)
  library(dplyr)
  library(jpeg)
  
  setwd(cur_wd)
  
  
  images = Sys.glob(file.path(cur_wd, "*.jpg"))
  
  #View(images)
  
  
  #for names
  
  allFiles = list.files(path = ".", pattern = ".jpg")
  
  
  #View(allFiles)
  
  allInfo = image_info(image_read(allFiles))
  
  #View(allInfo)
  
  allInfo$fileName = allFiles
  
  
  counter = 1
  
  for (image in images) 
    
    {
    
    image = readJPEG(image)
    
    
    r<-image[,,1]
    g<-image[,,2]
    b<-image[,,3]
    
    # <-------------------------->
    #For Red
    
    image.r.pca <- prcomp(r, center = FALSE)
    
    cumprop = cumsum((apply(image.r.pca$x,2,var)))/sum(apply(image.r.pca$x,2,var))*100
    
    
    count = 1
    for (i in cumprop) {
      if(i>=99.5){
        break
      }
      count = count+1
    }

    count
    
    R = image.r.pca$x[,1:count]%*%t(image.r.pca$rotation[,1:count])
    
    
    R <- ifelse(R >1 ,1 , R)
    R <- ifelse(R < 0 ,0 , R)
    
    
    
    # <---------------------------------------->
    #for Green

    image.g.pca <- prcomp(g, center = FALSE)

    cumprop = cumsum((apply(image.g.pca$x,2,var)))/sum(apply(image.g.pca$x,2,var))*100

    count = 1
    for (i in cumprop) 
      {
      if(i>=reso){
        break
      }
      count = count+1
    }

    G = image.g.pca$x[,1:count]%*%t(image.g.pca$rotation[,1:count])

   

    G <- ifelse(G >1 ,1 , G)
    G <- ifelse(G < 0 ,0 , G)

    # <---------------------------------------->
    # for B

    image.b.pca <- prcomp(b, center = FALSE)

    cumprop = cumsum((apply(image.b.pca$x,2,var)))/sum(apply(image.b.pca$x,2,var))*100

    count = 1
    for (i in cumprop) {
      if(i>=reso){
        break
      }
      count = count+1
    }

    B = image.b.pca$x[,1:count]%*%t(image.b.pca$rotation[,1:count])


    B<- ifelse(B >1 ,1 , B)
    B<- ifelse(B < 0 ,0 , B)

   # <----------------------------------------->
    
    rgb.pca <- list(image.r.pca, image.g.pca, image.b.pca)
    
    
    
    
  
    img = array(c(R,G,B), dim = c(dim(image)[1:2],3))
    setwd(new_wd)
    writeJPEG(img , allInfo$fileName[counter])
    counter = counter +1
    
  }
  
  
  
  setwd(cur_wd)
 
  delete_files <- readline(prompt="Do you want to delete the existing files (y/n): ")
  
  if(delete_files == 'y')
    {
   
    answer = readline(prompt="Are you Sure??? (y/n): ")
    
    if (answer == 'y') 
      {
      
      file.remove(allFiles, cur_wd)
    }
   
    else 
      {
      print('Thankyou')
    }
  }
  
  else
    {
    print('Thankyou')
    
  }
  
  
}

print("Enter the full location if your images for which PCA needs to be done, replace slash to backslash ")
Current <- readline(prompt = "Enter Location here: ")

print("Enter the location where you would like to store the images, replace slash to backslash")
New <- readline(prompt = "Enter the location here: ")


New <- paste(New,"/PCA_images")
New<-gsub(" ", "", New, fixed = TRUE)

dir.create(path = New)

pca(Current, New, 99.5)






