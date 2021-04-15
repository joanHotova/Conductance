library(igraph)
library(stringr)


#------------return_rows_from_feature_file------------

return_rows_from_feature_file <- function(f){
  rows <- readLines(f)
  return (rows)
}



#----------------return_nodes_and_feature_name_given_row_from_feature_file-----------

return_nodes_and_feature_name_given_row_from_feature_file <- function(row){

  m <- (str_split(row, "\t", simplify = TRUE))
  
  fnodes <- as.list(m[,2])      
  
  feature <- as.list(m[,1]) #oles oi grammes ths prwths sthlhs
  
  tempList2 <- list("fnodes"=fnodes,"feature"=feature)
  return(tempList2)
}



#---------------returns_vectorS---------------


returns_vectorS <-function(G,n){
  
  n <- (str_split(n, ",", simplify = TRUE)[1,]) # n= pinakas me 1-1 nodes tou panw n
  for (t in n){  #gyros1: a1, a2, a4 / gyros2: a3, a5 /gyros3: a2, a3 
    
    Ps <- t  
    x[[paste0("", t)]] <- Ps
    mx <- matrix(unlist(x))
    
    S<-list(mx)
    
    if (!(is.null(S))){
      vectorS<-unlist(S)
    }
  }

  return (vectorS)
}



#---------------returns_vectorT---------------
returns_vectorT <-function(G,vectorS,nnmatrix){
  T<-NULL
  for (sn in nnmatrix[,1]){
    if (!(sn %in% vectorS)){ #an den einai ola sto S, o,ti perisseyei mpainei sto T
      Ps2 <- sn  
      y[[paste0("", sn)]] <- Ps2
      my <- matrix(unlist(y))
      
      node_degrees2 <- as.list(degree(G,v=my[,1]))
      ndeg2<-as.numeric(paste(unlist(node_degrees2))) 
      my<-cbind(my, ndeg2)

      T<-list(my[,1])
      
    }
  }
  

  if (!(is.null(T))){
    vectorT<-as.character(unlist(T))
    return (vectorT)
  }else{
    return (NULL)
    
  }
  

}


#---------------boundary---------------
  
  
  boundary <-function(G,vectorS,vectorT){
    epair<-0
    
    for(n1 in vectorS){
      for (i in 1:length(neighbors(G,n1))){
        u<-(neighbors(G,n1)[[i]])
        if (u$name %in% vectorT){
          n2<-u$name
          lista<- list(n1,n2)
          sub<-induced_subgraph(G,unlist(lista)) 
          epair<- epair+ as.numeric(E(sub)$weight)
        }
      }
    }
    return (epair)
  }
  
  
  
  



#-------------------MAIN---------------------

#args <- commandArgs(trailingOnly = TRUE)
#net_path <- args[1]
#anno_path <- args[2]

net_path <- "WtestNetwork2.txt"  
anno_path <-"testAnnotationFile2.txt"

network_file_table <- read.table(net_path)
annotation_file_table <- read.table(anno_path)

network_file_matrix <- as.matrix(network_file_table)
annotation_file_list <- as.list(annotation_file_table)


if(ncol(network_file_matrix)==3){
  weights_matrix<-network_file_matrix[,3]
}else{
  weights_matrix<-matrix(1, nrow = nrow(network_file_matrix), ncol = 1) 
  network_file_matrix<-cbind(network_file_matrix,weights_matrix)
}



G <- graph_from_data_frame(network_file_table, directed = FALSE)%>%
  set_edge_attr( "weight", value= weights_matrix)



rows <-  return_rows_from_feature_file(anno_path)
netnodes <- (network_file_matrix)
nnlist <- as.list(netnodes[,1:2])
nnmatrix<-as.matrix(unique(nnlist))

x <- list()
y<-list()
c2<-""
c3<-NULL

for (row in rows){
  
  f2 <- return_nodes_and_feature_name_given_row_from_feature_file(row) #pairnei 1-1 tis grammes
  fnodes <- f2$fnodes #an to nodes to kanw show mesa se for tote ta deixnei ola osa exei mesa kai oxi mono to teleytaio
  feature <- f2$feature #to idio kai edw

   for (n in fnodes){ #n = node,node....
        
   vectorS<- returns_vectorS(G,n)
   vectorT<- returns_vectorT (G,vectorS,nnmatrix)
   
   
   Ssum<-0
   for(s in vectorS){
     indexs<-which(as.matrix(netnodes[,1:2])  == s, arr.ind = TRUE)
     Ssum<-Ssum+sum(as.numeric(netnodes[indexs[,1],3]))
   }

  if (!(is.null(vectorT))){
     Tsum<-0
     for(t in vectorT){
       indext<-which(as.matrix(netnodes[,1:2]) == t, arr.ind = TRUE)
       Tsum<-Tsum+sum(as.numeric(netnodes[indext[,1],3]))
     }
     bound<-boundary(G,vectorS,vectorT)
     conductance<-bound/min(Ssum,Tsum)
     c1<-toString(conductance)
     c2<-paste(c2,c1,sep="-")
     c3<-as.list(as.numeric(strsplit(c2, "-")[[1]]))
     show(conductance)
     
   }else{
     conductance<-0
     show(conductance)
   }
   
   
   
  }
}

if(!(is.null(c3))){ #conductance!=0
  
  if(c3[[2]]==1){ #conductance=1
    c4<-as.list(as.numeric(c3)+0.1)
    v_c4<-unlist(c4)
    v_c3<-unlist(c3)

    hist(v_c3[c(2:length(v_c3))],main="Histogram of Conductance",xlab="Conductance",ylab="Number of samples in Range",xlim=c(0,1),breaks=c(v_c3[c(2)],v_c4[c(2)]) ,col = "orange") #number of samples...=number clusters
  
    }else{ #conductance = (0,1)
    
    v_c3<-unlist(c3)
    hist(v_c3[c(2:length(v_c3))],main="Histogram of Conductance",xlab="Conductance",ylab="Number of samples in Range",xlim=c(0,1) ,col = "orange") #number of samples...=number clusters
    
  }
  
}else{ #conductance=0
  hist(conductance,main="Histogram of Conductance",xlab="Conductance",ylab="Number of samples in Range",xlim=c(0,1),breaks=c(0,0.1),col = "orange") #number of samples...=number clusters
}

