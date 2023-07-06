libdir=getOption("fspls.libdir","C:/Users/LCOIN/R-4.0.2/library")
.libPaths(libdir)
#install.packages("ggraph")

library(ggraph)
library(tidygraph)
library(readxl)

##FUNCTIONS

.removeDupl<-function(edges2){
  edges2[!duplicated(apply(edges2, 1, function(v) paste(sort(v[1:2]), collapse="_"))),]
}

.readInputFile<-function(input_file){
  matr = read_xlsx(input_file, sheet="distance_matrix")
  #matr = read.delim(input_file ,head=F,sep=",")
  #rownames(matr) = matr[,1]
  vertices = read_xlsx(input_file, sheet="metadata")
  vertices = vertices[!is.na(vertices[,1]),,drop=F]
  mtch = read_xlsx(input_file, sheet="match")
  thresholds = read_xlsx(input_file, sheet="thresholds")
  
  for(k in 1:nrow(mtch)){
    if(mtch$Type[[k]]=="numeric") vertices[[mtch$Key[[k]]]]=as.numeric(vertices[[mtch$Key[[k]]]])
    if(mtch$Type[[k]]=="categorical") vertices[[mtch$Key[[k]]]]=as.factor(vertices[[mtch$Key[[k]]]])
    
  }

#  rownames(vertices) = vertices[,1]
  
 
 # colnames(matr) = matr[1,]
  matr= apply(as.matrix(matr[,-1]),c(1,2), as.numeric)
  rownames(matr) = colnames(matr)
  list(matr =matr, vertices = vertices, match = mtch, thresholds = thresholds)
}


.getEdges<-function(matr, thresh){
  print(thresh)
  tv = paste(thresh, collapse=" ")
  print(tv)
  fn = eval(str2lang(paste("function(x) x", thresh[[1]], thresh[[2]]))) #type))
  
  edges=apply(matr,1,function(v) {
    v[which(fn(v))]
  })
  edges = edges[unlist(lapply(edges, length))>0]
  if(length(edges)==0){
    edges2 = data.frame(matrix(nrow=0, ncol = 4))
  }else{
  edges1 = unlist(lapply(1:length(edges), function(k){
    nme = names(edges)[k]
    edgek=lapply(1:length(edges[[k]]), function(i1) c(names(edges[[k]])[i1],nme,edges[[k]][[i1]]))
    names(edgek)=lapply(edgek,function(v) paste(v[1:2], collapse="_"))
    edgek[unlist(lapply(edgek, function(v)v[1]!=v[2]))]
  }), rec=F)
  }
  edges2= data.frame(cbind(t(data.frame(edges1[!duplicated(names(edges1))])), tv))
  names(edges2) =c("from","to","dist","thresh")
  edges2$dist = as.numeric(edges2$dist)
  edges3 = .removeDupl(edges2)
  edges3
}

.plotNetwork<-function(edges3, vertices, mtch, ncol = 2){
  color = subset(mtch, Value=="color")
  shape = subset(mtch, Value=="shape")
  label = subset(mtch, Value=="label")
  
  g1 <- igraph::graph_from_data_frame(edges3, directed=FALSE, vertices=vertices)
  gg = ggraph(g1, layout = 'kk') +
    #geom_edge_fan()+
    geom_edge_fan(aes(color = dist)) + 
    geom_node_point(size=3, aes_string( shape=shape$Key,color=color$Key)) +
    #scale_color_brewer(type="qual", palette=3) +
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white', base_size = 14) +
      facet_wrap(~thresh, scales = "free", ncol = ncol) +
    scale_edge_colour_gradientn(colours = c('#313695', '#ffffbf', '#a50026'))
  gg<-gg+ geom_node_label(aes_string(label = label$Key),  
                                       repel = TRUE, show.legend = FALSE, family = "serif")
  gg
}
.merge<-function(edges_all){
  edges3 = edges_all[[1]]
  for(k in 2:length(edges_all)){
    edges3 =rbind(edges3,edges_all[[k]])
  }
  edges3
}

.runAll<-function(input_file, output_pdf = "network.pdf", multiplier=15){
 
  
    inp =.readInputFile(input_file)
    matr = inp$matr
    vertices = inp$vertices
    thresholds =inp$thresholds
    edges3 =.merge(apply(thresholds,1, function(t) .getEdges(matr,  t)))
    ncol = 3
    nrow = ceiling(length(thresholds)/3)
    gg<-.plotNetwork(edges3,vertices, inp$match,ncol=ncol)
      #edges3$'dist'=as.numeric(edges3$dist)
    ggsave(output_pdf, width = ncol * multiplier, height = nrow * multiplier, units="cm", device = cairo_pdf)
    invisible(gg)
}
##THIS IS BIT THAT RUNS CODE 
#source("graph.R")
input_file="../20230309_ExampleMetadata.xlsx"
.runAll(input_file, output_pdf="network1.pdf")

