libdir=getOption("fspls.libdir","C:/Users/LCOIN/R-4.0.2/library")
.libPaths(libdir)
#install.packages("ggraph")

library(ggraph)
library(tidygraph)

##FUNCTIONS

.removeDupl<-function(edges2){
  edges2[!duplicated(apply(edges2, 1, function(v) paste(sort(v[1:2]), collapse="_"))),]
}

.readInputFile<-function(input_file){
  matr = read.delim(input_file ,head=F,sep=",")
  rownames(matr) = matr[,1]
  vertices = data.frame(matr[-1,1,drop=F] )
  colnames(matr) = matr[1,]
  matr = apply(matr[-1,-1],c(1,2),as.numeric)
  names(vertices) = "name"
  vertices = cbind(vertices, t(data.frame(apply(vertices, 1,strsplit,"_"))))
  names(vertices)[4:5]=c("year","type")
  vertices$year =as.numeric(as.character(vertices$year))
  vertices$type = factor(vertices$type)
  list(matr = matr, vertices = vertices)
}


.getEdges<-function(matr,vertices, thresh){
  print(thresh)
  edges=apply(matr,1,function(v) {
    v[which(v>=thresh)]
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
  edges2= data.frame(cbind(t(data.frame(edges1[!duplicated(names(edges1))])), thresh))
  }
  names(edges2) =c("from","to","dist","thresh")
  edges2$dist = as.numeric(edges2$dist)
  edges2$thresh = as.numeric(edges2$thresh)
  edges3 = .removeDupl(edges2)
  edges3
}

.plotNetwork<-function(edges3, vertices, showlabel=F){
  g1 <- igraph::graph_from_data_frame(edges3, directed=FALSE, vertices=vertices)
  gg = ggraph(g1, layout = 'kk') +
    #geom_edge_fan()+
    geom_edge_fan(aes(colour = dist)) + 
    geom_node_point(size=3, aes( shape=type,color=year)) +
    #scale_color_brewer(type="qual", palette=3) +
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white', base_size = 14) +
      facet_wrap(~thresh, scales = "free", ncol = 2) +
    scale_edge_colour_gradientn(colours = c('#313695', '#ffffbf', '#a50026'))
  if(showlabel)gg<-gg+ geom_node_label(aes(label = name),  
                                       repel = TRUE, show.legend = FALSE, family = "serif")
  gg
}



input_file= "../../SC2-care-homes-anonymised/processed_data/alignment_carehome_20200720_snp_dist.txt"
input_file = "../20230705_HCVExampleData_PercentID.csv"
inp =.readInputFile(input_file)
matr = inp$matr
vertices = inp$vertices
thresholds = seq(80, 95,5)
edges3 = .getEdges(matr, vertices, thresholds[[1]])
for(k in 2:length(thresholds)){
  edges3 = rbind(edges3, .getEdges(matr, vertices, thresholds[[k]]))
}
gg<-.plotNetwork(edges3,vertices, FALSE)
  #edges3$'dist'=as.numeric(edges3$dist)
ggsave("network.pdf", width = 9, height = 12, device = cairo_pdf)


