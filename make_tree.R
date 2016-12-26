make_tree <-function(taxa_table=taxa.dataframe){

  edge.list1 <- NULL

#add a root node when have bacteria and archaea
  if (length(unique(taxa_table[,1]))>1) {
  edge.list1 <- data.frame(parent="Root",
               child=unique(taxa_table[,1]))
  }

#make the edge list from phylum to family
  for (i in 2:5){
    taxa <- unique(taxa_table[,i])
    unknown <- grep(pattern = "character",
                    x = taxa)
    if(length(unknown)>0){
    taxa <- taxa[-unknown]}
    edge.list2 <- taxa_table[pmatch(x = taxa,
                                    taxa_table[,i]),
                             (i-1):i]
    colnames(edge.list2) <- c("parent","child")
    edge.list1 <- rbind(edge.list1,edge.list2)
  }

# the edge list of genera
  genera <- unique(taxa_table[,6])
  family <- taxa_table[pmatch(x = genera,
                    taxa_table[,6]),
             2:5]
  family[family[,4]=="character",4] <- family[family[,4]=="character",3]
  family[family[,4]=="character",4] <- family[family[,4]=="character",2]
  family[family[,4]=="character",4] <- family[family[,4]=="character",1]

  edge.list3 <- data.frame(parent=family[,4],
                           child=genera)

  parent <- c(as.character(edge.list1[,1]),family[,4])
  child  <- c(as.character(edge.list1[,2]),genera)
  nnode <- sum(child %in% parent) + 1
  tip<-child[!child %in% parent]

# the final edge list
  edgelist <- matrix(c(factor(c(parent,child),
                    levels = unique(c(parent,child)))),
                         ncol=2,
                         byrow = F)

# find tips
  edgelist1<- edgelist[order(edgelist[,1]),]
  edgelist1 <- edgelist1[edgelist1[,1]!=edgelist1[,2],]
  tips <- edgelist1[!edgelist1[,2] %in% edgelist1[,1],] # tips do not present in parent
  left <- edgelist1[edgelist1[,2] %in% edgelist1[,1],] # get the edges without tips
  group1=c()
  for(i in 1:length(unique(tips[,1]))){
    group1<-c(group1,paste(tips[tips[,1]==unique(tips[,1])[i],2],collapse = ","))
  }
  # add brackets for groups
  ##group1[grep(pattern = ",",x = group1,fixed = TRUE)]<- paste("(",
  #                                                              group1[grep(pattern = ",",
  #                                                                          x = group1,
  #                                                                          fixed = TRUE)],
  #                                                              ")",unique(tips[,1])[table(tips[,1])>1],sep="")

  group1[table(tips[,1])>1]<-paste("(",group1[table(tips[,1])>1],")",
                                   unique(tips[,1])[table(tips[,1])>1],
                                   sep="")

  edgelist2 <- left
  left2 <- edgelist2[edgelist2[,2] %in% edgelist2[,1],]
  nodel2 <- edgelist2[!edgelist2[,2] %in% edgelist2[,1],]
  group2 <- c()
  for(j in 1 :length(unique(nodel2[,1]))){
    group2<-c(group2,paste(group1[pmatch(nodel2[nodel2[,1]==unique(nodel2[,1])[j],2],
                                   unique(tips[,1]))],collapse = ",")
              )
  }

  # find out the new groups without brackets
  group2[table(nodel2[,1])>1]<-paste("(",group2[table(nodel2[,1])>1],")",
                                   unique(nodel2[,1])[table(nodel2[,1])>1],
                                   sep="")


  edgelist3 <- left2
  left3 <- edgelist3[edgelist3[,2] %in% edgelist3[,1],]
  nodel3 <- edgelist3[!edgelist3[,2] %in% edgelist3[,1],]
  group3 <- c()
  for(k in 1:length(unique(nodel3[,1]))){
    group3<-c(group3,paste(group2[pmatch(nodel3[nodel3[,1]==unique(nodel3[,1])[k],2],
                                         unique(nodel2[,1]))],collapse = ",")
    )
  }

  group3[table(nodel3[,1])>1]<-paste("(",group3[table(nodel3[,1])>1],")",
                                     unique(nodel3[,1])[table(nodel3[,1])>1],
                                     sep="")

  edgelist4 <- left3
  left4 <- edgelist4[edgelist4[,2] %in% edgelist4[,1],]
  nodel4 <- edgelist4[!edgelist4[,2] %in% edgelist4[,1],]
  group4 <- c()
  for(l in 1:length(unique(nodel4[,1]))){
    group4<-c(group4,paste(group3[pmatch(nodel4[nodel4[,1]==unique(nodel4[,1])[l],2],
                                         unique(nodel3[,1]))],collapse = ",")
    )
  }

  group4[table(nodel4[,1])>1]<-paste("(",group4[table(nodel4[,1])>1],")",
                                     unique(nodel4[,1])[table(nodel4[,1])>1],
                                     sep="")


  edgelist5 <- left4
  left5 <- edgelist5[edgelist5[,2] %in% edgelist5[,1],]
  nodel5 <- edgelist5[!edgelist5[,2] %in% edgelist5[,1],]
  group5 <- c()
  for(m in 1:length(unique(nodel5[,1]))){
    group5 <- c(group5,paste(group4[pmatch(nodel5[nodel5[,1]==unique(nodel5[,1])[m],2],
                                         unique(nodel4[,1]))],collapse = ",")
    )
  }

  group5 <- paste("(",group5,")",c("Bacteria","Archaea"),sep="")

  group6 <- paste("(",paste(group5,collapse = ","),")Root;",sep = "")
  write.table(x = group6,
              file = "tree.tre",
              quote = FALSE,
              row.names = F,
              col.names = F)

  tree <- read.tree("tree.tre")
  edge.level <- unique(c(parent,child))
  tree$tip.label <- edge.level[as.numeric(tree$tip.label)]
  tree$node.label <- edge.level[as.numeric(tree$node.label)]
  tree$node.label[is.na(tree$node.label)]<-c("Root","Bacteria","Archaea")
  tree
}

