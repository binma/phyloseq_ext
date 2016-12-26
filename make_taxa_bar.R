make.bar <- function(level=6, keepnum=30, phyloseq=epl.otu.norm, sample.class=paste(epl.sample$matrix,
                                                                                    epl.sample$column)){
  sarah.phyla <- apply(otu_table(phyloseq),MARGIN = 2,
                       FUN = function(x){tapply(x,INDEX = tax_table(phyloseq)[,level],sum)})

  sarah.phyla.rank <- sarah.phyla[order(rowSums(sarah.phyla),
                                        decreasing = TRUE),]

  sarah.phyla.rank1<-  sarah.phyla.rank[rownames(sarah.phyla.rank)!="character",]

  sarah.phyla.rank.prune <- rbind(sarah.phyla.rank1[c(1:keepnum),],
                                  Others = colSums(sarah.phyla.rank1[-c(1:keepnum),])
  )

  class.name <- tax_table(phyloseq)[pmatch(rownames(sarah.phyla.rank.prune)[1:(keepnum-1)],tax_table(phyloseq)[,level]),3]


  sarah.phyla.percent <- t(t(sarah.phyla.rank.prune)/colSums(sarah.phyla.rank.prune))

  sarah.phyla.percent.comb <- apply(sarah.phyla.percent,MARGIN = 1,
                                    FUN=function(x){tapply(x,sample.class,
                                                           FUN=mean)})

  list(df=sarah.phyla.percent.comb[,c(order(class.name),keepnum+1)],
       class=sort(class.name))
}

