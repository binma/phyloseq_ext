taxa.df <- function(biom = test.biom){
        taxa.split<-strsplit(observation_metadata(biom)[,],",")
        taxa.df <- data.frame(Domain = gsub(pattern = "d:",
                                    replacement = "" ,
                                    as.character(sapply(X = taxa.split, function(x){x[grep(pattern = "d:",x)]}))),
                      Phylum = gsub(pattern = "p:",
                                    replacement = "" ,
                                    as.character(sapply(X = taxa.split, function(x){x[grep(pattern = "p:",x)]}))),
                      Class = gsub(pattern = "c:",
                                   replacement = "" ,
                                   as.character(sapply(X = taxa.split, function(x){x[grep(pattern = "c:",x)]}))),
                      Order = gsub(pattern = "o:",
                                   replacement = "" ,
                                   as.character(sapply(X = taxa.split, function(x){x[grep(pattern = "o:",x)]}))),
                      Family = gsub(pattern = "f:",
                                    replacement = "" ,
                                    as.character(sapply(X = taxa.split, function(x){x[grep(pattern = "f:",x)]}))),
                      Genus = gsub(pattern = "g:",
                                   replacement = "" ,
                                   as.character(sapply(X = taxa.split, function(x){x[grep(pattern = "g:",x)]})))
        )

        taxa.df.clean1 <- apply(taxa.df,
                        MARGIN = 2,
                        function(x){gsub(pattern = "\\([0-9.]*\\)",
                                         replacement = "" ,
                                         x = x,
                                         perl = T)})

        taxa.df.clean2 <- apply(taxa.df.clean1,
                        MARGIN = 2,
                        function(x){gsub(pattern = "\"",
                                         replacement = "" ,
                                         x = x,
                                         perl = F)})

        row.names(taxa.df.clean2)  <- row.names(biom_data(biom))
        taxa.df.clean2
}
