require(phytools)
require(phangorn)

condition = commandArgs(trailingOnly=TRUE)[1]

root.to.singleton<-function(tree){
  if(!inherits(tree,"phylo"))
    stop("tree should be object of class \"phylo\".")
  if(!is.null(tree$root.edge)){
    tree$edge[tree$edge>Ntip(tree)]<-
      tree$edge[tree$edge>Ntip(tree)]+1
    if(attr(tree,"order")%in%c("postorder","pruningwise")){
      tree$edge<-rbind(tree$edge,c(1,2)+Ntip(tree))
      tree$edge.length<-c(tree$edge.length,tree$root.edge)
    } else {
      tree$edge<-rbind(c(1,2)+Ntip(tree),tree$edge)
      tree$edge.length<-c(tree$root.edge,tree$edge.length)
    }
    tree$root.edge<-NULL
    tree$Nnode<-tree$Nnode+1
    if(!is.null(tree$node.label)) 
      tree$node.label<-c("",tree$node.label)
  }
  tree
}

get.birth.branches <- function(tree,x) {
  sites <- c()
  C <- length(which(x[1:(length(tree$tip.label)),1] != 1))
  for (i in length(tree$tip.label):(length(tree$tip.label)+tree$Nnode)) {
    y <- getDescendants(tree,i)
    y <- y[y<=length(tree$tip.label)]
    if (length(which(x[tree$tip.label[y],1] != 1)) == C) {
      sites <- c(sites,which(tree$edge[,2]==i))
    }
  }
  return(sites)
}

data.df <- read.csv(paste('../data/',condition,'_character_data.tsv',sep=''),row.names=1,sep='\t')

trees <- read.nexus('../data/austronesian-mapped.nex')
tree <- maxCladeCred(trees)
tree <- keep.tip(tree,rownames(data.df))
tree$root.edge <- 1000
tree <- reorder.phylo(tree,'pruningwise')
tree <- root.to.singleton(tree) 

J = length(unique(lapply(strsplit(colnames(data.df),'\\.'),function(x){x[2]})))
D = ncol(data.df)/J

bin.states <- data.df[tree$tip.label,]
bin.states <- rbind(as.matrix(bin.states),matrix(1,nrow=tree$Nnode,ncol=ncol(bin.states)))

parent <- tree$edge[,1]
child <- tree$edge[,2]
b.lens <- tree$edge.length/1000
N <- length(unique(c(parent,child)))
T <- length(child[which(!child %in% parent)])
B=length(parent)

tip.lik <- matrix(nrow=D,ncol=3+N*J+2*B)
for (d in 1:D) {
  bin.states_ = bin.states[,((J*d)-(J-1)):(J*d)]
  dim(bin.states_) <- c(N*J)
  bin.states_ <- c(N,B,J,bin.states_,child,parent)
  tip.lik[d,] <- bin.states_
}

birth.sites <- matrix(nrow=D,ncol=2*B)
for (d in 1:D) {
  sites <- get.birth.branches(tree,bin.states[,((J*d)-(J-1)):(J*d)])
  sites <- ifelse(c(1:B) %in% sites, 1, 0)
  birth.sites[d,] <- c(b.lens,sites)
}

data.list <- list(D=D,
                  N=N,
                  J=J,
                  B=B,
                  xi=tip.lik,
                  xr=birth.sites,
                  brlen=b.lens)

saveRDS(data.list,file=paste(condition,'_data.RDS',sep=''))