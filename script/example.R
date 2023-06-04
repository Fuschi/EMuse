# Load and install packages
library(remotes)
install_github("https://github.com/Fuschi/EMuse")
library(EMuse)


# Load data
#------------------------------------------------------------------------------#
data("bacteria")
data("taxonomy")


# Filter rarest taxa
#------------------------------------------------------------------------------#
prevalence <- function(X){ colSums(X>0)/nrow(X) }
median.non.zero <- function(X){ apply(X,2,function(x)median(x[x>0])) }
bac.filt <- bacteria[, prevalence(bacteria)>=.33 & median.non.zero(bacteria)>=5]
tax.filt <- taxonomy[colnames(bac.filt),]

# Get CLR Abundances
#------------------------------------------------------------------------------#
clr.bac.filt <- clr(bac.filt+1)

# Correlation on between CLR tranformed taxa
#------------------------------------------------------------------------------#
cor <- cor(clr.bac.filt)

# Get adjacency via edge density threshold
#------------------------------------------------------------------------------#
adj <- cor*(abs(cor)>.3)                                            # absolute
adj <- adjacency_edge_density(cor,.02)                              # edge density
adj <- adjacency_p_adjust(clr.bac.filt,method="pearson",alpha=.05)  # p-value

# Create Graph
#------------------------------------------------------------------------------#
g <- graph_from_adjacency_matrix(adjmatrix=adj,mode="undirected",weighted=TRUE)

# Get Communities
#------------------------------------------------------------------------------#
comm <- cluster_signed_louvain(g,OS="Linux")

# Add graphical properties
#------------------------------------------------------------------------------#
V(g)$size <- colMeans(clr.bac.filt) + 6
E(g)$color <- ifelse(E(g)$weight>0,rgb(0,0,1,.5),rgb(1,0,0,.5))

# Generate color palette for communities and classifications.
rank <- "class"
palette.rank <- generate_taxonomy_palette(taxonomy[,rank])
palette.comm <- generate_communities_palette(20)

# Plot Graph
#------------------------------------------------------------------------------#
png(width=1200, height=400)
# X11(width=12,height=4)
layout(t(c(1,2,3)),widths=c(1/3,1/3,1/3))
par(mar=c(0,0,2,0),cex.main=2)

plot.new()
legend(x=.0,y=.75,legend=names(palette.rank),
       fill=palette.rank,cex=.75,ncol=2)

plot(g,layout=layout_signed(g),vertex.label=NA, main=rank,
     vertex.color=palette.rank[tax.filt[,rank]])

plot(g,layout=layout_signed(g),vertex.label=NA, main="communities",
     vertex.color=palette.comm[as.character(membership(comm))])
dev.off()


# Plot Graph
#------------------------------------------------------------------------------#
X11(width=12,height=4)
layout(t(c(1,2,3)),widths=c(1/3,1/3,1/3))
par(mar=c(0,0,2,0),cex.main=2)

g.pos <- subgraph.edges(g, eids=which(E(g)$weight>0), delete.vertices=FALSE)
g.neg <- subgraph.edges(g, eids=which(E(g)$weight<0), delete.vertices=FALSE)

plot.new()
legend(x=.0,y=.75,legend=names(palette.rank),
       fill=palette.rank,cex=.75,ncol=2)

plot(g,layout=layout_signed(g),vertex.label=NA, main=rank,
     vertex.color=palette.rank[tax.filt[,rank]],
     vertex.size=(degree(g.pos)/max(degree(g.pos)))*10 )

plot(g,layout=layout_signed(g),vertex.label=NA, main="communities",
     vertex.color=palette.comm[as.character(membership(comm))],
     vertex.size=(degree(g.neg)/max(degree(g.neg)))*10 )





