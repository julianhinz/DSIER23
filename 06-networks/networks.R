
### set wd to source file dir
setwd("~/work/Teaching/DSIER23/06-networks")

library(tidyverse)
library(igraph)
library(data.table)

set.seed(1234)
edge_list <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 1, 4, 2, 1))
node_list <- tibble(id = 1:4)
g <- igraph::graph_from_data_frame(d = edge_list, vertices = node_list, directed = TRUE)

plot(g, edge.arrow.size = 0.7, vertex.label.cex=3)
igraph::get.adjacency(g)

# create the same network from the adj. matrix
adjmatrix <- matrix(c(0,1,0,0,
                      1,0,0,1,
                      0,1,0,0,
                      1,0,0,0), nrow = 4, ncol = 4, byrow = TRUE)
g <- graph_from_adjacency_matrix(
  adjmatrix,
  mode = c("directed"),
  weighted = NULL,
  diag = TRUE,
  add.colnames = NULL,
  add.rownames = NA
)
plot(g, edge.arrow.size = 0.7)

# try yourself using the figure!

# path, walks, length
igraph::all_simple_paths(g, 3, 1)
igraph::shortest_paths(g, 3, 1)
plot(g)

# type of graphs
full_graph <- make_full_graph(6, directed = FALSE, loops = FALSE)
plot(full_graph)
star <- make_star(6, mode = c("directed"), center = 1)
plot(star, edge.arrow.size = 0.7)

# generate a dataframe to represents all the edges of your bipartite ntw
d <- data.frame(country=c("DEU", "DEU", "FRA", "FRA", "CAN","CAN", "USA"), 
                        trade_agr=c("CETA","EU", "EU", "CETA","CETA","USMCA","USMCA"))
# trasform it in a graph
g <- graph_from_data_frame(d, directed = FALSE)
as_data_frame(g, what="vertices")
# define color and shape mappings to distinguish nodes type
V(g)$label <- V(g)$name
V(g)$type <- 1
V(g)[name %in% d$trade_agr]$type <- 2
col <- c("steelblue", "orange")
shape <- c("circle", "square")

# plot it!
plot(g,
     vertex.color = col[V(g)$type],
     vertex.shape = shape[V(g)$type]
)

# Inspect the world trade network
## 1. BACI data available at http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37

# Import the conversion from BACI code number country to isocode 3 digits (DEU, ESP...)
conversion <- read_csv("country_codes_cepii_V2021.csv.gz") %>% 
  mutate(i=country_code, j= country_code) %>% 
  select(isocode3=iso_3digit_alpha, i, j)

## Use 2017 Baci and attach conversion codes
file1 = "BACI_HS07_Y2017_V202001_p1.csv.gz"
file2 = "BACI_HS07_Y2017_V202001_p2.csv.gz"

data_baci <- rbind(fread(file1), fread(file2)) %>%
    filter(!(str_detect(k,"^98") | str_detect(k,"^99"))) %>% select(t,i,j,k,v) %>% 
    left_join(select(conversion,i,isocode3), by=c("i")) %>% mutate(i=isocode3) %>% 
    select(-isocode3) %>% 
    left_join(select(conversion,j,isocode3), by=c("j")) %>% 
    rename(exp=i,imp=isocode3)

# remove the conversion file
rm(conversion)

# plot the degree distribution 
data_baci %>% select(exp,imp) %>% distinct() %>% group_by(exp) %>% 
  mutate(degree=n()) %>% select(exp, degree) %>% distinct() %>% 
  ggplot(., aes(x = degree)) +
  geom_histogram(aes(y = ..density..), bins = 10, color="white", fill="blue") +
  xlab("Degrees") + ylab("Frequencies") + ggtitle("Out-Degree distribution of the Trade Network in 2017") +
  theme_minimal()

# compute out- in-degree 
data_baci %>% select(exp,imp) %>% distinct() %>% group_by(exp) %>% 
  mutate(outdegree=n()) %>% select(exp, outdegree) %>% distinct() %>% 
  arrange(-outdegree)

data_baci %>% select(exp,imp) %>% distinct() %>% group_by(imp) %>% 
  mutate(indegree=n()) %>% select(imp, indegree) %>% distinct() %>% 
  arrange(-indegree)

# compute closeness centrality
trade_network <- data_baci %>% select(exp,imp) %>% distinct() %>% 
  graph_from_data_frame(., directed = TRUE) %>% na.omit()

setNames(rownames_to_column(data.frame(closeness( # assign names coutry and centrality to the output
  trade_network,
  mode = c("in"),
  weights = NULL,
  normalized = TRUE))), c("country", "centrality")) %>% 
  arrange(-centrality)  %>% 
  head()


# WEIGHTED NETWORK
# Mock example, assign random weight (1-5) according to certain probability distr.

random.seed(1234)
adjm <- matrix(sample(0:5, 25, replace=TRUE,
                      prob=c(0.9,0.02,0.02,0.02,0.02,0.02)), ncol=5)
g2 <- graph_from_adjacency_matrix(adjm, weighted=TRUE)
plot(g2, vertex.label = V(g)$name, vertex.label.cex=3)
E(g2)$weight

# R read all what comes next the first 2 columns as attributes of edges, in this case they are weight
edgelist <- read.table(text = "
V1 V2 weight
                       A B 1
                       B C 8
                       C D 6
                       D E 9
                       C F 12
                       F G 15",header=T)
g <- graph_from_data_frame(edgelist)
is_weighted(g)

# account for trade flows weights
w_trade_network <- data_baci %>% group_by(exp,imp) %>% mutate(weight=sum(v, na.rm=TRUE)) %>% 
  select(exp,imp,weight=v) %>% distinct() %>% na.omit() %>% 
  graph_from_data_frame(., directed = TRUE) 

is_weighted(w_trade_network)


setNames(rownames_to_column(data.frame(strength(
  w_trade_network,
  mode = c("in"),
  loops = TRUE,
  weights = w_trade_network$weight))), 
  c("country", "degree")) %>% 
  arrange(-degree)  %>% 
  head()


out_d <- setNames(rownames_to_column(data.frame(strength(
  w_trade_network,
  mode = c("out"),
  loops = TRUE,
  weights = w_trade_network$weight))), 
  c("country", "out_degree")) 

in_d <- setNames(rownames_to_column(data.frame(strength(
  w_trade_network,
  mode = c("in"),
  loops = TRUE,
  weights = w_trade_network$weight))), 
  c("country", "in_degree")) 

# what is the out-in degree rank correlation?
out_d %>% full_join(in_d, by=c("country")) %>% 
  summarise(cor(in_degree, out_degree, method = c("pearson")))
rm(out_d, in_d)

# nb when you have weights, the centrality is not the average number of edges of 
# the shortest paths to each other node, but the average sum of weights on the shortest path for the distance.

# Produce a table with the closeness index computed on the weighted network
setNames(rownames_to_column(data.frame(closeness(
  w_trade_network,
  mode = c("out"),
  weights = w_trade_network$weight,
  normalized = TRUE,
  cutoff = -1))), 
  c("country", "centrality")) %>% 
  arrange(-centrality)  %>% 
  head()

# What has changed? Why ?

# Exercise: input either through adj matrix or as a dataframe the network as plotted above

#SOLUTIONS TO EXERCISE
# create a network, inspect the adj. matrix and plot
adjmatrix <- matrix(c(0,1,1,0,0,
                      1,0,1,0,0,
                      0,0,0,0,0,
                      0,0,1,0,0,
                      0,0,1,0,0), nrow = 5, ncol = 5, byrow = TRUE)
g <- graph_from_adjacency_matrix(
  adjmatrix,
  mode = c("directed", "undirected", "max", "min", "upper", "lower", "plus"),
  weighted = NULL,
  diag = TRUE,
  add.colnames = NULL,
  add.rownames = NA
)
plot(g)

edge_list <- tibble(from = c(5,5, 6, 6, 8, 9), to = c(6, 7, 7, 5, 7, 7))
node_list <- tibble(id = 5:9)
g<- igraph::graph_from_data_frame(d = edge_list, vertices = node_list, directed = TRUE)
igraph::get.adjacency(g)
plot(g, edge.arrow.size = 0.7)
