library(igraph)
library(tidygraph)
library(ggraph)
library(tidyverse)

edgelist <- read.csv("data/DAG_edgelist.csv", header = F) |> 
  as.matrix()

g <- graph_from_edgelist(edgelist)

g_tidy <- as_tbl_graph(g) |> 
  activate(nodes) %>%
  mutate(wrapped_name = str_wrap(name, width = 15))

ggraph(g_tidy, layout = 'igraph', algorithm = 'sugiyama',
       maxiter = 100) + 
  geom_edge_link(edge_colour = 'grey',
                 aes(start_cap = label_rect(node1.wrapped_name), 
                     end_cap = label_rect(node2.wrapped_name)),
                 #start_cap = circle(10, 'mm'),
                 arrow = arrow(length = unit(4, 'mm'))
                 ) + 
  geom_node_label(
                 #check_overlap = T,
                 label.padding = unit(0.25, "lines"),
                 aes(label =  wrapped_name)) +
  ggtitle("Basic Network Plot") +
  theme_void()



plot(g,
     edge.arrow.size = 0.5,
     vertex.label.color = "black", 
     edge.color = 'gray77',
     vertex.frame.color=NA,
     vertex.size = 0,
     vertex.label.dist = 1.2,
     layout = layout_nicely(g))