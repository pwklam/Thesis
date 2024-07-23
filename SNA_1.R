#loading package
library(tidygraph)
library(tidyverse)

library(ggraph)
library(showtext)
showtext_auto()

#mkaing node list and edge list
node_list<-read.csv("/Users/paullam/winner_node_list.csv")
node_list

edge_list<- read.csv("/Users/paullam/winner_edge_list.csv")
edge_list

df= edge_list[-(493:1460),]

edge_list<- df

names<- edge_list %>%
  distinct(Name)%>%
  rename(label = Name)

organizations<- edge_list %>%
  distinct(Organization)%>%
  rename(label = Organization)  

nodes <- full_join(names, organizations, by = "label")
nodes <- rowid_to_column(nodes, "id")
nodes

per_route <- edge_list %>%  
  group_by(Name, Organization) %>%
  summarise(weight = n(), .groups = "drop")
per_route

edges <- per_route %>% 
  left_join(nodes, by = c("Name" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("Organization" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)
edges

#activate network object

library(network)

legco_network <- network(edges,
                         vertex.attr = nodes,
                         matrix.type = "edgelist",
                         ignore.eval = FALSE)
class(legco_network)
summary(legco_network)


library(tidygraph)
library(ggraph)
legco <- tbl_graph(nodes = nodes,
                   edges = edges,
                   directed = TRUE)
legco_tidy <- as_tbl_graph(routes_igraph)

class(legco_tidy)
class(legco)
class(routes_igraph)

legco_tidy

legco_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

#calulate the degree centrality

Degree_Centrality<- degree(legco_tidy) 

data.frame(Degree_Centrality)

Degree_Centrality_list <-cbind(nodes, Degree_Centrality)

library("writexl")

write_xlsx(Degree_Centrality_list,"/Users/paullam/Degree_Centrality.xlsx")

#calulate the network graph of degree centrality 

library(visNetwork)
visNetwork(nodes, edges)
edges <- mutate(edges, width = weight/5 + 1)

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")
legco_tidy

legco_tidy %>% 
  mutate(degree = centrality_degree(mode = "in")) %>%
  ggraph(layout = "fr") +
  geom_edge_fan(alpha = 0.25) +
  geom_node_point(aes(size = degree,color = degree)) +
  geom_node_text(aes(size = degree,label = label),
                 repel = T,show.legend = F) +
  scale_color_continuous(guide = "legend") +
  theme_graph() + labs(title = "Winner's group",
                       subtitle = "Based on Degree Centrality")

legco_tidy %>% 
  mutate(closeness = centrality_closeness()) %>%
  ggraph(layout = "nicely") +
  geom_edge_fan(alpha = 0.5) +
  geom_node_point(aes(size = closeness,color = closeness)) +
  geom_node_text(aes(size = closeness, label = label),
                 repel = T, show.legend = F) +
  scale_color_continuous(guide = "legend") +
  theme_graph() + labs(title = "Winner's group",
                       subtitle = "Based on Closeness Centrality")


