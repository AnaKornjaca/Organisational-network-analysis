library(igraph)
#Ovaj paket omogucava da se napravi pajp line
library(magrittr)
library(ggplot2)
library(tidyr)
library(dplyr)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("visNetwork")
library(visNetwork)

#0.SVE mreze kao jedna lista sa atributima,
#kao sto naziv kaze

networks_with_atr = list(Information_or_advice=information_or_advice_3_graph, 
                    Value=value_3_graph,
                    Advice=advice_3_graph,
                    Awareness=awareness_3_graph)
networks_with_atr
networks_low_weight<-list(Information_or_advice=information_or_advice_3_graph_low_weight,
                          Value=value_3_graph_disagree,
                          Advice=advice_3_graph_low_weight,
                          Awareness=awareness_3_graph_disagree)
networks_low_weight




##
#0.1.Gigant components
##
#Components are subsets of a graph that are connected within, 
#but disconnected between sub-graphs.
#Within a component, all actors are connected through paths, 
#but no paths run to points outside the component.
#Isolates within graphs are also regarded as components. 
#The pattern of components of a graph – their number and size – is taken as
#an indication of the opportunities and obstacles to communication 
#or transfer of resources in the associated network.
#(Chan, K., & Liebowitz, J. (2006). The synergy of social network analysis and knowledge mapping, a case study. International Journal of Management and Decision Making, 7(1), 19.)




?edge_connectivity


components(information_or_advice_3_graph)


is.connected(information_or_advice_3_graph, 
             mode='strong')
information_or_advice_3_graph_comp <- components(information_or_advice_3_graph,
                                                 mode = 'strong')
str(information_or_advice_3_graph_comp)
information_or_advice_3_graph_comp$membership
#indentifikovao je 4 komponente, jedna sadrzi 40 cvorova 
#ostale tri sadrze po jedan cvor sto znaci da su to izolati,
#izolovani cvorovi, inače tokom analize mogli smo  zakljuciti da je
#graf u weak modu povezan
not_in_gc_information_or_advice_3_graph <- which(information_or_advice_3_graph_comp$membership != 1)
which(information_or_advice_3_graph_comp$membership != 1)

# create the giant component by removing these five nodes
information_or_advice_3_graph_gcomp <- delete.vertices(information_or_advice_3_graph ,
                             not_in_gc_information_or_advice_3_graph)
summary(information_or_advice_3_graph_gcomp)
is.connected(information_or_advice_3_graph_gcomp, mode='strong')



#1

networks_with_atr_components<-lapply(networks_with_atr,
       components, 
       mode=c("strong"))


networks_with_atr_components

networks_with_atr_components$Value
value_3_graph
not_in_gc_value_3_graph <- which(networks_with_atr_components$Value$membership != 1)
which(networks_with_atr_components$Value$membership != 1)
not_in_gc_value_3_graph
# create the giant component by removing these five nodes
value_3_graph_gcomp <- delete.vertices(value_3_graph,
                                       not_in_gc_value_3_graph)
summary(value_3_graph_gcomp)
is.connected(value_3_graph_gcomp, mode='strong')

summary(value_3_graph_gcomp)
print_all(value_3_graph_gcomp)
V(value_3_graph_gcomp)$name
print_all(value_3_graph)


not_in_gc_advice_3_graph <- which(networks_with_atr_components$Advice$membership != 1)
networks_with_atr_components$Advice$membership 
not_in_gc_advice_3_graph
# create the giant component by removing these five nodes
advice_3_graph_gcomp <- delete.vertices(advice_3_graph,
                                        not_in_gc_advice_3_graph)
summary(advice_3_graph_gcomp)
is.connected(advice_3_graph_gcomp, mode='strong')

summary(advice_3_graph_gcomp)
print_all(advice_3_graph_gcomp)
V(advice_3_graph_gcomp)$name



awareness_3_graph

networks_with_atr_components$Awareness$membership
networks_with_atr_components$Awareness$csize
networks_with_atr_components$Awareness$no

is.connected(awareness_3_graph, mode='strong')

#TRUE

#Graf awareness je jako povezan, ostali grafovi nisu bili
#medjutim pri izbacivanju čvoreva koji nisu deo najveće komponente
#broj čvoreva je uvek bio mali 
#za  value_3_graph samo dva čvora (30 i 24) 
#za advice_3_graph samo 3 čvora(10, 65 i 73)
#za information_or_advice_3_graph samo 3 čvora (16, 37 i 15) 

# Ovo može da se tumači kao u slučaju grafa awerness da je mreža 
#dobro povezana i da postoji vrlo razvijena mreža svesti o ekspertizi
#kolega. Sobzirom da je svest o tome šta neko zna jedan od osnovnih
#parametara da bi se nekom obratili za pomoć, možemo reći da u ovoj
#kompaniji postoji čvrsta osnova za brzu i efikasnu informacionu mrežu
#nekada je važno da prosto pri traženju informacija ne lutamo
#i da dobro razumemo sposobnosti naših kolega kako bi smo na kraju
#brzo i efektivno završili svoj posao i omogućili tako da mi imamo
#više slobodnog vremena, a da u firmi na globalu sa druge strane postoji 
#bolja produktivnost.

#Kod ostalih mreža su  primeceni izolati, medjutim karakteristično
#je da oni u suštini nisu izolovani u potpunosti, oni imaju ustvari 
#sa ostatkom mreže vezu samo u jednom smeru, bilo da imaju
#samo ivice koje uviru u njih ili samo koje izviru.
#Kada bi smo grafove posmatrali kao neusmerene postojala bi potpuna 
#povezanost, Što nije loš pokazatelj. Međutim kada malo preciznije razmislimo 
#ovo je svakako loš znak za te aktere koj su izolovani 
#jer su oni očigledno periferni akteri u ovoj mreži, 
#čije se sposobnosti i mogućnosti ne koriste adekvatno.
#s jedne strane imamo visoke centralne aktere, koji odgovaraju na mnogo 
#pitanja i postavljaju ih vrlo često, a sa druge strane imamo ove aktere
#koji očigledno ne koriste resurse grupe dovoljne, niti grupa korissti njihove mogućnosti.
# Važno je identifikovati ove članove kolektiva i zbog toga što su
#to ljudi koji imaju veće šanse da odustanu i napuste firmu, oni 
#u suštini nisu uspeli da se prilagode radnoj zajednici.
#Kada bi smo pravilno iskoristili i njihove potencijale 
# možemo predpostaviti  da bi mreža bila efikasnije u širenju
#informacija i možemo slobodno reći znanja. 

#1
#
# 1.1 Reachability
#
print_all(information_or_advice_3_graph)
# The subcomponent function allows us to compute reachability for 
# an individual node. For example, reachability for node 1:
subcomponent(information_or_advice_3_graph, 1, mode = 'out')
subcomponent(information_or_advice_3_graph, 15, mode = 'in')#cvor 16
subcomponent(information_or_advice_3_graph, 34, mode = 'in')#cvor 37
subcomponent(information_or_advice_3_graph, 43, mode = 'in')#cvor 15
subcomponent(value_3_graph, 1, mode = 'out')
subcomponent(advice_3_graph, 1, mode = 'in')

# Note that the subcomponent function returns a vector of ids
# of vertices that are in the same component as the given vertex;
# so, the result includes the vertex itself.

# To get graph-wide statistics, I need a loop that will produce
# a matrix with reachability indicators (0/1) for each pair of 
# nodes in the graph

reachability <- function(g, m) {
  reach_mat = matrix(data = 0, nrow = vcount(g), ncol = vcount(g))
  for (i in 1:vcount(g)) {
     reach_mat[i,] = 0
    node_i_reach <- subcomponent(g, i, mode = m)  
    for (alter in node_i_reach) 
      reach_mat[i, alter] = 1
  }
  return(reach_mat)
}

# Let's compare networks with respect to reachability

# Compute reachability matrices for the information_or_advice network,
# first for incoming links, than for outgoing edges:
reach_information_or_advice_3_in <- reachability(information_or_advice_3_graph,
                                                 'in')
View(reach_information_or_advice_3_in)
# It seems that in the information_or_advice network I do not have 
#a perfect reachability.
# Let's check:
all(reach_information_or_advice_3_in == 1)


#than for outgoing edges:
reach_information_or_advice_3_out <- reachability(information_or_advice_3_graph, 
                                                  'out')
?reachability
View(reach_information_or_advice_3_out)
all(reach_information_or_advice_3_out == 1)


## So, in information or advice  network, each node is not reachable 
#from any other node.

# I can further verify that by checking if the information or advice 
#network is a weakly connected network, how we can assume:
is.connected(information_or_advice_3_graph, mode = 'strong')
is.connected(information_or_advice_3_graph, mode = 'weak')


# Know, we can compute, for each node in a graph, the number of other 
# nodes reachable by the given node:
apply(reach_information_or_advice_3_out, 1, function(x) sum(x==1))
apply(reach_information_or_advice_3_in, 1, function(x) sum(x==1))
# As we saw above, every vertex is not reachable from any other vertex
#Preciznije za reach_information_or_advice_3_out skoro sve mogu da dopru
#do 43 druga cvora, osim tri koja mogu samo do 1,
#ovde mogu da vidim a su redni brojevi tih cvorova
#(15,34,43, medjutim id-evi se ne poklapaju 
#te su to ustvari cvorevi 15,16,37)
#to sto mogu da dopru de jednog cvora, znaci da su povezani sami 
#sa sobom, kada je u pitanju reach out( u sustini oni ni od koga ne traze savet )
#za reach_information_or_advice_3_in, dostupno je ili 40 cvorova
#sto je masovono, ili kao kod tri cvora gde je 41 cvor dostupan
#ti cvorovi su opet (15,34,43 medjutim id-evi se ne poklapaju 
#te su to ustvari cvorevi 15,16,37))
V(information_or_advice_3_graph)$name
#1  2  3  4  5  6  7  8  9 10 11 12 13 14 16 17 18 19 20 
#21 22 23 25 26 27 28 29 31 32 33 34 35 36 37 38 39 40 41 42 44 45 46 15

# To better understand these results, I can visualize the network.
# I will use the visNetwork package to create an 
# interactive visualization that allows for focusing on those 
# vertices we are interested in.

# First, prepare the graph data in the format required by visNetwork.
# As a minimum for graph visualization, visNetwork requires:
# - nodes data frame, with 'id' column (plus any additional columns with node attributes)
# - edges data.frame, with 'from' and 'to' columns (plus any additional columns with edge attributes)
V(information_or_advice_3_graph)$name
vis_net_information_or_advice_3_graph<-toVisNetworkData(information_or_advice_3_graph,
                                                        idToLabel = TRUE)
vis_net_information_or_advice_3_graph
# add arrows to display edge direction:
vis_net_information_or_advice_3_graph$edges$arrows<-"to"

# Finally, display the network
vis_net_1_information_or_advice_3_graph <- visNetwork(nodes = vis_net_information_or_advice_3_graph$nodes, 
                              edges = vis_net_information_or_advice_3_graph$edges,
                              width = "100%",
                              main="Information or advice network") 
vis_net_1_information_or_advice_3_graph
# Add highlighting to show reachable nodes
vis_net_1_information_or_advice_3_graph %>% 
  visOptions(highlightNearest = list(enabled=TRUE, degree=2),
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE)


# An average of reach for a network reveals what percent of the network 
# is connected in some way.
# the level of reach is high, 93%:
sum(reach_information_or_advice_3_in)/(vcount(information_or_advice_3_graph)^2)
sum(reach_information_or_advice_3_out)/(vcount(information_or_advice_3_graph)^2)

#U slučaju težinskog grafa, gustina se definiše kao količnik sume
#težina svih konekcija i broja mogućih konekcija u grafu

##
# 1.2 Reachability for value_3_graph network
##
value_3_graph

# The subcomponent function allows us to compute reachability for 
# an individual node. For example, reachability for node 1:
#primeri:
subcomponent(information_or_advice_3_graph, 1, mode = 'out')
subcomponent(value_3_graph, 1, mode = 'out')
subcomponent(value_3_graph, 1, mode = 'in')
subcomponent(advice_3_graph, 1, mode = 'in')

# Note that the subcomponent function returns a vector of ids
# of vertices that are in the same component as the given vertex;
# so, the result includes the vertex itself.


# Let's compare networks with respect to reachability

# Compute reachability matrices for the value_3_graph network,
# first for incoming links, than for outgoing edges:
reach_value_3_in <- reachability(value_3_graph,
                                          'in')
V(value_3_graph)
View(reach_value_3_in)
# It seems that in the value_3_graph network I do not have 
#a perfect reachability.
#(nodes that have reachability in 0 for all nodes are 29 and 46)
# Let's check:
all(reach_value_3_in == 1)

#than for outgoing edges:
reach_value_3_out <- reachability(value_3_graph, 
                                         'out')
View(reach_value_3_out)
all(reach_value_3_out == 1)
which(reach_value_3_out==0)

## So, in information or advice  network, each node is not reachable 
#from any other node.

# I can further verify that by checking if the value_3_graph 
#network is a weakly connected network, how we can assume:
is.connected(value_3_graph, mode = 'strong')
is.connected(value_3_graph, mode = 'weak')
value_3_graph

# Know, we can compute, for each node in a graph, the number of other 
# nodes reachable by the given node:
apply(reach_value_3_out, 1, function(x) sum(x==1))
apply(reach_value_3_in, 1, function(x) sum(x==1))
# As we saw above, every vertex is not reachable from any other vertex
#Preciznije za reach_value_3_out skoro sve mogu da dopru
#do 46 drugih cvoreva, osim dva cvora gde cvor 29(tj.30 zaparvo) moze da dopre do
#2 cvora medjutim to znaci fakticki da moze da dodje do samog sebe
#i uvidom u tableu mozemo da vidimo da je preko reach out povezan
#sa cvorem 46(tj po pravoj numeraciji sa cvorem 24).
#46 moze samo do jednog cvora, do samog sebe
#za  reach_value_3_in, dostupno je ili 44 cvorova
#sto je masovono, ili kao kod dva cvora gde je 45 cvorova dostupno
#ti cvorovi su opet (29,46)-> zapravo cvorevi 30 i 24


# To better understand these results, I can visualize the network.
# I will use the visNetwork package to create an 
# interactive visualization that allows for focusing on those 
# vertices we are interested in.

# First, prepare the graph data in the format required by visNetwork.
# As a minimum for graph visualization, visNetwork requires:
# - nodes data frame, with 'id' column (plus any additional columns with node attributes)
# - edges data.frame, with 'from' and 'to' columns (plus any additional columns with edge attributes)
?order
vis_net_value_3_graph<-toVisNetworkData(value_3_graph,
                                      idToLabel = TRUE)
vis_net_value_3_graph
# add arrows to display edge direction:
vis_net_value_3_graph$edges$arrows<-"to"

# Finally, display the network
vis_net_1_value_3_graph <- visNetwork(nodes = vis_net_value_3_graph$nodes,
                                     edges = vis_net_value_3_graph$edges,
                                    width = "100%",
                                    main="value net") 
vis_net_1_value_3_graph
# Add highlighting to show reachable nodes
vis_net_1_value_3_graph %>% 
  visOptions(highlightNearest = list(enabled=TRUE, degree=2),
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE)


# An average of reach for a network reveals what percent of the network 
# is connected in some way.
# the level of reach is high, 95,79%:
sum(reach_value_3_in)/(vcount(value_3_graph)^2)
sum(reach_value_3_out)/(vcount(value_3_graph)^2)


#1.3 Reachability fo advice network
#

# The subcomponent function allows us to compute reachability for 
# an individual node. For example, reachability for node 1:
subcomponent(information_or_advice_3_graph, 1, mode = 'out')
subcomponent(information_or_advice_3_graph, 15, mode = 'in')#cvor 16
subcomponent(information_or_advice_3_graph, 34, mode = 'in')#cvor 37
subcomponent(information_or_advice_3_graph, 43, mode = 'in')#cvor 15
subcomponent(value_3_graph, 1, mode = 'out')
subcomponent(advice_3_graph, 77, mode = 'in')#cvor 73

# Note that the subcomponent function returns a vector of ids
# of vertices that are in the same component as the given vertex;
# so, the result includes the vertex itself.


# Let's compare networks with respect to reachability

# Compute reachability matrices for the information_or_advice network,
# first for incoming links, than for outgoing edges:
reach_advice_3_in <- reachability(advice_3_graph,
                                            'in')
View(reach_advice_3_in)
# It seems that in the information_or_advice network I do not have 
#a perfect reachability.
# Let's check:
all(reach_advice_3_in == 1)


#than for outgoing edges:
reach_advice_3_out <- reachability(advice_3_graph, 
                                          'out')
?reachability
View(reach_advice_3_out)
all(reach_advice_3_out == 1)


## So, in information or advice  network, each node is not reachable 
#from any other node.

# I can further verify that by checking if the information or advice 
#network is a weakly connected network, how we can assume:
is.connected(advice_3_graph, mode = 'strong')
is.connected(advice_3_graph, mode = 'weak')


# Know, we can compute, for each node in a graph, the number of other 
# nodes reachable by the given node:
apply(reach_advice_3_out, 1, function(x) sum(x==1))
apply(reach_advice_3_in, 1, function(x) sum(x==1))
# As we saw above, every vertex is not reachable from any other vertex
#Preciznije za reach_advice_3_out skoro sve mogu da dopru
#do 77 drugih cvorova, osim tri koja mogu samo do 1,
#ovde mogu da vidim a su redni brojevi tih cvorova
#(10,65,77)-> zapravo pravi id-evi ovih cvorova su 10,65,73
#to sto mogu da dopru de jednog cvora, znaci da su jedino povezani sami 
#sa sobom, kada je u pitanju reach out( u sustini oni ni od koga ne traze savet )
#za reach_advice_3_in, dostupno je ili 74 cvora
#sto je masovono, ili kao kod tri cvora gde je 75 cvorova dostupno
#ti cvorevi su opet (10,65,77 medjutim id-evi se ne poklapaju 
#te su to ustvari cvorevi 10,65,73))


# To better understand these results, I can visualize the network.
# I will use the visNetwork package to create an 
# interactive visualization that allows for focusing on those 
# vertices we are interested in.

# First, prepare the graph data in the format required by visNetwork.
# As a minimum for graph visualization, visNetwork requires:
# - nodes data frame, with 'id' column (plus any additional columns with node attributes)
# - edges data.frame, with 'from' and 'to' columns (plus any additional columns with edge attributes)

vis_net_advice_3_graph<-toVisNetworkData(advice_3_graph,
                                      idToLabel = TRUE)
vis_net_advice_3_graph
# add arrows to display edge direction:
vis_net_advice_3_graph$edges$arrows<-"to"

# Finally, display the network
vis_net_1_advice_3_graph <- visNetwork(nodes = vis_net_advice_3_graph$nodes, 
                                       edges = vis_net_advice_3_graph$edges,
                                       width = "100%",
                                       main="Advice network") 
vis_net_1_advice_3_graph
# Add highlighting to show reachable nodes
vis_net_1_advice_3_graph %>% 
  visOptions(highlightNearest = list(enabled=TRUE, degree=2),
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE)


# An average of reach for a network reveals what percent of the network 
# is connected in some way.
# the level of reach is high, 96%:
sum(reach_advice_3_out)/(vcount(advice_3_graph)^2)
sum(reach_advice_3_in)/(vcount(advice_3_graph)^2)


#1.4 Reachability fo awareness network
#

# The subcomponent function allows us to compute reachability for 
# an individual node. For example, reachability for node 1:
subcomponent(information_or_advice_3_graph, 1, mode = 'out')
subcomponent(information_or_advice_3_graph, 15, mode = 'in')#cvor 16
subcomponent(information_or_advice_3_graph, 34, mode = 'in')#cvor 37
subcomponent(information_or_advice_3_graph, 43, mode = 'in')#cvor 15
subcomponent(value_3_graph, 1, mode = 'out')
subcomponent(advice_3_graph, 77, mode = 'in')#cvor 73
subcomponent(awareness_3_graph, 77, mode = 'in')#cvor 77
# Note that the subcomponent function returns a vector of ids
# of vertices that are in the same component as the given vertex;
# so, the result includes the vertex itself.

# Let's compare networks with respect to reachability

# Compute reachability matrices for the awareness network,
# first for incoming links, than for outgoing edges:
reach_awareness_3_in <- reachability(awareness_3_graph,
                                  'in')
View(reach_awareness_3_in)
# It seems that in the awareness_3_graph network I have 
#a perfect reachability.
# Let's check:
all(reach_awareness_3_in == 1)
#TRUE

#than for outgoing edges:
reach_awareness_3_out <- reachability(awareness_3_graph, 
                                   'out')
?reachability
View(reach_awareness_3_out)
all(reach_awareness_3_out == 1)
#TRUE

## So, in  awareness  network, each node is reachable 
#from any other node.

# I can further verify that by checking if the awareness 
#network is a strongly connected network, how we can assume:
is.connected(awareness_3_graph, mode = 'strong')
#TRUE

# Know, we can compute, for each node in a graph, the number of other 
# nodes reachable by the given node:
apply(reach_awareness_3_out, 1, function(x) sum(x==1))
apply(reach_awareness_3_out, 1, function(x) sum(x==1))
# As we saw above, every vertex is reachable from any other vertex


# To better understand these results, I can visualize the network.
# I will use the visNetwork package to create an 
# interactive visualization that allows for focusing on those 
# vertices we are interested in.

# First, prepare the graph data in the format required by visNetwork.
# As a minimum for graph visualization, visNetwork requires:
# - nodes data frame, with 'id' column (plus any additional columns with node attributes)
# - edges data.frame, with 'from' and 'to' columns (plus any additional columns with edge attributes)

vis_net_awareness_3_graph<-toVisNetworkData(awareness_3_graph,
                                         idToLabel = TRUE)
vis_net_awareness_3_graph
# add arrows to display edge direction:
vis_net_awareness_3_graph$edges$arrows<-"to"

# Finally, display the network
vis_net_1_awareness_3_graph <- visNetwork(nodes = vis_net_awareness_3_graph$nodes, 
                                       edges = vis_net_awareness_3_graph$edges,
                                       width = "100%",
                                       main="Awareness network") 
vis_net_1_awareness_3_graph
# Add highlighting to show reachable nodes
vis_net_1_awareness_3_graph %>% 
  visOptions(highlightNearest = list(enabled=TRUE, degree=2),
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE)


# An average of reach for a network reveals what percent of the network 
# is connected in some way.
# the level of reach is high, accualy it is 100%:
sum(reach_awareness_3_out)/(vcount(awareness_3_graph)^2)
sum(reach_awareness_3_in)/(vcount(awareness_3_graph)^2)



##
#2.1 DENSITY
##
#First, to explore the overall tendency to ask advice,the density of the
#information_or_advice_3_graph network can be calculated for consulting comp.
#Gustina mreže
#ukazuje na stepen povezanosti čvorova mreže
#daje uvid u pojave poput brzine širenja informacija kroz mrezu

#U slučaju težinskog grafa, gustina se definiše kao količnik sume
#težina svih konekcija i broja mogućih konekcija u grafu
is.simple(information_or_advice_3_graph)
is.simple(simplify(information_or_advice_3_graph))
?edge_density
round(edge_density(simplify(information_or_advice_3_graph),
                   loops=TRUE),digits = 3)#0.168
summary(information_or_advice_3_graph)#43 aktera i 312 veza
#link per node moze da se koristi za poredjenje mreza sa razlicitim brojem
#aktera i ustvari predstavlja odnos izmedju broja ivica i broja cvorova
information_or_advice_3_graph
links_per_node_inf_or_advice<-312/43
links_per_node_inf_or_advice#7.25


#2.2.DENSITY
#To explore overall value of expertize for consulting comp, the density
#of value network will be computed
round(edge_density(simplify(value_3_graph),
                   loops=TRUE),digits = 3)#0.377
summary(value_3_graph)
value_3_graph
E(value_3_graph)$weight
links_per_node_value<-801/46
links_per_node_value#17.413

##
#2.3 DENSITY
##
#Know we explore the overall tendency to ask advice in research team in 
#a manufacturing company,the density of the advice_3_graph network can be
#calculated. 
advice_3_graph

round(edge_density(advice_3_graph),digits = 3)#0.129
summary(advice_3_graph)
advice_3_graph
E(advice_3_graph)$weight

links_per_node_advice<-753/77
links_per_node_advice#9.77

##
#2.3 DENSITY
##
#To explore overall value of expertize in research team in a manufacturing
#company, the density of awareness_2_graph network will be computed
awareness_3_graph
#77 nodes and 1842 edges
round(edge_density(awareness_3_graph),digits = 3)#0.315
summary(awareness_3_graph)
E(awareness_3_graph)$weight

links_per_node_awareness<-1842/77
links_per_node_awareness#23.92

links_per_node_advice/links_per_node_awareness*100#40.87%

#DENSITY- function -> edge_density
?edge_density

networks
lapply(networks_with_atr, edge_density)
#$Information_or_advice
#[1] 0.1727575

#$Value
#[1] 0.3869565

#$Advice
#[1] 0.128674

#$Awareness
#[1] 0.3147642


##
#3.1.DEGREE
##
#information or advice network
V(information_or_advice_3_graph)$name
E(information_or_advice_3_graph)$weight
?degree
in_degree_inf_or_adv_3_graph<-degree(information_or_advice_3_graph,
                                     mode=c("in")
                                     ,loops = TRUE)
in_degree_inf_or_adv_3_graph
max(in_degree_inf_or_adv_3_graph)#15
mean(in_degree_inf_or_adv_3_graph)#7.25
median(in_degree_inf_or_adv_3_graph)#6
table(in_degree_inf_or_adv_3_graph)
#Degree out
out_degree_inf_or_adv_3_graph<-degree(information_or_advice_3_graph,
                                    mode=c("out"),
                                    loops = TRUE)
out_degree_inf_or_adv_3_graph
max(out_degree_inf_or_adv_3_graph)#18
mean(out_degree_inf_or_adv_3_graph)#7.25
median(out_degree_inf_or_adv_3_graph)#7
table(out_degree_inf_or_adv_3_graph)
# To better appreciate the computed values,I will visualise them

# First, create a data frame that integrates the computed 
# in and out degree measures for all the nodes
deg_inf_or_advice_3_df <- data.frame(node_id=as.integer(V(information_or_advice_3_graph)$name), 
                            in_degree=in_degree_inf_or_adv_3_graph,
                            out_degree=out_degree_inf_or_adv_3_graph)
head(deg_inf_or_advice_3_df)
tail(deg_inf_or_advice_3_df)

#13 aktera sa najvecom vrednoscu za in degree
Top_in_degree_inf_or_adv_3 <- deg_inf_or_advice_3_df %>% 
  select(node_id,in_degree)%>%
  arrange(desc(deg_inf_or_advice_3_df$in_degree)) %>%
  head(n=13)
View(Top_in_degree_inf_or_adv_3)
#cuvanje df za kasniji prikaz 
write.csv(Top_in_degree_inf_or_adv_3,
          file = 'output/Top_in_degree_inf_or_adv_3.csv')
#13 aktera sa najvecom vrednoscu za out degree
Top_out_degree_inf_or_adv_3 <- deg_inf_or_advice_3_df %>% 
  select(node_id,out_degree)%>%
  arrange(desc(deg_inf_or_advice_3_df$out_degree)) %>%
  head(n=13)
View(Top_out_degree_inf_or_adv_3)
#cuvanje df za kasniji prikaz 
write.csv(Top_out_degree_inf_or_adv_3, file = 'output/Top_out_degree_inf_or_adv_3.csv')


# Then, transform the data frame from wide to long format suitable
# for plotting; to that end, I'll use the pivot_longer() f. from 
# the tidyr package
?pivot_longer
deg_inf_or_advice_3_df_long <- pivot_longer(data = deg_inf_or_advice_3_df, 
                                   cols = in_degree:out_degree,
                                   names_to = 'degree_type', 
                                   names_ptypes = list(degree_type = factor()),
                                   values_to = 'degree_value')
head(deg_inf_or_advice_3_df_long)
tail(deg_inf_or_advice_3_df_long)
deg_inf_or_advice_3_df_long

# I create a barplot plot. 
?png
png("graphs/deg_inf_or_advice.png")
ggplot(data = deg_inf_or_advice_3_df_long, 
       mapping = aes(x=node_id, y=degree_value, fill=degree_type)) +
  geom_col(position = 'dodge') +
  labs(x = "Node ID", y = "In- and Out-Degrees", title = "In and Out Degree  for the Information or advice network") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,48,2)) +
  theme_bw()
dev.off()

# We can also make a plot (histogram) to examine degree distribution
max_degree_inf_or_advice <- max(deg_inf_or_advice_3_df_long$degree_value)
max_degree_inf_or_advice

#pdf("graphs/deg_inf_or_advice_ditribution.pdf")
png("graphs/deg_inf_or_advice_ditribution.png")
ggplot(data = deg_inf_or_advice_3_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  #geom_histogram(bins = 15, position = 'dodge') +
  geom_density(alpha = 0.2) +
  labs(x = "Degree value", title = "Degree distribution for the Information or advice network") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,max_degree_inf_or_advice,1)) +
  theme_bw()
dev.off()

#pdf("graphs/deg_inf_or_advice_ditribution_2.pdf")
png("graphs/deg_inf_or_advice_ditribution_2.png")
ggplot(data = deg_inf_or_advice_3_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  geom_histogram(bins = 15, position = 'dodge') +
  #geom_density(alpha = 0.2) +
  labs(x = "Degree value", title = "Degree distribution for the Information or advice network") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,max_degree_inf_or_advice,1)) +
  theme_bw()
dev.off()


#Sada cu pokusati da izracunam korleaciju izmedju in i out degree centralnosti
#za mrezu information_or_advice
#Korelacija meri JACINU ili STEPEN linearne veze izmedju dve ili vise varijabli
#The indegree–outdegree correlation captures the correlation between the 
#indegree of an actor i and the outdegree of the same actor i. Hence, 
#at a descriptive level we observe similarity between the number of 
#colleagues who approach the focal actor for advice, 
#and the number of colleagues whom a focal actor asks for advice
#Smatra se ako postoji korelacije preko 0.7 da je onda jaka
#srednja 0,3-0,7
#niska <0.3
in_out_deg_inf_or_advice_3 <-deg_inf_or_advice_3_df[,c(2,3)]

in_out_deg_inf_or_advice_3
head(in_out_deg_inf_or_advice_3)
tail(in_out_deg_inf_or_advice_3)
summary(in_out_deg_inf_or_advice_3)


ggqqplot(in_out_deg_inf_or_advice_3$in_degree)
ggqqplot(in_out_deg_inf_or_advice_3$out_degree)
#Pomocu ove vrste ggqqplot-a moguce je proveriti koliko se raspodela
#uklapa u normalnu

ggdensity(in_out_deg_inf_or_advice_3$in_degree, 
          main = "Density plot of in degree",
          xlab = "in degree")
ggdensity(in_out_deg_inf_or_advice_3$out_degree, 
          main = "Density plot of out degree",
          xlab = "out degree")

shapiro.test(in_out_deg_inf_or_advice_3$in_degree)

#data:  in_out_deg_inf_or_advice_2$in_degree
#W = 0.94416, p-value = 0.0366

#nema normalnu raspodelu jer je vrednos p manja od 0,05

shapiro.test(in_out_deg_inf_or_advice_3$out_degree)

#data:  in_out_deg_inf_or_advice_2$out_degree
#W = 0.95956, p-value = 0.1336

#ima normalnu raspodelu

#From the output, the p-value > 0.05 implying that the distribution of the
#data are not significantly different from normal distribution.
#In other words, we can assume the normality.

#Sprovodim spearmanov test korelacije posto i in i out degree nemaju normalnu
#raspodelu
#tj. in degree nema
#pri tome uzorak nije veliki

cor(in_out_deg_inf_or_advice_3,
    use="all.obs",
    method = "pearson")

cor_in_out_deg_inf_or_advice_3<-cor(in_out_deg_inf_or_advice_3,
    use="all.obs",
    method = "spearman")
            #in_degree out_degree
#in_degree  1.0000000  0.8450142
#out_degree 0.8450142  1.0000000
#Svakako rezultati pokazuju jaku korelaciju jer iznosi 0,845>0,7(sve iznad
#0,7 je jaka korelacija)
cor_in_out_deg_inf_or_advice_3
write.csv(cor_in_out_deg_inf_or_advice_3,
          file = 'output/cor_in_out_deg_inf_or_advice_3.csv')




##
#3.2.Degree for Value network in consulting company
##

V(value_3_graph)$name
E(value_3_graph)$weight
is.directed(value_3_graph)
?degree
in_degree_value_3_graph<-degree(value_3_graph,
                               mode=c("in"),
                              loops = TRUE)
in_degree_value_3_graph
max(in_degree_value_3_graph)#34
mean(in_degree_value_3_graph)#17.413
median(in_degree_value_3_graph)#18
table(in_degree_value_3_graph)
#Degree out
out_degree_value_3_graph<-degree(value_3_graph,
                                      mode=c("out"),
                                      loops = TRUE)
out_degree_value_3_graph
max(out_degree_value_3_graph)#45
mean(out_degree_value_3_graph)#17.413
median(out_degree_value_3_graph)#16
table(out_degree_value_3_graph)
# To better appreciate the computed values,I will visualise them

# First, create a data frame that integrates the computed 
# in and out degree measures for all the nodes
deg_value_3_df <- data.frame(node_id=as.integer(V(value_3_graph)$name), 
                                     in_degree=in_degree_value_3_graph,
                                     out_degree=out_degree_value_3_graph)
head(deg_value_3_df)
tail(deg_value_3_df)
#13 aktera sa najvecom vrednoscu za in degree
Top_in_deg_value_3_df <- deg_value_3_df %>% 
  select(node_id,in_degree)%>%
  arrange(desc(deg_value_3_df$in_degree)) %>%
  head(n=13)
View(Top_in_deg_value_3_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_in_deg_value_3_df,
          file = 'output/Top_in_deg_value_3_df.csv')
#13 aktera sa najvecom vrednoscu za out degree
Top_out_deg_value_3_df <- deg_value_3_df %>% 
  select(node_id,out_degree)%>%
  arrange(desc(deg_value_3_df$out_degree)) %>%
  head(n=13)
View(Top_out_deg_value_3_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_out_deg_value_3_df, file = 'output/Top_out_deg_value_3_df.csv')


# Then, transform the data frame from wide to long format suitable
# for plotting; to that end, we'll use the pivot_longer() f. from 
# the tidyr package
?pivot_longer
deg_value_3_df_long <- pivot_longer(data = deg_value_3_df, 
                                            cols = in_degree:out_degree,
                                            names_to = 'degree_type', 
                                            names_ptypes = list(degree_type = factor()),
                                            values_to = 'degree_value')
head(deg_value_3_df_long,10)
tail(deg_value_3_df_long,10)
deg_value_3_df_long

# I create a barplot plot. 
#pdf("graphs/deg_value_2.pdf")
png("graphs/deg_value_2.png")
ggplot(data = deg_value_3_df_long, 
       mapping = aes(x=node_id, y=degree_value, fill=degree_type)) +
  geom_col(position = 'dodge') +
  labs(x = "Node ID", y = "In- and Out-Degrees", title = "In and Out Degree  for Value network") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,48,2)) +
  scale_y_continuous(breaks = seq(0,45,5)) +
  theme_bw()
dev.off()

# We can also make a plot (histogram) to examine degree distribution
max_degree_value_3 <- max(deg_value_3_df_long$degree_value)
max_degree_value_3#45

#pdf("graphs/deg_value_3_ditribution.pdf")
png("graphs/deg_value_3_ditribution.png")
ggplot(data = deg_value_3_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  #geom_histogram(bins = 15, position = 'dodge') +
  geom_density(alpha = 0.2) +
  labs(x = "Degree value", title = "Degree distribution for the Value net.") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,max_degree_value_3,2)) +
  theme_bw()
dev.off()

#pdf("graphs/deg_value_2_ditribution_3.pdf")
png("graphs/deg_value_3_ditribution_2.png")
ggplot(data = deg_value_3_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  geom_histogram(bins = 15, position = 'dodge') +
  #geom_density(alpha = 0.2) +
  labs(x = "Degree value", title = "Degree distribution for the Value net.") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,max_degree_value_3,2)) +
  theme_bw()
dev.off()


#Sada cu pokusati da izracunam korleaciju izmedju in i out degree centralnosti
#za mrezu value_3
#Korelacija meri JACINU ili STEPEN linearne veze izmedju dve ili vise varijabli
#The indegree–outdegree correlation captures the correlation between the 
#indegree of an actor i and the outdegree of the same actor i. Hence, 
#at a descriptive level we observe similarity between the number of 
#colleagues who recognize the focal actor expertize, 
#and the number of colleagues which knowlege  was recognize by the focal actor 
#Smatra se ako postoji korelacije preko 0.7 da je onda jaka
#srednja 0,3-0,7
#niska <0.3
deg_value_3_df
in_out_deg_value_3_df <-deg_value_3_df[,c(2,3)]

in_out_deg_value_3_df
head(in_out_deg_value_3_df)
tail(in_out_deg_value_3_df)
summary(in_out_deg_value_3_df)

#install.packages("ggpubr")
library(ggpubr)
ggqqplot(in_out_deg_value_3_df$in_degree)
ggqqplot(in_out_deg_value_3_df$out_degree)
#Pomocu ove vrste ggqqplot-a moguce je proveriti koliko se raspodela
#uklapa u normalnu

ggdensity(in_out_deg_value_3_df$in_degree, 
          main = "Density plot of in degree",
          xlab = "in degree")
ggdensity(in_out_deg_value_3_df$out_degree, 
          main = "Density plot of out degree",
          xlab = "out degree")

shapiro.test(in_out_deg_value_3_df$in_degree)

#Shapiro-Wilk normality test

#data:  in_out_deg_value_2_df$in_degree
#W = 0.98602, p-value = 0.8483
#IMA normalnu raspodelu jer je vrednos p veca od 0,05

shapiro.test(in_out_deg_value_3_df$out_degree)
#Shapiro-Wilk normality test

#data:  in_out_deg_value_2_df$out_degree
#W = 0.9688, p-value = 0.25


#Ima normalnu raspodelu

#From the output, the p-value > 0.05 implying that the distribution of the
#data are not significantly different from normal distribution.
#In other words, we can assume the normality.

#Sprovodim Pearson-ov test korelacije posto i in i out degree imaju normalnu
#raspodelu

cor_in_out_deg_value_3_df<-cor(in_out_deg_value_3_df,
    use="all.obs",
    method = "pearson")
           #in_degree out_degree
#in_degree  1.0000000  0.6343096
#out_degree 0.6343096  1.0000000
cor_in_out_deg_value_3_df


#Svakako rezultati pokazuju  korelaciju srednje jacine jer iznosi 0.634<0,7
#od 0,3-0,7 smatra se da korleacija ima srednju jacinu


cor_in_out_deg_value_3_df
write.csv(cor_in_out_deg_value_3_df,
          file = 'output/cor_in_out_deg_value_3_df.csv')



##
#3.3.Degree for Advice network in manufacturing company
##

V(advice_3_graph)$name
E(advice_3_graph)$weight
?degree
in_degree_advice_3_graph<-degree(advice_3_graph,
                                mode=c("in"))
in_degree_advice_3_graph
max(in_degree_advice_3_graph)#25
mean(in_degree_advice_3_graph)#9,77
median(in_degree_advice_3_graph)#9
table(in_degree_advice_3_graph)
#Degree out
out_degree_advice_3_graph<-degree(advice_3_graph,
                                 mode=c("out"))
out_degree_advice_3_graph
max(out_degree_advice_3_graph)#32
mean(out_degree_advice_3_graph)#9.77
median(out_degree_advice_3_graph)#8
table(out_degree_advice_3_graph)
# To better appreciate the computed values,I will visualise them

# First, create a data frame that integrates the computed 
# in and out degree measures for all the nodes
deg_advice_3_df <- data.frame(node_id=as.integer(V(advice_3_graph)$name), 
                             in_degree=in_degree_advice_3_graph,
                             out_degree=out_degree_advice_3_graph)
head(deg_advice_3_df)
tail(deg_advice_3_df)



#13 aktera sa najvecom vrednoscu za in degree
Top_in_deg_advice_3_df <- deg_advice_3_df %>% 
  select(node_id,in_degree)%>%
  arrange(desc(deg_advice_3_df$in_degree)) %>%
  head(n=13)
View(Top_in_deg_advice_3_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_in_deg_advice_3_df,
          file = 'output/Top_in_deg_advice_3_df.csv')
#13 aktera sa najvecom vrednoscu za out degree
Top_out_deg_advice_3_df <- deg_advice_3_df %>% 
  select(node_id,out_degree)%>%
  arrange(desc(deg_advice_3_df$out_degree)) %>%
  head(n=13)
View(Top_out_deg_advice_3_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_out_deg_advice_3_df, file = 'output/Top_out_deg_advice_3_df.csv')


# Then, transform the data frame from wide to long format suitable
# for plotting; to that end, we'll use the pivot_longer() f. from 
# the tidyr package
?pivot_longer
deg_advice_3_df_long <- pivot_longer(data = deg_advice_3_df, 
                                    cols = in_degree:out_degree,
                                    names_to = 'degree_type', 
                                    names_ptypes = list(degree_type = factor()),
                                    values_to = 'degree_value')
head(deg_advice_3_df_long,10)
tail(deg_advice_3_df_long,10)
deg_value_2_df_long

# I create a barplot plot. 
#pdf("graphs/deg_advice_3.pdf")
png("graphs/deg_advice_3.png")
ggplot(data = deg_advice_3_df_long, 
       mapping = aes(x=node_id, y=degree_value, fill=degree_type)) +
  geom_col(position = 'dodge') +
  labs(x = "Node ID", y = "In- and Out-Degrees", title = "In and Out Degree  for Advice net") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,78,4)) +
  scale_y_continuous(breaks = seq(0,32,4)) +
  theme_bw()
dev.off()

# We can also make a plot (histogram) to examine degree distribution
max_degree_advice_3 <- max(deg_advice_3_df_long$degree_value)
max_degree_advice_3#32

#pdf("graphs/deg_advice_3_ditribution.pdf")
png("graphs/deg_advice_3_ditribution.png")
ggplot(data = deg_advice_3_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  #geom_histogram(bins = 15, position = 'dodge') +
  geom_density(alpha = 0.2) +
  labs(x = "Degree value", title = "Degree distribution for the Advice net.") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,max_degree_advice_3,2)) +
  theme_bw()
dev.off()

#pdf("graphs/deg_advice_3_ditribution_2.pdf")
png("graphs/deg_advice_3_ditribution_2.png")
ggplot(data = deg_advice_3_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  geom_histogram(bins = 15, position = 'dodge') +
  #geom_density(alpha = 0.2) +
  labs(x = "Degree value", title = "Degree distribution for the Advice net.") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,max_degree_advice_3,2)) +
  theme_bw()
dev.off()


#Sada cu pokusati da izracunam korleaciju izmedju in i out degree centralnosti
#za mrezu advice
#Korelacija meri JACINU ili STEPEN linearne veze izmedju dve ili vise varijabli
#The indegree–outdegree correlation captures the correlation between the 
#indegree of an actor i and the outdegree of the same actor i. Hence, 
#at a descriptive level we observe similarity between the number of 
#colleagues who approach the focal actor for advice, 
#and the number of colleagues whom a focal actor asks for advice
#Smatra se ako postoji korelacije preko 0.7 da je onda jaka
#srednja 0,3-0,7
#niska <0.3
deg_advice_3_df
in_out_deg_advice_3_df <-deg_advice_3_df[,c(2,3)]

in_out_deg_advice_3_df
head(in_out_deg_advice_3_df)
tail(in_out_deg_advice_3_df)
summary(in_out_deg_advice_3_df)

#install.packages("ggpubr")
library(ggpubr)
ggqqplot(in_out_deg_advice_3_df$in_degree)
ggqqplot(in_out_deg_advice_3_df$out_degree)
#Pomocu ove vrste ggqqplot-a moguce je proveriti koliko se raspodela
#uklapa u normalnu

ggdensity(in_out_deg_advice_3_df$in_degree, 
          main = "Density plot of in degree",
          xlab = "in degree")
ggdensity(in_out_deg_advice_3_df$out_degree, 
          main = "Density plot of out degree",
          xlab = "out degree")

shapiro.test(in_out_deg_advice_3_df$in_degree)

#Shapiro-Wilk normality test

#data:  in_out_deg_advice_2_df$in_degree
#W = 0.9245, p-value = 0.0002088
#NEMA normalnu raspodelu jer je vrednos p mnogo manja od 0,05

shapiro.test(in_out_deg_advice_3_df$out_degree)
#Shapiro-Wilk normality test

#data:  in_out_deg_advice_2_df$out_degree
#W = 0.88756, p-value = 5.423e-06

# NEMA normalnu raspodelu, izuzetno niska p vrednost

#From the output, the p-value > 0.05 implying that the distribution of the
#data are not significantly different from normal distribution.
#In other words, we can assume the normality.

#Sprovodim Spearman-ov test korelacije posto i in i out degree nemaju normalnu
#raspodelu

cor_in_out_deg_advice_3_df<-cor(in_out_deg_advice_3_df,
    use="all.obs",
    method = "spearman")
#           in_degree out_degree
#in_degree  1.0000000  0.4274325
#out_degree 0.4274325  1.0000000

#Svakako rezultati pokazuju  korelaciju srednje jacine jer iznosi 0.427<0,7
#od 0,3-0,7 smatra se da korleacija ima srednju jacinu
cor_in_out_deg_advice_3_df
write.csv(cor_in_out_deg_advice_3_df,
          file = 'output/cor_in_out_deg_advice_3_df.csv')

##
#3.4.Degree for Awareness network in manufacturing company
##

V(awareness_3_graph)$name
E(awareness_3_graph)$weight
?degree
in_degree_awareness_3_graph<-degree(awareness_3_graph,
                                mode=c("in"))
in_degree_awareness_3_graph
max(in_degree_awareness_3_graph)#54
mean(in_degree_awareness_3_graph)#23.92
median(in_degree_awareness_3_graph)#24
table(in_degree_awareness_3_graph)
#Degree out
out_degree_awareness_3_graph<-degree(awareness_3_graph,
                                 mode=c("out"))
out_degree_awareness_3_graph
max(out_degree_awareness_3_graph)#54
mean(out_degree_awareness_3_graph)#23.92
median(out_degree_awareness_3_graph)#23
table(out_degree_awareness_3_graph)
# To better appreciate the computed values,I will visualise them

# First, create a data frame that integrates the computed 
# in and out degree measures for all the nodes
deg_awareness_3_df <- data.frame(node_id=as.integer(V(awareness_3_graph)$name), 
                             in_degree=in_degree_awareness_3_graph,
                             out_degree=out_degree_awareness_3_graph)
head(deg_awareness_3_df)
tail(deg_awareness_3_df)


View(deg_awareness_3_df)

#13 aktera sa najvecom vrednoscu za in degree
Top_in_deg_awareness_3_df <- deg_awareness_3_df %>% 
  select(node_id,in_degree)%>%
  arrange(desc(deg_awareness_3_df$in_degree)) %>%
  head(n=13)
View(Top_in_deg_awareness_3_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_in_deg_awareness_3_df,
          file = 'output/Top_in_deg_awareness_3_df.csv')
#13 aktera sa najvecom vrednoscu za out degree
Top_out_deg_awareness_3_df <- deg_awareness_3_df %>% 
  select(node_id,out_degree)%>%
  arrange(desc(deg_awareness_3_df$out_degree)) %>%
  head(n=13)
View(Top_out_deg_awareness_3_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_out_deg_awareness_3_df,
          file = 'output/Top_out_deg_awareness_3_df.csv')





# Then, transform the data frame from wide to long format suitable
# for plotting; to that end, we'll use the pivot_longer() f. from 
# the tidyr package
?pivot_longer
deg_awareness_3_df_long <- pivot_longer(data = deg_awareness_3_df, 
                                    cols = in_degree:out_degree,
                                    names_to = 'degree_type', 
                                    names_ptypes = list(degree_type = factor()),
                                    values_to = 'degree_value')
head(deg_awareness_3_df_long,10)
tail(deg_awareness_3_df_long,10)
deg_awareness_2_df_long

# I create a barplot plot. 
#pdf("graphs/deg_awareness_3.pdf")
png("graphs/deg_awareness_3.png")
ggplot(data = deg_awareness_3_df_long, 
       mapping = aes(x=node_id, y=degree_value, fill=degree_type)) +
  geom_col(position = 'dodge') +
  labs(x = "Node ID", y = "In- and Out-Degrees", title = "In and Out Degree  for Awareness net") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,78,4)) +
  scale_y_continuous(breaks = seq(0,54,4)) +
  theme_bw()
dev.off()

# We can also make a plot (histogram) to examine degree distribution
max_degree_awareness_3 <- max(deg_awareness_3_df_long$degree_value)
max_degree_awareness_3#54

#pdf("graphs/deg_awareness_3_ditribution.pdf")
png("graphs/deg_awareness_3_ditribution.png")
ggplot(data = deg_awareness_3_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  #geom_histogram(bins = 15, position = 'dodge') +
  geom_density(alpha = 0.2) +
  labs(x = "Degree value", title = "Degree distribution for the Awareness net.") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,max_degree_awareness_3,2)) +
  theme_bw()
dev.off()

#pdf("graphs/deg_awareness_3_ditribution.pdf.pdf")
png("graphs/deg_awareness_3_ditribution_2.png")
ggplot(data = deg_awareness_3_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  geom_histogram(bins = 15, position = 'dodge') +
  #geom_density(alpha = 0.2) +
  labs(x = "Degree value", title = "Degree distribution for the Awareness net.") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,max_degree_awareness_3,2)) +
  theme_bw()
dev.off()


#Sada cu pokusati da izracunam korleaciju izmedju in i out degree centralnosti
#za mrezu Awareness
#Korelacija meri JACINU ili STEPEN linearne veze izmedju dve ili vise varijabli
#The indegree–outdegree correlation captures the correlation between the 
#indegree of an actor i and the outdegree of the same actor i. Hence, 
#at a descriptive level we observe similarity between the number of 
#colleagues who recognize the focal actor expertize, 
#and the number of colleagues which knowlege  was recognize by the focal actor 
#Smatra se ako postoji korelacije preko 0.7 da je onda jaka
#srednja 0,3-0,7
#niska <0.3
deg_awareness_3_df
in_out_deg_awareness_3_df<-deg_awareness_3_df[,c(2,3)]

in_out_deg_awareness_3_df
head(in_out_deg_awareness_3_df)
tail(in_out_deg_awareness_3_df)
summary(in_out_deg_awareness_3_df)

#install.packages("ggpubr")
library(ggpubr)
ggqqplot(in_out_deg_awareness_3_df$in_degree)
ggqqplot(in_out_deg_awareness_3_df$out_degree)
#Pomocu ove vrste ggqqplot-a moguce je proveriti koliko se raspodela
#uklapa u normalnu

ggdensity(in_out_deg_awareness_3_df$in_degree, 
          main = "Density plot of in degree",
          xlab = "in degree")
ggdensity(in_out_deg_awareness_3_df$out_degree, 
          main = "Density plot of out degree",
          xlab = "out degree")

shapiro.test(in_out_deg_awareness_3_df$in_degree)

#Shapiro-Wilk normality test

#data:  in_out_deg_awareness_2_df$in_degree
#W = 0.94449, p-value = 0.002144

#NEMA normalnu raspodelu jer je vrednos p manja od 0,05

shapiro.test(in_out_deg_awareness_3_df$out_degree)
#Shapiro-Wilk normality test

#data:  in_out_deg_awareness_2_df$out_degree
#W = 0.97841, p-value = 0.2143

#Ima normalnu raspodelu

#From the output, the p-value > 0.05 implying that the distribution of the
#data are not significantly different from normal distribution.
#In other words, we can assume the normality.

#Sprovodim Spearman-ov test korelacije posto i in i out degree nemaju normalnu
#raspodelu
#tacnije nema in degree

cor_in_out_deg_awareness_3_df<-cor(in_out_deg_awareness_3_df,
    use="all.obs",
    method = "spearman")
#in_degree out_degree
#in_degree   1.000000   0.820536
#out_degree  0.820536   1.000000

#Svakako rezultati pokazuju  korelaciju vrlo jaka jer iznosi 0.820>0,7
#sve preko 0,7 smatra se da postoji jaka korelacija

cor_in_out_deg_awareness_3_df
write.csv(cor_in_out_deg_awareness_3_df,
          file = 'output/cor_in_out_deg_awareness_3_df.csv')

##
#### 4.1. Shortest path (geodesic) 
##
##geodezic- najkraca putanja i 
##diameter-  najduzi put od svih najkracih putanja

#Short distances transmit information accurately and in a timely way, 
#while long distances transmit slowly and can distort the information.

#Information or advice, shortest paths to each vertex
information_or_advice_3_graph
sp_information_or_advice_3_graph_in <- distances(information_or_advice_3_graph,
                                                 mode='in',
                                                 weights =NaN)
View(sp_information_or_advice_3_graph_in)

#Information or advice, shortest paths from each vertex
sp_information_or_advice_3_graph_out <- distances(information_or_advice_3_graph,
                                                  mode='out',
                                                  weights = NaN)
sp_information_or_advice_3_graph_out
View(sp_information_or_advice_3_graph_out)
#inf vrednosti nema mnogo, samo za tri cvora ne postoje konekcije(15,16,37) 
#Zamena Inf values sa NA
sp_information_or_advice_3_graph_in[is.infinite(sp_information_or_advice_3_graph_in)] <- NA
sp_information_or_advice_3_graph_in
sp_information_or_advice_3_graph_out[is.infinite(sp_information_or_advice_3_graph_out)] <- NA
View(sp_information_or_advice_3_graph_out)


#Then, I can compute average shortest path for each node in the 
# information_or_advice network
mean_sp_information_or_advice_3_graph_in <- apply(sp_information_or_advice_3_graph_in,
                                                  1,
                                       function(x) mean(x, na.rm = TRUE))
mean_sp_information_or_advice_3_graph_out <- apply(sp_information_or_advice_3_graph_out,
                                                   1,
                                        function(x) mean(x, na.rm = TRUE))
summary(mean_sp_information_or_advice_3_graph_in)
summary(mean_sp_information_or_advice_3_graph_out)
mean_sp_information_or_advice_3_graph_out
mean_sp_information_or_advice_3_graph_in
View(mean_sp_information_or_advice_3_graph_out)
View(mean_sp_information_or_advice_3_graph_in)

##
#4.1.1.APL
##
#Average path length
#
#Može se desiti da mreža sadrži par čvorova koji su značajno
#više međusobno udaljeni nego što je to slučaj sa ostalim
#parovima čvorova (outliers)
#U tom slučaju, umesto diametra, bolji indikator udaljenosti
#čvorova u mreži je prosečna dužina putanja u mreži
#(Average Path Length - APL)
#APL se definiše kao prosečna dužina najkraćih putanja
#između svaka dva čvora u mreži
mean_distance
is.connected(information_or_advice_3_graph, mode="strong")
#Dva nacina za izracunavanje
#1.
mean_distance(information_or_advice_3_graph,
              directed = TRUE,
              unconnected = TRUE)
#2.
average.path.length(information_or_advice_3_graph,
                    directed = TRUE,
                    unconnected = TRUE)
#2.394643
average.path.length(information_or_advice_3_graph_gcomp,
                    directed =TRUE,
                    unconnected = FALSE)

#2.34
diameter(information_or_advice_3_graph)
farthest_vertices(information_or_advice_3_graph)
most_apart_information_or_advice_3<- farthest_vertices(information_or_advice_3_graph)
# Take the vertices that are most apart:
most_apart_information_or_advice_3 <- farthest_vertices(information_or_advice_3_graph)$vertices

# Get the shortest path (sequence of edges) that connect the two vertices
# that are most apart:
most_apart_path_information_or_advice_3 <- shortest_paths(information_or_advice_3_graph, 
                                  from = most_apart_information_or_advice_3[1], 
                                  to = most_apart_information_or_advice_3[2], 
                                  output = 'epath')
most_apart_path_information_or_advice_3
most_apart_path_information_or_advice_3 <- most_apart_path_information_or_advice_3$epath[[1]]

# Let's plot the two most apart nodes and the shortest path that 
# connects them. 
# Define node color so that all the nodes are gold except the 
# two most distant ones which will be painted in red 
node_colors <- rep('gold', times=vcount(information_or_advice_3_graph))
node_colors[most_apart_information_or_advice_3] <- 'red3'
# Define edge color so that all edges are grey except those that
# connect the two most distant nodes - these will be red
edge_colors <- rep('grey', times=ecount(information_or_advice_3_graph))
edge_colors[most_apart_path_information_or_advice_3] <- 'red3'
# Define edge width (thickness) so that the width of all edges 
# is 1 except those that connect the two most distant nodes - their
# width will be 3.5
edge_width <- rep(0.03, times=ecount(information_or_advice_3_graph))
edge_width[most_apart_path_information_or_advice_3] <- 3.5
# Now, plot the network
png("graphs/longest_geosecic_in_information_or_advice_3_graph.png",
    width = 1100,
    height = 1100)
plot(information_or_advice_3_graph, 
     layout=layout_with_lgl(information_or_advice_3_graph), 
     vertex.color=node_colors, 
     edge.color=edge_colors,
     edge.width=edge_width,
     edge.arrow.size=.5,
     main="The longest geodesic in the information_or_advice_3_graph network")
dev.off()

# Let's visualize these values to better understand them
# First, prepare the data
mean_sp_information_or_advice_3_df <- data.frame(node_id=as.integer(V(information_or_advice_3_graph)$name),
                        sp_in = mean_sp_information_or_advice_3_graph_in,
                        sp_out = mean_sp_information_or_advice_3_graph_out)
mean_sp_information_or_advice_3_df


#13 aktera sa najvecom vrednoscu za shortest paths
Top_in_mean_sp_information_or_advice_3_df <- mean_sp_information_or_advice_3_df %>% 
  select(node_id,sp_in)%>%
  arrange(desc(mean_sp_information_or_advice_3_df$sp_in)) %>%
  tail(n=13)
View(Top_in_mean_sp_information_or_advice_3_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_in_mean_sp_information_or_advice_3_df,
          file = 'output/Top_in_mean_sp_information_or_advice_3_df.csv')
#13 aktera sa najvecom vrednoscu za shortest paths
Top_out_mean_sp_information_or_advice_3_df <- mean_sp_information_or_advice_3_df %>% 
  select(node_id,sp_out)%>%
  arrange(desc(mean_sp_information_or_advice_3_df$sp_out)) %>%
  tail(n=13)
View(Top_out_mean_sp_information_or_advice_3_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_out_mean_sp_information_or_advice_3_df,
          file = 'output/Top_out_mean_sp_information_or_advice_3_df.csv')



mean_sp_information_or_advice_3_df_longer <- pivot_longer(data = mean_sp_information_or_advice_3_df, 
                                           cols = starts_with("sp"),
                                           names_to = "mode",
                                           names_ptypes = list(Mode=factor()),
                                           values_to = "SP")
View(mean_sp_information_or_advice_3_df_longer)
head(mean_sp_information_or_advice_3_df_longer)
# Then, make a plot (bar plot)
#pdf("graphs/mean_sp_information_or_advice_3.pdf")
png("graphs/mean_sp_information_or_advice_3.png")
ggplot(data = mean_sp_information_or_advice_3_df_longer,
       mapping = aes(x = node_id, y = SP, fill=mode)) +
  geom_col(position = 'dodge') +
  scale_fill_discrete(name='Kind of shortest path (SP)',
                      # breaks=c('sp_in', 'sp_out'),
                      labels=c('SP to the vertex', 'SP from the vertex')) +
  labs(x = 'Node ID', y = "Average Shortest Path") +
  scale_x_continuous(breaks = seq(1,46,1)) +
  scale_y_continuous(breaks = seq(1,16,2)) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  coord_flip()
  dev.off()
  
  
  
  
  
  ##
  #### 4.2. Shortest path (geodesic)  
  ##
  ##geodezic- najkraca putanja i 
  ##diameter-  najduzi put od svih najkracih putanja
  
  #Short distances transmit information accurately and in a timely way, 
  #while long distances transmit slowly and can distort the information.
  
  #Value network, shortest paths to each vertex
  value_3_graph
  sp_value_3_graph_in <- distances(value_3_graph,
                                      mode='in',
                                      weights = NULL)
  View(sp_value_3_graph_in)
  
  #Information or advice, shortest paths from each vertex
  sp_value_3_graph_out <- distances(value_3_graph,
                                      mode='out',
                                      weights = NULL)
  sp_value_3_graph_out
  View(sp_value_3_graph_out)
  #inf vrednosti nema mnogo, samo za dva cvora ne postoje konekcije(30,24) 
  #Zamena Inf values sa NA
  sp_value_3_graph_in[is.infinite(sp_value_3_graph_in)] <- NA
  sp_information_or_advice_3_graph_in
  sp_value_3_graph_out[is.infinite(sp_value_3_graph_out)] <- NA
  View(sp_value_3_graph_out)
  
  
  #Then, I can compute average shortest path for each node in the 
  # sp_value_3_graph_in
  mean_sp_value_3_graph_in<- apply(sp_value_3_graph_in,
                                      1,
                                    function(x) mean(x, na.rm = TRUE))
  mean_sp_value_3_graph_out <- apply(sp_value_3_graph_out,
                                        1,
                                       function(x) mean(x, na.rm = TRUE))
  summary(mean_sp_value_3_graph_in)
  summary(mean_sp_value_3_graph_out)
  mean_sp_value_3_graph_out
  mean_sp_value_3_graph_in
  View(mean_sp_value_3_graph_out)
  View(mean_sp_value_3_graph_in)
  
  ##
  #4.2.1.APL
  ##
  #average.path.length
  #DIAMETAR I PROSEČNA DUŽINA PUTANJE
  #Može se desiti da mreža sadrži par čvorova koji su značajno
  #više međusobno udaljeni nego što je to slučaj sa ostalim
  #parovima čvorova (outliers)
  #U tom slučaju, umesto diametra, bolji indikator udaljenosti
  #čvorova u mreži je prosečna dužina putanja u mreži
  #(Average Path Length - APL)
  #APL se definiše kao prosečna dužina najkraćih putanja
  #između svaka dva čvora u mreži
  mean_distance
  is.connected(value_3_graph, mode="strong")
  #1.
  mean_distance(value_3_graph,
                directed = TRUE,
                unconnected = TRUE)
  #2.
  average.path.length(value_3_graph,
                      directed = TRUE,
                      unconnected = TRUE)
  #1.679
  diameter(value_3_graph)
  most_apart_value_3<- farthest_vertices(value_3_graph)
  # Take the vertices that are most apart:
  most_apart_value_3 <- farthest_vertices(value_3_graph)$vertices
  
  # Get the shortest path (sequence of edges) that connect the two vertices
  # that are most apart:
  most_apart_path <- shortest_paths(value_3_graph, 
                                    from = most_apart_value_3[1], 
                                    to = most_apart_value_3[2], 
                                    output = 'epath')
  most_apart_path
  most_apart_path <- most_apart_path$epath[[1]]
  
  # Let's plot the two most apart nodes and the shortest path that 
  # connects them. 
  # Define node color so that all the nodes are gold except the 
  # two most distant ones which will be painted in red 
  node_colors <- rep('gold', times=vcount(value_3_graph))
  node_colors[most_apart_value_3] <- 'red3'
  # Define edge color so that all edges are grey except those that
  # connect the two most distant nodes - these will be red
  edge_colors <- rep('grey', times=ecount(value_3_graph))
  edge_colors[most_apart_path] <- 'red3'
  # Define edge width (thickness) so that the width of all edges 
  # is 1 except those that connect the two most distant nodes - their
  # width will be 3.5
  edge_width <- rep(0.03, times=ecount(value_3_graph))
  edge_width[most_apart_path] <- 3.5
  # Now, plot the network
  png("graphs/longest_geosecic_in_value_3_graph.png",
      width = 1100,
      height = 1100)
  plot(value_3_graph, 
       layout=layout_with_lgl(value_3_graph), 
       vertex.color=node_colors, 
       edge.color=edge_colors,
       edge.width=edge_width,
       edge.arrow.size=.5,
       main="The longest geodesic in the value_3_graph network")
  dev.off()
  
  # Let's visualize these values to better understand them
  # First, prepare the data
 V(value_3_graph)$name
 mean_sp_value_3_graph_in
  mean_sp_value_3_df <- data.frame(node_id=as.integer(V(value_3_graph)$name),
                          sp_in = mean_sp_value_3_graph_in,
                          sp_out = mean_sp_value_3_graph_out)
  mean_sp_value_3_df
  
  
  #13 aktera sa najvecom vrednoscu za shortest paths
  Top_in_mean_sp_value_3_df <- mean_sp_value_3_df %>% 
    select(node_id,sp_in)%>%
    arrange(desc(mean_sp_value_3_df$sp_in)) %>%
    tail(n=13)
  View(Top_in_mean_sp_value_3_df)
  #cuvanje df za kasniji prikaz 
  write.csv(Top_in_mean_sp_value_3_df,
            file = 'output/Top_in_mean_sp_value_3_df.csv')
  #13 aktera sa najvecom vrednoscu za shortest paths
  Top_out_mean_sp_value_3_df <- mean_sp_value_3_df %>% 
    select(node_id,sp_out)%>%
    arrange(desc(mean_sp_value_3_df$sp_out)) %>%
    tail(n=13)
  View(Top_out_mean_sp_value_3_df)
  #cuvanje df za kasniji prikaz 
  write.csv(Top_out_mean_sp_value_3_df,
            file = 'output/Top_out_mean_sp_value_3_df.csv')
  
  
  
  
  mean_sp_value_3_df_longer <- pivot_longer(data = mean_sp_value_3_df, 
                                              cols = starts_with("sp"),
                                              names_to = "mode",
                                              names_ptypes = list(Mode=factor()),
                                              values_to = "SP")
  View(mean_sp_value_3_df_longer)
  head(mean_sp_value_3_df_longer)
  # Then, make a plot (bar plot)
  #pdf("graphs/mean_sp_value_3.pdf")
  png("graphs/mean_sp_value_3.png")
  ggplot(data = mean_sp_value_3_df_longer,
         mapping = aes(x = node_id, y = SP, fill=mode)) +
    geom_col(position = 'dodge') +
    scale_fill_discrete(name='Kind of shortest path (SP)',
                        breaks=c('sp_in', 'sp_out'),
                        labels=c('SP to the vertex', 'SP from the vertex')) +
    labs(x = 'Node ID', y = "Average Shortest Path", title = "Mean shortest path for Value net") +
    scale_x_continuous(breaks = seq(1,48,2)) +
    scale_y_continuous(breaks = seq(1,9,1)) +
    theme_bw() +
    theme(legend.position = 'bottom') +
    coord_flip()
  dev.off()  
  
  
##
#### 4.3. Shortest path (geodesic) 
##
##geodezic- najkraca putanja i 
##diameter-  najduzi put od svih najkracih putanja
  
#Short distances transmit information accurately and in a timely way, 
#while long distances transmit slowly and can distort the information.
  
#Advice network, shortest paths to each vertex
  advice_3_graph
  sp_advice_3_graph_in <- distances(advice_3_graph,
                                   mode='in',
                                   weights = NULL)
  View(sp_advice_3_graph_in)
  
  #Information or advice, shortest paths from each vertex
  sp_advice_3_graph_out <- distances(advice_3_graph,
                                    mode='out',
                                    weights = NULL)
  sp_advice_3_graph_out
  View(sp_advice_3_graph_out)
  #inf vrednosti nema mnogo, samo za tri cvora ne postoje konekcije(10,65,73) 
  #Zamena Inf values sa NA
  sp_advice_3_graph_in[is.infinite(sp_advice_3_graph_in)] <- NA
  sp_advice_3_graph_in
  sp_advice_3_graph_out[is.infinite(sp_advice_3_graph_out)] <- NA
  View(sp_advice_3_graph_out)
  
  
  #Then, I can compute average shortest path for each node in the 
  # sp_value_3_graph_in
  mean_sp_advice_3_graph_in<- apply(sp_advice_3_graph_in,
                                   1,
                                   function(x) mean(x, na.rm = TRUE))
  mean_sp_advice_3_graph_out <- apply(sp_advice_3_graph_out,
                                     1,
                                     function(x) mean(x, na.rm = TRUE))
  summary(mean_sp_advice_3_graph_in)
  summary(mean_sp_advice_3_graph_out)
  mean_sp_advice_3_graph_out
  mean_sp_advice_3_graph_in
  View(mean_sp_advice_3_graph_out)
  View(mean_sp_advice_3_graph_in)
  
  ##
  #4.3.1.APL
  ##
  #average.path.length
  #DIAMETAR I PROSEČNA DUŽINA PUTANJE
  #Može se desiti da mreža sadrži par čvorova koji su značajno
  #više međusobno udaljeni nego što je to slučaj sa ostalim
  #parovima čvorova (outliers)
  #U tom slučaju, umesto diametra, bolji indikator udaljenosti
  #čvorova u mreži je prosečna dužina putanja u mreži
  #(Average Path Length - APL)
  #APL se definiše kao prosečna dužina najkraćih putanja
  #između svaka dva čvora u mreži
  mean_distance
  
  is.connected(advice_3_graph, mode="strong")
  
  mean_distance(advice_3_graph,
                directed = TRUE,
                unconnected = TRUE)
  
  average.path.length(advice_3_graph,
                      directed = TRUE,
                      unconnected = TRUE)
                         #2.445
  
  diameter(advice_3_graph)
  farthest_vertices(advice_3_graph)
  most_apart_advice_3<- farthest_vertices(advice_3_graph)
  # Take the vertices that are most apart:
  most_apart_advice_3<- farthest_vertices(advice_3_graph)$vertices
  most_apart_advice_3
  # Get the shortest path (sequence of edges) that connect the two vertices
  # that are most apart:
  most_apart_path_advice_3 <- shortest_paths(advice_3_graph, 
                                            from = most_apart_advice_3[1], 
                                            to = most_apart_advice_3[2], 
                                            output = 'epath')
  most_apart_path_advice_3
  most_apart_path_advice_3 <- most_apart_path_advice_3$epath[[1]]
  
  # Let's plot the two most apart nodes and the shortest path that 
  # connects them. 
  # Define node color so that all the nodes are gold except the 
  # two most distant ones which will be painted in red 
  node_colors <- rep('gold', times=vcount(advice_3_graph))
  node_colors[most_apart_advice_3] <- 'red3'
  # Define edge color so that all edges are grey except those that
  # connect the two most distant nodes - these will be red
  edge_colors <- rep('grey', times=ecount(advice_3_graph))
  edge_colors[most_apart_path_advice_3] <- 'red3'
  # Define edge width (thickness) so that the width of all edges 
  # is 1 except those that connect the two most distant nodes - their
  # width will be 3.5
  edge_width <- rep(0.03, times=ecount(advice_3_graph))
  edge_width[most_apart_path_advice_3] <- 3.5
  # Now, plot the network
  png("graphs/longest_geosecic_in_advice_3_graph.png",
      width = 1100,
      height = 1100)
  plot(advice_3_graph, 
       layout=layout_with_lgl(advice_3_graph), 
       vertex.color=node_colors, 
       edge.color=edge_colors,
       edge.width=edge_width,
       edge.arrow.size=.3,
       main="The longest geodesic in the advice_3_graph network")
  dev.off()
  
  
  
  # Let's visualize these values to better understand them
  # First, prepare the data
  V(advice_3_graph)$name
  mean_sp_advice_3_graph_in
  mean_sp_advice_3_df <- data.frame(node_id=as.integer(V(advice_3_graph)$name),
                                   sp_in = mean_sp_advice_3_graph_in,
                                   sp_out = mean_sp_advice_3_graph_out)
  View(mean_sp_advice_3_df)
  
  
  #13 aktera sa najvecom vrednoscu za shortest paths
  Top_in_mean_sp_advice_3_df <- mean_sp_advice_3_df %>% 
    select(node_id,sp_in)%>%
    arrange(desc(mean_sp_advice_3_df$sp_in)) %>%
    tail(n=13)
  View(Top_in_mean_sp_advice_3_df)
  #cuvanje df za kasniji prikaz 
  write.csv(Top_in_mean_sp_advice_3_df,
            file = 'output/Top_in_mean_sp_advice_3_df.csv')
  #13 aktera sa najvecom vrednoscu za shortest paths
  Top_out_mean_sp_advice_3_df <- mean_sp_advice_3_df %>% 
    select(node_id,sp_out)%>%
    arrange(desc(mean_sp_advice_3_df$sp_out)) %>%
    tail(n=13)
  View(Top_out_mean_sp_advice_3_df)
  #cuvanje df za kasniji prikaz 
  write.csv(Top_out_mean_sp_advice_3_df,
            file = 'output/Top_out_mean_sp_advice_3_df.csv')
  
  
  
  
  mean_sp_advice_3_df_longer <- pivot_longer(data = mean_sp_advice_3_df, 
                                            cols = starts_with("sp"),
                                            names_to = "mode",
                                            names_ptypes = list(Mode=factor()),
                                            values_to = "SP")
  View(mean_sp_advice_3_df_longer)
  head(mean_sp_advice_3_df_longer)
  
  
  # Then, make a plot (bar plot)
  #pdf("graphs/mean_sp_advice_3.pdf")
  png("graphs/mean_sp_advice_3.png")
  ggplot(data = mean_sp_advice_3_df_longer,
         mapping = aes(x = node_id, y = SP, fill=mode)) +
    geom_col(position = 'dodge') +
    scale_fill_discrete(name='Kind of shortest path (SP)',
                        breaks=c('sp_in', 'sp_out'),
                        labels=c('SP to the vertex', 'SP from the vertex')) +
    labs(x = 'Node ID', y = "Average Shortest Path", title = "Mean shortest path for Advice net") +
    scale_x_continuous(breaks = seq(1,77,4)) +
    scale_y_continuous(breaks = seq(1,22,2)) +
    theme_bw() +
    theme(legend.position = 'bottom') +
    coord_flip()
  dev.off()  
  
  
  ##
  #### 4.4. Shortest path (geodesic) 
  ##
  ##geodezic- najkraca putanja i 
  ##diameter-  najduzi put od svih najkracih putanja
  
  #Short distances transmit information accurately and in a timely way, 
  #while long distances transmit slowly and can distort the information.
  #U slučaju težinskih grafova, interpretacija najkraće putanje je
  #drugačija i zavisi od toga šta težine konekcija predstavljaju
  #Najkraća putanja je ona sa najmanjom ukupnom težinom
  
  
  #Awareness network, shortest paths to each vertex
  awareness_3_graph
  sp_awareness_3_graph_in <- distances(awareness_3_graph,
                                    mode='in',
                                    weights = NULL)
  View(sp_awareness_3_graph_in)
  
  #Information or advice, shortest paths from each vertex
  sp_awareness_3_graph_out <- distances(awareness_3_graph,
                                     mode='out',
                                     weights = NULL)
  sp_awareness_3_graph_out
  View(sp_awareness_3_graph_out)
  #inf vrednosti nema 
  
  #Then, I can compute average shortest path for each node in the 

  mean_sp_awareness_3_graph_in<- apply(sp_awareness_3_graph_in,
                                    1,
                                    function(x) mean(x, na.rm = TRUE))
  mean_sp_awareness_3_graph_out <- apply(sp_awareness_3_graph_out,
                                      1,
                                      function(x) mean(x, na.rm = TRUE))
  summary(mean_sp_awareness_3_graph_in)
  summary(mean_sp_awareness_3_graph_out)
  mean_sp_awareness_3_graph_in
  mean_sp_awareness_3_graph_out
  View(mean_sp_awareness_3_graph_in)
  View(mean_sp_awareness_3_graph_out)
  
  ##
  #4.4.1.APL
  ##
  #average.path.length
  #DIAMETAR I PROSEČNA DUŽINA PUTANJE
  #Može se desiti da mreža sadrži par čvorova koji su značajno
  #više međusobno udaljeni nego što je to slučaj sa ostalim
  #parovima čvorova (outliers)
  #U tom slučaju, umesto diametra, bolji indikator udaljenosti
  #čvorova u mreži je prosečna dužina putanja u mreži
  #(Average Path Length - APL)
  #APL se definiše kao prosečna dužina najkraćih putanja
  #između svaka dva čvora u mreži
  mean_distance
  is.connected(awareness_3_graph, mode="strong")
  mean_distance(awareness_3_graph,
                directed = TRUE,
                unconnected = TRUE)
  average.path.length(awareness_3_graph,
                      directed = TRUE,
                      unconnected = TRUE)
  #1.780
  
  #Diametar pokazuje koliko je “koraka” u mreži najviše potrebno napraviti 
  #da bi se polazeći od  bilo kog čvora stiglo do bilo kog drugog čvora mreže
  diameter(awareness_3_graph,
           directed = TRUE,
           weights = NaN)
  #3
  diameter(awareness_3_graph,
           directed = TRUE,
           weights = NULL)
  #15
  
  farthest_vertices(awareness_3_graph,
                    directed = TRUE,
                    weights=NaN)
  
  farthest_vertices(awareness_3_graph,
                    directed = TRUE,
                    weights=NULL)
  #najduzi put od svih najkracih puteva iznosi 3
  #izmedju cvora 53 i 32.
  #Sve ove metrice, shortest path, APL i diametar govore koliko je ustvari
  #struktrura informacione mreze u kompaniji razvijena
  # Take the vertices that are most apart:
  most_apart_awareness_3 <- farthest_vertices(awareness_3_graph,
                                             weights = NULL)
  most_apart_awareness_3<-most_apart_awareness_3$vertices
  most_apart_awareness_3
  # Get the shortest path (sequence of edges) that connect the two vertices
  # that are most apart:
  most_apart_path_awareness_3 <- shortest_paths(awareness_3_graph, 
                                               from = most_apart_awareness_3[1], 
                                               to = most_apart_awareness_3[2], 
                                               output = 'epath')
  
  most_apart_path_awareness_3
  most_apart_path_awareness_3 <- most_apart_path_awareness_3$epath[[1]]
  most_apart_path_awareness_3
  #53->56 56->29 29->32
  # Let's plot the two most apart nodes and the shortest path that 
  # connects them. 
  # Define node color so that all the nodes are gold except the 
  # two most distant ones which will be painted in red 
  node_colors_awareness_3<- rep('gold', times=vcount(awareness_3_graph))
  node_colors_awareness_3[most_apart_awareness_3] <- 'red3'
  # Define edge color so that all edges are grey except those that
  # connect the two most distant nodes - these will be red
  edge_colors_awareness_3 <- rep('grey', times=ecount(awareness_3_graph))
  edge_colors_awareness_3[most_apart_path_awareness_3] <- 'red3'
  # Define edge width (thickness) so that the width of all edges 
  # is 1 except those that connect the two most distant nodes - their
  # width will be 3.5
  edge_width_awareness_3 <- rep(1, times=ecount(awareness_3_graph))
  edge_width_awareness_3[most_apart_path_awareness_3] <- 3.5
  # Now, plot the network
  #pdf("graphs/longest_geodesic_awareness_3_graph.pdf")
  
  png("graphs/longest_geodesic_awareness_3_graph.png",
      width = 1100,
      height = 1100)
  plot(awareness_3_graph, 
       layout=layout_with_lgl(awareness_3_graph), 
       vertex.color=node_colors_awareness_3,
       vertex.frame.color=node_colors_awareness_3,
       edge.color=edge_colors_awareness_3,
       edge.width=edge_width_awareness_3,
       vertex.size=10,
       edge.arrow.size=.15,
       main="The longest geodesic in the awareness_3_graph network")
  dev.off()
  
  
  
  # Let's visualize these values to better understand them
  # First, prepare the data
  V(awareness_3_graph)$name
  mean_sp_awareness_3_graph_in
  mean_sp_awareness_3_df <- data.frame(node_id=as.integer(V(awareness_3_graph)$name),
                                    sp_in = mean_sp_awareness_3_graph_in,
                                    sp_out = mean_sp_awareness_3_graph_out)
  View(mean_sp_awareness_3_df)
  
  
  #13 aktera sa najvecom vrednoscu za shortest paths
  Top_in_mean_sp_awareness_3_df <- mean_sp_awareness_3_df %>% 
    select(node_id,sp_in)%>%
    arrange(desc(mean_sp_awareness_3_df$sp_in)) %>%
    tail(n=13)
  View(Top_in_mean_sp_awareness_3_df)
  #cuvanje df za kasniji prikaz 
  write.csv(Top_in_mean_sp_awareness_3_df,
            file = 'output/Top_in_mean_sp_awareness_3_df.csv')
  #13 aktera sa najvecom vrednoscu za shortest paths
  Top_out_mean_sp_awareness_3_df <- mean_sp_awareness_3_df %>% 
    select(node_id,sp_out)%>%
    arrange(desc(mean_sp_awareness_3_df$sp_out)) %>%
    tail(n=13)
  View(Top_out_mean_sp_awareness_3_df )
  #cuvanje df za kasniji prikaz 
  write.csv(Top_out_mean_sp_awareness_3_df,
            file = 'output/Top_out_mean_sp_awareness_3_df.csv')
  
  
  
  
  mean_sp_awareness_3_df_longer <- pivot_longer(data = mean_sp_awareness_3_df, 
                                             cols = starts_with("sp"),
                                             names_to = "mode",
                                             names_ptypes = list(Mode=factor()),
                                             values_to = "SP")
  View(mean_sp_awareness_3_df_longer)
  head(mean_sp_awareness_3_df_longer)
  # Then, make a plot (bar plot)
  #pdf("graphs/mean_sp_advice_3.pdf")
  png("graphs/mean_sp_awareness_3.png")
  ggplot(data = mean_sp_awareness_3_df_longer,
         mapping = aes(x = node_id, y = SP, fill=mode)) +
    geom_col(position = 'dodge') +
    scale_fill_discrete(name='Kind of shortest path (SP)',
                        # breaks=c('sp_in', 'sp_out'),
                        labels=c('SP to the vertex', 'SP from the vertex')) +
    labs(x = 'Node ID', y = "Average Shortest Path") +
    scale_x_continuous(breaks = seq(1,77,4)) +
    scale_y_continuous(breaks = seq(1,22,2)) +
    theme_bw() +
    theme(legend.position = 'bottom') +
    coord_flip()
  dev.off()  
  
  
  

#6.ECCENTRICITY 
#Ova metrika pokazuje koliko je dati akter (čvor) udaljen od
#njemu najudaljenijeg aktera u mreži
#Za datog aktera, eccentricity predstavlja najdužu najkraću putanju 
#do ostalih čvorova u mreži,
eccentricity(information_or_advice_3_graph, mode=c("in"))
eccentricity(information_or_advice_3_graph, mode=c("out"))

is.connected(information_or_advice_3_graph)

in_eccentricity_networks_with_atr<-lapply(networks_with_atr,
                                                eccentricity,
                                                 mode=c("in"))
in_eccentricity_networks_with_atr
table(in_eccentricity_networks_with_atr$Information_or_advice)
#3  4  5 
#7 27  9 
table(in_eccentricity_networks_with_atr$Value)
#2  3 
#14 32 
table(in_eccentricity_networks_with_atr$Advice)
#3  4  5  6 
#4 45 25  3 
table(in_eccentricity_networks_with_atr$Awareness)
#2  3 
#20 57  
out_eccentricity_networks_with_atr<-lapply(networks_with_atr,
                                            eccentricity,
                                            mode=c("out"))
out_eccentricity_networks_with_atr
table(out_eccentricity_networks_with_atr$Information_or_advice)
#0  3  4  5 
#3  6 26  8 
table(out_eccentricity_networks_with_atr$Value)
#0  1  2  3 
#1  2 13 30 
table(out_eccentricity_networks_with_atr$Advice)
#0  3  4  5  6 
#3 32 39  2  1 
table(out_eccentricity_networks_with_atr$Awareness)
#2  3 
#34 43 


##
#7.Triad census for all networks
##
## Triad census is the number of different configurations
#of 3 vertices (triples) in a graph.
#It implicitly includes indexes of network reciprocity, clustering,
#density, and it reflects the structural balance level in 
#a network graph
census_labels = c('003',
                  '012',
                  '102',
                  '021D',
                  '021U',
                  '021C',
                  '111D',
                  '111U',
                  '030T',
                  '030C',
                  '201',
                  '120D',
                  '120U',
                  '120C',
                  '210',
                  '300')
triad_df <- data.frame(labels=census_labels,
                              Information_or_advice=triad.census(information_or_advice_3_graph), 
                              Value=triad.census(value_3_graph),
                              Advice=triad.census(advice_3_graph),
                              Awareness=triad.census(awareness_3_graph))
triad_df
?triad.census

#uk.triada
colSums(triad_df[,-1])

triad_prop <- apply(triad_df[,-1], 2, function(x) (x/sum(x)) %>% round(digits = 3)) 
triad_prop_df <- as.data.frame(triad_prop)
triad_prop_df <- cbind(labels=census_labels, triad_prop_df)
triad_prop_df 

    #labels Information_or_advice Value Advice Awareness
#1     003                 0.494 0.168  0.560     0.245
#2     012                 0.158 0.134  0.204     0.131
#3     102                 0.243 0.291  0.166     0.363
#4    021D                 0.006 0.030  0.008     0.008
#5    021U                 0.003 0.009  0.004     0.006
#6    021C                 0.006 0.007  0.004     0.016
#7    111D                 0.016 0.046  0.008     0.032
#8    111U                 0.024 0.072  0.015     0.046
#9    030T                 0.001 0.008  0.003     0.003
#10   030C                 0.000 0.000  0.000     0.000
#11    201                 0.021 0.071  0.011     0.054
#12   120D                 0.002 0.017  0.002     0.005
#13   120U                 0.002 0.014  0.004     0.012
#14   120C                 0.002 0.009  0.001     0.003
#15    210                 0.011 0.063  0.006     0.024
#16    300                 0.012 0.062  0.003     0.052

write.csv(triad_prop_df,
          file = 'output/triad_prop_df.csv')
##
#
# TASK 3: 
# (a) Identify the most dominant triad forms in each of the 4 examined networks. 
  
  #Information_or_advice mreza kao najzastupljeniju ima trijadu u kojoj
  #niko ni sa kim nije povezan->003  49,4%, zatim 24% su reciprocne veze
  #izmedju dva aktera dok je treci nepovezan-> 102, treca najzastupljenija trijada 
  #je 012, u kojoj je jedn akter povezan sa drugim i nem drugih veza, takozvana asimetricna dijada
  #cetvrta i peta trijada po zastupljenosti su 111U i  201, drUga se sastoji iz reciprocne veze 
  #izmedju akter a i b i reciprocne veze izmedju aktera a i c, prva se sastoji iz 
   #recirocne veze izmedju aktera a i b i asimetricne veze izmedju aktera a i c, od a ka c.
  #zanimljivo je da sve ostale vrste trijada postoje makar u malom procentu( sve ostale oko 1% ili ispod 1% )

 
  

  #Value mreza kao najzastupljeniju ima trijadu 102, koja podrazumeva 
#samo jednu reciprocnu vezu izmedju aktera a i b i to->29,1%
# (b) What do the identified triads tell us (for each network)? In other words, 
#     try to interpret the identified triad forms in the context of each network.
# (c) How do the networks differ with respect to their triad census?
#
##






#8. Dyad census and reciprocity in information or advice network

# To compute it, we can use the function that computes 
# dyad census, that is, the number of dyads (node pairs) with:
# - mutual connections ($mut)
# - non-mutual connections ($asym)
# - no connection at all ($null)
##diade grupe od dva clana, mogu biti povezane u jednom smeru,
#u oba smera ili 
#ne moraju biti povezane uopste
summary(information_or_advice_3_graph)
dc_information_or_advice_3_graph <- dyad_census(information_or_advice_3_graph)
dc_information_or_advice_3_graph
#$mut
#[1] 118
#118 parova cvorova ima medjusobnu konekciju, obostranu
#sto implicira da traze ali i daju savete drugom akteru
#$asym
#[1] 76
#76 parova ima asimetricnu vezu sto znaci da samo traze ili samo daju savete
#ne postoji uzajamnost ili reciprocitet
#$null
#[1] 709
#dok kod 709 parova uopste nema odnosa saradnje
#posto je ovo mreza koja uzima u obzir samo ove tri frekvencije veza: 
#3: Sometimes; 
#4: Often; and 
#5:Very Often
#a prve tri zanemariju
#0: I Do Not Know This Person; 
#1: Never; 
#2: Seldom; 
#Ovde nemamo potencijalno postojece veze u smislu da li se uopste znaju
#ili vrlo retko traze savet jedni od drugih

# Reciprocity represents the proportion of mutual (reciprocated) 
# connections in a directed graph. 
#Formally, the reciprocity of a directed network is the fraction of edges that
#belong to a loop of length two
# To get reciprocity:
(2*dc_information_or_advice_3_graph$mut)/ecount(information_or_advice_3_graph)
information_or_advice_3_graph
ecount(information_or_advice_3_graph)
#Po ovom sto vidim ispada da je 75,6% veza koje su reciprocne

# We can also use the reciprocity function:
reciprocity(information_or_advice_3_graph)

# Reciprocity is also defined as the (conditional) probability 
# that if A is connected to B, then B will be connected to A.
# In this interpretation, reciprocity is the following ratio:
dc_information_or_advice_3_graph$mut/(dc_information_or_advice_3_graph$mut + dc_information_or_advice_3_graph$asym)

# This can be also interpreted as the proportion of mutually
# connected node pairs out of all the pairs that are connected. 
# In this interpretation, reciprocity can be computed using 
# the reciprocity f. with the mode parameter set to 'ratio':
reciprocity(information_or_advice_3_graph, mode = 'ratio')
#0.60 ili 60,62% procenta

dyad_census



diyad_c_networks_with_atr<-lapply(networks_with_atr,
                                      dyad_census)
diyad_c_networks_with_atr
#$Information_or_advice
#$Information_or_advice$mut
#[1] 118
#$Information_or_advice$asym
#[1] 76
#$Information_or_advice$null
#[1] 709

#$Value
#$Value$mut
#[1] 312
#$Value$asym
#[1] 176
#$Value$null
#[1] 547


#$Advice
#$Advice$mut
#[1] 235
#$Advice$asym
#[1] 283
#$Advice$null
#[1] 2408


##$Awareness
#$Awareness$mut
#[1] 754
#$Awareness$asym
#[1] 334
#$Awareness$null
#[1] 1838

# Compare the four networks with respect to reciprocity:
networks_with_atr_reciprocity<-lapply(networks_with_atr,
                                      reciprocity)
networks_with_atr_reciprocity_2<-lapply(networks_with_atr,
                                        reciprocity,
                                        mode = 'ratio')
networks_with_atr_reciprocity

#$Information_or_advice
#[1] 0.7548387

#$Value
#[1] 0.7794486

#$Advice
#[1] 0.62417

#$Awareness
#[1] 0.8186754




networks_with_atr_reciprocity_2
#$Information_or_advice
#[1] 0.6062176

#Value
#[1] 0.6386037

#$Advice
#[1] 0.453668

#$Awareness
#[1] 0.693014

#Social Capital Hypothesis(Reciprocity): If an actor (B) asks advice from an 
                                                              #actor (A),
#actor (A) will be more likely in turn to ask advice from actor (B).

#Social Status Hypothesis (Non-Reciprocity): If an actor (B) asks advice 
                                                      #from an actor (A), 
#actor (A) will be less likely in turn to ask advice from actor (B).


##
### * 9. Koeficijent klasterovanja za sve mreze
##
?transitivity
# Transitivity implies that, if A is connected to B, and B is 
#connected to C,  then A is connected to C, as well. 
#In real networks, perfect transitivity israrely present, 
#partial transitivity is more typical: the fact that A is connected
#to B and B is connected to C does not guarantee that A will be 
#related to C, but makes it much more likely; for example,
#the friend of my friend is not necessarily my friend,
#but is far more likely to be my friend  than a randomly chosen 
#member of the population.

# Transitivity is also known as the clustering coefficient. 
# It can be:
# - global: refers to the graph as a whole; it is defined as the ratio 
#           of the triangles and the connected triples in the graph
# - local: refers to an individual vertex; it is defined as the ratio 
#          of the triangles connected to the vertex and the triples 
#          centered on the vertex (see, e.g.: 
#          https://www.researchgate.net/profile/Ruggero_G_Bettinardi/publication/317033037/figure/fig51/AS:496043125952517@1495277305701/Figure-C4-Local-clustering-coefficient-Schematic-representation-of-how-to-compute-the_W640.jpg)


#Perfect transitivity 
#It is very rare in real networks, since it implies that
#each component is a clique, that is, each pair of reachable nodes
#in the graph would be connected by an edge.

#An alternative definition for the transitivity coefficient, which
#is often referred to as clustering coefficient,
#is the mean local clustering coefficient of nodes in the network:
#This definition has the drawback that it tends to be dominated by 
#vertices with low degree, since they have a small number of
#possible pairs of neighbors (the denominator of the local clustering
# coefficient). In particular, a node with only two neighbors 
#which are connected each other has local clustering one.
#For this reason, the clustering coefficient is, in general, 
#higher than the transitivity coefficient . 
#If we have similar values for the
#average local clustering coefficient and
#for the global clustering coefficient, 
#it means that these groups are
#interconnected to a similar extent as within them. 

#Moreover, for vertices of degree zero or one the local clustering 
#coefficient is not defined (both numerator and denominator are 0).
#Hence for networks with a significant number of vertices with low 
#degree, the clustering coefficient  gives a rather poor picture
#of the overall properties of the network


?transitivity
# The transitivity function treats the input graph as undirected.

#Priprema kako bih racunala tranzitivnost za sve 4 mreze odjednom 
information_or_advice_3_graph_no_loops<-simplify(information_or_advice_3_graph,
                                                 remove.loops = TRUE)

value_3_graph_no_loops<-simplify(value_3_graph,
                                 remove.loops = TRUE)

networks_with_atr_no_loops<-list(Information_or_advice=information_or_advice_3_graph_no_loops, 
                                 Value=value_3_graph_no_loops,
                                 Advice=advice_3_graph,
                                 Awareness=awareness_3_graph)

networks_with_atr_no_loops

# Transforming the  network into undirected  network
information_or_advice_3_graph_no_loops_undir<-as.undirected(information_or_advice_3_graph_no_loops,
                                            mode=c("collapse"))


?as.undirected
value_3_graph_no_loops_undir<-as.undirected(value_3_graph_no_loops,
                                            mode=c("collapse"))

advice_3_graph_undir<-advice_3_graph_unirected<-as.undirected(advice_3_graph,
                                            mode=c("collapse"))

awareness_3_graph_undir<-as.undirected(awareness_3_graph,
                                           mode=c("collapse"))


networks_with_atr_no_loops_undir<-list(Information_or_advice=information_or_advice_3_graph_no_loops_undir, 
                                       Value=value_3_graph_no_loops_undir,
                                       Advice=advice_3_graph_undir,
                                       Awareness=awareness_3_graph_undir)

networks_with_atr_no_loops_undir

#GLOBAL clustering coef.
clus_coef_inf_or_advice_glob <- transitivity(information_or_advice_3_graph_no_loops_undir, 
                                         type = 'global')
clus_coef_inf_or_advice_glob
#0.539823
#53,98% is clustering coeficient for information or advice network
transitivity(information_or_advice_1_graph, 
                           type = 'global')

# We can also examine clustering at the local level:
#*****Kako sam shvatiila u dokumentaciji za funkciju trasitivity pise
#**: " local= The local transitivity of an undirected graph, this is calculated for each
#***vertex given in the vids argument. The local transitivity of a vertex is the
#**ratio of the triangles connected to the vertex and the triples centered on the
#*#vertex. For directed graph the direction of the edges is ignored.""
#*# Pise da je za usmerene grafove automatski zanemaren smer, nije nuzno
#* # da graf  pretvaram u undirected format
#* #za local trasitivity kada koristim pri racunanju nove neusmerene
#* #mreze, napravljene koriscenjem moda="collapse", donbijem 
#* #potpuno drugacije rezultate, nego kad koristim usmerene mreze.
#* #Medjutim pronalaskom paralelnog načina za izračunavanje 
#* prosečnog lokalnog koefijenta klasiterovanja, rešila sam dilemu.
#* #umesto da koristim funkciju mean(), bolje je 
#* #da koristim type = 'average', u okviru funkcije transitivity
#* #kako sam pronašla na internetu sledeće
#* #transitivity(graph, type = "average") being an 
#* #average of transitivity(graph, type = "local") 
#* #first computes the local clustering coefficients
#* # and then averages them.Što tačno odgovara mojim potrebama

?transitivity

clus_coef_inf_or_advice_loc <- transitivity(information_or_advice_3_graph_no_loops_undir, 
                                         type = 'local',
                                         vids = NULL,
                                         isolates= c( "zero"))
clus_coef_inf_or_advice_loc
V(information_or_advice_3_graph_no_loops)

#formula za prosečan lokalni koeficijent klasterovanja
1/43*sum(clus_coef_inf_or_advice_loc)
#0.5692022
#Može i ovako 
transitivity(information_or_advice_3_graph_no_loops, 
             type = 'average',
             vids = NULL,
             isolates= c( "zero"))
#0.5692022
##56,92% is average local clustering coeficient for 
#information or advice network

#Local clustering can be used for a probe for the existence of so-called
#structural holes in a network. While it is common, mainly in 
#social networks, for the neighbors of a vertex to be connected
#among themselves, it happens sometimes that these expected 
#connections are missing. The missing links are called structural
#holes. If we are interested in efficient spread of information or
#other traffic around a network, then structural holes are a bad 
#thing, since they reduce the number of alternative routes 
#information can take.

#On the other hand, structural holes are a good thing for the central
#vertex  whose friends lack connections, because they give 
# power over information flow between those friends.
#If two friends of A are connected directly then they can exchange 
#information directly without bothering A. On the other hand,
#if they are not connected, then there is a good change that they 
#exchange information through A. 



#Weighted transitivity 
transitivity_weighted<-lapply(networks_with_atr_no_loops_undir,
                              transitivity, 
                              type = "barrat",
                              vids=NULL,
                              isolates= c( "zero"))

transitivity_weighted
1/43*sum(transitivity_weighted$Information_or_advice)
1/43*sum(transitivity_weighted$Value)
1/77*sum(transitivity_weighted$Advice)
1/77*sum(transitivity_weighted$Awareness)
#Local transitivity
transitivity_local<-lapply(networks_with_atr_no_loops_undir,
                           transitivity, 
                           type = "local",
                           vids=NULL,
                           isolates= c( "zero"))
transitivity_local


#(local)Average trasitivity 

#transitivity(graph, type = "average") being an average of 
#transitivity(graph, type = "local") first computes
#the local clustering coefficients and then averages them

transitivity_average<-lapply(networks_with_atr_no_loops_undir,
                             transitivity, 
                             type = "average",
                             vids=NULL,
                             isolates= c( "zero"))
transitivity_average
#> transitivity_average
#$Information_or_advice
#[1] 0.5692022

#$Value
#[1] 0.7617762

#$Advice
#[1] 0.6543879

#$Awareness
#[1] 0.7206673

#GLOBAL TRANSITIVITY
transitivity_global<-lapply(networks_with_atr_no_loops_undir,
                            transitivity, 
                            type = 'global',
                            isolates= c( "zero"))
transitivity_global
#$Information_or_advice
#[1] 0.539823

#$Value
#[1] 0.6872918

#$Advice
#[1] 0.5452865

#$Awareness
#[1] 0.6477972


V(information_or_advice_3_graph)$name
information_or_advice_3_graph
transitivity_local$Information_or_advice
transitivity_local$Information_or_advice
V(information_or_advice_3_graph_no_loops_undir)
information_or_advice_3_trans_df <- data.frame(node_id=as.integer(V(information_or_advice_3_graph)$name),
                                               trans=transitivity_local$Information_or_advice)

head(information_or_advice_3_trans_df)
tail(information_or_advice_3_trans_df)
is.nan(information_or_advice_3_trans_df$trans)
mean(information_or_advice_3_trans_df$trans)#0.569
information_or_advice_3_trans_df
information_or_advice_3_trans_df_decr<-information_or_advice_3_trans_df[order(information_or_advice_3_trans_df$trans,
                                                                              decreasing = TRUE),]
information_or_advice_3_trans_df_decr


write.csv(information_or_advice_3_trans_df_decr,
          file = 'output/information_or_advice_3_trans_df_decr.csv')
#node_id    trans

# when examining these results consider that the information or advice network 
# is treated as an undirected network

 #  node_id     trans
#43     15 1.0000000 (povezan sa 3 aktera)
#36      39 0.9722222 (povezan sa 9 aktera)
#4        4 0.9000000 (5)
#33      36 0.8909091 (11)
#42      46 0.8333333(4)
#16      17 0.8222222(10)
#24      26 0.8181818(12)
#14      14 0.8000000(5)
#40      40 0.7878788(7)
#20      21 0.7857143(8)
#5        5 0.7818182(11)

#6        6 0.7307692
#13      13 0.7142857
#11      11 0.7000000
#9        9 0.6666667
#32      35 0.6666667
#38      41 0.6666667
#23      25 0.6410256
#26      28 0.6373626
#37      40 0.6190476
#29      32 0.6000000
#18      19 0.5666667
#22      23 0.5416667
#41      45 0.5294118
#3        3 0.5000000
#10      10 0.5000000
#17      18 0.5000000
#25      27 0.5000000
#31      34 0.5000000
#21      22 0.4761905
#7        7 0.4727273
#27      29 0.4696970
#12      12 0.4666667
#28      31 0.4444444
#1        1 0.4285714
#19      20 0.3789474
#2        2 0.3333333
#35      38 0.2967033
#30      33 0.2692308
#8        8 0.2666667
#15      16 0.0000000
#34      37 0.0000000
#39      42 0.0000000
# Let's visualise the undirected information or advice network to better understand
# the obtained results. I'll use visNetwork as it allows for interaction
# with the network nodes

vis_net_information_or_advice_3_graph_no_loops_undir<-toVisNetworkData(information_or_advice_3_graph_no_loops_undir,
                                                                       idToLabel = TRUE)
vis_net_information_or_advice_3_graph_no_loops_undir

# Finally, display the network
vis_net_1_information_or_advice_3_graph_no_loops_undir <- visNetwork(nodes = vis_net_information_or_advice_3_graph_no_loops_undir$nodes, 
                                                                     edges = vis_net_information_or_advice_3_graph_no_loops_undir$edges,
                                                                     width = "100%",
                                                                     main="Information or advice network-undirected-no loops") 
vis_net_information_or_advice_3_graph
vis_net_1_information_or_advice_3_graph_no_loops_undir
# Add highlighting to show reachable nodes
vis_net_1_information_or_advice_3_graph_no_loops_undir %>% 
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE) 






vis_net_value_3_graph_no_loops_undir<-toVisNetworkData(value_3_graph_no_loops_undir,
                                                                       idToLabel = TRUE)
vis_net_value_3_graph_no_loops_undir

# Finally, display the network
vis_net_1_value_3_graph_no_loops_undir <- visNetwork(nodes = vis_net_value_3_graph_no_loops_undir$nodes, 
                                                                     edges = vis_net_value_3_graph_no_loops_undir$edges,
                                                                     width = "100%",
                                                                     main="Value graph no loops - undir") 

vis_net_1_value_3_graph_no_loops_undir
# Add highlighting to show reachable nodes
vis_net_1_value_3_graph_no_loops_undir %>% 
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE) 



#
# 10. Homophily / Assortativity
#

# Homophily - also known as Assortativity or Assortative Mixing - can be defined 
# as the tendency of actors in a network to connect to other actors that are similar 
# to them in some way ("birds of feather flock together"). 
# Assortativity can be based on any attribute of a node, but often it is assortativity
# based on nodes' degree (number of direct connections) that is examined.

?assortativity
networks_with_atr
# The following function computes assortativity in the given network based on 
# Location, the_organisational_level and degree
#I took it among the functions we did in the lecture
compute_assortitivity <- function(g) {
  result = list()
  result$the_organisational_level <- assortativity_nominal(g, types = as.factor(V(g)$The_organisational_level))
  result$location <- assortativity(g, types1 = V(g)$Location)
  result$degree <- assortativity_degree(g)
  return(result)
  
}

#I want to examine assortativity for  my four  networks, 
#two for each company.
# (homophily is indicated by positive values; the higher 
# the value, the higher the homophily)


lapply(networks_with_atr, compute_assortitivity)
#$Information_or_advice
#$Information_or_advice$the_organisational_level
#[1] -0.01899117

#$Information_or_advice$location
#[1] 0.06598314

#$Information_or_advice$degree
#[1] 0.05730196


#$Value
#$Value$the_organisational_level
#[1] -0.02927964

#$Value$location
#[1] 0.06329238

#$Value$degree
#[1] -0.0649011


#$Advice
#$Advice$the_organisational_level
#[1] -0.03121663

#$Advice$location
#[1] 0.7760662
#The assortativity coefficient is calculated to be 0.77, which is
#moderately large. Therefore, we conclude that the Advice network 
#shows location-based homophily. That mean, the actors which giving
#each other's advice tend to come from the same location.
#To znači, glumci koji jedni drugima daju savete
#obično dolaze sa iste lokacije

#$Advice$degree
#[1] -0.01863412


#$Awareness
#$Awareness$the_organisational_level
#[1] -0.03445821

#$Awareness$location
#[1] 0.5265486
#The assortativity coefficient is calculated to be 0.52, which is
#moderate. Therefore, we conclude that the Awareness network 
#shows medium location-based homophily. That mean, that the
#actors which value  each other's competence have medium tendention 
#to come from the same location

#$Awareness$degree
#[1] 0.1156587




#
# 11. K-cores
#

# The k-core is the maximal subgraph in which every node has degree of at least k.
# In other words, it is a group of actors where each actor is directly connected
# to at least k other actors in the group.
# A node has coreness M if it belongs to a M-core but not to (M+1)-core.
# Coreness is used as a means of examining tightly connected groups in a network.
#K-cores -
 # grupe u kojoj je svaki clan direktno povezan sa minimum K
 #drugih clanova grupe

# The coreness function computes the coreness of each vertex in the network:
?coreness
information_or_advice_3_graph_core_in <- coreness(information_or_advice_3_graph, 
                                                  mode = 'in')
information_or_advice_3_graph_core_in
table(information_or_advice_3_graph_core_in)
write.csv(table(information_or_advice_3_graph_core_in),
          file = 'output/table_information_or_advice_3_graph_core_in.csv')
information_or_advice_3_graph_core_out <- coreness(information_or_advice_3_graph,
                                                   mode = 'out')
information_or_advice_3_graph_core_out
table(information_or_advice_3_graph_core_out)
write.csv(table(information_or_advice_3_graph_core_out),
          file = 'output/table_information_or_advice_3_graph_core_out.csv')
# To better understand the coreness results, let's visualize them


information_or_advice_3_graph_core_in
information_or_advice_3_graph_core_in_df <-data.frame(node_id=as.integer(V(information_or_advice_3_graph)$name),
                              core=information_or_advice_3_graph_core_in)
information_or_advice_3_graph_core_in_df

information_or_advice_3_graph_core_in_colors = attr_based_color_gradient(g_attr =information_or_advice_3_graph_core_in_df$core, 
                                                pal_end_points = c('#edf8fb', '#6e016b'))

information_or_advice_3_graph_core_in_colors
# We will color nodes based on their
#in-coreness,  that is, their coreness based on their
#incoming edges
png("graphs/inf_or_adv_color_in_core.png",
    width = 1100,
    height = 1100)
plot(information_or_advice_3_graph,
     layout = layout.fruchterman.reingold(information_or_advice_3_graph),
     vertex.size=4,
     vertex.frame.color=information_or_advice_3_graph_core_in_colors,
     vertex.color = information_or_advice_3_graph_core_in_colors,
     edge.arrow.size = 0.13,
     asp = -0.1,
     margin = 0,
     vertex.label.cex = 0.7,
     vertex.label.color="#fd8d3c",
     main="Information or advice network\n (node color denotes in-coreness)")
dev.off()
# To better understand the coreness results, let's visualize them
information_or_advice_3_graph_core_out_df <-data.frame(node_id=as.integer(V(information_or_advice_3_graph)$name),
                                                      core=information_or_advice_3_graph_core_out)
information_or_advice_3_graph_core_out_df

information_or_advice_3_graph_core_out_colors = attr_based_color_gradient(g_attr =information_or_advice_3_graph_core_out_df$core, 
                                                                         pal_end_points = c('#ffffb2', '#b10026'))

information_or_advice_3_graph_core_out_colors
?layout.fruchterman.reingold
# We will color nodes based on their
#out-coreness,  that is, their coreness based on their
#outcoming edges
png("graphs/inf_or_adv_color_out_core.png",
    width = 1100,
    height = 1100)
plot(information_or_advice_3_graph,
     layout = layout.fruchterman.reingold(information_or_advice_3_graph),
     vertex.size=4,
     vertex.frame.color=information_or_advice_3_graph_core_out_colors,
     vertex.color = information_or_advice_3_graph_core_out_colors,
     edge.arrow.size = 0.15,
     asp = -0.1,
     margin = 0,
     vertex.label.cex = 0.7,
     vertex.label.color="#74a9cf",
     main="Information or advice network\n (node color denotes out-coreness)")
dev.off()

# Now, let's inspect the most densely connected part of the network - nodes
# that have the highest values for both in- and out-coreness.
# First, identify them:

information_or_advice_3_graph_core <- which(information_or_advice_3_graph_core_in == max(information_or_advice_3_graph_core_in) & 
                                              information_or_advice_3_graph_core_out == max(information_or_advice_3_graph_core_out))
information_or_advice_3_graph_core
V(information_or_advice_3_graph)[information_or_advice_3_graph_core]

# Then, extract the subgraph with those nodes only:
information_or_advice_3_graph_core_net <- induced_subgraph(information_or_advice_3_graph, 
                                    V(information_or_advice_3_graph)[information_or_advice_3_graph_core])
summary(information_or_advice_3_graph_core_net)
#10 čvorova se nalazi u ovom najgušće povezanom k-koru i 84 veze 
#10/43 vertices, named, from 3021264:
#  [1] 5  6  19 23 26 28 36 39 44 45
edge_density(information_or_advice_3_graph_core_net)#0,93
#links_per_node_advice_3_core
84/10 #(8.4)
# finally, plot the densest subgraph:
#Mozemo slobodno reci da malo fali da u ovom klasteru svako bude povezan 
#sa svakim
#u ovom k-koru se takodje nalaze dva cvora koji imaju loop 6 i 26
#nisam htela da uklanjam te loop-ove zato sto je zanimljivo
#da su sebe same oznacili kao nekog od koga cesto traze savet
#ili je greska ili su time opet nesto hteli da poruce
?plot
png("graphs/inf_or_adv_densest_subgraph.png",
    width = 1100,
    height = 1100)
plot(information_or_advice_3_graph_core_net,
     layout = layout_nicely(information_or_advice_3_graph_core_net),
     vertex.frame.color=3,
     vertex.color=3,
     vertex.label.color="yellow",
     edge.arrow.size = 0.2,
     main="The densest subgraph of the information or advice network")
dev.off()



#K-cores - su grupe u kojoj je svaki član direktno povezan sa minimum K
#drugih članova grupe


# The coreness function computes the coreness of each vertex in the network:
?coreness
value_3_graph_core_in <- coreness(value_3_graph, 
                                     mode = 'in')
value_3_graph_core_in
table(value_3_graph_core_in)
write.csv(table(value_3_graph_core_in),
          file = 'output/table_value_3_graph_core_in.csv')
value_3_graph_core_out <- coreness(value_3_graph,
                                    mode = 'out')
value_3_graph_core_out
table(value_3_graph_core_out)
write.csv(table(value_3_graph_core_out),
          file = 'output/table_value_3_graph_core_out.csv')
# To better understand the coreness results, let's visualize them


value_3_graph_core_in
value_3_graph_core_in_df <-data.frame(node_id=as.integer(V(value_3_graph)$name),
                                                      core=value_3_graph_core_in)
value_3_graph_core_in_df
value_3_graph_core_in$core
value_3_graph_core_in_colors = attr_based_color_gradient(g_attr =value_3_graph_core_in_df$core, 
                                                         pal_end_points = c("#d53e4f",
                                                           "#fc8d59",
                                                           "#fee08b",
                                                           "#ffffbf",
                                                           "#e6f598",
                                                           "#99d594",
                                                           "#3288bd"))

value_3_graph_core_in_colors
# We will color nodes based on their
#in-coreness,  that is, their coreness based on their
#incoming edges
png("graphs/value_3_graph_color_in_core.png",
    width = 1100,
    height = 1100)
plot(value_3_graph,
     layout = layout.fruchterman.reingold(value_3_graph),
     vertex.size=4,
     vertex.frame.color=value_3_graph_core_in_colors,
     vertex.color = value_3_graph_core_in_colors,
     edge.arrow.size = 0.13,
     asp = -0.1,
     margin = 0,
     vertex.label.cex = 0.7,
     vertex.label.color="#fd8d3c",
     main="value_3_graph\n (node color denotes in-coreness)")
dev.off()
# To better understand the coreness results, let's visualize them
value_3_graph_core_out_df <-data.frame(node_id=as.integer(V(value_3_graph)$name),
                                                       core=value_3_graph_core_out)
value_3_graph_core_out_df

value_3_graph_core_out_colors = attr_based_color_gradient(g_attr =value_3_graph_core_out_df$core, 
                                                                          pal_end_points = c("#d53e4f",
                                                                                             "#fc8d59",
                                                                                             "#fee08b",
                                                                                             "#ffffbf",
                                                                                             "#e6f598",
                                                                                             "#99d594",
                                                                                             "#3288bd"))

value_3_graph_core_out_colors
?layout.fruchterman.reingold
# We will color nodes based on their
#out-coreness,  that is, their coreness based on their outcoming edges
png("graphs/value_3_graph_color_out_core.png",
    width = 1100,
    height = 1100)
plot(value_3_graph,
     layout = layout.fruchterman.reingold(value_3_graph),
     vertex.size=4,
     vertex.frame.color=value_3_graph_core_out_colors,
     vertex.color = value_3_graph_core_out_colors,
     edge.arrow.size = 0.15,
     asp = -0.1,
     margin = 0,
     vertex.label.cex = 0.7,
     vertex.label.color="#74a9cf",
     main="value_3_graph\n (node color denotes out-coreness)")
dev.off()



# Now, let's inspect the most densely connected part of the network - nodes
# that have the highest values for both in- and out-coreness. First, identify them:

value_3_graph_core <- which(value_3_graph_core_in == max(value_3_graph_core_in) & 
                              value_3_graph_core_out == max(value_3_graph_core_out))
value_3_graph_core
V(value_3_graph)[value_3_graph_core]

# Then, extract the subgraph with those nodes only:
value_3_graph_core_net <- induced_subgraph(value_3_graph, 
                                          V(value_3_graph)[value_3_graph_core])
summary(value_3_graph_core_net)
value_3_graph_core_net
#22 čvora se nalazi u ovom najgušće povezanom k-koru i 341 veza 
#22/46 vertices, named, from 5026e54:
# 2  5  6  7  11 13 14 17 18 19 20 21 23 25 26 27 34 36 38 39 44 45
edge_density(value_3_graph_core_net)#0.73
#links_per_node_advice_3_core
312/22#(14.18)
# finally, plot the densest subgraph:
#U ovom subgrafu koji se odnosi na najgušće povezan deo mreže value_3_graph,
#gde su uljučeni čvorevi sa najvišom vrednošću za in i out corness gustinaje
#umereno visoka  0.73, a links per node je 14.18 što implicira da je ostvareno
#73% veza u odnosu na uukupno moguće. Članovi ovog najgušće povezanog podgrafa
#su: 2,  5 , 6 , 7 , 11, 13, 14, 17, 18 ,19, 20, 21, 23, 25, 26, 27, 34, 36, 38,
#39, 44, 45. 
?plot
png("graphs/value_3_graph_densest_subgraph.png",
    width = 1100,
    height = 1100)
plot(value_3_graph_core_net,
     layout = layout_nicely(value_3_graph_core_net),
     vertex.frame.color=3,
     vertex.color=3,
     vertex.label.color="yellow",
     edge.arrow.size = 0.5,
     main="The densest subgraph of the value_3net")
dev.off()

#K-cores - su grupe u kojoj je svaki član direktno povezan sa minimum K
#drugih članova grupe


# The coreness function computes the coreness of each vertex in the network:
?coreness
advice_3_graph_core_in <- coreness(advice_3_graph, 
                                   mode = 'in')
advice_3_graph_core_in
table(advice_3_graph_core_in)
write.csv(table(advice_3_graph_core_in),
          file = 'output/table_advice_3_graph_core_in.csv')
advice_3_graph_core_out <- coreness(advice_3_graph,
                                    mode = 'out')
advice_3_graph_core_out
table(advice_3_graph_core_out)
write.csv(table(advice_3_graph_core_out),
          file = 'output/table_advice_3_graph_core_out.csv')

# Now, let's inspect the most densely connected part of the network - nodes
# that have the highest values for both in- and out-coreness. First, identify them:

advice_3_graph_core <- which(advice_3_graph_core_in == max(advice_3_graph_core_in) & 
                               advice_3_graph_core_out == max(advice_3_graph_core_out))
advice_3_graph_core
V(advice_3_graph)[advice_3_graph_core]
#24/77 vertices, named, from 4b28e16:
#[1] 18 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 38 40 41 43 44 6

# Then, extract the subgraph with those nodes only:
advice_3_graph_core_net <- induced_subgraph(advice_3_graph, 
                                            V(advice_3_graph)[advice_3_graph_core])
summary(advice_3_graph_core_net)
advice_3_graph_core_net
#24 čvora se nalazi u ovom najgušće povezanom k-koru i 256 veza 
#24/77 vertices, named, from 5026e54:
# 18 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 38 40 41 43 44 68
edge_density(advice_3_graph_core_net)#0.46
#links_per_node_advice_3_core
256/24#(10.66)
# finally, plot the densest subgraph:
#U ovom subgrafu koji se odnosi na najgušće povezan deo mreže advice_3_graph,
#gde su uljučeni čvorevi sa najvišom vrednošću za in i out corness gustinaje
#umerena  0.46, a links per node je 10.66 što implicira da je ostvareno
#46% veza u odnosu na uukupno moguće. Članovi ovog najgušće povezanog podgrafa
#su:18, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
#38, 40, 41, 43, 44, 68. 
?plot
png("graphs/advice_3_graph_densest_subgraph.png",
    width = 1100,
    height = 1100)
plot(advice_3_graph_core_net,
     layout = layout_nicely(advice_3_graph_core_net),
     vertex.frame.color=3,
     vertex.color=3,
     vertex.label.color="yellow",
     edge.arrow.size = 0.5,
     main="The densest subgraph of the advice_3_graph")
dev.off()



#K-cores - su grupe u kojoj je svaki član direktno povezan sa minimum K
#drugih članova grupe

awareness_3_graph
# The coreness function computes the coreness of each vertex in the network:
?coreness
awareness_3_graph_core_in <- coreness(awareness_3_graph, 
                                  mode = 'in')
awareness_3_graph_core_in
table(awareness_3_graph_core_in)
write.csv(table(awareness_3_graph_core_in),
          file = 'output/table_awareness_3_graph_core_in.csv')
awareness_3_graph_core_out <- coreness(awareness_3_graph,
                                   mode = 'out')
awareness_3_graph_core_out
table(awareness_3_graph_core_out)
write.csv(table(awareness_3_graph_core_out),
          file = 'output/table_awareness_3_graph_core_out.csv')

# Now, let's inspect the most densely connected part of the network - nodes
# that have the highest values for both in- and out-coreness. First, identify them:

awareness_3_graph_core <- which(awareness_3_graph_core_in == max(awareness_3_graph_core_in) & 
                               awareness_3_graph_core_out == max(awareness_3_graph_core_out))
awareness_3_graph_core
V(awareness_3_graph)[awareness_3_graph_core]
#24/77 vertices, named, from 1efebc7:
# 18 20 21 22 23 24 25 26 27 29 30 31 32 33 35 36 37 38 39 40 41 42 43 44

# Then, extract the subgraph with those nodes only:
awareness_3_graph_core_net <- induced_subgraph(awareness_3_graph, 
                                            V(awareness_3_graph)[awareness_3_graph_core])
summary(awareness_3_graph_core_net)
awareness_3_graph_core_net
#24 čvora se nalazi u ovom najgušće povezanom k-koru i 550 veza 
#24/77 vertices, named, from 5026e54:
# 18 20 21 22 23 24 25 26 27 29 30 31 32 33 35 36 37 38 39 40 41 42 43 44
round(edge_density(awareness_3_graph_core_net), digits = 4)#1 tj. preciznije 0.9964
#links_per_node_awareness_3_graph_core_net
550/24#(22.91)
# finally, plot the densest subgraph:
#U ovom subgrafu koji se odnosi na najgušće povezan deo mreže awareness_3_graph,
#gde su uljučeni čvorevi sa najvišom vrednošću za in i out corness gustina je izuzetno visoka i
# iznosi skoro pa 100% tačnije 0.9963(99.64%), a links per node je 22.91 što implicira da je
#ostvarena skoro pa savršena klika. Članovi ovog najgušće povezanog podgrafa
#su:18, 20, 21, 22, 23, 24, 25, 26, 27, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44. 
?plot
png("graphs/awareness_3_graph_densest_subgraph.png",
    width = 1100,
    height = 1100)
plot(awareness_3_graph_core_net,
     layout = layout.circle(awareness_3_graph_core_net),
     vertex.frame.color=3,
     vertex.color=3,
     vertex.label.color="yellow",
     edge.arrow.size = 0.5,
     main="The densest subgraph of the awareness_3_graph")
dev.off()



##
#12.Cliques
##

?cliques
#cliques find all complete subgraphs in the input graph, obeying the size 
#limitations given in the min and max arguments.

##
##clique information or advice network
##

cliques(information_or_advice_3_graph, min = 9)#(samo 3)
cliques(information_or_advice_3_graph, min = 8)#(30 clanova)
cliques(information_or_advice_3_graph, min = 7)#(136)
cliques(information_or_advice_3_graph, min = 6)#(378)
cliques(information_or_advice_3_graph, min = 5)#(751)
cliques(information_or_advice_3_graph, min = 4)#(1179)
cliques(information_or_advice_3_graph, min = 3)#(1545)



largest_cliques_inf_or_adv<-largest_cliques(information_or_advice_3_graph)
largest_cliques_inf_or_adv


vis_net_information_or_advice_3_graph<-toVisNetworkData(information_or_advice_3_graph,
                                                        idToLabel = TRUE)
vis_net_information_or_advice_3_graph
summary(vis_net_information_or_advice_3_graph)

b.visNetwork <- function(vis_net_information_or_advice_3_graph,
                         largest_cliques_inf_or_adv,
                         number){
  vis_net_information_or_advice_3_graph$nodes$group<-"other"
  vis_net_information_or_advice_3_graph$nodes$group[largest_cliques_inf_or_adv[[number]]] <- "clique"
  #Plot
  visNetwork(vis_net_information_or_advice_3_graph$nodes, 
             vis_net_information_or_advice_3_graph$edges) %>%
    visIgraphLayout() %>%
    visNodes(size = 40, shape = "circle") %>%
    visOptions(selectedBy="group",
               highlightNearest = TRUE,
               nodesIdSelection = TRUE) %>%
    visGroups(groupname = "clique", color = "orange")%>%
    visInteraction(keyboard = TRUE)%>%
    visLegend()
}
b.visNetwork(vis_net_information_or_advice_3_graph,
             largest_cliques_inf_or_adv,
             1)
b.visNetwork(vis_net_information_or_advice_3_graph,
             largest_cliques_inf_or_adv,
             2)
b.visNetwork(vis_net_information_or_advice_3_graph,
             largest_cliques_inf_or_adv,
             3)
#[[1]]
#+ 9/43 vertices, named, from f75baec:
 # [1] 39 5  6  19 23 26 36 45 44

#[[2]]
#+ 9/43 vertices, named, from f75baec:
 # [1] 39 5  6  19 23 26 36 45 25

#[[3]]
#+ 9/43 vertices, named, from f75baec:
 # [1] 36 6  19 23 26 45 28 17 44

#max_cliques finds all maximal cliques in the input graph. 
#A clique in maximal if it cannot be extended to a larger clique.
#The largest cliques are always maximal, but a maximal clique is not 
#neccessarily the largest
max_cliques(information_or_advice_3_graph,
            min = NULL, 
            max = NULL,
            subset = NULL,
            file = NULL)
#count_max_cliques counts the maximal cliques.
count_max_cliques(information_or_advice_3_graph)
#68
#clique_num calculates the size of the largest clique(s).
clique_num(information_or_advice_3_graph)
#9
#https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html



##
##clique value network
##
value_3_graph

cliques(value_3_graph, min =16)#(samo 1)
cliques(value_3_graph, min = 15)#(18 clanova)
cliques(value_3_graph, min = 14)#(159)
cliques(value_3_graph, min = 13)#(917)
cliques(value_3_graph, min = 12)#(3833)
cliques(value_3_graph, min = 11)#(12 196)
cliques(value_3_graph, min = 10)#(30461)



largest_cliques_value<-largest_cliques(value_3_graph)
largest_cliques_value
#16/46 vertices, named, from 5026e54:
  #[1] 22 20 44 19 45 23 26 27 28 5  17 6  21 25 36 39
vis_net_value_3_graph<-toVisNetworkData(value_3_graph,
                                    idToLabel = TRUE)
vis_net_value_3_graph
summary(vis_net_value_3_graph)
b.visNetwork_1 <- function(vis_net_value_3_graph,
                         largest_cliques_value,
                         number){
  vis_net_value_3_graph$nodes$group<-"other"
  vis_net_value_3_graph$nodes$group[largest_cliques_value[[number]]] <- "clique"
  #Plot
  visNetwork(vis_net_value_3_graph$nodes, 
             vis_net_value_3_graph$edges) %>%
    visIgraphLayout() %>%
    visNodes(size = 40, shape = "circle") %>%
    visOptions(selectedBy="group",
               highlightNearest = TRUE,
               nodesIdSelection = TRUE) %>%
    visGroups(groupname = "clique", color = "orange")%>%
    visInteraction(keyboard = TRUE)%>%
    visLegend()
}
b.visNetwork_1(vis_net_value_3_graph,
             largest_cliques_value,
             1)

#max_cliques finds all maximal cliques in the input graph. 
#A clique in maximal if it cannot be extended to a larger clique.
#The largest cliques are always maximal, but a maximal clique is not 
#neccessarily the largest
max_cliques(value_3_graph,
            min = NULL, 
            max = NULL,
            subset = NULL,
            file = NULL)
#count_max_cliques counts the maximal cliques.
count_max_cliques(value_3_graph)
#137
#clique_num calculates the size of the largest clique(s).
clique_num(value_3_graph)
#16




##
##clique ADVICE network
##

cliques(advice_3_graph, min =10)#(4)
cliques(advice_3_graph, min = 9)#(58 clanova)
cliques(advice_3_graph, min = 8)#(366)
cliques(advice_3_graph, min = 7)#(1363)
cliques(advice_3_graph, min = 6)#(3400)
cliques(advice_3_graph, min = 5)#(6147)
cliques(advice_3_graph, min = 4)#(8631)



largest_cliques_advice<-largest_cliques(advice_3_graph)
largest_cliques_advice
#[[1]]
#+ 10/77 vertices, named, from 4b28e16:
# [1] 19 24 26 28 29 31 34 38 40 44

#[[2]]
#+ 10/77 vertices, named, from 4b28e16:
# [1] 19 24 26 28 29 34 35 38 40 44

#[[3]]
#+ 10/77 vertices, named, from 4b28e16:
#[1] 19 24 28 29 31 32 33 34 38 40

#[[4]]
#+ 10/77 vertices, named, from 4b28e16:
# [1] 19 24 28 29 31 33 34 38 40 44
vis_net_advice_3_graph<-toVisNetworkData(advice_3_graph,
                                         idToLabel = TRUE)
vis_net_advice_3_graph
summary(vis_net_advice_3_graph)
b.visNetwork_2 <- function(vis_net_advice_3_graph,
                           largest_cliques_advice,
                           number){
  vis_net_advice_3_graph$nodes$group<-"other"
  vis_net_advice_3_graph$nodes$group[largest_cliques_advice[[number]]] <- "clique"
  #Plot
  visNetwork(vis_net_advice_3_graph$nodes, 
             vis_net_advice_3_graph$edges) %>%
    visIgraphLayout() %>%
    visNodes(size = 40, shape = "circle") %>%
    visOptions(selectedBy="group",
               highlightNearest = TRUE,
               nodesIdSelection = TRUE) %>%
    visGroups(groupname = "clique", color = "orange")%>%
    visInteraction(keyboard = TRUE)%>%
    visLegend()
}
b.visNetwork_2(vis_net_advice_3_graph,
               largest_cliques_advice,
               1)
b.visNetwork_2(vis_net_advice_3_graph,
               largest_cliques_advice,
               2)
b.visNetwork_2(vis_net_advice_3_graph,
               largest_cliques_advice,
               3)
b.visNetwork_2(vis_net_advice_3_graph,
               largest_cliques_advice,
               4)

#max_cliques finds all maximal cliques in the input graph. 
#A clique in maximal if it cannot be extended to a larger clique.
#The largest cliques are always maximal, but a maximal clique is not 
#neccessarily the largest
max_cliques(advice_3_graph,
            min = NULL, 
            max = NULL,
            subset = NULL,
            file = NULL)
#count_max_cliques counts the maximal cliques.
count_max_cliques(advice_3_graph)
#140
#clique_num calculates the size of the largest clique(s).
clique_num(advice_3_graph)
#10


##
##clique AWARENESS network
##
awareness_3_graph

cliques(awareness_3_graph, min =27)#(1)
cliques(awareness_3_graph, min = 26)#(28 clanova)
cliques(awareness_3_graph, min = 25)#(379)
cliques(awareness_3_graph, min = 24)#(3305)
cliques(awareness_3_graph, min = 23)#(20878)



largest_cliques_awareness<-largest_cliques(awareness_3_graph)
largest_cliques_awareness
#[[1]]
#+ 27/77 vertices, named, from 1efebc7:
 # [1] 42 18 21 25 26 29 30 35 20 22 23 32 33 38 41 43 19 24 27 28 31 34
#[23] 36 37 39 40 44


vis_net_awareness_3_graph<-toVisNetworkData(awareness_3_graph,
                                         idToLabel = TRUE)
vis_net_awareness_3_graph
summary(vis_net_awareness_3_graph)
b.visNetwork_3 <- function(vis_net_awareness_3_graph,
                           largest_cliques_awareness,
                           number){
  vis_net_awareness_3_graph$nodes$group<-"other"
  vis_net_awareness_3_graph$nodes$group[largest_cliques_awareness[[number]]] <- "clique"
  #Plot
  visNetwork(vis_net_awareness_3_graph$nodes, 
             vis_net_awareness_3_graph$edges) %>%
    visIgraphLayout() %>%
    visNodes(size = 40, shape = "circle") %>%
    visOptions(selectedBy="group",
               highlightNearest = TRUE,
               nodesIdSelection = TRUE) %>%
    visGroups(groupname = "clique", color = "orange")%>%
    visInteraction(keyboard = TRUE)%>%
    visLegend()
}
b.visNetwork_3(vis_net_awareness_3_graph,
               largest_cliques_awareness,
               1)

#max_cliques finds all maximal cliques in the input graph. 
#A clique in maximal if it cannot be extended to a larger clique.
#The largest cliques are always maximal, but a maximal clique is not 
#neccessarily the largest
max_cliques(awareness_3_graph,
            min = NULL, 
            max = NULL,
            subset = NULL,
            file = NULL)
#count_max_cliques counts the maximal cliques.
count_max_cliques(awareness_3_graph)
#275
#clique_num calculates the size of the largest clique(s).
clique_num(awareness_3_graph)
#27



