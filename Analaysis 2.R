
library(igraph)
#Ovaj paket omogucava da se napravi pajp line
library(magrittr)
library(ggplot2)
library(tidyr)
library(dplyr)
#install.packages("visNetwork")
library(visNetwork)

##
#13.STRENGTH
##
#Summing up the edge weights of the adjacent edges for each vertex.
?graph.strength
#STRENGTH IN information_or_advice_3_graph
strength_in_inf_or_advice<-graph.strength(information_or_advice_3_graph, 
                                       vids = V(information_or_advice_3_graph),
                                       mode = "in", 
                                       loops = FALSE,
                                       weights = NULL)
strength_in_inf_or_advice
strength_in_inf_or_advice_df <-data.frame(node_id=as.integer(V(information_or_advice_3_graph)$name),
                                                str_in=strength_in_inf_or_advice)
strength_in_inf_or_advice_df
str(strength_in_inf_or_advice_df)
summary(strength_in_inf_or_advice_df)
View(strength_in_inf_or_advice_df)
max(strength_in_inf_or_advice_df)
#57
# Koji akter ima najvecu vrednost za  strength in  
which(strength_in_inf_or_advice == max(strength_in_inf_or_advice))
#cvor 45 ima najvecu vrednost za strength in



#STRENGTH OUT information_or_advice_3_graph
strength_out_inf_or_advice<-graph.strength(information_or_advice_3_graph, 
                                          vids = V(information_or_advice_3_graph),
                                          mode = "out", 
                                          loops = FALSE,
                                          weights = NULL)
strength_out_inf_or_advice
strength_out_inf_or_advice_df <-data.frame(node_id=as.integer(V(information_or_advice_3_graph)$name),
                                          str_out=strength_out_inf_or_advice)
strength_out_inf_or_advice_df
str(strength_out_inf_or_advice_df)
summary(strength_out_inf_or_advice_df)
View(strength_out_inf_or_advice_df)
max(strength_out_inf_or_advice_df)
#67
# Koji akter ima najvecu vrednost za  strength out  
which(strength_out_inf_or_advice == max(strength_out_inf_or_advice))
#cvor 2 ima najvecu vrednost za strength out

#13 aktera sa najvecom vrednoscu za strength in
Top_in_strength_in_inf_or_advice_df <- strength_in_inf_or_advice_df %>% 
    select(node_id,str_in)%>%
    arrange(desc(strength_in_inf_or_advice_df$str_in)) %>%
    head(n=13)
View(Top_in_strength_in_inf_or_advice_df)
#cuvanje rezultata za kasniji prikaz 
write.csv(Top_in_strength_in_inf_or_advice_df,
          file = 'output/Top_in_strength_in_inf_or_advice_df.csv')
#13 aktera sa najvecom vrednoscu za strength out
Top_in_strength_out_inf_or_advice_df <- strength_out_inf_or_advice_df %>% 
    select(node_id,str_out)%>%
    arrange(desc(strength_out_inf_or_advice_df$str_out)) %>%
    head(n=13)
View(Top_in_strength_out_inf_or_advice_df)
#cuvanje rezultata za kasniji prikaz 
write.csv(Top_in_strength_out_inf_or_advice_df,
          file = 'output/Top_in_strength_out_inf_or_advice_df.csv')
?graph.strength




#strength za sve mreze
strength_in_all_net<-lapply(networks_with_atr,
       graph.strength,
       mode = "in", 
       loops = FALSE,
       weights = NULL)
strength_in_all_net



#VALUE NETWORK STRENGTH IN
strength_in_value_df <-data.frame(node_id=as.integer(V(value_3_graph)$name),
                                           str_in=strength_in_all_net$Value)
strength_in_value_df
strength_in_value_df
str(strength_in_value_df)
summary(strength_in_value_df)
View(strength_in_value_df)
max(strength_in_value_df)
#129
# Koji akter ima najvecu vrednost za  strength in  
which(strength_in_all_net$Value == max(strength_in_all_net$Value))
#cvor 20 ima najvecu vrednost za strength in



#VALUE NETWORK STRENGTH OUT

strength_out_all_net<-lapply(networks_with_atr,
       strength,
       mode = "out", 
       loops = FALSE,
       weights = NULL)
strength_out_all_net
strength_out_value_df <-data.frame(node_id=as.integer(V(value_3_graph)$name),
                                  str_out=strength_out_all_net$Value)
strength_out_value_df
strength_out_value_df
str(strength_out_value_df)
summary(strength_out_value_df)
View(strength_out_value_df)
max(strength_out_value_df)
#157
# Koji akter ima najvecu vrednost za  strength out  
which(strength_out_all_net$Value == max(strength_out_all_net$Value))
#cvor 22 ima najvecu vrednost za strength out


#13 aktera sa najvecom vrednoscu za strength in
Top_in_strength_in_value_df <- strength_in_value_df %>% 
    select(node_id,str_in)%>%
    arrange(desc(strength_in_value_df$str_in)) %>%
    head(n=13)
View(Top_in_strength_in_value_df)
#cuvanje rezultata za kasniji prikaz 
write.csv(Top_in_strength_in_value_df,
          file = 'output/Top_in_strength_in_value_df.csv')
#13 aktera sa najvecom vrednoscu za strength out
Top_in_strength_out_value_df <- strength_out_value_df %>% 
    select(node_id,str_out)%>%
    arrange(desc(strength_out_value_df$str_out)) %>%
    head(n=13)
View(Top_in_strength_out_value_df)
#cuvanje rezultata za kasniji prikaz 
write.csv(Top_in_strength_out_value_df,
          file = 'output/Top_in_strength_out_value_df.csv')

#HUB SCORE
?hub.score
#The hub scores of the vertices are defined as the principal eigenvector
#of A*t(A), where A is the adjacency matrix of the graph.
hub_score_inf_or_adv<-hub_score(information_or_advice_3_graph, 
                                scale = TRUE, 
                                weights = NULL,
                                options = arpack_defaults)
hub_score_inf_or_adv<-hub_score_inf_or_adv$vector
hub_score_inf_or_adv
?hub.score
#A named list with members:
#vector	: #The authority/hub scores of the vertices.
#value	 :#The corresponding eigenvalue of the calculated principal eigenvector.

#AUTHORUTY SCORE
#a node is given a high authority score by being linked from
#people that are recognized as Hubs for information.
authority_score_inf_or_adv<-authority_score(information_or_advice_3_graph, 
                                            scale = TRUE,
                                            weights = NULL)

authority_score_inf_or_adv<-authority_score_inf_or_adv$vector
authority_score_inf_or_adv

#Ne mogu do kraja da shvatim razliku izmedju authority scora 
#i hub score





#14.BETWEENNESS
?betweenness


#The vertex and edge betweenness are (roughly) defined by the number
#of geodesics (shortest paths) going through a vertex or an edge.
#betweenness centrality measures a vertex's control over information 
#flowing between all pairs of nodes in its component
information_or_advice_3_between<-betweenness(information_or_advice_3_graph, 
                                  v=V(information_or_advice_3_graph),
                                                     directed = TRUE,
                                                     weights = NULL,
                                                   normalized = TRUE)

betweenness(information_or_advice_3_graph,
            directed = TRUE,
            weights = NULL,
            normalized = FALSE)
betweenness(value_1_graph,
            directed = TRUE,
            normalized = FALSE)



information_or_advice_3_between
information_or_advice_3_between_df <-data.frame(node_id=as.integer(V(information_or_advice_3_graph)$name),
                             betw=information_or_advice_3_between)
information_or_advice_3_between_df
str(information_or_advice_3_between_df)


max(information_or_advice_3_between)

# Koji akter ima najvecu vrednost za  betweenness  
which(information_or_advice_3_between == max(information_or_advice_3_between))
#cvor 8 ima najvecu vrednost za  betweenness

#13 aktera sa najvecom vrednoscu za betweenness
Top_in_information_or_advice_3_between_df <- information_or_advice_3_between_df %>% 
    select(node_id,betw)%>%
    arrange(desc(information_or_advice_3_between_df$betw)) %>%
    head(n=13)
View(Top_in_information_or_advice_3_between_df)
#cuvanje rezultata za kasniji prikaz 
write.csv(Top_in_information_or_advice_3_between_df,
          file = 'output/Top_in_information_or_advice_3_between_df.csv')



betweenness_all_net<-lapply(networks_with_atr,
                             betweenness,
                             directed = TRUE,
                             weights = NULL,
                             normalized = TRUE)

betweenness_all_net
max(betweenness_all_net$Value)
#0.1088632
which(betweenness_all_net$Value==max(betweenness_all_net$Value))
#22
value_between_df <-data.frame(node_id=as.integer(V(value_3_graph)$name),
                                                betw=betweenness_all_net$Value)
value_between_df


#13 aktera sa najvecom vrednoscu za betweenness
Top_in_value_between_df <- value_between_df %>% 
    select(node_id,betw)%>%
    arrange(desc(value_between_df$betw)) %>%
    head(n=13)
View(Top_in_value_between_df)
#cuvanje rezultata za kasniji prikaz 
write.csv(Top_in_value_between_df,
          file = 'output/Top_in_value_between_df.csv')








max(betweenness_all_net$Value)
#0.1088632
which(betweenness_all_net$Value==max(betweenness_all_net$Value))
#22
value_between_df <-data.frame(node_id=as.integer(V(value_3_graph)$name),
                              betw=betweenness_all_net$Value)
value_between_df


#13 aktera sa najvecom vrednoscu za betweenness
Top_in_value_between_df <- value_between_df %>% 
    select(node_id,betw)%>%
    arrange(desc(value_between_df$betw)) %>%
    head(n=13)
View(Top_in_value_between_df)
#cuvanje rezultata za kasniji prikaz 
write.csv(Top_in_value_between_df,
          file = 'output/Top_in_value_between_df.csv')

?betweenness


#The vertex and edge betweenness are (roughly) defined by the number
#of geodesics (shortest paths) going through a vertex or an edge.
#betweenness centrality measures a vertex's control over information 
#flowing between all pairs of nodes in its component

advice_3_graph
betweenness_all_net
max(betweenness_all_net$Advice)
#0.233712
which(betweenness_all_net$Advice==max(betweenness_all_net$Advice))
#68
advice_between_df <-data.frame(node_id=as.integer(V(advice_3_graph)$name),
                              betw=betweenness_all_net$Advice)
advice_between_df


#13 aktera sa najvecom vrednoscu za betweenness
Top_in_advice_between_df <- advice_between_df %>% 
    select(node_id,betw)%>%
    arrange(desc(advice_between_df$betw)) %>%
    head(n=13)
View(Top_in_advice_between_df)
#cuvanje rezultata za kasniji prikaz 
write.csv(Top_in_advice_between_df,
          file = 'output/Top_in_advice_between_df.csv')


#The vertex and edge betweenness are (roughly) defined by the number
#of geodesics (shortest paths) going through a vertex or an edge.
#betweenness centrality measures a vertex's control over information 
#flowing between all pairs of nodes in its component

awareness_3_graph
betweenness_all_net
max(betweenness_all_net$Awareness)
#0.09425947
which(betweenness_all_net$Awareness==max(betweenness_all_net$Awareness))
#68
awareness_between_df <-data.frame(node_id=as.integer(V(awareness_3_graph)$name),
                               betw=betweenness_all_net$Awareness)
awareness_between_df


#13 aktera sa najvecom vrednoscu za betweenness
Top_in_awareness_between_df <- awareness_between_df %>% 
    select(node_id,betw)%>%
    arrange(desc(awareness_between_df$betw)) %>%
    head(n=13)
View(Top_in_awareness_between_df)
#cuvanje rezultata za kasniji prikaz 
write.csv(Top_in_awareness_between_df,
          file = 'output/Top_in_awareness_between_df.csv')






#15.EIGEN CENTRALITY
#centrality of each actor is proportional to the sum of 
#the centralities of those actors to whom he or she is connected

?eigen_centrality
#In general, vertices with high eigenvector centralities are those
#which are connected to many other vertices which are, 
#in turn, connected to many others (and so on).
#(The perceptive may realize that this implies that the largest
#values will be obtained by individuals in large cliques 
#(or high-density substructures).

#igraph experiences problems when computing weighted eigenvector 
# centrality for directed graphs. Therefore, this option can be effectively  
# used only for undirected graphs
##
#EIGEN CENTRALITY FOR ADVICE NETWORK
##


eigen_advice_3_graph_no_weights <-eigen_centrality(advice_3_graph,
                                        directed = TRUE,
                                        scale = TRUE,
                                        weights = NA)
str(eigen_advice_3_graph_no_weights)
eigen_advice_3_graph_no_weights<-eigen_advice_3_graph_no_weights$vector
eigen_advice_3_graph_no_weights
max(eigen_advice_3_graph_no_weights)
summary(eigen_advice_3_graph_no_weights)
# Koji akter ima najvecu vrednost za  eigen centrality  
which(eigen_advice_3_graph_no_weights == max(eigen_advice_3_graph_no_weights))
#cvor 29 ima najvecu vrednost za EIGEN CENTRALITY
eigen_no_weights_advice_3_df <- data.frame(node_id=as.integer(V(advice_3_graph)$name),
                                eigen=eigen_advice_3_graph_no_weights)
View(eigen_no_weights_advice_3_df)
summary(eigen_no_weights_advice_3_df)
eigen_no_weights_advice_3_df$eigen
#13 aktera sa najvecom vrednoscu za EIGEN CENTRALITY
Top_in_eigen_no_weights_advice_3_df <- eigen_no_weights_advice_3_df %>% 
    select(node_id,eigen)%>%
    arrange(desc(eigen_no_weights_advice_3_df$eigen)) %>%
    head(n=13)
View(Top_in_eigen_no_weights_advice_3_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_in_eigen_no_weights_advice_3_df,
          file = 'output/Top_in_eigen_no_weights_advice_3_df.csv')


#Uzela sam u obzir tezine, mada to necu prikazivati u radu, jer
#kako razumem na usmerenim mrezama ne bi trebala da se primenjuje ova
#opcija weights

eigen_advice_3_graph <-eigen_centrality(advice_3_graph,
                                        directed = TRUE,
                                        scale = TRUE,
                                        weights = NULL)
eigen_advice_3_graph<-eigen_advice_3_graph$vector
eigen_advice_3_graph
eigen_advice_3_df <- data.frame(node_id=as.integer(V(advice_3_graph)$name),
                                  eigen=eigen_advice_3_graph)
View(eigen_advice_3_df)
summary(eigen_advice_3_graph)
eigen_advice_3_df$eigen
#13 aktera sa najvecom vrednoscu za shortest paths
Top_in_eigen_advice_3_df <- eigen_advice_3_df %>% 
    select(node_id,eigen)%>%
    arrange(desc(eigen_advice_3_df$eigen)) %>%
    head(n=13)
View(Top_in_eigen_advice_3_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_in_eigen_advice_3_df,
          file = 'output/Top_in_eigen_advice_3_df.csv')


max(eigen_advice_3_df$eigen)
#1
# Koji akter ima najvecu vrednost za  eigen centrality  
which(eigen_advice_3_graph == max(eigen_advice_3_graph))
#cvor 29 ima najvecu vrednost za EIGEN CENTRALITY




##
#16.Closeness
##
#Cloness centrality measures how many steps is required to
#access every other vertex from a given vertex.
#I calculate it on gigant component, where all nodes are reachable
?closeness
information_or_advice_3_graph_gcomp
E(information_or_advice_3_graph_gcomp)$weight



# You can think of in-closeness centrality as the average number of steps 
# one would have to make to get TO a given node FROM all other reachable
# nodes in the network. Out-closeness centrality, not surprisingly, 
# measures the same thing with the directionality reversed: the average
# number of steps FROM the given node TO any other reachable node in the 
# network.

# We can also include edge attributes (weights) in the calculation of closeness.
# It is important to note that edge "weights are used for calculating weighted 
# shortest paths, so they are interpreted as distances".
# In my case higher values for the $weight attribute mean more friquent advice 
#realtionship, that is, lower distance. So, to appropriately calculate weighted 
#closeness, it is  better to take reciprocal value of the $weight  attribute:
in_closeness_inf_or_adv_3_graph<-closeness(information_or_advice_3_graph_gcomp,
          vids = V(information_or_advice_3_graph_gcomp),
          mode = c("in"),
          weights = 1/E(information_or_advice_3_graph_gcomp)$weight,
          normalized = TRUE)

in_closeness_inf_or_adv_3_graph
max(in_closeness_inf_or_adv_3_graph)
which(in_closeness_inf_or_adv_3_graph==max(in_closeness_inf_or_adv_3_graph))
#8 
out_closeness_inf_or_adv_3_graph<-closeness(information_or_advice_3_graph_gcomp,
                                           vids = V(information_or_advice_3_graph_gcomp),
                                           mode = c("out"),
                                           weights = 1/E(information_or_advice_3_graph_gcomp)$weight,
                                           normalized = TRUE)
out_closeness_inf_or_adv_3_graph
max(out_closeness_inf_or_adv_3_graph)
which(out_closeness_inf_or_adv_3_graph==max(out_closeness_inf_or_adv_3_graph))
#20
inf_or_adv_closeness_df <-data.frame(node_id=as.integer(V(information_or_advice_3_graph_gcomp)$name),
                              in_cl=in_closeness_inf_or_adv_3_graph,
                              out_cl=out_closeness_inf_or_adv_3_graph)

inf_or_adv_closeness_df                                 
str(inf_or_adv_closeness_df)
summary(inf_or_adv_closeness_df)
in_closeness_colors_inf_or_adv = attr_based_color_gradient(g_attr =inf_or_adv_closeness_df$in_cl , 
                                                    pal_end_points = c('grey80', 'dark red'))
in_closeness_colors_inf_or_adv


pdf("graphs/information_or_advice_3_graph_gcomp_color_in_closeness.pdf")
plot(information_or_advice_3_graph_gcomp,
     edge.arrow.size=.10,
     edge.width = E(information_or_advice_3_graph_gcomp)$weight/20,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_lgl(information_or_advice_3_graph_gcomp),
     vertex.size=3,
     vertex.color=in_closeness_colors_inf_or_adv,
     vertex.label.cex = 0.4,
     vertex.frame.color=in_closeness_colors_inf_or_adv,
     vertex.label.color="yellow",
     asp = 0,
     margin = 0,
     main="Network I->Information or advice-gigant component",
     sub="Color denotes in closeness")
dev.off()

inf_or_adv_closeness_df$in_cl
#13 aktera sa najvecom vrednoscu za closeness
Top_in_inf_or_adv_in_closeness_df <- inf_or_adv_closeness_df %>% 
    select(node_id,in_cl)%>%
    arrange(desc(inf_or_adv_closeness_df$in_cl)) %>%
    head(n=13)
View(Top_in_inf_or_adv_in_closeness_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_in_inf_or_adv_in_closeness_df,
          file = 'output/Top_in_inf_or_adv_in_closeness_df.csv')




Top_in_inf_or_adv_out_closeness_df <- inf_or_adv_closeness_df %>% 
    select(node_id,out_cl)%>%
    arrange(desc(inf_or_adv_closeness_df$out_cl)) %>%
    head(n=13)
View(Top_in_inf_or_adv_out_closeness_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_in_inf_or_adv_out_closeness_df,
          file = 'output/Top_in_inf_or_adv_out_closeness_df.csv')


#Cloness centrality measures how many steps is required to
#access every other vertex from a given vertex.
#I calculate it on gigant component, where all nodes are reachable
?closeness
advice_3_graph_gcomp
E(advice_3_graph_gcomp)$weight



# You can think of in-closeness centrality as the average number of steps 
# one would have to make to get TO a given node FROM all other reachable
# nodes in the network. Out-closeness centrality, not surprisingly, 
# measures the same thing with the directionality reversed: the average
# number of steps FROM the given node TO any other reachable node in the 
# network.

# We can also include edge attributes (weights) in the calculation of closeness.
# It is important to note that edge "weights are used for calculating weighted 
# shortest paths, so they are interpreted as distances".
# In my case higher values for the $weight attribute mean more friquent advice 
#realtionship, that is, lower distance. So, to appropriately calculate weighted 
#closeness, it is  better to take reciprocal value of the $weight  attribute:
in_closeness_adv_3_graph<-closeness(advice_3_graph_gcomp,
                                           vids = V(advice_3_graph_gcomp),
                                           mode = c("in"),
                                           weights = 1/E(advice_3_graph_gcomp)$weight,
                                           normalized = TRUE)

in_closeness_adv_3_graph
max(in_closeness_adv_3_graph)
which(in_closeness_adv_3_graph==max(in_closeness_adv_3_graph))
#68 
out_closeness_adv_3_graph<-closeness(advice_3_graph_gcomp,
                                            vids = V(advice_3_graph_gcomp),
                                            mode = c("out"),
                                            weights = 1/E(advice_3_graph_gcomp)$weight,
                                            normalized = TRUE)
out_closeness_adv_3_graph
max(out_closeness_adv_3_graph)
which(out_closeness_adv_3_graph==max(out_closeness_adv_3_graph))
#68
adv_closeness_df <-data.frame(node_id=as.integer(V(advice_3_graph_gcomp)$name),
                                     in_cl=in_closeness_adv_3_graph,
                                     out_cl=out_closeness_adv_3_graph)



adv_closeness_df
#13 aktera sa najvecom vrednoscu za closeness
Top_in_adv_in_closeness_df <- adv_closeness_df %>% 
    select(node_id,in_cl)%>%
    arrange(desc(adv_closeness_df$in_cl)) %>%
    head(n=13)
View(Top_in_adv_in_closeness_df)
#cuvanje df za kasniji prikaz 
write.csv(Top_in_adv_in_closeness_df,
          file = 'output/Top_in_adv_in_closeness_df.csv')



Top_in_adv_out_closeness_df <- adv_closeness_df %>% 
    select(node_id,out_cl)%>%
    arrange(desc(adv_closeness_df$out_cl)) %>%
    head(n=13)
View(Top_in_adv_out_closeness_df)
#cuvanje df za kasniji prikaz 

write.csv(Top_in_adv_out_closeness_df,
          file = 'output/Top_in_adv_out_closeness_df.csv')




#
# 2.5 Summary of centrality metrics
#

# I'll first construct a data frame with the vertices as rows 
# and the centrality scores as columns:
eigen_information_or_advice_3_graph<-eigen_centrality(information_or_advice_3_graph,
                 directed = TRUE,
                 scale = TRUE,
                 weights = NA)
eigen_information_or_advice_3_graph<-eigen_information_or_advice_3_graph$vector
information_or_advice_3_graph
information_or_advice_3_centrality_all <- data.frame(node_id=as.integer(V(information_or_advice_3_graph)$name),
                                        in_degree=degree(information_or_advice_3_graph,
                                                         mode = 'in',
                                                         loops = TRUE,
                                                         normalized = TRUE),
                                        out_degree=degree(information_or_advice_3_graph,
                                                          mode = 'out', 
                                                          loops = TRUE,
                                                          normalized = TRUE),
                                        betweenness=betweenness_all_net$Information_or_advice,
                                        eigen=eigen_information_or_advice_3_graph)
View(information_or_advice_3_centrality_all)
# Note that to compute in- and out-closeness we needed to remove a few vertices,
# so, we have to add these scores in a separate step, using the merge() function
?merge
information_or_advice_3_centrality_all <- merge(x = information_or_advice_3_centrality_all, 
                                    y = inf_or_adv_closeness_df,
                                   by = 'node_id',
                                   all = TRUE)
str(information_or_advice_3_centrality_all)
View(information_or_advice_3_centrality_all)

# Now we can sort the data frame to find the most central actors 
# according to different centrality measures we have computed.

# Sort by betwenness
View(information_or_advice_3_centrality_all[order(information_or_advice_3_centrality_all$betweenness,
                                                  decreasing = TRUE),])
# Sort by eigen
View(information_or_advice_3_centrality_all[order(information_or_advice_3_centrality_all$eigen,
                                                  decreasing = TRUE),])

# Note that Eigenvector c. seems to be correlated with in-degree and in-closeness




information_or_advice_3_centrality_all[,-1]

# We need to compute pairwise correlations, that is, to generate a table
# with correlation values for each centrality measures pair.
# To determine how to compute these correlations, we need to check if the 
# assumption of normal distribution applies to our centrality measures:
apply(information_or_advice_3_centrality_all[,-1], 2, shapiro.test)
apply(information_or_advice_3_centrality_all[,-1], 2, qqnorm)

# Not all metrics are not normally distributed . 
#The t-test is used to establish if the correlation coefficient is significantly
#different from zero, and, hence that there is evidence of an association 
#between the two variables. There is then the underlying assumption that the
#data is from a normal distribution sampled randomly. 
#If this is not true, the conclusions may well be invalidated.
#If this is the case, then it is better to use Spearman's coefficient 
#of rank correlation (for non-parametric variables)
# So, better compute Spearman correlation coefficient
centrality_cor_inf_or_adv <- cor(information_or_advice_3_centrality_all[,-1], 
                       use='complete.obs', # has to be set as we have a few NAs
                       method = 'spearman')
centrality_cor_inf_or_adv
#https://cambridge-intelligence.com/keylines-faqs-social-network-analysis/
#https://www.researchgate.net/publication/44633441_How_Correlated_Are_Network_Centrality_Measures
# Not that easy to read and follow...
# We will use the corrpolot() function from the *corrplot* R package 
# to visually represent the computed correlations table
#install.packages('corrplot')
library(corrplot)

# Interpretation:
# In-degree and out-degree have high positive correlation (rho = 0.82),
# indicating that asking someone for advice is not that often reciprocated
# (i.e., if A asks B for advice, it is very likely that B would 
# also ask A for advice)

# In-degree is highly positively correlated with eigenvectorcentrality (EC)->(0.85)
#and in-closeness(0.67),  
#and medium correlated with betweenness (0.57) and out_clossenes(0,58).
#This suggests that those who have been
# asked by many others for advice have good network position that is often
# associated with having power and influence in the network. 


#Out-degree is highly correlated with out-closeness(0.73) and eigenvector(0,72),
#and to a lesser extent with betweeness(0.62). This implies that those who have
#asked many others for advice have higher chances to act as intermediaries in
#the network (betweenness) and can reach many others through outbound edges,
#also it is highly  associated with having power and influence(eigen)

#There is medium positive correlation between in-closeness and
# eigenvector centrality. It suggests that those who are close to many others 
#by being asked for advice are  those who have medimly connected friends 
# (eigenvector).Also there is high correlation with betweenes(0,76)
#It suggests that those who are close to many others 
#by being asked for advice are also strong intermediaries in
#the network,. In-closeness are also highly correlated with in-degree. 


png("graphs/centrality_cor_inf_or_adv.png")
corrplot(corr = centrality_cor_inf_or_adv, 
         type = "upper", 
         diag = FALSE,
         addCoef.col = "black")
dev.off()

# Note:
# To examine the plotting options of the corrplot function
# check, for example, this page:
# https://rpubs.com/melike/corrplot
png("graphs/value_1_graph_disagree_edge_denotes_weight_node_color_gender.png")
plot(value_3_graph_disagree,
     edge.arrow.size=.20,
     edge.width = E(value_3_graph_disagree)$weight/3,
     edge.arrow.width = 0.25,
     edge.arrow.color =E(value_3_graph_disagree)$weight,
     edge.color=E(value_3_graph_disagree)$weight,
     layout=layout_nicely(value_3_graph_disagree),
     vertex.size=degree(value_3_graph_disagree, mode = "in")*2.5,
     vertex.color=V(value_3_graph_disagree)$Gender,
     vertex.label.cex = 0.4,
     vertex.frame.color=V(value_3_graph_disagree)$Gender,
     vertex.label.color="yellow",
     asp = -0.1,
     margin = 0,
     main="Network II->value of each others’ expertise-just low weight(1,2)",
     sub="yelow node=male\n
    blue node=female
    weight: orange= 1: Strongly Disagree; and blue= 2: Disagree;(edge colors)\n
    vertex size denotes degree in")
dev.off()

awareness_3_graph_disagree

pdf("graphs/awareness_3_graph_disagree.pdf")
plot(awareness_3_graph_disagree,
     edge.arrow.size=.20,
     edge.width = E(awareness_3_graph_disagree)$weight/5,
     edge.arrow.width = 0.25,
     edge.arrow.color =E(awareness_3_graph_disagree)$weight,
     edge.color=E(awareness_3_graph_disagree)$weight,
     layout=layout_with_lgl(awareness_3_graph_disagree),
     vertex.size=degree(awareness_3_graph_disagree, mode = "in")/2,
     vertex.color=V(awareness_3_graph_disagree)$Location,
     vertex.label.cex = 0.4,
     vertex.frame.color=V(awareness_3_graph_disagree)$Location,
     vertex.label.color="yellow",
     asp = -0.1,
     margin = 0,
     main="Network IV->awareness of each others’ expertise-\n
     just low weight(1,2,3)",
     sub=" #orange=1: Paris; blue=#2: Frankfurt;green=#3: Warsaw; and yelow= #4:Geneva\n
     nodes color
    weight: orange= 1: Strongly Disagree; blue= 2:Disagree\n
    and green=3:Somewhat Disagree;(edge colors)")
dev.off()
#1: Strongly Disagree;  
#2: Disagree; 
#3: Somewhat Disagree

information_or_advice_3_graph_low_weight
png("graphs/information_or_advice_3_graph_low_weight.png",
    width = 1100,
    height = 1100)
plot(information_or_advice_3_graph_low_weight,
     edge.arrow.size=.8,
     edge.width = E(information_or_advice_3_graph_low_weight)$weight/5,
     edge.arrow.width = 0.5,
     edge.arrow.color =E(information_or_advice_3_graph_low_weight)$weight,
     edge.color=E(information_or_advice_3_graph_low_weight)$weight,
     layout=layout_with_lgl(information_or_advice_3_graph_low_weight),
     vertex.size=degree(information_or_advice_3_graph_low_weight,
                          mode=c("in"),
                          loops=TRUE,
                        normalized = TRUE)*20,
     vertex.color="#b30047",
     vertex.label.cex =degree(information_or_advice_3_graph_low_weight,
                              mode=c("in"),
                              loops=TRUE,
                              normalized = TRUE)*3,
     vertex.frame.color="#b30047",
     vertex.label.color="yellow",
     asp = -0.1,
     margin = 0,
     main="Network I->information or advice low weight(1,2)",
     sub="yelow=1:Never & blue=2:Seldom \n
       weight>3
     size denotes degree in")
dev.off()

value_3_graph_gcomp
pdf("graphs/value_3_graph_gcomp.pdf")
plot(value_3_graph_gcomp,
     edge.arrow.size=.20,
     edge.width = E(value_3_graph_gcomp)$weight/3,
     edge.arrow.width = 0.3,
     layout=layout_with_lgl(value_3_graph_gcomp),
     vertex.size=5,
     vertex.color=V(value_3_graph_gcomp)$Gender,
     vertex.label.cex = 0.6,
     vertex.frame.color=V(value_3_graph_gcomp)$Gender,
     vertex.label.color="#ffff66",
     asp = -0.1,
     margin = 0,
     main="Network II-gigant component->value of each others’ expertise",
     sub="yelow node=male\n
    blue node=female")
dev.off()




pdf("graphs/value_1_graph_edges.pdf")
plot(value_1_graph,
     edge.arrow.size=.20,
     edge.width = E(value_1_graph)$weight/5,
     edge.arrow.width = 0.25,
     edge.arrow.color =E(value_1_graph)$weight,
     edge.color=E(value_1_graph)$weight,
     layout=layout_with_drl(value_1_graph),
     vertex.size=4,
     vertex.color=V(value_1_graph)$Gender,
     vertex.label.cex = 0.4,
     vertex.frame.color=V(value_1_graph)$Gender,
     vertex.label.color="#ffff66",
     asp = -0.1,
     margin = 0,
     main="Network II->value of each others’ expertise",
     sub="yelow node=male\n
    blue node=female")
dev.off()

information_or_advice_2_graph



png("graphs/information_or_advice_3_graph.png",
    width = 1100,
    height = 1100)
plot(information_or_advice_3_graph,
     edge.arrow.size=.20,
     edge.width = E(information_or_advice_3_graph)$weight/5,
     edge.arrow.width = 0.25,
     edge.arrow.color =E(information_or_advice_3_graph)$weight,
     edge.color=E(information_or_advice_3_graph)$weight,
     layout=layout_with_kk(information_or_advice_3_graph),
     vertex.size=degree(information_or_advice_3_graph, mode="in")*0.9,
     vertex.color="pink4",
     vertex.label.cex = sqrt(degree(information_or_advice_3_graph, mode="in"))/2,
     vertex.frame.color="pink4",
     vertex.label.color="yellow",
     asp = -0.1,
     margin = 0,
     main="Network I->information or advice",
     sub="weight>3")
dev.off()


pdf("graphs/information_or_advice_2_graph_strength.pdf")
plot(information_or_advice_2_graph,
     edge.arrow.size=.20,
     edge.width = E(information_or_advice_2_graph)$weight/5,
     edge.arrow.width = 0.25,
     edge.arrow.color =E(information_or_advice_2_graph)$weight,
     edge.color=E(information_or_advice_2_graph)$weight,
     layout=layout_with_lgl(information_or_advice_2_graph),
     vertex.size=strength_inf_or_advice/5,
     vertex.color="pink4",
     vertex.label.cex = 0.25,
     vertex.frame.color="pink4",
     vertex.label.color="yellow",
     asp = -0.1,
     margin = 0,
     main="Network I->information or advice",
     sub="weight>3
     size denotes weight strength")
dev.off()




png("graphs/information_or_advice_3_graph_between.png",
    width = 1100,
    height = 1100)
plot(information_or_advice_3_graph,
     edge.arrow.size=.20,
     edge.width = E(information_or_advice_3_graph)$weight/5,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_kk(information_or_advice_3_graph),
     vertex.size=betweenness(information_or_advice_3_graph, 
                             v=V(information_or_advice_3_graph),
                             directed = TRUE,
                             normalized = TRUE)*120,
     vertex.color="pink4",
     vertex.label.cex = 1,
     vertex.frame.color="pink4",
     vertex.label.color="yellow",
     asp = -0.1,
     margin = 0,
     main="Network I->information or advice",
     sub="weight>3
     size denotes betweeness")
dev.off()

png("graphs/advice_3_graph_between.png",
    width = 1100,
    height = 1100)
plot(advice_3_graph,
     edge.arrow.size=.20,
     edge.width = E(advice_3_graph)$weight/5,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_kk(advice_3_graph),
     vertex.size=betweenness(advice_3_graph, 
                             v=V(advice_3_graph),
                             directed = TRUE,
                             normalized = TRUE)*120,
     vertex.color="pink4",
     vertex.label.cex = 1,
     vertex.frame.color="pink4",
     vertex.label.color="yellow",
     asp = -0.1,
     margin = 0,
     main="Network I->advice_3_graph",
     sub="weight>3
     size denotes betweeness")
dev.off()


??plot
pdf("graphs/value_1_graph_level.pdf")
plot(value_1_graph,
     edge.arrow.size=.10,
     edge.width = E(value_1_graph)$weight/20,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_drl(value_1_graph),
     vertex.size=4,
     vertex.color=V(value_1_graph)$The_organisational_level,
     vertex.label.cex = 0.4,
     vertex.frame.color=V(value_1_graph)$The_organisational_level,
     vertex.label.color="yellow",
     asp = 0,
     margin = 0,
     main="Network II->value of each others’ expertise",
     sub="Yelow node=1:Research Assistant,Light blue node=2:Junior Consultant,\n
    Green=3:Senior Consultant,Light yelow=4: Managing Consultant,\n
    Dark blue=5:Partner")
dev.off()


V(value_1_graph)$The_organisational_level
#1:Research Assistant;
#2: Junior Consultant;
#3: Senior Consultant; 
#4: Managing Consultant; 
#5: Partner




#1: Very Infrequently; 
#2: Infrequently; 
#3: Somewhat Infrequently;
V(advice_3_graph_low_weight)$Location
advice_3_graph_low_weight
pdf("graphs/advice_3_graph_low_weight_edges_color_weight_node_color_location.pdf")
plot(advice_3_graph_low_weight,
     edge.arrow.size=.10,
     edge.width = E(advice_3_graph_low_weight)$weight/20,
     edge.arrow.width = 0.25,
     edge.arrow.color =E(advice_3_graph_low_weight)$weight,
     edge.color=E(advice_3_graph_low_weight)$weight,
     layout=layout_with_lgl(advice_3_graph_low_weight),
     vertex.size=4,
     vertex.color=V(advice_3_graph_low_weight)$Location,
     vertex.label.cex = 0.4,
     vertex.frame.color=V(advice_3_graph_low_weight)$Location,
     vertex.label.color="yellow",
     asp = 0,
     margin = 0,
     main="Network II->information or advice",
     sub="Location:
       orange= 1: Paris; blue=2: Frankfurt;green=3: Warsaw; light yellow=4: Geneva
     edge color:
     orange=1: Very Infrequently; blue=2: Infrequently; green=3: Somewhat Infrequently")
dev.off()

pdf("graphs/information_or_advice_1_graph_level.pdf")
plot(information_or_advice_1_graph,
     edge.arrow.size=.10,
     edge.width = E(information_or_advice_1_graph)$weight/20,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_drl(information_or_advice_1_graph),
     vertex.size=4,
     vertex.color=V(information_or_advice_1_graph)$The_organisational_level,
     vertex.label.cex = 0.4,
     vertex.frame.color=V(information_or_advice_1_graph)$The_organisational_level,
     vertex.label.color="yellow",
     asp = 0,
     margin = 0,
     main="Network II->information or advice",
     sub="Yelow node=1:Research Assistant,Light blue node=2:Junior Consultant,\n
    Green=3:Senior Consultant,Light yelow=4: Managing Consultant,\n
    Dark blue=5:Partner")
dev.off()


awareness_1_graph
pdf("graphs/awareness_1_graph_level.pdf")
plot(awareness_1_graph,
     edge.arrow.size=.10,
     edge.width = E(awareness_1_graph)$weight/20,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_lgl(awareness_1_graph),
     vertex.size=4,
     vertex.color=V(awareness_1_graph)$The_organisational_level,
     vertex.label.cex = 0.4,
     vertex.frame.color=V(awareness_1_graph)$The_organisational_level,
     vertex.label.color="yellow",
     asp = 0,
     margin = 0,
     main="Network II->awarenness of expertize",
     sub="Yelow node=1:Global Dept Manager,Light blue node=2:Local Dept Manager,\n
    Green=3:Project Leader,Light yelow=4: Researcher")
dev.off()
V(awareness_1_graph)$The_organisational_level
#1: Global Dept Manager; 
#2: Local Dept Manager; 
#3: Project Leader; 
#4: Researcher
V(advice_1_graph)$The_organisational_level==V(awareness_1_graph)$The_organisational_level
advice_1_graph
pdf("graphs/advice_1_graph_level.pdf")
plot(advice_1_graph,
     edge.arrow.size=.10,
     edge.width = E(advice_1_graph)$weight/20,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_lgl(advice_1_graph),
     vertex.size=4,
     vertex.color=V(advice_1_graph)$The_organisational_level,
     vertex.label.cex = 0.4,
     vertex.frame.color=V(advice_1_graph)$The_organisational_level,
     vertex.label.color="yellow",
     asp = 0,
     margin = 0,
     main="Network II->advice",
     sub="Yelow node=1:Global Dept Manager,Light blue node=2:Local Dept Manager,\n
    Green=3:Project Leader,Light yelow=4: Researcher")
dev.off()
advice_1_graph
V(advice_1_graph)$Location
#1: Paris; 
#2: Frankfurt;
#3: Warsaw; 
#4: Geneva
png("graphs/advice_3_graph_location.png",
    width = 1100,
    height = 1100)
plot(advice_3_graph,
     edge.arrow.size=.30,
     edge.width = E(advice_3_graph)$weight/20,
     edge.arrow.width = 0.5,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_lgl(advice_3_graph),
     vertex.size=4,
     vertex.color=V(advice_3_graph)$Location,
     vertex.label.cex = 0.7,
     vertex.frame.color=V(advice_3_graph)$Location,
     vertex.label.color="black",
     asp = 0,
     margin = 0,
     main="Network II->advice",
     sub="dark yelow=1: Paris; 
          light blue=2: Frankfurt;
          green=3: Warsaw; 
          light yelow=4: Geneva")
dev.off()


advice_1_graph
V(advice_1_graph)$Tenure
#tenure (1: 1-12 months; 
#2: 13-36 months; 
#3: 37-60 months; 
#4: 61+ months) 
pdf("graphs/advice_1_graph_tenure.pdf")
plot(advice_1_graph,
     edge.arrow.size=.10,
     edge.width = E(advice_1_graph)$weight/20,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_lgl(advice_1_graph),
     vertex.size=4,
     vertex.color=V(advice_1_graph)$Tenure,
     vertex.label.cex = 0.4,
     vertex.frame.color=V(advice_1_graph)$Tenure,
     vertex.label.color="yellow",
     asp = 0,
     margin = 0,
     main="Network II->advice",
     sub="dark yelow=1: 1-12 months; 
          light blue=2: 13-36 months;
          green=3:37-60 months; 
          light yelow=4: 61+ months")
dev.off()
degree(advice_1_graph, mode="out")
pdf("graphs/advice_1_graph_tenure_size_degree_out.pdf")
plot(advice_1_graph,
     edge.arrow.size=.10,
     edge.width = E(advice_1_graph)$weight/20,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_lgl(advice_1_graph),
     vertex.size=degree(advice_1_graph, mode="out")/10,
     vertex.color=V(advice_1_graph)$Tenure,
     vertex.label.cex = 0.4,
     vertex.frame.color=V(advice_1_graph)$Tenure,
     vertex.label.color="yellow",
     asp = 0,
     margin = 0,
     main="Network II->advice",
     sub="dark yelow=1: 1-12 months; 
          light blue=2: 13-36 months;
          green=3:37-60 months; 
          light yelow=4: 61+ months")
dev.off()
pdf("graphs/advice_1_graph_tenure_size_degree_in.pdf")
plot(advice_1_graph,
     edge.arrow.size=.10,
     edge.width = E(advice_1_graph)$weight/20,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_lgl(advice_1_graph),
     vertex.size=degree(advice_1_graph, mode="in")/10,
     vertex.color=V(advice_1_graph)$Tenure,
     vertex.label.cex = 0.4,
     vertex.frame.color=V(advice_1_graph)$Tenure,
     vertex.label.color="yellow",
     asp = 0,
     margin = 0,
     main="Network II->advice",
     sub="dark yelow=1: 1-12 months; 
          light blue=2: 13-36 months;
          green=3:37-60 months; 
          light yelow=4: 61+ months")
dev.off()


eigen_colors_advice = attr_based_color_gradient(g_attr =advice_eigen$ei, 
                                                pal_end_points = c('grey80', 'dark red'))

eigen_colors_advice

pdf("graphs/advice_1_graph_color_eigen.pdf")
plot(advice_1_graph,
     edge.arrow.size=.10,
     edge.width = E(advice_1_graph)$weight/20,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_lgl(advice_1_graph),
     vertex.size=degree(advice_1_graph, mode="in")/10,
     vertex.color=eigen_colors_advice,
     vertex.label.cex = 0.4,
     vertex.frame.color=eigen_colors_advice,
     vertex.label.color="yellow",
     asp = 0,
     margin = 0,
     main="Network II->advice")
dev.off()


betweenness_colors_advice = attr_based_color_gradient(g_attr =advice_betweenness$bw , 
                                                      pal_end_points = c('grey80', 'dark red'))
betweenness_colors_advice
pdf("graphs/advice_1_graph_color_betweenness.pdf")
plot(advice_1_graph,
     edge.arrow.size=.10,
     edge.width = E(advice_1_graph)$weight/20,
     edge.arrow.width = 0.25,
     edge.arrow.color =4,
     edge.color="gray56",
     layout=layout_with_lgl(advice_1_graph),
     vertex.size=degree(advice_1_graph, mode="in")/10,
     vertex.color=betweenness_colors_advice,
     vertex.label.cex = 0.4,
     vertex.frame.color=betweenness_colors_advice,
     vertex.label.color="yellow",
     asp = 0,
     margin = 0,
     main="Network II->advice")
dev.off()


