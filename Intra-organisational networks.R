library(igraph)
#Ovaj paket omogucava da se napravi pajp line
library(magrittr)
library(ggplot2)
library(tidyr)
library(dplyr)
getwd()
#This dataset contains four networks which are intra-organizational networks. 
#Two are from a consulting company:
                               #(46 employees) 
#and two are from a research team in a manufacturing company:
                              #(77 employees).
#These networks was used by Cross and Parker (2004).


#In the first network, the ties are differentiated on a scale from 0 to 5 
#in terms of frequency of information or advice requests
#(“Please indicate how often you have turned to this person for information
#or advice on work-related topics in the past three months”). 
#0: I Do Not Know This Person; 
#1: Never; 
#2: Seldom; 
#3: Sometimes; 
#4: Often; and 
#5:Very Often

information_or_advice_1<-read.csv("C:/Users/Nikola/Desktop/organization/information or advice.csv",
                                  header =TRUE)
information_or_advice_1
str(information_or_advice_1)
dim(information_or_advice_1)
apply(information_or_advice_1, 2, unique)
sum(apply(information_or_advice_1, 2, is.na))
is.data.frame(information_or_advice_1)
#879 ivica i 46 cvorova


#In addition to the relational data, the dataset also contains information 
#about the people (nodal attributes). 
#The following attributes are known for the consultancy firm:

#the organisational level
#1: Research Assistant;
#2: Junior Consultant;
#3: Senior Consultant; 
#4: Managing Consultant; 
#5: Partner, 

#gender #1: male; 
        #2: female, 

#region #1: Europe; 
        #2: USA, and 

#location
        #1: Boston; 
        #2: London; 
        #3: Paris; 
        #4: Rome; 
        #5: Madrid; 
        #6: Oslo;
        #7: Copenhagen

the_organisational_level<- c(3, 4, 1, 4, 4, 4, 3, 5, 2, 4, 4,
                             4, 2, 4, 3,3, 1, 3, 3, 5, 4, 2, 3,
                             1, 2, 4, 2, 3, 2, 1,
                             4, 2, 4, 4, 2, 4, 5,
                             3, 3, 4, 1, 1, 4, 4,
                             5,2)
gender<-c(1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2,
          1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1,
          2, 1, 1, 1, 1, 1, 1, 1, 1,2) 
region<-c(1,2,1,1,2,2,2,1,1,1,2,1,2,2,1,1,2,2,2,
          2,2,1,2,2,2,2,2,2,2,2,1,1,1,1,2,2,1,2,2,1,1,1,1,2,2,1)
location<-c(3, 1, 7, 4, 1, 1, 1, 3, 7, 3, 1, 3, 1, 1,
            6, 3, 1, 1, 1, 1, 1, 6, 1, 1, 1, 1, 1, 1,
            1, 1, 4, 3, 3, 5, 1, 1, 3, 1, 1, 6, 2, 5,
            3, 1, 1, 7)
#In the first network, the ties are differentiated on a scale from 0 to 5 
#in terms of frequency of information or advice requests
#(“Please indicate how often you have turned to this person for information
#or advice on work-related topics in the past three months”). 
                               #0: I Do Not Know This Person; 
                               #1: Never; 
                               #2: Seldom; 
                               #3: Sometimes; 
                               #4: Often; and 
                               #5:Very Often

#GRAPH format
?graph_from_data_frame
information_or_advice_1_graph<-graph_from_data_frame(information_or_advice_1,
                                                     directed = TRUE)
V(information_or_advice_1_graph)$name
E(information_or_advice_1_graph)

information_or_advice_1_graph
#DNW- 46 879 -- 
#links per node

round(879/46, digits = 2)
#19.11
round(edge_density(information_or_advice_1_graph,loops=TRUE),digits = 2)
#0.42- density


as.numeric(V(information_or_advice_1_graph)$name)
inf_or_adv_name<-as.numeric(V(information_or_advice_1_graph)$name)
is.numeric(inf_or_adv_name)

information_or_advice_1_graph<-delete_vertex_attr(information_or_advice_1_graph,
                                                  name="name")
V(information_or_advice_1_graph)$name<-inf_or_adv_name
information_or_advice_1_graph

which_multiple(information_or_advice_1_graph)
any_multiple(information_or_advice_1_graph)

#In graph theory, a loop (also called a self-loop or a "buckle") is an 
#edge that connects a vertex to itself. A simple graph contains no loops.
#For a directed graph, a loop adds one to
#the in degree and one to the out degree.
 
which_loop(information_or_advice_1_graph)
sum(which_loop(information_or_advice_1_graph))
table(which_loop(information_or_advice_1_graph))
is_simple(information_or_advice_1_graph)


#Dodajem atribute koje sam prethodno sacuvala kao vektore
information_or_advice_1_graph<-set.vertex.attribute(information_or_advice_1_graph,
                                    name="Region",
                                    index = V(information_or_advice_1_graph)$name,
                                    value = region)

information_or_advice_1_graph<-set.vertex.attribute(information_or_advice_1_graph,
                                    name="The_organisational_level",
                                    index = V(information_or_advice_1_graph)$name,
                                    value = the_organisational_level)
information_or_advice_1_graph

information_or_advice_1_graph<-set.vertex.attribute(information_or_advice_1_graph,
                                    name="Gender",
                                    index = V(information_or_advice_1_graph)$name,
                                    value = gender)
information_or_advice_1_graph

information_or_advice_1_graph<-set.vertex.attribute(information_or_advice_1_graph,
                                    name="Location",
                                    index = V(information_or_advice_1_graph)$name,
                                    value = location)
V(information_or_advice_1_graph)$name
print_all(information_or_advice_1_graph)


#SUBSET
#Information or advice, where weight >=3
#"...how often you have turned to this person for information
#or advice on work-related topics in the past three months"
#...
#3: Sometimes; 
#4: Often; and 
#5:Very Often
?subgraph.edges
information_or_advice_3_graph <- subgraph.edges(graph=information_or_advice_1_graph, 
                                 eids=which(E(information_or_advice_1_graph)$weight>=3), 
                                delete.vertices = TRUE)



V(information_or_advice_3_graph)$name
E(information_or_advice_3_graph)$weight
is.directed(information_or_advice_3_graph)
is.weighted(information_or_advice_3_graph)
print_all(information_or_advice_3_graph)
#43 nodes i 312 edges
round(edge_density(information_or_advice_3_graph, loops=TRUE),digits = 2)
#0,17
summary(information_or_advice_3_graph)

round(312/43, digits = 2)#7.26
round(312/879*100,digits = 2)
#[1] 35.49
# Procenata ivica koji je zadrzan  je 35,49% u odnosu na
#originalni graf information_or_advice_1_graph

#1: Never; 
#2: Seldom; 
information_or_advice_3_graph_low_weight<-subgraph.edges(graph=information_or_advice_1_graph, 
               eids=which(E(information_or_advice_1_graph)$weight<3), 
               delete.vertices = TRUE)

print_all(information_or_advice_3_graph_low_weight)
E(information_or_advice_3_graph_low_weight)$weight
is.loop(information_or_advice_3_graph_low_weight)

sum(which_loop(information_or_advice_3_graph_low_weight))
table(which_loop(information_or_advice_3_graph_low_weight))
is_simple(information_or_advice_3_graph_low_weight)
#TRUE

summary(information_or_advice_3_graph_low_weight)
#567 egdes and 45 nodes
round(567/45, digits = 2)#12.6

round(edge_density(information_or_advice_3_graph_low_weight,loops=TRUE),digits = 2)
#density 0.27

round(567/879*100, digits = 2)#64.51% ivica originalnog grafa je u okviru grafa sa niskim
#tezinama
#ove ivice sa tezinama jedan i dva oznacvaju:
                                           #1: Never; 
                                          #2: Seldom; 
         #kada je u pitanju frekventnost trazenja saveta ili informacija



#In the second network, ties are differentiated in terms of the value 
#placed on the information or advice received 
#“For each person in the list below, please show how strongly you 
#agree or disagree with the following statement: 
#In general, this person has expertise in areas that are important 
#in the kind of work I do.” 
#The weights in this network is also based on a scale from 0 to 5.
#0: I Do Not Know This Person; 
#1: Strongly Disagree; 
#2: Disagree; 
#3: Neutral; 
#4: Agree; and 
#5: Strongly Agree.

value_1<-read.csv("C:/Users/Nikola/Desktop/organization/Value.csv",
                                  header =TRUE)
str(value_1)
dim(value_1)
apply(value_1, 2, unique)
sum(apply(value_1, 2, is.na))
#Graph format
?graph_from_data_frame
value_1_graph<-graph_from_data_frame(value_1,
                                    directed = TRUE)
V(value_1_graph)$name

value_1_graph
as.numeric(V(value_1_graph)$name)
value_name<-as.numeric(V(value_1_graph)$name)
is.numeric(value_name)

value_1_graph<-delete_vertex_attr(value_1_graph, name="name")
V(value_1_graph)$name<-value_name
value_1_graph

any_multiple(value_1_graph)
which_multiple(value_1_graph)

sum(which_loop(value_1_graph))
is_simple(value_1_graph)



#Dodajem prethodno sacuvane atribute
value_1_graph<-set.vertex.attribute(value_1_graph,name="Region",
                     index = V(value_1_graph)$name,
                     value = region)
value_1_graph

value_1_graph<-set.vertex.attribute(value_1_graph,name="The_organisational_level",
                     index = V(value_1_graph)$name,
                     value = the_organisational_level)
value_1_graph

value_1_graph<-set.vertex.attribute(value_1_graph,name="Gender",
                                    index = V(value_1_graph)$name,
                                    value = gender)
value_1_graph<-set.vertex.attribute(value_1_graph,name="Location",
                                    index = V(value_1_graph)$name,
                                    value = location)
value_1_graph
#46 nodes and 858 edges
is.directed(value_1_graph)
is.weighted(value_1_graph)
print_all(value_1_graph)
# mozemo dad primetimo da samo 24 nikog nije prepoznala u value mrezi
#a cvor 30 je samo 24 prepoznao kao nekog sa expertizom, medjutim
#tezina je svega 3, sto oznacava"Neutral"
summary(value_1_graph)
#46 nodes, 858 edge
E(value_1_graph)$weight
V(value_1_graph)$name

#links per node
round(858/46,digits = 2)#18.65

round(edge_density(value_1_graph), digits = 2)
# 0.41

#Value network, where weight >=3
#The weights in this network is also based on a scale from 0 to 5.
#0: I Do Not Know This Person; 
#1: Strongly Disagree; 
#2: Disagree; 
#3: Neutral; 
#4: Agree; and 
#5: Strongly Agree
#Ostace naravno i svi zeljeni atributi, vezani za odabrane cvorove.
value_3_graph <- subgraph.edges(graph=value_1_graph, 
                              eids=which(E(value_1_graph)$weight>=3))

is.directed(value_3_graph)
is.weighted(value_3_graph)
print_all(value_3_graph)
type_sum(value_3_graph)
#46 nodes and 801 edge
summary(value_3_graph)
value_3_graph
E(value_3_graph)$weight

#new network value_3_graph zadrzala je 93.36% ivica originalnog grafa
#value_3_graph #46 nodes and 801 edges
#value_1_graph #46 nodes and 858 edges
round(801/858*100, digits = 2)

#links per node
round(801/46, digits = 2)#17.41
round(edge_density(value_3_graph), digits = 2)#0.39
#to ustvari znaci da je samo 6.64% ivica postojalo u orginalnom grafu
#koje su imale prve dve tezine:
#1: Strongly Disagree; 
#2: Disagree;
#to je ustvari apsolutno gledano 57 ivica od 858
858-801#57

value_3_graph_disagree <- subgraph.edges(graph=value_1_graph, 
                                eids=which(E(value_1_graph)$weight<3), 
                                delete.vertices = TRUE)
value_3_graph_disagree
print_all(value_3_graph_disagree)
#DNW- 35 57 --

is.loop(value_3_graph_disagree)
is.simple(value_3_graph_disagree)
#TRUE

summary(value_3_graph_disagree)
round(edge_density(value_3_graph_disagree), digits = 2)
#0.05- density
round(57/35, digits = 2)#1.63 links per node

round(57/858*100, digits = 2)
#6,64% ivica od originalnog grafa koje imaju negativnu konotaciju
#kada je u pitanju vrednosanje ekspertize kolege


#For the researchers in the manufacturing company,
#the following attributes are known: 

#Location 
        #1: Paris; 
        #2: Frankfurt;
        #3: Warsaw; 
        #4: Geneva), 

#Tenure 1: 1-12 months; 
        #2: 13-36 months; 
        #3: 37-60 months; 
        #4: 61+ months
#and the organisational level ------->       1: Global Dept Manager; 
                                            #2: Local Dept Manager; 
                                            #3: Project Leader; 
                                            #4: Researcher.


location_2<-c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
              2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
              2, 2, 2, 2, 2, 2, 2, 2, 
              2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
              4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4) 
tenure <-c(4, 4, 4, 4, 4, 3, 3, 3, 4, 4, 4, 3, 2, 4, 3,
           4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 3,
           4, 3, 4, 3, 4, 3, 4, 4, 4, 4, 4, 4, 4, 3, 4,
           3, 4, 3, 4, 4, 2, 3, 4, 3, 4, 4, 4, 4, 4, 3,
           4, 4, 4, 4, 4, 4, 4, 1, 3, 4, 3, 3, 3, 4, 4,
           4, 3)
the_organisational_level_2<-c(4, 4, 4, 4, 4, 3, 3, 3, 4, 4, 4, 3, 2, 4, 3,
                              4, 4, 3, 4, 4, 4, 4, 4,4, 4, 4, 4, 4, 2, 3,
                              4, 3, 4, 3, 4, 3, 4, 4, 4, 4, 4, 4, 4, 3, 4,
                              3, 4, 3, 4, 4, 2, 3, 4, 3, 4, 4, 4, 4, 4, 3,
                              4, 4, 4, 4, 4, 4, 4, 1, 3, 4, 3, 3, 3, 4, 4,
                              4, 3)


#In the third network, the ties among the researchers are differentiated 
#in terms of advice “Please indicate the extent to which the people listed
#below provide you with information you use to accomplish your work”. 
#The weights are based on the following scale:
              #0: I Do Not Know This Person/I Have Never Met this Person; 
              #1: Very Infrequently; 
              #2: Infrequently; 
              #3: Somewhat Infrequently; 
              #4: Somewhat Frequently; 
              #5: Frequently; 
              #and 6: Very Frequently.

advice_1<-read.csv("C:/Users/Nikola/Desktop/organization/advice.csv",
                  header =TRUE)
str(advice_1)
dim(advice_1)
apply(advice_1, 2, unique)
sum(apply(advice_1, 2, is.na))

#Transformisanje u GRAPH format
?graph_from_data_frame
advice_1_graph<-graph_from_data_frame(advice_1,
                                      directed = TRUE)

which_loop(advice_1_graph)
sum(which_loop(advice_1_graph))
is.simple(advice_1_graph)

V(advice_1_graph)
advice_1_graph

E(advice_1_graph)
print_all(advice_1_graph)

advice_1_graph
as.numeric(V(advice_1_graph)$name)
advice_name<-as.numeric(V(advice_1_graph)$name)
is.numeric(advice_name)

advice_1_graph<-delete_vertex_attr(advice_1_graph,
                                      name="name")
V(advice_1_graph)$name<-advice_name
advice_1_graph
#Dodajem prethodno sacuvane atribute(vectore)
the_organisational_level_2
type_sum(the_organisational_level_2)

advice_1_graph<-set.vertex.attribute(advice_1_graph,
                                  name="The_organisational_level",
                                  index = V(advice_1_graph)$name,
                                  value = the_organisational_level_2)
advice_1_graph
advice_1_graph<-set.vertex.attribute(advice_1_graph,
                                    name="Location",
                                    index = V(advice_1_graph)$name,
                                    value = location_2)
advice_1_graph
advice_1_graph<-set.vertex.attribute(advice_1_graph,
                                     name="Tenure",
                                     index = V(advice_1_graph)$name,
                                     value = tenure)


is.directed(advice_1_graph)
is.weighted(advice_1_graph)
round(edge_density(advice_1_graph), digits = 2)
#0.38

print_all(advice_1_graph)
summary(advice_1_graph)
#DNW- 77 2228 --
round(2228/77, digits = 2)#28.94 links per node

#Advice network, where weight>=4
#The weights are based on the following scale:
              #0: I Do Not Know This Person/I Have Never Met this Person; 
              #1: Very Infrequently; 
              #2: Infrequently; 
              #3: Somewhat Infrequently;

              #4: Somewhat Frequently; 
              #5: Frequently; 
              #6: Very Frequently.


advice_3_graph <- subgraph.edges(graph=advice_1_graph, 
                                eids=which(E(advice_1_graph)$weight>=4), 
                                delete.vertices = TRUE)
advice_3_graph
#DNW- 77 753 --
print_all(advice_3_graph)
E(advice_3_graph)$weight
  
round(753/2228*100, digits = 2)#33.8% ivica u odnosu na originalni graf je opstalo

round(edge_density(advice_3_graph), digits = 2)
#0.13

round(753/77, digits=2)#9.78 links per node

advice_3_graph_low_weight<-subgraph.edges(graph=advice_1_graph, 
                              eids=which(E(advice_1_graph)$weight<=3), 
                              delete.vertices = TRUE)
#1: Very Infrequently; 
#2: Infrequently; 
#3: Somewhat Infrequently;
print_all(advice_3_graph_low_weight)
E(advice_3_graph_low_weight)$weight

is_simple(advice_3_graph_low_weight)
#TRUE

summary(advice_3_graph_low_weight)
#DNW- 77 1475 -- 
round(1475/77, digits = 2)#19.16 links per node

round(edge_density(advice_3_graph_low_weight), digits = 2)
#density 0.25

round(1475/2228*100, digits = 2)#66.20% ivica originalnog grafa je u okviru grafa sa niskim
#tezinama
#ove ivice sa tezinama jedan,dva,tri oznacvaju:
#1: Very Infrequently; 
#2: Infrequently; 
#3: Somewhat Infrequently; 
#kada je u pitanju frekventnost trazenja saveta ili informacija




#The fourth network is based on the employees’ awareness of each others’ 
                                                             #knowledge
                                                             #and skills 
#“I understand this person’s knowledge and skills.
#This does not necessarily mean that I have these skills or 
#am knowledgeable in these domains but that I understand what skills this 
#person has and domains they are knowledgeable in”. 
              #The weight scale in this network is:
              #0: I Do Not Know This Person/I Have Never Met this Person;
              #1: Strongly Disagree;  
              #2: Disagree; 
              #3: Somewhat Disagree;

              #4: Somewhat Agree; 
              #5: Agree; and 
              #6: Strongly Agre


awareness_1<-read.csv("C:/Users/Nikola/Desktop/organization/awareness_1.csv",
                   header=TRUE)


str(awareness_1)
dim(awareness_1)
apply(awareness_1, 2, unique)
sum(apply(awareness_1, 2, is.na))

#GRAPH format
?graph_from_data_frame
awareness_1_graph<-graph_from_data_frame(awareness_1,
                                         directed = TRUE)
awareness_1_graph
#DNW- 77 2326 -- 
which_loop(awareness_1_graph)
sum(which_loop(awareness_1_graph))
is_simple(awareness_1_graph)
#TRUE

V(awareness_1_graph)
as.numeric(V(awareness_1_graph)$name)
awareness_name<-as.numeric(V(awareness_1_graph)$name)
is.numeric(awareness_name)

awareness_1_graph<-delete_vertex_attr(awareness_1_graph,
                                      name="name")
V(awareness_1_graph)$name<-awareness_name
awareness_1_graph
# DNW- 77 2326 -- 
#+ attr: name (v/n), weight (e/n)

#Sada dodajem kao i za prethodnu mrezu sve atribute
awareness_1_graph
awareness_1_graph<-set.vertex.attribute(awareness_1_graph,
                                  name="The_organisational_level",
                                  index = V(awareness_1_graph)$name,
                                  value = the_organisational_level_2)
awareness_1_graph
awareness_1_graph<-set.vertex.attribute(awareness_1_graph,
                                     name="Location",
                                     index = V(awareness_1_graph)$name,
                                     value = location_2)
awareness_1_graph
awareness_1_graph<-set.vertex.attribute(awareness_1_graph,
                                     name="Tenure",
                                     index = V(awareness_1_graph)$name,
                                     value = tenure)


is.directed(awareness_1_graph)
is.weighted(awareness_1_graph)
print_all(awareness_1_graph)
summary(awareness_1_graph)
#DNW- 77 2326 -- 
#+ attr: name (v/n), The_organisational_level (v/n), Location
#| (v/n), Tenure (v/n), weight (e/n)
#> 
round(edge_density(awareness_1_graph), digits = 2)# 0.4

round(2326/77, digits = 2)#30.21 links per node

#Awareness network, where weight>=4
#The weight scale in this network is:
            #0: I Do Not Know This Person/I Have Never Met this Person;
            #1: Strongly Disagree;  
            #2: Disagree; 
            #3: Somewhat Disagree;

            #4: Somewhat Agree; 
            #5: Agree; and 
            #6: Strongly Agre
awareness_3_graph <- subgraph.edges(graph=awareness_1_graph, 
                                 eids=which(E(awareness_1_graph)$weight>=4), 
                                 delete.vertices = TRUE)
awareness_3_graph
E(awareness_3_graph)$weight
#DNW- 77 1842 --

round(edge_density(awareness_3_graph), digits = 2)# 0.31

round(1842/77, digits = 2)#23.92 links per node

round(1842/2326*100, digits = 2)# 79.19% ivica u odnosu na originalan graf je opstalo
#ostale su ivice kao sto sam odabrala samo one sa velikim tezinma
#tezine oznacavaju:
              #4: Somewhat Agree; 
              #5: Agree; and 
              #6: Strongly Agre
                        #kada je u pitanju svest o koleginoj ekpertizi


#1: Strongly Disagree;  
#2: Disagree; 
#3: Somewhat Disagree;
awareness_3_graph_disagree <- subgraph.edges(graph=awareness_1_graph, 
                                 eids=which(E(awareness_1_graph)$weight<=3), 
                                 delete.vertices = TRUE)
awareness_3_graph_disagree
print_all(awareness_3_graph_disagree)
#DNW- 77 484 -- 

is.loop(awareness_3_graph_disagree)
is.simple(awareness_3_graph_disagree)
#TRUE

summary(awareness_3_graph_disagree)
round(edge_density(awareness_3_graph_disagree), digits = 2)#0.08 
round(484/77, digits = 2)#6.29 links per node
round(484/2326*100, digits = 2)
#[1] 20.81%
#opstalo je 20.81% ivica od originalnog grafa, ove ivice  imaju
#negativnu konotaciju kada je u pitanju vrednovanje ekspertize kolege:
#1: Strongly Disagree;  
#2: Disagree; 
#3: Somewhat Disagree;


#Preuzela sam funkciju koju smo koristili na predavanju, koja ce mi biti
#potrebna pri vizualizaciji kasnije

# The function creates a vector of color values for the given graph (node / edge)
# attribute (1st argument). The color vector consists of gradients of the given
# color palette: the 2nd argument is a vector of two elements defining the palette's
# end points (e.g.: c('yellow','red')). The resulting color vector has as many color
# gradients as there are different values in the given node / edge attribute.
# The function is based on the procedure described in:
# http://natpoor.blogspot.com/2016/07/making-spectrumgradient-color-palette.html

attr_based_color_gradient <- function(g_attr, pal_end_points) {
  require(dplyr)
  # 1) Set the resolution, that is, how many color nuances are to be created
  col_resolution = n_distinct(g_attr)
  # 2) Set palette end points 
  col_palette = colorRampPalette(pal_end_points)
  # 3) Get the max value of the attribute to make the ratio: 
  max_val = max(g_attr, na.rm = TRUE)
  # 4) Create a vector of values which will determine the color values 
  # for each node: 
  value_vector = g_attr / max_val
  # 5) Create the vector of color values, based on the value_vector, the 
  # palette end points and the resolution. This will produce a vector of 
  # color values with the correct color value in the correct location for 
  # the chosen graph attribute: 
  g_colors = col_palette(col_resolution)[as.numeric(cut(value_vector, breaks=col_resolution))]
  return(g_colors)
}

