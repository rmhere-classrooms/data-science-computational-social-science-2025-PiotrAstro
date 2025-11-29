# Piotr Zatwarnicki 272641

# load the igraph library for processing graphs
library(igraph)

# Wygeneruj sieć Erdős-Rényi o stu wierzchołkach i prawdopodobieństwie krawędzi = 0.05.
g <- erdos.renyi.game(p.or.m=0.05, n=100)

# na górze grafu widzimy, że jest on nieważony (w podsumowaniu)
summary(g)

# Wylistowanie wierzchołków
V(g)

# Wylistowanie krawędzi
E(g)

# Dodanie wag
E(g)$weight <- runif(length(E(g)), 0.01, 1)

# teraz na górze pojawiła się informacja W - weighted oraz atrybut weight
summary(g)

# Stopień wszystkich węzłów
degree(g)
hist(degree(g))

# liczba klastrów (w tym typie raczej napewno będzie 1):
components(g)
components(g)$no

# page rank węzłów na grafie
pr <- page_rank(g)$vector

plot(g, vertex.size=pr*300,
     vertex.label=NA, edge.arrow.size=.2)
