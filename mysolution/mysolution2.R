# Piotr Zatwarnicki 272641

# load the igraph library for processing graphs
library(igraph)

# Wygeneruj sieć Barabasi-ALbert
g <- barabasi.game(5000)
layout <- layout.fruchterman.reingold(g)
plot(g, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.05)
# najbardziej centralny węzeł
bet <- betweenness(g)
summary(bet)
most_central <- which.max(bet)
# najbardziej centralny węzeł
most_central
# miara betweeness
max(bet)

# średnica grafu
diameter(g)

# histogram - z ciekawości
hist(degree(g))
h <- hist(degree(g), plot = FALSE)
plot(h$mids, h$counts, 
     log = "xy",
     type = "h",
     lwd = 10,
     main = "Histogram stopni węzłów (log-log)",
     xlab = "Stopień węzła",
     ylab = "Liczba węzłów")

# Erdős-Rényi:
# równe prawdopodobieństwo wystąpienia dowolnych krawędzi
# w związku z tym wszystkie wierzchołki mniej więcej równe - histogram stopni węzłów przypomina rozkład normalny
# żadko występuje w rzeczywistości

# Barabasi-ALbert:
# sieć bezskalowa - może rosnąć w czasie, zachowując charakterystykę
# nowe węzły łączą się najczęściej z tymi o największym stopniu
# rozkład potęgowy stopni węzłów
# występowanie hubów - pojedynczych węzłów o bardzo dużym stopniu
# odzwierciedlenie wielu sieci, np. internetu

