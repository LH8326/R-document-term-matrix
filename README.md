# R-document-term-matrix

#alg_bipartitemapping

This R script reads and cleans a folder of text documents, creates a document-term matrix, and builds a bipartite graph linking documents to words. It then applies community detection to group similar documents based on shared terms and visualises the result, helping reveal topic structures within the text collection.

#alg_Document-Term BipartiteGraphwithCommunityDetection
This code filters out sparse terms, reshapes the DTM into a long format, and builds a bipartite graph where documents and words are connected. The script assigns visual attributes to the graph nodes and edges and then uses the Louvain algorithm to detect communities (groups of related documents and terms).

Finally, it plots the graph, colouring nodes by community to help visualise topic clusters in the text corpus.

#alg_centralitymeasures_engenvector
This R script performs text mining and network analysis on a folder of documents to visualise how words (terms) relate to one another.

#algrothrims_communitydetection
This script processes a folder of documents, cleans the text, and builds a document-term matrix. It then constructs a document similarity network where documents are connected if they share words. Using network analysis, it highlights important documents and clusters them into communities based on shared language. The network is visualised with nodes sized by centrality and coloured by community.
