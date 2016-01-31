# TextRank
Text Summarization


TextRank is the graph based keyword extraction and text summarization method using for Natural Language Processing which proposed by Mihalcea and Tarau. TextRank applications rely on Google's PageRank and HITS algorithm. To represent text with graph based ranking algorithm, text should be converted to graph. Vertices of graph are the terms of text and edges represent the strength between two terms which can be sentences,words or paragraph of a text. To create an edge, similarity measure which is defined by function of content overlap is used. In post-processing, every edges' weights are calculated and final ranking scores are calculated for each vertices then sentences which have highest rank are sorted to create a summary.


