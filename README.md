# Functorial Perspective on Manifold Learning: UMAP

## Project Overview

This project explores an innovative approach to manifold learning by implementing the UMAP (Uniform Manifold Approximation and Projection) algorithm from a functorial perspective. Developed by Hunter Hasenfus, this work leverages concepts from topology, simplicial complexes, stochastic gradient descent (SGD), and category theory to understand and reimplement UMAP in a novel way.

### Background

UMAP is a powerful dimensionality reduction technique widely used for visualizing complex, high-dimensional data in a lower-dimensional space. It achieves this by constructing a weighted graph representation of the data and optimizing this embedding based on fuzzy topology principles. This process allows UMAP to maintain critical data characteristics while reducing dimensionality.

### Functorial Decomposition

In this project, UMAP is approached through a series of categorical functors, decomposing it as follows:

1. Building a local uber-metric space around each point in the dataset (X).
2. Converting each local uber-metric space to a fuzzy simplicial complex.
3. Taking a fuzzy union of these fuzzy simplicial complexes.
4. Converting the resulting fuzzy simplicial complex to a fuzzy non-nested flag cover.
5. Utilizing a functor FCE that maps the pseudometric space (X, dX) to the UMAP embedding optimization problem.

### Goals and Implementation

The primary goal of this project is to reimplement UMAP using a functorial approach, thereby enhancing the understanding of its underlying mechanics in terms of category theory and related mathematical fields. The specific objectives include:

- Leveraging existing knowledge in topology, simplicial complexes, SGD, and category theory.
- Reading in data from a CSV format and outputting the resulting embeddings in a similar format.
- Utilizing prebuilt libraries for matrix operations while implementing custom data structures for data points and local spaces.

### References

1. Leland McInnes, John Healy, and James Melville. "UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction," 2020.
2. Dan Shiebler. "Functorial Manifold Learning." Electronic Proceedings in Theoretical Computer Science, 372:1–13, Nov 2022.

