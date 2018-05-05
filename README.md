# umap
R implementation of Uniform Manifold Approximation and Projection

![Status](https://travis-ci.org/tkonopka/umap.svg?branch=master)
[![codecov](https://codecov.io/gh/tkonopka/umap/branch/master/graph/badge.svg)](https://codecov.io/gh/tkonopka/umap)


Originally proposed by [McLelland and Heyes](https://arxiv.org/abs/1802.03426) and
implemented in a [python package umap](https://github.com/lmcinnes/umap), "uniform manifold approximation and projection" (UMAP) is an algorithm for dimensional reduction. This package provides a translation of the original algorithm into R.




## Examples

The figure below shows dimensional reduction on the [MNIST digits](https://en.wikipedia.org/wiki/MNIST_database) dataset, comprising of 70,000 observations in a 784-dimensional space and labeled by ten distinct classes. The left panel is based on R's well-known `cmdscale` function, applied on a small subset of the data for performance reasons. The right panel shows output produced by this package's `umap` function on the entire data. Beside efficiently handling larger datasets, the UMAP technique also provides better spearation of the observations into the underlying data groups.

<img src="https://github.com/tkonopka/umap/blob/master/images/readme_mnist.png?raw=true" alt="Visualization of MNIST data by cmdscale and UMAP" width="600px">
</img>

More information can be found in the package [vignettes]().




## References

The original UMAP algorithm is described in a publication

McInnes, Leland, and John Healy. "UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction." arXiv preprint arXiv:1802.03426 (2018).

The original python implementation is available in github [umap](https://github.com/lmcinnes/umap)


Disclaimer: The implementation in this package follows closely the original python code, but any bugs or errors should be regarded as arising solely from this implementation, not from the original.




## License

MIT License.


