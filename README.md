# umap
R implementation of Uniform Manifold Approximation and Projection

![Status](https://travis-ci.org/tkonopka/umap.svg?branch=master)
[![codecov](https://codecov.io/gh/tkonopka/umap/branch/master/graph/badge.svg)](https://codecov.io/gh/tkonopka/umap)


Uniform manifold appximation and projection (UMAP) is a technique for dimensional reduction. The original algorithm was proposed by [McLelland and Heyes](https://arxiv.org/abs/1802.03426) and
implemented in a python package [umap](https://github.com/lmcinnes/umap). This package provides a translation of the original algorithm into R. 




## Examples

The figure below shows dimensional reduction on the [MNIST digits](https://en.wikipedia.org/wiki/MNIST_database) dataset. This dataset consists of 70,000 observations in a 784-dimensional space and labeled by ten distinct classes. The output of this package's `umap' function provides the plot layout, i.e. the arrangement of dots on the plane. The coloring, added to visualize how the known labels are positioned within the layout, demonstrates separation of the underlying data groups.

<img src="https://github.com/tkonopka/umap/blob/master/images/readme_mnist.png?raw=true" alt="A UMAP visualization of the MNIST digits dataset" width="600px">
</img>

More information on usage can be found in the package [vignettes](https://github.com/tkonopka/umap/tree/master/vignettes).




## Implementation notes

The implementation of the UMAP algorithm in this package follows closely the original python code. However, any bugs or errors should be regarded as arising solely from this implementation, not from the original.

This package includes implementations for auxiliary algorithms, including approximate searches for nearest neighbors and manipulation of sparse matrices. 



## Performance

The focus of the implementation is to provide running time that are sub-quadratic in the number of data points in a dataset. In this regard, the package can be attractive for very large datasets in comparison with quadratic algorithms. However, this package does not attempt to provide maximal performance. For applications that require maximal performance, see the original implementation. 



## References

The original UMAP algorithm is described in the following article

McInnes, Leland, and John Healy. "UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction." [arXiv:1802.03426](https://arxiv.org/abs/1802.03426).

The original python implementation is available in github [umap](https://github.com/lmcinnes/umap)



### License

MIT License.


