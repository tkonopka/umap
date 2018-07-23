# umap
R implementation of Uniform Manifold Approximation and Projection

![Status](https://travis-ci.org/tkonopka/umap.svg?branch=master)
[![codecov](https://codecov.io/gh/tkonopka/umap/branch/master/graph/badge.svg)](https://codecov.io/gh/tkonopka/umap)


Uniform manifold appximation and projection (UMAP) is a technique for dimensional reduction. The original algorithm was proposed by [McInnes and Heyes](https://arxiv.org/abs/1802.03426) and
implemented in a python package [umap](https://github.com/lmcinnes/umap). This package provides an interface to the UMAP algorithm in R, including a translation of the original algorithm into R with minimal dependencies. 




## Examples

The figure below shows dimensional reduction on the [MNIST digits](https://en.wikipedia.org/wiki/MNIST_database) dataset. This dataset consists of 70,000 observations in a 784-dimensional space and labeled by ten distinct classes. The output of this package's `umap' function provides the plot layout, i.e. the arrangement of dots on the plane. The coloring, added to visualize how the known labels are positioned within the layout, demonstrates separation of the underlying data groups.

<img src="https://github.com/tkonopka/umap/blob/master/images/readme_mnist.png?raw=true" alt="A UMAP visualization of the MNIST digits dataset" width="600px">
</img>

The package also allows to project data onto an existing embedding. Below, the first figure shows a map created from a subset of 60,000 observations from the MNIST data. The second figure is a projection of the held-out 10,000 observations onto the layout defined by the training data. 

<img src="https://github.com/tkonopka/umap/blob/master/images/readme_mnist_training.png?raw=true" alt="A UMAP visualization of the MNIST digits dataset" width="350px"></img>
<img src="https://github.com/tkonopka/umap/blob/master/images/readme_mnist_test.png?raw=true" alt="A UMAP visualization of the MNIST digits dataset" width="350px"></img>

More information on usage can be found in the package [vignettes](https://github.com/tkonopka/umap/tree/master/vignettes).




## Implementations

The package provides two implementations of the UMAP algorithm.

The default implementation is one written in R and Rcpp. This implementation follows the original python code. However, any bugs or errors should be regarded as arising solely from this implementation, not from the original. The implementation has minimal dependencies and should work on most platforms. (The MNIST graphic is generated based on this default implementation).

A second implementation is a wrapper for the python package. This offers similar functionality to another existing package [umapr](https://github.com/ropenscilabs/umapr). To use this implementation, additional installation steps are required; see documentation for the [python package](https://github.com/lmcinnes/umap) for details. 

Note: an independent R implementation of UMAP is also available in a separate package [uwot](https://github.com/jlmelville/uwot).


## References

The original UMAP algorithm is described in the following article

McInnes, Leland, and John Healy. "UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction." [arXiv:1802.03426](https://arxiv.org/abs/1802.03426).




### License

MIT License.


