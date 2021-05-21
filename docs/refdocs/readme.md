# Reference documentation for ActorForth


## Category Theory

### Compiling to Categories

[Conal Elliot Website](http://conal.net/papers/compiling-to-categories/)

[International Conference on Functional Programming 2017](http://podcasts.ox.ac.uk/compiling-categories)

Conal Elliott, Target, USA, gives the first talk in the fourth panel, Program Construction, on the 2nd day of the ICFP conference.
It is well-known that the simply typed lambda-calculus is modeled by any cartesian closed category (CCC). This correspondence suggests giving typed functional programs a variety of interpretations, each corresponding to a different category. A convenient way to realize this idea is as a collection of meaning-preserving transformations added to an existing compiler, such as GHC for Haskell. This paper describes such an implementation and demonstrates its use for a variety of interpretations including hardware circuits, automatic differentiation, incremental computation, and interval analysis. Each such interpretation is a category easily defined in Haskell (outside of the compiler). The general technique appears to provide a compelling alternative to deeply embedded domain-specific languages.



[Paper](compiling-to-categories.pdf)

[Slides](compiling-to-categories-slides.pdf)

### Deconstructing Lambdas

[Deconstructing Lambdas—An Awkward Guide to Programming Without Functions](https://www.youtube.com/watch?v=xZmPuz9m2t0)

[Chris Penner](https://chrispenner.ca/)

## Capabilities

Deny Capabilities for Fast, Safe Actors
