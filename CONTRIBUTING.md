# Contributing to bllflow

This outlines how to propose a change to cchsflow. 

## Documentation


### Linking to Images

When linking to images in .Rmd files, make sure to add the relative path to the image in the resource_files section of the .Rmd file. An example is given below,

```YAML
resource_files:
  - ../man/figures/coding.png
```

### Linking to other .Rmd files

When linking to other .Rmd files make sure their extension is changed to .html. This ensures that the links continue to work when they are converted to the pkgdown site


## Filing an issue

The easiest way to propose a change or new feature is to file an issue. If you've found a
bug, you may also create an associated issue. If possible, try to illustrate your proposal or the bug with a minimal [reproducible example](https://www.tidyverse.org/help/#reprex).

## Pull request

*  Please create a Git branch for each pull request (PR). [Click here](https://help.github.com/en/articles/creating-a-pull-request-from-a-fork) for information on how to create a PR.
*  Your contributed code should roughly follow the tidyverse [style guide](http://style.tidyverse.org). _Exceptions_ from this guide: function names in PascalCase (e.g. NewFunction()) and variable names as camelCase (e.g newVariable <- "hi").
*  cchsflow uses [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html),
for documentation.
*  cchsflow uses [testthat](https://cran.r-project.org/package=testthat) (TBA). Adding tests to the PR makes it easier for me to merge your PR into the code base.
*  If your PR is a user-visible change, you may add a bullet to the top of `NEWS.md` describing the changes made. You may optionally add your GitHub username, and links to relevant issue(s)/PR(s).

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to
abide by its terms.
