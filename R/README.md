## Naming files

Files that contain a single public function should be named the same as the
function but follow kebab case. For example, if the name of the public function
is `pack_years_fun` then the file should be named `pack-years-fun.R`.

## Naming private functions

Functions that are not meant to be used by a package user should be named
starting with a dot. For example, `.validate_function_args`.

## Implicit Return

Do not use implicit returns, instead explicitly use the `return` function.

DO NOT DO THIS

```{r}
sum <- function(a, b) {
  a + b
}
```

DO THIS

```{r}
sum <- function(a, b) {
  return(a + b)
}
```
