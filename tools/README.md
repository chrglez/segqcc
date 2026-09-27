# Maintenance scripts

Not part of the package (`tools/` is in `.Rbuildignore`). Run from the package
root.

| Script | What it regenerates |
|---|---|
| `generate_data.R` | `data/*.rda` — the four bundled data sets. Seeded, so the output is reproducible. |
| `make_readme_figure.R` | `man/figures/README-example.png`. Needs `segqcc` installed. |
| `inspect_rda.R` | Prints a summary of the bundled `.rda` files. |

## Regenerating the help pages

The `man/*.Rd` files are the roxygen output, but they were last written by hand
because `roxygen2` could not be installed on the development machine (`xml2`
needs `libxml2-dev`). **They can therefore drift from the roxygen comments in
`R/`, which remain the source of truth.** Once the system library is available:

```sh
sudo apt install libxml2-dev
Rscript -e 'install.packages("roxygen2")'
Rscript -e 'roxygen2::roxygenise()'
```

and check `git diff man/ NAMESPACE` before committing.

## Building the vignette

`R CMD build` re-knits `vignettes/segqcc.Rmd`, which needs pandoc:

```sh
sudo apt install pandoc
```

Without it, build with `--no-build-vignettes`. The R code of the vignette can
be validated on its own with `knitr::knit()`, which does not need pandoc.

## Checking

```sh
R CMD build . && R CMD check --as-cran segqcc_*.tar.gz
```
