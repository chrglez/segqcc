# Maintenance scripts

Not part of the package (`tools/` is in `.Rbuildignore`). Run from the package
root.

| Script | What it regenerates |
|---|---|
| `generate_data.R` | `data/*.rda` — the four bundled data sets. Seeded, so the output is reproducible. |
| `make_readme_figure.R` | `man/figures/README-example.png`. Needs `segqcc` installed. |
| `inspect_rda.R` | Prints a summary of the bundled `.rda` files. |

## Regenerating the help pages

`man/*.Rd` and `NAMESPACE` are generated from the roxygen comments in `R/`,
which are the source of truth. Never edit them by hand:

```sh
Rscript -e 'roxygen2::roxygenise()'
```

`roxygen2` needs `xml2`, which needs the `libxml2-dev` system package.

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
