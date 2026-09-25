## Development

Create a `.Renviron` file in the project root to set environment variables, for example:

```sh
R_LIBS=.Rlib
MADRAT_MAINFOLDER=<absolute_path_to_madrat>
MADRAT_CACHEFOLDER=<absolute_path_to_madrat>/cache/mrindustry-dev
```

The `R_LIBS` environment variable instructs R to use a project-specific library located in the `.Rlib` folder within the project root; if you do not set this variable, the global R library will be used instead.
On the cluster, the `MADRAT_*` environment variables are already set, so you do not need to define them in `.Renviron`.
If you set these environment variables, make sure to not also set the corresponding MADRAT options in your `.Rprofile`.

To set up the development environment, start R and run the following commands:

```R
pak::repo_add(pik = "https://rse.pik-potsdam.de/r/packages")
pak::local_install_dev_deps()

# Check setup
madrat::getConfig()
```

You should now have a development environment set up with the necessary dependencies installed and the package loaded for development.
After making changes to the package code, you can reload it in the current R session by running:

```R
devtools::load_all()
```

A complete run is typically done via the `mrremind` package. But you can also test individual functions by calling them directly, for example:

```R
madrat::setConfig(ignorecache = c("calcindustry_max_secondary_steel_share"))
calcOutput("industry_max_secondary_steel_share", scenarios = "SSP2", regions = "DEU", aggregate = FALSE)
```
