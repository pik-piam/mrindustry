# Development commands

- The package has no renv. Dependencies are either installed via `pak` in a project-local library `.Rlib` (gitignored) or in the user library.
- Load the package for development: `devtools::load_all()`
- To test individual functions, **never** call `readX()`/`convertX()`/`calcX()` directly. Go through `readSource("X")`, `calcOutput("X")`, `retrieveData()`. For example,
  ```r
  madrat::setConfig(ignorecache = c("calcindustry_max_secondary_steel_share"))
  calcOutput("industry_max_secondary_steel_share", scenarios = "SSP2", regions = "DEU", aggregate = FALSE)
  ```
- Query magclass and madrat docs via Context7 `query-docs`: library IDs `/pik-piam/magclass` and
  `/pik-piam/madrat`.
- Big files (`calcFeDemandIndustry.R`, `calcIndustry_Value_Added.R`, `calcSteel_Projections.R`) are organised with `# section ----` comments; navigate by those instead of reading top to bottom.
- Madrat source files are located in `MADRAT_MAINFOLDER` (either specified by env variable or in `.Renviron`). The data in this folder is strictly **read-only**, do not modify it.

# Conventions

- madrat finds functions by name, not by export: `calcOutput("Cement")` looks for `calcCement`.
- The function name must be `read<Type>`, `convert<Type>`, `calc<Type>` exactly, where `<Type>` is the
  string passed to `readSource()`/`calcOutput()`.
- Define helpers inside the `calc`/`read` body, or as a `tool*` function; otherwise they will not be part of the cache key and changes to them will not invalidate caches.
- Read functions run with the working directory set to `<mainfolder>/sources/<Type>/`, so use
  relative paths (`'./file.xlsx'`).
- Prefer the *switchboard* pattern if there are subtypes (see `readworldsteel.R`): a named list of
  closures, then `stop('Invalid subtype -- supported subtypes are: ...')`. Document every subtype in `@param subtype` as a markdown list.
- Currently, no tests are defined. New tests are welcome but must not need source data
(use fixtures in `tests/testthat/fixtures/`).
- `Makefile`, `.pre-commit-config.yaml`, `codecov.yml` and `README.md` are overwritten from lucode2
templates. Do not edit them directly.
