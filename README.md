
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chewie

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/chewie)](https://CRAN.R-project.org/package=chewie)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-4.1.0-6666ff.svg)](https://cran.r-project.org/)
<!-- badges: end -->

The goal of chewie is to easily scrape pages without having to call
multiple extraction methods over and over again. It simplifies the
process by feeding a `scheme` to a single method called `chew`.

A `scheme` is like a recipe that gives chewie the guidelines to where
elements are in a page and how to extract them. Each page you choose to
“chew” should have a single scheme composed by a list of instruction
objects.

A `instruction` is formed by the following 6 fields (more fields could
be added on future releases):

1.  `title`: an arbitrary name to the scraped object
2.  `selector`: whether `path`/`alternative_path` is a css or xpath
    selector, defaults to `NULL`
3.  `path`: a css or xpath path to the object to be scraped
4.  `alternative_path`: an alternative css or xpath path to the object
    to be scraped
5.  `parse_as`: indicates if an extractor should be applied to the
    resulting scraped item. To return raw objects, just leave it as
    `NULL`. Currently available extractors are:
    -   `text`
    -   `numeric`
    -   `table`
    -   `date`
    -   `datetime`
    -   `difftime`
    -   `price`
6.  `pattern`: a RegEx pattern to be applied before parsing

## Quickstart

### Instalation

You can install the released version of `chewie` with:

``` r
remotes::install_github("leonardodiegues/chewie")
```

### Usage

Schemes can be loaded either from instantiating a `scheme` or a
`data.frame` object. The following chunk exemplifies both manners by
looking at Rio 2016 100 meters butterfly results:

``` r
library(chewie)

swimming_100m_butterfly <- "http://www.olympedia.org/results/357088"

# Load data from a `data.frame` containing columns corresponding to available fields.
# page_scheme <- read.csv("swimming_100m_butterfly_recipe.csv")

# Or manually add all fields
page_scheme <- scheme(
  list(
    instruction(
      title = "event_name",
      path = "h1:nth-of-type(1)",
      parse_as = "text",
    ),
    instruction(
      title = "event_location",
      path = "//table[1]/tr[3]/td[1]",
      selector = "xpath",
      parse_as = "text"
    ),
    instruction(
      title = "n_participants",
      path = "table:nth-of-type(1) > tr:nth-of-type(4) > td",
      parse_as = "numeric",
      pattern = "^(\\d+) "
    ),
    instruction(
      title = "event_results",
      path = "//table[2]",
      selector = "xpath",
      parse_as = "table"
    )
  )
)

# Chew page based on scheme
results <- chew(scheme = page_scheme, url = swimming_100m_butterfly)
#> New names:
#> * `` -> ...7
#> * `` -> ...8
#> * `` -> ...9

print(results)
#> [[1]]
#> <chewie_instruction>
#>     * title:    event_name
#>     * path:     h1:nth-of-type(1)
#>     * selector: css
#>     * parse as: text
#>     * pattern:  
#>     * result:   100 metres Butterfly, Men
#> 
#> [[2]]
#> <chewie_instruction>
#>     * title:    event_location
#>     * path:     //table[1]/tr[3]/td[1]
#>     * selector: xpath
#>     * parse as: text
#>     * pattern:  
#>     * result:   Estádio Aquático Olímpico, Parque Olímpico da Barra, Barra da Tijuca, Rio de Janeiro
#> 
#> [[3]]
#> <chewie_instruction>
#>     * title:    n_participants
#>     * path:     table:nth-of-type(1) > tr:nth-of-type(4) > td
#>     * selector: css
#>     * parse as: numeric
#>     * pattern:  ^(\d+) 
#>     * result:   43
#> 
#> [[4]]
#> <chewie_instruction>
#>     * title:    event_results
#>     * path:     //table[2]
#>     * selector: xpath
#>     * parse as: table
#>     * pattern:  
#>     * result:   a 43x11 `data.frame`
```

Generally extraction methods are wraps around `rvest::html_text2` and
`stringr::str_extract`. In the case of `extract_table` it would be
useful if we could not only pull the table as a `data.frame` (using
`rvest::html_table`) but add new URL columns based on columns that have
`a` tags attached to them. This is `extract_table` default behavior and
can’t be changed yet (work in progress).

Let’s check the resulting table from the fourth instruction:

``` r
tbl <- results[[4]]$result

print(tbl)
#> # A tibble: 43 x 11
#>    Pos   Swimmer  NOC   R1     SF    Final ...7  ...8  ...9  Swimmer_url NOC_url
#>    <chr> <chr>    <chr> <chr>  <chr> <chr> <chr> <chr> <lgl> <chr>       <chr>  
#>  1 1     Joseph … SGP   51.41… 50.8… 50.3… "Gol… "OR"  NA    /athletes/… /count…
#>  2 =2    Michael… USA   51.60… 51.5… 51.1… "Sil… ""    NA    /athletes/… /count…
#>  3 =2    Chad le… RSA   51.75… 51.4… 51.1… "Sil… ""    NA    /athletes/… /count…
#>  4 =2    László … HUN   51.52… 51.5… 51.1… "Sil… ""    NA    /athletes/… /count…
#>  5 5     Li Zhuh… CHN   51.78… 51.5… 51.2… ""    ""    NA    /athletes/… /count…
#>  6 6     Mehdy M… FRA   51.71… 51.7… 51.5… ""    ""    NA    /athletes/… /count…
#>  7 7     Tom Shi… USA   51.58… 51.6… 51.7… ""    ""    NA    /athletes/… /count…
#>  8 8     Aleksan… RUS   51.91… 51.7… 51.8… ""    ""    NA    /athletes/… /count…
#>  9 9     David M… AUS   51.81… 51.7… –     ""    ""    NA    /athletes/… /count…
#> 10 10    Konrad … POL   51.81… 51.8… –     ""    ""    NA    /athletes/… /count…
#> # … with 33 more rows
```

Parsed HTML pages can also be chewed:

``` r
swimming_100m_butterfly_page <- swimming_100m_butterfly |>
  httr::GET() |> 
  httr::content(as = "text") |> 
  rvest::read_html()

chew(scheme = page_scheme, page = swimming_100m_butterfly_page)
#> New names:
#> * `` -> ...7
#> * `` -> ...8
#> * `` -> ...9
#> [[1]]
#> <chewie_instruction>
#>     * title:    event_name
#>     * path:     h1:nth-of-type(1)
#>     * selector: css
#>     * parse as: text
#>     * pattern:  
#>     * result:   100 metres Butterfly, Men
#> 
#> [[2]]
#> <chewie_instruction>
#>     * title:    event_location
#>     * path:     //table[1]/tr[3]/td[1]
#>     * selector: xpath
#>     * parse as: text
#>     * pattern:  
#>     * result:   Estádio Aquático Olímpico, Parque Olímpico da Barra, Barra da Tijuca, Rio de Janeiro
#> 
#> [[3]]
#> <chewie_instruction>
#>     * title:    n_participants
#>     * path:     table:nth-of-type(1) > tr:nth-of-type(4) > td
#>     * selector: css
#>     * parse as: numeric
#>     * pattern:  ^(\d+) 
#>     * result:   43
#> 
#> [[4]]
#> <chewie_instruction>
#>     * title:    event_results
#>     * path:     //table[2]
#>     * selector: xpath
#>     * parse as: table
#>     * pattern:  
#>     * result:   a 43x11 `data.frame`
```
