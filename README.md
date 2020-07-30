
# Adjustr v0.5.0.0

<!-- badges: start -->

<!-- badges: end -->

The goal of Adjustr is to easily fit a given random variable in order to
find it’s distribution.

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("moutikabdessabour/adjustr")
```

# Using it

The package is made up of two main functions.

## getfitdistr

To fit continuous random variables

``` r
getfitdistr(x, 
            short=TRUE, 
            showplots=TRUE,
            plots.as.vars=FALSE, 
            xlab="Claim amounts",
            color=c(gamma = "steelblue", lognormal="firebrick",exponential= "green"))
```

The available arguments
are:

| Argument        | Description                                                                       |
| --------------- | --------------------------------------------------------------------------------- |
| `short`         | when used the function will return only the best fitted distribution if it exists |
| `x`             | The vector that contains the values of the continuous random variable             |
| `showplots`     | Whether To show the plots or not                                                  |
| `plots.as.vars` | Whether you want to add the plots to the result                                   |
| `xlab`          | what do you want to name the X axis?                                              |
| `color`         | The color you want to use in the plots                                            |

This code is all that’s needed to fit your random variable, the returned
value is a list. let `distribution` be the name of a distribution we
have:

| File            | Description                                                                                                                                                                                                                                       |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `distributions` | The distribution that the code looped through.                                                                                                                                                                                                    |
| `acceptedDistr` | A `data.frame` that contains the distributions that verify the null hypothesis order from the lowest **AIC** to the biggest.                                                                                                                      |
| `distribution`  | A list containing the `$estimate` for said distribution, the `$p.value` for the Kolmogorov-Smirnov test, `$accepthyp` when the Null Hypothesis is accepted or not and `$aic` containing the *Akaike information criterion* for this distribution. |
| `bestdistr`     | the best fitted distribution                                                                                                                                                                                                                      |

## getgoodfit

To fit discrete random
variables:

``` r
getgoodfit(x, showplots=F, short=TRUE, plots.as.vars=F, xlab="Number of claims")
```

The available arguments
are:

| Argument        | Description                                                                       |
| --------------- | --------------------------------------------------------------------------------- |
| `short`         | when used the function will return only the best fitted distribution if it exists |
| `x`             | The vector that contains the values of the discrete random variable               |
| `showplots`     | Whether To show the plots or not                                                  |
| `plots.as.vars` | Whether you want to add the plots to the result                                   |
| `xlab`          | what do you want to name the X axis?                                              |

This code is all that’s needed to fit your random variable, the returned
value is a list. let `distribution` be the name of a distribution we
have:

| File            | Description                                                                                                                                                                                                                                                                   |
| --------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `distributions` | The distribution that the code looped through.                                                                                                                                                                                                                                |
| `acceptedDistr` | A `data.frame` that contains the distributions that verify the null hypothesis order from the lowest **Chi-squared** to the biggest.                                                                                                                                          |
| `distribution`  | A list containing the `$estimate` for said distribution, the `$observed` and `$fitted` values, the `$count` and the `$prob`, the `$p.value`, `$Xsquared` and the `$df` degrees of freedom for the Chi-squared test, `$accepthyp` when the Null Hypothesis is accepted or not. |
| `bestdistr`     | the best fitted distribution                                                                                                                                                                                                                                                  |

# rmd.continuous & rmd.discrete

To create a PDF or HTML file analyzing a continuous/discrete random
variable use the following functions:

``` r
rmd.continuous(output_file, 
                  author, 
                  path, 
                  title="Ajustement d'une V.A", 
                  date=Sys.Date(),
                  output_dir=getwd(),
                  output_format, 
                  verbose=F, 
                  ...)
rmd.discrete(output_file,
             author,
             path,
             title="Ajustement d'une V.A",
             date=Sys.Date(),
             output_dir=getwd(),
             output_format,
             verbose=F,
             ...)
```

The available arguments
are:

| Argument        | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| --------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `output_file`   | The name of the output file. If using NULL then the output filename will be based on filename for the input file. If a filename is provided, a path to the output file can also be provided. Note that the output\_dir option allows for specifying the output file path as well, however, if also specifying the path, the directory must exist. If output\_file is specified but does not have a file extension, an extension will be automatically added according to the output format. To avoid the automatic file extension, put the output\_file value in `I()`, e.g., `I('my-output')`. |
| `author`        | The author to use in the template                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| `path`          | The path to the csv file. note that the column that contains the random variable values must be named x                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `title`         | The title to be used in the template                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| `date`          | The date to be used in the template                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| `output_dir`    | The output directory for the rendered output\_file. This allows for a choice of an alternate directory to which the output file should be written (the default output directory of that of the input file). If a path is provided with a filename in output\_file the directory specified here will take precedence. Please note that any directory path provided will create any necessary directories if they do not exist.                                                                                                                                                                   |
| `output_format` | The R Markdown output format to convert to. The option “all” will render all formats defined within the file. The option can be the name of a format (e.g. “html\_document”) and that will render the document to that single format. One can also use a vector of format names to render to multiple formats. Alternatively, you can pass an output format object (e.g. `html_document()`). If using NULL then the output format is the first one defined in the YAML frontmatter in the input file (this defaults to HTML if no format is specified there).                                   |
| `verbose`       | An option to suppress printing of the pandoc command line.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| `...`           | Additional arguments to be passed to `rmarkdown::render()`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |

**NB** The `Rmd` templates are only in french :cry: To see the results
check the following links

  - [continuous format
    html](https://moutikabdessabour.github.io/adjustr/inst/examples/continuous.html),
  - [continuous format
    pdf](https://moutikabdessabour.github.io/adjustr/inst/examples/continuous.pdf),
  - [discrete format
    html](https://moutikabdessabour.github.io/adjustr/inst/examples/discrete.html),
  - [discrete format
    pdf](https://moutikabdessabour.github.io/adjustr/inst/examples/discrete.pdf)

# Questions?

Confused by anything (there’s a lot to be confused by)? [Open an issue
on github](https://github.com/moutikabdessabour/adjustr/issues/new) and
let me know.
