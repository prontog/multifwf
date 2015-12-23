<!-- README.md is generated from README.Rmd. Please edit that file -->
multifwf
========

[![Build Status](https://travis-ci.org/prontog/multifwf.svg)](https://travis-ci.org/prontog/multifwf)

Read a table of fixed width formatted data of different types into a data.frame for each type. This package is for people that need to load a file that contains lines of different fixed with format. Think of it as an extension to the **read.fwf** function. One use-case is reading a log file containing messages of a fixed-width text protocol.

An example
----------

``` r
library(multifwf)

# Create a temp file with a few lines from a simple exchange protocol.
ff <- tempfile()
cat(file = ff, 
    '10:15:03:279NOSLMT0000666    EVILCORP00010.77SomeClientId    SomeAccountId   ',
    '10:15:03:793OC000001BLMT0000666    EVILCORP00010.77SomeClientId    SomeAccountId   ',
    '10:17:45:153NOBLMT0000666    EVILCORP00001.10AnotherClientId AnotherAccountId',
    '10:17:45:487RJAnotherClientId 004price out of range                              ',
    '10:18:28:045NOBLMT0000666    EVILCORP00011.00AnotherClientId AnotherAccountId',
    '10:18:28:472OC000002BLMT0000666    EVILCORP00011.00AnotherClientId AnotherAccountId',
    '10:18:28:642TR0000010000010000666    EVILCORP00010.77',
    '10:18:28:687TR0000010000020000666    EVILCORP00010.77', 
    sep = '\n')

# Create a list of specs. Each item contains the specification for each message
# type of this simple protocol.
specs <- list()
specs[['newOrder']] = data.frame(widths    = c(12, 2, 1, 3, 7, 
                                                12, 8, 16, 16), 
                                  col.names = c('timestamp', 'msgType', 'side', 'type', 'volume', 
                                                'symbol', 'price', 'clientId', 'accountId'))
specs[['orderConf']] = data.frame(widths   = c(12, 2, 6, 1, 3,
                                                7, 12, 8, 16, 16), 
                                  col.names = c('timestamp', 'msgType', 'orderId', 'side', 'type', 
                                                'volume', 'symbol', 'price', 'clientId', 'accountId'))

specs[['rejection']] = data.frame(widths    = c(12, 2, 16, 3, 48), 
                                  col.names = c('timestamp', 'msgType', 'clientId', 'rejectionCode', 'text'))

specs[['trade']] = data.frame(widths   = c(12, 2, 6, 6, 7,
                                           12, 8), 
                              col.names = c('timestamp', 'msgType', 'tradeId', 'orderId', 'volume', 
                                            'symbol', 'price'))

# The selector function is responsible for identifying the message type of a line.
myselector <- function(line, specs) {
    s <- substr(line, 13, 14)
    spec_name = ''
    if (s == 'NO')
        spec_name = 'newOrder'
    else if (s == 'OC')
        spec_name = 'orderConf'
    else if (s == 'TR')
        spec_name = 'trade'
    else if (s == 'RJ')
        spec_name = 'rejection'

    spec_name
}

read.multi.fwf(ff, multi.specs = specs, select = myselector)
#> $newOrder
#>      timestamp msgType side type volume       symbol price
#> 1 10:15:03:279      NO    S  LMT    666     EVILCORP 10.77
#> 2 10:17:45:153      NO    B  LMT    666     EVILCORP  1.10
#> 3 10:18:28:045      NO    B  LMT    666     EVILCORP 11.00
#>           clientId        accountId
#> 1 SomeClientId     SomeAccountId   
#> 2 AnotherClientId  AnotherAccountId
#> 3 AnotherClientId  AnotherAccountId
#> 
#> $orderConf
#>      timestamp msgType orderId side type volume       symbol price
#> 1 10:15:03:793      OC       1    B  LMT    666     EVILCORP 10.77
#> 2 10:18:28:472      OC       2    B  LMT    666     EVILCORP 11.00
#>           clientId        accountId
#> 1 SomeClientId     SomeAccountId   
#> 2 AnotherClientId  AnotherAccountId
#> 
#> $rejection
#>      timestamp msgType         clientId rejectionCode
#> 1 10:17:45:487      RJ AnotherClientId              4
#>                                               text
#> 1 price out of range                              
#> 
#> $trade
#>      timestamp msgType tradeId orderId volume       symbol price
#> 1 10:18:28:642      TR       1       1    666     EVILCORP 10.77
#> 2 10:18:28:687      TR       1       2    666     EVILCORP 10.77

unlink(ff)
```

Installation
------------

So far the package is not published in CRAN. You can either clone the repo and use RStudio to build and install the package or you could use [devtools](https://github.com/hadley/devtools) package to install directly from this repo.

To build the package you will also need: \* roxygen2 \* testthat (to check the package) \* rstudio (optional)
