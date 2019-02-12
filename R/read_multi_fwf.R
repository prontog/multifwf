#' Read Fixed Width Format Files containing lines of different Type
#' 
#' Read a table of fixed width formatted data of different types into a \link{tibble} for each type.
#' 
#' @param file either a path to a file, a connection, or literal data (either a single string or a raw vector). 
#' 
#' Files ending in .gz, .bz2, .xz, or .zip will be automatically uncompressed. Files starting with http://, https://, ftp://, or ftps:// will be automatically downloaded. Remote gz files can also be automatically downloaded and decompressed.
#' Literal data is most useful for examples and tests. It must contain at least one new line to be recognised as data (instead of a path) or be a vector of greater than length 1.
#' Using a value of clipboard() will read from the system clipboard.
#' @param multi.specs A named list of data.frames containing the following columns:
#'     \tabular{ll}{
#'         widths \tab   see \link{fwf_widths}\cr
#'         col_names \tab see \link{fwf_widths}\cr
#'     }
#' 
#' For more info on these fields see \code{\link{read_fwf}}.
#' 
#' Note that each list item should have a name. This is important for the select function.
#' @param select A function to select the type of a line. This selector should have parameters:
#'     \tabular{ll}{
#'         line \tab   the line\cr
#'         specs \tab  the multi.specs list that was passed to read.multi.fwf\cr
#'     }
#'     
#' The select function should return the name of the spec that matches the line. read.multi.fwf will then use this name to select the a spec from the passed multi.spec. This is why multi.spec should be a named list. 
#' If there is no match then NULL can be returned.
#' @param skip number of initial lines to skip; see \link{read_fwf}.
#' @param n the maximum number of records (lines) to be read, defaulting to no limit.
#' @param ... further arguments to be passed to \link{read_fwf}.
#' @return Return value is a named list with an item for each spec in multi.spec. If there was at least one line in file, matching a spec, then the named item will be a \link{tibble}. Otherwise it will be NULL.
#' @author
#' Panos Rontogiannis \email{panos@ronto.net}
#' @seealso \code{\link{read_fwf}}
#' @examples
#' ff <- tempfile()
#' cat(file = ff, '123456', '287654', '198765', sep = '\n')
#' specs <- list()
#' specs[['sp1']] = data.frame(widths = c(1, 2, 3), 
#'                             col_names = c('Col1', 'Col2', 'Col3'))
#' specs[['sp2']] = data.frame(widths = c(3, 2, 1), 
#'                             col_names = c('C1', 'C2', 'C3'))
#' 
#' myselector <- function(line, specs) {
#'     s <- substr(line, 1, 1)
#'     spec_name = ''
#'     if (s == '1')
#'         spec_name = 'sp1'
#'     else if (s == '2')
#'         spec_name = 'sp2'
#' 
#'     spec_name
#' }
#' 
#' read_multi_fwf(ff, multi.specs = specs, select = myselector)    
#' #> sp1: 1 23 456 \ 1 98 765, sp2: 287 65 4
#' 
#' unlink(ff)
#' @export
read_multi_fwf <- function(file,
                           multi.specs,
                           select,
                           skip = 0,
                           n = -1,
                           ...) {
    ##### Helper functions #############################
    
    # Convert each spec into a list containing a SPEC field. Also add
    # the FILENAME field for the temp file where all lines of this type (spec)
    # will be stored (temporarily) for read.fwf.
    prepareAsList <- function(s) {
        s <- list(SPEC = s)
        s$FILENAME <- tempfile("Rmfwf.")
        return(s)
    }
    
    # Parse a line and write to the temp file of the matching spec.
    doone <- function(line) {
        spec_name <- select(line, multi.specs)
        s <- extended.specs[[spec_name]]
        if (is.null(s))
            return()
        cat(file = s$FILENAME, line, "\n", append = TRUE)
        invisible()
    }
    
    # Read Fixed-Width Format temp file.
    readFwf <- function(s) {
        fi <- file.info(s$FILENAME)
        if (!is.na(fi$size) & fi$size > 0) {
            s$Data <- readr::read_fwf(file = s$FILENAME,
                               col_positions = readr::fwf_widths(widths = s$SPEC$widths,
                                                                 col_names = s$SPEC$col_names),
                               ...)
        }
        else {
            #s$Data <- NA
            s <- NULL
        }
        return(s)
    }
    
    # Prepare an element for return.
    prepareRetval <- function(s) {
        s <- s$Data
        return(s)
    }
    ####################################################
    
    extended.specs <- lapply(multi.specs, FUN = prepareAsList)
    
    if (is.character(file)) {
        file <- file(file, "rt")
        on.exit(close(file), add = TRUE)
    }
    else if (!isOpen(file)) {
        open(file, "rt")
        on.exit(close(file), add = TRUE)
    }
    
    # Skip lines.
    if (skip)
        readLines(file, n = skip)
    
    repeat ({
        if (n == 0L)
            break
        if (n == -1L)
            n <- 16
        
        raw <- readLines(file, n = n)
        nread <- length(raw)
        if (nread == 0)
            break
        
        lapply(raw, FUN = doone)
    })
    
    extended.specs <- lapply(extended.specs, FUN = readFwf)
    loaded.data <- lapply(extended.specs, FUN = prepareRetval)
    
    return(loaded.data)
}
