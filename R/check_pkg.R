#' Check a package documentation and DESCRIPTION file on spelling errors.
#'
#' \code{check_pkg} checks a package for typos and spelling errors, including
#' DESCRIPTION file (both Title: and Description:), as well as all .Rd files in
#' folder \code{/man}.
#'
#' The function's argument \code{pkg_dir} is the location of the package
#' directory. By default it is the current working directory. The function runs
#' two functions inside: \code{check_desc} and \code{check_dr}. If some
#' files are missing, the function throw a warning and continue executing the
#' rest of the code. Checks are done with a help of \code{hunspell} package.
#'
#' @param pkg_dir a character vector of length one, specifying the location of a
#' package. The default value is the working directory (\code{\link{getwd}}).
#' Missing value will be ignored.
#'
#' @return A data.frame with columns: File, Line, Word. The File is either
#' DESCRIPTION file, or the .R file, which generated .Rd file. Line is the
#' number of the line, where the error appears, and Word is the word with error.
#'
#' @seealso \code{\link{check_desc}}, \code{\link{check_rd}}
#' @export
check_pkg <- function(pkg_dir = getwd(), sections = c("title", "desc", "details",
                                            "params", "value")) {

    if(!dir.exists(pkg_dir)) {
        stop(paste("Directory", pkg_dir, "does not exist."))
    }
    typos <- data.frame(File = character(0), Line = integer(),
                        Word = character(0), stringsAsFactors = F)

    #---- check DESCRIPTION file
    if(file.exists(paste0(pkg_dir, "/DESCRIPTION"))) {
        typos <- rbind(typos, check_desc(pkg_dir = pkg_dir))
    } else {
        warning("Fail to find DESCRIPTION file. This part is skip.")
    }

    #---- check objects documentation
    if(dir.exists(paste0(pkg_dir, "/man")) &&
       length(list.files(path = paste0(pkg_dir, "/man"),
                         pattern = ".Rd")) > 0) {
        typos <- rbind(typos, check_rd(pkg_dir, sections = sections))
    } else {
        warning(paste("Fail to find man/ directory, or there are no object",
                      "documentation files. This part is skip."))
    }

    return(typos)
}
