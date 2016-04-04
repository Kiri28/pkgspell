#' @export
check_pkg <- function(pkg_dir, sections = c("title", "desc", "details",
                                            "params", "value")) {

    if(missing(pkg_dir)) {
        pkg_dir <- getwd()
    }

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
