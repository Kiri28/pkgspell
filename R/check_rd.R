#' Check each .Rd file on spelling errors.
#'
#' \code{check_rd} checks each .Rd generated file in folder \code{/man} on typos
#' and spelling errors.
#'
#' This function goes through each file in folder \code{/man}, which has
#' extenssion .Rd and checks for a wrong spelling. Only limited set of sections
#' supported: title, description, details, parameters, and return. If the
#' section is missing, the function throws an massage. Custom sections are not
#' supported, due to limitations of package \code{Rd2roxygen}. The function uses
#' \code{hunspell} package to find typos. Instead of indicating the location of
#' the typo in .Rd file, the function seeks for the .R file in folder \code{/R},
#' which generated the .Rd file, and returns the line of the roxygen2 comment,
#' which generates such an error.
#'
#' @param pkg_dir a character vector of length one, specifying the location of a
#' package. The default value is the working directory (\code{\link{getwd}}).
#' Missing value will be ignored. The folder must contain a folder \code{/man}
#' and \code{/R}.
#'
#' @return A data.frame with columns: File, Line, Word. The File is .R file,
#' which generated correspondent .Rd file with an error, Line is the number of
#' the line, where the error appears, and Word is the word with error.
#'
#' @seealso \code{\link{check_pkg}}, \code{\link{check_desc}}
#' @export
check_rd <- function(pkg_dir = getwd(), sections = c("title", "desc", "details", "params",
                                           "value")) {

    if(!dir.exists(paste0(pkg_dir, "/man")) ||
       length(list.files(path = paste0(pkg_dir, "/man"),
                         pattern = ".Rd")) == 0) {
        stop(paste0("Directory", paste0(pkg_dir, "/man"), "does not exist, ",
                    "or there no object documentation files"))
    }
    typos <- data.frame(File = character(0), Line = integer(),
                        Word = character(0), stringsAsFactors = F)
    for(file in list.files(path = paste0(pkg_dir, "/man"), pattern = ".Rd")) {
        full_path <- paste0(pkg_dir, "/man/", file)
        source_file <- substr(readLines(full_path)[2], start = 32,
                              stop = nchar(readLines(full_path)[2]))
        text <- Rd2roxygen::parse_file(path = full_path)
        for(sec in sections) {
            if(is.null(text[[sec]])) {
                message(paste("File", file, "does not contain section", sec))
            } else {
                bad_words <- unlist(hunspell::hunspell(text[[sec]]))
                for(word in bad_words) {
                    text_source <- readLines(paste0(pkg_dir, source_file))
                    lines <- grep(word, text_source)
                    for(i in lines) {
                        if(substr(text_source[i], start = 1,
                                  stop = 2) == "#'") {
                            typos <- rbind(typos,
                                           data.frame(File = source_file,
                                                      Line = i, Word = word,
                                                      stringsAsFactors = F))

                        }
                    }
                }
            }
        }
    }
    typos <- typos[duplicated(typos), ]
    return(typos)
}
