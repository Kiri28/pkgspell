#' Check a DESCRIPTION file on spelling errors.
#'
#' The function checks a package's DESCRIPTION file on spelling errors,
#' particularely Title: and Description: sections.
#'
#' This function looks for the DESCRIPTION file in \code{pkg_dir} directory,
#' reads this file, and checks Title: and Discription: sections in typos with a
#' help of \code{hunspell} package.
#'
#' \strong{Note:} Each line in section Description: must be intended by 4 spaces.
#'
#' @param pkg_dir a character vector of length one, specifying the location of a
#' package. The default value is the working directory (\code{\link{getwd}}).
#' Missing value will be ignored. The folder must contain file DESCRIPTION.
#'
#' @return A data.frame with columns: File, Line, Word. The File is DESCRIPTION
#' file; Line is the number of the line, where the error appears, and Word is
#' the word with error.
#'
#' @seealso \code{\link{check_pkg}}, \code{\link{check_rd}}
#' @export
check_desc <- function(pkg_dir = getwd()) {

    file <- paste0(pkg_dir, "/DESCRIPTION")
    if(!file.exists(file)) {
        stop("File DESCRIPTION does not exist.")
    }
    typos <- data.frame(File = character(0), Line = integer(),
                        Word = character(0), stringsAsFactors = F)
    fields <- c("Title:", "Description:")
    text <- readLines(con = file)
    for(field in fields) {
        line <- grep(field, text)
        is_curr_field <- length(line) == 1
        while(is_curr_field) {
            bad_words <- hunspell::hunspell(text[line])[[1]]
            for(word in bad_words) {
                typos <- rbind(typos, data.frame(File = "DESCRIPTION",
                                                 Line = line, Word = word,
                                                 stringsAsFactors = F))
            }
            line <- line + 1
            is_curr_field <- substr(text[line], start = 1, stop = 4) == "    "
        }
    }
    return(typos)
}
