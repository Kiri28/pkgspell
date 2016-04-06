# NOTE: DESCRIPTION has to have Title: and Description:
# each new line in Description: filed should be intended by 4 spaces
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
