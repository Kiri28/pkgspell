# sections namme as in Rd2roxygen::parse_file()
#' @export
check_rd <- function(pkg_dir, sections = c("title", "desc", "details", "params",
                                           "value")) {
    if(missing(pkg_dir)) {
        pkg_dir <- getwd()
    }

    if(!dir.exists(paste0(pkg_dir, "/man")) ||
       length(list.files(path = paste0(pkg_dir, "/man"),
                         pattern = ".Rd")) == 0) {
        stop(paste0("Directory", paste0(pkg_dir, "/man"), "does not exist,",
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
