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

# NOTE: DESCRIPTION has to have Title: and Description:
# each new line in Description: filed should be intended by 4 spaces
#' @export
check_desc <- function(pkg_dir) {
    if(missing(pkg_dir)) {
        pkg_dir <- getwd()
    }
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
