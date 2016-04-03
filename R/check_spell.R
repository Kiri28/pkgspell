check_spell <- function(path) {

    if(!dir.exists(path)) {
        stop(paste("Directory", path, "does not exist."))
    }
    typos <- data.frame(File = character(0), Line = integer(),
                        Word = character(0), stringsAsFactors = F)

    #---- check DESCRIPTION file
    if(file.exists(paste0(path, "DESCRIPTION"))) {
        typos <- rbind(typos, check_descr(paste0(path, "DESCRIPTION")))
    } else {
        warning("Fail to find DESCRIPTION file. This part is skip.")
    }

    #---- check objects documentation
    if(dir.exists(paste0(path, "man/"))) {
        files <- list.files(path = paste0(path, "man/"), pattern = ".Rd")
        if(length(files) != 0) {
            typos <- 1 # here go over all .rd files and find misspells
        } else {
            messege("There are no object documentation files.")
        }
    } else {
        warning("Fail to find man/ directory. This part is skip.")
    }

    return(typos)
}

# NOTE: DESCRIPTION has to have Title: and Description:
# each new line in Description: filed should be intended by 4 spaces
check_descr <- function(dfile) {
    if(!file.exists(dfile)) {
        stop("File DESCRIPTION does not exist.")
    }
    typos <- data.frame(File = character(0), Line = integer(),
                        Word = character(0), stringsAsFactors = F)
    fields <- c("Title:", "Description:")
    text <- readLines(con = dfile)
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
