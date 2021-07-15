## definition of the isa investigation file blocks
blocks <- c(
            "ONTOLOGY SOURCE REFERENCE",
            "INVESTIGATION",
            "INVESTIGATION PUBLICATIONS",
            "INVESTIGATION CONTACTS",
            "STUDY")

## definition of the isa investigation file study blocks
study_blocks <- c(
            "STUDY DESIGN DESCRIPTORS",
            "STUDY PUBLICATIONS",
            "STUDY FACTORS",
            "STUDY ASSAYS",
            "STUDY PROTOCOLS",
            "STUDY CONTACTS"
            )


## parse a single study protocol
.protocol_digest <- function(p_id, proto) {

  p_components <- 1:nrow(proto)
  names(p_components) <- proto[[1]]
  #ret <- lapply(p_components, function(pc) proto[[p_id]][pc])
  ret <- list()

  param_sel <- grep("Study Protocol Parameters", proto[["Section"]])
  text <- paste0(proto[[p_id]][param_sel], collapse="\n")
  ret[["parameters"]] <- cbind(
                                proto[["Section"]][param_sel],
                                read_delim(text, delim=";", col_names=FALSE))
                                     
  param_sel <- grep("Study Protocol Components", proto[["Section"]])
  text <- paste0(proto[[p_id]][param_sel], collapse="\n")
  ret[["components"]] <- cbind(
                                proto[["Section"]][param_sel],
                                read_delim(text, delim=";", col_names=FALSE))
                                     
  ret
}

## parse study protocols
.process_study_protocols <- function(proto) {

  protocol_ids <- paste0("ID", 1:(ncol(proto) - 1))
  
  names(protocol_ids) <- protocol_ids
  colnames(proto) <- c("Section", protocol_ids)

  protocols <- lapply(protocol_ids, .protocol_digest, proto)

  protocols
}

## parse the PROCESS section of a STUDY
.process_section <- function(section) {

  ret <- read_tsv(paste(section, collapse="\n"), col_names=FALSE)

  if(ncol(ret) < 2) {
    ret <- cbind(ret, "")
  }

  colnames(ret) <- c("Field", paste0("C", 1:(ncol(ret) - 1)))

  return(ret)
}

## parse the STUDY section of an INVESTIGATION
.process_study <- function(study) {

  study <- lapply(study, .process_section)
  study$.protocols <- .process_study_protocols(study[["STUDY PROTOCOLS"]])

  study
}

## parse the INVESTIGATION file. Called from read_isa()
.read_investigation <- function(file_name) {

  con <- file(file_name, open="r")
  on.exit(close(con))
  lines <- readLines(con)

  read_blocks <- list()
  .studies <- list()
  cur_block <- NA
  cur_study_block <- NA

  ## first, split everything into blocks
  for(l in lines) {

    if(l %in% blocks) {
      
      cur_block <- l

      if(cur_block == "STUDY") {
        cur_study_n <- length(.studies) + 1
        cur_study_block <- cur_block
        .studies[[cur_study_n]] <- list()
        #read_blocks[["studies"]][[cur_study_n]][[cur_study_block]] <- list()
      } else {
        ## only STUDY may be repeated multiple times
        if(!is.null(read_blocks[[cur_block]])) {
          stop(glue("Block {cur_block} in file {file_name} repeated twice, aborting"))
        }
      }

    } else {
      # not a major block
      if(cur_block == "STUDY") {
        # part of a study block
        if(l %in% study_blocks) {
          # start new study block
          cur_study_block <- l
          # study blocks cannot be repeated either
          stopifnot(is.null(.studies[[cur_study_n]][[cur_study_block]]))
          #read_blocks[["studies"]][[cur_study_n]][[cur_study_block]] <- list()
        } else {
          # append line to the cur_study_block
          .studies[[cur_study_n]][[cur_study_block]] <- c(
            .studies[[cur_study_n]][[cur_study_block]],
            l)
        }
      } else {
        # part of a regular block
        stopifnot(!is.na(cur_block))
        read_blocks[[cur_block]] <- c(read_blocks[[cur_block]], l)
      }
    }
  }

  
  read_blocks <- lapply(read_blocks, .process_section)
  read_blocks[[".studies"]] <- lapply(.studies, .process_study)


  class(read_blocks) <- "isa_i"
  return(read_blocks)
}

## ensure that data frames have the same columns and then rbind them
.vmerge <- function(x) {

  all_cols <- unique(unlist(lapply(x, colnames)))

  ret <- lapply(x, function(df) {
                for(.col in setdiff(all_cols, colnames(df))) {
                  df[[.col]] <- NA
                }
                df[ , all_cols ]
            })
  Reduce(rbind, ret)

}

#' @export
print.isa_i <- function(x, ...) {

  bl <- setdiff(blocks, "STUDY")
  names(bl) <- bl
  tmp <- lapply(bl, function(b) {
                  tibble(Section=b, Subsection=NA, x[[b]])
            })
  

  tmp.studies <- lapply(1:length(x$.studies),
                        function(i) {
                          ret <- lapply(study_blocks, function(sbl) {
                            tibble(Section=glue("STUDY {i}"), Subsection=sbl, x$.studies[[i]][[sbl]])
                          })
                          .vmerge(ret)
                        })

 print(as.colorDF(.vmerge(c(tmp, tmp.studies))), ...)
  
}

#' @export
summary.isa_i <- function(object, ...) {
  x <- object

  cat(glue("An object of class isa_i\nInvestigation with {length(x$.studies)} studies:"))
  cat("\n")
  
  tmp <- x[["INVESTIGATION"]] %>% 
    mutate(Field=gsub("Investigation ", "", .data[["Field"]])) %>%
    filter(!is.na(.data[["C1"]]))
  for(i in 1:nrow(tmp)) {
    cat("  ")
    cat(paste(tmp[i,], collapse=": "))
    cat("\n")
  }

  cat("Studies:\n")
  for(i in 1:length(x$.studies)) {
    cat(glue("  Study {i}:"))
    cat("\n")
    tmp <- x$.studies[[i]][["STUDY"]] %>% 
      mutate(Field=gsub("Study ", "", .data[["Field"]])) %>%
      filter(!is.na(.data[["C1"]]))
    for(i in 1:nrow(tmp)) {
      cat("    ")
      cat(paste(tmp[i,], collapse="\t"))
      cat("\n")
    }
  }


}


