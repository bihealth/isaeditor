## definition of the isa investigation file blocks
blocks <- c("ONTOLOGY SOURCE REFERENCE", "INVESTIGATION", "INVESTIGATION PUBLICATIONS", "INVESTIGATION CONTACTS", "STUDY")

blocks_names <- c(
 "ONTOLOGY SOURCE REFERENCE"  = "ONTOLOGY_SOURCE_REFERENCE",
 "INVESTIGATION"              = "INVESTIGATION",
 "INVESTIGATION PUBLICATIONS" = "INVESTIGATION_PUBLICATIONS",
 "INVESTIGATION CONTACTS"     = "INVESTIGATION_CONTACTS",
 "STUDY"                      = "STUDY"
)

## definition of the isa investigation file study blocks
study_blocks <- c("STUDY DESIGN DESCRIPTORS", "STUDY PUBLICATIONS", "STUDY FACTORS", "STUDY ASSAYS", "STUDY PROTOCOLS", "STUDY CONTACTS")

study_blocks_names <- c(
 "STUDY"                      = "STUDY",
 "STUDY DESIGN DESCRIPTORS"   = "STUDY_DESIGN_DESCRIPTORS",
 "STUDY PUBLICATIONS"         = "STUDY_PUBLICATIONS",
 "STUDY FACTORS"              = "STUDY_FACTORS",
 "STUDY ASSAYS"               = "STUDY_ASSAYS",
 "STUDY PROTOCOLS"            = "STUDY_PROTOCOLS",
 "STUDY CONTACTS"             = "STUDY_CONTACTS")

protocol_fields_names <- c(
 "Name"                                   = "Name",
 "Type"                                   = "Type",
 "Type Term Accession Number"             = "Type Term Accession Number",
 "Type Term Source REF"                   = "Type Term Source REF",
 "Description"                            = "Description",
 "URI"                                    = "URI",
 "Version"                                = "Version",
 "Parameters Name"                        = "Parameters_Name",
 "Parameters Name Term Accession Number"  = "Parameters_Name_Term_Accession_Number",
 "Parameters Name Term Source REF"        = "Parameters_Name_Term_Source_REF",
 "Components Name"                        = "Components_Name",
 "Components Type"                        = "Components_Type",
 "Components Type Term Accession Number"  = "Components_Type_Term_Accession_Number",
 "Components Type Term Source REF"        = "Components_Type_Term_Source_REF"
 )

protocol_fields <- c(
  "Name",
  "Type",
  "Type Term Accession Number",
  "Type Term Source REF",
  "Description",
  "URI",
  "Version",
  "Parameters Name",
  "Parameters Name Term Accession Number",
  "Parameters Name Term Source REF",
  "Components Name",
  "Components Type",
  "Components Type Term Accession Number",
  "Components Type Term Source REF")

# ---------------------------------------------------------------------
# Functions to convert the ISA investigation file to text
# ---------------------------------------------------------------------

# reverse the .process_section function
.section_to_text <- function(section, title = NULL) {

  section[is.na(section)] <- '""'

  # convert the data frame to a character vector of length 1
  # by pasting the columns and rows together

  # we don't use format_delim here because of the weird way that isatab is
  # requiring quotes
  ret <- paste0(
    # inner: paste the columns together
    apply(section, 1, \(x) paste(x, collapse = "\t")),
    collapse = "\n"
  )

  if(!is.null(title)) {
    ret <- paste0(title, "\n", ret)
  }

  ret
}

# convert the whole investigation to text
.investigation_to_text <- function(i) {

  if(!"isa_i" %in% class(i)) {
    stop("Object is not of class isa_i")
  }
  
  out <- ""

  ret <- lapply(blocks, \(section) {
      id <- blocks_names[section]

      # studies have their own function
      if(id == "STUDY") {
        .studies_to_text(i$STUDIES)
      } else {
        .section_to_text(i[[id]], section)
      }
  })

  ret <- paste0(ret, collapse = "\n")
  ret
}

# convert a protocol to a single data frame
.protocol_to_df <- function(prot) {

  ret <- prot$Protocol 

  colnames(ret) <- c("Section", "Value")
  ret$Value[is.na(ret$Value)] <- '""'

  p <- prot$Parameters
  p <- data.frame(Section = p$Label,
                  Value = apply(p[, -1, drop = F], 1, \(x) {
                          x[is.na(x)] <- ''
                          paste0('"', paste0(x, collapse = ";"), '"')
                  }))
  ret <- rbind(ret, p)

  p <- prot$Components
  p <- data.frame(Section = p$Label,
                  Value = apply(p[, -1, drop = F], 1, \(x) {
                          x[is.na(x)] <- ''
                          paste0('"', paste0(x, collapse = ";"), '"')
                  }))
  ret <- rbind(ret, p)

  # make sure that the protocol fields are correct
  if(!setequal(ret$Section, protocol_fields)) {
    actual <- paste(ret$Section, collapse = ", ")
    forbidden <- paste(setdiff(ret$Section, protocol_fields), collapse = ", ")
    missing <- paste(setdiff(protocol_fields, ret$Section), collapse = ", ")
    stop(glue("Protocol fields do not match the expected fields;\nactual: {actual}\nmissing: {missing}\nnot allowed: {forbidden}"))
  }

  # put the fields in a specific order
  ret <- ret[ match(protocol_fields, ret$Section), ]

  # add the section name
  ret$Section <- paste0("Study Protocol ", ret$Section)
  ret
}

# convert a list of protocols to text
.protocol_to_text <- function(protocols) {

  prot_df <- lapply(protocols, .protocol_to_df)
  prot_df <- Reduce(\(x, y) cbind(x, y$Value), prot_df)
  .section_to_text(prot_df, "STUDY PROTOCOLS")

}

# convert a study to text
.study_to_text <- function(study) {

  ret <- lapply(study_blocks, \(section) {
      id <- study_blocks_names[section]

      # protocols have their own function
      if(id == "STUDY_PROTOCOLS") {
        .protocol_to_text(study$STUDY_PROTOCOLS)
      } else {
        .section_to_text(study[[id]], section)
      }
  })

  ret <- paste0(ret, collapse = "\n")
  ret <- paste0(.section_to_text(study$STUDY, "STUDY"), "\n", ret)
  ret
}

# convert a list of studies to text
.studies_to_text <- function(studies) {

  ret <- lapply(studies, .study_to_text)
  paste0(ret, collapse = "\n")
}


# ---------------------------------------------------------------------
# Functions to parse the ISA investigation file
# ---------------------------------------------------------------------

# read_delim with sensible options
.text_to_df <- function(text) {
  read_delim(text, delim = ";", na="NA", col_names = FALSE, 
                   show_col_types = FALSE, progress = FALSE, name_repair = "unique_quiet")
}

## parse a single study protocol
.protocol_digest <- function(p_id, proto) {

  ret <- list()
  proto[["Section"]] <- gsub("Study Protocol ", "", proto[["Section"]])

  param_sel <- !grepl("^(Parameters|Components)", proto[["Section"]])
  ret$Protocol <- proto[param_sel, c("Section", p_id)]

  param_sel <- grep("^Parameters", proto[["Section"]])
  text <- paste0(proto[[p_id]][param_sel], collapse = "\n")

  df <- .text_to_df(text)

  ret[["Parameters"]] <- cbind(proto[["Section"]][param_sel], df)
  colnames(ret[["Parameters"]]) <- c("Label", paste0("P", 1:(ncol(ret[["Parameters"]]) - 1)))

  param_sel <- grep("^Components", proto[["Section"]])
  text <- paste0(proto[[p_id]][param_sel], collapse = "\n")

  df <- .text_to_df(text)

  ret[["Components"]] <- cbind(proto[["Section"]][param_sel], df)
  colnames(ret[["Components"]]) <- c("Label", paste0("C", 1:(ncol(ret[["Components"]]) - 1)))

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

## parse a section
.process_section <- function(section) {

    ret <- read_tsv(paste(section, collapse = "\n"), 
                    col_names = FALSE, 
                    progress = FALSE, 
                    show_col_types = FALSE,
                    name_repair = "unique_quiet")

    colnames(ret) <- c("Field", paste0("C", 1:(ncol(ret) - 1)))

    return(ret)
}

## parse the STUDY section of an INVESTIGATION
.process_study <- function(study, dir) {

  study <- lapply(study, .process_section)
  study$STUDY_PROTOCOLS <- .process_study_protocols(study[["STUDY_PROTOCOLS"]])

  f_name <- study$STUDY$C1[ study$STUDY$Field == "Study File Name" ]
  message("Reading study isatab file ", f_name)
  study$ISATAB <- read_isa(file.path(dir, f_name))

  assays <- setdiff(colnames(study$STUDY_ASSAYS), "Field")
  sel <- which(study$STUDY_ASSAYS$Field == "Study Assay File Name")
  assay_files <- lapply(assays, \(x) {
      study$STUDY_ASSAYS[[x]][sel]
  })

  study$.assays <- lapply(file.path(dir, assay_files), read_isa)
  names(study$.assays) <- assays

  study
}

.parse_blocks <- function(lines) {

  read_blocks <- list()
  .studies <- list()
  cur_block <- NA
  cur_study_block <- NA

  ## first, split everything into blocks
  for (l in lines) {

      if (l %in% blocks) {

          cur_block <- l
          cur_block_name <- blocks_names[cur_block]

          if (cur_block == "STUDY") {
              cur_study_n <- length(.studies) + 1

              cur_study_block <- "STUDY"
              cur_study_block_name <- "STUDY"

              .studies[[cur_study_n]] <- list()
              # read_blocks[['studies']][[cur_study_n]][[cur_study_block]] <- list()
          } else {
              ## only STUDY may be repeated multiple times
              if (!is.null(read_blocks[[cur_block_name]])) {
                stop(glue("Block {cur_block} in file {file_name} repeated twice, aborting"))
              }
          }

      } else {
          # not a major block
          if (cur_block == "STUDY") {
              # part of a study block
              if (l %in% study_blocks) {
                # start new study block
                cur_study_block <- l
                cur_study_block_name <- study_blocks_names[cur_study_block]

                # study blocks cannot be repeated either
                stopifnot(is.null(.studies[[cur_study_n]][[cur_study_block_name]]))

                # read_blocks[['studies']][[cur_study_n]][[cur_study_block]] <- list()
              } else {
                # append line to the cur_study_block
                .studies[[cur_study_n]][[cur_study_block_name]] <- c(.studies[[cur_study_n]][[cur_study_block_name]], l)
              }
          } else {
              # part of a regular block
              stopifnot(!is.na(cur_block))
              read_blocks[[cur_block_name]] <- c(read_blocks[[cur_block_name]], l)
          }
      }
  }

  return(list(blocks = read_blocks, studies = .studies))
}

## parse the INVESTIGATION file. Called from read_isa()
.read_investigation <- function(file_name) {

  dir  <- dirname(file_name)
  base <- basename(file_name)

  con <- file(file_name, open = "r")
  on.exit(close(con))
  lines <- readLines(con)

  parsed <- .parse_blocks(lines)
  read_blocks <- parsed$blocks
  studies <- parsed$studies
  read_blocks <- lapply(read_blocks, .process_section)
  read_blocks[["STUDIES"]] <- lapply(studies, .process_study, dir = dir)

  class(read_blocks) <- "isa_i"
  return(read_blocks)
}

## ensure that data frames have the same columns and then rbind them
.vmerge <- function(x) {

    all_cols <- unique(unlist(lapply(x, colnames)))

    ret <- lapply(x, function(df) {
        for (.col in setdiff(all_cols, colnames(df))) {
            df[[.col]] <- NA
        }
        df[, all_cols]
    })
    Reduce(rbind, ret)

}

#' @export
print.isa_i <- function(x, ...) {

    bl <- setdiff(blocks, "STUDY")
    names(bl) <- bl
    tmp <- lapply(bl, function(b) {
        tibble(Section = b, Subsection = NA, x[[b]])
    })


    tmp.studies <- lapply(1:length(x$STUDIES), function(i) {
        ret <- lapply(study_blocks, function(sbl) {
            tibble(Section = glue("STUDY {i}"), Subsection = sbl, x$STUDIES[[i]][[sbl]])
        })
        .vmerge(ret)
    })

    print(as.colorDF(.vmerge(c(tmp, tmp.studies))), ...)

}

.study_summary <- function(study, prefix = "") {

  tmp <- study[["STUDY"]] |>
      mutate(Field = gsub("Study ", "", .data[["Field"]])) |>
      filter(!is.na(.data[["C1"]]))

  for (i in 1:nrow(tmp)) {
    cat(prefix)
    cat(paste(tmp[i, ], collapse = "\t"))
    cat("\n")
  }

  cat(glue("{prefix}Assays:"))
  cat("\n")
  assays <- setdiff(colnames(study$STUDY_ASSAYS), "Field")
  sel <- which(study$STUDY_ASSAYS$Field == "Study Assay File Name")
  for(a in assays) {
    cat(glue("{prefix}  file: {study$STUDY_ASSAYS[[a]][sel]}"))
    cat("\n")
  }

}


#' @export
summary.isa_i <- function(object, ...) {
    x <- object

    cat(glue("An object of class isa_i\nInvestigation with {length(x$STUDIES)} studies:"))
    cat("\n")

    tmp <- x[["INVESTIGATION"]] %>%
        mutate(Field = gsub("Investigation ", "", .data[["Field"]])) %>%
        filter(!is.na(.data[["C1"]]))
    for (i in 1:nrow(tmp)) {
        cat("  ")
        cat(paste(tmp[i, ], collapse = ": "))
        cat("\n")
    }

    cat("Studies:\n")
    for (i in 1:length(x$STUDIES)) {
        cat(glue("  Study {i}:"))
        cat("\n")
        .study_summary(x$STUDIES[[i]], "    ")
    }


}

#' Write the ISAtab investigation to a file
#'
#' Write the ISAtab investigation to a file
#' @param file The name of the file to write to. If missing, the
#' investigation is returned as a character vector.
#' @param isa_i The isa_i object to write
#' @return If file is missing, the investigation as a character vector. If
#' not, the file is written and the return value is invisible.
#' @export
write_investigation <- function(isa_i, file = NULL) {

    ret <- .investigation_to_text(isa_i)

    if(!is.null(file)) {
      con <- file(file, open = "w")
      on.exit(close(con))
      cat(ret, file = con)
      return(invisible(ret))
    } else {
      return(ret)
    }
}






