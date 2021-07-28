## make sure that isa_stru and contents elements of isatab object match
.check_integrity <- function(x) {

    stopifnot(!any(duplicated(x$col_id)))
    stopifnot(nrow(x$isa_stru) == ncol(x$contents))
    stopifnot(setequal(x$isa_stru$col_id, colnames(x$contents)))

    x
}

## get the col_name from col_id
.col_id_to_name <- function(x, id) {
    x$isa_stru$col_name[x$isa_stru$col_id == id]
}

## provide a summary of values
.val_summary <- function(x) {
    if (all(is.na(x))) {
        return("All missing")
    }

    n_uniq <- length(unique(x))
    if (n_uniq == 1) {
        return(glue("One value: {x[1]}"))
    }

    if (n_uniq == length(x)) {
        return(glue("All unique; {x[1]}..."))
    }

    n <- length(unique(x))
    glue("{n} unique: {x[1]}...")
}

## generate an ID which is not present in the x
.new_id <- function(x, n = 1) {
    stopifnot(is(x, "isatab"))

    ids <- as.numeric(gsub("ID", "", x$isa_stru$col_id))

    ret <- (max(ids) + 1):(max(ids) + n)
    ret <- paste0("ID", ret)
    ret
}


