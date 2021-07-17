## check integrity of an isatab object
check_integrity <- function(object) {

  expect_equal(nrow(object$contents), object$n)
  expect_true(all(object$node_id %in% object$col_id))
  expect_equal(length(unique(object$node_id)), sum(object$is_node))
  expect_equal(ncol(object$contents), nrow(object$isa_stru))
  expect_equal(colnames(object$contents), object$isa_stru$col_id)
  cols_are_vectors <- unlist(lapply(object$contents, is.vector))
  expect_true(all(cols_are_vectors))

}
