
file <- system.file("extdata", "s_isatab.txt", package="isaeditor")
isa_s <- read_isa(file)

check_integrity <- function(object) {

  expect_equal(nrow(object$contents), object$n)
  expect_true(all(object$node_id %in% object$col_id))
  expect_equal(length(unique(object$node_id)), sum(object$is_node))
  expect_equal(ncol(object$contents), nrow(object$isa_stru))
  expect_equal(colnames(object$contents), object$isa_stru$col_id)

}

test_that("Reading ISA tab study file works", {

  expect_equal(n_row(isa_s), 3)
  expect_equal(length(isa_s), 4)
  expect_setequal(names(isa_s), c("isa_stru", "contents", "n", "type"))
  expect_equal(isa_s$type, "study")
  check_integrity(isa_s)

})

test_that("Removing nodes works", {
  x <- isa_node_rm(isa_s, "ID20")

  expect_false("ID20" %in% x$isa_stru$node_id)
  expect_false("ID20" %in% x$isa_stru$col_id)
  expect_equal(colnames(x$contents), 
c("ID1", "ID2", "ID3", "ID4", "ID5", "ID6", "ID7", "ID8", "ID9", "ID10", "ID11", "ID12", "ID13", "ID14", "ID15", "ID16", "ID17", "ID18", "ID19", "ID22", "ID23", "ID24", "ID25", "ID26", "ID27", 
"ID28", "ID29"))
  check_integrity(x) 

})

test_that("Adding nodes works", {
  x <- isa_node_add(isa_s, "Test Node", columns=c("Test Prop 1", "Test Prop 2")) 
  check_integrity(x)

  expect_error(isa_node_add(isa_s, "Test Node", 
                            columns=c("Test Prop 1", "Test Prop 2"),
                            after_node="ID2")) 
  x <- isa_node_add(isa_s, "Test Node", columns=c("Test Prop 1", "Test Prop 2"), 
                    after_node="ID20")
  check_integrity(x)



})

test_that("Removing properties works", {

  x <- isa_property_rm(isa_s, c("ID3", "ID29"))
  expect_equal(nrow(x$isa_stru), nrow(isa_s$isa_stru) - 2)
  expect_false(any(c("ID3", "ID29") %in% colnames(x$contents)))
  expect_false(any(c("ID3", "ID29") %in% x$isa_stru$col_id))

  check_integrity(x) 

})


test_that("Adding properties works", {
  expect_error(x <- isa_property_add(isa_s, "Test Property", node_id="ID20", after_id="ID4"))
  x <- isa_property_add(isa_s, "Test Property", node_id="ID20", after_id="ID21")

  expect_equal(nrow(x$isa_stru), nrow(isa_s$isa_stru) + 1)
  expect_equal(
               x$isa_stru$col_id[ which(x$isa_stru$col_id == "ID21") + 1 ],
               "ID30")
  expect_equal(x$isa_stru$node_id[ x$isa_stru$col_id == "ID30" ],
               "ID20")

  check_integrity(x)
})
