test_that("Reading ISA tab study file works", {

  expect_equal(n_row(isa_s), 3)
  expect_equal(length(isa_s), 4)
  expect_setequal(names(isa_s), c("isa_stru", "contents", "n", "type"))
  expect_equal(isa_s$type, "study")
  check_integrity(isa_s)

})

test_that("Removing nodes works", {
  x <- isa_node_rm(isa_s, "ID8")

  expect_false("ID8" %in% x$isa_stru$node_id)
  expect_false("ID8" %in% x$isa_stru$col_id)
  expect_equal(colnames(x$contents), c("ID1", "ID2", "ID3", "ID4", "ID5", "ID6", "ID7"))
  check_integrity(x) 

})

test_that("Adding nodes works", {
  x <- isa_node_add(isa_s, "Test Node", columns=c("Test Prop 1", "Test Prop 2")) 
  check_integrity(x)

  expect_error(isa_node_add(isa_s, "Test Node", 
                            columns=c("Test Prop 1", "Test Prop 2"),
                            after_node="ID2")) 
  x <- isa_node_add(isa_s, "Test Node", columns=c("Test Prop 1", "Test Prop 2"), 
                    after_node="ID8")
  check_integrity(x)



})

test_that("Removing properties works", {

  x <- isa_property_rm(isa_s, c("ID9", "ID15"))
  expect_equal(nrow(x$isa_stru), nrow(isa_s$isa_stru) - 2)
  expect_false(any(c("ID9", "ID15") %in% colnames(x$contents)))
  expect_false(any(c("ID9", "ID15") %in% x$isa_stru$col_id))

  check_integrity(x) 

})


test_that("Adding properties works", {
  expect_error(x <- isa_property_add(isa_s, "Test Property", node_id="ID8", after_id="ID4"))
  expect_error(x <- isa_property_add(isa_s, "Test Property", node_id="ID20", after_id="ID4"))
  x <- isa_property_add(isa_s, "Test Property", node_id="ID8", after_id="ID9")

  expect_equal(nrow(x$isa_stru), nrow(isa_s$isa_stru) + 1)
  expect_equal(
               x$isa_stru$col_id[ which(x$isa_stru$col_id == "ID9") + 1 ],
               "ID16")
  expect_equal(x$isa_stru$node_id[ x$isa_stru$col_id == "ID16" ],
               "ID8")

  check_integrity(x)
})
