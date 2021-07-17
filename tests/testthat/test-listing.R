## check whether listing of different objects works

test_that("node_list works", {

  nl <- node_list(isa_s)
  expect_true(is(nl, "data.frame"))
  expect_equal(nrow(nl), 3)
  expect_equal(ncol(nl), 4)
  expect_true(setequal(nl$node_id, isa_s$isa_stru$node_id))

})

test_that("node_show works", {

  ns <- node_show(isa_s, "ID1")
  expect_true(is(ns, "data.frame"))
  expect_equal(nrow(ns), 19)
  expect_equal(ncol(ns), 4)
  expect_equal(sum(ns$is_node), 1)
  expect_true(all(ns$col_id %in% 
                  isa_s$isa_stru$col_id[ isa_s$isa_stru$node_id == "ID1" ]))


})


test_that("node_select works", {
  expect_error(node_select(isa_s, c("ID1", "ID2")))
  x <- node_select(isa_s, c("ID1", "ID22"))
  check_integrity(x)
  expect_true(setequal(x$isa_stru$node_id, c("ID1", "ID22")))
  x <- node_select(isa_s, c("ID1"), inverse=TRUE)
  check_integrity(x)
  expect_false("ID1" %in% x$isa_stru$col_id)
})


test_that("prop_select works", {
  expect_error(prop_select(isa_s, c("ID1")))
  x <- prop_select(isa_s, c("ID2"))
  check_integrity(x)
  expect_equal(nrow(x$isa_s), 4)
  x <- prop_select(isa_s, c("ID2"), inverse=TRUE)

})
