test_that("`[[` works", {
  expect_equal(isa_s[["ID1"]], c("alpha", "beta", "gamma"))
  expect_equal(isa_s[["ID1"]], isa_s$contents[["ID1"]])
  expect_error(isa_s[["XXX"]])
})


test_that("`[[<-` works", {

  x <- isa_s
  x[["ID1"]] <- 1:3

  expect_equal(x[["ID1"]], 1:3)
  expect_equal(x$contents[["ID1"]], 1:3)
  expect_error(x[["XXX"]] <- 1:3)
  expect_s3_class(x[[c("ID1", "ID3")]], "data.frame")

  check_integrity(x)
})


test_that("`[` works", {

  x <- isa_a
  check_integrity(x)

  expect_equal(x["Sample Name"], x$contents[["ID1"]])
  expect_error(x["Extract Name"])
  expect_equal(x["Extract Name", n=1], x$contents[["ID6"]])
  expect_equal(x["Extract Name", n=2], x$contents[["ID34"]])
})


test_that("`[<-` works", {
  x <- isa_s

  # simple inserting of values
  x[ "Source Name" ] <- 1:3
  check_integrity(x)
  x[ "Source Name" ] <- data.frame(1:3)
  check_integrity(x)
  x[ "Source Name", "Characteristics[UUID]" ] <- 1:3
  check_integrity(x)
  x[ "Source Name", "Characteristics[UUID]" ] <- data.frame(1:3)
  check_integrity(x)

  x <- isa_s
  
  # check that removing properties works
  expect_message(x["Source Name", "Characteristics[UUID]"] <- NULL)
  check_integrity(x)
  y <- isa_property_rm(isa_s, prop_ids = "ID2")
  expect_equal(ncol(x$contents), ncol(isa_s$contents) - 1)
  expect_mapequal(x$contents, y$contents)
  expect_mapequal(x$isa_stru, y$isa_stru)

  # check that adding a node works
  expect_message(x["Test Node"] <- 1:3)
  check_integrity(x)
  expect_equal(sum(x$isa_stru$is_node), sum(isa_s$isa_stru$is_node) + 1)

  expect_message(x["Test Node", "Test Property"] <- data.frame(5:7))
  check_integrity(x)
  expect_message(x["Test Node", "Test Property"] <- data.frame(5:7))
  check_integrity(x)

  # this should not work
  expect_error(x["Test Node 2", "Test Property"] <- data.frame(5:7))

  # this should
  expect_message(x["Test Node 2", "Test Property"] <- data.frame(5:7, 9:11))
  expect_equal(x[ "Test Node 2" ], 5:7)
  expect_equal(x[ "Test Node 2", "Test Property" ], 9:11)
  check_integrity(x)

  expect_message(x["Test Node 3", c("Test Property", "Test Property 2") ] <- data.frame(5:7, 9:11, NA))
  check_integrity(x)
  expect_equal(x[ "Test Node 3", "Test Property 2" ], rep(NA, 3))

  

  x <- isa_a
  expect_error(x[ "Extract Name" ] <- 1:3)
  x[ "Test Node", "Test Property" ] <- data.frame(1:3, 4:6)
  x[ "Test Node" ] <- NULL
  expect_equal(as.matrix(x$contents), as.matrix(isa_a$contents))
  expect_mapequal(x$isa_stru, isa_a$isa_stru)


  x[ "Extract Name", new=TRUE ] <- 1:3
  check_integrity(x)
  expect_equal(sum(x$isa_stru$is_node & x$isa_stru$node_name == "Extract Name"), 3)

})
