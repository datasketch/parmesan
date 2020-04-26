library(usethis)
library(testthat)

# Create test files and directories
use_directory("parmesan/tests/")

# run a test file
test_file("tests.R")

# run all test files in a directory (filenames must start with "test-", otherwise it won't work)
test_dir("parmesan/tests/")
