

eval_conditions <- function(value1, condition, value2){
  do.call(paste0("eval_condition_", condition),
          list(value1, value2))
}

eval_condition_equals <- function(x, y) x == y
eval_condition_not_equals <- function(x, y) x != y
eval_condition_is_any_of <- function(x, y) x %in% y
eval_condition_is_none_of <- function(x, y) !x %in% y
eval_condition_has <- function(x, y) !is.null(x) && y %in% x
eval_condition_contains  <- function(x, y) grepl(y, x)
eval_condition_any_contains  <- function(x, y) any(grepl(y, x))
eval_condition_contained_in  <- function(x, y) grepl(x, y)
eval_condition_contained_in_all_of  <- function(x, y) all(grepl(x, y))
eval_condition_contained_in_any_of  <- function(x, y) any(grepl(x, y))
eval_condition_contained_in_none_of  <- function(x, y) !all(grepl(x, y))
eval_condition_does_not_contain <- function(x, y) grepl(y, x)
eval_condition_in <- function(x, y) x %in% y
eval_condition_not_in  <- function(x, y) !x %in% y
eval_condition_in_range <- function(x, y) x <= max(y) && x >= min(y)
eval_condition_is_between <- function(x, y) x < max(y) && x > min(y)
eval_condition_less_than <- function(x, y) x < y
eval_condition_is_within <- function(x, y) x <= max(y) && x >= min(y)
eval_condition_is_before <- function(x, y) x < y
eval_condition_is_after <- function(x, y) x > y
eval_condition_less_than <- function(x, y) x < y
eval_condition_greater_than <- function(x, y) x > y

eval_condition_is_empty <- function(x,y) is.empty(x)
eval_condition_is_not_empty <- function(x, y) !is.empty(x)



