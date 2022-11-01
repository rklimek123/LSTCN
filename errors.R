library(types)

gen_error <- function(function_name = ? character,
                      object_class = ? character) {
  paste("Generic function",
        function_name,
        "not implemented for classes:",
        object_class)
}

error_msg <- function(message = ? character) {
  simpleCondition(message)
}
