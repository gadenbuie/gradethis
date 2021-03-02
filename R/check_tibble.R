check_tibble <- function(
  x,
  reference = NULL,
  ...,
  classes = NULL,
  n_rows = NULL,
  n_cols = NULL,
  col_names = NULL,
  col_types = NULL,
  hint = getOption("gradethis.hint", FALSE),
  x_name = "your table",
  intro = "That wasn't quite what I expected. {random_encouragement()}\n\n",
  template = NULL
) {
  if (isTRUE(hint)) {
    env <- rlang::env_clone(rlang::current_env(), parent = parent.frame())
    return(
      give_code_feedback(
        check_tibble(
          x = x,
          reference = reference,
          ...,
          classes = classes,
          n_rows = n_rows,
          n_cols = n_cols,
          col_names = col_names,
          col_types = col_types,
          hint = FALSE,
          x_name = x_name,
          intro = intro,
          template = template
        ),
        before = "\n- ",
        location = "after",
        env = env
      )
    )
  }
  
  intro <- glue::glue(intro, .trim = FALSE)
  template <- template %||% check_tibble_template
  
  if (!is.null(reference)) {
    pass_if_equal(x = x, y = reference, message = template[["pass equals reference"]])
    `%||%` <- purrr::`%||%`
    
    classes <- classes %||% class(reference)
    n_rows <- n_rows %||% nrow(reference)
    n_cols <- n_cols %||% ncol(reference)
    col_names <- col_names %||% colnames(reference)
    col_types <- col_types %||% vapply(reference, class, character(1))
  }
  
  for (s3_class in classes) {
    if (!inherits(x, s3_class)) {
      fail(template[["fail not data frame"]])
    }
  }
  
  if (!is.null(col_names)) {
    if (!is.null(n_cols) && length(col_names) != n_cols) {
      graded(logical(), feedback_grading_problem()$message)
    }
    if (!base::setequal(names(x), col_names)) {
      extra <- setdiff(names(x), col_names)
      missing <- setdiff(col_names, names(x))
      extra_cols <- if (!length(extra)) "" else {
        n <- length(extra)
        glue::glue(
          template[["extra columns"]], 
          cols = glue::glue_collapse(glue::glue("`{extra}`"), last = ", or ")
        )
      }
      missing_cols <- if (!length(missing)) "" else {
        n <- length(missing)
        glue::glue(
          template[["missing columns"]], 
          cols = glue::glue_collapse(glue::glue("`{missing}`"), sep =", ", last = ", and "),
          .trim = FALSE
        )
      }
      fail(template[["fail col_names"]])
    }
  }
  
  if (!is.null(n_cols) && ncol(x) != n_cols) {
    n <- n_cols
    fail(template[["fail n_cols"]])
  }
  
  
  if (!is.null(n_rows) && nrow(x) != n_rows) {
    n <- n_rows
    fail(template[["fail n_rows"]])
  }
  
  if (!is.null(col_types)) {
    if (!is.null(n_cols) && length(col_types) != n_cols) {
      graded(logical(), feedback_grading_problem()$message, type = "warning")
    }
    x_types <- vapply(x, class, character(1))
    if (is.null(col_names)) {
      col_names <- seq_along(col_types)
    }
    
    x_types <- x_types[col_names]
    not_same <- col_types[x_types != col_types]
    names(not_same) <- names(x_types)[x_types != col_types]
    
    if (length(not_same)) {
      wrong <- purrr::map2_chr(names(not_same), not_same, template[["col_type mismatch"]])
      
      wrong <- glue::glue_collapse(wrong, last = ", and ")
      fail(template[["fail col_types"]])
    }
  }
}

check_tibble_template <- list(
  "fail not data frame" = "{intro}- I expected {x_name} to be a `{s3_class}`.",
  "fail col_names"      = "{intro}{missing_cols}{extra_cols}",
  "extra columns"       = "- I didn't expect {x_name} to have the column{if (n != 1) 's' else ''} {cols}.",
  "missing columns"     = "- I expected {x_name} to also have the column{if (n != 1) 's' else ''} {cols}.\n",
  "fail n_cols"         = "{intro}- I expected a table with {n_cols} column{if (n != 1) 's' else ''}, but {x_name} has **{ncol(x)}**.",
  "fail n_rows"         = "{intro}- I expected a table with {n_rows} row{if (n != 1) 's' else ''}, but {x_name} has **{nrow(x)}**.",
  "fail col_types"      = "{intro}- In {x_name}, I expected:{wrong}",
  "col_type mismatch"   = function(name, type) {
    # this function is applied to each column `name` which should have `type`
    pred <- if (substr(type, 1, 1) %in% c("a", "e", "i", "o", "u")) "an" else "a"
    as.character(glue::glue("\n    - `{name}` to be {pred} {type}", .trim = FALSE))
  }
)
