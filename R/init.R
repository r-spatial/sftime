#' @import sf
NULL

# from: https://github.com/cran/sf/blob/master/R/tidyverse.R:
# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
# Thu Apr 19 10:53:24 CEST 2018
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

register_all_s3_methods <- function() {
  
  # tidyverse joins
  register_s3_method("dplyr", "inner_join", "sftime")
  register_s3_method("dplyr", "left_join", "sftime")
  register_s3_method("dplyr", "right_join", "sftime")
  register_s3_method("dplyr", "full_join", "sftime")
  register_s3_method("dplyr", "semi_join", "sftime")
  register_s3_method("dplyr", "anti_join", "sftime")
  
  register_s3_method("dplyr", "filter", "sftime")
  register_s3_method("dplyr", "arrange", "sftime")
  register_s3_method("dplyr", "distinct", "sftime")
  register_s3_method("dplyr", "group_by", "sftime")
  register_s3_method("dplyr", "mutate", "sftime")
  register_s3_method("dplyr", "dplyr_reconstruct", "sftime")
  register_s3_method("dplyr", "rename", "sftime")
  register_s3_method("dplyr", "rowwise", "sftime")
  register_s3_method("dplyr", "sample_frac", "sftime")
  register_s3_method("dplyr", "sample_n", "sftime")
  register_s3_method("dplyr", "select", "sftime")
  register_s3_method("dplyr", "slice", "sftime")
  register_s3_method("dplyr", "summarise", "sftime")
  register_s3_method("dplyr", "summarize", "sftime")
  register_s3_method("dplyr", "transmute", "sftime")
  register_s3_method("dplyr", "ungroup", "sftime")
  register_s3_method("tidyr", "gather", "sftime")
  register_s3_method("tidyr", "pivot_longer", "sftime")
  register_s3_method("tidyr", "spread", "sftime")
  register_s3_method("tidyr", "nest", "sftime")
  register_s3_method("tidyr", "separate", "sftime")
  register_s3_method("tidyr", "separate_rows", "sftime")
  register_s3_method("tidyr", "unite", "sftime")
  register_s3_method("tidyr", "unnest", "sftime")
  register_s3_method("tidyr", "drop_na", "sftime")
  
}

.onLoad <- function(libname, pkgname) {
  
  register_all_s3_methods()
  
}