#' Fix errors introduced in package creation by forgetting to qualify
#' namespaces.
#'
#' This is a code linting function and expected to be called at the console
#' during package development. It will scan the files in the current project and
#' replace unqualified references to e.g. `mutate` with ones to `dplyr::mutate`
#' etc.
#'
#' @param rDirectories the locations of the R code to fix (by default R scripts,
#'   vignettes, and tests)
#' @param description the location of the description file
#' @param dry_run by default this function will not actually do anything unless
#'   this is set to FALSE. However the dry run output can be manually compared
#'   with a diff tool to interactively accept changes.
#'
#' @return nothing. called for side effects.
#' @export
fix_unqualified_fns = function(rDirectories = c(here::here("R"),here::here("vignettes"),here::here("tests/testthat")), description = here::here("DESCRIPTION"), dry_run = TRUE ) {
  
  devtools::load_all(rDirectories[[1]])
  matches = path = package = function_name = f = generic = content.old = name = value = content = changed = NULL
  
  files = dplyr::bind_rows(lapply(rDirectories, fs::dir_info)) %>% dplyr::filter(fs::path_ext(path) %in% c("R","Rmd"))
  dMap = yaml::read_yaml(description)
  imports = dMap$Imports %>% stringr::str_split(",\\s+") %>% `[[`(1) %>%
    stringr::str_extract("^[a-zA-Z0-9\\.]*")
  loaded = (.packages())
  packages = unique(c(imports,loaded))
  packages = packages[!packages %in% c(dMap$Package,"base")]
  files = files %>% dplyr::mutate(content.old = purrr::map(path, ~ readr::read_lines(.x)))
  packageMap = tibble::tibble(package = c("base",packages)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(function_name = list(ls(envir = asNamespace(package)))) %>%
    tidyr::unnest(function_name) %>%
    dplyr::mutate( f = purrr::map2(function_name, package, function(f,p) {
      tryCatch(utils::getFromNamespace(f,p), error = function(e) {function(){}})
    })) %>%
    dplyr::mutate( generic = .isGeneric(f)) %>%
    dplyr::select(-f)
  
  # functions from base or this package
  theseFunctions = c(ls(envir = asNamespace(dMap$Package)),ls(asNamespace("base")))
  
  packageMap2 = packageMap %>% dplyr::group_by(function_name) %>%
    dplyr::mutate(package = ordered(package,levels = c("base",packages))) %>%
    dplyr::arrange(dplyr::desc(generic),package) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::filter(package != "base") %>%
    dplyr::filter(!function_name %in% theseFunctions) %>%
    dplyr::filter(stringr::str_detect(function_name,"[a-zA-Z]")) %>%
    dplyr::filter(!is.na(function_name))
  
  files = files %>% dplyr::mutate(content = content.old, matches=list(tibble::tibble()))
  
  for (pkg in packages) {
    # pkg = "dplyr"
    functions = packageMap2 %>% dplyr::filter(package == pkg) %>% dplyr::pull(function_name)
    
    if (length(functions) > 0) {
      functionRegex = paste0(lapply(functions, .escape),collapse = "|")
      # TODO: does not match function as first position on start of line
      functionRegex = paste0("(^|[^:a-zA-Z0-9\\._])(",functionRegex,")\\(")
      replacement = paste0("\\1",pkg,"::\\2(")
      # c = files$content.old[[1]]
      for (i in 1:nrow(files)) {
        file = files %>% purrr::map(~ .x[[i]])
        files$content[[i]] = file$content %>% stringr::str_replace_all(functionRegex, replacement)
        tmp = stringr::str_match_all(file$content,functionRegex) %>% purrr::map(~ .x[,3]) %>%
          unlist()
        if (is.null(tmp)) tmp = character()
        tmp = tibble::tibble(name=tmp) %>% dplyr::group_by(name) %>%
          dplyr::summarise(value = dplyr::n()) %>% dplyr::mutate(pkg =pkg)
        files$matches[[i]] = file$matches %>% dplyr::bind_rows(tmp)
      }
    }
  }
  
  files = files %>% dplyr::mutate(changed = purrr::map2_lgl(content.old, content, ~ any(.x!=.y)))
  tmp = files %>% dplyr::select(path,matches) %>% tidyr::unnest(matches)
  
  if(any(files$changed)) {
    message(sum(tmp$value)," function calls missing namespaces found: ", paste0(unique(tmp$pkg), collapse = "; "))
    files %>% dplyr::filter(changed) %>% purrr::pwalk(function(path,content.old,...) message(path))
    
    if(dry_run) {
      fixme = 3
    } else {
      fixme = utils::menu(c("Yes","No","Dry-run"), "Would you like me to fix these?")
    }
    dry_run = fixme==3
    
    if(fixme %in% c(1,3)) {
      
      if (dry_run) message("dry run: this is what would have been done")
      message("backing originals up to:")
      files %>% dplyr::filter(changed) %>% purrr::pwalk(function(path,content.old,...) message("\t",path,".old"))
      if (!dry_run) files %>% dplyr::filter(changed) %>% purrr::pwalk(function(path,content.old,...) .write_safe(content.old,paste0(path,".old")))
      
      message("writing modified files to: ")
      files %>% dplyr::filter(changed) %>% purrr::pwalk(function(path,content,...) message("\t",path))
      if (!dry_run) files %>% dplyr::filter(changed) %>% purrr::pwalk(function(path,content,...) readr::write_lines(content,path))
      if (dry_run) files %>% dplyr::filter(changed) %>% purrr::pwalk(function(path,content,...) readr::write_lines(content,paste0(path,".dry_run")))
      
    }
    
  }
  
  
  nsMissing = tmp %>% dplyr::filter(!(pkg %in% imports)) %>% dplyr::pull(pkg) %>% unique()
  if(length(nsMissing) > 0) {
    message("Your DESCRIPTION file is missing packages that are currently loaded and used in your code. These are: ",paste(nsMissing,collapse = "; "))
    fixns = utils::menu(c("Yes","No"), title = "Would you like me to fix these?")
    if (fixns==1) {
      x = lapply(nsMissing, function(p) usethis::use_package(p))
    }
  }
  
  # TODO: format a diff output
  # tmp = diffobj::diffChr(files$content[[6]],files$content.old[[6]])
  
  message("Done. You may want to run some tests before deleting the backup files.")
  return(files)
}

## private helper functions ----

# from rex:::escape.character
# escape a regex
.escape = function (x) {
  chars <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[",
             "]", "{", "}", "\\")
  .sanitize(x, chars)
}

.sanitize = function (x, chars) {
  gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"),
       "\\\\\\1", x, perl = TRUE)
}

# move a file ensuring a backup exists
.move_safe = function(file, new_file = paste0(file,".old")) {
  if (!fs::file_exists(new_file)) {
    fs::file_move(file, new_file)
  } else {
    .move_safe(file = new_file)
    fs::file_move(file, new_file)
  }
}

# write a file ensuring a backup exists
.write_safe = function(x, file) {
  if (fs::file_exists(file)) .move_safe(file)
  readr::write_lines(x,file)
}

.isGeneric = function(listOfFunctions) {
  unname(sapply(listOfFunctions, function(x) tryCatch(suppressWarnings(utils::isS3stdGeneric(x)), error = function(e) FALSE)))
}

