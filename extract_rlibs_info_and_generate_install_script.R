extract_package_info <- function(lib_path = "R_libs",
                                 rhistory_path = ".Rhistory") {
  # Read .Rhistory if available
  history_lines <- if (file.exists(rhistory_path))
    readLines(rhistory_path, warn = FALSE)
  else
    character(0)
  
  # Define install command patterns
  install_patterns <- c(
    "install\\.packages\\(",
    "BiocManager::install\\(",
    "biocLite\\(",
    "devtools::install_github\\(",
    "remotes::install_github\\(",
    "remotes::install_cran\\(",
    "remotes::install_bioc\\(",
    "remotes::install_gitlab\\(",
    "remotes::install_bitbucket\\("
  )
  combined_pattern <- paste(install_patterns, collapse = "|")
  install_commands <- grep(combined_pattern, history_lines, value = TRUE)
  
  # Get all package directories
  pkg_dirs <- list.dirs(lib_path, full.names = TRUE, recursive = FALSE)
  
  manually_installed <- list()
  dependencies <- list()
  
  for (pkg_dir in pkg_dirs) {
    desc_file <- file.path(pkg_dir, "DESCRIPTION")
    pkg_name <- basename(pkg_dir)
    
    if (file.exists(desc_file)) {
      desc <- read.dcf(desc_file)
      metadata <- as.list(desc[1, ])
      
      # Try to find matching install command from history
      matching_cmd <- grep(paste0(pkg_name, "\"?\\)?"), install_commands, value = TRUE)
      metadata$InstallCommandUsed <- if (length(matching_cmd) > 0)
        matching_cmd[1]
      else
        NA
      
      # Reconstruct install command
      version <- metadata$Version
      repo <- metadata$Repository
      remote_type <- metadata$RemoteType
      remote_repo <- metadata$RemoteRepo
      remote_user <- metadata$RemoteUsername
      remote_ref <- metadata$RemoteRef
      
      if (!is.null(remote_type) && !is.na(remote_type) &&
          remote_type == "github" &&
          !is.null(remote_repo) && !is.na(remote_repo) &&
          !is.null(remote_user) && !is.na(remote_user)) {
        ref <- if (!is.null(remote_ref) &&
                   !is.na(remote_ref))
          remote_ref
        else
          "HEAD"
        metadata$ReproduceInstall <- paste0("remotes::install_github(\"",
                                            remote_user,
                                            "/",
                                            remote_repo,
                                            "@",
                                            ref,
                                            "\")")
        
      } else if (!is.null(repo) &&
                 !is.na(repo) && grepl("BioC", repo)) {
        metadata$ReproduceInstall <- paste0("BiocManager::install(\"",
                                            pkg_name,
                                            "\") # version: ",
                                            version)
        
      } else if (!is.null(repo) && !is.na(repo) && repo == "CRAN") {
        metadata$ReproduceInstall <- paste0("remotes::install_version(\"",
                                            pkg_name,
                                            "\", version = \"",
                                            version,
                                            "\")")
        
      } else {
        # Fallback: assume CRAN if version is present
        if (!is.null(version) && !is.na(version)) {
          metadata$ReproduceInstall <- paste0("remotes::install_version(\"",
                                              pkg_name,
                                              "\", version = \"",
                                              version,
                                              "\")")
        } else {
          metadata$ReproduceInstall <- paste0("# Unknown source for ", pkg_name)
        }
      }
      
      # Classify
      if (!is.na(metadata$InstallCommandUsed)) {
        manually_installed[[pkg_name]] <- metadata
      } else {
        dependencies[[pkg_name]] <- metadata
      }
    }
  }
  
  return(list(manually_installed = manually_installed, dependencies = dependencies))
}


generate_install_script <- function(package_info,
                                    use_reproduce = FALSE,
                                    include_dependencies = FALSE,
                                    wrap_in_r_command = FALSE,
                                    output_file = "install_local_packages.R",
                                    add_shebang = TRUE) {
  get_command <- function(pkg_data) {
    if (use_reproduce && !is.null(pkg_data$ReproduceInstall)) {
      return(pkg_data$ReproduceInstall)
    } else if (!is.null(pkg_data$InstallCommandUsed)) {
      return(pkg_data$InstallCommandUsed)
    } else {
      return(NULL)
    }
  }
  
  manual_cmds <- lapply(package_info$manually_installed, get_command)
  manual_cmds <- Filter(Negate(is.null), manual_cmds)
  
  dep_cmds <- character(0)
  if (include_dependencies) {
    dep_cmds <- lapply(package_info$dependencies, function(pkg_data) {
      if (!is.null(pkg_data$ReproduceInstall)) {
        return(pkg_data$ReproduceInstall)
      } else {
        return(NULL)
      }
    })
    dep_cmds <- Filter(Negate(is.null), dep_cmds)
  }
  
  all_cmds <- unique(c(manual_cmds, dep_cmds))
  
  if (wrap_in_r_command) {
    all_cmds <- vapply(all_cmds, function(cmd) {
      paste0('R -e "', cmd, '"')
    }, character(1))
  }
  
  script_lines <- all_cmds
  if (add_shebang && !wrap_in_r_command) {
    script_lines <- c("#!/usr/bin/env Rscript", "", script_lines)
  }
  
  script <- paste(script_lines, collapse = "\n")
  
  if (!is.null(output_file)) {
    writeLines(script, output_file)
  }
  
  return(script)
}
