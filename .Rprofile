# .Rprofile
cat("Loading project .Rprofile...\n")

# Ensure project R library is first in path
.libPaths(c("/project/R_libs", .libPaths()))

# Set working directory to project
if (interactive()) {
    setwd("/project")
    cat("Working directory set to /project\n")
    cat("Library paths (in priority order):\n")

    # Replace for loop with sapply
    invisible(sapply(seq_along(.libPaths()), function(x) {
        cat(paste0("  ", x, ". ", .libPaths()[x], "\n"))
    }))
}

cat("Ready to work! Package installation will default to /project/R_libs\n")
