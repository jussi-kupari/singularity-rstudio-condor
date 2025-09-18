#!/bin/bash

# Get the directory where this script is located (project root)
PROJECT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Create R_libs directory if it doesn't exist
mkdir -p "${PROJECT_DIR}/R_libs"

# Launch container
singularity exec \
        --bind "${PROJECT_DIR}:/project" \
            --bind "${PROJECT_DIR}/R_libs:/project/R_libs" \
                "${PROJECT_DIR}"/r-seurat.sif \
                    R "$@"
