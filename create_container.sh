#!/bin/bash

# Usage function
usage() {
    echo "Usage: $0 <definition_file.def> [output_container.sif]"
    echo ""
    echo "Creates a Singularity container from a definition file using --fakeroot"
    echo ""
    echo "Examples:"
    echo "  $0 r-seurat.def                      # Creates r-seurat.sif"
    echo "  $0 r-seurat.def my_container.sif    # Creates my_container.sif"
    echo ""
    echo "Build output will be saved to: singularity_build.out"
    exit 1
}

# Check if definition file argument is provided
if [ $# -eq 0 ]; then
    usage
fi

DEFINITION_FILE="$1"

# Check if definition file exists
if [ ! -f "$DEFINITION_FILE" ]; then
    echo "ERROR: Definition file '$DEFINITION_FILE' not found!"
    exit 1
fi

# Determine output container name
if [ $# -eq 2 ]; then
    CONTAINER_NAME="$2"
else
    BASENAME=$(basename "$DEFINITION_FILE" .def)
    CONTAINER_NAME="${BASENAME}.sif"
fi

DEFINITION_FILE_ABS=$(realpath "$DEFINITION_FILE")
CONTAINER_NAME_ABS=$(realpath "$CONTAINER_NAME")

echo "=== Singularity Container Builder (R + Seurat) ==="
echo "Definition file: $DEFINITION_FILE_ABS"
echo "Output container: $CONTAINER_NAME_ABS"
echo "Build log: $(pwd)/singularity_build.out"
echo ""

# Check if output container already exists
if [ -f "$CONTAINER_NAME" ]; then
    echo "WARNING: Container '$CONTAINER_NAME' already exists!"
    echo -n "Do you want to overwrite it? [y/N]: "
    read -r response
    if [[ ! "$response" =~ ^[Yy]$ ]]; then
        echo "Build cancelled."
        exit 0
    fi
    echo "Removing existing container..."
    rm -f "$CONTAINER_NAME"
fi
# Check if singularity is available
if ! command -v singularity >/dev/null 2>&1; then
    echo "ERROR: Singularity not found in PATH!"
    exit 1
fi

SINGULARITY_VERSION=$(singularity version 2>/dev/null || singularity --version 2>/dev/null)
echo "Singularity version: $SINGULARITY_VERSION"
echo ""

# Validate definition file format
echo "Validating definition file..."
if ! grep -q "^Bootstrap:" "$DEFINITION_FILE"; then
    echo "ERROR: Definition file appears to be invalid (missing Bootstrap line)"
    exit 1
fi

echo "Definition file looks valid."
echo ""

# Start the build
echo "=== Starting Container Build ==="
BUILD_START=$(date)
BUILD_START_TIMESTAMP=$(date +%s)

echo "Running: singularity build --fakeroot $CONTAINER_NAME $DEFINITION_FILE"
echo ""

if singularity build --fakeroot "$CONTAINER_NAME" "$DEFINITION_FILE" 2>&1 | tee singularity_build.out; then
    BUILD_END=$(date)
    BUILD_END_TIMESTAMP=$(date +%s)
    BUILD_DURATION=$((BUILD_END_TIMESTAMP - BUILD_START_TIMESTAMP))

    echo ""
    echo "=== Build Completed Successfully! ==="
    echo "Container: $CONTAINER_NAME_ABS"
    echo "Size: $(du -h "$CONTAINER_NAME" | cut -f1)"
    echo "Started: $BUILD_START"
    echo "Finished: $BUILD_END"
    echo "Duration: $(printf '%02d:%02d:%02d' $((BUILD_DURATION/3600)) $((BUILD_DURATION%3600/60)) $((BUILD_DURATION%60)))"
    echo ""

    # Test the container
    echo "=== Testing Container ==="
    echo "Testing basic container execution..."
    if singularity exec "$CONTAINER_NAME" echo "Container test: OK"; then
        echo "✓ Container execution test passed"
    else
        echo "✗ Container execution test failed"
    fi

    echo "Testing R installation..."
    if singularity exec "$CONTAINER_NAME" R --version >/dev/null 2>&1; then
        echo "✓ R is available"
    else
        echo "✗ R not available"
    fi

    echo "Testing Seurat installation..."
    if singularity exec "$CONTAINER_NAME" Rscript -e "library(Seurat)" >/dev/null 2>&1; then
        echo "✓ Seurat loaded successfully"
    else
        echo "✗ Seurat failed to load"
    fi

else
    BUILD_END=$(date)
    echo ""
    echo "=== Build Failed! ==="
    echo "Started: $BUILD_START"
    echo "Failed: $BUILD_END"
    echo ""
    echo "Check the build log for errors:"
    echo "  cat singularity_build.out"
    exit 1
fi
