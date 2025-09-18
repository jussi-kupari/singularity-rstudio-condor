#!/bin/bash

# Configuration
PROJECT_DIR=$(pwd)
SINGULARITY_NODE="monod10.mbb.ki.se"
PORT=$(shuf -i 8000-9000 -n 1)

echo "=== RStudio Server Launcher ==="
echo "Compute node: $(hostname)"
echo "Singularity node: ${SINGULARITY_NODE}"
echo "Project directory: ${PROJECT_DIR}"
echo "Port: ${PORT}"

# Create directories
mkdir -p R_libs

# Test SSH connection
echo "Testing SSH connection to ${SINGULARITY_NODE}..."
if ! ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no ${SINGULARITY_NODE} "echo 'SSH connection successful'"; then
    echo "ERROR: Cannot SSH to ${SINGULARITY_NODE}"
    exit 1
fi

# Test if Singularity is available on target node
echo "Testing Singularity on ${SINGULARITY_NODE}..."
if ! ssh -o StrictHostKeyChecking=no ${SINGULARITY_NODE} "command -v singularity >/dev/null 2>&1"; then
    echo "ERROR: Singularity not found on ${SINGULARITY_NODE}"
    exit 1
fi

# Test if container exists
echo "Testing container access..."
if ! ssh -o StrictHostKeyChecking=no ${SINGULARITY_NODE} "test -f '${PROJECT_DIR}/r-seurat.sif'"; then
    echo "ERROR: Container not found at ${PROJECT_DIR}/r-seurat.sif on ${SINGULARITY_NODE}"
    exit 1
fi

# Write connection info
cat > connection_info.txt << INFO_EOF
RStudio Server Connection Information
====================================
Job started: $(date)
Compute node: $(hostname)
Singularity node: ${SINGULARITY_NODE}
Port: ${PORT}
Username: $(whoami)

To connect:
1. SSH tunnel: ssh -N -L ${PORT}:${SINGULARITY_NODE}:${PORT} $(whoami)@monod.mbb.ki.se
2. Browser: http://localhost:${PORT}
3. Login with your regular username/password

Job will keep running until manually stopped.
====================================
INFO_EOF

echo "All tests passed. Starting RStudio Server..."
echo "Connection details written to: ${PROJECT_DIR}/connection_info.txt"

# SSH to Singularity node and run RStudio Server
ssh -o StrictHostKeyChecking=no ${SINGULARITY_NODE} "
    echo 'Connected to ${SINGULARITY_NODE}';
    cd '${PROJECT_DIR}' || exit 1;
    mkdir -p R_libs;
    echo 'Starting RStudio Server...';
    singularity exec \
        --scratch /run,/var/lib/rstudio-server \
        --workdir \$(mktemp -d) \
        --bind '${PROJECT_DIR}:/project' \
        --bind '${PROJECT_DIR}/R_libs:/project/R_libs' \
        r-seurat.sif \
        rserver --www-address=0.0.0.0 --www-port=${PORT} --server-user=\$(whoami) --auth-timeout-minutes=0 --auth-stay-signed-in-days=30
"

echo "RStudio Server session ended at $(date)"
