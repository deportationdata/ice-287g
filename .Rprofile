# R arrow bundles its own libarrow and sf's GDAL links a second; one system allocator for
# both copies keeps them from freeing each other's memory, which corrupts a GDAL Parquet write.
Sys.setenv(ARROW_DEFAULT_MEMORY_POOL = "system")
source("renv/activate.R")
