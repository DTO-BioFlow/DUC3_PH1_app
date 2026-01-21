library(arrow)
library(dplyr)


# 1. Connect to MinIO (S3-compatible)
fs <- S3FileSystem$create(
  anonymous = TRUE,
  scheme = "https",
  endpoint_override = "minio.dive.edito.eu"
)

# 2. Specify the Parquet file path (omit s3://)
parquet_path <- "oidc-willemboone/PLET/assessment_data.parquet"

# 3. Read the single Parquet file directly
assessment <- read_parquet(
  fs$path(parquet_path)
)

# 4. Filter with dplyr
my_selection <- assessment |> 
  filter(region_id == 'SNS') |> 
  collect()  # collect() works if assessment is a Dataset, for Arrow Table it's already in memory, so collect() is optional

# 5. Inspect the result
print(my_selection)
