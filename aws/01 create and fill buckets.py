# Purpose:
# Create S3 Buckets if not already existing and drop files required for
# app run inside them.

# Packages:
import boto3
import os

# Set file path up one folder from script path:
scriptPath =  os.path.dirname(os.path.realpath(__file__))
os.chdir(scriptPath)
os.chdir("..")

# Assert the directory was changed to the correct folder by
# identifying associated folders
assert os.path.isdir("downstream"), "downstream folder not located"

# Create Instance
s3 = boto3.client('s3')

# Create Variables
bucketName  = "cfbapp23"
pathToFiles = os.getcwd()

# Create bucket if not already initialized----
# Print buckets
activeBuckets = s3.list_buckets()
for bucket in activeBuckets['Buckets']:
    print(f"found {bucket}")

# Check if named bucket is in the active bucket list
bucketExists = any(bucket['Name'] == bucketName for bucket in activeBuckets['Buckets'])

# Create bucket if logic passes
if not bucketExists:
    try:
        s3.create_bucket(Bucket = bucketName)
        print(f"Bucket '{bucketName}' successfully created.")
    except Exception as e:
        print(e)
        print(f"Creation failed.")
else:
    print(f"{bucketName} already hosted on AWS")

# Place files in Bucket after changing directory
os.chdir("downstream")
downstreamFiles = os.listdir(os.getcwd())

for file in downstreamFiles:
    s3.upload_file(os.path.join(os.getcwd(), file), bucketName, file)
    print(f"{file} uploaded to {bucketName}")

