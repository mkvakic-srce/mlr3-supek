library(AzureStor)

blob_key = readLines('./blob_key.txt')
endpoint = "https://snpmarketdata.blob.core.windows.net/"
BLOBENDPOINT = storage_endpoint(endpoint, key=blob_key)
cont = storage_container(BLOBENDPOINT, "jphd")
storage_download(cont, "pead-predictors-sample.csv", overwrite=T)
