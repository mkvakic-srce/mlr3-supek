library(AzureStor)

# downlaod data from Azure blob
blob_key = readLines('./blob_key.txt')
endpoint = "https://snpmarketdata.blob.core.windows.net/"
BLOBENDPOINT = storage_endpoint(endpoint, key=blob_key)
cont = storage_container(BLOBENDPOINT, "jphd")
<<<<<<< HEAD
# storage_download(cont, "pead-predictors-sample.csv", overwrite=TRUE) # uncomment this for sample
system.time({storage_download(cont, "pead-predictors.csv", overwrite=TRUE)}) # uncomment this for full dataset
=======
storage_download(cont, "pead-predictors-sample.csv", overwrite=TRUE)
>>>>>>> 8423c21c819d823256d8b8e1c170b64f73a5ce42
