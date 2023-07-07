library(data.table)
library(mlr3)
library(mlr3viz)
library(AzureStor)
library(ggplot2)



# azure creds
# blob_key = Sys.getenv("KEY")
# endpoint = Sys.getenv("ENDPOINT")
# BLOBENDPOINT = storage_endpoint(endpoint, key=blob_key)
mlr3_save_path = "F:/H2"

# benchmark files
files_ = list.files(mlr3_save_path, full.names = TRUE)
files_info = file.info(files_)
files_info = files_info[order(files_info$ctime), ]

# import all benchmarks
bmrs = readRDS(rownames(files_info)[1])
Reduce(function(x) bmrs$combine(x), lapply(rownames(files_info)[2:3], readRDS))
bmrs = as.data.table(bmrs)

bmrs <- Reduce(function(x, y) x$combine(y), lapply(rownames(files_info)[1:3], readRDS))

# vector of id coluimns
id_cols = c("symbol", "date", "yearmonthid", "..row_id")

# get backends
backs = lapply(as.data.table(bmrs)$task, function(task) {
  cbind(task$backend$data(cols = c(id_cols, "eps_diff", "nincr", "nincr_2y", "nincr_3y"),
                    rows = task$row_ids),
        task_name = task$id)
})
lapply(backs, setnames, "..row_id", "row_ids")
lapply(backs, function(dt) dt[, yearmonthid := as.Date(yearmonthid, origin = "1970-01-01")])

# get predictions
predictions = lapply(bmrs$prediction, function(x) as.data.table(x))
names(predictions) <- task_names

# merge backs and predictions
predictions <- lapply(seq_along(predictions), function(i) {
  y = backs[[i]][predictions[[i]], on = "row_ids"]
  # y[, date := as.Date(date, origin = "1970-01-01")]
  cbind(task_name = names(backs)[i], y)
})
predictions = rbindlist(predictions)
setorder(predictions, task_name, yearmonthid)

# aggregated results
aggregate = bmrs$aggregate(msrs(c("regr.mse", "regr.mae")))
months = 

