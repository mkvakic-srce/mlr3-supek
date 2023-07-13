library(data.table)
library(gausscov)
library(paradox)
library(mlr3)
library(mlr3pipelines)
library(mlr3viz)
library(mlr3tuning)
library(mlr3misc)
library(future)
library(future.apply)

# SETUP -------------------------------------------------------------------
# create folder in which we will save results
mlr3_save_path = paste0("./H2-jobarray-", Sys.getenv('PBS_ARRAY_ID'))
if (!dir.exists(mlr3_save_path)) {
  dir.create(mlr3_save_path)
}

# utils https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates
monnb <- function(d) {
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon }
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

# PREPARE DATA ------------------------------------------------------------
print("Prepare data")

# read predictors
data_tbl = fread("./pead-predictors.csv")

# convert tibble to data.table
DT = as.data.table(data_tbl)

# create group variable
DT[, date_rolling := as.IDate(date_rolling)]
DT[, yearmonthid := round(date_rolling, digits = "month")]
DT[, .(date, date_rolling, yearmonthid)]
DT[, yearmonthid := as.integer(yearmonthid)]
DT[, .(date, date_rolling, yearmonthid)]

# define predictors
cols_non_features <- c("symbol", "date", "time", "right_time",
                       "bmo_return", "amc_return",
                       "open", "high", "low", "close", "volume", "returns",
                       "yearmonthid", "date_rolling"
)
targets <- c(colnames(DT)[grep("ret_excess", colnames(DT))])
cols_features <- setdiff(colnames(DT), c(cols_non_features, targets))

# convert columns to numeric. This is important only if we import existing features
chr_to_num_cols <- setdiff(colnames(DT[, .SD, .SDcols = is.character]), c("symbol", "time", "right_time"))
print(chr_to_num_cols)
DT <- DT[, (chr_to_num_cols) := lapply(.SD, as.numeric), .SDcols = chr_to_num_cols]

# remove constant columns in set
features_ <- DT[, ..cols_features]
remove_cols <- colnames(features_)[apply(features_, 2, var, na.rm=TRUE) == 0]
print(paste0("Removing feature with 0 standard deviation: ", remove_cols))
cols_features <- setdiff(cols_features, remove_cols)

# convert variables with low number of unique values to factors
int_numbers = na.omit(DT[, ..cols_features])[, lapply(.SD, function(x) all(floor(x) == x))]
int_cols = colnames(DT[, ..cols_features])[as.matrix(int_numbers)[1,]]
factor_cols = DT[, ..int_cols][, lapply(.SD, function(x) length(unique(x)))]
factor_cols = as.matrix(factor_cols)[1, ]
factor_cols = factor_cols[factor_cols <= 100]
DT = DT[, (names(factor_cols)) := lapply(.SD, as.factor), .SD = names(factor_cols)]

# remove observations with missing target
# if we want to keep as much data as possible an use only one predicitn horizont
# we can skeep this step
DT = na.omit(DT, cols = setdiff(targets, colnames(DT)[grep("extreme", colnames(DT))]))

# change IDate to date, because of error
# Assertion on 'feature types' failed: Must be a subset of
# {'logical','integer','numeric','character','factor','ordered','POSIXct'},
# but has additional elements {'IDate'}.
DT[, date := as.POSIXct(date, tz = "UTC")]
# DT[, .(symbol,date, date_rolling, yearmonthid)]

# sort
# this returns error on HPC. Some problem with memory
# setorder(DT, date)
print("This was the problem")
# DT = DT[order(date)] # DOESNT WORK TOO
DT = DT[order(yearmonthid)]
print("This was the problem. Solved.")



# TASKS -------------------------------------------------------------------
print("Tasks")

# id coluns we always keep
id_cols = c("symbol", "date", "yearmonthid")

# convert date to PosixCt because it is requireed by mlr3
DT[, date := as.POSIXct(date, tz = "UTC")]

# task with future week returns as target
target_ = colnames(DT)[grep("^ret_excess_stand_5", colnames(DT))]
cols_ = c(id_cols, target_, cols_features)
task_ret_week <- as_task_regr(DT[, ..cols_],
                              id = "task_ret_week",
                              target = target_)

# task with future month returns as target
target_ = colnames(DT)[grep("^ret_excess_stand_22", colnames(DT))]
cols_ = c(id_cols, target_, cols_features)
task_ret_month <- as_task_regr(DT[, ..cols_],
                               id = "task_ret_month",
                               target = target_)

# task with future 2 months returns as target
target_ = colnames(DT)[grep("^ret_excess_stand_44", colnames(DT))]
cols_ = c(id_cols, target_, cols_features)
task_ret_month2 <- as_task_regr(DT[, ..cols_],
                                id = "task_ret_month2",
                                target = target_)

# task with future 2 months returns as target
target_ = colnames(DT)[grep("^ret_excess_stand_66", colnames(DT))]
cols_ = c(id_cols, target_, cols_features)
task_ret_quarter <- as_task_regr(DT[, ..cols_],
                                 id = "task_ret_quarter",
                                 target = target_)

# set roles for symbol, date and yearmonth_id
task_ret_week$col_roles$feature = setdiff(task_ret_week$col_roles$feature,
                                          id_cols)
task_ret_month$col_roles$feature = setdiff(task_ret_month$col_roles$feature,
                                           id_cols)
task_ret_month2$col_roles$feature = setdiff(task_ret_month2$col_roles$feature,
                                            id_cols)
task_ret_quarter$col_roles$feature = setdiff(task_ret_quarter$col_roles$feature,
                                             id_cols)



# CROSS VALIDATIONS -------------------------------------------------------
print("Cross validations")

# create train, tune and test set
nested_cv_split = function(task,
                           train_length = 60,
                           tune_length = 6,
                           test_length = 1) {

  # create cusom CV's for inner and outer sampling
  custom_inner = rsmp("custom")
  custom_outer = rsmp("custom")

  # get year month id data
  # task = task_ret_week$clone()
  task_ = task$clone()
  yearmonthid_ = task_$backend$data(cols = c("yearmonthid", "..row_id"),
                                    rows = 1:task_$nrow)
  stopifnot(all(task_$row_ids == yearmonthid_$`..row_id`))
  groups_v = yearmonthid_[, unlist(unique(yearmonthid))]

  # util vars
  start_folds = 1:(length(groups_v)-train_length-tune_length-test_length)
  get_row_ids = function(mid) unlist(yearmonthid_[yearmonthid %in% mid, 2], use.names = FALSE)

  # create train data
  train_groups <- lapply(start_folds,
                         function(x) groups_v[x:(x+train_length-1)])
  train_sets <- lapply(train_groups, get_row_ids)

  # create tune set
  tune_groups <- lapply(start_folds,
                        function(x) groups_v[(x+train_length):(x+train_length+tune_length-1)])
  tune_sets <- lapply(tune_groups, get_row_ids)

  # test train and tune
  test_1 = vapply(seq_along(train_groups), function(i) {
    mondf(
      tail(as.Date(train_groups[[i]], origin = "1970-01-01"), 1),
      head(as.Date(tune_groups[[i]], origin = "1970-01-01"), 1)
    )
  }, FUN.VALUE = numeric(1L))
  stopifnot(all(test_1 == 1))
  test_2 = vapply(seq_along(train_groups), function(i) {
    unlist(head(tune_sets[[i]], 1) - tail(train_sets[[i]], 1))
  }, FUN.VALUE = numeric(1L))
  stopifnot(all(test_2 == 1))

  # create test sets
  insample_length = train_length + tune_length
  test_groups <- lapply(start_folds,
                        function(x) groups_v[(x+insample_length):(x+insample_length+test_length-1)])
  test_sets <- lapply(test_groups, get_row_ids)

  # test tune and test
  test_3 = vapply(seq_along(train_groups), function(i) {
    mondf(
      tail(as.Date(tune_groups[[i]], origin = "1970-01-01"), 1),
      head(as.Date(test_groups[[i]], origin = "1970-01-01"), 1)
    )
  }, FUN.VALUE = numeric(1L))
  stopifnot(all(test_1 == 1))
  test_4 = vapply(seq_along(train_groups), function(i) {
    unlist(head(test_sets[[i]], 1) - tail(tune_sets[[i]], 1))
  }, FUN.VALUE = numeric(1L))
  stopifnot(all(test_2 == 1))

  # create inner and outer resamplings
  custom_inner$instantiate(task, train_sets, tune_sets)
  inner_sets = lapply(seq_along(train_groups), function(i) {
    c(train_sets[[i]], tune_sets[[i]])
  })
  custom_outer$instantiate(task, inner_sets, test_sets)
  return(list(custom_inner = custom_inner, custom_outer = custom_outer))
}
custom_cvs = nested_cv_split(task_ret_week, 36, 3, 1)
custom_inner = custom_cvs$custom_inner
custom_outer = custom_cvs$custom_outer

# test set start after train set
all(vapply(1:custom_inner$iters, function(i) {
  (tail(custom_inner$train_set(i), 1) + 1) == custom_inner$test_set(i)[1]
}, FUN.VALUE = logical(1L)))

# train set in outersample contains ids in innersample 1
all(vapply(1:custom_inner$iters, function(i) {
  all(c(custom_inner$train_set(i),
        custom_inner$test_set(i)) == custom_outer$train_set(i))
}, FUN.VALUE = logical(1L)))



# ADD PIPELINES -----------------------------------------------------------
print("Add pipelines")

# source pipes, filters and other
source("mlr3_winsorization.R")
source("mlr3_uniformization.R")
source("mlr3_gausscov_f1st.R")
source("mlr3_gausscov_f3st.R")
source("mlr3_dropna.R")
source("mlr3_dropnacol.R")
source("mlr3_filter_drop_corr.R")
source("mlr3_winsorizationsimple.R")
source("mlr3_winsorizationsimplegroup.R")
source("PipeOpPCAExplained.R")
# measures
source("Linex.R")
source("AdjLoss2.R")

# add my pipes to mlr dictionary
mlr_pipeops$add("uniformization", PipeOpUniform)
mlr_pipeops$add("winsorize", PipeOpWinsorize)
mlr_pipeops$add("winsorizesimple", PipeOpWinsorizeSimple)
mlr_pipeops$add("winsorizesimplegroup", PipeOpWinsorizeSimpleGroup)
mlr_pipeops$add("dropna", PipeOpDropNA)
mlr_pipeops$add("dropnacol", PipeOpDropNACol)
mlr_pipeops$add("dropcorr", PipeOpDropCorr)
mlr_pipeops$add("pca_explained", PipeOpPCAExplained)
mlr_filters$add("gausscov_f1st", FilterGausscovF1st)
mlr_filters$add("gausscov_f3st", FilterGausscovF3st)
mlr_measures$add("linex", Linex)
mlr_measures$add("adjloss2", AdjLoss2)



# GRAPH V2 ----------------------------------------------------------------
print("Create graph")

# non pca ghraph
graph_nonpca = po("dropnacol", id = "dropnacol", cutoff = 0.05) %>>%
  po("dropna", id = "dropna") %>>%
  po("removeconstants", id = "removeconstants_1", ratio = 0)  %>>%
  po("winsorizesimple", id = "winsorizesimple", probs_low = 0.01, probs_high = 0.99, na.rm = TRUE) %>>%
  po("removeconstants", id = "removeconstants_2", ratio = 0)  %>>%
  po("dropcorr", id = "dropcorr", cutoff = 0.99) %>>%
  po("uniformization") %>>%
  po("dropna", id = "dropna_v2") %>>%
  po("learner", learner = lrn("regr.ranger"))
plot(graph_nonpca)
graph_nonpca_lrn = as_learner(graph_nonpca)

# pca ghraph
graph_pca = po("dropnacol", id = "dropnacol", cutoff = 0.05) %>>%
  po("dropna", id = "dropna") %>>%
  po("removeconstants", id = "removeconstants_1", ratio = 0)  %>>%
  po("winsorizesimple", id = "winsorizesimple", probs_low = 0.01, probs_high = 0.99, na.rm = TRUE) %>>%
  po("removeconstants", id = "removeconstants_2", ratio = 0)  %>>%
  po("dropcorr", id = "dropcorr", cutoff = 0.99) %>>%
  po("uniformization") %>>%
  po("dropna", id = "dropna_v2") %>>%
  # po("pca") %>>%
  po("pca_explained", var. = 0.99) %>>%
  po("learner", learner = lrn("regr.ranger"))
plot(graph_pca)
graph_pca_lrn = as_learner(graph_pca)

# threads
threads = as.integer(Sys.getenv("NCPUS"))
set_threads(graph_pca_lrn, n = threads)
set_threads(graph_nonpca_lrn, n = threads)

# pca params
as.data.table(graph_pca_lrn$param_set)[, .(id, class, lower, upper)]
search_space = ps(
  pca_explained.var. = p_fct(levels = c("0.90", "0.95", "0.99"),
                             trafo = function(x, param_set) {
                               switch(x,
                                      "0.90" = 0.90,
                                      "0.95" = 0.95,
                                      "0.99" = 0.99)
                             })
)

# inspect search space - test how it looks
design = rbindlist(generate_design_grid(search_space, 20)$transpose(), fill = TRUE)
design

# NESTED CV BENCHMARK -----------------------------------------------------
print("Benchmark")

# nested for loop
list.files(mlr3_save_path, full.names = TRUE)
nested_cv_benchmark <- function(i) {

  # debug
  print(i)

  # inner resampling
  print("Define CV")
  custom_ = rsmp("custom")
  custom_$instantiate(task_ret_week,
                      list(custom_inner$train_set(i)),
                      list(custom_inner$test_set(i)))

  # auto tuner
  print("Define autotuner")
  at_pca = auto_tuner(
    tuner = tnr("grid_search", resolution = 20, batch_size = 2),
    learner = graph_pca_lrn,
    resampling = custom_,
    measure = msr("regr.mse"),
    search_space = search_space
  )

  # outer resampling
  print("Define outer CV")
  customo_ = rsmp("custom")
  customo_$instantiate(task_ret_week, list(custom_outer$train_set(i)), list(custom_outer$test_set(i)))

  # nested CV for one round
  print("Benchmark!")
  design = benchmark_grid(
    tasks = list(task_ret_week, task_ret_month), #, task_ret_month2, task_ret_quarter
    learners = list(at_pca, graph_nonpca_lrn),
    resamplings = customo_
  )
  system.time({bmr = benchmark(design, store_models = TRUE)})

  # save locally and to list
  print("Save")
  time_ = format.POSIXct(Sys.time(), format = "%Y%m%d%H%M%S")
  saveRDS(bmr, file.path(mlr3_save_path, paste0(i, "-", time_, ".rds")))
  return(NULL)

}

i = as.integer(Sys.getenv('PBS_ARRAY_INDEX'))
start_time = Sys.time()
nested_cv_benchmark(i)
end_time = Sys.time()
end_time - start_time
