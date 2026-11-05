# Captures real ds.glm, ds.glmSLMA and ds.lmerSLMA outputs (dsBaseClient/dsBase 6.3.5) as fixtures.
# DSLite cannot run ds.glmSLMA because dsBase evaluates `dataName` in the global environment, so
# DSI calls are mocked to run the dsBase server functions in the global environment, one study at
# a time, which is what a real DataSHIELD server does.
# Run from the package root: Rscript tests/testthat/fixtures/make-fixtures.R
suppressPackageStartupMessages({
  library(DSLite); library(DSI); library(dsBase); library(dsBaseClient)
})
out_dir <- "tests/testthat/fixtures"
options(
  datashield.privacyControlLevel = "banana", nfilter.tab = 3, nfilter.subset = 3, nfilter.glm = 0.33,
  nfilter.string = 80, nfilter.stringShort = 20, nfilter.kNN = 3, nfilter.levels.density = 0.33,
  nfilter.levels.max = 40, nfilter.noise = 0.25, nfilter.privacy.old = 5
)
data("CNSIM1", "CNSIM2", "CNSIM3", package = "DSLite")
set.seed(1)

make_study_data <- function(d) {
  d <- d[stats::complete.cases(d[, c("LAB_HDL", "LAB_TSC", "GENDER", "PM_BMI_CONTINUOUS")]), ][1:400, ]
  d$clinic <- factor(sample(paste0("c", 1:8), nrow(d), replace = TRUE))
  d$LAB_HDL <- d$LAB_HDL + as.numeric(d$clinic) / 10
  d$GENDER <- factor(d$GENDER)
  d$obese <- as.integer(d$PM_BMI_CONTINUOUS > 30)
  d
}
studies <- list(study1 = make_study_data(CNSIM1), study2 = make_study_data(CNSIM2), study3 = make_study_data(CNSIM3))
disclosive <- studies$study3[1:20, ]
disclosive$GENDER <- factor(c(1, rep(0, 19)))
one_clinic <- studies$study2
one_clinic$clinic <- factor("c1")

server <- newDSLiteServer(config = defaultDSConfiguration(include = "dsBase"))
builder <- DSI::newDSLoginBuilder()
for (n in names(studies)) builder$append(server = n, url = "server", driver = "DSLiteDriver")
conns <- datashield.login(builder$build())

study_envs <- NULL
assign_data <- list()
load_study <- function(n) for (v in ls(study_envs[[n]])) assign(v, get(v, study_envs[[n]]), globalenv())
fake_aggregate <- function(conns, expr, ...) {
  if (is.character(expr)) expr <- parse(text = expr)[[1]]
  sapply(names(conns), function(n) {
    load_study(n)
    eval(expr, globalenv())
  }, simplify = FALSE)
}
fake_assign <- function(conns, symbol, value, ...) {
  for (n in names(conns)) {
    load_study(n)
    if (!is.null(assign_data[[n]])) assign("D", assign_data[[n]], globalenv())
    assign(symbol, eval(value, globalenv()), study_envs[[n]])
  }
  invisible(NULL)
}
run <- function(data, expr) {
  study_envs <<- lapply(data, function(d) list2env(list(D = d)))
  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      suppressMessages(expr),
      datashield.aggregate = fake_aggregate, datashield.assign = fake_assign, .package = "DSI"
    ),
    datashield.aggregate = fake_aggregate, .package = "dsBaseClient"
  )
}

drop_envs <- function(x) {
  if (inherits(x, "formula")) {
    environment(x) <- globalenv()
  } else if (is.call(x)) {
    x <- as.call(lapply(as.list(x), drop_envs))
  } else if (is.list(x) && !is.data.frame(x)) {
    x[] <- lapply(x, drop_envs)
  }
  x
}
save_fixture <- function(x, name) saveRDS(drop_envs(x), file.path(out_dir, paste0(name, ".rds")))

f <- "LAB_HDL ~ GENDER + PM_BMI_CONTINUOUS"
save_fixture(run(studies, ds.glm(f, family = "gaussian", data = "D", datasources = conns)), "glm_gaussian")
save_fixture(
  run(studies, ds.glm("obese ~ LAB_HDL + GENDER", family = "binomial", data = "D", datasources = conns)),
  "glm_binomial"
)
save_fixture(run(studies, ds.glmSLMA(f, family = "gaussian", dataName = "D", datasources = conns)), "glmSLMA_full")
save_fixture(
  run(studies, ds.glmSLMA(f, family = "gaussian", dataName = "D", datasources = conns, combine.with.metafor = FALSE)),
  "glmSLMA_no_metafor"
)
assign_data <- list(study1 = disclosive)
save_fixture(
  run(studies, ds.glmSLMA(f, family = "gaussian", dataName = "D", datasources = conns)),
  "glmSLMA_study1_failed"
)
assign_data <- list()

fl <- "LAB_HDL ~ LAB_TSC + (1 + LAB_TSC|clinic)"
save_fixture(run(studies, ds.lmerSLMA(fl, dataName = "D", datasources = conns)), "lmerSLMA_full")
save_fixture(
  run(studies, ds.lmerSLMA(fl, dataName = "D", datasources = conns, combine.with.metafor = FALSE)),
  "lmerSLMA_no_metafor"
)
save_fixture(
  run(
    list(study1 = studies$study1, study2 = one_clinic, study3 = disclosive),
    ds.lmerSLMA("LAB_HDL ~ LAB_TSC + GENDER + (1|clinic)", dataName = "D", datasources = conns)
  ),
  "lmerSLMA_studies_failed"
)
datashield.logout(conns)
