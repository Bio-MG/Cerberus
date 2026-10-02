# =============================================================================
# test-contract-guard.R — le garde §7 se prouve lui-même (doctrine du dépôt :
# « le sonde ment, pas le code »). Sans ce test, un garde de skip pourrait
# pourrir en pass silencieux ou en erreur de fichier — exactement ce qu'il
# existe pour empêcher (piège C8 de l'audit 2026-10-01).
# =============================================================================

test_that("le garde §7 lève un SKIP nommant §7 et le fichier (jamais une erreur)", {
  absent <- file.path(ts_project_root(), "docs", "contracts", "__N_EXISTE_PAS__.md")
  cnd <- tryCatch(
    .ts_contract_readlines(absent),
    skip = function(c) c,
    error = function(e) e
  )
  expect_s3_class(cnd, "skip")
  expect_match(conditionMessage(cnd), "\u00a77", fixed = TRUE)
  expect_match(conditionMessage(cnd), "__N_EXISTE_PAS__.md", fixed = TRUE)
})

test_that("le garde §7 lit normalement un contrat présent (zéro changement local)", {
  p <- file.path(ts_project_root(), "docs", "contracts", "SC_MULTI_CONTRACT.md")
  skip_if_not(file.exists(p), .ts_contract_skip_msg(p))
  expect_gt(length(.ts_contract_readlines(p, warn = FALSE)), 0L)
})
