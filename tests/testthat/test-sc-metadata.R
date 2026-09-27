# =============================================================================
# test-sc-metadata.R — unitaires de la logique pure du design condition/réplicat
# =============================================================================
source_project_file("R/sc/sc_metadata.R")

meta6 <- data.frame(
  orig.ident    = rep(c("1a", "2a", "3a", "1b", "2b", "3b"), each = 3),
  nFeature_RNA  = c(200, 300, 250, 210, 320, 280, 190, 310, 240, 205, 315, 275, 195, 305, 245, 215, 325, 285),
  stringsAsFactors = FALSE
)

# ── Table design ─────────────────────────────────────────────────────────────
test_that("sc_metadata_sample_table extrait un échantillon par niveau, trié", {
  tbl <- sc_metadata_sample_table(meta6)
  expect_identical(tbl$sample, c("1a", "1b", "2a", "2b", "3a", "3b"))
  expect_true(all(is.na(tbl$condition)) && all(is.na(tbl$replicate)))
  expect_error(sc_metadata_sample_table(meta6, sample_col = "absente"),
               class = sc_metadata_error_class())
})

# ── Parse des noms (deux conventions) ───────────────────────────────────────
test_that("parse_sample_label : préfixe (A_1) et suffixe (1a)", {
  p1 <- sc_metadata_parse_sample_label("A_1", "first")
  expect_identical(p1$condition, "A"); expect_identical(p1$replicate, "1")

  p2 <- sc_metadata_parse_sample_label("1a", "last")
  expect_identical(p2$condition, "a"); expect_identical(p2$replicate, "1")

  # Collé sans séparateur
  p3 <- sc_metadata_parse_sample_label("B2", "first")
  expect_identical(p3$condition, "B"); expect_identical(p3$replicate, "2")

  # Indéductible : condition = nom, réplicat NA (éditable, jamais fabriqué)
  p4 <- sc_metadata_parse_sample_label("patient1", "first")
  expect_identical(p4$condition, "patient1"); expect_true(is.na(p4$replicate))

  expect_error(sc_metadata_parse_sample_label(""), class = sc_metadata_error_class())
  expect_error(sc_metadata_parse_sample_label(123),  class = sc_metadata_error_class())
})

test_that("parse_sample_table remplit les 6 échantillons du design cible", {
  tbl <- sc_metadata_parse_sample_table(sc_metadata_sample_table(meta6), cond_position = "last")
  expect_identical(tbl$condition, c("a", "b", "a", "b", "a", "b"))   # trié : 1a 1b 2a 2b 3a 3b
  expect_identical(tbl$replicate, c("1", "1", "2", "2", "3", "3"))
})

# ── Apply : échec sans repli silencieux ─────────────────────────────────────
test_that("sc_metadata_apply propage condition/réplicat au niveau cellule", {
  tbl <- sc_metadata_parse_sample_table(sc_metadata_sample_table(meta6), cond_position = "last")
  out <- sc_metadata_apply(meta6, tbl)
  # meta6 = 1a,2a,3a (condition a) puis 1b,2b,3b (condition b), 3 cellules chacun
  expect_identical(as.character(out$condition), rep(c("a", "b"), each = 9))
  expect_identical(as.character(unique(out$replicate[out$orig.ident == "1a"])), "1")
  expect_identical(as.character(unique(out$replicate[out$orig.ident == "3b"])), "3")
})

test_that("sc_metadata_apply ÉCHOUE (sans fabrication) si un échantillon manque", {
  tbl <- sc_metadata_parse_sample_table(sc_metadata_sample_table(meta6), "last")
  tbl <- tbl[1:5, ]  # il manque "3b"
  expect_error(sc_metadata_apply(meta6, tbl), regexp = "3b",
               class = sc_metadata_error_class())
})

test_that("sc_metadata_apply : condition manquante = erreur explicite", {
  tbl <- sc_metadata_sample_table(meta6)
  expect_error(sc_metadata_apply(meta6, tbl), regexp = "Condition manquante",
               class = sc_metadata_error_class())
})

test_that("sc_metadata_apply : réplicat vide replie sur l'identifiant échantillon", {
  tbl <- sc_metadata_sample_table(meta6)
  tbl$condition <- c("a", "b", "a", "b", "a", "b")
  out <- sc_metadata_apply(meta6, tbl)
  expect_identical(sort(unique(out$replicate[out$orig.ident == "1a"])), "1a")
})

# ── CSV join ─────────────────────────────────────────────────────────────────
test_that("sc_metadata_join_csv joint par clé et replie le réplicat manquant", {
  csv <- data.frame(sample = c("1a", "1b", "2a", "2b", "3a", "3b"),
                    condition = c("A", "B", "A", "B", "A", "B"),
                    replicate = c("1", "1", "2", "2", "3", "3"),
                    stringsAsFactors = FALSE)
  out <- sc_metadata_join_csv(meta6, csv)
  expect_true(all(out$condition[out$orig.ident == "1b"] == "B"))
  expect_identical(as.character(unique(out$replicate[out$orig.ident == "3b"])), "3")

  csv_norep <- csv[, c("sample", "condition")]
  out2 <- sc_metadata_join_csv(meta6, csv_norep)
  expect_false("replicate" %in% colnames(out2))

  expect_error(sc_metadata_join_csv(meta6, csv[1:5, ]),
               regexp = "3b", class = sc_metadata_error_class())
  expect_error(sc_metadata_join_csv(meta6, data.frame(wrong = "x")),
               regexp = "sample", class = sc_metadata_error_class())
  expect_error(sc_metadata_join_csv(meta6, data.frame(sample = c("1a", "1b", "2a", "2b", "3a", "3b"))),
               regexp = "condition", class = sc_metadata_error_class())
})

# ── Récapitulatif design ─────────────────────────────────────────────────────
test_that("sc_metadata_design_recap récapitule et bloque la pseudoreplication", {
  cells <- table(meta6$orig.ident)
  tbl_ok <- data.frame(sample = names(cells), condition = rep(c("a", "b"), 3),
                       replicate = c("1", "1", "2", "2", "3", "3"),
                       stringsAsFactors = FALSE)
  res <- sc_metadata_design_recap(tbl_ok, cells_per_sample = cells)
  expect_true(res$ok)
  expect_identical(sort(res$recap$condition), c("a", "b"))
  expect_true(all(res$recap$n_replicates == 3L))
  expect_true(all(res$recap$n_cells == 9L))

  tbl_mono <- tbl_ok; tbl_mono$condition <- "a"
  res2 <- sc_metadata_design_recap(tbl_mono)
  expect_false(res2$ok)
  expect_true(any(grepl("condition", res2$blockers)))

  tbl_1rep <- tbl_ok; tbl_1rep$replicate <- "1"
  res3 <- sc_metadata_design_recap(tbl_1rep)
  expect_false(res3$ok)
  expect_true(any(grepl("Pseudoreplication", res3$blockers)))
})

# ── Constructeur d'erreur classée (C9b : fonction possédée mentionnée) ──────
test_that(".sc_metadata_stop produit une erreur classée sc_metadata_error, sans call", {
  err <- tryCatch(.sc_metadata_stop("sonde C9b"), error = function(e) e)
  expect_s3_class(err, "sc_metadata_error")
  expect_identical(conditionMessage(err), "sonde C9b")
  # call. = FALSE (style C10) : la trace n'est pas fabriquée par le garde
  expect_null(attr(err, "call"))
})
