# =============================================================================
# test-cerberus-dock.R — N3 cerberus-dock (satellites slice S7) : lib PUR
# =============================================================================
# FICHE (fait foi) : docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_
# 2026-10-26.md §2-S7 — « Manifest first (frozen format) » ; « FIRST TENANT =
# N1 watchtower (dogfood) » ; « One process per satellite (O6 untouched) » ;
# « the dock never auto-starts anything without the operator's command ».
# Contrat figé : tools/satellites/dock/MANIFEST.md v1.0.0 (schémas dock_manifest
# + capabilities/<name>.json, verbes CLI, exit codes).
# Rails RED (règle error=0) : au stade RED les corps sont des STUBS (NULL) —
# chaque expectation échoue comme ÉCHEC, jamais comme erreur ; skip_if_not sur
# les sections qui itèrent. Le R du dock vit sous tools/ (hors conventions gate
# par construction — note MANIFEST.md §1) : CE test est la seule porte du dock.
# Cibles mesurées disque : dock_manifest.json + capabilities/watchtower.json +
# la page watchtower (le locataire pointe vers du RÉEL).
# =============================================================================

.dock_dir <- file.path(ts_project_root(), "tools", "satellites", "dock")
source(file.path(.dock_dir, "dock_lib.R"))

# ── (a) Le manifeste shippé existe et valide ---------------------------------

test_that("(a) dock_manifest.json shippé : parse + validateur TRUE", {
  expect_true(file.exists(file.path(.dock_dir, "MANIFEST.md")),
              info = "le contrat figé MANIFEST.md doit exister (contract-first)")
  expect_true(file.exists(file.path(.dock_dir, "dock_manifest.json")))
  m <- ts_dock_manifest_load(file.path(.dock_dir, "dock_manifest.json"))
  expect_true(is.list(m),
              info = "le manifeste doit se charger (stub NULL = FAIL attendu au RED)")
  skip_if_not(is.list(m))
  expect_true(isTRUE(ts_dock_manifest_valid(m)))
  expect_identical(m$format_version, "1.0.0",
                   info = "format FIGÉ v1.0.0 — toute évolution = version nouvelle")
})

# ── (b) Le validateur refuse les corruptions (format figé, champs inconnus) --

test_that("(b) ts_dock_manifest_valid — FALSE sur 8 corruptions", {
  m <- ts_dock_manifest_load(file.path(.dock_dir, "dock_manifest.json"))
  skip_if_not(is.list(m) && is.list(m$satellites) && length(m$satellites) >= 1L)
  base <- m

  # 1. version de format autre que la figée
  bad <- base; bad$format_version <- "1.0.1"
  expect_false(isTRUE(ts_dock_manifest_valid(bad)),
               info = "une autre version que 1.0.0 est refusée (figé)")
  # 2. champ top-level INCONNU (frozen format)
  bad <- base; bad$extra <- 1
  expect_false(isTRUE(ts_dock_manifest_valid(bad)),
               info = "champ top-level inconnu = refus (frozen format)")
  # 3. satellites vide
  bad <- base; bad$satellites <- list()
  expect_false(isTRUE(ts_dock_manifest_valid(bad)))
  # 4. name invalide (caractères hors [a-z0-9_])
  bad <- base; bad$satellites[[1]]$name <- "WatchTower!"
  expect_false(isTRUE(ts_dock_manifest_valid(bad)))
  # 5. command vide
  bad <- base; bad$satellites[[1]]$command <- character(0)
  expect_false(isTRUE(ts_dock_manifest_valid(bad)))
  # 6. port hors plage
  bad <- base; bad$satellites[[1]]$port <- 99999
  expect_false(isTRUE(ts_dock_manifest_valid(bad)))
  # 7. health.url non http(s)
  bad <- base; bad$satellites[[1]]$health$url <- "file:///etc/passwd"
  expect_false(isTRUE(ts_dock_manifest_valid(bad)))
  # 8. shares_app_process = true (règle O6 — JAMAIS partagé avec l'app)
  cap_path <- file.path(.dock_dir, m$satellites[[1]]$capability)
  skip_if_not(file.exists(cap_path))
  cap <- jsonlite::fromJSON(cap_path, simplifyVector = FALSE)
  bad_cap <- cap; bad_cap$process$shares_app_process <- TRUE
  expect_false(isTRUE(ts_dock_capability_valid(bad_cap)),
               info = "un satellite partageant le process de l'app viole O6")

  # validator pur : n'explose jamais sur du malformé
  expect_false(isTRUE(ts_dock_manifest_valid(NULL)))
  expect_false(isTRUE(ts_dock_manifest_valid(list())))
})

# ── (c) Le verdict de santé : décision PURE ----------------------------------

test_that("(c) ts_dock_health_verdict — matrice OK/FAIL/TIMEOUT", {
  v <- ts_dock_health_verdict(200L, 200L, 0.4, 5)
  expect_identical(v$verdict, "OK",
                   info = "200 attendu + sous le timeout = OK (stub NULL = FAIL au RED)")
  skip_if_not(is.list(v))
  v <- ts_dock_health_verdict(404L, 200L, 0.3, 5)
  expect_identical(v$verdict, "FAIL", info = "mauvais status = FAIL")
  v <- ts_dock_health_verdict(200L, 200L, 7.2, 5)
  expect_identical(v$verdict, "TIMEOUT", info = "au-delà du timeout = TIMEOUT")
  v <- ts_dock_health_verdict(500L, 200L, 7.2, 5)
  expect_identical(v$verdict, "FAIL", info = "status faux prime : FAIL")
  # jamais d'erreur sur du bornier
  expect_identical(ts_dock_health_verdict(NA_integer_, 200L, 1, 5)$verdict, "FAIL")
  expect_identical(ts_dock_health_verdict(200L, NA_integer_, 1, 5)$verdict, "FAIL")
})

# ── (d) Le locataire watchtower pointe vers du RÉEL (cohérence disque) -------

test_that("(d) locataire watchtower : page existante, port cohérent, capability alignée", {
  m <- ts_dock_manifest_load(file.path(.dock_dir, "dock_manifest.json"))
  skip_if_not(is.list(m) && is.list(m$satellites) && length(m$satellites) >= 1L)
  s <- ts_dock_satellite(m, "watchtower")
  expect_true(is.list(s), info = "watchtower = PREMIER LOCATAIRE (fiche §2-S7)")

  # la commande sert la page depuis la racine : la page EXISTE dans le dépôt
  expect_true(file.exists(file.path(ts_project_root(),
                                    "tools", "satellites", "watchtower", "index.html")),
              info = "l'entrée de santé pointe vers la page réelle de S1")

  # port : le PORT du manifeste figure DANS la ligne de commande (cohérence)
  skip_if_not(is.list(s))
  expect_true(as.character(s$port) %in% s$command,
              info = "le port déclaré doit figurer dans command (serveur statique)")

  # health.url porte le même port
  expect_true(grepl(paste0(":", s$port), s$health$url, fixed = TRUE),
              info = "health.url et port doivent concorder")

  # capability : existe, alignée (name), read-only, O6
  cap_path <- file.path(.dock_dir, s$capability)
  expect_true(file.exists(cap_path), info = "capability déclaré doit exister")
  cap <- jsonlite::fromJSON(cap_path, simplifyVector = FALSE)
  expect_true(isTRUE(ts_dock_capability_valid(cap)))
  expect_identical(cap$satellite, s$name)
  expect_length(cap$surface$writes, 0L) # v1 : satellites read-only (writes vide)

  # un seul process par satellite (O6) : ports UNIQUES dans le manifeste
  ports <- vapply(m$satellites, function(x) x$port, numeric(1))
  expect_false(anyDuplicated(ports) > 0L,
               info = "un port par satellite — jamais partagé (O6)")
})

# ── (e) Le pid-file : chemin déterministe sous tools/_drive/satellites/ ------

test_that("(e) ts_dock_pid_file — chemin figé, hors git (répertoire satellites)", {
  p <- ts_dock_pid_file(ts_project_root(), "watchtower")
  expect_true(is.character(p) && length(p) == 1L && nzchar(p),
              info = "chemin de pid-file (stub NULL = FAIL au RED)")
  skip_if_not(is.character(p))
  expect_true(grepl("tools/_drive/satellites/watchtower.pid", p, fixed = TRUE) ||
                grepl("tools\\\\_drive\\\\satellites\\\\watchtower.pid", p, fixed = TRUE),
              info = "état runtime sous tools/_drive/satellites/ (gitigné)")
})
