# modules/mod_import_sc.R
# Step-3.6 fixes:
#   - .ensure_10x_features(): pads single-column genes.tsv/features.tsv into a 3-column
#     features.tsv.gz and deactivates the offending genes.tsv (Read10X prefers genes.tsv
#     and crashes with "undefined columns selected" when it has <2 columns)
#   - load_single_cell_data(): checks matrix.mtx exists before Read10X(); fixes add_log&& bug
#   - prepare_seurat_object(): handles SCE, list, sparse/dense matrix
# Step-3.8 fix:
#   - .verify_upload_integrity(): a large upload (e.g. a multi-GB .h5) whose
#     temp-file write fails partway through (typically: the drive R's
#     tempdir() lives on runs out of space mid-upload — observed with
#     TMPDIR defaulting to a nearly-full C: drive even though the app/libs
#     live on D:) used to surface as a cryptic low-level HDF5 C++ stack
#     trace ("H5Fopen(): unable to open file... Iteration failed...") with
#     no indication of the real cause. Browser-reported size (input$file$size)
#     vs actual bytes written to datapath is a cheap, reliable signal for
#     exactly this failure mode -- checked before attempting to open the
#     file at all, replaced with an actionable French message pointing at
#     disk space / TMPDIR instead of the raw HDF5 error.

`%||%` <- function(a, b) if (is.null(a)) b else a

# ── Helper: CellRanger v2 / single-column compat ────────────────────────────────────────────
# Seurat::Read10X prefers genes.tsv whenever it exists (pre_ver_3 <- file.exists(genes.tsv))
# and then evaluates feature.names[, gene.column] with gene.column = 2 — a 1-column
# genes.tsv (symbols only, e.g. GSE176078 Wu2021) throws "undefined columns selected".
# Writing a padded features.tsv.gz is NOT enough while genes.tsv is still present, so it
# must be deactivated (renamed) after the repair. Same repair applies to a features file
# that itself has a single column.
.ensure_10x_features <- function(dir_path, log_fn = NULL) {
  log <- function(msg) if (!is.null(log_fn)) log_fn(msg)

  feat_tsv  <- file.path(dir_path, "features.tsv")
  feat_gz   <- file.path(dir_path, "features.tsv.gz")
  gene_tsv  <- file.path(dir_path, "genes.tsv")
  gene_gz   <- file.path(dir_path, "genes.tsv.gz")

  read_cols <- function(path) {
    tryCatch({
      if (grepl("\\.gz$", path)) {
        con <- gzfile(path, "rt"); on.exit(close(con)); lines <- readLines(con)
      } else {
        lines <- readLines(path)
      }
      length(read.table(text = lines, sep = "\t", header = FALSE,
                        quote = "", nrows = 1))
    }, error = function(e) 0L)
  }

  ncol_genes  <- if (file.exists(gene_tsv)) read_cols(gene_tsv) else 0L
  ncol_featgz <- if (file.exists(feat_gz))  read_cols(feat_gz)  else 0L
  ncol_featts <- if (file.exists(feat_tsv)) read_cols(feat_tsv) else 0L

  # Already importable by Read10X (genes.tsv wins if present, else features.tsv.gz,
  # else features.tsv): nothing to do
  if (ncol_genes >= 2 ||
      (ncol_genes == 0 && (ncol_featgz >= 2 ||
                           (ncol_featgz == 0 && ncol_featts >= 2)))) {
    return(invisible(NULL))
  }

  # Pick the best repair source: genes.tsv(.gz), then an existing features file
  src <- c(gene_tsv, gene_gz, feat_tsv, feat_gz)
  src <- src[file.exists(src)]
  if (length(src) == 0L) return(invisible(NULL))
  src <- src[1]

  gdf <- tryCatch({
    if (grepl("\\.gz$", src)) {
      con <- gzfile(src, "rt"); on.exit(close(con)); readLines(con)
    } else readLines(src)
  }, error = function(e) { log(paste("  ⚠ Lecture", basename(src), ":", e$message)); NULL })
  gdf <- tryCatch(
    read.table(text = gdf, sep = "\t", header = FALSE, stringsAsFactors = FALSE, quote = ""),
    error = function(e) { log(paste("  ⚠ Lecture", basename(src), ":", e$message)); NULL }
  )
  if (is.null(gdf) || ncol(gdf) == 0) return(invisible(NULL))

  # Pad to 3 columns: ID, Symbol, Type
  if      (ncol(gdf) == 1) { gdf$V2 <- gdf$V1; gdf$V3 <- "Gene Expression" }
  else if (ncol(gdf) == 2) { gdf$V3 <- "Gene Expression" }

  wrote <- tryCatch({
    gz_con <- gzfile(feat_gz, "wt")
    write.table(gdf, gz_con, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
    close(gz_con)
    TRUE
  }, error = function(e) { log(paste("  ⚠ Création features.tsv.gz:", e$message)); FALSE })
  if (!wrote) return(invisible(NULL))
  log("  ✓ CellRanger v2: features.tsv.gz (id\tsymbol\ttype) généré automatiquement")

  # Deactivate a 1-column genes.tsv — Read10X would still pick it and crash on [, 2]
  if (ncol_genes == 1) {
    bak <- paste(gene_tsv, "bak", sep = ".")
    if (file.rename(gene_tsv, bak)) {
      log(paste0("  ✓ genes.tsv à 1 colonne désactivé (→ ", basename(bak), "), ",
                 "features.tsv.gz sera utilisé"))
    } else {
      # rename can fail on locked files (Windows): overwrite in place with 2 columns
      tryCatch({
        write.table(gdf[, 1:2], gene_tsv, sep = "\t", col.names = FALSE,
                    row.names = FALSE, quote = FALSE)
        log("  ✓ genes.tsv à 1 colonne réécrit sur 2 colonnes (id\\tsymbol)")
      }, error = function(e) log(paste("  ⚠ Désactivation genes.tsv:", e$message)))
    }

    # With genes.tsv gone Read10X takes its v3 branch, which expects the
    # .gz forms of barcodes/matrix — compress them if only plain ones exist
    for (nm in c("barcodes.tsv", "matrix.mtx")) {
      plain <- file.path(dir_path, nm)
      gzf   <- paste0(plain, ".gz")
      if (file.exists(plain) && !file.exists(gzf)) {
        tryCatch({
          con_in <- file(plain, "rt"); lines <- readLines(con_in); close(con_in)
          con_out <- gzfile(gzf, "wt"); writeLines(lines, con_out); close(con_out)
          log(paste0("  ✓ ", nm, " compressé en ", basename(gzf), " (attendu par la branche v3 de Read10X)"))
        }, error = function(e) log(paste("  ⚠ Compression", nm, ":", e$message)))
      }
    }
  }

  invisible(feat_gz)
}

# ── Helper (Step-3.8): detect a truncated/corrupted upload before opening it ────────────────
#' Compare the browser-reported upload size to the actual bytes written to
#' the temp file. A mismatch means the write to disk failed partway through
#' (almost always: destination drive ran out of space) -- catching this here
#' turns an opaque low-level HDF5/Seurat parser crash into an actionable
#' message that names the actual cause and how to fix it.
#'
#' @param datapath  Path to the uploaded temp file (input$file$datapath).
#' @param expected_size  Browser-reported size in bytes (input$file$size).
#' @return list(ok, msg). ok=TRUE means sizes match (or expected_size unknown
#'   -- fileInput doesn't always populate $size reliably, in which case this
#'   check is silently skipped rather than raising a false alarm).
.verify_upload_integrity <- function(datapath, expected_size) {
  if (is.null(expected_size) || is.na(expected_size) || expected_size <= 0)
    return(list(ok = TRUE, msg = NULL))   # nothing to compare against — skip

  actual_size <- tryCatch(file.info(datapath)$size, error = function(e) NA_real_)
  if (is.na(actual_size))
    return(list(ok = FALSE, msg = .tr_plain("Fichier temporaire introuvable après upload — l'écriture a probablement échoué.")))

  if (actual_size < expected_size) {
    gb <- function(b) sprintf("%.2f Go", b / 1024^3)
    return(list(
      ok = FALSE,
      msg = sprintf(
        paste0(
          "\u274c Upload incomplet : %s reçus sur %s attendus. ",
          "Cause la plus probable : l'espace disque du dossier temporaire R (TMPDIR, ",
          "généralement sur le disque C:) est insuffisant pour ce fichier, MÊME SI l'app et ",
          "vos bibliothèques R sont installées ailleurs (ex: D:). ",
          "Solution : libérez de l'espace sur le disque C:, OU redirigez TMPDIR/TMP/TEMP vers ",
          "un disque avec plus d'espace libre via un fichier .Renviron ",
          "(ex: TMPDIR=D:/Rtemp), puis redémarrez la session R et réessayez l'import."
        ),
        gb(actual_size), gb(expected_size)
      )
    ))
  }

  list(ok = TRUE, msg = NULL)
}

# ── UI ────────────────────────────────────────────────────────────────────────────────────────
mod_import_sc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 400, title = i18n$t("Import Single-Cell"),
        # LOT 4A (V1.x UX): mapping stays in the analysis module (no UI move);
        # remind + native jump to the existing SC "0. Mapping IDs" panel.
        div(class = "alert alert-light", style = "font-size:0.78rem;",
            bsicons::bs_icon("info-circle"), " ",
            i18n$t("Identifiants non-symboles (Ensembl, Entrez, sondes Affymetrix) ? Le mapping d'IDs est disponible à l'étape 0 de l'analyse.")),
        actionButton(ns("goto_mapping"), i18n$t("Aller au mapping des IDs"),
                     class = "btn-outline-secondary btn-sm w-100 mb-2", icon = icon("arrow-right")),
        accordion(
          accordion_panel(
            i18n$t("Option A: Dossiers Multiples (10X)"),
            value = "opt_a",
            div(class = "alert alert-info", style = "font-size:0.85rem;",
                bsicons::bs_icon("info-circle"),
                " ", i18n$t("Importez plusieurs échantillons pour Harmony.")),
            # V1.x UX multi-échantillons : rendre le modèle mental explicite
            # (chaque import = un orig.ident distinct ; analyses aval compatibles).
            div(class = "alert alert-light", style = "font-size:0.8rem;border-left:3px solid #2C3E50;",
                bsicons::bs_icon("collection"),
                " ", i18n$t("Multi-échantillons : pour des expériences à plusieurs échantillons (ex: Contrôle & Traitement), importez chaque échantillon séparément. L'identité de l'échantillon est préservée pour la correction de batch et les analyses différentielles.")),
            div(class = "alert alert-light", style = "font-size:0.8rem;",
                bsicons::bs_icon("lightbulb"),
                " ", i18n$t("Formats acceptés : barcodes.tsv(.gz), features.tsv(.gz) ou genes.tsv(.gz), matrix.mtx(.gz).")),
            uiOutput(ns("dir_select_ui")),
            # Feature 6×10X (audit 2026-09-27, roadmap 3.1) : auto-découverte
            # des triplets 10X — 1 sélection remplace 6×(dossier + nom).
            uiOutput(ns("dir_scan_ui")),
            textInput(ns("sample_name"), i18n$t("Nom de l'échantillon"), placeholder = "Ex: Patient1"),
            actionButton(ns("btn_add_sample"), i18n$t("➕ Ajouter à la liste"), class = "btn-info w-100 mt-2"),
            hr(),
            h6(i18n$t("Échantillons ajoutés:"), style = "font-weight:bold;"),
            div(style = "max-height:200px;overflow-y:auto;border:1px solid #ddd;padding:10px;border-radius:5px;",
                uiOutput(ns("sample_list_display"))),
            actionButton(ns("btn_clear_samples"), i18n$t("🗑️ Tout Effacer"),
                         class = "btn-outline-danger btn-sm w-100 mt-2"),
            hr(),
            # Roadmap 3.2 : nom de design optionnel (project Seurat du merge).
            textInput(ns("design_name"), i18n$t("Nom du design (optionnel)"),
                      placeholder = .tr_plain("ex : Dose_2026 — sinon MultiSample")),
            verbatimTextOutput(ns("path_display"), placeholder = TRUE),
            # Roadmap 3.4 : annulation coopérative de la boucle d'import.
            actionButton(ns("btn_cancel_import_a"), i18n$t("⛔ Annuler l'import"),
                         class = "btn-outline-danger w-100 mb-1"),
            actionButton(ns("btn_load_dir"), i18n$t("🚀 Charger Tous les Échantillons"),
                         class = "btn-success w-100 mt-2", icon = icon("play"))
          ),
          accordion_panel(
            i18n$t("Option B: Fichiers Multiples (.rds, .h5, .h5ad)"),
            value = "opt_b",
            div(class = "alert alert-info", style = "font-size:0.85rem;",
                bsicons::bs_icon("info-circle"), " ", i18n$t("Importez plusieurs fichiers pour les fusionner.")),
            # V1.x UX multi-échantillons : même message que l'Option A (clé partagée).
            div(class = "alert alert-light", style = "font-size:0.8rem;border-left:3px solid #2C3E50;",
                bsicons::bs_icon("collection"),
                " ", i18n$t("Multi-échantillons : pour des expériences à plusieurs échantillons (ex: Contrôle & Traitement), importez chaque échantillon séparément. L'identité de l'échantillon est préservée pour la correction de batch et les analyses différentielles.")),
            div(class = "alert alert-light", style = "font-size:0.78rem;",
                bsicons::bs_icon("hdd"),
                " ", i18n$t("Fichiers volumineux (> quelques Go) : vérifiez l'espace disque disponible sur le disque où pointe le dossier temporaire de R (TMPDIR), pas seulement celui de l'app.")),
            fileInput(ns("file_upload"), i18n$t("Ajouter Fichier(s)"),
                      accept = c(".rds", ".h5", ".h5ad", ".loom", ".rda", ".RData"), multiple = TRUE),
            uiOutput(ns("file_list_display")),
            # Roadmap 3.4 : sélection par fichier avant chargement.
            uiOutput(ns("file_select_ui")),
            actionButton(ns("btn_cancel_import_b"), i18n$t("⛔ Annuler l'import"),
                         class = "btn-outline-danger w-100 mb-1"),
            actionButton(ns("btn_load_file"), i18n$t("🚀 Charger"), class = "btn-primary w-100", icon = icon("play"))
          ),
          accordion_panel(
            i18n$t("Option C: Fichier Unique (Classique)"),
            value = "opt_c",
            fileInput(ns("single_file_upload"), i18n$t("Charger un seul fichier"),
                      accept = c(".rds", ".h5", ".h5ad", ".loom", ".rda", ".RData")),
            helpText(i18n$t("Pour un seul échantillon.")),
            actionButton(ns("btn_load_single"), i18n$t("Charger"), class = "btn-warning w-100"),
            # .rda/.RData "Inspect & Select" — carte conditionnelle mutualisée
            # (contrat docs/contracts/RDATA_IMPORT_CONTRACT.md)
            rdata_picker_ui(ns("rdata_picker_sc"))
          ),
          # MD-4 (décision 5) : label optionnel + relation déclarée — un
          # import étiqueté enregistre une COPIE du jeu importé dans le
          # conteneur sc_datasets (contrat docs/contracts/SC_MULTI_CONTRACT.md).
          # SANS label, le comportement des options A/B/C est strictement
          # inchangé (garde zéro changement).
          accordion_panel(
            i18n$t("Multi-datasets SC (double jeu)"),
            value = "opt_multi",
            div(class = "alert alert-light", style = "font-size:0.78rem;",
                bsicons::bs_icon("info-circle"), " ",
                i18n$t("Renseignez un label pour enregistrer une copie du jeu importé dans le conteneur sc_datasets (le jeu actif reste l'objet importé). Relation : mode 1 = traiter avec les mêmes réglages que le jeu de référence ; mode 2 = réglages propres au jeu.")),
            textInput(ns("multi_label"), i18n$t("Label multi-datasets SC"),
                      placeholder = .tr_plain("ex : Rep2_T2")),
            selectInput(ns("multi_relation"), i18n$t("Relation déclarée (décision 5)"),
                        choices = setNames(
                          c("standalone", "shared_params", "distinct_params"),
                          c(.tr_plain("Jeu indépendant (aucune relation)"),
                            .tr_plain("Mode 1 — analyses séparées, paramètres partagés"),
                            .tr_plain("Mode 2 — analyses séparées, paramètres distincts"))
                        ), selected = "standalone")
          )
        )
      ),
      card(
        card_header(i18n$t("Résumé de l'objet chargé")),
        layout_columns(
          value_box(title = i18n$t("Cellules"), value = textOutput(ns("nb_cells")),
                    showcase = bsicons::bs_icon("people"), theme = "primary"),
          value_box(title = i18n$t("Gènes"), value = textOutput(ns("nb_genes")),
                    showcase = bsicons::bs_icon("diagram-3"), theme = "secondary"),
          value_box(title = i18n$t("Échantillons"), value = textOutput(ns("nb_samples")),
                    showcase = bsicons::bs_icon("collection"), theme = "info"),
          value_box(title = i18n$t("Statut"), value = textOutput(ns("status_obj")),
                    showcase = bsicons::bs_icon("check-circle"), theme = "light")
        ),
        # V1.x UX multi-échantillons : confirmation visuelle immédiate que les
        # échantillons sont reconnus comme entités distinctes (>= 2 orig.ident).
        uiOutput(ns("multisample_summary_ui")),
        card_body(h5(i18n$t("Console de Log"), class = "text-muted"),
                  verbatimTextOutput(ns("console_log"), placeholder = TRUE))
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────────────────────
mod_import_sc_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    logs <- reactiveVal("En attente d'import...")
    add_log <- function(msg) {
      logs(paste0("[", format(Sys.time(),"%H:%M:%S"), "] ", msg, "\n", logs()))
    }

    # ── The ONE place a (directory, sample name) pair becomes a Seurat object ──
    # Extracted from the body of the `btn_load_dir` observer below, which called
    # `load_single_cell_data()` -> `prepare_seurat_object()` -> `obj$orig.ident`
    # inline. The drive importer needs the SAME three steps, and the S3 decision
    # forbids a parallel SC path: a second copy of these three lines is a second
    # definition of what a sample is, and the two would drift the first time one
    # of them gained a step.
    #
    # `sample_name` is an ARGUMENT and is never derived here — no `basename()`
    # fallback. A human supplies it in the `sample_name` textInput; the drive
    # supplies it in the `import` block, and `ts_drive_validate_sc_import()`
    # refuses the payload when it is absent. That is deliberate and it differs
    # from the Spatial importer, which does fall back to `basename(dir_path())`:
    # `orig.ident` is the grouping key of every downstream SC reader, so a
    # directory label is not an acceptable substitute for a sample identity.
    sc_sample_object <- function(path, sample_name, log_fn = NULL) {
      log <- function(msg) if (!is.null(log_fn)) log_fn(msg)
      raw <- load_single_cell_data(path, log)
      obj <- prepare_seurat_object(raw, sample_name)
      obj$orig.ident <- sample_name
      obj
    }

    # ── MD-4 (décision 5) : producteur "import" du conteneur sc_datasets ───
    # Label optionnel renseigné → enregistre une COPIE du jeu importé
    # (contrat docs/contracts/SC_MULTI_CONTRACT.md). L'échec est une ALERTE :
    # l'import ne doit JAMAIS être interrompu (aucun stop() dans ce helper).
    .register_sc_multi_dataset <- function(obj) {
      lbl_raw <- input$multi_label %||% ""
      if (!nzchar(trimws(lbl_raw))) return(invisible(NULL))
      res <- tryCatch(
        sc_multi_register(global_data$sc_datasets, lbl_raw, obj,
                          relation = input$multi_relation %||% "standalone",
                          producer = "import"),
        sc_multi_error = function(e) e)
      if (inherits(res, "sc_multi_error")) {
        add_log(paste("⚠", res$message))
        showNotification(res$message, type = "warning", duration = 10)
        return(invisible(NULL))
      }
      global_data$sc_datasets <- res
      add_log(paste("🗄️", sprintf(.tr("Jeu SC « %s » enregistré dans sc_datasets (producteur import)."), trimws(lbl_raw))))
    }

    # ── .rda/.RData "Inspect & Select" (composant mutualisé) ───────────────
    # Contrat docs/contracts/RDATA_IMPORT_CONTRACT.md : le workspace est
    # inspecté dans un env isolé ; la classe est validée AVANT le commit.
    # L'hôte garde le commit : normalisation prepare_seurat_object puis
    # global_data$sc_obj (précédent des imports existants).
    .RDA_SC_EXPECTED <- c("Seurat", "SingleCellExperiment", "matrix",
                          "dgCMatrix", "dgTMatrix", "data.frame")
    rdata_file_rv <- reactiveVal(NULL)
    rdata_picker_server(
      "rdata_picker_sc",
      file_rv   = rdata_file_rv,
      commit_fn = function(obj, obj_name) {
        if (is.data.frame(obj)) obj <- as.matrix(obj)
        prepared <- prepare_seurat_object(obj, "SingleSample")
        global_data$sc_obj <- prepared
        global_data$sc_obj_epoch <- (global_data$sc_obj_epoch %||% 0L) + 1L  # purge résultats partagés (jeu remplacé)
        .register_sc_multi_dataset(prepared)
        add_log(paste(.tr("✅ Import réussi:"), ncol(prepared), .tr("cellules"), "—", obj_name))
        showNotification(paste(.tr("✅ Import réussi:"), ncol(prepared), .tr("cellules")),
                         type = "message", duration = 5)
      },
      expected  = .RDA_SC_EXPECTED,
      context   = "import single-cell (.RData)",
      tr        = .tr,
      log_fn    = add_log
    )

    # Dépôt d'un fichier .rda/.RData en Option C : contrôle d'intégrité puis
    # transfert au composant (aperçu + auto-import si objet unique compatible)
    observeEvent(input$single_file_upload, {
      f <- input$single_file_upload
      req(f)
      if (!rdata_is_explorable_file(f$datapath)) {
        rdata_file_rv(NULL)
        return()
      }
      integrity <- .verify_upload_integrity(f$datapath, f$size)
      if (!integrity$ok) {
        add_log(paste("❌", integrity$msg))
        showNotification(integrity$msg, type = "error", duration = 10)
        return()
      }
      rdata_file_rv(list(datapath = f$datapath, name = f$name, size = f$size))
    })


    # ── LOT 4A (V1.x UX): native jump to the EXISTING SC mapping panel ──────
    # No UI duplication: select the analysis page (top-level page_navbar via
    # the root session handle) and open the nested accordions hosting the
    # "0. Mapping IDs" panel. Silent no-op if the handles are missing.
    observeEvent(input$goto_mapping, {
      sess <- global_data$session
      req(!is.null(sess))
      nav_select(id = "main_nav", selected = "tab_sc", session = sess)
      try(accordion_panel_open(id = "sc-acc_workflow", values = "grp_prep", session = sess), silent = TRUE)
      try(accordion_panel_open(id = "sc-acc_prep", values = "0_mapping", session = sess), silent = TRUE)
    })

    # Re-push translated labels on language switch
    observeEvent(global_data$language, {
      updateTextInput(session, "sample_name", label = .tr("Nom de l'échantillon"))
    }, ignoreInit = TRUE)

    sample_list <- reactiveVal(list())
    # Roadmap 3.4 : annulation coopérative des boucles d'import (A et B).
    import_cancel <- reactiveVal(FALSE)
    observeEvent(input$btn_cancel_import_a, import_cancel(TRUE))
    observeEvent(input$btn_cancel_import_b, import_cancel(TRUE))
    volumes     <- c(Home = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "dir_select", roots = volumes, session = session)
    # Feature 6×10X (roadmap 3.1) : deuxième binding pour l'auto-découverte.
    shinyDirChoose(input, "dir_scan", roots = volumes, session = session)

    output$dir_select_ui <- renderUI({
      global_data$language
      shinyDirButton(
        ns("dir_select"),
        label = .tr("📁 Ajouter un Dossier"),
        title = .tr("Sélectionner dossier contenant matrix.mtx"),
        class = "btn-secondary w-100",
        icon  = icon("folder-open")
      )
    })

    output$dir_scan_ui <- renderUI({
      global_data$language
      shinyDirButton(
        ns("dir_scan"),
        label = .tr("⚡ Scanner un dossier parent (auto-découverte des échantillons)"),
        title = .tr("Sélectionner le dossier parent contenant UN sous-dossier par échantillon (chaque sous-dossier = triplets 10X)"),
        class = "btn-outline-info w-100",
        icon  = icon("folder-tree")
      )
    })

    # Auto-découverte : chaque sous-dossier du dossier parent contenant un
    # triplet 10X (matrix.mtx(.gz)) devient un échantillon nommé basename().
    # Les noms déjà présents dans la liste sont ignorés (garde anti-doublon).
    observeEvent(input$dir_scan, {
      base <- parseDirPath(volumes, input$dir_scan)
      req(length(base) > 0)
      add_log(paste(.tr("🔍 Scan du dossier parent:"), base))
      subdirs <- list.dirs(base, recursive = FALSE)
      hits  <- character(0)
      for (d in subdirs) {
        has_matrix <- any(file.exists(file.path(d, c("matrix.mtx", "matrix.mtx.gz"))))
        if (!has_matrix) next
        sn <- utils::basename(d)
        cs <- sample_list()
        if (sn %in% names(cs)) {
          add_log(paste("  =", sn, "—", .tr("déjà dans la liste, ignoré")))
          next
        }
        cs[[sn]] <- d; sample_list(cs)
        hits <- c(hits, sn)
      }
      if (length(hits)) {
        add_log(paste("  ✓", length(hits), .tr("échantillon(s) détecté(s):"), paste(hits, collapse = ", ")))
        showNotification(
          sprintf(.tr("⚡ %d échantillon(s) 10X détecté(s) et ajouté(s) : %s"),
                  length(hits), paste(hits, collapse = ", ")),
          type = "message", duration = 8)
      } else {
        showNotification(.tr("Aucun sous-dossier contenant matrix.mtx(.gz) trouvé — utilisez « Ajouter un Dossier » manuellement."),
                         type = "warning", duration = 8)
      }
    })

    dir_path <- reactiveVal(NULL)
    observeEvent(input$dir_select, {
      path <- parseDirPath(volumes, input$dir_select)
      if (length(path) > 0) { dir_path(path); add_log(paste(.tr("Dossier:"), path)) }
    })
    output$path_display <- renderText({
      global_data$language
      if (is.null(dir_path())) .tr("Aucun dossier sélectionné") else dir_path()
    })

    observeEvent(input$btn_add_sample, {
      req(dir_path(), input$sample_name)
      if (!nchar(trimws(input$sample_name))) {
        showNotification(.tr("⚠️ Nom vide."), type = "warning"); return()
      }
      cs <- sample_list()
      if (input$sample_name %in% names(cs)) {
        showNotification(.tr("⚠️ Ce nom existe déjà."), type = "warning"); return()
      }
      cs[[input$sample_name]] <- dir_path(); sample_list(cs)
      add_log(paste(.tr("Échantillon ajouté:"), input$sample_name))
    })

    output$sample_list_display <- renderUI({
      s <- names(sample_list())
      if (!length(s)) return(tags$em(.tr("Aucun échantillon"), style = "color:#999;"))
      tags$ul(lapply(s, tags$li))
    })

    observeEvent(input$btn_clear_samples, { sample_list(list()); add_log(.tr("Liste effacée")) })

    # ── Option A ────────────────────────────────────────────────────────────
    observeEvent(input$btn_load_dir, {
      req(sample_list())
      samples <- sample_list()
      add_log(paste(.tr("🔄 Import de"), length(samples), .tr("dossiers 10X...")))
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Chargement..."), value = 0)
      import_cancel(FALSE)
      tryCatch({
        obj_list <- list()
        for (i in seq_along(samples)) {
          if (import_cancel()) {
            add_log(.tr("⛔ Import annulé par l'utilisateur."))
            showNotification(.tr("Import annulé."), type = "warning", duration = 6)
            return()
          }
          sn <- names(samples)[i]; path <- samples[[i]]
          p$set(i/length(samples), detail=sn)
          add_log(paste("  📂", .tr("Lecture:"), path))
          obj <- sc_sample_object(path, sn, add_log)
          obj_list[[sn]] <- obj
          add_log(paste("    ✓", ncol(obj), .tr("cellules"), "—", nrow(obj), .tr("gènes")))
        }
        p$set(0.9, .tr("Fusion..."))
        merged <- if (length(obj_list)==1) obj_list[[1]] else
          merge(obj_list[[1]], y=obj_list[-1], add.cell.ids=names(obj_list),
                project = if (!is.null(input$design_name) && nzchar(trimws(input$design_name)))
                  trimws(input$design_name) else "MultiSample")
        # Roadmap 3.2 (audit 2026-09-27 §1.3 corrigé) : Seurat merge l'UNION des
        # gènes (zéro-remplissage). On journalise l'union et le manque par
        # échantillon, puis on joint les couches v5 (sinon l'assay reste
        # éclaté en counts.<sample> et certaines analyses aval dégradent).
        if (length(obj_list) > 1) {
          per_feat <- vapply(obj_list, function(o) nrow(o), integer(1))
          add_log(paste("  🧬", .tr("Gènes par échantillon :"),
                        paste(paste0(names(per_feat), "=", per_feat), collapse = ", ")))
          add_log(paste("  🧬", .tr("Union au merge :"), nrow(merged), .tr("gènes"),
                        "—", .tr("gènes absents = zéro-remplis (aucune suppression)")))
          merged <- tryCatch({
            a <- SeuratObject::JoinLayers(merged[["RNA"]])
            merged[["RNA"]] <- a
            merged
          }, error = function(e) merged)
        }
        global_data$sc_obj <- merged
        global_data$sc_obj_epoch <- (global_data$sc_obj_epoch %||% 0L) + 1L  # purge résultats partagés (jeu remplacé)
        .register_sc_multi_dataset(merged)
        add_log(paste("✅", ncol(merged), .tr("cellules,"), length(unique(merged$orig.ident)), .tr("échantillon(s)")))
        showNotification(paste(.tr("✅ Import réussi:"), ncol(merged), .tr("cellules")), type = "message", duration = 5)
      }, error = function(e) {
        msg <- paste(.tr("❌ Erreur:"), conditionMessage(e))
        add_log(msg); showNotification(msg, type = "error", duration = 10)
      })
    })

    # ── DRIVE LIVE CONTROL (S3) : the `import_sc` importer ──────────────────
    # Same contract as the Spatial and Bulk importers: `dir_path` and
    # `sample_name` arrived as DATA, already confined to the allowlisted roots,
    # already proven to be a 10x-v3 triplet, and already trimmed by
    # `ts_drive_validate_sc_import()`. Nothing here re-checks or re-derives them.
    #
    # The two-step HUMAN flow (`btn_add_sample` then `btn_load_dir`) is NOT
    # replayed. The drive calls `sc_sample_object()` — the same builder the human
    # loop calls — so there is one definition of "load a sample", and a
    # half-registered `sample_list()` is a state the drive simply cannot produce.
    #
    # ONE sample per call, and deliberately no `merge()`. The human path merges
    # when `sample_list()` holds several; a drive scenario carries ONE explicit
    # `sample_name`, so merging would mean either inventing extra names or
    # overwriting `sc_obj` on a second call. Both are a bigger contract than S3
    # authorises, so a second `import_file` REPLACES the object — which is the
    # honest reading of one name in, one object out.
    #
    # No `tryCatch`: the loader raises `sc_import_error` (the class the whole
    # module already uses, §2bv), and the watcher maps a raised condition onto an
    # `error` verdict with its message. Wrapping it here would only create a
    # second place for the two channels to disagree — the same argument
    # mod_import_spatial.R:764 makes for its own importer.
    ts_drive_publish_importer(global_data, TS_DRIVE_SC_IMPORT_MODULE, function(request) {
      obj <- sc_sample_object(request$dir_path, request$sample_name, add_log)
      global_data$sc_obj <- obj
      .register_sc_multi_dataset(obj)
      add_log(paste("✅", ncol(obj), .tr("cellules,"), request$sample_name))
      list(ok = TRUE, status = "applied", errors = character(0),
           warnings = character(0),
           # Reported as a WARNING, not an error: the import itself succeeded, and
           # the only thing that did not happen is the MD-4 multi-dataset
           # registration, which is a container concern. A verdict that said
           # `error` here would tell the agent to send a different payload, and
           # no different payload would help.
           warnings = character(0))
    })

    # ── Option B ────────────────────────────────────────────────────────────
    output$file_list_display <- renderUI({
      req(input$file_upload)
      files <- input$file_upload
      tags$ul(style="list-style:none;padding:0;",
        lapply(1:nrow(files), function(i) {
          tags$li(style="padding:4px;border-bottom:1px solid #eee;",
                  "📄 ", files$name[i],
                  tags$small(style="color:#666;", paste0(" (", round(files$size[i]/1024^2,1), " MB)")))
        }))
    })

    # Roadmap 3.4 : sélection par fichier avant chargement (tout coché par
    # défaut — décocher exclut le fichier du merge).
    output$file_select_ui <- renderUI({
      req(input$file_upload)
      files <- input$file_upload
      checkboxGroupInput(ns("file_select"),
                         .tr("Fichiers à charger (décochez pour exclure)"),
                         choices = files$name, selected = files$name)
    })

    observeEvent(input$btn_load_file, {
      req(input$file_upload)
      files <- input$file_upload
      selected <- if (is.null(input$file_select)) files$name else
        intersect(input$file_select, files$name)
      if (!length(selected)) {
        showNotification(.tr("Aucun fichier sélectionné — cochez au moins un fichier."),
                         type = "warning", duration = 6)
        return()
      }
      add_log(paste(.tr("🔄 Import de"), length(selected), "/", nrow(files), "fichier(s)..."))
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Chargement..."), value = 0)
      import_cancel(FALSE)
      tryCatch({
        obj_list <- list()
        for (i in which(files$name %in% selected)) {
          if (import_cancel()) {
            add_log(.tr("⛔ Import annulé par l'utilisateur."))
            showNotification(.tr("Import annulé."), type = "warning", duration = 6)
            return()
          }
          fn <- tools::file_path_sans_ext(files$name[i])
          p$set(i/nrow(files), detail=files$name[i])

          # Roadmap 3.3 (audit 2026-09-27 §1.6 corrigé) : file_path_sans_ext ne
          # retire qu'UNE extension — deux fichiers de même stem (S1.rds +
          # S1.h5 → « S1 ») s'écrasaient silencieusement. Garde explicite.
          if (fn %in% names(obj_list)) {
            stop(errorCondition(sprintf(
              "%s : un fichier « %s » a déjà été chargé (même nom après retrait d'extension). Renommez un des fichiers ou utilisez l'Option A (dossiers) pour préserver les deux jeux de cellules.",
              files$name[i], fn), class = "sc_import_error"))
          }

          # Step-3.8: verify the upload actually completed before attempting
          # to open it (see .verify_upload_integrity() docstring above).
          integrity <- .verify_upload_integrity(files$datapath[i], files$size[i])
          if (!integrity$ok) {
            add_log(paste("  ❌", files$name[i], "—", integrity$msg))
            stop(errorCondition(integrity$msg, class = "sc_import_error"))
          }

          add_log(paste("  📄", files$name[i]))
          raw <- load_single_cell_data(files$datapath[i], add_log)
          obj <- prepare_seurat_object(raw, fn)
          obj$orig.ident <- fn; obj_list[[fn]] <- obj
          add_log(paste("    ✓", ncol(obj), .tr("cellules")))
        }
        p$set(0.9, .tr("Fusion..."))
        merged <- if (length(obj_list)==1) obj_list[[1]] else
          merge(obj_list[[1]], y=obj_list[-1], add.cell.ids=names(obj_list), project="MultiFile")
        if (length(obj_list) > 1) {
          add_log(paste("  🧬", .tr("Union au merge :"), nrow(merged), .tr("gènes"),
                        "—", .tr("gènes absents = zéro-remplis (aucune suppression)")))
          merged <- tryCatch({
            a <- SeuratObject::JoinLayers(merged[["RNA"]])
            merged[["RNA"]] <- a
            merged
          }, error = function(e) merged)
        }
        global_data$sc_obj <- merged
        global_data$sc_obj_epoch <- (global_data$sc_obj_epoch %||% 0L) + 1L  # purge résultats partagés (jeu remplacé)
        .register_sc_multi_dataset(merged)
        add_log(paste("✅", ncol(merged), .tr("cellules")))
        showNotification(paste(.tr("✅ Import réussi:"), ncol(merged), .tr("cellules")), type = "message", duration = 5)
      }, error = function(e) {
        msg <- paste(.tr("❌ Erreur:"), conditionMessage(e))
        add_log(msg); showNotification(msg, type = "error", duration = 12)
      })
    })

    # ── Option C ────────────────────────────────────────────────────────────
    observeEvent(input$btn_load_single, {
      req(input$single_file_upload)
      if (rdata_is_explorable_file(input$single_file_upload$datapath)) {
        # Le contenu .RData/.rds est inspecté/importé dès le dépôt du fichier
        # (observeEvent single_file_upload -> composant mutualisé).
        add_log(.tr("Le contenu .RData est inspecté automatiquement dès le dépôt du fichier (carte ci-dessous)."))
        return()
      }
      add_log(paste(.tr("Import fichier unique...")))
      withProgress(message = .tr("Chargement..."), {
        tryCatch({
          # Step-3.8: same upload-integrity check as Option B — this is the
          # path the user's 1M-neurons .h5 import went through when it hit
          # the HDF5 "unable to open file" crash (root cause: TMPDIR on a
          # nearly-full C: drive truncating the ~4.5Go upload mid-write).
          integrity <- .verify_upload_integrity(input$single_file_upload$datapath,
                                                input$single_file_upload$size)
          if (!integrity$ok) {
            add_log(paste("❌", integrity$msg))
            stop(errorCondition(integrity$msg, class = "sc_import_error"))
          }

          raw <- load_single_cell_data(input$single_file_upload$datapath, add_log)
          obj <- prepare_seurat_object(raw, "SingleSample")
          global_data$sc_obj <- obj
          global_data$sc_obj_epoch <- (global_data$sc_obj_epoch %||% 0L) + 1L  # purge résultats partagés (jeu remplacé)
          .register_sc_multi_dataset(obj)
          add_log(paste(.tr("✅ Import réussi:"), ncol(obj), .tr("cellules")))
          showNotification(.tr("✅ Import réussi:"), type = "message")
        }, error = function(e) {
          msg <- paste(.tr("❌ Erreur:"), conditionMessage(e))
          add_log(msg); showNotification(msg, type = "error", duration = 12)
        })
      })
    })

    # ── Outputs ──────────────────────────────────────────────────────────────
    output$nb_cells   <- renderText({ if(is.null(global_data$sc_obj)) "-" else format(ncol(global_data$sc_obj), big.mark=",") })
    output$nb_genes   <- renderText({ if(is.null(global_data$sc_obj)) "-" else format(nrow(global_data$sc_obj), big.mark=",") })
    output$nb_samples <- renderText({ if(is.null(global_data$sc_obj)) "-" else length(unique(global_data$sc_obj$orig.ident)) })
    output$status_obj <- renderText({
      global_data$language
      if(is.null(global_data$sc_obj)) .tr("⚪ Inactif")
      else { n <- length(unique(global_data$sc_obj$orig.ident))
             if(n>1) paste("🟢 Multi (",n,")") else "🟡 Mono" }
    })
    output$console_log <- renderText({
      global_data$language
      txt <- logs()
      if (identical(txt, "En attente d'import...")) .tr("En attente d'import...") else txt
    })

    # ── V1.x UX multi-échantillons : aperçu récapitulatif (≥ 2 orig.ident) ───
    # Confirmation visuelle immédiate que les échantillons importés sont
    # reconnus comme entités distinctes. Reste light par construction :
    # méta.data uniquement (aucun passage sur la matrice de comptages, aucune
    # densification — cap mémoire respecté). Gènes = médiane de nFeature_RNA
    # par échantillon (repli : nombre de gènes de l'objet).
    output$multisample_summary_ui <- renderUI({
      global_data$language
      obj <- global_data$sc_obj
      if (is.null(obj) || is.null(obj$orig.ident)) return(NULL)
      if (length(unique(obj$orig.ident)) < 2) return(NULL)
      tagList(
        hr(),
        h6(.tr("Aperçu multi-échantillons — chaque échantillon est reconnu comme une entité distincte :"),
           style = "font-weight:bold;"),
        DT::dataTableOutput(ns("multisample_summary"), height = "auto")
      )
    })

    output$multisample_summary <- DT::renderDataTable({
      global_data$language
      obj <- global_data$sc_obj
      req(obj, obj$orig.ident)
      meta    <- obj@meta.data
      ids     <- obj$orig.ident
      samples <- levels(factor(ids))
      smry <- list()
      smry[[.tr("Échantillon")]] <- samples
      smry[[.tr("Cellules")]]    <- as.integer(table(ids)[samples])
      if (!is.null(meta$nFeature_RNA)) {
        gmed <- suppressWarnings(tapply(meta$nFeature_RNA, ids,
                                        function(x) round(stats::median(x, na.rm = TRUE))))
        smry[[.tr("Gènes (méd./cellule)")]] <- as.integer(gmed[samples])
      } else {
        smry[[.tr("Gènes")]] <- rep(nrow(obj), length(samples))
      }
      if ("condition" %in% colnames(meta)) {
        smry[[.tr("Condition")]] <- tapply(as.character(meta$condition), ids,
          function(x) paste(unique(x), collapse = ", "))[samples]
      }
      if ("batch" %in% colnames(meta)) {
        smry[[.tr("Batch")]] <- tapply(as.character(meta$batch), ids,
          function(x) paste(unique(x), collapse = ", "))[samples]
      }
      if ("replicate" %in% colnames(meta)) {
        smry[[.tr("Réplicat")]] <- tapply(as.character(meta$replicate), ids,
          function(x) paste(unique(x), collapse = ", "))[samples]
      }
      df <- as.data.frame(smry, check.names = FALSE, stringsAsFactors = FALSE)
      ts_datatable(df, page_length = 10, buttons = FALSE,
                   dom = "t", filter = "none")
    })

    # ── load_single_cell_data ─────────────────────────────────────────────
    load_single_cell_data <- function(path, log_fn = NULL) {
      log <- function(msg) if (!is.null(log_fn)) log_fn(msg)

      # 1. Directory
      if (dir.exists(path)) {
        h5_path <- file.path(path, "filtered_feature_bc_matrix.h5")
        if (file.exists(h5_path)) return(Read10X_h5(h5_path))

        # Only call Read10X when matrix.mtx actually exists
        has_matrix <- any(file.exists(file.path(path, c("matrix.mtx", "matrix.mtx.gz"))))
        if (!has_matrix) {
          # Look for a single .rds inside the directory
          rds <- list.files(path, pattern="\\.rds$", ignore.case=TRUE, full.names=TRUE)
          if (length(rds) == 1) { log(paste("  ℹ RDS dans dossier:", basename(rds))); return(readRDS(rds)) }
          stop(errorCondition(paste0(
            "Dossier sans fichier matrix.mtx(.gz) ni filtered_feature_bc_matrix.h5.\n",
            .tr_plain("Si ce dossier contient un .rds ou .h5ad, utilisez l'Option B/C.")),
            class = "sc_import_error"))
        }

        # CellRanger v2 compat: create features.tsv.gz if only genes.tsv present
        .ensure_10x_features(path, log_fn)
        return(Read10X(path))
      }

      ext <- tolower(tools::file_ext(path))

      # 2. .rds
      if (ext == "rds") return(readRDS(path))

      # 2bis. .rda/.RData — inspecté par le composant mutualisé ; en flux
      # direct (Option B : fusion multi-fichiers), seul un workspace à objet
      # unique est importable ici (sinon message orientant vers l'Option C).
      if (rdata_is_supported_file(path)) {
        env <- rdata_load_env(path)
        on.exit(rdata_free(env), add = TRUE)
        nms <- ls(envir = env)
        if (length(nms) > 1L) {
          stop(paste0(
            "Ce fichier .RData contient ", length(nms),
            " objets. Utilisez l'Option C (Fichier Unique) pour sélectionner ",
            "l'objet à importer, ou exportez les objets souhaités depuis l'aperçu."),
            call. = FALSE)
        }
        log(paste("  ℹ Objet unique détecté :", nms[1]))
        return(rdata_extract_object(env, nms[1]))
      }

      # 3. .h5 — BPCells for large files
      if (ext == "h5") {
        if (requireNamespace("BPCells", quietly=TRUE) && file.size(path) > 1e9) {
          mat     <- BPCells::open_matrix_10x_hdf5(path)
          tmp_dir <- tempfile(pattern="bpcells_10x_")
          BPCells::write_matrix_dir(mat=mat, dir=tmp_dir)
          return(BPCells::open_matrix_dir(dir=tmp_dir))
        }
        return(Read10X_h5(path))
      }

      # 4. .h5ad — cascade of converters
      # Roadmap 5.4 (audit 2026-09-27 §1.7) : chaque convertisseur est tenté
      # et son échec CAPTURÉ + journalisé — le message terminal cite les
      # causes réelles au lieu d'accuser à tort des paquets installés.
      if (ext == "h5ad") {
        h5ad_errs <- character(0)
        if (requireNamespace("BPCells", quietly=TRUE)) {
          tryCatch({ mat <- BPCells::open_matrix_anndata_hdf5(path)
                tmp <- tempfile(pattern="bpcells_h5ad_")
                BPCells::write_matrix_dir(mat=mat, dir=tmp)
                return(BPCells::open_matrix_dir(dir=tmp)) },
                error = function(e) {
                  h5ad_errs <<- c(h5ad_errs, paste0("BPCells : ", conditionMessage(e)))
                  log(paste("  ⚠ BPCells h5ad :", conditionMessage(e)))
                  NULL })
        }
        if (requireNamespace("zellkonverter", quietly=TRUE)) {
          tryCatch({ sce <- zellkonverter::readH5AD(file=path, use_hdf5=TRUE, raw=TRUE)
                if (!"counts" %in% SummarizedExperiment::assayNames(sce))
                  SummarizedExperiment::assay(sce,"counts") <- SummarizedExperiment::assay(sce,SummarizedExperiment::assayNames(sce)[1])
                return(Seurat::as.Seurat(sce, counts="counts", data=NULL)) },
                error = function(e) {
                  h5ad_errs <<- c(h5ad_errs, paste0("zellkonverter : ", conditionMessage(e)))
                  log(paste("  ⚠ zellkonverter h5ad :", conditionMessage(e)))
                  NULL })
        }
        if (requireNamespace("sceasy", quietly=TRUE)) {
          tryCatch({ tmp_rds <- tempfile(fileext=".rds")
                sceasy::convertFormat(path, from="anndata", to="seurat", outFile=tmp_rds)
                return(readRDS(tmp_rds)) },
                error = function(e) {
                  h5ad_errs <<- c(h5ad_errs, paste0("sceasy : ", conditionMessage(e)))
                  log(paste("  ⚠ sceasy h5ad :", conditionMessage(e)))
                  NULL })
        }
        stop(errorCondition(paste0(
          "Impossible de charger .h5ad.",
          if (length(h5ad_errs)) paste0(
            " Causes rencontrées (dans l'ordre) : ", paste(h5ad_errs, collapse = " | "),
            " — résolvez la cause la plus profonde ou convertissez le fichier en .h5/.rds.")
          else " Installez BPCells, zellkonverter ou sceasy."),
          class = "sc_import_error"))
      }

      # 5. .loom
      if (ext == "loom") {
        if (!requireNamespace("loomR", quietly=TRUE)) stop(errorCondition("Package 'loomR' requis.", class = "sc_import_error"))
        lconn <- loomR::connect(path, mode="r"); on.exit(lconn$close())
        return(Seurat::as.Seurat(lconn))
      }

      stop(errorCondition(paste0("Format non supporté : ", ext), class = "sc_import_error"))
    }

    # ── prepare_seurat_object — class detection ───────────────────────────
    prepare_seurat_object <- function(raw, sample_name = NULL) {
      proj <- sample_name %||% "scData"

      if (inherits(raw, "Seurat")) {
        if (!is.null(raw[["RNA"]]) && !inherits(raw[["RNA"]], "Assay5"))
          tryCatch({ raw[["RNA"]] <- as(raw[["RNA"]], "Assay5") }, error=function(e) NULL)
        return(raw)
      }

      if (inherits(raw, "SingleCellExperiment")) {
        mat <- tryCatch({
          cn <- SummarizedExperiment::assayNames(raw)
          SummarizedExperiment::assay(raw, if ("counts" %in% cn) "counts" else cn[1])
        }, error=function(e) NULL)
        if (!is.null(mat)) return(CreateSeuratObject(counts=mat, project=proj))
        return(Seurat::as.Seurat(raw))
      }

      # Multi-modal list (Read10X with multiple modalities)
      if (is.list(raw) && !is.data.frame(raw) && length(raw) > 0) {
        key <- if ("Gene Expression" %in% names(raw)) "Gene Expression" else names(raw)[1]
        return(CreateSeuratObject(counts=raw[[key]], project=proj))
      }

      # Sparse / dense matrix fallback
      return(CreateSeuratObject(counts=raw, project=proj))
    }

  })
}
