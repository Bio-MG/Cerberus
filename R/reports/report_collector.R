# =============================================================================
# R/reports/report_collector.R — Rapport consolide 4F : COLLECTEUR (Stage 17)
# =============================================================================
# PRINCIPE (roadmap Stage 17) : le rapport est un COMPILATEUR de l'etat
# canonique + de la provenance, PAS un moteur de reconstruction. Aucune
# re-execution d'analyse, aucun parametre de figure modifie, aucune section
# fabriquee. Ce fichier COLLECTE ce qui existe deja :
#   - identite du jeu de donnees (empreinte v2 reutilisee de
#     build_object_identity_v2() — jamais re-implementee) ;
#   - apercu pipeline (snapshot QC deja produit par le module 1) ;
#   - resultats canoniques exposes dans l'etat partage SC (velocity,
#     communication : expositions additives des modules 8/8b ; DA 4E :
#     expositions des modules 8c-8e) — resumes DESCRIPTIFS uniquement ;
#   - vues croisees reconstruites via build_da_cross_method_summary()
#     (vue pure et deterministe — sanctionne par le contrat DA_CROSS_VIEWS §7) ;
#   - manifeste de provenance CONSOLIDE (provenance_to_dataframe, regle 7) ;
#   - session info + snapshot des constantes TS_* consommees.
#
# Les resume par domaine ne lisent QUE des champs figes des contrats
# (VELOCITY / COMMUNICATION / DA_DESIGN / MILO / SCCODA_RESULT_CONTRACT).
# Toute extraction fragile est tryCatch : un echec de resume ne fabrique
# jamais une valeur — il est reporte tel quel dans le resume.
#
# Sourcing (app.R section 4c) : APRES tous les domaines R/sc (besoin des
# resultats figes, de sc_r_script_text et des gardes assert_*).
# =============================================================================

# Domaines suivis par le rapport — liste FIGEE (ordre d'affichage du rapport).
# Post-V1.0 (mandat utilisateur) : pseudobulk + correlation ajoutes (sections
# tables pures, meme esprit compilateur). 4F-EXT (2026-09-13, parking V1.x
# consomme — rapport MD-2 §5.2) : bulk_multi_comparison ajoute.
.report_analysis_domains <- c(
  "markers", "pseudobulk", "correlation", "pathways", "trajectory",
  "velocity", "communication",
  "da_design", "da_milo", "da_sccoda", "da_cross",
  "bulk_multi_comparison"
)

# Domaines "contrat" : resultats canoniques a identite verifiee (empreinte v2)
# et provenance OBLIGATOIRE — une section sans provenance est REFUSEE.
.report_contract_domains <- c(
  "velocity", "communication", "da_design", "da_milo", "da_sccoda"
)

# Domaines "legacy" : resultats pre-contra (data.frames de l'etat partage),
# traces par la provenance partagee — jamais bloques, jamais drapeautes
# "valides" au sens contrat.
.report_legacy_domains <- c("markers", "pseudobulk", "correlation", "pathways", "trajectory")

# Domaines "global" (4F-EXT) : resultats plats AUTO-DATES lus dans global_data
# (hors etat partage SC) — pas de provenance partagee ni d'empreinte v2
# possible (resultat cross-datasets, non lie a l'objet SC courant). Le verdict
# reste valid_legacy mais avec un libelle dedie (voir report_validator.R) ;
# la tracabilite repose sur l'horodatage ran_at du resultat (contrat
# BULK_MULTI_CONTRACT §10.4) et l'analysis_id "bulk-multi-compare".
.report_global_domains <- c("bulk_multi_comparison")

# Constantes TS_* photographiees dans le rapport (snapshot de configuration —
# consommees, jamais redefinies ici).
.report_config_keys <- c(
  "TS_DA_MILO_DISPLAY_ALPHA", "TS_DA_CROSS_SIGNIF_FRACTION",
  "TS_DA_SCCODA_FDR_TARGET", "TS_DA_MILO_SEED", "TS_DA_SCCODA_SEED",
  "TS_DA_MIN_REPLICATES_PER_CONDITION", "TS_REPORT_MAX_TABLE_ROWS",
  "TS_REPORT_MAX_PROVENANCE_ROWS"
)

.rep_stop <- function(msg) {
  stop(errorCondition(
    msg,
    state = "invalid_input",
    class = "report_error"
  ))
}

#' Resume "champ / valeur" a deux colonnes (aplatit une liste nommee en
#' chaines — CSV/HTML-safe, aucune valeur inventee).
.report_kv_df <- function(x) {
  if (length(x) == 0L) {
    return(data.frame(champ = character(0), valeur = character(0),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    champ = names(x),
    valeur = vapply(x, function(v) {
      if (is.null(v) || length(v) == 0L || all(is.na(v))) ""
      else paste(format(v), collapse = ", ")
    }, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

#' Resume descriptif d'un domaine du rapport (jamais une re-analyse)
#'
#' @param domain Un nom de .report_analysis_domains.
#' @param sc_obj Objet SC courant (ou stub a dimnames).
#' @param shared_rv Etat partage SC (ou NULL).
#' @param global_data Etat global applicatif (reactiveValues ou liste, ou
#'   NULL) — lu UNIQUEMENT pour les domaines "global" (.report_global_domains,
#'   4F-EXT : bulk_multi_comparison) ; jamais ecrit.
#' @return list(present, summary (data.frame champ/valeur), analysis_ids,
#'   provenance_available, identity_checked, identity_ok (TRUE/FALSE/NA),
#'   identity_note (character), extras (liste libre, p.ex. vues croisées)).
.report_domain_summary <- function(domain, sc_obj, shared_rv,
                                   global_data = NULL) {
  out <- list(
    present = FALSE, summary = .report_kv_df(NULL), analysis_ids = character(0),
    provenance_available = FALSE, identity_checked = FALSE,
    identity_ok = NA, identity_note = character(0), extras = list()
  )

  # ── Domaines legacy (etat partage, data.frames) ────────────────────────────
  if (domain == "markers") {
    md <- state_get(shared_rv, "markers_data")
    if (is.data.frame(md) && nrow(md) > 0L) {
      out$present <- TRUE
      out$summary <- .report_kv_df(c(
        n_lignes = nrow(md), n_colonnes = ncol(md),
        groupes = tryCatch(
          if ("cluster" %in% names(md)) length(unique(md$cluster)) else NA_integer_,
          error = function(e) { ts_log_swallow("report_collector.report_domain_summary", e); NA_integer_ })
      ))
      out$extras$table <- md
    }
    return(out)
  }
  if (domain == "correlation") {
    cg <- state_get(shared_rv, "correlated_genes")
    tgt <- state_get(shared_rv, "corr_target_gene")
    if (is.data.frame(cg) && nrow(cg) > 0L) {
      out$present <- TRUE
      cor_max <- tryCatch({
        v <- cg$correlation
        if (is.null(v) || !is.numeric(v)) NA_real_ else round(max(abs(v), na.rm = TRUE), 3)
      }, error = function(e) { ts_log_swallow("report_collector.report_domain_summary", e); NA_real_ })
      out$summary <- .report_kv_df(c(
        gene_cible = tgt,
        n_lignes = nrow(cg),
        correlation_abs_max = cor_max
      ))
      out$extras$table <- cg
    }
    return(out)
  }
  if (domain == "pseudobulk") {
    pr <- state_get(shared_rv, "pseudobulk_result")
    if (is.list(pr) && identical(pr$type, "sc_pseudobulk_de") &&
        is.data.frame(pr$de_table) && nrow(pr$de_table) > 0L) {
      out$present <- TRUE
      out$summary <- .report_kv_df(c(
        moteur = pr$engine,
        groupe_cible = pr$target,
        groupe_reference = pr$reference,
        n_genes_testes = pr$n_genes,
        n_significatifs = pr$n_significant
      ))
      out$extras$de_table <- pr$de_table
    }
    return(out)
  }
  if (domain == "pathways") {
    pr <- state_get(shared_rv, "pathway_results")
    pdb <- state_get(shared_rv, "pathway_db")
    if (is.data.frame(pr) && nrow(pr) > 0L) {
      out$present <- TRUE
      out$summary <- .report_kv_df(c(
        n_lignes = nrow(pr), n_colonnes = ncol(pr),
        base = if (is.null(pdb)) NA_character_ else as.character(pdb)
      ))
      out$extras$table <- pr
    }
    return(out)
  }
  if (domain == "trajectory") {
    meta <- tryCatch(sc_obj@meta.data, error = function(e) { ts_log_swallow("report_collector.report_domain_summary", e); NULL })
    has_pt <- !is.null(meta) && "pseudotime" %in% colnames(meta)
    traj_red <- state_get(shared_rv, "traj_reduction")
    traj_mth <- state_get(shared_rv, "traj_method")
    traj_gns <- state_get(shared_rv, "traj_genes")
    if (isTRUE(has_pt) || !is.null(traj_mth) || length(traj_gns) > 0L) {
      out$present <- TRUE
      out$summary <- .report_kv_df(c(
        pseudotime_dans_metadata = has_pt,
        reduction = traj_red,
        methode = traj_mth,
        n_genes_trajet = length(traj_gns %||% character(0))
      ))
    }
    return(out)
  }

  # ── Domaines a contrat (resultats canoniques) ──────────────────────────────
  if (domain == "velocity") {
    res <- state_get(shared_rv, "velocity_result")
    if (!is.null(res) && identical(res$type, "rna_velocity")) {
      out$present <- TRUE
      out$analysis_ids <- as.character(res$analysis_id %||% character(0))
      out$provenance_available <- !is.null(res$provenance) &&
        !is.null(res$provenance$analysis_id)
      out$identity_checked <- TRUE
      out$identity_ok <- tryCatch({
        s <- velocity_result_is_stale(res, sc_obj)
        if (is.na(s)) NA else !s
      }, error = function(e) { ts_log_swallow("report_collector.report_domain_summary", e); NA })
      dims <- res$dimensions %||% NULL
      out$summary <- .report_kv_df(c(
        statut = as.character(res$status %||% NA_character_),
        n_cellules = if (!is.null(dims)) unname(dims[1]) else NULL,
        n_genes = if (!is.null(dims)) unname(dims[2]) else NULL,
        n_avertissements = length(res$warnings %||% character(0))
      ))
      out$extras$has_vectors <- !is.null(res$velocity_vectors)
    }
    return(out)
  }
  if (domain == "communication") {
    res <- state_get(shared_rv, "communication_result")
    if (!is.null(res) && identical(res$type, "cell_cell_communication")) {
      out$present <- TRUE
      out$analysis_ids <- as.character(res$analysis_id %||% character(0))
      out$provenance_available <- !is.null(res$provenance) &&
        !is.null(res$provenance$analysis_id)
      out$identity_checked <- TRUE
      collection <- state_get(shared_rv, "communication_collection")
      active_sample <- state_get(shared_rv, "active_communication_sample")
      collection_error <- NULL
      liana <- tryCatch(
        if (!is.null(collection)) {
          assert_liana_collection(collection, sc_obj)
          list(
            manifest = liana_collection_sample_manifest(collection),
            condition_summary = liana_collection_condition_summary(collection),
            by_sample = build_liana_collection_table(collection)
          )
        } else NULL,
        error = function(e) {
          collection_error <<- conditionMessage(e)
          NULL
        }
      )
      if (!is.null(collection_error)) {
        out$extras$collection_error <- as.character(collection_error)
      }
      out$identity_ok <- if (!is.null(collection_error)) {
        FALSE
      } else if (!is.null(liana) &&
                 !is.null(collection$object_identity$fingerprint)) {
        TRUE
      } else {
        tryCatch({
          s <- communication_result_is_stale(res, sc_obj)
          if (is.na(s)) NA else !s
        }, error = function(e) { ts_log_swallow("report_collector.report_domain_summary", e); NA })
      }
      inp <- res$input_summary %||% NULL
      rank_value <- function(field) {
        if (!field %in% colnames(res$canonical_table)) return(NA_character_)
        values <- unique(as.character(res$canonical_table[[field]]))
        values <- values[!is.na(values) & nzchar(values)]
        if (length(values)) values[[1L]] else NA_character_
      }
      summary_values <- c(
        statut = as.character(res$status %||% NA_character_),
        methode_source = as.character(res$source_method %||% NA_character_),
        n_lignes_entree = if (!is.null(inp)) inp$n_rows_input else NULL,
        n_lignes_canoniques = if (!is.null(inp)) inp$n_rows_canonical else NULL,
        colonne_identite = as.character(res$identity_column %||% NA_character_),
        direction_rang = rank_value("rank_direction"),
        aggregation_rang = rank_value("rank_aggregation_mode")
      )
      if (!is.null(liana)) {
        active_text <- if (length(active_sample) == 1L) {
          as.character(active_sample)
        } else {
          NA_character_
        }
        active_valid <- length(active_sample) == 1L &&
          !is.na(active_sample) && nzchar(as.character(active_sample)) &&
          as.character(active_sample) %in% liana$manifest$sample_key
        summary_values <- c(
          summary_values,
          n_echantillons = nrow(liana$manifest),
          n_conditions = nrow(liana$condition_summary),
          methode_liana = as.character(collection$method %||% NA_character_),
          ressource_liana = as.character(collection$resource %||% NA_character_),
          n_echantillon_actif = as.integer(active_valid),
          echantillon_actif = active_text
        )
        manifest_ids <- if ("analysis_id" %in% colnames(liana$manifest)) {
          as.character(liana$manifest$analysis_id)
        } else {
          character(0)
        }
        sample_result_ids <- unlist(lapply(collection$results, function(result) {
          as.character(result$analysis_id %||% character(0))
        }), use.names = FALSE) %||% character(0)
        ids <- unique(c(
          as.character(collection$analysis_id %||% character(0)),
          manifest_ids,
          sample_result_ids,
          as.character(res$analysis_id %||% character(0))
        ))
        out$analysis_ids <- unique(ids[!is.na(ids) & nzchar(ids)])
        out$extras$sample_manifest <- liana$manifest
        out$extras$condition_summary <- liana$condition_summary
        out$extras$by_sample <- liana$by_sample
        if (!isTRUE(active_valid)) out$identity_ok <- FALSE
      }
      out$summary <- .report_kv_df(summary_values)
      out$extras$canonical_table <- res$canonical_table
    }
    return(out)
  }
  if (domain == "da_design") {
    res <- state_get(shared_rv, "da_design_result")
    if (!is.null(res) && identical(res$type, "da_design")) {
      out$present <- TRUE
      out$analysis_ids <- as.character(res$analysis_id %||% character(0))
      out$provenance_available <- !is.null(res$provenance) &&
        !is.null(res$provenance$analysis_id)
      out$identity_checked <- TRUE
      out$identity_ok <- tryCatch({
        s <- da_design_result_is_stale(res, sc_obj)
        if (is.na(s)) NA else !s
      }, error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA })
      cfg <- res$config %||% list()
      out$summary <- .report_kv_df(c(
        statut = as.character(res$status %||% NA_character_),
        unite_composition = as.character(res$composition_unit %||% NA_character_),
        colonne_echantillon = as.character(cfg$sample_id %||% NA_character_),
        colonne_condition = as.character(cfg$condition %||% NA_character_),
        n_conditions = tryCatch(
          if (is.data.frame(res$condition_summary)) nrow(res$condition_summary) else NA_integer_,
          error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_integer_ }),
        n_echantillons = tryCatch(
          if (is.data.frame(res$sample_summary)) nrow(res$sample_summary) else NA_integer_,
          error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_integer_ }),
        milo_eligible = tryCatch(
          as.character(res$milo_eligibility$eligible), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_character_ }),
        sccoda_eligible = tryCatch(
          as.character(res$sccoda_eligibility$eligible), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_character_ })
      ))
      out$extras$condition_summary <- res$condition_summary
      out$extras$sample_summary <- res$sample_summary
      out$extras$exclusions <- res$exclusions
    }
    return(out)
  }
  if (domain == "da_milo") {
    res <- state_get(shared_rv, "da_milo_result")
    if (!is.null(res) && identical(res$type, "milo_da")) {
      out$present <- TRUE
      out$analysis_ids <- as.character(res$analysis_id %||% character(0))
      out$provenance_available <- !is.null(res$provenance) &&
        !is.null(res$provenance$analysis_id)
      out$identity_checked <- TRUE
      out$identity_ok <- tryCatch({
        s <- milo_result_is_stale(res, sc_obj)
        if (is.na(s)) NA else !s
      }, error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA })
      tc <- res$tested_contrast %||% list()
      out$summary <- .report_kv_df(c(
        statut = as.character(res$status %||% NA_character_),
        condition_cible = as.character(tc$target %||% NA_character_),
        condition_reference = as.character(tc$reference %||% NA_character_),
        reduction = tryCatch(
          as.character(res$parameters$reduction), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_character_ }),
        graine = tryCatch(
          as.character(res$parameters$seed), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_character_ }),
        n_voisinages = tryCatch(
          if (is.data.frame(res$DA_table)) nrow(res$DA_table) else NA_integer_,
          error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_integer_ }),
        n_cellules_dans_voisinages = tryCatch(
          if (is.data.frame(res$neighbourhood_summary))
            res$neighbourhood_summary$n_cells_in_nhoods[1] else NA_integer_,
          error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_integer_ })
      ))
      out$extras$DA_table <- res$DA_table
      out$extras$neighbourhood_summary <- res$neighbourhood_summary
      out$extras$sample_composition <- res$sample_composition
    }
    return(out)
  }
  if (domain == "da_sccoda") {
    res <- state_get(shared_rv, "da_sccoda_result")
    if (!is.null(res) && identical(res$type, "sccoda_da")) {
      out$present <- TRUE
      out$analysis_ids <- as.character(res$analysis_id %||% character(0))
      out$provenance_available <- !is.null(res$provenance) &&
        !is.null(res$provenance$analysis_id)
      out$identity_checked <- TRUE
      out$identity_ok <- tryCatch({
        s <- sccoda_result_is_stale(res, sc_obj)
        if (is.na(s)) NA else !s
      }, error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA })
      diag <- res$convergence_diagnostics %||% list()
      out$summary <- .report_kv_df(c(
        statut = as.character(res$status %||% NA_character_),
        identite_reference = as.character(res$reference_identity %||% NA_character_),
        effets_credibles = paste(res$credible_effects %||% character(0), collapse = "; "),
        ess_min = tryCatch(as.character(diag$ess_min), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_character_ }),
        rhat_max = tryCatch(as.character(diag$rhat_max), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_character_ }),
        n_lignes_effets = tryCatch(
          if (is.data.frame(res$effect_table)) nrow(res$effect_table) else NA_integer_,
          error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_integer_ })
      ))
      out$extras$effect_table <- res$effect_table
      out$extras$composition_table <- res$composition_table
    }
    return(out)
  }
  if (domain == "da_cross") {
    milo <- state_get(shared_rv, "da_milo_result")
    sccoda <- state_get(shared_rv, "da_sccoda_result")
    if (!is.null(milo) && !is.null(sccoda)) {
      # Vue pure et deterministe des deux resultats figes (contrat
      # DA_CROSS_VIEWS §7 : "le rapport consolide consommera ces vues via les
      # contrats") — AUCUNE methode DA reexecutee.
      sm <- tryCatch(
        build_da_cross_method_summary(milo, sccoda),
        error = function(e) { ts_log_swallow("report_collector.rank_value", e); NULL })
      if (!is.null(sm)) {
        out$present <- TRUE
        out$analysis_ids <- unique(c(
          as.character(milo$analysis_id %||% character(0)),
          as.character(sccoda$analysis_id %||% character(0)),
          "sc-da-cross"
        ))
        out$provenance_available <- TRUE  # derivee des deux resultats a contrat
        out$identity_checked <- TRUE
        out$identity_ok <- NA
        out$identity_note <- "Vue descriptive reconstruite a partir des deux resultats figes (aucune reanalyse)."
        conc <- sm$concordance %||% NULL
        dis <- sm$disagreement %||% NULL
        cmp <- sm$comparability %||% list()
        out$summary <- .report_kv_df(c(
          n_identites_comparees = tryCatch(
            if (is.data.frame(conc)) nrow(conc) else NA_integer_,
            error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_integer_ }),
          n_desaccords = tryCatch(
            if (is.data.frame(dis)) nrow(dis) else NA_integer_,
            error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_integer_ }),
          entierement_comparables = tryCatch(
            as.character(cmp$fully_comparable), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_character_ }),
          reserves = paste(cmp$caveats %||% character(0), collapse = " | ")
        ))
        out$extras$method_summary <- sm
      }
    }
    return(out)
  }
  # ── Domaines "global" (4F-EXT) : resultats plats auto-dates de global_data ─
  if (domain == "bulk_multi_comparison") {
    res <- if (!is.null(global_data)) state_get(global_data, "bulk_multi_comparison") else NULL
    # Garde de forme (contrat BULK_MULTI_CONTRACT §10.4) : liste plate avec
    # per_dataset + concordance data.frames et ran_at horodaté — on ne
    # "répare" jamais un résultat partial, il est simplement absent.
    if (is.list(res) && is.data.frame(res$per_dataset) &&
        nrow(res$per_dataset) > 0L && is.data.frame(res$concordance) &&
        !is.null(res$ran_at)) {
      out$present <- TRUE
      out$analysis_ids <- "bulk-multi-compare"
      out$provenance_available <- FALSE
      out$identity_checked <- FALSE
      out$identity_ok <- NA
      out$identity_note <- paste(
        "Résultat global auto-daté (ran_at) — comparaison CROSS-datasets,",
        "non liée à l'objet SC courant : pas d'empreinte v2 applicable.")
      out$summary <- .report_kv_df(c(
        n_datasets = tryCatch(length(res$datasets), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_integer_ }),
        datasets = tryCatch(paste(res$datasets, collapse = "; "), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_character_ }),
        contraste = tryCatch(as.character(res$contrast), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_character_ }),
        seuil_lfc = tryCatch(as.numeric(res$lfc_thresh), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_real_ }),
        seuil_padj = tryCatch(as.numeric(res$padj_thresh), error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_real_ }),
        date_calcul = tryCatch(format(res$ran_at, "%Y-%m-%d %H:%M:%S"),
                               error = function(e) { ts_log_swallow("report_collector.rank_value", e); NA_character_ })
      ))
      out$extras$per_dataset <- res$per_dataset
      out$extras$concordance <- res$concordance
      out$extras$intersection_dt <- res$intersection_dt
    }
    return(out)
  }
  out
}

#' Collecter l'entree canonique du rapport consolide (COMPILATEUR, 4F)
#'
#' Lit l'etat existant (objet SC + etat partage) et produit l'objet
#' \code{consolidated_report_input} — aucune analyse n'est (re)executee.
#'
#' @param sc_obj Objet Seurat courant (un stub a dimnames suffit en tests).
#' @param shared_rv Etat partage SC (create_sc_shared_state) ou NULL.
#' @param options Liste nommee : \code{title}, \code{subtitle}, \code{notes},
#'   \code{language} ("fr"/"en"), \code{include_tables} (logical).
#' @param global_data Etat global applicatif (ou NULL — defaut) : lu pour les
#'   domaines "global" uniquement (4F-EXT : bulk_multi_comparison), jamais
#'   ecrit. NULL = comportement d'origine strictement conserve.
#' @return Liste canonique \code{type = "consolidated_report_input"} (champs
#'   figes, voir docs/contracts/CONSOLIDATED_REPORT_CONTRACT.md).
#' @errors \code{report_error} (\code{invalid_input}) si \code{sc_obj} est
#'   absent — le rapport compile un projet EXISTANT, il n'en fabrique pas.
collect_consolidated_report_input <- function(sc_obj, shared_rv = NULL,
                                              options = list(),
                                              global_data = NULL) {
  if (is.null(sc_obj) || is.null(tryCatch(dim(sc_obj), error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NULL }))) {
    .rep_stop(paste0(
      "collect_consolidated_report_input() : aucun objet Single-Cell charge — ",
      "le rapport consolide compile un projet existant, il ne fabrique pas de ",
      "contenu. Importez et traitez un objet SC d'abord."
    ))
  }
  opts <- list(
    title = as.character(options$title %||% "Rapport consolidé du projet")[1],
    subtitle = as.character(options$subtitle %||% "")[1],
    notes = as.character(options$notes %||% "")[1],
    language = as.character(options$language %||% "fr")[1],
    include_tables = isTRUE(options$include_tables %||% TRUE)
  )
  warns <- character(0)

  # ── Jeu de donnees ──────────────────────────────────────────────────────────
  is_seurat <- inherits(sc_obj, "Seurat")
  meta <- tryCatch(sc_obj@meta.data, error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NULL })
  ds_idents <- tryCatch({
    if (is_seurat) as.data.frame(table(SeuratObject::Idents(sc_obj),
                                       dnn = c("identite", "n_cellules")))
    else NULL
  }, error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NULL })
  fingerprint <- tryCatch(build_object_identity_v2(sc_obj), error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NULL })
  if (is.null(fingerprint)) {
    warns <- c(warns, "Empreinte d'objet indisponible — la vérification d'identité des résultats est dégradée.")
  }
  dataset <- list(
    n_cells = tryCatch(as.integer(ncol(sc_obj)), error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NA_integer_ }),
    n_genes = tryCatch(as.integer(nrow(sc_obj)), error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NA_integer_ }),
    is_seurat = is_seurat,
    assays = tryCatch(as.character(names(sc_obj@assays)), error = function(e) character(0)),
    default_assay = tryCatch(as.character(SeuratObject::DefaultAssay(sc_obj)),
                              error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NA_character_ }),
    reductions = tryCatch(as.character(names(sc_obj@reductions)), error = function(e) character(0)),
    identities = ds_idents,
    metadata_columns = if (!is.null(meta)) colnames(meta) else character(0),
    backend = tryCatch(as.character(sc_backend_status(sc_obj)), error = function(e) "inconnu"),
    fingerprint = fingerprint
  )

  # ── Pipeline (snapshot deja produit — aucune relance) ─────────────────────
  qc_snapshot <- state_get(shared_rv, "qc_snapshot")
  vfeatures <- tryCatch(length(SeuratObject::VariableFeatures(sc_obj)),
                        error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NA_integer_ })
  n_clusters <- tryCatch(
    if (!is.null(meta) && "seurat_clusters" %in% colnames(meta))
      length(levels(factor(meta$seurat_clusters))) else NA_integer_,
    error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NA_integer_ })
  pipeline <- list(
    qc_present = !is.null(qc_snapshot),
    qc_snapshot = qc_snapshot,
    summary = .report_kv_df(c(
      n_cellules = dataset$n_cells,
      n_genes = dataset$n_genes,
      n_genes_variables = vfeatures,
      n_clusters = n_clusters,
      reductions = paste(dataset$reductions, collapse = "; "),
      backend = dataset$backend,
      plafond_marqueurs_corr = state_get(shared_rv, "max_cells_heavy") %||% NA
    ))
  )

  # ── Analyses (resumes descriptifs par domaine fige) ────────────────────────
  analyses <- setNames(lapply(.report_analysis_domains, function(dm) {
    entry <- tryCatch(
      .report_domain_summary(dm, sc_obj, shared_rv, global_data = global_data),
      error = function(e) {
        list(present = TRUE, summary = .report_kv_df(c(
          erreur_resume = conditionMessage(e))),
          analysis_ids = character(0), provenance_available = FALSE,
          identity_checked = FALSE, identity_ok = NA,
          identity_note = "Échec du résumé descriptif (résultat brut conservé dans l'état).",
          extras = list())
      }
    )
    entry$domain <- dm
    entry
  }), .report_analysis_domains)

  # ── Provenance CONSOLIDEE (regle 7 : lecture de ce qui a ete produit) ─────
  provenance_df <- provenance_to_dataframe(shared_rv)

  # ── Script R reproductible (capture a la collection — bundle) ─────────────
  r_script_text <- tryCatch(sc_r_script_text(sc_obj, shared_rv), error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NULL })
  if (is.null(r_script_text)) {
    warns <- c(warns, "Script R reproductible indisponible pour cet objet (il ne figurera pas dans le bundle).")
  }

  # ── Session info ────────────────────────────────────────────────────────────
  pkg_keys <- c("Seurat", "SeuratObject", "miloR", "edgeR", "slingshot",
                "BPCells", "shiny", "shiny.i18n", "ggplot2", "rmarkdown",
                "reticulate", "mirai", "digest")
  pkgs <- vapply(pkg_keys, function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NA_character_ })
  }, character(1))
  pkgs <- pkgs[!is.na(pkgs)]
  session_info <- list(
    r_version = R.version.string,
    platform = R.version$platform,
    locale = Sys.getlocale("LC_COLLATE"),
    key_packages = data.frame(package = names(pkgs), version = unname(pkgs),
                              stringsAsFactors = FALSE),
    full_text = paste(tryCatch(
      utils::capture.output(utils::sessionInfo()), error = function(e) ""), collapse = "\n")
  )

  # ── Snapshot des constantes TS_* (consommees, jamais recalculees) ─────────
  cfg_vals <- vapply(.report_config_keys, function(k) {
    v <- tryCatch(get(k, envir = globalenv()), error = function(e) { ts_log_swallow("report_collector.collect_consolidated_report_input", e); NULL })
    if (is.null(v)) "<absente>" else paste(format(v), collapse = ", ")
  }, character(1))
  config_snapshot <- data.frame(constante = names(cfg_vals), valeur = unname(cfg_vals),
                                stringsAsFactors = FALSE)

  list(
    type = "consolidated_report_input",
    version = "1.0",
    analysis_id = "sc-report-consolide",
    generated_at = Sys.time(),
    dataset = dataset,
    pipeline = pipeline,
    analyses = analyses,
    provenance_df = provenance_df,
    r_script_text = r_script_text,
    session_info = session_info,
    config_snapshot = config_snapshot,
    options = opts,
    warnings = warns
  )
}

#' Domaines suivis par le rapport (liste figée)
consolidated_report_analyses <- function() .report_analysis_domains

#' Empreinte textuelle d'une liste d'entrée de rapport (diagnostic/exports)
consolidated_report_input_recap <- function(report_input) {
  if (!is.list(report_input) || !identical(report_input$type, "consolidated_report_input")) {
    .rep_stop("consolidated_report_input_recap() : attend une entrée de collect_consolidated_report_input().")
  }
  present <- vapply(report_input$analyses, function(a) isTRUE(a$present), logical(1))
  sprintf(
    paste0("Rapport consolidé %s (analysis_id : %s) — objet : %d cellules × %d ",
           "gènes ; analyses présentes : %s ; entrées de provenance : %d."),
    format(report_input$generated_at, "%Y-%m-%d %H:%M:%S"),
    report_input$analysis_id,
    report_input$dataset$n_cells, report_input$dataset$n_genes,
    if (any(present)) paste(names(present)[present], collapse = ", ") else "aucune",
    nrow(report_input$provenance_df)
  )
}

report_public_api <- function() {
  list(
    contract = "docs/contracts/CONSOLIDATED_REPORT_CONTRACT.md",
    functions = c(
      "report_public_api", "collect_consolidated_report_input",
      "consolidated_report_analyses", "consolidated_report_input_recap",
      "validate_consolidated_report_input",
      "build_consolidated_report_html", "write_consolidated_report_html",
      "build_report_bundle", "consolidated_report_export_filename",
      "consolidated_report_validation_states"
    )
  )
}
