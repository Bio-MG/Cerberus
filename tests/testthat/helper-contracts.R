# =============================================================================
# helper-contracts.R — garde §7 pour les lectures de contrats gelés
# =============================================================================
# POURQUOI CE GARDE EXISTE (audit 2026-10-02, décision §7 confirmée par
# l'utilisateur) : `docs/` n'est PAS suivi dans git (docs/STATUS.md §7 — les
# documents restent machine-locaux, jamais poussés). Sur un checkout frais, les
# contrats de docs/contracts/ sont absents et un `readLines()` non gardé lève
# une ERREUR DE FICHIER (0 test exécuté dans le fichier) — exactement l'incident
# type documenté pour test-sc-da-cross-views (2026-10-01).
#
# Ce garde convertit cette situation en SKIP EXPLICITE — jamais en erreur de
# fichier, jamais en pass silencieux (le piège « C8 » de l'audit : une règle
# qui passe quand son sujet est absent). Le message nomme §7 et le fichier
# manquant, pour qu'un `grep -i skip` dans la sortie de la suite raconte toute
# l'histoire sans relire le code.
#
# CÔTÉ MACHINE DE L'OPÉRATEUR (docs/contracts/ présent), le garde ne change
# RIEN : tous les tests de gel s'exécutent normalement.
# =============================================================================

.ts_contract_skip_msg <- function(path) {
  sprintf("Contrat absent de l'arbre : %s \u2014 politique docs/STATUS.md \u00a77 (docs/ non suivi sur GitHub) ; test de gel inapplicable sur un checkout frais",
          path)
}

# Lecteur gardé : drop-in pour readLines(path, ...) sur un contrat.
.ts_contract_readlines <- function(path, ...) {
  if (!file.exists(path)) {
    skip(.ts_contract_skip_msg(path))
  }
  readLines(path, ...)
}
