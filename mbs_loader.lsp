;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - Module Loader
;; VERSION 5.5
;;; ========================================================================
;; This module loads all the other modules in the correct order.
;;; ========================================================================

;; Set global version
(setq MBS:version "5.5")

;; Function to load a module file
(defun MBS:load-module (module-name / file-path)
  (setq file-path (findfile module-name))
  (if file-path
    (progn
      (prompt (strcat "\nLoading " module-name "..."))
      (load file-path)
      T
    )
    (progn
      (prompt (strcat "\nModule not found: " module-name))
      nil
    )
  )
)

;; Load modules in the correct order
(MBS:load-module "mbs_core.lsp")        ;; Core functionality (no dependencies)
(MBS:load-module "mbs_debug.lsp") 		;; Debug functionality (depends on core)
(MBS:load-module "mbs_config.lsp")      ;; Configuration (depends on core)
(MBS:load-module "mbs_csv.lsp")         ;; CSV handling (depends on core and config)
(MBS:load-module "mbs_transaction.lsp") ;; Transaction system (depends on core)
(MBS:load-module "mbs_blocks.lsp")      ;; Block operations (depends on core, config, csv, transaction)
(MBS:load-module "mbs_dialog.lsp")      ;; Dialog interface (depends on everything above)

;; Display initialization complete message
(prompt (strcat "\n\nMaterial Block Sync v" MBS:version " initialization complete."))
(prompt "\nType MSYNC to display the menu.")
(princ)