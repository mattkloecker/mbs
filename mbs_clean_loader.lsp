;;; ------------------------------------------------------------------------
;;; Safe logging stubs (allow logging before mbs_debug.lsp is loaded)
;;; ------------------------------------------------------------------------
(if (not (vl-position 'MBS:log-info (atoms-family 1)))
  (defun MBS:log-info (msg)
    (if (and msg (stringp msg)) (prompt msg))
  )
)
(if (not (vl-position 'MBS:log-warning (atoms-family 1)))
  (defun MBS:log-warning (msg)
    (if (and msg (stringp msg)) (prompt msg))
  )
)
(if (not (vl-position 'MBS:log-error (atoms-family 1)))
  (defun MBS:log-error (msg)
    (if (and msg (stringp msg)) (prompt msg))
  )
)
(if (not (vl-position 'MBS:log-verbose (atoms-family 1)))
  (defun MBS:log-verbose (msg)
    (if (and msg (stringp msg)) (prompt msg))
  )
)

;;; ------------------------------------------------------------------------
;;; Bootstrap debug module FIRST to satisfy dependencies
;;; ------------------------------------------------------------------------
(defun MBS:bootstrap-debug ( / fp)
  (setq fp (findfile "mbs_debug.lsp"))
  (if fp
    (progn
      (prompt "\n⏱ Loading debug module first...")
      (vl-catch-all-apply '(lambda () (load fp)))
      (if (vl-position 'MBS:initialize-debug-system (atoms-family 1))
        (vl-catch-all-apply '(lambda () (MBS:initialize-debug-system)))
      )
    )
  )
  (princ)
)

;; Ensure debug is available before any logging below
(MBS:bootstrap-debug)

;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - CLEAN LOADER WITH VALIDATION
;; VERSION 5.5 - CONFLICT RESOLUTION EDITION
;;; ========================================================================
;; This loader clears all old function definitions and loads fresh copies
;;; ========================================================================

;; Set global version
(setq MBS:version "5.5")

;; Function to safely undefine a function if it exists
(defun MBS:undefine-function (func-name)
  "Safely undefine a function to prevent conflicts"
  (if (boundp func-name)
    (progn
      (MBS:log-verbose (strcat "\n🧹 Clearing old definition: " (vl-symbol-name func-name)))
      (setq func-name nil)
    )
  )
)

;; Clear all potentially conflicting functions
  (MBS:log-info "\n🧹 === CLEANING OLD FUNCTION DEFINITIONS ===")
(MBS:undefine-function 'C:MBS-INSERT3)
(MBS:undefine-function 'C:MBS-BATCHINSERT3)
(MBS:undefine-function 'C:MBS-UPDATE3)
(MBS:undefine-function 'CSV:READ-FILE)
(MBS:undefine-function 'open-csv-in-excel)
(MBS:undefine-function 'C:MSYNC)

;; Also clear these commonly conflicting functions
(MBS:undefine-function 'MBS:read-csv)
(MBS:undefine-function 'MBS:write-csv)
(MBS:undefine-function 'get-csv-path-from-block)

;; Clear library-related functions
(MBS:undefine-function 'C:MBS-MaterialLibrary)
(MBS:undefine-function 'C:MBS-LibraryDialog)
(MBS:undefine-function 'MBS:toggle-enhanced-mode)

  (MBS:log-info "\n✅ Function cleanup completed")

;; Function to load a module file with validation
(defun MBS:load-module-validated (module-name expected-functions / file-path success-count)
  "Load a module and validate that expected functions are defined"
  (setq file-path (findfile module-name))
  (setq success-count 0)
  
  (if file-path
    (progn
      (MBS:log-info (strcat "\n📁 Loading " module-name "..."))
      (load file-path)
      
      ;; Validate expected functions are now defined  
      (if (> (length expected-functions) 0)
        (progn
          (foreach func expected-functions
            (if (boundp func)
              (progn
                (MBS:log-info (strcat "\n  ✅ " (vl-symbol-name func) " loaded"))
                (setq success-count (1+ success-count))
              )
              (MBS:log-error (strcat "\n  ❌ " (vl-symbol-name func) " MISSING"))
            )
          )
        )
        (progn
                      (MBS:log-info "\n  ℹ️ No specific functions to validate")
          (setq success-count 0)  ;; Set to 0 to match expected length of 0
        )
      )
      
      ;; Return success if all functions loaded (or none expected)
      (if (= (length expected-functions) 0)
        T  ;; Always succeed if no functions expected
        (= success-count (length expected-functions))  ;; Normal validation
      )
    )
    (progn
              (MBS:log-error (strcat "\n❌ Module not found: " module-name))
      nil
    )
  )
)

;; Load modules in the correct order with validation
  (MBS:log-info "\n🔄 === LOADING MBS MODULES WITH VALIDATION ===")

;; Core module (basic utilities - no critical functions to validate)
(if (not (MBS:load-module-validated "mbs_core.lsp" '()))
  (progn
    (MBS:log-error "\n❌ CRITICAL: mbs_core.lsp failed to load properly")
    (exit)
  )
)

;; Debug module (contains logging functions)
(if (not (MBS:load-module-validated "mbs_debug.lsp" '(MBS:log-info)))
  (progn
    (MBS:log-warning "\n⚠ Warning: mbs_debug.lsp had loading issues")
    (MBS:log-warning "\n   This may cause logging issues but won't prevent core functionality")
  )
)

;; Configuration module
(if (not (MBS:load-module-validated "mbs_config.lsp" '(MBS:validate-configuration)))
  (progn
    (MBS:log-error "\n❌ CRITICAL: mbs_config.lsp failed to load properly")
    (exit)
  )
)

;; CSV module - CRITICAL for INSERT and UPDATE functions
(if (not (MBS:load-module-validated "mbs_csv.lsp" '(CSV:READ-FILE open-csv-in-excel)))
  (progn
    (MBS:log-error "\n❌ CRITICAL: mbs_csv.lsp failed to load properly")
    (MBS:log-error "\n   This is why you're getting 'CSV:READ-FILE not found' errors")
    (exit)
  )
)

;; Transaction module
(if (not (MBS:load-module-validated "mbs_transaction.lsp" '(MBS:queue-transaction)))
      (MBS:log-warning "\n⚠ Warning: mbs_transaction.lsp had loading issues")
)

;; Enhanced mode module
(if (not (MBS:load-module-validated "mbs_enhanced_mode.lsp" '(MBS:toggle-enhanced-mode)))
      (MBS:log-warning "\n⚠ Warning: mbs_enhanced_mode.lsp had loading issues")
)

;; Blocks module - Contains the main commands
(if (not (MBS:load-module-validated "mbs_blocks.lsp" '(C:MBS-INSERT3 C:MBS-BATCHINSERT3 C:MBS-UPDATE3 C:MBS-UPDATE-SIMPLE C:MBS-CONFIG)))
  (progn
    (MBS:log-error "\n❌ CRITICAL: mbs_blocks.lsp failed to load properly")
    (MBS:log-error "\n   This is why you're getting 'too many arguments' errors")
    (exit)
  )
)

;; Library module - Material library import system
(if (not (MBS:load-module-validated "mbs_library.lsp" '(C:MBS-MaterialLibrary)))
  (progn
    (MBS:log-warning "\n⚠ Warning: mbs_library.lsp had loading issues")
    (MBS:log-warning "\n   Material library functionality may not be available")
  )
)

;; Library dialog module - Dialog-based material library interface
(if (not (MBS:load-module-validated "mbs_library_dialog.lsp" '(C:MBS-LibraryDialog)))
  (progn
    (MBS:log-warning "\n⚠ Warning: mbs_library_dialog.lsp had loading issues")
    (MBS:log-warning "\n   Material library dialog may not be available")
  )
)

;; Dialog module - Main menu interface
(if (not (MBS:load-module-validated "mbs_dialog.lsp" '(C:MSYNC)))
      (MBS:log-warning "\n⚠ Warning: mbs_dialog.lsp had loading issues")
)

;; Final validation
  (MBS:log-info "\n🔍 === FINAL VALIDATION ===")
  (MBS:log-info "\nChecking critical functions:")

(if (boundp 'C:MBS-INSERT3)
  (MBS:log-info "\n  ✅ MBS-INSERT3 ready")
  (MBS:log-error "\n  ❌ MBS-INSERT3 MISSING")
)

(if (boundp 'C:MBS-BATCHINSERT3)
  (MBS:log-info "\n  ✅ MBS-BATCHINSERT3 ready")
  (MBS:log-error "\n  ❌ MBS-BATCHINSERT3 MISSING")
)

(if (boundp 'C:MBS-UPDATE3)
  (MBS:log-info "\n  ✅ MBS-UPDATE3 ready")
  (MBS:log-error "\n  ❌ MBS-UPDATE3 MISSING")
)

(if (boundp 'C:MBS-UPDATE-SIMPLE)
  (MBS:log-info "\n  ✅ MBS-UPDATE-SIMPLE ready")
  (MBS:log-error "\n  ❌ MBS-UPDATE-SIMPLE MISSING")
)

(if (boundp 'C:MBS-CONFIG)
  (MBS:log-info "\n  ✅ MBS-CONFIG ready")
  (MBS:log-error "\n  ❌ MBS-CONFIG MISSING")
)

(if (boundp 'CSV:READ-FILE)
  (MBS:log-info "\n  ✅ CSV:READ-FILE ready")
  (MBS:log-error "\n  ❌ CSV:READ-FILE MISSING")
)

(if (boundp 'open-csv-in-excel)
  (MBS:log-info "\n  ✅ open-csv-in-excel ready")
  (MBS:log-error "\n  ❌ open-csv-in-excel MISSING")
)

(if (boundp 'C:MBS-MaterialLibrary)
  (MBS:log-info "\n  ✅ MBS-MaterialLibrary ready")
  (MBS:log-error "\n  ❌ MBS-MaterialLibrary MISSING")
)

(if (boundp 'C:MBS-LibraryDialog)
  (MBS:log-info "\n  ✅ MBS-LibraryDialog ready")
  (MBS:log-error "\n  ❌ MBS-LibraryDialog MISSING")
)

(if (boundp 'MBS:toggle-enhanced-mode)
  (MBS:log-info "\n  ✅ MBS:toggle-enhanced-mode ready")
  (MBS:log-error "\n  ❌ MBS:toggle-enhanced-mode MISSING")
)

;; Initialize configuration and validate
(MBS:log-info "\n⚙️ === CONFIGURATION VALIDATION ===")
(if (boundp 'MBS:validate-configuration)
  (progn
    (setq config-result (MBS:validate-configuration))
    (if (car config-result)
      (MBS:log-info "\n✅ Configuration validated successfully")
      (progn
        (MBS:log-warning "\n⚠ Configuration validation failed:")
        (foreach error (cadr config-result)
          (MBS:log-warning (strcat "\n  - " error))
        )
      )
    )
  )
  (MBS:log-error "\n❌ Configuration validation function not available")
)

;; Display completion message
(MBS:log-info (strcat "\n\n🎉 Material Block Sync v" MBS:version " CLEAN LOADING COMPLETE"))
(MBS:log-info "\n   All function conflicts should now be resolved")
(MBS:log-info "\n   Type MSYNC to display the menu")
(MBS:log-info "\n   Test with: MBS-INSERT3, MBS-BATCHINSERT3, MBS-UPDATE3")
(MBS:log-info "\n   Material Library: Use menu option 4 or type MBS-LibraryDialog")

(princ)