;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - Configuration Module
;; VERSION 5.5
;;; ========================================================================
;; This module centralizes all configuration settings and provides validation
;; functions to ensure operations succeed.
;;; ========================================================================

;;; ========================================================================
;; CONFIGURATION VALIDATION FUNCTIONS
;;; ========================================================================

;; Validate CSV structure before processing
(defun MBS:validate-csv-structure (csv-path / csv-data header records validation-result)
  (setq validation-result (list t ""))  ;; (is-valid error-message)
  
  ;; Read CSV data
  (setq csv-data (MBS:read-csv csv-path))
  (if (not csv-data)
    (setq validation-result (list nil "Failed to read CSV file"))
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Check if header exists and has required columns
      (if (not header)
        (setq validation-result (list nil "CSV file has no header row"))
        (progn
          ;; Check for required columns
          (setq required-columns (list "ITEM NO." "DESCRIPTION" (strcase id-attribute)))
          (setq missing-columns '())
          
          (foreach col required-columns
            (if (not (get-column-index header col))
              (setq missing-columns (append missing-columns (list col)))
            )
          )
          
          (if missing-columns
            (setq validation-result (list nil (strcat "Missing required columns: " (vl-princ-to-string missing-columns))))
            ;; Check if we have any data rows
            (if (= (length records) 0)
              (setq validation-result (list nil "CSV file has no data rows"))
              (setq validation-result (list t "CSV structure is valid"))
            )
          )
        )
      )
    )
  )
  
  validation-result
)
  
    (MBS:log-info "\n✓ Validate csv loaded successfully")


;; Validate block structure before processing
(defun MBS:validate-block-structure (block / att-list required-attrs missing-attrs)
  (setq required-attrs (list "##" "DESCRIPTION" (strcase id-attribute)))
  (setq missing-attrs '())
  
  ;; Get block attributes
  (setq att-list (MBS:get-block-attributes-safe block))
  (if (not att-list)
    (list nil "Failed to get block attributes")
    (progn
      ;; Check for required attributes
      (foreach req-attr required-attrs
        (setq found nil)
        (foreach att att-list
          (if (= (strcase (vlax-get att 'TagString)) (strcase req-attr))
            (setq found t)
          )
        )
        (if (not found)
          (setq missing-attrs (append missing-attrs (list req-attr)))
        )
      )
      
      (if missing-attrs
        (list nil (strcat "Missing required attributes: " (vl-princ-to-string missing-attrs)))
        (list t "Block structure is valid")
      )
    )
  )
)

    (MBS:log-info "\n✓ Validate block structure successfully")

;; Validate transaction queue before execution
(defun MBS:validate-transaction-queue (/ valid-count invalid-count)
  (setq valid-count 0)
  (setq invalid-count 0)
  
  (if (not (boundp 'MBS:transaction-queue))
    (list nil "No transaction queue found")
    (progn
      (foreach tx MBS:transaction-queue
        (if (and (listp tx) (> (length tx) 0))
          (setq valid-count (1+ valid-count))
          (setq invalid-count (1+ invalid-count))
        )
      )
      
      (if (> invalid-count 0)
        (list nil (strcat "Found " (itoa invalid-count) " invalid transactions"))
        (list t (strcat "All " (itoa valid-count) " transactions are valid"))
      )
    )
  )
)

    (MBS:log-info "\n✓ Validate structure successfully")

;;; ========================================================================
;; ENHANCED CONFIGURATION SETTINGS
;;; ========================================================================

;; Initialize enhanced configuration with validation
(defun MBS:initialize-configuration ()
  ;; Core settings
  (if (not (boundp 'current-mode))          (setq current-mode "LOM"))
  (if (not (boundp 'prefix-mode))           (setq prefix-mode "AUTO"))
  (if (not (boundp 'description-comma-limit)) (setq description-comma-limit 2))
  (if (not (boundp 'auto-update-after-insert)) (setq auto-update-after-insert T))
  (if (not (boundp 'total-weight-calculation)) (setq total-weight-calculation T))
  (if (not (boundp 'auto-increment-item-numbers)) (setq auto-increment-item-numbers T))
  (if (not (boundp 'current-drawing-type))  (setq current-drawing-type "P"))
  (if (not (boundp 'auto-accept-item-number-changes)) (setq auto-accept-item-number-changes T))
  
  ;; Error handling settings
  (if (not (boundp 'MBS:error-recovery-mode)) (setq MBS:error-recovery-mode "prompt"))
  (if (not (boundp 'MBS:max-retry-attempts)) (setq MBS:max-retry-attempts 3))
  (if (not (boundp 'MBS:backup-before-operations)) (setq MBS:backup-before-operations T))
  
  ;; File locking settings
  (if (not (boundp 'MBS:file-lock-timeout)) (setq MBS:file-lock-timeout 30))
  (if (not (boundp 'MBS:enable-file-lock-detection)) (setq MBS:enable-file-lock-detection T))
  (if (not (boundp 'MBS:file-operation-retry-delay)) (setq MBS:file-operation-retry-delay 200))
  
  ;; Performance settings
  (if (not (boundp 'MBS:batch-size)) (setq MBS:batch-size 50))
  (if (not (boundp 'MBS:enable-caching)) (setq MBS:enable-caching T))
  
  ;; User interface settings
  (if (not (boundp 'MBS:show-location-info)) (setq MBS:show-location-info T))
  (if (not (boundp 'MBS:show-progress-indicators)) (setq MBS:show-progress-indicators T))
  (if (not (boundp 'MBS:detailed-error-messages)) (setq MBS:detailed-error-messages T))
  
      (MBS:log-info "\n✓ Configuration initialized successfully")
)

    (MBS:log-info "\n✓ init config loaded successfully")

;; Get configuration value with validation
(defun MBS:get-config (key default-value)
  (if (boundp key)
    (eval key)
    default-value
  )
)

    (MBS:log-info "\n✓ MBS:get-config loaded successfully")

;; Set configuration value with validation
(defun MBS:set-config (key value / valid-types)
  (setq valid-types (list 'STR 'INT 'REAL 'SYM 'LIST))
  
  (if (member (type value) valid-types)
    (progn
      (set key value)
      t
    )
    (progn
      (MBS:log-warning (strcat "\n⚠ Invalid value type for configuration key: " (vl-princ-to-string key)))
      nil
    )
  )
)

    (MBS:log-info "\n✓ MBS:set-config loaded successfully")

;; Validate all configuration settings
(defun MBS:validate-configuration (/ validation-errors)
  (setq validation-errors '())
  
  ;; Check required settings
  (if (not (boundp 'current-mode))
    (setq validation-errors (append validation-errors (list "current-mode not set")))
  )
  
  ;; id-attribute is now auto-initialized in mbs_core.lsp based on current-mode
  ;; No longer need to validate its existence
  
  ;; Check setting ranges
  (if (and (boundp 'description-comma-limit) (< description-comma-limit 0))
    (setq validation-errors (append validation-errors (list "description-comma-limit must be >= 0")))
  )
  
  (if (and (boundp 'MBS:max-retry-attempts) (< MBS:max-retry-attempts 1))
    (setq validation-errors (append validation-errors (list "max-retry-attempts must be >= 1")))
  )
  
  (if (and (boundp 'MBS:batch-size) (< MBS:batch-size 1))
    (setq validation-errors (append validation-errors (list "batch-size must be >= 1")))
  )
  
  ;; Return validation result
  (if validation-errors
    (list nil validation-errors)
    (list t "Configuration is valid")
  )
)

;;; ========================================================================
;; CONFIGURATION PERSISTENCE
;;; ========================================================================

;; Save configuration to file
(defun MBS:save-configuration (file-path / f)
  (setq f (open file-path "w"))
  (if f
    (progn
      ;; Write configuration as LISP expressions
      (write-line "(setq current-mode \"" f)
      (write-line (strcat current-mode "\")") f)
      (write-line "(setq prefix-mode \"" f)
      (write-line (strcat prefix-mode "\")") f)
      ;; Add more configuration settings as needed
      (close f)
      t
    )
    nil
  )
)

;; Load configuration from file
(defun MBS:load-configuration (file-path / f line)
  (if (findfile file-path)
    (progn
      (setq f (open file-path "r"))
      (if f
        (progn
          (while (setq line (read-line f))
            (if (and line (> (strlen line) 0))
              (vl-catch-all-apply '(lambda () (eval (read line))))
            )
          )
          (close f)
          t
        )
        nil
      )
    )
    nil
  )
)

;;; ========================================================================
;; INITIALIZATION
;;; ========================================================================

;; Initialize configuration when module is loaded
(MBS:initialize-configuration)

;; Validate configuration
(setq config-validation (MBS:validate-configuration))
(if (not (car config-validation))
  (progn
    (MBS:log-warning "\n⚠ Configuration validation failed:")
    (foreach error (cadr config-validation)
      (MBS:log-warning (strcat "\n  - " error))
    )
  )
  (MBS:log-info "\n✓ Configuration validation passed")
)

;; Function to toggle between LOM and LOR modes
(defun toggle-mode ()
  (if (= current-mode "LOM")
    (setq current-mode "LOR")
    (setq current-mode "LOM")
  )
      (MBS:log-info (strcat "\nSwitched to " current-mode " mode"))
)

;; Configuration based on current mode
(defun get-mode-config ( / config)
  (if (= current-mode "LOM")
    ;; LOM Configuration
    (setq config '(
      ("BLOCK_NAME" . "_CACI_ITEMLEADER")
      ("ID_ATTRIBUTE" . "MATERIAL_ID")
      ("PATH_ATTRIBUTE" . "CSVFILEPATH")
      ("REL_PATH_ATTRIBUTE" . "CSVRELFILEPATH")
      ("ID_COLUMN" . "MATERIAL_ID")
      ("WEIGHT_COLUMN" . "TOTAL WT")
      ("UNIT_WEIGHT_COLUMN" . "UNIT WT")
      ("PREFIX_ENABLED" . T)
      ("FIXED_PREFIX" . "")
    ))
    ;; LOR Configuration
    (setq config '(
      ("BLOCK_NAME" . "_CACI_REMOVALLEADER")
      ("ID_ATTRIBUTE" . "REMOVAL_ID")
      ("PATH_ATTRIBUTE" . "CSVLORFILEPATH")
      ("REL_PATH_ATTRIBUTE" . "CSVLORRELFILEPATH")
      ("ID_COLUMN" . "REMOVAL_ID")
      ("WEIGHT_COLUMN" . "TOTAL WT")
      ("UNIT_WEIGHT_COLUMN" . "UNIT WT")
      ("PREFIX_ENABLED" . T)
      ("FIXED_PREFIX" . "R-")
    ))
  )
  config  ;; Return the config
)

;; Helper functions to get specific config values
(defun get-block-name ()
  (cdr (assoc "BLOCK_NAME" (get-mode-config)))
)

(defun get-id-attribute ()
  (cdr (assoc "ID_ATTRIBUTE" (get-mode-config)))
)

(defun get-path-attribute ()
  (cdr (assoc "PATH_ATTRIBUTE" (get-mode-config)))
)

(defun get-rel-path-attribute ()
  (cdr (assoc "REL_PATH_ATTRIBUTE" (get-mode-config)))
)

(defun get-id-column ()
  (cdr (assoc "ID_COLUMN" (get-mode-config)))
)

(defun get-weight-column ()
  (cdr (assoc "WEIGHT_COLUMN" (get-mode-config)))
)

(defun get-unit-weight-column ()
  (cdr (assoc "UNIT_WEIGHT_COLUMN" (get-mode-config)))
)

(defun is-prefix-enabled ()
  (cdr (assoc "PREFIX_ENABLED" (get-mode-config)))
)

(defun get-fixed-prefix ()
  (cdr (assoc "FIXED_PREFIX" (get-mode-config)))
)

(princ "\nMaterial Block Sync Configuration module loaded.")
(princ)