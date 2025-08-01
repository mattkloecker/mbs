;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - Mode Configuration Module
;; VERSION 5.5
;;; ========================================================================
;; This module contains mode selection and configuration functions.
;; Dependencies: mbs_core.lsp
;;; ========================================================================

;;; ========================================================================
;; MODE SELECTION AND CONFIG FUNCTIONS 
;;; ========================================================================

;; Global variable for auto-accepting item number changes
(if (not (boundp 'auto-accept-item-number-changes))
  (setq auto-accept-item-number-changes nil)  ;; Default to OFF
)

;; Global mode variable - LOM or LOR
(if (not (boundp 'current-mode))
  (setq current-mode "LOM")  ;; Default to LOM mode
)

;; Global variables
(if (not (boundp 'csv-sync-file))
  (setq csv-sync-file nil)
)

;; Global variable for prefix mode
(if (not (boundp 'prefix-mode))
  (setq prefix-mode "AUTO")  ;; Default to AUTO mode
)

;; Global variable for ID attribute
(if (not (boundp 'id-attribute))
  (setq id-attribute "MATERIAL_ID")  ;; Default for LOM mode
)

;; Function to toggle auto-update
(defun toggle-auto-update ()
  (setq auto-update-after-insert (not auto-update-after-insert))
  (prompt (strcat "\nAuto-update after insert is now " (if auto-update-after-insert "ON" "OFF")))
)

;; Function to toggle prefix mode
(defun toggle-prefix-mode ()
  (setq prefix-mode (if (= prefix-mode "AUTO") "MANUAL" "AUTO"))
  (prompt (strcat "\nPrefix mode set to: " prefix-mode))
)

(if (not (boundp 'auto-update-after-insert))
  (setq auto-update-after-insert nil)
)

;; Function to toggle between LOM and LOR modes
(defun toggle-mode ()
  (if (= current-mode "LOM")
    (progn
      (setq current-mode "LOR")
      (setq id-attribute "REMOVAL_ID")
    )
    (progn
      (setq current-mode "LOM")
      (setq id-attribute "MATERIAL_ID")
    )
  )
  (prompt (strcat "\nSwitched to " current-mode " mode"))
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

;;; ========================================================================
;; PREFIX MAPPING SYSTEM
;;; ========================================================================

;; Global variable for drawing type
(if (not (boundp 'current-drawing-type))
  (setq current-drawing-type "P")  ;; Default to type P
)

;; Drawing Type P prefix map (MECHANICAL (PIPING))
(setq prefix-keyword-map-P
  '(
    ;; Pipe related keywords
    ("PIPE" . "P")
    ("PIPING" . "P")  
    ("TUBING" . "P")
    
    ;; Valve related keywords
    ("VALVE" . "V")
    ("BALL" . "V")
    ("GATE" . "V")
    ("CHECK" . "V")
    ("BUTTERFLY" . "V")
    
    ;; Fitting related keywords
    ("FITTING" . "F")
    ("ELBOW" . "F")
    ("TEE" . "F")
    ("REDUCER" . "F")
    ("UNION" . "F")
    ("COUPLING" . "F")
    ("FLANGE" . "F")
    ("ADAPTER" . "F")
    ("HOSE ASSEMBLY" . "F")
    ("JOINT" . "F")
    ("NIPPLE" . "F")
    ("WELDOLET" . "F")
    ("ELBOLET" . "F")
    ("SLEEVE" . "F")
    ("CAP" . "F")
    ("PLUG"  . "F")
    ("BUSHING" . "F")
    ("GASKET" . "F")
    ("HOSE"  . "F")
  )
)

;; Drawing Type E prefix map (ELECTRICAL)
(setq prefix-keyword-map-E
  '(
    ;; MODULES
    ("ASSEMBLY" . "A")
    ("EQUIPMENT" . "E")  
  )
)

;; Drawing Type S prefix map (STRUCTURAL)
(setq prefix-keyword-map-S
  '(
    ;; MODULES
    ("ASSEMBLY" . "A")
    ("EQUIPMENT" . "E")  
  )
)

;; Drawing Type A prefix map (ARRANGEMENT)
(setq prefix-keyword-map-A
  '(
    ;; MODULES
    ("ASSEMBLY" . "A")
    ("EQUIPMENT" . "E")  
  )
)

;; Drawing Type H prefix map (ARRANGEMENT)
(setq prefix-keyword-map-H
  '(
    ;; MODULES
    ("SHEET METAL" . "S")
    ("SHEET" . "S")
    ("PLATE" . "S")
    ("ANGLE" . "S")
  )
)

;; Function to get the current prefix map based on drawing type
(defun get-current-prefix-map ()
  (cond
    ((= current-drawing-type "P") prefix-keyword-map-P)
    ((= current-drawing-type "E") prefix-keyword-map-E)
    ((= current-drawing-type "S") prefix-keyword-map-S)
    ((= current-drawing-type "A") prefix-keyword-map-A)
    ((= current-drawing-type "H") prefix-keyword-map-H)
    (t prefix-keyword-map-A) ;; Default to A if unknown
  )
)

;; Function to toggle drawing type
(defun c:MBS-ToggleDrawingType ()
  (initget "P E S A H")
  (setq new-type (getkword "\nSelect drawing type [P/E/S/A/H]: "))
  
  (if new-type
    (progn
      (setq current-drawing-type new-type)
      (prompt (strcat "\nDrawing type set to: " current-drawing-type))
      (prompt (strcat "\nUsing " 
                     (cond 
                       ((= current-drawing-type "P") "Piping")
                       ((= current-drawing-type "E") "Electrical")
                       ((= current-drawing-type "S") "Structural")
                       ((= current-drawing-type "A") "Arrangement")
                       ((= current-drawing-type "H") "HVAC")
                     ) 
                     " prefix mapping"))
    )
    (prompt "\nInvalid input. Drawing type unchanged.")
  )
  (princ)
)

;; Enhanced function to detect prefix from description - only looking before first comma
(defun detect-prefix-from-description (description / description-upper keyword-part keyword pair prefix-map)
  (if (or (not description) (= description ""))
    ""  ;; Return empty string for nil/empty descriptions
    (progn
      ;; Extract only text before first comma
      (setq comma-pos (vl-string-search "," description))
      (if comma-pos
        (setq keyword-part (substr description 1 comma-pos))
        (setq keyword-part description)
      )
      
      ;; Convert to uppercase for case-insensitive matching
      (setq description-upper (strcase keyword-part))
      
      ;; Get the appropriate prefix map based on drawing type
      (setq prefix-map (get-current-prefix-map))
      
      ;; Search for keywords in the description
      (setq found-prefix nil)
      (foreach pair prefix-map
        (setq keyword (car pair))
        (if (and (not found-prefix) 
                 (vl-string-search keyword description-upper))
          (progn
            (setq found-prefix (cdr pair))
          )
        )
      )
      
      ;; Return the found prefix or empty string for regular items
      (if found-prefix
        found-prefix
        ""  ;; Empty prefix for regular items
      )
    )
  )
)

;; Initialize id-attribute based on current mode
(setq id-attribute (get-id-attribute))

(prompt "\nMaterial Block Sync Config Module loaded.")
(princ)