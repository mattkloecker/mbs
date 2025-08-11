;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - Dialog Interface Module
;; VERSION 5.5
;;; ========================================================================
;; This module implements a dialog-based menu system for Material Block Sync
;; with compatibility with the text-based menu.
;; Dependencies: mbs_core.lsp, mbs_config.lsp, mbs_csv.lsp
;;; ========================================================================

;; Clear any existing value to ensure loading happens
(setq MBS:dialog-loaded nil)

;; ========================================================================
;; SETTINGS LOADING FUNCTION - Global scope
;; ========================================================================

;; Load settings from attributes on _MBS_SETTINGS_MARKER block
(defun MBS:load-settings-from-drawing (/ doc success settings-string settings-list setting-pair marker-block)
  "Load settings from attributes on _MBS_SETTINGS_MARKER block"
  (MBS:log-info "\n🔧 === LOADING SETTINGS FROM DRAWING ===")
  (setq success nil)
  
  (vl-catch-all-apply
    '(lambda ()
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq modelspace (vla-get-ModelSpace doc))
      
      ;; First try to find settings marker block
      (setq marker-block nil)
      (vlax-for ent modelspace
        (if (and (= (vla-get-ObjectName ent) "AcDbBlockReference")
                 (= (vlax-get ent 'EffectiveName) "_MBS_SETTINGS_MARKER"))
          (setq marker-block (vlax-vla-object->ename ent))
        )
      )
      
      ;; Try to load from attributes first
      (if marker-block
        (progn
          (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke (vlax-ename->vla-object marker-block) 'GetAttributes))))
          
          (if (and (not (vl-catch-all-error-p att-list)) (> (length att-list) 0))
            (progn
              ;; Load settings from attributes
              (foreach att att-list
                (setq tag (strcase (vlax-get att 'TagString)))
                (setq value (vlax-get att 'TextString))
                
                (cond
                  ((= tag "AUTO_ACCEPT")
                   (setq auto-accept-item-number-changes (= value "1"))
                   (if (boundp 'MBS:auto-accept-item-number-changes)
                     (setq MBS:auto-accept-item-number-changes auto-accept-item-number-changes)
                   ))
                  ((= tag "AUTO_NUMBER")
                   (setq auto-increment-item-numbers (= value "1"))
                   (if (boundp 'MBS:enable-auto-increment-item-numbers)
                     (setq MBS:enable-auto-increment-item-numbers auto-increment-item-numbers)
                   ))
                  ((= tag "AUTO_WEIGHT")
                   (setq total-weight-calculation (= value "1"))
                   (if (boundp 'MBS:enable-weight-calculation)
                     (setq MBS:enable-weight-calculation total-weight-calculation)
                   ))
                  ((= tag "AUTO_UPDATE")
                   (setq auto-update-after-insert (= value "1")))
                  ((= tag "USE_TRANSACTIONS")
                   (setq use-two-stage-transactions (= value "1")))
                  ((= tag "PREFIX_CONTROL")
                   (setq prefix-mode value))
                  ((= tag "DWG_TYPE")
                   (setq current-drawing-type value))
                  ((= tag "DESCRIPTION_SHORTENING")
                   (setq description-comma-limit (atoi value)))
                )
              )
              (MBS:log-info "\n✅ Settings loaded from attributes")
              (MBS:log-info "\n📊 Loaded settings summary:")
              (MBS:log-info (strcat "\n  Description comma limit: " (itoa description-comma-limit)))
              (MBS:log-info (strcat "\n  Auto-update after insert: " (if auto-update-after-insert "ON" "OFF")))
              (MBS:log-info (strcat "\n  Total weight calculation: " (if total-weight-calculation "ON" "OFF")))
              (MBS:log-info (strcat "\n  Auto increment item numbers: " (if auto-increment-item-numbers "ON" "OFF")))
              (MBS:log-info (strcat "\n  Auto accept item number changes: " (if auto-accept-item-number-changes "ON" "OFF")))
              (MBS:log-info (strcat "\n  Prefix mode: " prefix-mode))
              (MBS:log-info (strcat "\n  Drawing type: " current-drawing-type))
              (setq success t)
            )
            ;; Fallback to XDATA if attributes fail
            (progn
              (setq entdata (entget marker-block '("MBS_SETTINGS")))
              (setq xdata (assoc -3 entdata))
            )
          )
        )
        ;; Fallback to modelspace XDATA
        (progn
          (setq entdata (entget (vlax-vla-object->ename modelspace) '("MBS_SETTINGS")))
          (setq xdata (assoc -3 entdata))
        )
      )
      
      (if xdata
        (progn
          (setq xdata-app (assoc "MBS_SETTINGS" (cdr xdata)))
          (if xdata-app
            (progn
              ;; Extract settings string
              (setq settings-string (cdr (assoc 1000 (cdr xdata-app))))
              
              ;; Parse settings string
              (setq settings-list (MBS:split-string settings-string "|"))
              
              ;; Load each setting
              (foreach setting settings-list
                (setq setting-pair (MBS:split-string setting "="))
                (if (= (length setting-pair) 2)
                  (cond
                    ((= (car setting-pair) "DESC_COMMA_LIMIT")
                     (setq description-comma-limit (atoi (cadr setting-pair))))
                    ((= (car setting-pair) "AUTO_UPDATE")
                     (setq auto-update-after-insert (= (cadr setting-pair) "1")))
                    ((= (car setting-pair) "TOTAL_WEIGHT")
                     (setq total-weight-calculation (= (cadr setting-pair) "1")))
                    ((= (car setting-pair) "AUTO_INCREMENT")
                     (setq auto-increment-item-numbers (= (cadr setting-pair) "1")))
                    ((= (car setting-pair) "AUTO_ACCEPT")
                     (setq auto-accept-item-number-changes (= (cadr setting-pair) "1")))
                    ((= (car setting-pair) "USE_TRANSACTIONS")
                     (setq use-two-stage-transactions (= (cadr setting-pair) "1")))
                    ((= (car setting-pair) "PREFIX_MODE")
                     (setq prefix-mode (cadr setting-pair)))
                    ((= (car setting-pair) "DRAWING_TYPE")
                     (setq current-drawing-type (cadr setting-pair)))
                  )
                )
              )
              (MBS:log-info "\n✅ Settings loaded from drawing")
              (MBS:log-info "\n📊 Loaded settings summary:")
              (MBS:log-info (strcat "\n  Description comma limit: " (itoa description-comma-limit)))
              (MBS:log-info (strcat "\n  Auto-update after insert: " (if auto-update-after-insert "ON" "OFF")))
              (MBS:log-info (strcat "\n  Total weight calculation: " (if total-weight-calculation "ON" "OFF")))
              (MBS:log-info (strcat "\n  Auto increment item numbers: " (if auto-increment-item-numbers "ON" "OFF")))
              (MBS:log-info (strcat "\n  Auto accept item number changes: " (if auto-accept-item-number-changes "ON" "OFF")))
              (MBS:log-info (strcat "\n  Prefix mode: " prefix-mode))
              (MBS:log-info (strcat "\n  Drawing type: " current-drawing-type))
              (setq success t)
            )
          )
        )
      )
    )
  )
  
  success
)

;; Ensure we don't redefine existing functions when loading this file
(if (not (boundp 'MBS:dialog-loaded))
  (progn  ;; Start of the true branch
    (setq MBS:dialog-loaded T)
    
    ;; Create DCL file in temp directory
    (defun MBS:create-dcl-file ( / dcl-file f)
      (setq dcl-file (strcat (getvar "TEMPPREFIX") "matblock.dcl"))
      (setq f (open dcl-file "w"))
      
      ;; Write DCL content for main menu
      (write-line "// Main menu dialog" f)
      (write-line "matblock_menu : dialog {" f)
      (write-line "  key = \"main_dialog\";" f)
      (write-line "  label = \"Material Block System (MBS)\";" f)
      (write-line "  aspect_ratio = 0;" f)
      (write-line "  " f)
      (write-line "  // Title" f)
      (write-line "  : row {" f)
      (write-line "    : text {" f)
      (write-line "      label = \"Material Block System (MBS) - Version 5.5\";" f)
      (write-line "      alignment = centered;" f)
      (write-line "      width = 40;" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  " f)
      (write-line "  // Mode indicator" f)
      (write-line "  : row {" f)
      (write-line "    : text {" f)
      (write-line "      label = \"Current Mode:\";" f)
      (write-line "      width = 15;" f)
      (write-line "    }" f)
      (write-line "    : text {" f)
      (write-line "      key = \"mode_display\";" f)
      (write-line "      width = 25;" f)
      (write-line "      alignment = left;" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  " f)
      (write-line "  // Drawing Type indicator  " f)
      (write-line "  : row {" f)
      (write-line "    : text {" f)
      (write-line "      label = \"Drawing Type:\";" f)
      (write-line "      width = 15;" f)
      (write-line "    }" f)
      (write-line "    : text {" f)
      (write-line "      key = \"drawing_type_display\";" f)
      (write-line "      width = 25;" f)
      (write-line "      alignment = left;" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  " f)
      (write-line "  : spacer { height = 0.1; }" f)
      (write-line "  " f)
      (write-line "  // Operations box" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Operations\";" f)
      (write-line "    width = 40;" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"add_block\";" f)
      (write-line "      label = \"INSERT NEW MATERIAL - QUICK\";" f)
      (write-line "      mnemonic = \"1\";" f)
      (write-line "      is_default = true;" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"insert\";" f)
      (write-line "      label = \"INSERT MATERIAL FROM CSV\";" f)
      (write-line "      mnemonic = \"1\";" f)
      (write-line "      is_default = true;" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"material_library\";" f)
      (write-line "      label = \"INSERT MATERIAL FROM LIBRARY\";" f)
      (write-line "      mnemonic = \"4\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"update_basic\";" f)
      (write-line "      label = \"UPDATE BLOCKS (BASIC)\";" f)
      (write-line "      mnemonic = \"2\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"update_full\";" f)
      (write-line "      label = \"UPDATE BLOCKS (FULL SCAN AUDIT)\";" f)
      (write-line "      mnemonic = \"3\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  " f)
      (write-line "  : spacer { height = 0.05; }" f)
      (write-line "  " f)
      (write-line "  // CSV Management box" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"CSV Management\";" f)
      (write-line "    width = 40;" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"select_csv\";" f)
      (write-line "      label = \"CREATE OR SELECT CSV FILES\";" f)
      (write-line "      mnemonic = \"5\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"open_excel\";" f)
      (write-line "      label = \"OPEN CSV FILE IN EXCEL\";" f)
      (write-line "      mnemonic = \"6\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"backup_csv\";" f)
      (write-line "      label = \"CREATE CSV BACKUP\";" f)
      (write-line "      mnemonic = \"7\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"restore_csv\";" f)
      (write-line "      label = \"RESTORE CSV BACKUP\";" f)
      (write-line "      mnemonic = \"8\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : text {" f)
      (write-line "      key = \"csv_status\";" f)
      (write-line "      width = 36;" f)
      (write-line "      alignment = left;" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  " f)
      (write-line "  : spacer { height = 0.05; }" f)
      (write-line "  " f)
      (write-line "  // Settings box" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Settings\";" f)
      (write-line "    width = 40;" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"toggle_mode\";" f)
      (write-line "      label = \"TOGGLE MODE (LOM/LOR)\";" f)
      (write-line "      mnemonic = \"T\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"settings\";" f)
      (write-line "      label = \"SETTINGS\";" f)
      (write-line "      mnemonic = \"0\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"debug\";" f)
      (write-line "      label = \"DEBUG\";" f)
      (write-line "      mnemonic = \"D\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  " f)
      (write-line "  : spacer { height = 0.1; }" f)
      (write-line "  " f)
      (write-line "  // OK/Cancel buttons" f)
      (write-line "  : row {" f)
      (write-line "    fixed_width = true;" f)
      (write-line "    alignment = centered;" f)
      (write-line "    : button {" f)
      (write-line "      key = \"cancel\";" f)
      (write-line "      label = \"Exit\";" f)
      (write-line "      is_cancel = true;" f)
      (write-line "      width = 10;" f)
      (write-line "    }" f)
      (write-line "    : spacer { width = 3; }" f)
      (write-line "    : button {" f)
      (write-line "      key = \"help\";" f)
      (write-line "      label = \"Help\";" f)
      (write-line "      width = 10;" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "}" f)
      (write-line "" f)
      
      ;; Write DCL content for settings dialog
      (write-line "// Settings dialog" f)
      (write-line "matblock_settings : dialog {" f)
      (write-line "  key = \"settings_dialog\";" f)
      (write-line "  label = \"Material Block System Settings\";" f)
      (write-line "  aspect_ratio = 0;" f)
      (write-line "  " f)
      (write-line "  // Description Settings" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Description Settings\";" f)
      (write-line "    width = 40;" f)
      (write-line "    " f)
      (write-line "    : toggle {" f)
      (write-line "      key = \"desc_shorten\";" f)
      (write-line "      label = \"Enable Description Shortening\";" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : row {" f)
      (write-line "      : text {" f)
      (write-line "        label = \"Description Length to Show in DWG (by comma):\";" f)
      (write-line "        width = 25;" f)
      (write-line "      }" f)
      (write-line "      : edit_box {" f)
      (write-line "        key = \"desc_commas\";" f)
      (write-line "        width = 5;" f)
      (write-line "        edit_width = 3;" f)
      (write-line "        edit_limit = 2;" f)
      (write-line "      }" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  " f)
      (write-line "  : spacer { height = 0.1; }" f)
      (write-line "  " f)
      (write-line "  // Auto Update Settings" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Auto Update Settings\";" f)
      (write-line "    width = 40;" f)
      (write-line "    " f)
      (write-line "    : toggle {" f)
      (write-line "      key = \"total_weight\";" f)
      (write-line "      label = \"Enable Total Weight Calculation\";" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : toggle {" f)
      (write-line "      key = \"auto_increment\";" f)
      (write-line "      label = \"Enable Auto Increment of Item Numbers\";" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : toggle {" f)
      (write-line "      key = \"auto_accept\";" f)
      (write-line "      label = \"Auto-Accept Item Number Changes\";" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  " f)
      (write-line "  : spacer { height = 0.1; }" f)
      (write-line "  " f)
      (write-line "  // Prefix Settings" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Prefix Settings\";" f)
      (write-line "    width = 40;" f)
      (write-line "    " f)
      (write-line "    : radio_column {" f)
      (write-line "      key = \"prefix_mode\";" f)
      (write-line "      : radio_button {" f)
      (write-line "        key = \"prefix_auto\";" f)
      (write-line "        label = \"Auto\";" f)
      (write-line "      }" f)
      (write-line "      : radio_button {" f)
      (write-line "        key = \"prefix_manual\";" f)
      (write-line "        label = \"Manual\";" f)
      (write-line "      }" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : row {" f)
      (write-line "      : text {" f)
      (write-line "        label = \"Drawing Type:\";" f)
      (write-line "        width = 15;" f)
      (write-line "      }" f)
      (write-line "      : popup_list {" f)
      (write-line "        key = \"drawing_type\";" f)
      (write-line "        width = 25;" f)
      (write-line "        edit_width = 25;" f)
      (write-line "      }" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "  " f)
      (write-line "  : spacer { height = 0.1; }" f)
      (write-line "  " f)
      (write-line "  // Dialog buttons" f)
      (write-line "  : row {" f)
      (write-line "    fixed_width = true;" f)
      (write-line "    alignment = centered;" f)
      (write-line "    : button {" f)
      (write-line "      key = \"accept\";" f)
      (write-line "      label = \"OK\";" f)
      (write-line "      is_default = true;" f)
      (write-line "      width = 10;" f)
      (write-line "    }" f)
      (write-line "    : spacer { width = 3; }" f)
      (write-line "    : button {" f)
      (write-line "      key = \"cancel\";" f)
      (write-line "      label = \"Cancel\";" f)
      (write-line "      is_cancel = true;" f)
      (write-line "      width = 10;" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "}" f)
      
      (close f)
      dcl-file
    )
	
    ;; Help dialog function
    (defun MBS:show-help-dialog ()
      (alert 
        (strcat 
          "Material Block Sync - Quick Help\n\n"
          "This tool helps manage and sync material blocks in AutoCAD drawings with a List of Materials or Removals.\n\n"
          "Operations:\n"
          "1. INSERT BLOCKS FROM CSV - Insert individual material blocks from CSV\n"
          "2. BATCH INSERT BLOCKS - Insert all material blocks in a grid pattern\n"
          "3. UPDATE BLOCKS (FULL SCAN) - Sync all blocks with CSV and audit drawing\n"
          "4. MATERIAL LIBRARY - Browse and manage materials catalog\n\n"
          "CSV Management:\n"
          "5. CREATE/SELECT CSV - Create or choose a CSV file\n"
          "6. OPEN CSV IN EXCEL - Open & edit current CSV file\n"
          "7. CREATE CSV BACKUP - Create a backup of the current CSV file\n"
          "8. RESTORE CSV BACKUP - Restore from a previous backup\n\n"
          "Settings:\n"
          "9. TOGGLE MODE - Switch between LOM and LOR modes\n"
          "0. SETTINGS - Configure various program options\n\n"
          "New in version 5.5:\n"
          "- Added two-stage transaction system for more reliable updates\n"
          "- Auto-accept item number changes option\n"
          "- Improved CSV backup and restore system\n"
          "- Material Library functionality\n\n"
          "For detailed help, run script-help command and select MSYNC help option.\n\n"
          "Current Version: 5.5"
        )
      )
      nil
    )
    
    ;; Function to safely get CSV path
    (defun MBS:get-csv-path ()
      (if (and (boundp 'csv-path) csv-path)
        csv-path
        (if (boundp 'get-quiet-csv-path)
          (get-quiet-csv-path)
          nil
        )
      )
    )
    
    ;; Settings Dialog function with Auto-Increment toggle and auto-accept feature
    (defun MBS:show-settings-dialog ( / dcl-id dcl-file result)
      ;; Create DCL file
      (setq dcl-file (MBS:create-dcl-file))
      (setq dcl-id (load_dialog dcl-file))
      
      (if (< dcl-id 0)
        (progn
          (alert "Could not load the dialog definition file.")
          nil
        )
      )
      
      ;; Initialize and display the dialog
      (if (not (new_dialog "matblock_settings" dcl-id))
        (progn
          (alert "Could not create the dialog box.")
          nil
        )
      )
      
      ;; Load settings from persistent storage BEFORE setting dialog values
      (MBS:log-info "\n🔧 Loading settings from drawing...")
      (MBS:load-settings-from-drawing)
      
      ;; Synchronize settings with newer MBS:enable variables (in case they were changed via C:MBS-CONFIG)
      ;; This should happen AFTER loading from block, not before
      (if (boundp 'MBS:enable-weight-calculation)
        (setq total-weight-calculation MBS:enable-weight-calculation)
      )
      (if (boundp 'MBS:enable-auto-increment-item-numbers)
        (setq auto-increment-item-numbers MBS:enable-auto-increment-item-numbers)
      )
      (if (boundp 'MBS:auto-accept-item-number-changes)
        (setq auto-accept-item-number-changes MBS:auto-accept-item-number-changes)
      )
      
      ;; Set initial values for settings (using loaded values)
      (MBS:log-info "\n🔧 Initializing dialog with loaded settings...")
      (MBS:log-info "\n📊 Current variable values after loading:")
      (MBS:log-info (strcat "\n  description-comma-limit: " (itoa description-comma-limit)))
      (MBS:log-info (strcat "\n  auto-update-after-insert: " (if auto-update-after-insert "ON" "OFF")))
      (MBS:log-info (strcat "\n  total-weight-calculation: " (if total-weight-calculation "ON" "OFF")))
      (MBS:log-info (strcat "\n  auto-increment-item-numbers: " (if auto-increment-item-numbers "ON" "OFF")))
      (MBS:log-info (strcat "\n  auto-accept-item-number-changes: " (if auto-accept-item-number-changes "ON" "OFF")))
      (MBS:log-info (strcat "\n  prefix-mode: " prefix-mode))
      (MBS:log-info (strcat "\n  current-drawing-type: " current-drawing-type))
      
      ;; Description shortening settings
      (set_tile "desc_shorten" (if (> description-comma-limit 0) "1" "0"))
      (set_tile "desc_commas" (itoa description-comma-limit))
      (MBS:log-info (strcat "\n  Description comma limit: " (itoa description-comma-limit)))
      
      ;; Auto-update settings
      (set_tile "auto_update" (if auto-update-after-insert "1" "0"))
      (MBS:log-info (strcat "\n  Auto-update after insert: " (if auto-update-after-insert "ON" "OFF")))
      
      ;; Weight calculation settings
      (set_tile "total_weight" (if total-weight-calculation "1" "0"))
      (MBS:log-info (strcat "\n  Total weight calculation: " (if total-weight-calculation "ON" "OFF")))
      
      ;; Auto increment settings
      (set_tile "auto_increment" (if auto-increment-item-numbers "1" "0"))
      (MBS:log-info (strcat "\n  Auto increment item numbers: " (if auto-increment-item-numbers "ON" "OFF")))
      
      ;; Auto accept settings
      (set_tile "auto_accept" (if auto-accept-item-number-changes "1" "0"))
      (MBS:log-info (strcat "\n  Auto accept item number changes: " (if auto-accept-item-number-changes "ON" "OFF")))
      
      ;; Transaction system settings
      (set_tile "use_transactions" (if (boundp 'use-two-stage-transactions) 
                                      (if use-two-stage-transactions "1" "0")
                                      "1")) ;; Default to on for new installations
      (MBS:log-info (strcat "\n  Use two-stage transactions: " (if (and (boundp 'use-two-stage-transactions) use-two-stage-transactions) "ON" "OFF")))
      
      ;; Prefix mode settings
      (if (= prefix-mode "AUTO")
        (set_tile "prefix_auto" "1")
        (set_tile "prefix_manual" "1")
      )
      (MBS:log-info (strcat "\n  Prefix mode: " prefix-mode))
      
      ;; Populate drawing type list
      (start_list "drawing_type")
      (add_list "PIPING")
      (add_list "ELECTRICAL")
      (add_list "STRUCTURAL")
      (add_list "ARRANGEMENT")
      (add_list "HVAC")
      (end_list)
      
      ;; Set current drawing type
      (cond
        ((= current-drawing-type "P") (set_tile "drawing_type" "0"))
        ((= current-drawing-type "E") (set_tile "drawing_type" "1"))
        ((= current-drawing-type "S") (set_tile "drawing_type" "2"))
        ((= current-drawing-type "A") (set_tile "drawing_type" "3"))
        ((= current-drawing-type "H") (set_tile "drawing_type" "4"))
      )
      (MBS:log-info (strcat "\n  Drawing type: " current-drawing-type))
      
      (MBS:log-info "\n✅ Dialog initialized with loaded settings")
      
      ;; Define actions
      (action_tile "desc_shorten" "(MBS:update-desc-shortening $value)")
      (action_tile "prefix_auto" "(set_tile \"prefix_manual\" \"0\")")
      (action_tile "prefix_manual" "(set_tile \"prefix_auto\" \"0\")")
      (action_tile "accept" "(MBS:save-settings) (done_dialog 2)")
      (action_tile "cancel" "(done_dialog 0)")
      
      ;; Helper function to update description shortening UI
      (defun MBS:update-desc-shortening (value)
        (if (= value "0")
          (mode_tile "desc_commas" 1)  ;; Disable commas field
          (mode_tile "desc_commas" 0)  ;; Enable commas field
        )
      )
      
      ;; Function to save settings
      (defun MBS:save-settings ()
        ;; Save description settings
        (setq description-comma-limit 
          (if (= (get_tile "desc_shorten") "1")
            (atoi (get_tile "desc_commas"))
            0
          ))
        
        ;; Save auto-update setting
        (setq auto-update-after-insert (= (get_tile "auto_update") "1"))
        
        ;; Save total weight calculation setting
        (setq total-weight-calculation (= (get_tile "total_weight") "1"))
        
        ;; Save auto-increment setting
        (setq auto-increment-item-numbers (= (get_tile "auto_increment") "1"))
        
        ;; Save auto-accept setting
        (setq auto-accept-item-number-changes (= (get_tile "auto_accept") "1"))
        
        ;; Save transaction system setting
        (setq use-two-stage-transactions (= (get_tile "use_transactions") "1"))
        
        ;; Save prefix mode setting
        (setq prefix-mode (if (= (get_tile "prefix_auto") "1") "AUTO" "MANUAL"))
        
        ;; Save drawing type
        (setq current-drawing-type
          (cond
            ((= (get_tile "drawing_type") "0") "P")
            ((= (get_tile "drawing_type") "1") "E")
            ((= (get_tile "drawing_type") "2") "S")
            ((= (get_tile "drawing_type") "3") "A")
            ((= (get_tile "drawing_type") "4") "H")
            (t "P")
          )
        )
        
        ;; Synchronize newer MBS:enable variables with settings dialog variables
        (if (boundp 'MBS:enable-weight-calculation)
          (setq MBS:enable-weight-calculation total-weight-calculation)
        )
        (if (boundp 'MBS:enable-auto-increment-item-numbers)
          (setq MBS:enable-auto-increment-item-numbers auto-increment-item-numbers)
        )
        (if (boundp 'MBS:auto-accept-item-number-changes)
          (setq MBS:auto-accept-item-number-changes auto-accept-item-number-changes)
        )
        
        ;; Save settings to drawing using XDATA
        (MBS:log-info "\n✅ Settings saved to memory")
        (if (MBS:save-settings-to-xdata)
          (MBS:log-info "\n✅ Settings saved to drawing")
          (MBS:log-warning "\n⚠ Could not save settings to drawing")
        )
      )
      
      ;; Save settings to attributes on _MBS_SETTINGS_MARKER block
      (defun MBS:save-settings-to-xdata (/ doc success marker-block)
        "Save current settings to attributes on _MBS_SETTINGS_MARKER block"
        (setq success nil)
        
        (vl-catch-all-apply
          '(lambda ()
            (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
            
            ;; Find or create _MBS_SETTINGS_MARKER block
            (setq marker-block (MBS:find-or-create-settings-marker))
            (if marker-block
              (progn
                ;; Get attributes from the marker block
                (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke (vlax-ename->vla-object marker-block) 'GetAttributes))))
                
                (if (vl-catch-all-error-p att-list)
                  (progn
                    ;; Fallback to XDATA if attributes fail
                    (regapp "MBS_SETTINGS")
                    (setq settings-string 
                      (strcat 
                        "DESC_COMMA_LIMIT=" (itoa description-comma-limit) "|"
                        "AUTO_UPDATE=" (if auto-update-after-insert "1" "0") "|"
                        "TOTAL_WEIGHT=" (if total-weight-calculation "1" "0") "|"
                        "AUTO_INCREMENT=" (if auto-increment-item-numbers "1" "0") "|"
                        "AUTO_ACCEPT=" (if auto-accept-item-number-changes "1" "0") "|"
                        "USE_TRANSACTIONS=" (if use-two-stage-transactions "1" "0") "|"
                        "PREFIX_MODE=" prefix-mode "|"
                        "DRAWING_TYPE=" current-drawing-type
                      )
                    )
                    (setq xdata-list (list (list -3 (list "MBS_SETTINGS" (cons 1000 settings-string)))))
                    (entmod (append (entget marker-block) xdata-list))
                    (setq success t)
                  )
                  (progn
                                         ;; Update attributes with current settings
                     (foreach att att-list
                       (setq tag (strcase (vlax-get att 'TagString)))
                       (cond
                         ((= tag "AUTO_ACCEPT")
                          (vlax-put att 'TextString (if auto-accept-item-number-changes "1" "0")))
                         ((= tag "AUTO_NUMBER")
                          (vlax-put att 'TextString (if auto-increment-item-numbers "1" "0")))
                         ((= tag "AUTO_WEIGHT")
                          (vlax-put att 'TextString (if total-weight-calculation "1" "0")))
                         ((= tag "AUTO_UPDATE")
                          (vlax-put att 'TextString (if auto-update-after-insert "1" "0")))
                         ((= tag "USE_TRANSACTIONS")
                          (vlax-put att 'TextString (if use-two-stage-transactions "1" "0")))
                         ((= tag "PREFIX_CONTROL")
                          (vlax-put att 'TextString prefix-mode))
                         ((= tag "DWG_TYPE")
                          (vlax-put att 'TextString current-drawing-type))
                         ((= tag "DESCRIPTION_SHORTENING")
                          (vlax-put att 'TextString (itoa description-comma-limit)))
                       )
                     )
                    (vla-Update (vlax-ename->vla-object marker-block))
                    (setq success t)
                  )
                )
              )
              (progn
                ;; Fallback to modelspace if marker block creation fails
                (regapp "MBS_SETTINGS")
                (setq modelspace (vla-get-ModelSpace doc))
                (setq settings-string 
                  (strcat 
                    "DESC_COMMA_LIMIT=" (itoa description-comma-limit) "|"
                    "AUTO_UPDATE=" (if auto-update-after-insert "1" "0") "|"
                    "TOTAL_WEIGHT=" (if total-weight-calculation "1" "0") "|"
                    "AUTO_INCREMENT=" (if auto-increment-item-numbers "1" "0") "|"
                    "AUTO_ACCEPT=" (if auto-accept-item-number-changes "1" "0") "|"
                    "USE_TRANSACTIONS=" (if use-two-stage-transactions "1" "0") "|"
                    "PREFIX_MODE=" prefix-mode "|"
                    "DRAWING_TYPE=" current-drawing-type
                  )
                )
                (setq xdata-list (list (list -3 (list "MBS_SETTINGS" (cons 1000 settings-string)))))
                (entmod (append (entget (vlax-vla-object->ename modelspace)) xdata-list))
                (setq success t)
              )
            )
          )
        )
        
        success
      )
      
      ;; Find or create _MBS_SETTINGS_MARKER block
      (defun MBS:find-or-create-settings-marker (/ doc modelspace marker-block)
        "Find existing _MBS_SETTINGS_MARKER block or create one"
        (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
        (setq modelspace (vla-get-ModelSpace doc))
        
        ;; First try to find existing marker block
        (vlax-for ent modelspace
          (if (and (= (vla-get-ObjectName ent) "AcDbBlockReference")
                   (= (vlax-get ent 'EffectiveName) "_MBS_SETTINGS_MARKER"))
            (setq marker-block (vlax-vla-object->ename ent))
          )
        )
        
        ;; If not found, create a new one
        (if (not marker-block)
          (progn
            ;; Create a simple invisible block for settings
            (command "._insert" "_MBS_SETTINGS_MARKER" '(0 0 0) 1 1 0)
            (setq marker-block (entlast))
            ;; Make it invisible
            (entmod (subst (cons 62 0) (assoc 62 (entget marker-block)) (entget marker-block)))
          )
        )
        
        marker-block
      )
      
      ;; Note: MBS:load-settings-from-drawing is now defined at global scope
      
      ;; Helper function to split strings
      (defun MBS:split-string (str delimiter / pos result)
        "Split string by delimiter"
        (setq result '())
        (while (setq pos (vl-string-search delimiter str))
          (setq result (append result (list (substr str 1 pos))))
          (setq str (substr str (+ pos (strlen delimiter) 1)))
        )
        (if (/= str "")
          (setq result (append result (list str)))
        )
        result
      )
      
      ;; Set the initial state of the description commas field
      (MBS:update-desc-shortening (get_tile "desc_shorten"))
      
      ;; Display dialog and get result
      (setq result (start_dialog))
      (unload_dialog dcl-id)
      
      ;; Clean up
      (vl-file-delete dcl-file)
      
      result
    )
	
    ;; Function to create a CSV backup from the menu
    (defun MBS:backup-csv-from-menu (/ csv-path result)
      (setq csv-path (MBS:get-csv-path))
      
      (if csv-path
        (progn
          (setq result (MBS:backup-csv-file csv-path))
          (if result
            (alert "CSV file backed up successfully.")
            (alert "Failed to backup CSV file. Check permissions or disk space.")
          )
          result
        )
        (progn
          (alert "No CSV file selected. Please select a CSV file first.")
          nil
        )
      )
    )
    
    ;; Placeholder for Material Library function
    (defun c:MBS-MaterialLibrary ()
      (MBS:log-info "\n=== MATERIAL LIBRARY ===")
      (MBS:log-info "\nThis feature will be implemented in a future update.")
      (princ)
    )
    
    ;; Main menu dialog function - uses existing functions when available
    (defun MBS:show-main-dialog ( / dcl-id dcl-file result)
      ;; Check if CSV files are configured - if not, launch CSV selection first
      (if (not (MBS:check-csv-configured))
        (progn
          (MBS:log-warning "\n⚠ No CSV files configured for this drawing.")
          (MBS:log-info "\n🔧 Launching CSV file selection/creation...")
          
          ;; Try to launch CSV selection
          (if (boundp 'MBS:select-csv-files)
            (MBS:select-csv-files)
            (progn
              (alert "CSV selection function not available. Please configure CSV files manually.")
              (MBS:log-info "\n💡 Use 'MBS-SELECT-CSV-FILES' to configure CSV files")
            )
          )
          
          ;; After CSV selection, check if we should continue with main menu
          (if (not (MBS:check-csv-configured))
            (progn
              (MBS:log-error "\n❌ No CSV files configured. Main menu unavailable.")
              (MBS:log-info "\n💡 Please configure CSV files first using 'MBS-SELECT-CSV-FILES'")
              (exit)
            )
          )
        )
      )
      
      ;; Create DCL file
      (setq dcl-file (MBS:create-dcl-file))
      (setq dcl-id (load_dialog dcl-file))
      
      (if (< dcl-id 0)
        (progn
          (alert "Could not load the dialog definition file.")
          (exit)
        )
      )
      
      ;; Initialize and display the dialog
      (if (not (new_dialog "matblock_menu" dcl-id))
        (progn
          (alert "Could not create the dialog box.")
          (exit)
        )
      )
      
      ;; Set the initial values for the dialog fields
      (set_tile "mode_display" 
        (strcat current-mode " (" 
               (if (= current-mode "LOM") "LIST OF MATERIAL" "LIST OF REMOVAL") 
               ")"))
      
      (set_tile "drawing_type_display" 
        (strcat "Type " current-drawing-type " (" 
               (cond 
                 ((= current-drawing-type "P") "PIPING")
                 ((= current-drawing-type "E") "ELECTRICAL")
                 ((= current-drawing-type "S") "STRUCTURAL")
                 ((= current-drawing-type "A") "ARRANGEMENT")
                 ((= current-drawing-type "H") "HVAC")
                 (t "UNKNOWN")
               ) 
               ")"))
      
      ;; Set CSV status - using safe function
      (setq current-csv-path (MBS:get-csv-path))
      
      (set_tile "csv_status" 
        (if (and current-csv-path (findfile current-csv-path))
          (strcat "Current CSV: " (MBS:filename-base current-csv-path))
          "No CSV file selected"
        )
      )
      
      ;; Define actions for buttons
      (action_tile "insert" "(done_dialog 1)")
      (action_tile "update_basic" "(done_dialog 2)")
      (action_tile "update_full" "(done_dialog 3)")
      (action_tile "material_library" "(done_dialog 4)")
      (action_tile "select_csv" "(done_dialog 5)")
      (action_tile "open_excel" "(done_dialog 6)")
      (action_tile "backup_csv" "(done_dialog 7)")
      (action_tile "restore_csv" "(done_dialog 8)")
      (action_tile "add_block" "(done_dialog 9)")
      (action_tile "toggle_mode" "(done_dialog 10)")
      (action_tile "settings" "(done_dialog 11)")
      (action_tile "debug" "(done_dialog 13)")
      (action_tile "help" "(done_dialog 12)")
      (action_tile "cancel" "(done_dialog 0)")
      
      ;; Display dialog and get result
      (setq result (start_dialog))
      (unload_dialog dcl-id)
      
      ;; Clean up
      (vl-file-delete dcl-file)
      
      ;; Process dialog result - use error handling instead of checking if functions exist
      (cond
        ((= result 1) 
         (if (boundp 'C:MBS-INSERT3)
           (C:MBS-INSERT3)
           (alert "Function MBS-INSERT3 not found")
         )
        )
        ((= result 2) 
         (if (boundp 'C:MBS-UPDATE-SIMPLE)
           (C:MBS-UPDATE-SIMPLE)
           (alert "MBS-UPDATE-SIMPLE not found")
         )
        )
        ((= result 3) 
         (if (boundp 'C:MBS-UPDATE3)
           (C:MBS-UPDATE3)
           (alert "MBS-UPDATE3 not found")
         )
        )
        ((= result 4) 
         (progn
           (MBS:log-verbose "\n🔍 DEBUG: Checking for C:MBS-LibraryDialog function...")
           (if (boundp 'C:MBS-LibraryDialog)
             (progn
               (MBS:log-info "\n✅ Function found, calling C:MBS-LibraryDialog...")
               (C:MBS-LibraryDialog)
             )
             (progn
               (MBS:log-error "\n❌ Function C:MBS-LibraryDialog not found!")
               (MBS:log-error "\n❌ Available functions starting with 'C:MBS-Library':")
               (vl-symbolp 'C:MBS-LibraryDialog)
               (alert "Material Library Dialog function not available yet")
             )
           )
         )
        )
        ((= result 5) 
         (if (boundp 'MBS:select-csv-files)
           (MBS:select-csv-files)
           (progn
             (alert "Function MBS:select-csv-files not found")
             (setq MBS:dialog-error t)
           )
         )
        )
        ((= result 6) 
         (if (boundp 'open-csv-in-excel)
           (open-csv-in-excel)
           (alert "Function open-csv-in-excel not found")
         )
        )
        ((= result 7)
         (if (boundp 'MBS:backup-csv-file)
           (MBS:backup-csv-from-menu)
           (alert "Function MBS:backup-csv-file not found")
         )
        )
        ((= result 8)
         (if (boundp 'c:MBS-RestoreCSV)
           (c:MBS-RestoreCSV)
           (alert "Function MBS-RestoreCSV not found")
         )
        )
        ((= result 9)
         (if (boundp 'C:MBS-ADD-BLOCK)
           (C:MBS-ADD-BLOCK)
           (alert "Function C:MBS-ADD-BLOCK not found")
         )
        )
        ((= result 10) 
         ;; Use the enhanced mode switching commands that update both global config and settings marker
         (if (boundp 'MBS:toggle-enhanced-mode)
           (progn
             (MBS:log-info "\n🔄 Toggling mode with enhanced system...")
             (MBS:toggle-enhanced-mode)
           )
           (progn
             ;; Fallback to basic mode switching if enhanced system not available
             (MBS:log-warning "\n⚠ Enhanced mode system not available, using basic toggle...")
             (if (= current-mode "LOM")
               (progn
                 (MBS:log-info "\nSwitching to LOR mode...")
                 (if (boundp 'C:MBS-SETLOR)
                   (C:MBS-SETLOR)
                   (progn
                     (setq current-mode "LOR")
                     (MBS:log-info "\nMode changed to: LOR")
                   )
                 )
               )
               (progn
                 (MBS:log-info "\nSwitching to LOM mode...")
                 (if (boundp 'C:MBS-SETLOM)
                   (C:MBS-SETLOM)
                   (progn
                     (setq current-mode "LOM")
                     (MBS:log-info "\nMode changed to: LOM")
                   )
                 )
               )
             )
           )
         )
        )
        ((= result 11) 
          (setq settings-result (MBS:show-settings-dialog))
          ;; If settings were saved (return value 2), we'll automatically redisplay the main menu
          ;; If settings were cancelled (return value 0), we'll also redisplay the main menu
          ;; The main menu will redisplay anyway due to the existing logic at the end of the function
        )
        ((= result 12) (MBS:show-help-dialog))
        ((= result 13) 
          (if (boundp 'C:MBS-Debug)
            (C:MBS-Debug)
            (alert "Debug function not available")
          )
        )
      )
      
      ;; If we did something except exit, redisplay the dialog
      ;; But don't redisplay if there was an error (function not found)
      (if (and result (/= result 0) (not (boundp 'MBS:dialog-error)))
        (MBS:show-main-dialog)
      )
      
      result
    )
    
    ;; Helper function to check if CSV files are properly configured
    (defun MBS:check-csv-configured ( / csv-path)
      (setq csv-path (MBS:get-csv-path))
      (and csv-path (findfile csv-path))
    )
    
    ;; Test if DCL is supported
    (defun MBS:test-dcl-support ( / dcl-id dialog-file f result)
      (setq result T) ;; Assume DCL is supported
      
      ;; Create a simple test DCL file
      (setq dialog-file (strcat (getvar "TEMPPREFIX") "dcl_test.dcl"))
      (setq f (open dialog-file "w"))
      (write-line "test : dialog {" f)
      (write-line "  label = \"DCL Test\";" f)
      (write-line "  : text { label = \"This is a test\"; }" f)
      (write-line "  ok_cancel;" f)
      (write-line "}" f)
      (close f)
      
      ;; Try to load and create a dialog
      (setq dcl-id (load_dialog dialog-file))
      (if (< dcl-id 0)
        (setq result nil)
        (progn
          (if (not (new_dialog "test" dcl-id))
            (setq result nil)
            (done_dialog)
          )
          (unload_dialog dcl-id)
        )
      )
      
      ;; Clean up
      (vl-file-delete dialog-file)
      
      ;; Return result
      result
    )
    
    ;; Dialog-based SYNC command (default)
    (defun c:MSYNC ()
      ;; Try to use the dialog interface
      (if (not (MBS:test-dcl-support))
        ;; Fall back to text menu if DCL isn't supported
        (if (boundp 'c:MBS-MENU)
          (c:MBS-MENU)
          (alert "Text menu (MBS-MENU) not found")
        )
        ;; Use dialog interface
        (MBS:show-main-dialog)
      )
      (princ)
    )
    
    ;; Legacy text menu command - now renamed
    (defun c:MSYNC-TEXT ()
      (if (boundp 'c:MBS-MENU)
        (c:MBS-MENU)
        (alert "Text menu (MBS-MENU) not found")
      )
      (princ)
    )
    
    ;; Also define aliases
    (defun c:CSV-BSE () (c:MSYNC))
    (defun c:LOM () (setq current-mode "LOM") (c:MSYNC))
    (defun c:LOR () (setq current-mode "LOR") (c:MSYNC))
  ) ;; End of progn (true branch of if)
) ;; End of if

(MBS:log-info "\nMaterial Block Sync Dialog Interface loaded.")
(princ)