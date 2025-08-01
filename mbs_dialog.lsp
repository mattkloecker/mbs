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
      (write-line "  label = \"Material Block Sync\";" f)
      (write-line "  aspect_ratio = 0;" f)
      (write-line "  " f)
      (write-line "  // Title" f)
      (write-line "  : row {" f)
      (write-line "    : text {" f)
      (write-line "      label = \"Material Block Sync\";" f)
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
      (write-line "      key = \"insert\";" f)
      (write-line "      label = \"1. INSERT BLOCKS FROM CSV\";" f)
      (write-line "      mnemonic = \"1\";" f)
      (write-line "      is_default = true;" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"batch_insert\";" f)
      (write-line "      label = \"2. BATCH INSERT BLOCKS FROM CSV\";" f)
      (write-line "      mnemonic = \"2\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"update_full\";" f)
      (write-line "      label = \"3. UPDATE BLOCKS (FULL SCAN)\";" f)
      (write-line "      mnemonic = \"3\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"material_library\";" f)
      (write-line "      label = \"4. MATERIAL LIBRARY\";" f)
      (write-line "      mnemonic = \"4\";" f)
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
      (write-line "      label = \"5. CREATE OR SELECT CSV FILES\";" f)
      (write-line "      mnemonic = \"5\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"open_excel\";" f)
      (write-line "      label = \"6. OPEN CSV FILE IN EXCEL\";" f)
      (write-line "      mnemonic = \"6\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"backup_csv\";" f)
      (write-line "      label = \"7. CREATE CSV BACKUP\";" f)
      (write-line "      mnemonic = \"7\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"restore_csv\";" f)
      (write-line "      label = \"8. RESTORE CSV BACKUP\";" f)
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
      (write-line "      label = \"9. TOGGLE MODE (LOM/LOR)\";" f)
      (write-line "      mnemonic = \"9\";" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"settings\";" f)
      (write-line "      label = \"0. SETTINGS\";" f)
      (write-line "      mnemonic = \"0\";" f)
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
      (write-line "  label = \"Material Block Sync Settings\";" f)
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
      (write-line "        label = \"Number of Commas to Keep:\";" f)
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
      (write-line "      key = \"auto_update\";" f)
      (write-line "      label = \"Enable Auto Update After Insert\";" f)
      (write-line "    }" f)
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
      (write-line "  // Transaction Settings" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Transaction System\";" f)
      (write-line "    width = 40;" f)
      (write-line "    " f)
      (write-line "    : toggle {" f)
      (write-line "      key = \"use_transactions\";" f)
      (write-line "      label = \"Use Two-Stage Transaction System\";" f)
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
      
      ;; Set initial values for settings
      (set_tile "desc_shorten" (if (> description-comma-limit 0) "1" "0"))
      (set_tile "desc_commas" (itoa description-comma-limit))
      (set_tile "auto_update" (if auto-update-after-insert "1" "0"))
      (set_tile "total_weight" (if total-weight-calculation "1" "0"))
      
      ;; Set auto increment toggle
      (set_tile "auto_increment" (if auto-increment-item-numbers "1" "0"))
      
      ;; Set auto-accept changes toggle
      (set_tile "auto_accept" (if auto-accept-item-number-changes "1" "0"))
      
      ;; Set transaction system toggle
      (set_tile "use_transactions" (if (boundp 'use-two-stage-transactions) 
                                      (if use-two-stage-transactions "1" "0")
                                      "1")) ;; Default to on for new installations
      
      ;; Set prefix mode radio buttons
      (if (= prefix-mode "AUTO")
        (set_tile "prefix_auto" "1")
        (set_tile "prefix_manual" "1")
      )
      
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
        
        ;; Save settings to drawing
        (if (boundp 'MBS:save-settings-to-drawing)
          (MBS:save-settings-to-drawing)
        )
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
      (prompt "\n=== MATERIAL LIBRARY ===")
      (prompt "\nThis feature will be implemented in a future update.")
      (princ)
    )
    
    ;; Main menu dialog function - uses existing functions when available
    (defun MBS:show-main-dialog ( / dcl-id dcl-file result)
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
        (if current-csv-path 
          (strcat "Current CSV: " (MBS:filename-base current-csv-path))
          "No CSV file selected"
        )
      )
      
      ;; Define actions for buttons
      (action_tile "insert" "(done_dialog 1)")
      (action_tile "batch_insert" "(done_dialog 2)")
      (action_tile "update_full" "(done_dialog 3)")
      (action_tile "material_library" "(done_dialog 4)")
      (action_tile "select_csv" "(done_dialog 5)")
      (action_tile "open_excel" "(done_dialog 6)")
      (action_tile "backup_csv" "(done_dialog 7)")
      (action_tile "restore_csv" "(done_dialog 8)")
      (action_tile "toggle_mode" "(done_dialog 9)")
      (action_tile "settings" "(done_dialog 10)")
      (action_tile "help" "(done_dialog 11)")
      (action_tile "cancel" "(done_dialog 0)")
      
      ;; Display dialog and get result
      (setq result (start_dialog))
      (unload_dialog dcl-id)
      
      ;; Clean up
      (vl-file-delete dcl-file)
      
      ;; Process dialog result - use error handling instead of checking if functions exist
      (cond
        ((= result 1) 
         (if (boundp 'c:MBS-INSERT2)
           (c:MBS-Insert2)
           (alert "Function MBS-INSERT2 not found")
         )
        )
        ((= result 2) 
         (if (boundp 'c:MBS-BATCHINSERT2)
           (c:MBS-BATCHINSERT2)
           (alert "Function MBS-BATCHINSERT2 not found")
         )
        )
        ((= result 3) 
         (if (boundp 'c:MBS-UpdateBlocks2)
           (c:MBS-UpdateBlocks2)
           (alert "MBS-UpdateBlocks2 not found")
         )
        )
        ((= result 4) 
         (if (boundp 'c:MBS-MaterialLibrary)
           (c:MBS-MaterialLibrary)
           (alert "Material Library function not available yet")
         )
        )
        ((= result 5) 
         (if (boundp 'select-csv-files)
           (select-csv-files)
           (alert "Function select-csv-files not found")
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
         (if (boundp 'toggle-mode)
           (toggle-mode)
           (progn
             (setq current-mode (if (= current-mode "LOM") "LOR" "LOM"))
             (prompt (strcat "\nMode changed to: " current-mode))
           )
         )
        )
        ((= result 10) 
          (setq settings-result (MBS:show-settings-dialog))
          ;; If settings were saved (return value 2), we'll automatically redisplay the main menu
          ;; If settings were cancelled (return value 0), we'll also redisplay the main menu
          ;; The main menu will redisplay anyway due to the existing logic at the end of the function
        )
        ((= result 11) (MBS:show-help-dialog))
      )
      
      ;; If we did something except exit, redisplay the dialog
      (if (and result (/= result 0))
        (MBS:show-main-dialog)
      )
      
      result
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

(prompt "\nMaterial Block Sync Dialog Interface loaded.")
(princ)