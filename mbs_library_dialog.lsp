;; ========================================================================
;; MATERIAL BLOCK SYNC - Library Dialog Interface
;; VERSION 1.0
;; ========================================================================
;; Dialog-based interface for importing materials from external CSV library files
;; DEPENDENCIES: This file must be loaded AFTER mbs_blocks.lsp and mbs_library.lsp
;; ========================================================================

(prompt "\n[DEBUG] Starting library dialog interface load...")
(prompt "\n[DEBUG] Loading mbs_library_dialog.lsp...")

;; ========================================================================
;; SETTINGS MANAGEMENT - Library Paths via Block Attributes
;; ========================================================================

;; Get library settings from _MBS_SETTINGS_MARKET block attributes
(defun MBS:get-library-settings ( / block settings)
  "Get library paths from _MBS_SETTINGS_MARKET block attributes"
  
  (setq block (MBS:find-settings-block))
  (if block
    (progn
      (setq settings (list
        (MBS:get-block-attribute block "LIBRARY1")
        (MBS:get-block-attribute block "LIBRARY2")
        (MBS:get-block-attribute block "LIBRARY3")
      ))
      ;; Filter out empty or nil values
      (setq settings (vl-remove-if '(lambda (x) (or (not x) (= x ""))) settings))
      settings
    )
    (progn
      (prompt "\n⚠ No settings block found - using default empty settings")
      '("" "" "")
    )
  )
)

;; Set library settings to _MBS_SETTINGS_MARKET block attributes
(defun MBS:set-library-settings (library-paths / block)
  "Set library paths to _MBS_SETTINGS_MARKET block attributes"
  
  (setq block (MBS:find-settings-block))
  (if block
    (progn
      ;; Ensure we have 3 paths (pad with empty strings if needed)
      (while (< (length library-paths) 3)
        (setq library-paths (append library-paths (list "")))
      )
      
      ;; Set the attributes
      (MBS:set-block-attribute block "LIBRARY1" (nth 0 library-paths))
      (MBS:set-block-attribute block "LIBRARY2" (nth 1 library-paths))
      (MBS:set-block-attribute block "LIBRARY3" (nth 2 library-paths))
      
      (prompt "\n✅ Library settings saved to settings block")
      t
    )
    (progn
      (prompt "\n❌ No settings block found - cannot save library settings")
      nil
    )
  )
)

;; ========================================================================
;; MULTI-LIBRARY SEARCH
;; ========================================================================

;; Search across multiple selected libraries
(defun MBS:search-multiple-libraries (search-term library-paths / results)
  "Search across multiple selected libraries"
  
  (setq results '())
  (foreach library-path library-paths
    (if (and library-path (/= library-path ""))
      (progn
        (prompt (strcat "\n🔍 Searching library: " library-path))
        (setq library-results (MBS:search-library-smart library-path search-term))
        (if library-results
          (setq results (append results library-results))
        )
      )
    )
  )
  
  (prompt (strcat "\n✅ Found " (itoa (length results)) " total matches across all libraries"))
  results
)

;; Enhanced search with real-time dialog status updates
(defun MBS:search-multiple-libraries-enhanced (search-term library-paths / results library-count current-library)
  "Search across multiple selected libraries with real-time status updates"
  
  (setq results '())
  (setq library-count (length library-paths))
  (setq current-library 0)
  
  ;; Update initial status
  (set_tile "status" (strcat "Starting search for: '" search-term "'..."))
  
  (foreach library-path library-paths
    (setq current-library (1+ current-library))
    
    (if (and library-path (/= library-path ""))
      (progn
        ;; Update status for current library
        (set_tile "status" (strcat "Searching library " (itoa current-library) "/" (itoa library-count) ": " (vl-filename-base library-path)))
        
        ;; Get library results with search type indication
        (setq library-results (MBS:search-library-smart-with-status library-path search-term))
        
        (if library-results
          (setq results (append results library-results))
        )
      )
    )
  )
  
  ;; Update final status
  (set_tile "status" (strcat "Search completed - " (itoa (length results)) " matches found"))

  ;; Enforce overall cap at 150 results with info dialog
  (if (> (length results) 150)
    (progn
      (alert (strcat "Your search returned " (itoa (length results)) " results.\n"
                     "For performance, only the first 150 will be shown."))
      (set_tile "status" (strcat "⚠ Showing first 150 of " (itoa (length results)) " results"))
      (setq tmp '())
      (setq i 0)
      (foreach r results
        (if (< i 150) (setq tmp (append tmp (list r))))
        (setq i (1+ i))
      )
      (setq results tmp)
    )
  )
  
  (MBS:log-info (strcat "\n✅ Found " (itoa (length results)) " total matches across all libraries"))
  results
)

;; Search library with status updates
(defun MBS:search-library-smart-with-status (library-path search-term / search-term-length search-type)
  "Search library with real-time status updates"
  
  (setq search-term-length (strlen search-term))
  
  ;; Determine search type and update status
  (cond
    ;; Very short terms (1-2 chars) - use fast search with small limit
    ((<= search-term-length 2)
     (setq search-type "FAST")
     (set_tile "status" (strcat "🚀 Fast search for: '" search-term "' (max 50 results)"))
     (MBS:search-library-fast-with-progress library-path search-term)
    )
    ;; Medium terms (3-5 chars) - use optimized search with medium limit
    ((<= search-term-length 5)
     (setq search-type "OPTIMIZED")
     (set_tile "status" (strcat "🔍 Optimized search for: '" search-term "' (max 100 results)"))
     (MBS:search-library-optimized-with-progress library-path search-term)
    )
    ;; Long terms (6+ chars) - use original search (likely to be specific)
    (t
     (setq search-type "COMPREHENSIVE")
     (set_tile "status" (strcat "🔍 Comprehensive search for: '" search-term "' (all results)"))
     (MBS:search-library library-path search-term)
    )
  )
)

;; Enhanced fast search with progress updates
(defun MBS:search-library-fast-with-progress (library-path search-term / csv-data header records results row-index max-results total-records desc-index mfr-index nsn-index search-term-upper recs record description manufacturer nsn-part)
  "Fast search with real-time progress updates in dialog (no hard exits)"
  
  ;; Set maximum results to prevent dialog overload
  (setq max-results 50)
  
  (setq csv-data (MBS:read-csv library-path))
  (if (not csv-data)
    (progn
      (set_tile "status" "❌ Failed to read library file")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      (setq total-records (length records))
      (setq results '())
      (setq row-index 0)
      
      ;; Get column indices once
      (setq desc-index (get-column-index header "DESCRIPTION"))
      (setq mfr-index (get-column-index header "MANUFACTURER"))
      (setq nsn-index (get-column-index header "NSN OR MFR PART NO."))
      
      ;; Convert search term to uppercase once
      (setq search-term-upper (strcase search-term))
      
      ;; Show progress for large files
      (if (> total-records 1000)
        (set_tile "status" (strcat "📊 Searching " (itoa total-records) " records..."))
      )
      
      ;; Search through records with early termination (replace (exit) with loop condition)
      (setq recs records)
      (while (and recs (< (length results) max-results))
        (setq row-index (1+ row-index))
        (setq record (car recs))
        
        ;; Show progress every 500 records for large files
        (if (and (> total-records 1000) (= (rem row-index 500) 0))
          (set_tile "status" (strcat "📊 Progress: " (itoa row-index) "/" (itoa total-records) " records"))
        )
        
        ;; Extract fields once
        (setq description (if desc-index (nth desc-index record) ""))
        (setq manufacturer (if mfr-index (nth mfr-index record) ""))
        (setq nsn-part (if nsn-index (nth nsn-index record) ""))
        
        ;; Optimized string search - convert to uppercase once
        (if (or 
              (and description (/= description "") (vl-string-search search-term-upper (strcase description)))
              (and manufacturer (/= manufacturer "") (vl-string-search search-term-upper (strcase manufacturer)))
              (and nsn-part (/= nsn-part "") (vl-string-search search-term-upper (strcase nsn-part)))
            )
          (setq results (append results (list (list row-index record library-path))))
        )
        
        (setq recs (cdr recs))
      )
      (if (>= (length results) max-results)
        (set_tile "status" (strcat "⚠ Search limited to first " (itoa max-results) " results"))
      )
      
      (set_tile "status" (strcat "✅ Found " (itoa (length results)) " matches"))
      results
    )
  )
)

;; Enhanced optimized search with progress updates
(defun MBS:search-library-optimized-with-progress (library-path search-term / csv-data header records results row-index max-results desc-index mfr-index nsn-index search-term-upper recs record description manufacturer nsn-part)
  "Optimized search with real-time progress updates in dialog (no hard exits)"
  
  ;; Set maximum results to prevent dialog overload
  (setq max-results 100)
  
  (setq csv-data (MBS:read-csv library-path))
  (if (not csv-data)
    (progn
      (set_tile "status" "❌ Failed to read library file")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      (setq results '())
      (setq row-index 0)
      
      ;; Get column indices once
      (setq desc-index (get-column-index header "DESCRIPTION"))
      (setq mfr-index (get-column-index header "MANUFACTURER"))
      (setq nsn-index (get-column-index header "NSN OR MFR PART NO."))
      
      ;; Convert search term to uppercase once
      (setq search-term-upper (strcase search-term))
      
      ;; Search through records with early termination (replace (exit) with loop condition)
      (setq recs records)
      (while (and recs (< (length results) max-results))
        (setq row-index (1+ row-index))
        (setq record (car recs))
        
        ;; Extract fields once
        (setq description (if desc-index (nth desc-index record) ""))
        (setq manufacturer (if mfr-index (nth mfr-index record) ""))
        (setq nsn-part (if nsn-index (nth nsn-index record) ""))
        
        ;; Optimized string search - convert to uppercase once
        (if (or 
              (and description (/= description "") (vl-string-search search-term-upper (strcase description)))
              (and manufacturer (/= manufacturer "") (vl-string-search search-term-upper (strcase manufacturer)))
              (and nsn-part (/= nsn-part "") (vl-string-search search-term-upper (strcase nsn-part)))
            )
          (setq results (append results (list (list row-index record library-path))))
        )
        
        (setq recs (cdr recs))
      )
      (if (>= (length results) max-results)
        (set_tile "status" (strcat "⚠ Search limited to first " (itoa max-results) " results"))
      )
      
      (set_tile "status" (strcat "✅ Found " (itoa (length results)) " matches"))
      results
    )
  )
)

;; ========================================================================
;; DIALOG INTERFACE
;; ========================================================================

;; Main dialog command
(defun C:MBS-LibraryDialog ()
  "Show the library import dialog"
  (prompt "\n📚 === MATERIAL LIBRARY DIALOG ===")
  
  ;; Check for LOR mode warning
  (if (not (MBS:check-lor-mode-warning))
    (progn
      (princ)
    )
    (progn
      ;; Check if we have a current CSV file
      (setq current-csv-path (get-csv-path-from-block nil))
      (if (not current-csv-path)
        (progn
          (prompt "\n❌ No current CSV file found")
          (prompt "\n  Please use 'Select or Create CSV Files' first")
          (princ)
        )
        (progn
          ;; Show the dialog
          (MBS:show-library-dialog)
        )
      )
    )
  )
  (princ)
)

;; Show the main library dialog
(defun MBS:show-library-dialog ( / dcl_id)
  "Show the main library import dialog"
  
  ;; Load the DCL file
  (setq dcl_id (load_dialog "mbs_library.dcl"))
  (if (not dcl_id)
    (progn
      (prompt "\n❌ Failed to load dialog file: mbs_library.dcl")
      (prompt "\n  Falling back to command-line interface...")
      (C:MBS-MaterialLibrary)
      (princ)
    )
    (progn
      ;; Show the dialog
      (if (new_dialog "library_import" dcl_id)
        (progn
          ;; Initialize dialog with current settings
          (MBS:init-library-dialog)
          
          ;; Set up action handlers
          (action_tile "search" "(MBS:handle-search)")
          (action_tile "import" "(MBS:handle-import)")
          (action_tile "browse1" "(MBS:handle-browse 1)")
          (action_tile "browse2" "(MBS:handle-browse 2)")
          (action_tile "browse3" "(MBS:handle-browse 3)")
          ;; Instant-persist toggles for library enable flags
          (action_tile "lib1" "(MBS:handle-toggle-lib 1)")
          (action_tile "lib2" "(MBS:handle-toggle-lib 2)")
          (action_tile "lib3" "(MBS:handle-toggle-lib 3)")
          (action_tile "select_all" "(MBS:handle-select-all)")
          (action_tile "clear_all" "(MBS:handle-clear-all)")
          (action_tile "cancel" "(MBS:handle-cancel)")
          
          ;; Show dialog and get result
          (setq result (start_dialog))
          
          ;; Handle dialog result
          (if (= result 1)
            (progn
              (prompt "\n✅ Dialog completed successfully")
              ;; Save checkbox states only on successful completion
              (MBS:save-library-checkbox-states)
              ;; Import selected materials after dialog closes
              (if (and MBS:selected-import-indices MBS:selected-import-results MBS:selected-import-csv-path)
                (MBS:import-selected-from-dialog MBS:selected-import-indices MBS:selected-import-results MBS:selected-import-csv-path)
              )
            )
            (prompt "\n❌ Dialog cancelled")
          )
        )
        (progn
          (prompt "\n❌ Failed to create dialog")
          (prompt "\n  Falling back to command-line interface...")
          (C:MBS-MaterialLibrary)
        )
      )
      
      ;; Clean up
      (unload_dialog dcl_id)
    )
  )
)

;; Initialize dialog with current settings
(defun MBS:init-library-dialog ( / settings block lib1-name lib2-name lib3-name)
  "Initialize dialog with current library settings"
  
  ;; Get current settings
  (setq settings (MBS:get-library-settings))
  
  ;; Set library path fields
  (set_tile "lib1_path" (nth 0 settings))
  (set_tile "lib2_path" (nth 1 settings))
  (set_tile "lib3_path" (nth 2 settings))
  
  ;; Extract filenames for labels
  (setq lib1-name (MBS:get-filename-from-path (nth 0 settings)))
  (setq lib2-name (MBS:get-filename-from-path (nth 1 settings)))
  (setq lib3-name (MBS:get-filename-from-path (nth 2 settings)))
  
  ;; Update checkbox labels with filenames
  (set_tile "lib1_label" (if lib1-name lib1-name "LIBRARY 1"))
  (set_tile "lib2_label" (if lib2-name lib2-name "LIBRARY 2"))
  (set_tile "lib3_label" (if lib3-name lib3-name "LIBRARY 3"))
  
  ;; Get checkbox states from settings block
  (setq block (MBS:find-settings-block))
  (if block
    (progn
      ;; Set library toggles from saved states
      (set_tile "lib1" (if (= (MBS:get-block-attribute block "LIBRARY1_ENABLE") "1") "1" "0"))
      (set_tile "lib2" (if (= (MBS:get-block-attribute block "LIBRARY2_ENABLE") "1") "1" "0"))
      (set_tile "lib3" (if (= (MBS:get-block-attribute block "LIBRARY3_ENABLE") "1") "1" "0"))
    )
    (progn
      ;; Fallback: Set library toggles based on whether paths exist
      (set_tile "lib1" (if (/= (nth 0 settings) "") "1" "0"))
      (set_tile "lib2" (if (/= (nth 1 settings) "") "1" "0"))
      (set_tile "lib3" (if (/= (nth 2 settings) "") "1" "0"))
    )
  )
  
  (prompt "\n🔧 Dialog initialized with current settings")
)

;; Handle checkbox toggle: instantly persist LIBRARYx_ENABLE to settings block
(defun MBS:handle-toggle-lib (lib-index / state block attr-name ok)
  (setq state (get_tile (strcat "lib" (itoa lib-index))))
  (setq attr-name (strcat "LIBRARY" (itoa lib-index) "_ENABLE"))
  (setq block (MBS:find-settings-block))
  (if block
    (progn
      (setq ok (MBS:set-block-attribute block attr-name (if state state "0")))
      (if ok
        (set_tile "status" (strcat "Saved " attr-name ": " (if state state "0")))
        (set_tile "status" (strcat "Failed to save " attr-name))
      )
      ;; keep the in-memory snapshot in sync for later OK save
      (setq MBS:dialog-checkbox-states (list (get_tile "lib1") (get_tile "lib2") (get_tile "lib3")))
    )
    (set_tile "status" "⚠ Settings block not found; cannot save checkbox state")
  )
  (princ)
)

;; Helper: confirm continuation for large result sets
(defun MBS:confirm-continue-large-results (count)
  (if (> count 150)
    (progn
      (setq msg (strcat "Your search returned " (itoa count) " results.\n"
                         "For better performance, try a more specific search.\n\n"
                         "Press C to continue anyway, or ESC to cancel."))
      ;; Prefer dialog alert then kword fallback
      (alert msg)
      (initget "Continue")
      (setq choice (getkword "\nContinue anyway? [C]ontinue/ESC: "))
      (= choice "Continue")
    )
    T
  )
)

;; Update: Handle search button with safeguards
(defun MBS:handle-search ( / search-term library-paths results)
  "Handle search button click with input validations"
  
  ;; Normalize search term once
  (setq search-term (vl-string-trim " \t\r\n" (get_tile "search_field")))
  
  (if (or (not search-term) (= search-term ""))
    (progn
      (alert "Please enter a search term before searching.")
      (set_tile "status" "❌ Enter a search term and try again")
      (princ)
    )
    (progn
      ;; Only proceed when we have a non-empty search term
      (setq library-paths (MBS:get-selected-libraries))
      (if (not library-paths)
        (progn
          (alert "Please select at least one library to search.")
          (set_tile "status" "❌ Select at least one library and try again")
          (princ)
        )
        (progn
          ;; Perform search with enhanced status updates
          (prompt (strcat "\n🔍 Searching for: '" search-term "'"))
          (setq results (MBS:search-multiple-libraries-enhanced search-term library-paths))
          
          ;; Safeguard: too many results prompt
          (if (not (MBS:confirm-continue-large-results (length results)))
            (progn
              (set_tile "status" "⚠ Search cancelled. Refine your criteria and try again.")
              (princ)
            )
            (progn
              ;; Update results list with progress indication
              (MBS:update-results-list-ultra-fast results)
            )
          )
        )
      )
    )
  )
)

;; Handle import button
(defun MBS:handle-import ( / selected-indices results current-csv-path)
  "Handle import button click"
  
  ;; Get selected items from results list
  (setq selected-indices (MBS:get-selected-results))
  (if (not selected-indices)
    (progn
      (alert "Please select at least one material to import")
      (exit)
    )
  )
  
  ;; Get the search results (we'll need to store these globally)
  (setq results MBS:dialog-search-results)
  
  ;; Get current CSV path
  (setq current-csv-path (get-csv-path-from-block nil))
  
  ;; Store data in global variables for processing after dialog closes
  (setq MBS:selected-import-indices selected-indices)
  (setq MBS:selected-import-results results)
  (setq MBS:selected-import-csv-path current-csv-path)
  
  ;; Store checkbox states before closing dialog
  (setq MBS:dialog-checkbox-states (list (get_tile "lib1") (get_tile "lib2") (get_tile "lib3")))
  
  ;; Close dialog - import will be handled after dialog closes
  (done_dialog 1)
)

;; Save library checkbox states to settings block
(defun MBS:save-library-checkbox-states ( / block lib1-state lib2-state lib3-state)
  "Save the current checkbox states to the settings block"
  
  (prompt "\n🔧 === SAVING CHECKBOX STATES ===")
  
  ;; Get current checkbox states from stored values
  (if MBS:dialog-checkbox-states
    (progn
      (setq lib1-state (nth 0 MBS:dialog-checkbox-states))
      (setq lib2-state (nth 1 MBS:dialog-checkbox-states))
      (setq lib3-state (nth 2 MBS:dialog-checkbox-states))
    )
    (progn
      ;; Fallback to reading from dialog (may not work if dialog is closed)
      (setq lib1-state (get_tile "lib1"))
      (setq lib2-state (get_tile "lib2"))
      (setq lib3-state (get_tile "lib3"))
    )
  )
  
  (prompt (strcat "\n📊 Current checkbox states:"))
  (prompt (strcat "\n  lib1: '" (if lib1-state lib1-state "NIL") "'"))
  (prompt (strcat "\n  lib2: '" (if lib2-state lib2-state "NIL") "'"))
  (prompt (strcat "\n  lib3: '" (if lib3-state lib3-state "NIL") "'"))
  
  (setq block (MBS:find-settings-block))
  (if block
    (progn
      (prompt "\n✅ Found settings block")
      
      ;; Try to save checkbox states with detailed feedback
      (prompt "\n🔧 Writing LIBRARY1_ENABLE...")
      (setq result1 (MBS:set-block-attribute block "LIBRARY1_ENABLE" lib1-state))
      (prompt (strcat "\n  Result: " (if result1 "SUCCESS" "FAILED")))
      
      (prompt "\n🔧 Writing LIBRARY2_ENABLE...")
      (setq result2 (MBS:set-block-attribute block "LIBRARY2_ENABLE" lib2-state))
      (prompt (strcat "\n  Result: " (if result2 "SUCCESS" "FAILED")))
      
      (prompt "\n🔧 Writing LIBRARY3_ENABLE...")
      (setq result3 (MBS:set-block-attribute block "LIBRARY3_ENABLE" lib3-state))
      (prompt (strcat "\n  Result: " (if result3 "SUCCESS" "FAILED")))
      
      ;; Show summary
      (prompt "\n📊 SAVE SUMMARY:")
      (prompt (strcat "\n  LIBRARY1_ENABLE: " (if result1 "SUCCESS" "FAILED")))
      (prompt (strcat "\n  LIBRARY2_ENABLE: " (if result2 "SUCCESS" "FAILED")))
      (prompt (strcat "\n  LIBRARY3_ENABLE: " (if result3 "SUCCESS" "FAILED")))
    )
    (prompt "\n❌ No settings block found")
  )
)

;; Get selected libraries from dialog
(defun MBS:get-selected-libraries ( / settings selected)
  "Get list of selected library paths from dialog"
  
  (setq settings (MBS:get-library-settings))
  (setq selected '())
  
  ;; Check which libraries are selected
  (if (= (get_tile "lib1") "1")
    (setq selected (append selected (list (nth 0 settings)))))
  (if (= (get_tile "lib2") "1")
    (setq selected (append selected (list (nth 1 settings)))))
  (if (= (get_tile "lib3") "1")
    (setq selected (append selected (list (nth 2 settings)))))
  
  ;; Filter out empty paths and non-existent files (suppress warnings for unselected libraries)
  (setq selected (vl-remove-if 
    '(lambda (x) 
       (or 
         (not x) 
         (= x "") 
         (not (findfile x))
       )
     ) 
    selected
  ))
  
  selected
)

;; Ultra-fast update that chooses the fastest method for large result sets
(defun MBS:update-results-list-ultra-fast (results / result-count)
  "Ultra-fast update that chooses the fastest method based on result count"
  
  (setq result-count (length results))
  
  ;; Hard cap safeguard: enforce 150 results max in UI
  (if (> result-count 150)
    (setq result-count 150)
  )
  
  ;; Choose strategy based on result count
  (cond
    ;; Small result set - use original method (for compatibility)
    ((<= result-count 10)
     (MBS:update-results-list results)
    )
    ;; Medium result set - use fast direct method
    ((<= result-count 100)
     (MBS:update-results-list-fast-direct results)
    )
    ;; Large result set - use ultra-fast method with limiting
    (t
     (MBS:update-results-list-fast results)
    )
  )
)

;; Ultra-fast results display - direct column concatenation
(defun MBS:update-results-list-fast-direct (results / result-list display-string)
  "Ultra-fast update using direct column concatenation"
  
  ;; Store results globally for import
  (setq MBS:dialog-search-results results)
  
  ;; Update status immediately
  (set_tile "status" (strcat "📊 Loading " (itoa (length results)) " results..."))
  
  ;; Build list directly from record data
  (setq result-list '())
  ;; cap displayed items at 150 for UI speed
  (setq display-cap (min 150 (length results)))
  (foreach result results
    (setq row-index (nth 0 result))
    (setq record (nth 1 result))
    (setq library-path (nth 2 result))
    
    ;; Direct column access (assuming standard library format)
    ;; Column 1: DESCRIPTION, Column 3: MANUFACTURER, Column 4: NSN/PART, Column 5: MATERIAL SPEC
    (setq description (nth 1 record))  ;; DESCRIPTION
    (setq manufacturer (nth 3 record)) ;; MANUFACTURER  
    (setq nsn-part (nth 4 record))    ;; NSN OR MFR PART NO.
    (setq material-spec (nth 5 record)) ;; MATERIAL SPEC (if available)
    
    ;; Simple concatenation without complex string building
    (setq display-string (strcat description " (" manufacturer " - " nsn-part " - " material-spec ")"))
    (setq result-list (append result-list (list display-string)))
  )
  
  ;; Update dialog list in one operation
  (start_list "results")
  (mapcar 'add_list result-list)
  (end_list)
  
  ;; Update final status
  (set_tile "status" (strcat "✅ Results loaded: " (itoa (length MBS:dialog-search-results)) " matches"))
  
  (MBS:log-info (strcat "\n✅ Fast results display completed: " (itoa (length results)) " items"))
)

;; Fast update for very large result sets
(defun MBS:update-results-list-fast (results / result-list max-display)
  "Fast update for very large result sets with display limiting"
  
  ;; Set maximum items to display in dialog
  (setq max-display 150)
  
  ;; Store results globally for import
  (setq MBS:dialog-search-results results)
  
  ;; Limit results for display if too many
  (if (> (length results) max-display)
    (progn
      (MBS:log-warning (strcat "\n⚠ Limiting display to first " (itoa max-display) " results"))
      (set_tile "status" (strcat "⚠ Limiting display to first " (itoa max-display) " results"))
      (setq display-results (take results max-display))
    )
    (setq display-results results)
  )
  
  ;; Build list for dialog efficiently
  (setq result-list '())
  (foreach result display-results
    (setq row-index (nth 0 result))
    (setq record (nth 1 result))
    (setq library-path (nth 2 result))
    (setq description (nth 1 record))
    (setq manufacturer (nth 3 record))
    (setq nsn-part (nth 4 record))
    
    ;; Try to pull MATERIAL SPEC if present at expected position (fast path)
    (setq material-spec (if (> (length record) 5) (nth 5 record) ""))
    
    ;; Build display string efficiently
    (setq display-string (strcat description " (" manufacturer " - " nsn-part " - " material-spec ")"))
    (setq result-list (append result-list (list display-string)))
  )
  
  ;; Update dialog list efficiently
  (start_list "results")
  (mapcar 'add_list result-list)
  (end_list)
  
  ;; Update status with full count
  (set_tile "status" (strcat "✅ Results loaded: " (itoa (length results)) " matches (showing " (itoa (length display-results)) ")"))
  
  (MBS:log-info (strcat "\n✅ Updated results list with " (itoa (length results)) " items (displayed " (itoa (length display-results)) ")"))
)

;; Smart update that chooses strategy based on result count
(defun MBS:update-results-list-smart (results / result-count)
  "Smart update that chooses strategy based on result count"
  
  (setq result-count (length results))
  
  ;; Choose strategy based on result count
  (cond
    ;; Small result set - use original method
    ((<= result-count 20)
     (MBS:update-results-list results)
    )
    ;; Medium result set - use optimized method
    ((<= result-count 100)
     (MBS:update-results-list-optimized results)
    )
    ;; Large result set - use fast method with limiting
    (t
     (MBS:update-results-list-fast results)
    )
  )
)

;; Update results list in dialog (original function)
(defun MBS:update-results-list (results / result-list)
  "Update the results list in the dialog"
  
  ;; Store results globally for import
  (setq MBS:dialog-search-results results)
  
  ;; Build list for dialog
  (setq result-list '())
  (foreach result results
    (setq row-index (nth 0 result))
    (setq record (nth 1 result))
    (setq library-path (nth 2 result))
    (setq description (nth 1 record))
    (setq manufacturer (nth 3 record))
    (setq nsn-part (nth 4 record))
    ;; Get MATERIAL SPEC dynamically using column index
    (setq library-header (car (MBS:read-csv library-path)))
    (setq material-spec-index (get-column-index library-header "MATERIAL SPEC"))
    (setq material-spec (if (and material-spec-index (>= material-spec-index 0)) (nth material-spec-index record) ""))
    (setq result-list (append result-list (list (strcat description " (" manufacturer " - " nsn-part " - " material-spec ")"))))
  )
  
  ;; Update dialog list
  (start_list "results")
  (mapcar 'add_list result-list)
  (end_list)
  
  ;; Update status
  (set_tile "status" (strcat "Found " (itoa (length results)) " matches"))
  
  (MBS:log-info (strcat "\n✅ Updated results list with " (itoa (length results)) " items"))
)

;; Get selected results from dialog
(defun MBS:get-selected-results ( / selected result)
  "Get list of selected result indices from dialog"
  
  (setq selected (get_tile "results"))
  (prompt (strcat "\n[DEBUG] Raw selection from dialog: '" selected "'"))
  
  (if (and selected (/= selected ""))
    (progn
      ;; Parse space-separated list properly
      (setq result (MBS:parse-selection-string selected))
      (prompt (strcat "\n[DEBUG] Parsed result: " (vl-princ-to-string result)))
      result
    )
    (progn
      (prompt "\n[DEBUG] No selection found, returning empty list")
      '()
    )
  )
)

;; Import selected materials from dialog
(defun MBS:import-selected-from-dialog (selected-indices results current-csv-path / imported-count)
  "Import selected materials from dialog results"
  
  (prompt "\n📥 === IMPORTING SELECTED MATERIALS ===")
  
  (setq imported-count 0)
  (foreach index selected-indices
    (setq material-pair (nth index results))
    (setq row-index (nth 0 material-pair))
    (setq library-record (nth 1 material-pair))
    (setq library-path (nth 2 material-pair))
    
    ;; Generate new ID
    (setq current-mode (MBS:get-mode))
    (setq id-prefix (if (= current-mode "LOR") "REM" "MAT"))
    (setq new-id (MBS:generate-new-material-id id-prefix))
    
    ;; Create new record
    (setq new-record (MBS:create-library-import-record library-record new-id current-mode library-path))
    
    ;; Add to CSV
    (if (MBS:add-record-to-csv current-csv-path new-record)
      (progn
        (setq imported-description (nth 3 new-record))
        (prompt (strcat "\n✅ Imported: " imported-description))
        
        ;; Insert block
        (setq block-name (if (= current-mode "LOR") "_CACI_REMOVALLEADER" "_CACI_ITEMLEADER"))
        (if (boundp 'MBS:insert-block-for-new-material)
          (setq block-result (MBS:insert-block-for-new-material new-id imported-description current-csv-path block-name))
          (setq block-result nil)
        )
        
        (if block-result
          (progn
            (prompt (strcat "\n✅ Block inserted for: " imported-description))
            (setq imported-count (1+ imported-count))
          )
          (progn
            (prompt (strcat "\n⚠ Block insertion failed for: " imported-description))
            (setq imported-count (1+ imported-count))
          )
        )
      )
      (progn
        (setq imported-description (nth 3 new-record))
        (prompt (strcat "\n❌ Failed to import: " imported-description))
      )
    )
  )
  
  ;; Update item numbers and sync blocks
  (if (> imported-count 0)
    (progn
      (prompt (strcat "\n🔄 Updating item numbers for " (itoa imported-count) " imported materials..."))
      
      ;; Call MBS-UPDATE-SIMPLE to properly populate ITEM NO fields
      (prompt "\n🔄 Running MBS-UPDATE-SIMPLE to populate item numbers...")
      (C:MBS-UPDATE-SIMPLE)
      
      (prompt "\n🔄 Synchronizing blocks...")
      (MBS:sync-blocks-with-csv current-csv-path)
      
      (prompt (strcat "\n🎉 Successfully imported " (itoa imported-count) " materials!"))
    )
    (prompt "\n❌ No materials were imported")
  )
)

;; ========================================================================
;; HELPER FUNCTIONS
;; ========================================================================

;; Find settings block
(defun MBS:find-settings-block ( / ss i block)
  "Find the _MBS_SETTINGS_MARKER block"
  
  (setq ss (ssget "X" '((0 . "INSERT") (2 . "_MBS_SETTINGS_MARKER"))))
  (if ss
    (progn
      (setq i 0)
      (setq block (entget (ssname ss i)))
      block
    )
    nil
  )
)

;; Get block attribute value
(defun MBS:get-block-attribute (block attribute-name / attrs attr result)
  "Get attribute value from block"
  
  (setq attrs (entget (entnext (cdr (assoc -1 block)))))
  (setq result "")
  (while (and attrs (/= (cdr (assoc 0 attrs)) "SEQEND"))
    (if (= (cdr (assoc 2 attrs)) attribute-name)
      (setq result (cdr (assoc 1 attrs)))
    )
    (setq attrs (entget (entnext (cdr (assoc -1 attrs)))))
  )
  result
)

;; Set block attribute value
(defun MBS:set-block-attribute (block attribute-name value / attrs result found)
  "Set attribute value in block"
  
  ;; Use vl-catch-all-apply to handle errors gracefully
  (setq result (vl-catch-all-apply
    '(lambda ()
      (setq attrs (entget (entnext (cdr (assoc -1 block)))))
      (setq found nil)
      
      ;; First, try to find and update existing attribute
      (while (and attrs (/= (cdr (assoc 0 attrs)) "SEQEND"))
        (if (= (cdr (assoc 2 attrs)) attribute-name)
          (progn
            ;; Create new attribute list with updated value
            (setq new-attrs (subst (cons 1 value) (assoc 1 attrs) attrs))
            ;; Update the attribute
            (if (entmod new-attrs)
              (setq found t)
              (prompt (strcat "\n⚠ Failed to update attribute: " attribute-name))
            )
          )
        )
        (setq attrs (entget (entnext (cdr (assoc -1 attrs)))))
      )
      
      ;; If attribute wasn't found, skip it for now
      (if (not found)
        (prompt (strcat "\n⚠ Attribute not found: " attribute-name))
      )
      
      found
    )
  ))
  
  ;; Return result (t if successful, nil if failed)
  (if (vl-catch-all-error-p result)
    (progn
      (prompt (strcat "\n❌ Error updating attribute: " attribute-name))
      nil
    )
    result
  )
)

;; ========================================================================
;; HELPER FUNCTIONS FOR DIALOG
;; ========================================================================

;; Update a list at a specific index
(defun MBS:update-list (lst index new-value / result i)
  "Update a list at a specific index"
  
  (setq result '())
  (setq i 0)
  (foreach item lst
    (if (= i index)
      (setq result (append result (list new-value)))
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  result
)

;; Parse selection string from dialog
(defun MBS:parse-selection-string (selection-string / result tokens token)
  "Parse space-separated selection string into list of integers"
  
  (setq result '())
  (setq tokens (MBS:split-string selection-string " "))
  
  (foreach token tokens
    (if (and token (/= token ""))
      (setq result (append result (list (atoi token))))
    )
  )
  result
)

;; Split string by delimiter
(defun MBS:split-string (str delimiter / result pos)
  "Split string by delimiter"
  
  (setq result '())
  (while (and str (/= str ""))
    (setq pos (vl-string-search delimiter str))
    (if pos
      (progn
        (setq result (append result (list (substr str 1 pos))))
        (setq str (substr str (+ pos 2)))
      )
      (progn
        (setq result (append result (list str)))
        (setq str "")
      )
    )
  )
  result
)

;; Helper function to extract filename from path
(defun MBS:get-filename-from-path (file-path / filename)
  "Extract filename from full file path"
  
  (if (and file-path (/= file-path ""))
    (progn
      ;; Find the last backslash or forward slash
      (setq filename file-path)
      (while (and filename (vl-string-search "\\" filename))
        (setq filename (substr filename (+ (vl-string-search "\\" filename) 2)))
      )
      (while (and filename (vl-string-search "/" filename))
        (setq filename (substr filename (+ (vl-string-search "/" filename) 2)))
      )
      ;; Remove file extension if present
      (if (vl-string-search "." filename)
        (setq filename (substr filename 1 (vl-string-search "." filename)))
      )
      filename
    )
    nil
  )
)

;; Handle cancel button
(defun MBS:handle-cancel ( / )
  "Handle cancel button click"
  
  ;; Store checkbox states before closing dialog
  (setq MBS:dialog-checkbox-states (list (get_tile "lib1") (get_tile "lib2") (get_tile "lib3")))
  
  ;; Close dialog
  (done_dialog 0)
)

;; Handle browse button
(defun MBS:handle-browse (library-number / file-path settings)
  "Handle browse button click"
  
  ;; Get file path from user
  (setq file-path (getfiled "Select Library CSV File" "" "csv" 0))
  
  (if file-path
    (progn
      ;; Get current settings
      (setq settings (MBS:get-library-settings))
      
      ;; Ensure we have 3 settings (pad if needed)
      (while (< (length settings) 3)
        (setq settings (append settings (list "")))
      )
      
      ;; Update the appropriate library path
      (setq settings (MBS:update-list settings (1- library-number) file-path))
      
      ;; Save settings to block
      (if (MBS:set-library-settings settings)
        (progn
          ;; Update dialog display
          (set_tile (strcat "lib" (itoa library-number) "_path") file-path)
          (set_tile (strcat "lib" (itoa library-number)) "1")
          
          ;; Update status
          (set_tile "status" (strcat "Library " (itoa library-number) " set to: " (vl-filename-base file-path)))
          (prompt (strcat "\n✅ Library " (itoa library-number) " set to: " file-path))
        )
        (prompt "\n❌ Failed to save library settings")
      )
    )
    (prompt "\n❌ No file selected")
  )
)

;; Handle select all button
(defun MBS:handle-select-all ( / count i)
  "Handle select all button click"
  
  ;; Get the number of items in the list
  (setq count (length MBS:dialog-search-results))
  
  ;; Build selection string (0-based indices)
  (setq selection "")
  (setq i 0)
  (while (< i count)
    (setq selection (strcat selection (itoa i)))
    (setq i (1+ i))
    (if (< i count)
      (setq selection (strcat selection " "))
    )
  )
  
  ;; Set the selection
  (set_tile "results" selection)
  
  (prompt (strcat "\n✅ Selected all " (itoa count) " results"))
)

;; Handle clear all button
(defun MBS:handle-clear-all ( / )
  "Handle clear all button click"
  
  ;; Clear the selection
  (set_tile "results" "")
  
  (prompt "\n✅ Cleared all selections")
)

;; ----------------------------------------------------------------------------
;; Help file generator
;; ----------------------------------------------------------------------------
(defun MBS:write-help-file (command-name filename description / path f now)
  (setq path (strcat (getvar "TEMPPREFIX") filename))
  (setq now (menucmd "m=$(edtime,$(getvar,date),DD-MON-YYYY HH:MM)"))
  (setq f (open path "w"))
  (if f
    (progn
      (write-line "<!DOCTYPE html>" f)
      (write-line "<html>" f)
      (write-line "<head>" f)
      (write-line (strcat "    <title>" command-name " - AutoCAD Script Help</title>") f)
      (write-line "    <style>" f)
      (write-line "        body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }" f)
      (write-line "        .container { max-width: 800px; margin: 0 auto; background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }" f)
      (write-line "        h1 { color: #2c5aa0; border-bottom: 2px solid #2c5aa0; padding-bottom: 10px; }" f)
      (write-line "        h2 { color: #666; margin-top: 30px; }" f)
      (write-line "        .command { background: #e8f4fd; padding: 10px; border-left: 4px solid #2c5aa0; margin: 10px 0; }" f)
      (write-line "        .example { background: #f8f8f8; padding: 15px; border: 1px solid #ddd; margin: 10px 0; font-family: monospace; }" f)
      (write-line "        .warning { background: #fff3cd; padding: 10px; border-left: 4px solid #ffc107; margin: 10px 0; }" f)
      (write-line "        .tip { background: #d1ecf1; padding: 10px; border-left: 4px solid #17a2b8; margin: 10px 0; }" f)
      (write-line "        ul, ol { line-height: 1.6; }" f)
      (write-line "        code { background: #f1f1f1; padding: 2px 5px; border-radius: 3px; }" f)
      (write-line "        table { width: 100%; border-collapse: collapse; margin: 10px 0; }" f)
      (write-line "        th, td { padding: 8px; text-align: left; border: 1px solid #ddd; }" f)
      (write-line "        th { background: #f1f1f1; }" f)
      (write-line "    </style>" f)
      (write-line "</head>" f)
      (write-line "<body>" f)
      (write-line "    <div class=\"container\">" f)
      (write-line (strcat "        <h1>" command-name "</h1>") f)
      (write-line "        " f)
      (write-line "        <div class=\"command\">" f)
      (write-line (strcat "            <strong>Command:</strong> " command-name "<br>") f)
      (write-line (strcat "            <strong>Script File:</strong> " filename "<br>") f)
      (write-line (strcat "            <strong>Description:</strong> " description) f)
      (write-line "        </div>" f)
      (write-line "        " f)
      (write-line "        <h2>&#128195 Overview</h2>" f)
      (write-line "        <p>This script provides functionality for [detailed description of what this script does].</p>" f)
      (write-line "        <p>Use this command when you need to [specific use cases].</p>" f)
      (write-line "        " f)
      (write-line "        <h2>&#128195 Usage Instructions</h2>" f)
      (write-line "        <ol>" f)
      (write-line (strcat "            <li>Type <code>" command-name "</code> in the AutoCAD command line</li>") f)
      (write-line "            <li>Follow the command prompts that appear</li>" f)
      (write-line "            <li>[Add specific steps here]</li>" f)
      (write-line "            <li>The script will complete the operation</li>" f)
      (write-line "        </ol>" f)
      (write-line "        " f)
      (write-line "        <div class=\"tip\">" f)
      (write-line "            <strong>&#128161 Tip:</strong> [Add helpful tips here]" f)
      (write-line "        </div>" f)
      (write-line "        " f)
      (write-line "        <h2>&#128295 Available Options</h2>" f)
      (write-line "        <ul>" f)
      (write-line "            <li><strong>Option 1:</strong> [Description of option]</li>" f)
      (write-line "            <li><strong>Option 2:</strong> [Description of option]</li>" f)
      (write-line "            <li><strong>Option 3:</strong> [Description of option]</li>" f)
      (write-line "        </ul>" f)
      (write-line "        " f)
      (write-line "        <h2>&#128221 Example Usage</h2>" f)
      (write-line "        <div class=\"example\">" f)
      (write-line (strcat "Command: " command-name "<br>") f)
      (write-line "[Example command prompt]<br>" f)
      (write-line "Select objects: [User selects drawing objects]<br>" f)
      (write-line "Specify point: [User clicks location]<br>" f)
      (write-line "Operation completed successfully." f)
      (write-line "        </div>" f)
      (write-line "        " f)
      (write-line "        <h2>&#128205 Important Notes</h2>" f)
      (write-line "        <div class=\"warning\">" f)
      (write-line "            <strong>Warning:</strong> Always save your drawing before running this command." f)
      (write-line "        </div>" f)
      (write-line "        " f)
      (write-line "        <ul>" f)
      (write-line "            <li>Ensure all required objects are properly selected</li>" f)
      (write-line "            <li>Check layer settings before execution</li>" f)
      (write-line "            <li>[Add other important notes]</li>" f)
      (write-line "        </ul>" f)
      (write-line "        " f)
      (write-line "        <h2>&#128206 Troubleshooting</h2>" f)
      (write-line "        <table>" f)
      (write-line "            <tr>" f)
      (write-line "                <th>Issue</th>" f)
      (write-line "                <th>Solution</th>" f)
      (write-line "            </tr>" f)
      (write-line "            <tr>" f)
      (write-line "                <td>Command not found</td>" f)
      (write-line "                <td>Run <code>STARTUP-LOADER</code> to reload scripts</td>" f)
      (write-line "            </tr>" f)
      (write-line "            <tr>" f)
      (write-line "                <td>Script fails to execute</td>" f)
      (write-line "                <td>Check that required objects are selected</td>" f)
      (write-line "            </tr>" f)
      (write-line "            <tr>" f)
      (write-line "                <td>[Add common issue]</td>" f)
      (write-line "                <td>[Add solution]</td>" f)
      (write-line "            </tr>" f)
      (write-line "        </table>" f)
      (write-line "        " f)
      (write-line "        <h2>Related Commands</h2>" f)
      (write-line "        <ul>" f)
      (write-line "            <li><code>STARTUP-LOADER</code> - Reload all scripts</li>" f)
      (write-line "            <li><code>SCRIPT-COMMANDS</code> - List all available script commands</li>" f)
      (write-line "            <li>[Add related commands]</li>" f)
      (write-line "        </ul>" f)
      (write-line "        " f)
      (write-line "        <h2>Support Information</h2>" f)
      (write-line "        <p><strong>Script Version:</strong> [Add version info]</p>" f)
      (write-line (strcat "        <p><strong>Last Updated:</strong> " now "</p>") f)
      (write-line "        <p><strong>Help File Location:</strong> <code> TEMP</code></p>" f)
      (write-line "    </div>" f)
      (write-line "</body>" f)
      (write-line "</html>" f)
      (close f)
      path
    )
    (progn
      (prompt "\n❌ Could not create help file")
      nil
    )
  )
)

(defun C:MBS-LibraryHelp ( / path)
  (setq path (MBS:write-help-file "MBS-LibraryDialog" "MBS_Library_Help.html" "Dialog-based Material Library search and import."))
  (if path
    (progn
      (prompt (strcat "\n📖 Help written to: " path))
      (startapp path)
    )
  )
  (princ)
)

;; ========================================================================
;; INITIALIZATION
;; ========================================================================

(prompt "\n[DEBUG] Library dialog interface loaded!")
(prompt "\n[DEBUG] Available commands:")
(prompt "\n  C:MBS-LibraryDialog - Show library import dialog")
(prompt "\n  C:MBS-MaterialLibrary - Command-line interface (backup)")
(prompt "\n[DEBUG] Function C:MBS-LibraryDialog is now available")
(prompt "\n[DEBUG] End of mbs_library_dialog.lsp file")

;; Test if function is defined
(if (boundp 'C:MBS-LibraryDialog)
  (prompt "\n[DEBUG] ✅ C:MBS-LibraryDialog function is defined")
  (prompt "\n[DEBUG] ❌ C:MBS-LibraryDialog function is NOT defined")
)

(princ) 