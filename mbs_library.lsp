;;; ========================================================================
;; MATERIAL BLOCK SYNC - Library Import System
;; VERSION 5.5
;;; ========================================================================
;; This module handles importing materials from external CSV library files
;; DEPENDENCIES: This file must be loaded AFTER mbs_blocks.lsp to access
;;               MBS:insert-block-for-new-material and other block functions
;;; ========================================================================

(MBS:log-verbose "\n🔍 Starting library file load...")

;; Global variables for library management
(if (not (boundp 'MBS:active-library-path))
  (setq MBS:active-library-path nil)
)

(if (not (boundp 'MBS:library-files))
  (setq MBS:library-files '())
)

(MBS:log-verbose "\n✅ Global variables initialized")

;; ========================================================================
;; LOR MODE WARNING SYSTEM
;; ========================================================================

;; Check if user should be warned about LOR mode
(defun MBS:check-lor-mode-warning ()
  "Check if user is in LOR mode and warn them about using library functions"
  (setq current-mode (MBS:get-mode))
  
  (if (= current-mode "LOR")
    (progn
      (prompt "\n⚠ Warning! You are in LOR mode. Any material you add will be added to the List of Removals!")
      (prompt "\n  Change modes before using Library. Press C to continue anyway or hit ESC to exit.")
      
      (initget "Continue")
      (setq choice (getkword "\nContinue anyway? [C]ontinue/ESC: "))
      
      (if (= choice "Continue")
        (progn
          (prompt "\n✅ Continuing with LOR mode...")
          t
        )
        (progn
          (prompt "\n❌ Operation cancelled by user.")
          nil
        )
      )
    )
    t  ;; Not in LOR mode, no warning needed
  )
)

;; ========================================================================
;; MAIN LIBRARY COMMANDS
;; ========================================================================

(MBS:log-verbose "\n🔍 Defining C:MBS-MaterialLibrary...")
(defun C:MBS-MaterialLibrary ()
  "Import materials from external CSV library files"
  (MBS:log-info "\n📚 === MATERIAL LIBRARY IMPORT ===")
  
  ;; Check for LOR mode warning
  (if (not (MBS:check-lor-mode-warning))
    (progn
      (princ)
    )
    (progn
      ;; Initialize environment
      (if (not (MBS:init-environment))
        (progn
          (MBS:log-error "\n❌ Failed to initialize environment")
      (princ)
    )
    (progn
      ;; Check if we have a current CSV file
      (setq current-csv-path (get-csv-path-from-block nil))
      (if (not current-csv-path)
        (progn
              (MBS:log-error "\n❌ No current CSV file found")
              (MBS:log-error "\n  Please use 'Select or Create CSV Files' first")
          (princ)
        )
        (progn
          ;; Start library import workflow
          (MBS:library-import-workflow current-csv-path)
            )
          )
        )
      )
    )
  )
  (princ)
)

(MBS:log-verbose "\n✅ C:MBS-MaterialLibrary defined")

;; Main library import workflow
(MBS:log-verbose "\n🔍 Defining MBS:library-import-workflow...")
(defun MBS:library-import-workflow (current-csv-path / library-path search-term selected-materials)
  "Complete workflow for importing materials from library"
  
  ;; Step 1: Select or load library file
  (setq library-path (MBS:select-library-file))
  (if (not library-path)
    (progn
      (MBS:log-error "\n❌ No library file selected")
      (princ)
    )
    (progn
      ;; Step 2: Search library
      (setq search-term (MBS:get-library-search-term))
      (if (not search-term)
        (progn
          (MBS:log-error "\n❌ No search term provided")
          (princ)
        )
        (progn
          ;; Step 3: Search and display results
          (setq search-results (MBS:search-library current-csv-path search-term))
          (if (not search-results)
            (progn
              (MBS:log-warning "\n❌ No matching materials found")
              (princ)
            )
            (progn
              ;; Step 4: Select materials
              (setq selected-materials (MBS:select-materials-from-results search-results))
              (if (not selected-materials)
                (progn
                  (MBS:log-warning "\n❌ No materials selected")
                  (princ)
                )
                (progn
                  ;; Step 5: Preview and confirm
                  (if (MBS:preview-selected-materials selected-materials)
                    ;; Step 6: Import materials
                    (MBS:import-selected-materials selected-materials current-csv-path)
                    (MBS:log-info "\n❌ Import cancelled")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

(MBS:log-verbose "\n✅ MBS:library-import-workflow defined")

;; Select or load library file
(MBS:log-verbose "\n🔍 Defining MBS:select-library-file...")
(defun MBS:select-library-file ( / library-path)
  "Select a library file to use for import"
  
  (MBS:log-info "\n📁 === LIBRARY FILE SELECTION ===")
  
  ;; Check if we have a previously loaded library
  (if MBS:active-library-path
    (progn
      (MBS:log-info (strcat "\n📚 Active library: " MBS:active-library-path))
      (initget "Continue Switch Browse")
      (setq choice (getkword "\nUse (C)ontinue with current library, (S)witch to different file, or (B)rowse for new file: "))
      
      (cond
        ((= choice "Continue")
         MBS:active-library-path
        )
        ((or (= choice "Browse") (= choice "Switch"))
         (MBS:browse-for-library-file)
        )
        (t
         MBS:active-library-path
        )
      )
    )
    (progn
      (MBS:log-info "\n📚 No library file currently loaded")
      (MBS:browse-for-library-file)
    )
  )
)

(MBS:log-verbose "\n✅ MBS:select-library-file defined")

;; Browse for library file
(defun MBS:browse-for-library-file ( / file-path)
  "Browse and select a library CSV file"
  
  (MBS:log-info "\n📂 Browse for library CSV file...")
  (setq file-path (getfiled "Select Library CSV File" "" "csv" 0))
  
  (if file-path
    (progn
      ;; Validate the file structure
      (if (MBS:validate-library-file file-path)
        (progn
          (setq MBS:active-library-path file-path)
          (MBS:log-info (strcat "\n✅ Library loaded: " file-path))
          file-path
        )
        (progn
          (MBS:log-error "\n❌ Invalid library file format")
          (MBS:log-error "\n  Library files must have: UNITS, DESCRIPTION, MANUFACTURER, NSN OR MFR PART NO.")
          nil
        )
      )
    )
    (progn
      (MBS:log-warning "\n❌ No file selected")
      nil
    )
  )
)
(MBS:log-verbose "\n✅ MBS:browse-for-library-file defined")

;; Validate library file structure
(defun MBS:validate-library-file (file-path / csv-data header required-columns)
  "Validate that the library file has the required structure"
  
  (setq csv-data (MBS:read-csv file-path))
  (if (not csv-data)
    nil
    (progn
      (setq header (car csv-data))
      (setq required-columns '("UNITS" "DESCRIPTION" "MANUFACTURER" "NSN OR MFR PART NO."))
      
      ;; Check if all required columns exist
      (setq missing-columns '())
      (foreach col required-columns
        (if (not (get-column-index header col))
          (setq missing-columns (append missing-columns (list col)))
        )
      )
      
      (if missing-columns
        (progn
          (MBS:log-error (strcat "\n❌ Missing required columns: " (vl-string-right-trim ", " (apply 'strcat (mapcar '(lambda (x) (strcat x ", ")) missing-columns)))))
          nil
        )
        t
      )
    )
  )
)
(MBS:log-verbose "\n✅ MBS:validate-library-file defined")

;; Get search term from user
(defun MBS:get-library-search-term ( / search-term)
  "Get search term from user"
  
  (MBS:log-info "\n🔍 === LIBRARY SEARCH ===")
  (MBS:log-info "\nSearch by DESCRIPTION, MANUFACTURER, or NSN OR MFR PART NO.")
  (setq search-term (getstring t "\nEnter search term: "))
  
  (if (and search-term (/= search-term ""))
    search-term
    nil
  )
)
(MBS:log-verbose "\n✅ MBS:get-library-search-term defined")

;; Search library for matching materials
(defun MBS:search-library (library-path search-term / csv-data header records results row-index)
  "Search library file for materials matching the search term"
  
  (MBS:log-info (strcat "\n🔍 Searching library for: '" search-term "'"))
  
  (setq csv-data (MBS:read-csv library-path))
  (if (not csv-data)
    (progn
      (MBS:log-error "\n❌ Failed to read library file")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      (setq results '())
      (setq row-index 0)
      
      ;; Get column indices
      (setq desc-index (get-column-index header "DESCRIPTION"))
      (setq mfr-index (get-column-index header "MANUFACTURER"))
      (setq nsn-index (get-column-index header "NSN OR MFR PART NO."))
      
      ;; Search through all records
      (foreach record records
        (setq row-index (1+ row-index))
        (setq description (if desc-index (nth desc-index record) ""))
        (setq manufacturer (if mfr-index (nth mfr-index record) ""))
        (setq nsn-part (if nsn-index (nth nsn-index record) ""))
        
        ;; Check if search term matches any field
        (if (or 
              (and description (vl-string-search (strcase search-term) (strcase description)))
              (and manufacturer (vl-string-search (strcase search-term) (strcase manufacturer)))
              (and nsn-part (vl-string-search (strcase search-term) (strcase nsn-part)))
            )
          (setq results (append results (list (list row-index record library-path))))
        )
      )
      
      (MBS:log-info (strcat "\n✅ Found " (itoa (length results)) " matching materials"))
      results
    )
  )
)
(MBS:log-verbose "\n✅ MBS:search-library defined")

;; Display search results and get user selection
(defun MBS:select-materials-from-results (search-results / selected-indices)
  "Display search results and get user selection"
  
  (MBS:log-info "\n📋 === SEARCH RESULTS ===")
  
  ;; Display results
  (setq result-count 0)
  (foreach result-pair search-results
    (setq result-count (1+ result-count))
    (setq row-index (nth 0 result-pair))
    (setq record (nth 1 result-pair))
    (setq library-path (nth 2 result-pair))
    
    ;; Extract display fields from library record
    (setq description (nth 1 record))  ;; DESCRIPTION is column 1 in library
    (setq manufacturer (nth 3 record)) ;; MANUFACTURER is column 3 in library
    (setq nsn-part (nth 4 record))    ;; NSN OR MFR PART NO. is column 4 in library
    
    (MBS:log-info (strcat "\n" (itoa result-count) ". " description))
    (MBS:log-info (strcat "\n   Manufacturer: " manufacturer))
    (MBS:log-info (strcat "\n   NSN/Part: " nsn-part))
    (MBS:log-info "\n")
  )
  
  ;; Get user selection
  (prompt "\nSelect materials to import (comma-separated numbers, e.g., 1,3,5): ")
  (setq selection-input (getstring t))
  
  (if (and selection-input (/= selection-input ""))
    (MBS:parse-material-selection selection-input search-results)
    nil
  )
)
(MBS:log-verbose "\n✅ MBS:select-materials-from-results defined")

;; Parse user selection string - using the robust MBS:parse-selection function
(defun MBS:parse-material-selection (selection-input search-results / selected-materials)
  "Parse comma-separated selection string and return selected materials"
  
  ;; Use the existing robust parsing function from mbs_blocks.lsp
  (setq selected-materials (MBS:parse-selection selection-input search-results))
  
  selected-materials
)
(MBS:log-verbose "\n✅ MBS:parse-material-selection defined")

;; Preview selected materials
(defun MBS:preview-selected-materials (selected-materials / preview-text)
  "Show preview of selected materials and get confirmation"
  
  (MBS:log-info "\n👀 === IMPORT PREVIEW ===")
  (MBS:log-info "\nThe following materials will be imported:")
  
  (foreach material-pair selected-materials
    (setq library-record (cdr material-pair))
    (setq description (nth 1 library-record))     ;; DESCRIPTION is field 1 in library
    (setq manufacturer (nth 3 library-record))    ;; MANUFACTURER is field 3 in library
    (setq nsn-part (nth 4 library-record))       ;; NSN OR MFR PART NO. is field 4 in library
    
    (MBS:log-info (strcat "\n• " description))
    (MBS:log-info (strcat "\n  Manufacturer: " manufacturer))
    (MBS:log-info (strcat "\n  NSN/Part: " nsn-part))
  )
  
  (MBS:log-info (strcat "\n📊 Total materials to import: " (itoa (length selected-materials))))
  
  ;; Get confirmation
  (initget "Yes No")
  (setq confirm (getkword "\nProceed with import? (Yes/No): "))
  
  (= confirm "Yes")
)
(MBS:log-verbose "\n✅ MBS:preview-selected-materials defined")


;; Import selected materials to current CSV
(defun MBS:import-selected-materials (selected-materials current-csv-path / imported-count)
  "Import selected materials to current CSV and insert blocks"
  
  (MBS:log-info "\n📥 === IMPORTING MATERIALS ===")
  
  (setq imported-count 0)
  (foreach material-pair selected-materials
    (setq row-index (nth 0 material-pair))
    (setq library-record (nth 1 material-pair))
    (setq library-path (nth 2 material-pair))
    
    ;; Generate new ID using the same logic as MBS-ADD-BLOCK
    (setq current-mode (MBS:get-mode))
    (setq id-prefix (if (= current-mode "LOR") "REM" "MAT"))
    (setq new-id (MBS:generate-new-material-id id-prefix))
    
    ;; Create new record for current CSV using the entire library record
    (setq new-record (MBS:create-library-import-record library-record new-id current-mode library-path))
    
    ;; Add to current CSV using the proper CSV writing mechanism
                (if (MBS:add-record-to-csv current-csv-path new-record)
        (progn
          ;; Get description from the new record for messages
          (setq imported-description (nth 3 new-record))
          (MBS:log-info (strcat "\n✅ Imported: " imported-description))
          
          ;; Insert block for the imported material
          (setq block-name (if (= current-mode "LOR") "_CACI_REMOVALLEADER" "_CACI_ITEMLEADER"))
          
          ;; Check if block insertion function is available
          (if (boundp 'MBS:insert-block-for-new-material)
            (setq block-result (MBS:insert-block-for-new-material new-id imported-description current-csv-path block-name))
            (progn
              (MBS:log-warning "\n⚠ Block insertion function not available")
              (MBS:log-warning "\n  (Make sure mbs_blocks.lsp is loaded before mbs_library.lsp)")
              (setq block-result nil)
            )
          )
          
          (if block-result
      (progn
              (MBS:log-info (strcat "\n✅ Block inserted for: " imported-description))
        (setq imported-count (1+ imported-count))
      )
            (progn
              (MBS:log-warning (strcat "\n⚠ Block insertion failed for: " imported-description))
              (MBS:log-warning "\n  (Record was added to CSV, but no block was created)")
              (setq imported-count (1+ imported-count))  ;; Still count as imported since CSV was updated
            )
          )
        )
        (progn
          (setq imported-description (nth 3 new-record))
          (MBS:log-error (strcat "\n❌ Failed to import: " imported-description))
        )
    )
  )
  
  
  ;; Update item numbers and sync blocks
  (if (> imported-count 0)
    (progn
       (MBS:log-info (strcat "\n🔄 Updating item numbers for " (itoa imported-count) " imported materials..."))
       
       ;; Call MBS-UPDATE-SIMPLE to properly populate ITEM NO fields
       (MBS:log-info "\n🔄 Running MBS-UPDATE-SIMPLE to populate item numbers...")
       (C:MBS-UPDATE-SIMPLE)
       
       (MBS:log-info "\n🔄 Synchronizing blocks...")
      (MBS:sync-blocks-with-csv current-csv-path)
      
       (MBS:log-info (strcat "\n🎉 Successfully imported " (itoa imported-count) " materials!"))
     )
     (MBS:log-warning "\n❌ No materials were imported")
   )
)
(MBS:log-verbose "\n✅ MBS:import-selected-materials defined")


;; Create new record for library import - COLUMN-NAME BASED MAPPING
(defun MBS:create-library-import-record (library-record new-id current-mode library-path / record id-field-index library-header lom-header)
  "Create a new CSV record for imported library material with column-name-based mapping"
  
  ;; DEBUG: Check input parameters
  (MBS:log-verbose "\n🔍 DEBUG: MBS:create-library-import-record called with:")
  (MBS:log-verbose (strcat "\n  - library-record type: " (if library-record (vl-princ-to-string (type library-record)) "NIL")))
  (MBS:log-verbose (strcat "\n  - library-record length: " (if (and library-record (listp library-record)) (itoa (length library-record)) "N/A")))
  (MBS:log-verbose (strcat "\n  - new-id: '" (if new-id new-id "NIL") "'"))
  (MBS:log-verbose (strcat "\n  - current-mode: '" (if current-mode current-mode "NIL") "'"))
  
  ;; Determine ID field index based on mode
  (setq id-field-index (if (= current-mode "LOR") 13 15))
  
  ;; Create record with proper structure for current CSV (24 columns)
  (setq record (MBS:make-list 24 ""))
  
  ;; Get library header from the passed library path
  (if (not library-path)
    (progn
      (MBS:log-error "\n❌ ERROR: library-path is not provided")
      (exit)
    )
  )
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Reading library header from: " library-path))
  (setq library-header (car (MBS:read-csv library-path)))
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Library header type: " (if library-header (vl-princ-to-string (type library-header)) "NIL")))
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Library header length: " (if (and library-header (listp library-header)) (itoa (length library-header)) "N/A")))
  
  ;; Get LOM CSV header to map by column names
  (setq lom-csv-path (get-csv-path-from-block nil))
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Reading LOM header from: " lom-csv-path))
  (setq lom-header (car (MBS:read-csv lom-csv-path)))
  (MBS:log-verbose (strcat "\n🔍 DEBUG: LOM header type: " (if lom-header (vl-princ-to-string (type lom-header)) "NIL")))
  (MBS:log-verbose (strcat "\n🔍 DEBUG: LOM header length: " (if (and lom-header (listp lom-header)) (itoa (length lom-header)) "N/A")))
  
  ;; DEBUG: Show headers
  (MBS:log-verbose "\n🔍 DEBUG: Library header fields:")
  (foreach field library-header
    (MBS:log-verbose (strcat "\n  - '" field "'"))
  )
  
  (MBS:log-verbose "\n🔍 DEBUG: LOM CSV header fields:")
  (foreach field lom-header
    (MBS:log-verbose (strcat "\n  - '" field "'"))
  )
  
  ;; DEBUG: Show raw library record
  (MBS:log-verbose "\n🔍 DEBUG: Raw library record:")
  (setq field-index 0)
  (foreach field library-record
    (MBS:log-verbose (strcat "\n  [" (itoa field-index) "] '" (if field field "NIL") "'"))
    (setq field-index (1+ field-index))
  )
  
  ;; Map library fields to LOM CSV fields by column name
  (setq record (MBS:set-nth 0 record ""))  ;; ITEM NO (always empty for auto-generation)
  (setq record (MBS:set-nth 1 record ""))  ;; QTY (always empty for user to fill)
  
  ;; Safety check: ensure library-record is valid
  (if (not library-record)
    (progn
      (MBS:log-error "\n❌ ERROR: library-record is nil")
      (exit)
    )
  )
  (if (not (listp library-record))
    (progn
      (MBS:log-error "\n❌ ERROR: library-record is not a list")
      (exit)
    )
  )
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Library record length: " (itoa (length library-record))))
  
  ;; Map UNITS field
  (MBS:log-verbose "\n🔍 DEBUG: Looking for UNITS field...")
  (setq library-units-index (get-column-index library-header "UNITS"))
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Library UNITS index: " (if library-units-index (itoa library-units-index) "NIL")))
  (if (and library-units-index (>= library-units-index 0))
    (progn
      (setq library-units (nth library-units-index library-record))
      (MBS:log-verbose (strcat "\n🔍 DEBUG: Library UNITS value: '" (if library-units library-units "NIL") "'"))
      (setq lom-units-index (get-column-index lom-header "UNITS"))
      (MBS:log-verbose (strcat "\n🔍 DEBUG: LOM UNITS index: " (if lom-units-index (itoa lom-units-index) "NIL")))
      (if (and lom-units-index (>= lom-units-index 0))
        (progn
          (setq record (MBS:set-nth lom-units-index record library-units))
          (MBS:log-verbose (strcat "\n🔍 DEBUG: Mapped UNITS '" library-units "' to LOM field " (itoa lom-units-index)))
        )
        (MBS:log-warning "\n❌ DEBUG: UNITS column not found in LOM CSV")
      )
    )
    (MBS:log-warning "\n❌ DEBUG: UNITS column not found in library")
  )
  
  ;; Map DESCRIPTION field
  (MBS:log-verbose "\n🔍 DEBUG: Looking for DESCRIPTION field...")
  (setq library-desc-index (get-column-index library-header "DESCRIPTION"))
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Library DESCRIPTION index: " (if library-desc-index (itoa library-desc-index) "NIL")))
  (if (and library-desc-index (>= library-desc-index 0))
    (progn
      (setq library-description (nth library-desc-index library-record))
      (MBS:log-verbose (strcat "\n🔍 DEBUG: Library DESCRIPTION value: '" (if library-description library-description "NIL") "'"))
      (setq lom-desc-index (get-column-index lom-header "DESCRIPTION"))
      (MBS:log-verbose (strcat "\n🔍 DEBUG: LOM DESCRIPTION index: " (if lom-desc-index (itoa lom-desc-index) "NIL")))
      (if (and lom-desc-index (>= lom-desc-index 0))
        (progn
          (setq record (MBS:set-nth lom-desc-index record library-description))
          (MBS:log-verbose (strcat "\n🔍 DEBUG: Mapped DESCRIPTION '" library-description "' to LOM field " (itoa lom-desc-index)))
        )
        (MBS:log-warning "\n❌ DEBUG: DESCRIPTION column not found in LOM CSV")
      )
    )
    (MBS:log-warning "\n❌ DEBUG: DESCRIPTION column not found in library")
  )
  
  ;; Map MATERIAL SPEC field
  (setq library-spec-index (get-column-index library-header "MATERIAL SPEC"))
  (if (and library-spec-index (>= library-spec-index 0))
    (progn
      (setq library-material-spec (nth library-spec-index library-record))
      (setq lom-spec-index (get-column-index lom-header "MATERIAL SPEC"))
      (if (and lom-spec-index (>= lom-spec-index 0))
        (progn
          (setq record (MBS:set-nth lom-spec-index record library-material-spec))
          (MBS:log-verbose (strcat "\n🔍 DEBUG: Mapped MATERIAL SPEC '" library-material-spec "' to LOM field " (itoa lom-spec-index)))
        )
        (MBS:log-warning "\n❌ DEBUG: MATERIAL SPEC column not found in LOM CSV")
      )
    )
    (MBS:log-warning "\n❌ DEBUG: MATERIAL SPEC column not found in library")
  )
  
  ;; Map MANUFACTURER field
  (setq library-mfr-index (get-column-index library-header "MANUFACTURER"))
  (if (and library-mfr-index (>= library-mfr-index 0))
    (progn
      (setq library-manufacturer (nth library-mfr-index library-record))
      (setq lom-mfr-index (get-column-index lom-header "MANUFACTURER"))
      (if (and lom-mfr-index (>= lom-mfr-index 0))
        (progn
          (setq record (MBS:set-nth lom-mfr-index record library-manufacturer))
          (MBS:log-verbose (strcat "\n🔍 DEBUG: Mapped MANUFACTURER '" library-manufacturer "' to LOM field " (itoa lom-mfr-index)))
        )
        (MBS:log-warning "\n❌ DEBUG: MANUFACTURER column not found in LOM CSV")
      )
    )
    (MBS:log-warning "\n❌ DEBUG: MANUFACTURER column not found in library")
  )
  
  ;; Map NSN OR MFR PART NO. field
  (setq library-nsn-index (get-column-index library-header "NSN OR MFR PART NO."))
  (if (and library-nsn-index (>= library-nsn-index 0))
    (progn
      (setq library-nsn-part (nth library-nsn-index library-record))
      (setq lom-nsn-index (get-column-index lom-header "NSN OR MFR PART NO."))
      (if (and lom-nsn-index (>= lom-nsn-index 0))
        (progn
          (setq record (MBS:set-nth lom-nsn-index record library-nsn-part))
          (MBS:log-verbose (strcat "\n🔍 DEBUG: Mapped NSN PART '" library-nsn-part "' to LOM field " (itoa lom-nsn-index)))
        )
        (MBS:log-warning "\n❌ DEBUG: NSN OR MFR PART NO. column not found in LOM CSV")
      )
    )
    (MBS:log-warning "\n❌ DEBUG: NSN OR MFR PART NO. column not found in library")
  )
  
  ;; Map STANDARD field
  (setq library-std-index (get-column-index library-header "STANDARD"))
  (if (and library-std-index (>= library-std-index 0))
    (progn
      (setq library-standard (nth library-std-index library-record))
      (setq lom-std-index (get-column-index lom-header "STANDARD"))
      (if (and lom-std-index (>= lom-std-index 0))
        (progn
          (setq record (MBS:set-nth lom-std-index record library-standard))
          (MBS:log-verbose (strcat "\n🔍 DEBUG: Mapped STANDARD '" library-standard "' to LOM field " (itoa lom-std-index)))
        )
        (MBS:log-warning "\n❌ DEBUG: STANDARD column not found in LOM CSV")
      )
    )
    (MBS:log-warning "\n❌ DEBUG: STANDARD column not found in library")
  )
  
  ;; Map UNIT WT (LBS) field
  (setq library-wt-index (get-column-index library-header "UNIT WT (LBS)"))
  (if (and library-wt-index (>= library-wt-index 0))
    (progn
      (setq library-unit-wt (nth library-wt-index library-record))
      (setq lom-wt-index (get-column-index lom-header "UNIT WT (LBS)"))
      (if (and lom-wt-index (>= lom-wt-index 0))
        (progn
          (setq record (MBS:set-nth lom-wt-index record library-unit-wt))
          (MBS:log-verbose (strcat "\n🔍 DEBUG: Mapped UNIT WT '" library-unit-wt "' to LOM field " (itoa lom-wt-index)))
        )
        (MBS:log-warning "\n❌ DEBUG: UNIT WT (LBS) column not found in LOM CSV")
      )
    )
    (MBS:log-warning "\n❌ DEBUG: UNIT WT (LBS) column not found in library")
  )
  
  ;; Map REMARKS/SALIENT CHARACTERISTICS field
  (setq library-remarks-index (get-column-index library-header "REMARKS/SALIENT CHARACTERISTICS"))
  (if (and library-remarks-index (>= library-remarks-index 0))
    (progn
      (setq library-remarks (nth library-remarks-index library-record))
      (setq lom-remarks-index (get-column-index lom-header "REMARKS/SALIENT CHARACTERISTICS"))
      (if (and lom-remarks-index (>= lom-remarks-index 0))
        (progn
          (setq record (MBS:set-nth lom-remarks-index record library-remarks))
          (MBS:log-verbose (strcat "\n🔍 DEBUG: Mapped REMARKS '" library-remarks "' to LOM field " (itoa lom-remarks-index)))
        )
        (MBS:log-warning "\n❌ DEBUG: REMARKS/SALIENT CHARACTERISTICS column not found in LOM CSV")
      )
    )
    (MBS:log-warning "\n❌ DEBUG: REMARKS/SALIENT CHARACTERISTICS column not found in library")
  )
  
  ;; Set MATERIAL_ID or REMOVAL_ID
  (setq record (MBS:set-nth id-field-index record new-id))
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Set MATERIAL_ID '" new-id "' to LOM field " (itoa id-field-index)))
  
  ;; DEBUG: Show final mapped record
  (MBS:log-verbose "\n🔍 DEBUG: Final mapped record:")
  (setq field-index 0)
  (foreach field record
    (MBS:log-verbose (strcat "\n  [" (itoa field-index) "] '" field "'"))
    (setq field-index (1+ field-index))
  )
  
  record
)
(MBS:log-verbose "\n✅ MBS:create-library-import-record defined")

(MBS:log-verbose "\n🔍 About to define MBS:set-nth...")

;; Helper function to set nth element in a list
(defun MBS:set-nth (index lst value / result i)
  "Set the nth element in a list to a new value"
  (setq result '())
  (setq i 0)
  (foreach item lst
    (if (= i index)
      (setq result (append result (list value)))
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  result
)
(MBS:log-verbose "\n✅ MBS:set-nth defined")

;; Helper function to create a list of specified length with default value
(defun MBS:make-list (length default-value / result i)
  "Create a list of specified length filled with default value"
  (setq result '())
  (setq i 0)
  (while (< i length)
    (setq result (append result (list default-value)))
    (setq i (1+ i))
  )
  result
)

;; Helper function to take first N elements of a list
(defun take (lst n / result i)
  "Take first N elements from a list"
  (setq result '())
  (setq i 0)
  (foreach item lst
    (if (< i n)
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  result
)
(MBS:log-verbose "\n✅ MBS:make-list and take functions defined")

;; Add record to CSV using SIMPLE DIRECT APPEND approach
(defun MBS:add-record-to-csv (csv-path new-record / f csv-line)
  "Add a new record to CSV using simple direct append"
  
  ;; Validate input
  (if (not new-record)
    (progn
      (MBS:log-error "\n❌ ERROR: new-record is nil")
      nil
    )
    (if (not (listp new-record))
      (progn
        (MBS:log-error "\n❌ ERROR: new-record is not a list")
        nil
      )
      (progn
        (MBS:log-verbose (strcat "\n🔍 DEBUG: Adding record to CSV: " csv-path))
        (MBS:log-verbose (strcat "\n🔍 DEBUG: New record length: " (itoa (length new-record))))
        (MBS:log-verbose (strcat "\n🔍 DEBUG: New record DESCRIPTION: '" (if (and new-record (>= (length new-record) 4) (nth 3 new-record)) (nth 3 new-record) "N/A") "'"))
  
  ;; DEBUG: Show the new record we're trying to add
  (MBS:log-verbose "\n🔍 DEBUG: New record to add (first 10 fields):")
  (setq field-index 0)
  (foreach field (take new-record 10)
    (MBS:log-verbose (strcat "\n  [" (itoa field-index) "] '" field "'"))
    (setq field-index (1+ field-index))
  )
  
  ;; Convert record to CSV line using manual approach to preserve empty fields
  (setq csv-line "")
  (setq field-index 0)
  (foreach field new-record
    (if (> field-index 0)
      (setq csv-line (strcat csv-line ","))
    )
    (setq csv-line (strcat csv-line (escape-csv-field field)))
    (setq field-index (1+ field-index))
  )
  (MBS:log-verbose (strcat "\n🔍 DEBUG: CSV line to append: '" csv-line "'"))
  
  ;; Open file in append mode and add the new line
  (setq f (open csv-path "a"))
  (if f
    (progn
      (write-line csv-line f)
      (close f)
      (MBS:log-info "\n✅ Record added to CSV successfully")
      t
    )
    (progn
      (MBS:log-error "\n❌ Failed to open CSV file for appending")
      nil
    )
  )
        )
      )
    )
  )

(MBS:log-verbose "\n✅ MBS:add-record-to-csv defined")

(MBS:log-verbose "\n🔍 Using existing MBS:parse-selection function from mbs_blocks.lsp")

;; ========================================================================
;; OPTIMIZED SEARCH FUNCTIONS
;; ========================================================================

;; Optimized search library with performance improvements
(defun MBS:search-library-optimized (library-path search-term / csv-data header records results row-index max-results)
  "Optimized search library file for materials matching the search term"
  
  ;; Set maximum results to prevent dialog overload
  (setq max-results 100)
  
  (MBS:log-info (strcat "\n🔍 Searching library for: '" search-term "'"))
  
  (setq csv-data (MBS:read-csv library-path))
  (if (not csv-data)
    (progn
      (MBS:log-error "\n❌ Failed to read library file")
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
      
      ;; Search through records with early termination
      (foreach record records
        (setq row-index (1+ row-index))
        
        ;; Check if we've reached the maximum results
        (if (>= (length results) max-results)
          (progn
            (MBS:log-warning (strcat "\n⚠ Search limited to first " (itoa max-results) " results"))
            (exit)  ;; Exit the foreach loop
          )
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
      )
      
      (MBS:log-info (strcat "\n✅ Found " (itoa (length results)) " matching materials"))
      results
    )
  )
)

;; Fast search with result limiting and progress indication
(defun MBS:search-library-fast (library-path search-term / csv-data header records results row-index max-results total-records)
  "Fast search with progress indication and result limiting"
  
  ;; Set maximum results to prevent dialog overload
  (setq max-results 50)
  
  (MBS:log-info (strcat "\n🔍 Fast search for: '" search-term "'"))
  
  (setq csv-data (MBS:read-csv library-path))
  (if (not csv-data)
    (progn
      (MBS:log-error "\n❌ Failed to read library file")
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
        (MBS:log-info (strcat "\n📊 Searching " (itoa total-records) " records..."))
      )
      
      ;; Search through records with early termination
      (foreach record records
        (setq row-index (1+ row-index))
        
        ;; Show progress every 1000 records
        (if (and (> total-records 1000) (= (rem row-index 1000) 0))
          (MBS:log-verbose (strcat "\n📊 Progress: " (itoa row-index) "/" (itoa total-records)))
        )
        
        ;; Check if we've reached the maximum results
        (if (>= (length results) max-results)
          (progn
            (MBS:log-warning (strcat "\n⚠ Search limited to first " (itoa max-results) " results"))
            (exit)  ;; Exit the foreach loop
          )
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
      )
      
      (MBS:log-info (strcat "\n✅ Found " (itoa (length results)) " matching materials"))
      results
    )
  )
)

;; Smart search that adapts based on search term length
(defun MBS:search-library-smart (library-path search-term / search-term-length)
  "Smart search that adapts strategy based on search term"
  
  (setq search-term-length (strlen search-term))
  
  ;; Use different strategies based on search term length
  (cond
    ;; Very short terms (1-2 chars) - use fast search with small limit
    ((<= search-term-length 2)
     (MBS:search-library-fast library-path search-term)
    )
    ;; Medium terms (3-5 chars) - use optimized search with medium limit
    ((<= search-term-length 5)
     (MBS:search-library-optimized library-path search-term)
    )
    ;; Long terms (6+ chars) - use original search (likely to be specific)
    (t
     (MBS:search-library library-path search-term)
    )
  )
)

;; ========================================================================

;; ========================================================================
;; PERFORMANCE MONITORING AND CONTROL
;; ========================================================================

;; Performance monitoring function
(defun MBS:monitor-search-performance (search-term result-count search-time / performance-level)
  "Monitor search performance and provide feedback"
  
  ;; Calculate performance metrics
  (setq performance-level 
    (cond
      ((and (<= result-count 10) (<= search-time 1.0)) "EXCELLENT")
      ((and (<= result-count 50) (<= search-time 3.0)) "GOOD")
      ((and (<= result-count 100) (<= search-time 5.0)) "ACCEPTABLE")
      (t "SLOW")
    )
  )
  
  ;; Log performance feedback
  (MBS:log-info (strcat "\n📊 Search Performance: " performance-level))
  (MBS:log-info (strcat "\n   Results: " (itoa result-count)))
  (MBS:log-info (strcat "\n   Time: " (rtos search-time 2 2) " seconds"))
  
  ;; Provide suggestions for slow searches
  (if (= performance-level "SLOW")
    (progn
      (MBS:log-warning "\n💡 Performance Tips:")
      (MBS:log-warning "\n   • Use more specific search terms")
      (MBS:log-warning "\n   • Try searching by manufacturer or part number")
      (MBS:log-warning "\n   • Consider using exact matches")
    )
  )
  
  performance-level
)

;; Search strategy selector based on user preferences
(defun MBS:select-search-strategy (search-term / search-length)
  "Select the best search strategy based on search term and user preferences"
  
  (setq search-length (strlen search-term))
  
  ;; Check if user has set performance preferences
  (if (boundp 'MBS:prefer-fast-search)
    (if MBS:prefer-fast-search
      (MBS:search-library-fast library-path search-term)
      (MBS:search-library-smart library-path search-term)
    )
    ;; Default to smart search
    (MBS:search-library-smart library-path search-term)
  )
)

;; Function to set search performance preferences
(defun MBS:set-search-performance-mode (mode)
  "Set search performance mode: 'FAST' or 'COMPREHENSIVE'"
  
  (cond
    ((= (strcase mode) "FAST")
     (setq MBS:prefer-fast-search t)
     (MBS:log-info "\n✅ Search mode set to FAST (limited results, quick response)")
    )
    ((= (strcase mode) "COMPREHENSIVE")
     (setq MBS:prefer-fast-search nil)
     (MBS:log-info "\n✅ Search mode set to COMPREHENSIVE (all results, may be slower)")
    )
    (t
     (MBS:log-warning "\n⚠ Invalid mode. Use 'FAST' or 'COMPREHENSIVE'")
    )
  )
)

;; ========================================================================

(MBS:log-verbose "\n🔍 About to finish loading library file...")
(MBS:log-info "\n📚 Material Library Import System loaded!")
(MBS:log-verbose "\n✅ Library file load complete!")
(princ) 
