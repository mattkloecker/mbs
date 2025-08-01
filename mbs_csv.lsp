;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - CSV Module
;; VERSION 5.5
;;; ========================================================================
;; This module contains all functionality related to CSV file handling.
;; Dependencies: mbs_core.lsp, mbs_config.lsp
;;; ========================================================================

;;; ========================================================================
;; CSV PARSING AND MANIPULATION FUNCTIONS
;;; ========================================================================

;; Enhanced parse-csv-line function with PROPER quote handling
(defun parse-csv-line (line / result current in-quotes i len chr)
  (setq result '())
  (setq current "")
  (setq in-quotes nil)
  (setq i 1)
  (setq len (strlen line))
  
  (while (<= i len)
    (setq chr (substr line i 1))
    (cond
      ;; Handle quotes
      ((= chr "\"")
       (if in-quotes
           ;; We're inside a quoted section
           (if (and (<= (+ i 1) len) (= (substr line (+ i 1) 1) "\""))
               ;; Double quote inside quoted string - add a single quote and skip the next one
               (progn
                 (setq current (strcat current "\""))
                 (setq i (1+ i))
               )
               ;; End of quoted section
               (setq in-quotes nil)
           )
           ;; Start of quoted section
           (setq in-quotes t)
       )
      )
      
      ;; Handle commas
      ((= chr ",")
       (if in-quotes
           ;; Inside quotes, comma is part of the field
           (setq current (strcat current chr))
           ;; Outside quotes, comma is a delimiter
           (progn
             (setq result (append result (list current)))
             (setq current "")
           )
       )
      )
      
      ;; Handle all other characters
      (t
       (setq current (strcat current chr))
      )
    )
    (setq i (1+ i))
  )
  
  ;; Add the last field
  (setq result (append result (list current)))
  
  result
)

;; FIXED escape-csv-field function for proper quoting
(defun escape-csv-field (field / result)
  (if (= (type field) 'STR)
    (progn
      (if (or (vl-string-search "," field) 
              (vl-string-search "\"" field) 
              (vl-string-search "\n" field))
        (progn
          ;; Replace quotes with double quotes (CRITICAL FIX)
          (setq result field)
          (while (vl-string-search "\"" result)
            (setq pos (vl-string-search "\"" result))
            (setq result (strcat (substr result 1 pos) 
                                 "\"\"" 
                                 (substr result (+ pos 2))))
          )
          ;; Wrap in quotes
          (strcat "\"" result "\"")
        )
        field  ;; No special characters, no need for quotes
      )
    )
    ;; Handle non-string values
    (if field
      (if (= (type field) 'REAL)
        (rtos field 2 8)  ;; Format numbers nicely
        (vl-princ-to-string field)
      )
      ""  ;; Return empty string for nil
    )
  )
)

;; Fixed list-to-csv-line function
(defun list-to-csv-line (field-list / result)
  (setq result "")
  (foreach field field-list
    (if (= result "")
      (setq result (escape-csv-field (if field field "")))
      (setq result (strcat result "," (escape-csv-field (if field field ""))))
    )
  )
  result
)

;; Enhanced get-column-index function with fallback to partial matching
(defun get-column-index (header column-name / i result)
  "Finds column by exact match first, then tries partial match as fallback"
  (setq i 0)
  (setq result nil)
  
  ;; First attempt: exact match (original behavior)
  (foreach col header
    (if (and (not result) (= (strcase col) (strcase column-name)))
      (setq result i)
    )
    (if (not result)
      (setq i (1+ i))
    )
  )
  
  ;; If exact match failed and column-name doesn't contain parentheses,
  ;; try partial matching (for columns like "UNIT WT (LBS)")
  (if (and (not result) (not (vl-string-search "(" column-name)))
    (progn
      (setq i 0)
      (foreach col header
        (if (and (not result) 
                 (>= (strlen col) (strlen column-name))
                 (= (strcase (substr col 1 (strlen column-name))) (strcase column-name)))
          (setq result i)
        )
        (if (not result)
          (setq i (1+ i))
        )
      )
    )
  )
  
  result  ;; Return result
)

;; Function to cache and retrieve column indices
(defun MBS:get-cached-column-indices (header / indices)
  ;; Store commonly used indices in a global variable if not already set
  (if (not (boundp 'MBS:cached-column-indices))
    (progn
      (setq MBS:cached-column-indices (list
        (cons "ITEM_NO" (get-column-index header "ITEM NO."))
        (cons "DESCRIPTION" (get-column-index header "DESCRIPTION"))
        (cons "MATERIAL_ID" (get-column-index header (strcase id-attribute)))
      ))
    )
  )
  
  ;; Return the cached indices
  MBS:cached-column-indices
)

;; Read CSV file safely with error handling and logging
(defun MBS:read-csv (file-path / f line header records)
  (MBS:log-info (strcat "Reading CSV file: " file-path))
  
  (if (not (findfile file-path))
    (progn
      (MBS:log-critical (strcat "File not found: " file-path))
      nil
    )
    (progn
      (setq f (open file-path "r"))
      (if (not f)
        (progn
          (MBS:log-critical (strcat "Failed to open file: " file-path))
          nil
        )
        (progn
          ;; Read header first
          (setq header (parse-csv-line (read-line f)))
          (MBS:log-verbose (strcat "Found " (itoa (length header)) " columns in header"))
          (MBS:log-data "Header" header)
          
          ;; Read data lines
          (setq records '())
          (while (setq line (read-line f))
            (if (> (strlen line) 0)
              (setq records (append records (list (parse-csv-line line))))
            )
          )
          (close f)
          
          (MBS:log-info (strcat "Successfully read " (itoa (length records)) " data rows"))
          (list header records)
        )
      )
    )
  )
)

;; Enhanced write CSV function with proper backup handling
(defun MBS:write-csv (file-path header records / f temp-path backup-result)
  (MBS:log-info (strcat "Writing " (itoa (length records)) " records to " file-path))
  
  ;; Check if file exists first before attempting backup
  (if (findfile file-path)
    (progn
      ;; Create backup of existing file with explicit result handling
      (setq backup-result (MBS:backup-csv-file file-path))
      (if (not backup-result)
        (MBS:log-warning "Backup creation failed, but will continue with file update")
      )
    )
  )
  
  ;; Use a temp file to prevent corruption
  (setq temp-path (strcat (getenv "TEMP") "\\mbs_csv_" (itoa (getvar "MILLISECS")) ".csv"))
  
  ;; Open temp file
  (setq f (open temp-path "w"))
  (if (not f)
    (progn
      (MBS:log-critical (strcat "Failed to create temporary file: " temp-path))
      nil
    )
    (progn
      ;; Write header line
      (write-line (list-to-csv-line header) f)
      
      ;; Write data rows with proper column alignment
      (setq rows-written 0)
      (foreach record records
        ;; Ensure record has the right number of columns
        (setq aligned-record (MBS:align-record record header))
        
        ;; Write the record
        (write-line (list-to-csv-line aligned-record) f)
        (setq rows-written (1+ rows-written))
      )
      
      ;; Close temp file
      (close f)
      
      ;; Add a small delay to ensure file operations complete
      (vl-cmdf "delay" 100)
      
      ;; Replace original with temp file - with better error handling
      (if (findfile file-path)
        (progn
          (vl-catch-all-apply '(lambda () (vl-file-delete file-path)))
          ;; Add a small delay after deletion
          (vl-cmdf "delay" 100)
        )
      )
      
      (if (vl-file-copy temp-path file-path)
        (progn
          (vl-file-delete temp-path)
          (MBS:log-info (strcat "Successfully wrote " (itoa rows-written) " records to CSV"))
          t
        )
        (progn
          (MBS:log-critical (strcat "Failed to replace original file. Temp file is at: " temp-path))
          nil
        )
      )
    )
  )
)

;; Ensure record has exactly the right number of columns
(defun MBS:align-record (record header / aligned-record)
  (setq aligned-record '())
  (setq i 0)
  
  ;; Copy fields up to header length
  (while (< i (length header))
    (if (< i (length record))
      (setq aligned-record (append aligned-record (list (nth i record))))
      (setq aligned-record (append aligned-record (list "")))
    )
    (setq i (1+ i))
  )
  
  aligned-record
)

;; Improve MBS:create-empty-record function
(defun MBS:create-empty-record (header)
  (setq record '())
  (setq i 0)
  (while (< i (length header))
    (setq record (append record (list "")))
    (setq i (1+ i))
  )
  record
)

;; Add validation function
(defun MBS:validate-record-structure (record header)
  (and (listp record)
       (= (length record) (length header))
       (not (member nil record))))


;;; ========================================================================
;; CSV FILE MANAGEMENT
;;; ========================================================================

;; get-csv-path-from-block to use mode-specific attributes
(defun get-csv-path-from-block (/ doc blkName foundBlk attList att path relpath dwg-path full-rel-path
                                 path-attribute rel-path-attribute quiet)
  ;; Initialize quiet parameter as nil (not quiet) by default
  (setq quiet nil)
  
  ;; Get the optional quiet parameter if it was passed
  (if (= (type quiet-param) 'INT)
    (setq quiet quiet-param)
  )
  
  (vl-load-com)
  (setq blkName "_CACI_CSV_LINK_ANCHOR")
  (setq path-attribute (get-path-attribute))          ;; Get from config
  (setq rel-path-attribute (get-rel-path-attribute))  ;; Get from config
  
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq foundBlk nil)

  ;; Get current drawing path for relative path resolution
  (setq dwg-path (vl-filename-directory (vla-get-fullname doc)))

  ;; Search for the anchor block in modelspace
  (vlax-for ent (vla-get-ModelSpace doc)
    (if (and (= (vla-get-objectname ent) "AcDbBlockReference")
             (eq (strcase (vla-get-EffectiveName ent)) (strcase blkName)))
      (setq foundBlk ent)
    )
  )

  (if foundBlk
    (progn
      (setq attList (vlax-invoke foundBlk 'GetAttributes))
      (setq path nil)
      (setq relpath nil)
      
      ;; First check for attributes - use mode-specific attribute names
      (foreach att attList
        (cond
          ((= (strcase (vlax-get att 'TagString)) (strcase rel-path-attribute))
           (setq relpath (vlax-get att 'TextString)))
          ((= (strcase (vlax-get att 'TagString)) (strcase path-attribute))
           (setq path (vlax-get att 'TextString)))
        )
      )

      ;; Try the relative path first
      (if (and relpath (/= relpath ""))
        (progn
          ;; Use our custom join-path function
          (setq full-rel-path (join-path dwg-path relpath))
          
          (if (findfile full-rel-path)
            (progn
              ;; Only output messages if not in quiet mode
              (if (not quiet)
                (progn
                  (prompt (strcat "\nUsing relative CSV path: " relpath))
                  (prompt (strcat "\n(Resolved to: " full-rel-path ")"))
                )
              )
              full-rel-path  ;; Return the resolved path
            )
            ;; Relative file not found, try absolute path
            (if (and path (/= path ""))
              (progn
                ;; Only output messages if not in quiet mode
                (if (not quiet)
                  (prompt (strcat "\nRelative file not found. Using absolute path: " path))
                )
                path
              )
              ;; Neither path works, call select-csv-files instead of direct prompt
              (progn
                (prompt "\nNo valid CSV file path found.")
                (if (not quiet)
                  (progn
                    (if (boundp 'select-csv-files)
                      (select-csv-files)  ;; Call select-csv-files function
                      (alert "Function select-csv-files not available yet"))
                    (setq quiet-param nil)  ;; Reset quiet param before recursive call
                    (get-csv-path-from-block)  ;; Recursive call to get the new path
                  )
                  nil  ;; In quiet mode, just return nil
                )
              )
            )
          )
        )
        ;; No relative path, try absolute path
        (if (and path (/= path ""))
          (progn
            ;; Only output messages if not in quiet mode
            (if (not quiet)
              (prompt (strcat "\nUsing CSV path from block: " path))
            )
            path
          )
          ;; If attribute exists but empty, call select-csv-files
          (progn
            (prompt "\nCSV path attributes are empty.")
            (if (not quiet)
              (progn
                (if (boundp 'select-csv-files)
                  (select-csv-files)  ;; Call select-csv-files function
                  (alert "Function select-csv-files not available yet"))
                (setq quiet-param nil)  ;; Reset quiet param before recursive call
                (get-csv-path-from-block)  ;; Recursive call to get the new path
              )
              nil  ;; In quiet mode, just return nil
            )
          )
        )
      )
    )
    ;; Block not found, call select-csv-files instead of direct prompt
    (progn
      (prompt "\nAnchor block not found.")
      (if (not quiet)
        (progn
          (if (boundp 'select-csv-files)
            (select-csv-files)  ;; Call select-csv-files function
            (alert "Function select-csv-files not available yet"))
          (setq quiet-param nil)  ;; Reset quiet param before recursive call
          (get-csv-path-from-block)  ;; Recursive call to get the new path
        )
        nil  ;; In quiet mode, just return nil
      )
    )
  )
)

;; Function to get CSV path in quiet mode (for menu display)
(defun get-quiet-csv-path ()
  (setq quiet-param 1)  ;; Set global parameter to indicate quiet mode
  (setq result (get-csv-path-from-block))  ;; Call function with quiet mode
  (setq quiet-param nil)  ;; Reset the parameter
  result  ;; Return the result
)

;; Function to update or create the anchor block with both absolute and relative paths
(defun update-csv-path-in-block (filePath / doc blkName foundBlk attList dwg-path rel-path)
  (vl-load-com)
  (setq blkName "_CACI_CSV_LINK_ANCHOR")
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq foundBlk nil)
  
  ;; Get current drawing path for relative path calculation
  (setq dwg-path (vl-filename-directory (vla-get-fullname doc)))
  
  ;; Calculate relative path
  (setq rel-path (create-relative-path filePath dwg-path))

  ;; Search for the anchor block in modelspace
  (vlax-for ent (vla-get-ModelSpace doc)
    (if (and (= (vla-get-objectname ent) "AcDbBlockReference")
             (eq (strcase (vla-get-EffectiveName ent)) (strcase blkName)))
      (setq foundBlk ent)
    )
  )

  (if foundBlk
    (progn
      ;; Block found, update the paths
      (setq attList (vlax-invoke foundBlk 'GetAttributes))
      (setq path-updated nil)
      (setq rel-path-updated nil)
      
      (foreach att attList
        (cond
          ((= (strcase (vlax-get att 'TagString)) (get-path-attribute))
           (vlax-put att 'TextString filePath)
           (setq path-updated t))
          ((= (strcase (vlax-get att 'TagString)) (get-rel-path-attribute))
           (vlax-put att 'TextString rel-path)
           (setq rel-path-updated t))
        )
      )
      
      (cond
        ((and path-updated rel-path-updated)
         (prompt (strcat "\nUpdated CSV paths in anchor block:"
                        "\n  Absolute: " filePath
                        "\n  Relative: " rel-path)))
        (path-updated
         (prompt (strcat "\nUpdated CSV absolute path in anchor block: " filePath
                        "\nWarning: REL_PATH_ATTRIBUTE attribute not found.")))
        (rel-path-updated 
         (prompt (strcat "\nUpdated CSV relative path in anchor block: " rel-path
                        "\nWarning: PATH_ATTRIBUTE attribute not found.")))
        (t
         (prompt "\nWarning: Neither PATH_ATTRIBUTE nor REL_PATH_ATTRIBUTE attributes found in anchor block."))
      )
    )
    ;; Block not found, create and insert it
    (progn
      (prompt "\nAnchor block not found. Creating new anchor block.")
      
      ;; Create block definition if it doesn't exist
      (if (not (tblsearch "BLOCK" blkName))
        (progn
          (command "_.-block" blkName "0,0" "0,0" "")
          (command "_.-attdef" "" "C" "0" "Standard" "0,0" (get-path-attribute) "CSV File Path:" "")
          (command "_.-attdef" "" "C" "0" "Standard" "0,-0.5" (get-rel-path-attribute) "CSV Relative Path:" "")
          (command ".")
        )
      )
      
      ;; Insert block
      (if (tblsearch "BLOCK" blkName)
        (progn
          (setq insPt (vlax-3d-point '(0 0 0)))
          (setq blkRef (vla-InsertBlock (vla-get-ModelSpace doc) insPt blkName 1.0 1.0 1.0 0))
          
          (if blkRef
            (progn
              (setq attList (vlax-invoke blkRef 'GetAttributes))
              (setq path-set nil)
              (setq rel-path-set nil)
              
              (foreach att attList
                (cond
                  ((= (strcase (vlax-get att 'TagString)) (get-path-attribute))
                   (vlax-put att 'TextString filePath)
                   (setq path-set t))
                  ((= (strcase (vlax-get att 'TagString)) (get-rel-path-attribute))
                   (vlax-put att 'TextString rel-path)
                   (setq rel-path-set t))
                )
              )
              
              (cond
                ((and path-set rel-path-set)
                 (prompt (strcat "\nCreated new anchor block with CSV paths:"
                                "\n  Absolute: " filePath
                                "\n  Relative: " rel-path)))
                (path-set
                 (prompt (strcat "\nCreated new anchor block with absolute path: " filePath
                                "\nWarning: REL_PATH_ATTRIBUTE attribute not found.")))
                (rel-path-set
                 (prompt (strcat "\nCreated new anchor block with relative path: " rel-path
                                "\nWarning: PATH_ATTRIBUTE attribute not found.")))
                (t
                 (prompt "\nWarning: New anchor block does not have the required attributes."))
              )
            )
            (prompt "\nError: Failed to insert anchor block.")
          )
        )
        (prompt "\nError: Block _CACI_CSV_LINK_ANCHOR not defined in drawing.")
      )
    )
  )
)

;; Function to update anchor block with both LOM and LOR paths
(defun update-csv-paths-in-block (lom-path lor-path / doc blkName foundBlk attList dwg-path rel-lom-path rel-lor-path)
  (vl-load-com)
  (setq blkName "_CACI_CSV_LINK_ANCHOR")
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq foundBlk nil)
  
  ;; Get current drawing path for relative path calculation
  (setq dwg-path (vl-filename-directory (vla-get-fullname doc)))
  
  ;; Calculate relative paths
  (if (and lom-path (/= lom-path ""))
    (setq rel-lom-path (create-relative-path lom-path dwg-path))
    (setq rel-lom-path "")
  )
  
  (if (and lor-path (/= lor-path ""))
    (setq rel-lor-path (create-relative-path lor-path dwg-path))
    (setq rel-lor-path "")
  )

  ;; Search for the anchor block in modelspace
  (vlax-for ent (vla-get-ModelSpace doc)
    (if (and (= (vla-get-objectname ent) "AcDbBlockReference")
             (eq (strcase (vla-get-EffectiveName ent)) (strcase blkName)))
      (setq foundBlk ent)
    )
  )

  (if foundBlk
    (progn
      ;; Block found, update all paths
      (setq attList (vlax-invoke foundBlk 'GetAttributes))
      (setq lom-path-updated nil)
      (setq lom-rel-path-updated nil)
      (setq lor-path-updated nil)
      (setq lor-rel-path-updated nil)
      
      (foreach att attList
        (cond
          ((= (strcase (vlax-get att 'TagString)) "CSVFILEPATH")
           (vlax-put att 'TextString lom-path)
           (setq lom-path-updated t))
          ((= (strcase (vlax-get att 'TagString)) "CSVRELFILEPATH")
           (vlax-put att 'TextString rel-lom-path)
           (setq lom-rel-path-updated t))
          ((= (strcase (vlax-get att 'TagString)) "CSVLORFILEPATH")
           (vlax-put att 'TextString lor-path)
           (setq lor-path-updated t))
          ((= (strcase (vlax-get att 'TagString)) "CSVLORRELFILEPATH")
           (vlax-put att 'TextString rel-lor-path)
           (setq lor-rel-path-updated t))
        )
      )
      
      (prompt "\nUpdated CSV paths in anchor block:")
      (prompt (strcat "\n  LOM Absolute: " lom-path))
      (prompt (strcat "\n  LOM Relative: " rel-lom-path))
      (prompt (strcat "\n  LOR Absolute: " lor-path))
      (prompt (strcat "\n  LOR Relative: " rel-lor-path))
    )
    ;; Block not found, create and insert it
    (progn
      (prompt "\nAnchor block not found. Creating new anchor block.")
      
      ;; Create block definition if it doesn't exist
      (if (not (tblsearch "BLOCK" blkName))
        (progn
          (command "_.-block" blkName "0,0" "0,0" "")
          (command "_.-attdef" "" "C" "0" "Standard" "0,0" "CSVFILEPATH" "LOM CSV Path:" "")
          (command "_.-attdef" "" "C" "0" "Standard" "0,-0.5" "CSVRELFILEPATH" "LOM Relative Path:" "")
          (command "_.-attdef" "" "C" "0" "Standard" "0,-1.0" "CSVLORFILEPATH" "LOR CSV Path:" "")
          (command "_.-attdef" "" "C" "0" "Standard" "0,-1.5" "CSVLORRELFILEPATH" "LOR Relative Path:" "")
          (command ".")
        )
      )
      
      ;; Insert block
      (if (tblsearch "BLOCK" blkName)
        (progn
          (setq insPt (vlax-3d-point '(0 0 0)))
          (setq blkRef (vla-InsertBlock (vla-get-ModelSpace doc) insPt blkName 1.0 1.0 1.0 0))
          
          (if blkRef
            (progn
              (setq attList (vlax-invoke blkRef 'GetAttributes))
              
              (foreach att attList
                (cond
                  ((= (strcase (vlax-get att 'TagString)) "CSVFILEPATH")
                   (vlax-put att 'TextString lom-path))
                  ((= (strcase (vlax-get att 'TagString)) "CSVRELFILEPATH")
                   (vlax-put att 'TextString rel-lom-path))
                  ((= (strcase (vlax-get att 'TagString)) "CSVLORFILEPATH")
                   (vlax-put att 'TextString lor-path))
                  ((= (strcase (vlax-get att 'TagString)) "CSVLORRELFILEPATH")
                   (vlax-put att 'TextString rel-lor-path))
                )
              )
              
              (prompt "\nCreated new anchor block with CSV paths:")
              (prompt (strcat "\n  LOM Absolute: " lom-path))
              (prompt (strcat "\n  LOM Relative: " rel-lom-path))
              (prompt (strcat "\n  LOR Absolute: " lor-path))
              (prompt (strcat "\n  LOR Relative: " rel-lor-path))
            )
            (prompt "\nError: Failed to insert anchor block.")
          )
        )
        (prompt "\nError: Block _CACI_CSV_LINK_ANCHOR not defined in drawing.")
      )
    )
  )
)

;; Improved function to create backup of a CSV file before modification
(defun MBS:backup-csv-file (csv-path / backup-dir backup-path)
  (if (and csv-path (findfile csv-path))
    (progn
      ;; Create backup directory path - use backslashes for Windows
      (setq backup-dir (strcat (vl-filename-directory csv-path) "\\BACKUP"))
      
      ;; Ensure backup directory exists
      (if (not (vl-file-directory-p backup-dir))
        (vl-mkdir backup-dir)
      )
      
      ;; Create backup file path with timestamp to prevent overwriting issues
      (setq timestamp (menucmd "m=$(edtime,$(getvar,date),YYYYMMDD_HHMMSS)"))
      (setq backup-path (strcat backup-dir "\\" (vl-filename-base csv-path) ".bak"))
      
      ;; Add to existing backup instead of overwriting
      (if (findfile backup-path)
        (progn
          ;; Append to existing backup file
          (setq f-in (open csv-path "r"))
          (setq f-out (open backup-path "a"))
          
          (if (and f-in f-out)
            (progn
              ;; Add separator between backups
              (write-line "=== BACKUP " f-out)
              (write-line (strcat "=== " timestamp) f-out)
              
              ;; Copy content
              (while (setq line (read-line f-in))
                (write-line line f-out)
              )
              
              ;; Close files
              (close f-in)
              (close f-out)
              
              (MBS:log-info (strcat "Appended backup to: " backup-path))
              (prompt (strcat "\n[BACKUP] Appended backup to: " backup-path))
              T
            )
            (progn
              ;; Close any open files
              (if f-in (close f-in))
              (if f-out (close f-out))
              
              ;; Try simple file copy as fallback
              (if (vl-file-copy csv-path backup-path T)
                (progn
                  (MBS:log-info (strcat "Created backup (fallback method): " backup-path))
                  (prompt (strcat "\n[BACKUP] Created backup of CSV file: " backup-path))
                  T
                )
                (progn
                  (MBS:log-warning (strcat "Failed to create backup: " backup-path))
                  (prompt "\n[BACKUP] Warning: Failed to create backup file")
                  nil
                )
              )
            )
          )
        )
        ;; No existing backup, just copy the file
        (if (vl-file-copy csv-path backup-path nil)
          (progn
            (MBS:log-info (strcat "Created backup: " backup-path))
            (prompt (strcat "\n[BACKUP] Created backup of CSV file: " backup-path))
            T
          )
          (progn
            (MBS:log-warning (strcat "Failed to create backup: " backup-path))
            (prompt "\n[BACKUP] Warning: Failed to create backup file")
            nil
          )
        )
      )
    )
    (progn
      (MBS:log-warning "Cannot backup - CSV file not found")
      nil
    )
  )
)

;; Helper function to extract filename from path
(defun MBS:filename-base (filepath / pos)
  (if filepath
    (progn
      (setq pos (vl-string-search "." (vl-filename-base filepath)))
      (if pos
        (substr (vl-filename-base filepath) 1 pos)
        (vl-filename-base filepath)
      )
    )
    ""
  )
)

;; Helper to get relative CSV path for display
(defun MBS:get-relative-csv-path (csv-path / dwg-path)
  ;; Get current drawing path
  (setq dwg-path (vl-filename-directory (vla-get-fullname (vla-get-ActiveDocument (vlax-get-acad-object)))))
  
  ;; Create relative path from absolute path
  (create-relative-path csv-path dwg-path)
)

;; Comprehensive function to open CSV in Excel with multiple fallback methods
(defun open-csv-in-excel ( / csv-path result)
  ;; Get CSV file path
  (setq csv-path (get-csv-path-from-block))
  
  (if (not csv-path)
    (progn
      (prompt "\nNo CSV file path found. Please select a CSV file first.")
      (princ)
      nil
    )
    (if (findfile csv-path)
      (progn
        (prompt (strcat "\nAttempting to open CSV file in Excel: " csv-path))
        
        ;; Try multiple methods in sequence until one works
        (setq result nil)
        
        ;; Method 1: Use ShellExecute Windows API
        (if (not result)
          (progn
            (prompt "\nTrying method 1: ShellExecute...")
            (setq result (vl-catch-all-apply 
                          '(lambda () 
                             (vl-load-com)
                             (vlax-invoke 
                               (vlax-get-or-create-object "Shell.Application")
                               "ShellExecute"
                               "EXCEL.EXE"
                               (strcat "\"" csv-path "\"")
                               ""
                               ""
                               1)
                             t)))
            (if (vl-catch-all-error-p result)
              (setq result nil)
              (prompt "\n  Method 1 executed.")
            )
          )
        )
        
        ;; Method 2: Use Shell.Application Open
        (if (not result)
          (progn
            (prompt "\nTrying method 2: Shell.Application Open...")
            (setq result (vl-catch-all-apply 
                          '(lambda ()
                             (vl-load-com)
                             (setq shell (vlax-create-object "Shell.Application"))
                             (vlax-invoke shell 'Open (strcat "\"" csv-path "\""))
                             (vlax-release-object shell)
                             t)))
            (if (vl-catch-all-error-p result)
              (setq result nil)
              (prompt "\n  Method 2 executed.")
            )
          )
        )
        
        ;; Method 3: Use direct Excel Automation
        (if (not result)
          (progn
            (prompt "\nTrying method 3: Direct Excel Automation...")
            (setq result (vl-catch-all-apply 
                          '(lambda ()
                             (vl-load-com)
                             (setq excel (vlax-get-or-create-object "Excel.Application"))
                             (vlax-put-property excel "Visible" :vlax-true)
                             (vlax-invoke-method (vlax-get-property excel "Workbooks") "Open" csv-path)
                             t)))
            (if (vl-catch-all-error-p result)
              (setq result nil)
              (prompt "\n  Method 3 executed.")
            )
          )
        )
        
        ;; Method 4: Use startapp with full path to Excel
        (if (not result)
          (progn
            (prompt "\nTrying method 4: startapp with Excel paths...")
            
            ;; Try common Excel paths
            (setq excel-paths (list
                               "C:\\Program Files\\Microsoft Office\\root\\Office16\\EXCEL.EXE"
                               "C:\\Program Files (x86)\\Microsoft Office\\root\\Office16\\EXCEL.EXE"
                               "C:\\Program Files\\Microsoft Office\\Office16\\EXCEL.EXE"
                               "C:\\Program Files (x86)\\Microsoft Office\\Office16\\EXCEL.EXE"
                               "C:\\Program Files\\Microsoft Office\\root\\Office15\\EXCEL.EXE"
                               "C:\\Program Files (x86)\\Microsoft Office\\root\\Office15\\EXCEL.EXE"
                               "C:\\Program Files\\Microsoft Office\\Office15\\EXCEL.EXE"
                               "C:\\Program Files (x86)\\Microsoft Office\\Office15\\EXCEL.EXE"
                               "EXCEL.EXE"  ;; Simple fallback using PATH
                              ))
            
            (foreach excel-path excel-paths
              (if (and (not result) (or (= excel-path "EXCEL.EXE") (findfile excel-path)))
                (progn
                  (prompt (strcat "\n  Trying Excel path: " excel-path))
                  (setq result (vl-catch-all-apply 
                                '(lambda ()
                                   (startapp excel-path (strcat "\"" csv-path "\""))
                                   t)))
                  (if (vl-catch-all-error-p result)
                    (setq result nil)
                    (prompt "\n  Method 4 executed.")
                  )
                )
              )
            )
          )
        )
        
        ;; Method 5: Use Windows "start" command
        (if (not result)
          (progn
            (prompt "\nTrying method 5: Windows start command...")
            (setq result (vl-catch-all-apply 
                          '(lambda ()
                             (command "._SHELL" (strcat "start \"\" \"" csv-path "\""))
                             t)))
            (if (vl-catch-all-error-p result)
              (setq result nil)
              (prompt "\n  Method 5 executed.")
            )
          )
        )
        
        ;; Method 6: Use DOS open command
        (if (not result)
          (progn
            (prompt "\nTrying method 6: DOS open command...")
            (setq result (vl-catch-all-apply 
                          '(lambda ()
                             (command "._SHELL" (strcat "cmd /c \"" csv-path "\""))
                             t)))
            (if (vl-catch-all-error-p result)
              (setq result nil)
              (prompt "\n  Method 6 executed.")
            )
          )
        )
        
        ;; Final result
        (if result
          (prompt "\n✓ File opened successfully. Make your changes and save before running update.")
          (prompt "\n❌ Failed to open the file. Please open it manually in Excel.")
        )
        
        (princ)
        t
      )
      (progn
        (prompt (strcat "\nFile not found: " csv-path))
        (prompt "\nWould you like to create it? [Y/N]: ")
        (if (= (strcase (getstring)) "Y")
          (progn
            (if (boundp 'initialize-csv-file)
              (initialize-csv-file csv-path)
              (alert "Function initialize-csv-file not available yet"))
            
            ;; Try to open the new file (simplified version)
            (vl-catch-all-apply '(lambda () (startapp "EXCEL.EXE" (strcat "\"" csv-path "\""))))
            
            (prompt "\n✓ New file created and Excel launch attempted.")
            t
          )
          (prompt "\nOperation cancelled.")
        )
      )
    )
  )
)

;; Find the index of a column header (case-insensitive)
(defun MBS:find-header-index (header column-name / index)
  (setq index 0)
  (setq column-name (strcase column-name))
  
  ;; Search through header list for a case-insensitive match
  (foreach col header
    (if (= (strcase col) column-name)
      (progn
        ;; Return the current index if found
        (setq result index)
        (setq index (length header))  ;; Exit loop
      )
      ;; Otherwise, increment index and continue
      (setq index (1+ index))
    )
  )
  
  ;; If result is not set, column was not found
  (if (not (boundp 'result))
    (setq result -1)
  )
  
  result
)

;; Sort CSV file by ITEM NO
(defun MBS:sort-csv (csv-path / csv-data header records)
  (MBS:log-info (strcat "Sorting CSV file: " csv-path))
  
  ;; Read the CSV
  (setq csv-data (MBS:read-csv csv-path))
  (if (not csv-data)
    (progn 
      (MBS:log-critical "Failed to read CSV for sorting")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Find ITEM NO column
      (setq num-index (get-column-index header "ITEM NO."))
      (if (not num-index)
        (progn
          (MBS:log-warning "ITEM NO. column not found, cannot sort")
          nil
        )
        (progn
          ;; Sort records
          (setq sorted-records (MBS:sort-records-by-item-no records num-index))
          
          ;; Write sorted CSV
          (MBS:log-info "Writing sorted records to CSV")
          (MBS:write-csv csv-path header sorted-records)
        )
      )
    )
  )
)

;; Sort records by ITEM NO with special handling for prefixes
(defun MBS:sort-records-by-item-no (records num-index / grouped-records sorted-groups result)
  ;; Group records by prefix
  (setq grouped-records (list (cons "" '())  ;; No prefix
                             (cons "P" '())  ;; Pipe
                             (cons "V" '())  ;; Valve
                             (cons "F" '())  ;; Fitting
                             (cons "OTHER" '())))  ;; Other prefixes
  
  ;; Place records in appropriate groups
  (foreach record records
    (if (>= (length record) (1+ num-index))
      (progn
        (setq item-no (nth num-index record))
        
        ;; Extract prefix if any
        (setq prefix "")
        (if (and item-no (> (strlen item-no) 0))
          (if (alpha-p (substr item-no 1 1))
            (progn
              ;; Find first non-alphabetic character
              (setq i 1)
              (while (and (<= i (strlen item-no)) 
                         (alpha-p (substr item-no i 1)))
                (setq i (1+ i))
              )
              (setq prefix (substr item-no 1 (1- i)))
            )
          )
        )
        
        ;; Add to appropriate group
        (cond
          ((= prefix "P") 
           (setq group (assoc "P" grouped-records)))
          ((= prefix "V") 
           (setq group (assoc "V" grouped-records)))
          ((= prefix "F") 
           (setq group (assoc "F" grouped-records)))
          ((= prefix "") 
           (setq group (assoc "" grouped-records)))
          (t 
           (setq group (assoc "OTHER" grouped-records)))
        )
        
        ;; Add record to its group
        (if group
          (setq grouped-records 
                (MBS:update-assoc-value grouped-records (car group) 
                                      (append (cdr group) (list record))))
        )
      )
    )
  )
  
  ;; Sort each group numerically
  (setq sorted-groups '())
  (foreach group grouped-records
    (setq prefix (car group))
    (setq group-records (cdr group))
    
    ;; Sort this group's records
    (setq sorted-group-records 
          (MBS:sort-group-by-number group-records num-index prefix))
    
    ;; Add sorted group to result
    (setq sorted-groups (append sorted-groups sorted-group-records))
  )
  
  sorted-groups
)

;; Sort a group of records by extracting the numeric part
(defun MBS:sort-group-by-number (records num-index prefix / records-with-keys)
  ;; Create list of (key . record) pairs for sorting
  (setq records-with-keys '())
  (foreach record records
    (if (>= (length record) (1+ num-index))
      (progn
        (setq item-no (nth num-index record))
        (setq numeric-key (MBS:extract-numeric-value item-no prefix))
        (setq records-with-keys 
              (cons (cons numeric-key record) records-with-keys))
      )
    )
  )
  
  ;; Sort by numeric key
  (setq sorted-pairs (vl-sort records-with-keys '(lambda (a b) (< (car a) (car b)))))
  
  ;; Extract just the records
  (setq result '())
  (foreach pair sorted-pairs
    (setq result (append result (list (cdr pair))))
  )
  
  result
)

;; Extract numeric value from item number for sorting
(defun MBS:extract-numeric-value (item-no prefix / num-part)
  (if (and item-no (> (strlen item-no) 0))
    (progn
      ;; If there's a prefix, remove it
      (if (and prefix (/= prefix ""))
        (if (= (strcase (substr item-no 1 (strlen prefix))) 
               (strcase prefix))
          (setq num-part (substr item-no (1+ (strlen prefix))))
          (setq num-part item-no)
        )
        (setq num-part item-no)
      )
      
      ;; Find first digit in the remaining string
      (setq i 1)
      (while (and (<= i (strlen num-part)) 
                 (not (is-digit (substr num-part i 1))))
        (setq i (1+ i))
      )
      
      ;; Extract numeric portion
      (if (<= i (strlen num-part))
        (progn
          (setq j i)
          (while (and (<= j (strlen num-part)) 
                     (is-digit (substr num-part j 1)))
            (setq j (1+ j))
          )
          (setq num-str (substr num-part i (- j i)))
          (if (> (strlen num-str) 0)
            (atoi num-str)
            9999  ;; Default high value for items without numbers
          )
        )
        9999  ;; Default high value for items without numbers
      )
    )
    9999  ;; Default high value for empty items
  )
)

;; Function to back up CSV file from menu
(defun MBS:backup-csv-from-menu (/ csv-path result)
  ;; Get CSV file path
  (setq csv-path (get-csv-path-from-block))
  
  (if (not csv-path)
    (progn
      (prompt "\nNo CSV file path found. Please select a CSV file first.")
      (princ)
      nil
    )
    (progn
      (setq result (MBS:backup-csv-file csv-path))
      (if result
        (alert "CSV file backed up successfully.")
        (alert "Failed to backup CSV file. Check permissions or disk space.")
      )
      result
    )
  )
)

;; ===============================================
;;; MODULE - CSV BACKUP
;; ===============================================

;; Improved function to create backup of a CSV file before modification
(defun MBS:backup-csv-file (csv-path / backup-dir backup-path)
  (if (and csv-path (findfile csv-path))
    (progn
      ;; Create backup directory path - use backslashes for Windows
      (setq backup-dir (strcat (vl-filename-directory csv-path) "\\BACKUP"))
      
      ;; Ensure backup directory exists
      (if (not (vl-file-directory-p backup-dir))
        (vl-mkdir backup-dir)
      )
      
      ;; Create backup file path with timestamp to prevent overwriting issues
      (setq timestamp (menucmd "m=$(edtime,$(getvar,date),YYYYMMDD_HHMMSS)"))
      (setq backup-path (strcat backup-dir "\\" (vl-filename-base csv-path) "_" timestamp ".bak"))
      
      ;; Instead of appending to existing backups, create a new file each time
      (if (vl-file-copy csv-path backup-path nil)
        (progn
          (MBS:log-info (strcat "Created backup: " backup-path))
          (prompt (strcat "\n[BACKUP] Created backup of CSV file: " backup-path))
          T
        )
        (progn
          (MBS:log-warning (strcat "Failed to create backup: " backup-path))
          (prompt "\n[BACKUP] Warning: Failed to create backup file")
          nil
        )
      )
    )
    (progn
      (MBS:log-warning "Cannot backup - CSV file not found")
      nil
    )
  )
)

;; Manual backup command
(defun c:MBS-BackupCSV (/ csv-path)
  (prompt "\n=== CREATE CSV BACKUP ===")
  
  ;; Get current CSV path
  (setq csv-path (get-csv-path-from-block))
  
  (if (not csv-path)
    (progn
      (prompt "\nNo CSV file path found.")
      (princ)
    )
    (progn
      ;; Create a backup
      (if (MBS:backup-csv-file csv-path)
        (prompt "\n✓ CSV backup created successfully.")
        (prompt "\n❌ Failed to create CSV backup.")
      )
      (princ)
    )
  )
)

;; Compare two timestamps (returns T if t1 > t2)
(defun MBS:compare-timestamps (t1 t2 / y1 m1 d1 h1 min1 s1 y2 m2 d2 h2 min2 s2)
  ;; Ensure timestamps are strings
  (if (not (= (type t1) 'STR))
    (setq t1 (vl-princ-to-string t1))
  )
  
  (if (not (= (type t2) 'STR))
    (setq t2 (vl-princ-to-string t2))
  )
  
  ;; Extract timestamp parts from filename (typically YYYYMMDD_HHMMSS format)
  ;; or use default comparison if the format doesn't match expectations
  (if (and (>= (strlen t1) 15) (>= (strlen t2) 15))
    (progn
      ;; Parse timestamp parts from filenames
      (setq y1 (atoi (substr t1 1 4)))
      (setq m1 (atoi (substr t1 5 2)))
      (setq d1 (atoi (substr t1 7 2)))
      (setq h1 (atoi (substr t1 10 2)))
      (setq min1 (atoi (substr t1 12 2)))
      (setq s1 (atoi (substr t1 14 2)))
      
      (setq y2 (atoi (substr t2 1 4)))
      (setq m2 (atoi (substr t2 5 2)))
      (setq d2 (atoi (substr t2 7 2)))
      (setq h2 (atoi (substr t2 10 2)))
      (setq min2 (atoi (substr t2 12 2)))
      (setq s2 (atoi (substr t2 14 2)))
      
      ;; Compare year, month, day, hour, minute, second in sequence
      (cond
        ((> y1 y2) T)
        ((< y1 y2) nil)
        ((> m1 m2) T)
        ((< m1 m2) nil)
        ((> d1 d2) T)
        ((< d1 d2) nil)
        ((> h1 h2) T)
        ((< h1 h2) nil)
        ((> min1 min2) T)
        ((< min1 min2) nil)
        ((> s1 s2) T)
        (T nil)
      )
    )
    ;; Fallback: direct string comparison 
    (> t1 t2)
  )
)

;; Improved restore function that properly handles backup files
(defun c:MBS-RestoreCSV (/ csv-path backup-dir files backup-files selected-backup backup-path)
  (prompt "\n=== RESTORE CSV FROM BACKUP ===")
  
  ;; Get current CSV path
  (setq csv-path (get-csv-path-from-block))
  
  (if (not csv-path)
    (progn
      (prompt "\nNo CSV file path found.")
      (princ)
    )
    (progn
      (prompt (strcat "\nCSV file to restore: " csv-path))
      
      ;; Get backup directory
      (setq backup-dir (strcat (vl-filename-directory csv-path) "\\BACKUP"))
      (prompt (strcat "\nLooking for backups in: " backup-dir))
      
      ;; Check if backup directory exists
      (if (not (vl-file-directory-p backup-dir))
        (progn
          (prompt "\nNo backup directory found.")
          (princ)
        )
        (progn
          ;; Get all .bak files
          (setq files (vl-directory-files backup-dir "*.bak" 1))
          (prompt (strcat "\nFound " (itoa (length files)) " .bak files"))
          
          ;; Filter for this CSV's backups
          (setq base-name (vl-filename-base csv-path))
          (setq backup-files '())
          
          (foreach file files
            (if (vl-string-search base-name file)
              (setq backup-files (append backup-files (list file)))
            )
          )
          
          (if (= (length backup-files) 0)
            (progn
              (prompt "\nNo backup files found for this CSV.")
              (princ)
            )
            (progn
              ;; Display available backups
              (prompt (strcat "\nFound " (itoa (length backup-files)) " backups for this CSV:"))
              (setq i 1)
              (foreach backup backup-files
                (prompt (strcat "\n" (itoa i) ": " backup))
                (setq i (1+ i))
              )
              
              ;; Prompt for selection
              (prompt "\n\nEnter backup number to restore (0 to cancel): ")
              (setq selection (getint))
              
              (if (and (> selection 0) (<= selection (length backup-files)))
                (progn
                  ;; Get the selected backup file
                  (setq selected-backup (nth (1- selection) backup-files))
                  (setq backup-path (strcat backup-dir "\\" selected-backup))
                  
                  ;; Confirm restoration
                  (prompt (strcat "\nRestore from: " backup-path))
                  (prompt "\nAre you sure you want to restore this backup? This will overwrite the current CSV file. [Y/N]: ")
                  (if (= (strcase (getstring)) "Y")
                    (progn
                      ;; Create backup of current file first
                      (MBS:backup-csv-file csv-path)
                      
                      ;; IMPROVED: Properly extract and restore the backup
                      (if (MBS:extract-and-restore-backup backup-path csv-path)
                        (prompt "\n✓ CSV file restored from backup successfully.")
                        (prompt "\n❌ Failed to restore CSV from backup.")
                      )
                    )
                    (prompt "\nRestore operation cancelled.")
                  )
                )
                (if (/= selection 0)
                  (prompt "\nInvalid backup number.")
                )
              )
              (princ)
            )
          )
        )
      )
    )
  )
)

Copilot said: I see the issue with the backup
I see the issue with the backup and restore functions. The problem is that the backup files contain multiple saved versions with separator lines, but the restore function is simply copying the entire backup file over the CSV without extracting just one version.

Here's a fixed version of the c:MBS-RestoreCSV function that properly restores a backup by extracting only the desired version:

lisp
;; Improved restore function that properly handles backup files
(defun c:MBS-RestoreCSV (/ csv-path backup-dir files backup-files selected-backup backup-path)
  (prompt "\n=== RESTORE CSV FROM BACKUP ===")
  
  ;; Get current CSV path
  (setq csv-path (get-csv-path-from-block))
  
  (if (not csv-path)
    (progn
      (prompt "\nNo CSV file path found.")
      (princ)
    )
    (progn
      (prompt (strcat "\nCSV file to restore: " csv-path))
      
      ;; Get backup directory
      (setq backup-dir (strcat (vl-filename-directory csv-path) "\\BACKUP"))
      (prompt (strcat "\nLooking for backups in: " backup-dir))
      
      ;; Check if backup directory exists
      (if (not (vl-file-directory-p backup-dir))
        (progn
          (prompt "\nNo backup directory found.")
          (princ)
        )
        (progn
          ;; Get all .bak files
          (setq files (vl-directory-files backup-dir "*.bak" 1))
          (prompt (strcat "\nFound " (itoa (length files)) " .bak files"))
          
          ;; Filter for this CSV's backups
          (setq base-name (vl-filename-base csv-path))
          (setq backup-files '())
          
          (foreach file files
            (if (vl-string-search base-name file)
              (setq backup-files (append backup-files (list file)))
            )
          )
          
          (if (= (length backup-files) 0)
            (progn
              (prompt "\nNo backup files found for this CSV.")
              (princ)
            )
            (progn
              ;; Display available backups
              (prompt (strcat "\nFound " (itoa (length backup-files)) " backups for this CSV:"))
              (setq i 1)
              (foreach backup backup-files
                (prompt (strcat "\n" (itoa i) ": " backup))
                (setq i (1+ i))
              )
              
              ;; Prompt for selection
              (prompt "\n\nEnter backup number to restore (0 to cancel): ")
              (setq selection (getint))
              
              (if (and (> selection 0) (<= selection (length backup-files)))
                (progn
                  ;; Get the selected backup file
                  (setq selected-backup (nth (1- selection) backup-files))
                  (setq backup-path (strcat backup-dir "\\" selected-backup))
                  
                  ;; Confirm restoration
                  (prompt (strcat "\nRestore from: " backup-path))
                  (prompt "\nAre you sure you want to restore this backup? This will overwrite the current CSV file. [Y/N]: ")
                  (if (= (strcase (getstring)) "Y")
                    (progn
                      ;; Create backup of current file first
                      (MBS:backup-csv-file csv-path)
                      
                      ;; IMPROVED: Properly extract and restore the backup
                      (if (MBS:extract-and-restore-backup backup-path csv-path)
                        (prompt "\n✓ CSV file restored from backup successfully.")
                        (prompt "\n❌ Failed to restore CSV from backup.")
                      )
                    )
                    (prompt "\nRestore operation cancelled.")
                  )
                )
                (if (/= selection 0)
                  (prompt "\nInvalid backup number.")
                )
              )
              (princ)
            )
          )
        )
      )
    )
  )
)
And let's add a new function to properly extract and restore a backup:

lisp
;; Extract data from backup file and restore to target file
(defun MBS:extract-and-restore-backup (backup-path target-path / f-in temp-path f-out line header-found in-backup success)
  (prompt (strcat "\nExtracting data from backup: " backup-path))
  
  ;; Create a temporary file
  (setq temp-path (strcat (getenv "TEMP") "\\mbs_restore_" (itoa (getvar "MILLISECS")) ".csv"))
  
  ;; Open backup file for reading
  (setq f-in (open backup-path "r"))
  (if (not f-in)
    (progn
      (prompt "\nFailed to open backup file.")
      nil
    )
    (progn
      ;; Open temp file for writing
      (setq f-out (open temp-path "w"))
      (if (not f-out)
        (progn
          (close f-in)
          (prompt "\nFailed to create temporary file.")
          nil
        )
        (progn
          ;; Extract the first CSV version (or the complete file if it's not an appended backup)
          (setq header-found nil)
          (setq in-backup t)
          
          ;; Read and process each line
          (while (and in-backup (setq line (read-line f-in)))
            ;; Check if this is a separator line indicating the start of a new backup
            (if (and (> (strlen line) 3) (= (substr line 1 3) "==="))
              (progn
                ;; If we've already found the header, this means we've reached the next backup
                ;; so we should stop
                (if header-found
                  (setq in-backup nil)
                )
              )
              ;; Not a separator, process the line
              (progn
                ;; Write the line to the output file
                (write-line line f-out)
                
                ;; Check if this is the header (contains ITEM NO. or similar)
                (if (and (not header-found) 
                         (or (vl-string-search "ITEM NO." line)
                             (vl-string-search "MATERIAL_ID" line)))
                  (setq header-found t)
                )
              )
            )
          )
          
          ;; Close files
          (close f-in)
          (close f-out)
          
          ;; Now copy the temp file to the target file
          (if (vl-file-copy temp-path target-path T)
            (progn
              (vl-file-delete temp-path)
              (prompt "\nBackup data extracted and restored successfully.")
              t
            )
            (progn
              (prompt (strcat "\nFailed to copy temporary file to target. Temp file is at: " temp-path))
              nil
            )
          )
        )
      )
    )
  )
)

;; Function to get all available backups for a CSV file - with debug output
(defun MBS:get-available-backups (csv-path / backup-dir files backup-files)
  (prompt "\n[DEBUG] Starting MBS:get-available-backups")
  (prompt (strcat "\n[DEBUG] CSV path: " csv-path))
  
  (if (and csv-path (findfile csv-path))
    (progn
      ;; Get the backup directory
      (setq backup-dir (strcat (vl-filename-directory csv-path) "\\BACKUP"))
      (prompt (strcat "\n[DEBUG] Backup directory: " backup-dir))
      
      ;; Check if backup directory exists
      (if (vl-file-directory-p backup-dir)
        (progn
          (prompt (strcat "\n[DEBUG] Backup directory exists"))
          
          ;; Get list of files in backup directory
          (setq files (vl-directory-files backup-dir "*.bak" 1))
          (prompt (strcat "\n[DEBUG] Found " (itoa (length files)) " .bak files"))
          
          ;; Filter for this CSV's backups
          (setq base-name (vl-filename-base csv-path))
          (prompt (strcat "\n[DEBUG] Base name for filtering: " base-name))
          
          (setq backup-files '())
          (foreach file files
            (prompt (strcat "\n[DEBUG] Checking file: " file))
            (if (vl-string-search base-name file)
              (progn
                (prompt (strcat "\n[DEBUG] File matches base name: " file))
                (setq backup-files (append backup-files (list file)))
              )
              (prompt (strcat "\n[DEBUG] File doesn't match base name"))
            )
          )
          
          (prompt (strcat "\n[DEBUG] Found " (itoa (length backup-files)) " matching backup files"))
          
          ;; Let's skip the sorting for now to debug the issue
          (prompt "\n[DEBUG] Returning backup files without sorting")
          backup-files
        )
        (progn
          (prompt "\n[DEBUG] Backup directory doesn't exist")
          nil
        )
      )
    )
    (progn
      (prompt "\n[DEBUG] CSV path not valid")
      nil
    )
  )
)



(prompt "\nMaterial Block Sync CSV Module loaded.")
(princ)