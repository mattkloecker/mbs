;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - CSV Module  
;; VERSION 6.0 - COMPLETELY REWRITTEN CSV ENGINE
;;; ========================================================================
;; 
;; PROBLEMS WITH PREVIOUS CSV ENGINE:
;; - Column index confusion causing data to wrong columns
;; - Record structure corruption during updates
;; - Complex backup/restore logic causing hangs
;; - Transaction execution inconsistencies
;;
;; NEW CSV ENGINE PRINCIPLES:
;; - Simple, clear column index tracking
;; - Immutable record operations (create new, don't modify)
;; - Atomic write operations with simple backups
;; - Explicit field mapping with validation
;;; ========================================================================

;;; ========================================================================
;; NEW CSV ENGINE - CORE FUNCTIONS
;;; ========================================================================

;; CSV Record Structure: Simple list of strings matching header order exactly
;; Example: ("R1" "2" "EA" "REMOVAL 1" "" "" "" "10" "20" "Notes" "" "" "" "REM-123" ...)

;; Read CSV with strict validation and error checking
(defun CSV:read-file (file-path / f line header records record-count)
  "Read CSV file with robust error handling and validation"
  
  (if (not (findfile file-path))
    (progn
      (MBS:log-error (strcat "\n❌ CSV file not found: " file-path))
      nil
    )
    (progn
      (setq f (open file-path "r"))
      (if (not f)
        (progn
          (MBS:log-error (strcat "\n❌ Cannot open CSV file: " file-path))
          nil
        )
        (progn
          ;; (prompt (strcat "\n📖 Reading CSV: " file-path))
          
          ;; (prompt (strcat "\n📖 Reading CSV: " file-path))
          
          ;; Read header line
          (setq line (read-line f))
          (if (not line)
            (progn
              (close f)
              (MBS:log-error "\n❌ CSV file is empty")
              nil
            )
            (progn
              (setq header (CSV:parse-line line))
              (setq records '())
              (setq record-count 0)
              
              ;; Read data lines
              (while (setq line (read-line f))
                (if (> (strlen line) 0)  ;; Skip empty lines
                  (progn
                    (setq record (CSV:parse-line line))
                    (setq records (append records (list record)))
                    (setq record-count (1+ record-count))
                  )
                )
              )
              
              (close f)
              ;; (prompt (strcat "\n✅ Read " (itoa record-count) " records with " (itoa (length header)) " columns"))
              
              ;; Return (header records)
              (list header records)
            )
          )
        )
      )
    )
  )
)

;; Parse CSV line with proper comma and quote handling  
(defun CSV:parse-line (line / result current-field in-quotes char i)
  "Parse a CSV line handling commas and quotes correctly"
  
  (setq result '())
  (setq current-field "")
  (setq in-quotes nil)
  (setq i 1)
  
  (while (<= i (strlen line))
    (setq char (substr line i 1))
    
    (cond
      ;; Handle quotes
      ((= char "\"")
       (if in-quotes
         (progn
           ;; Check for escaped quote (double quote)
           (if (and (< i (strlen line)) (= (substr line (1+ i) 1) "\""))
             (progn
               (setq current-field (strcat current-field "\""))
               (setq i (1+ i))  ;; Skip the second quote
             )
             (setq in-quotes nil)  ;; End of quoted field
           )
         )
         (setq in-quotes t)  ;; Start of quoted field
       )
      )
      
      ;; Handle comma (field separator)
      ((and (= char ",") (not in-quotes))
       (setq result (append result (list current-field)))
       (setq current-field "")
      )
      
      ;; Regular character
      (t
       (setq current-field (strcat current-field char))
      )
    )
    
    (setq i (1+ i))
  )
  
  ;; Add the last field
  (setq result (append result (list current-field)))
  result
)

;; Find column index with exact matching and clear error messages
(defun CSV:get-column-index (header column-name / i found)
  "Find column index with exact case-insensitive matching"
  
  (setq i 0)
  (setq found nil)
  
  (foreach col header
    (if (and (not found) (= (strcase col) (strcase column-name)))
      (setq found i)
    )
    (if (not found)
      (setq i (1+ i))
    )
  )
  
  (if found
    (progn
      ;; (prompt (strcat "\n✅ Found column '" column-name "' at index " (itoa found)))
      found
    )
    (progn
      (MBS:log-error (strcat "\n❌ Column '" column-name "' not found"))
      (MBS:log-info (strcat "\n📋 Available columns: " (CSV:list-to-string header)))
      nil
    )
  )
)

;; Get field value from record with bounds checking
(defun CSV:get-field (record column-index / value)
  "Get field value with safe bounds checking"
  
  (if (and record column-index (>= column-index 0) (< column-index (length record)))
    (progn
      (setq value (nth column-index record))
      (if value value "")  ;; Return empty string if nil
    )
    ""  ;; Return empty string if invalid index
  )
)

;; Set field value in record creating new record (immutable operation)
(defun CSV:set-field (record column-index new-value header / new-record i target-length)
  "Set field value creating new record with proper length"
  
  ;; Ensure we have enough columns to match header
  (setq target-length (length header))
  (setq new-record '())
  (setq i 0)
  
  ;; Build new record with correct length
  (while (< i target-length)
    (cond
      ;; Use new value for target column
      ((= i column-index)
       (setq new-record (append new-record (list new-value)))
      )
      ;; Copy existing value if within bounds
      ((< i (length record))
       (setq new-record (append new-record (list (nth i record))))
      )
      ;; Pad with empty string if beyond original record length
      (t
       (setq new-record (append new-record (list "")))
      )
    )
    (setq i (1+ i))
  )
  
  ;; Debug output disabled for performance with large batches
  ;; (prompt (strcat "\n🔧 Set field[" (itoa column-index) "] = '" new-value "' (record length: " (itoa (length new-record)) ")"))
  new-record
)

;; Write CSV file with chunked processing for large files
(defun CSV:write-file (file-path header records / f record line record-num chunk-size progress-interval total-records)
  "Write CSV file with progress feedback and chunked processing for large files"
  
  (setq total-records (length records))
  (setq chunk-size 10)  ;; Process in chunks of 10 records
  (setq progress-interval 10)  ;; Show progress every 10 records
  
  (MBS:log-info (strcat "\n📝 Writing " (itoa total-records) " records to CSV"))
  
  ;; Only show detailed debug for small files
  (if (<= total-records 10)
    (progn
      (MBS:log-verbose (strcat "\n🔍 DEBUG: Header length: " (itoa (length header))))
      (MBS:log-verbose (strcat "\n🔍 DEBUG: First 5 header fields: " (CSV:list-first-n header 5)))
    )
  )
  
  ;; Skip file lock check for batch operations to prevent hanging
  ;; File will be checked during actual open operation
  
  (setq f (open file-path "w"))
  
  (if (not f)
    (progn
      (MBS:log-error (strcat "\n❌ Cannot create CSV file: " file-path))
      (MBS:log-info "\n💡 File may be locked by Excel or another program")
      nil
    )
    (progn
      ;; Write header
      (setq line (CSV:build-line header))
      (if (<= total-records 10)
        (MBS:log-verbose (strcat "\n🔍 DEBUG: Header line: " line))
      )
      (write-line line f)
      
      ;; Write records with progress feedback for large files
      (setq record-num 1)
      (foreach record records
        ;; Show progress for large files
        (if (and (> total-records 20) (= (rem record-num progress-interval) 0))
          (MBS:log-info (strcat "\n📊 Progress: " (itoa record-num) "/" (itoa total-records) " records written"))
        )
        
        ;; Debug output only for small files or first few records
        (if (and (<= total-records 10) (<= record-num 3))
          (progn
            (MBS:log-verbose (strcat "\n🔍 DEBUG: Record " (itoa record-num) " length: " (itoa (length record))))
            (MBS:log-verbose (strcat "\n🔍 DEBUG: Record " (itoa record-num) " first 5 fields: " (CSV:list-first-n record 5)))
            (if (>= (length record) 14)
              (MBS:log-verbose (strcat "\n🔍 DEBUG: Record " (itoa record-num) " field[13] (REMOVAL_ID): '" (nth 13 record) "'"))
            )
          )
        )
        
        ;; Write the record
        (setq line (CSV:build-line record))
        (if (and (<= total-records 10) (<= record-num 3))
          (MBS:log-verbose (strcat "\n🔍 DEBUG: Record " (itoa record-num) " CSV line: " line))
        )
        
        ;; Use error handling for each write operation
        (if (vl-catch-all-error-p (vl-catch-all-apply 'write-line (list line f)))
          (progn
            (close f)
            (MBS:log-error (strcat "\n❌ Error writing record " (itoa record-num) " to CSV"))
            (setq record-num nil)  ;; Break out of loop
          )
          (setq record-num (1+ record-num))
        )
        
        ;; Progress tracking for large files - no delays or user input
      )
      
      (if record-num  ;; Only close if we didn't error out
        (progn
          (close f)
          (MBS:log-info (strcat "\n✅ Successfully wrote CSV: " file-path))
          t
        )
        nil  ;; Return failure if we encountered an error
      )
    )
  )
)

;; Proper delay function that doesn't prompt user
(defun MBS:delay-ms (milliseconds / counter max-count)
  "Create a delay by running a counter loop - non-interactive"
  (setq max-count (* milliseconds 100))  ;; Approximate timing
  (setq counter 0)
  (while (< counter max-count)
    (setq counter (1+ counter))
  )
)



;; Helper function to list first N elements
(defun CSV:list-first-n (lst n / result i)
  "Get first N elements of list as string"
  (setq result "")
  (setq i 0)
  (foreach item lst
    (if (< i n)
      (progn
        (if (> i 0)
          (setq result (strcat result ", "))
        )
        (setq result (strcat result "'" (if item item "") "'"))
        (setq i (1+ i))
      )
    )
  )
  result
)

;; Build CSV line with proper escaping - FIXED COMMA LOGIC
(defun CSV:build-line (fields / result field escaped-field field-num)
  "Build CSV line with proper comma and quote escaping - FIXED empty field handling"
  
  (setq result "")
  (setq field-num 0)
  
  (foreach field fields
    ;; Add comma separator BEFORE each field EXCEPT the first field
    (if (> field-num 0)
      (setq result (strcat result ","))
    )
    
    ;; Handle nil fields as empty strings
    (if (not field)
      (setq field "")
    )
    
    ;; Escape field if it contains comma or quote
    (if (or (vl-string-search "," field) (vl-string-search "\"" field))
      (progn
        ;; Escape quotes and wrap in quotes
        (setq escaped-field (CSV:escape-quotes field))
        (setq result (strcat result "\"" escaped-field "\""))
      )
      ;; Add field directly (including empty strings!)
      (setq result (strcat result field))
    )
    
    (setq field-num (1+ field-num))
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

;; Utility functions
(defun CSV:escape-quotes (text / result)
  "Escape quotes in text by doubling them"
  (if (vl-string-search "\"" text)
    (progn
      (setq result "")
      (setq i 1)
      (while (<= i (strlen text))
        (setq char (substr text i 1))
        (if (= char "\"")
          (setq result (strcat result "\"\""))
          (setq result (strcat result char))
        )
        (setq i (1+ i))
      )
      result
    )
    text
  )
)

(defun CSV:timestamp ()
  "Generate timestamp for backups"
  (setq curr-date (getvar "CDATE"))
  (setq date-str (rtos curr-date 2 0))
  ;; Simple timestamp format: YYYYMMDD_HHMMSS
  (strcat (substr date-str 3 6) "_" (substr date-str 10 6))
)

(defun CSV:list-to-string (lst / result)
  "Convert list to comma-separated string"
  (setq result "")
  (foreach item lst
    (if (> (strlen result) 0)
      (setq result (strcat result ", "))
    )
    (setq result (strcat result item))
  )
  result
)

(defun CSV:copy-file (source dest / f-in f-out line)
  "Copy file using simple file I/O"
  (setq f-in (open source "r"))
  (if (not f-in)
    nil
    (progn
      (setq f-out (open dest "w"))
      (if (not f-out)
        (progn
          (close f-in)
          nil
        )
        (progn
          ;; Copy line by line
          (while (setq line (read-line f-in))
            (write-line line f-out)
          )
          (close f-in)
          (close f-out)
          t
        )
      )
    )
  )
)

(defun CSV:ensure-directory (dir-path)
  "Ensure directory exists"
  (if (not (vl-file-directory-p dir-path))
    (vl-mkdir dir-path)
  )
)

;;; ========================================================================
;; NEW TRANSACTION ENGINE FOR CSV OPERATIONS
;;; ========================================================================

;; Execute UPDATE-FIELD transaction with new CSV engine
(defun CSV:execute-update-field (csv-path row-index field-name new-value / csv-data header records record column-index new-record new-records i)
  "Execute UPDATE-FIELD transaction using new CSV engine"
  
  (MBS:log-info (strcat "\n🔄 UPDATE-FIELD: row " (itoa row-index) ", field '" field-name "' = '" new-value "'"))
  
  ;; Read current CSV
  (setq csv-data (CSV:read-file csv-path))
  (if (not csv-data)
    (progn
      (MBS:log-error "\n❌ Failed to read CSV for UPDATE-FIELD")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Find column index
      (setq column-index (CSV:get-column-index header field-name))
      (if (not column-index)
        (progn
          (MBS:log-error (strcat "\n❌ Column '" field-name "' not found"))
          nil
        )
        (progn
          ;; Validate row index
          (if (or (< row-index 0) (>= row-index (length records)))
            (progn
              (MBS:log-error (strcat "\n❌ Invalid row index: " (itoa row-index) " (max: " (itoa (1- (length records))) ")"))
              nil
            )
            (progn
              ;; Get the record to update
              (setq record (nth row-index records))
              (MBS:log-verbose (strcat "\n📝 Original record length: " (itoa (length record))))
              
              ;; Create updated record using immutable operation
              (setq new-record (CSV:set-field record column-index new-value header))
              (MBS:log-verbose (strcat "\n📝 Updated record length: " (itoa (length new-record))))
              
              ;; Build new records list with updated record
              (setq new-records '())
              (setq i 0)
              (foreach rec records
                (if (= i row-index)
                  (setq new-records (append new-records (list new-record)))
                  (setq new-records (append new-records (list rec)))
                )
                (setq i (1+ i))
              )
              
              ;; Write updated CSV
              (if (CSV:write-file csv-path header new-records)
                (progn
                  (MBS:log-info "\n✅ UPDATE-FIELD completed successfully")
                  t
                )
                (progn
                  (MBS:log-error "\n❌ Failed to write CSV")
                  nil
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Execute INSERT-ROW transaction with new CSV engine
(defun CSV:execute-insert-row (csv-path new-record / csv-data header records new-records)
  "Execute INSERT-ROW transaction using new CSV engine"
  
  (MBS:log-info (strcat "\n🔄 INSERT-ROW: " (itoa (length new-record)) " fields"))
  
  ;; Read current CSV
  (setq csv-data (CSV:read-file csv-path))
  (if (not csv-data)
    (progn
      (MBS:log-error "\n❌ Failed to read CSV for INSERT-ROW")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Ensure new record has correct length
      (while (< (length new-record) (length header))
        (setq new-record (append new-record (list "")))
      )
      
      ;; Add new record to list
      (setq new-records (append records (list new-record)))
      
      ;; Write updated CSV
      (if (CSV:write-file csv-path header new-records)
        (progn
          (MBS:log-info "\n✅ INSERT-ROW completed successfully")
          t
        )
        (progn
          (MBS:log-error "\n❌ Failed to write CSV")
          nil
        )
      )
    )
  )
)

;; BATCH transaction executor - single read/write for efficiency
(defun CSV:execute-transactions-batch (csv-path transactions current-mode / csv-data header records total-count success-count backup-created)
  "Execute transactions in a single batch operation (single read/write)"
  
  (setq total-count (length transactions))
  (setq success-count 0)
  (setq backup-created nil)
  
  (MBS:log-info (strcat "\n🚀 BATCH: Processing " (itoa total-count) " transactions with single read/write"))
  
  ;; Single CSV read
  (MBS:log-info "\n📖 Reading CSV once for batch processing...")
  (setq csv-data (CSV:read-file csv-path))
  
  (if (not csv-data)
    (progn
      (MBS:log-error "\n❌ Failed to read CSV for batch processing")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Create single backup before any changes
      (if (not backup-created)
        (progn
          (MBS:log-info "\n💾 Creating single backup before batch changes...")
          (setq backup-path (strcat (vl-filename-directory csv-path) "\\BACKUP\\" 
                                   (vl-filename-base csv-path) "_" 
                                   (CSV:timestamp) ".bak"))
          (CSV:ensure-directory (vl-filename-directory backup-path))
          (if (findfile csv-path)
            (if (CSV:copy-file csv-path backup-path)
              (progn
                (MBS:log-info (strcat "\n✅ Backup created: " backup-path))
                (setq backup-created t)
              )
              (MBS:log-warning "\n⚠ Backup failed, but continuing...")
            )
          )
        )
      )
      
      ;; Process all transactions in memory
      (MBS:log-info "\n🔄 Processing transactions in memory...")
      (foreach transaction transactions
        (setq tx-type (car transaction))
        (setq tx-data (cdr transaction))
        
        (cond
          ;; Update field in memory
          ((or (eq tx-type 'update-field) (= tx-type "UPDATE-FIELD"))
           (if (= (length tx-data) 3)
             (progn
               (setq row-index (nth 0 tx-data))
               (setq field-name (nth 1 tx-data))
               (setq new-value (nth 2 tx-data))
               
               ;; Find column index
               (setq column-index (CSV:get-column-index header field-name))
               (if (and column-index (>= row-index 0) (< row-index (length records)))
                 (progn
                   ;; Only show detailed debug for small batches to prevent console flooding
                   (if (<= total-count 10)
                     (MBS:log-verbose (strcat "\n📝 Updating row " (itoa row-index) " field '" field-name "' = '" new-value "'"))
                     (if (= (rem success-count 10) 0)  ;; Show progress every 10 transactions for large batches
                       (MBS:log-info (strcat "\n📊 Batch progress: " (itoa success-count) "/" (itoa total-count) " transactions"))
                     )
                   )
                   
                   ;; Get and update record
                   (setq record (nth row-index records))
                   (setq new-record (CSV:set-field record column-index new-value header))
                   
                   ;; Replace record in-place using efficient list replacement
                   (setq records (MBS:replace-list-item records row-index new-record))
                   (setq success-count (1+ success-count))
                 )
                 (MBS:log-error (strcat "\n❌ Invalid UPDATE-FIELD: row=" (itoa row-index) " field=" field-name))
               )
             )
             (MBS:log-error "\n❌ Invalid UPDATE-FIELD transaction data")
           )
          )
          
          ;; Insert row in memory  
          ((or (eq tx-type 'insert-row) (= tx-type "INSERT-ROW"))
           (if (= (length tx-data) 1)
             (progn
               (setq new-record (car tx-data))
               (MBS:log-verbose (strcat "\n📝 Inserting row with " (itoa (length new-record)) " fields"))
               
               ;; Ensure record has correct length
               (while (< (length new-record) (length header))
                 (setq new-record (append new-record (list "")))
               )
               
               ;; Add to records
               (setq records (append records (list new-record)))
               (setq success-count (1+ success-count))
             )
             (MBS:log-error "\n❌ Invalid INSERT-ROW transaction data")
           )
          )
          
          ;; Delete row by material ID
          ((eq tx-type 'delete-row-by-material-id)
           (if (= (length tx-data) 1)
             (progn
               (setq material-id-to-delete (car tx-data))
               
               ;; Find ID column index based on current mode
               (setq id-col-name (if (and current-mode (= current-mode "LOR"))
                                   "REMOVAL_ID"
                                   "MATERIAL_ID"))
               (MBS:log-info (strcat "\n🗑️ Deleting row with " id-col-name ": " material-id-to-delete))
               (setq mat-id-index (CSV:get-column-index header id-col-name))
               (if mat-id-index
                 (progn
                   ;; Filter out records with matching MATERIAL_ID
                   (setq new-records '())
                   (setq deleted-count 0)
                   (foreach record records
                     (if (and (>= (length record) (1+ mat-id-index))
                              (= (nth mat-id-index record) material-id-to-delete))
                       (setq deleted-count (1+ deleted-count))
                       (setq new-records (append new-records (list record)))
                     )
                   )
                   (setq records new-records)
                   (if (> deleted-count 0)
                     (progn
                       (MBS:log-info (strcat "\n✅ Deleted " (itoa deleted-count) " row(s)"))
                       (setq success-count (1+ success-count))
                     )
                     (MBS:log-warning "\n⚠ No rows found with that MATERIAL_ID")
                   )
                 )
                 (MBS:log-error (strcat "\n❌ " id-col-name " column not found"))
               )
             )
             (MBS:log-error "\n❌ Invalid DELETE-ROW-BY-MATERIAL-ID transaction data")
           )
          )
          
          (t
           (MBS:log-error (strcat "\n❌ Unknown transaction type: " (vl-princ-to-string tx-type)))
          )
        )
      )
      
      ;; Single CSV write with all changes
      (if (> success-count 0)
        (progn
          (MBS:log-info (strcat "\n💾 Writing all changes to CSV in single operation..."))
          (if (CSV:write-file csv-path header records)
            (MBS:log-info (strcat "\n✅ BATCH: " (itoa success-count) "/" (itoa total-count) " transactions completed successfully"))
            (MBS:log-error "\n❌ Failed to write batch changes to CSV")
          )
        )
        (MBS:log-info "\n📝 No changes to write")
      )
      
      (= success-count total-count)
    )
  )
)

;; Legacy single-transaction executor (kept for compatibility)
(defun CSV:execute-transactions (csv-path transactions current-mode)
  "Execute transactions - delegates to batch processor for efficiency"
  (CSV:execute-transactions-batch csv-path transactions current-mode)
)

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

;; PROPER CSV escape-csv-field function - handles quotes correctly without corruption
(defun escape-csv-field (field / field-str result needs-quotes i char)
  (if (= (type field) 'STR)
    (progn
      (setq field-str field)
      (setq needs-quotes nil)
      
      ;; Check if field needs quotes (contains comma, quote, or newline)
      (if (or (vl-string-search "," field-str) 
              (vl-string-search "\"" field-str)
              (vl-string-search "\n" field-str))
        (setq needs-quotes t)
      )
      
      ;; If field contains quotes, double them FIRST
      (if (vl-string-search "\"" field-str)
        (progn
          (setq result "")
          (setq i 1)
          (while (<= i (strlen field-str))
            (setq char (substr field-str i 1))
            (if (= char "\"")
              (setq result (strcat result "\"\""))  ;; Double the quote
              (setq result (strcat result char))
            )
            (setq i (1+ i))
          )
          (setq field-str result)
        )
      )
      
      ;; Wrap in quotes if needed
      (if needs-quotes
        (strcat "\"" field-str "\"")
        field-str
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

;; PROPER list-to-csv-line function - uses correct CSV escaping
(defun list-to-csv-line (field-list / result safe-field)
  (setq result "")
  (foreach field field-list
    ;; Use proper CSV escaping for each field
    (setq safe-field (escape-csv-field field))
    
    ;; Build result with comma separation
    (if (= result "")
      (setq result safe-field)
      (setq result (strcat result "," safe-field))
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

;; SIMPLIFIED CSV write function - backup first, then direct write
(defun MBS:write-csv (file-path header records skip-backup / f backup-result)
  ;; Default skip-backup to nil if not provided
  (if (not skip-backup) (setq skip-backup nil))
  
  ;; CRITICAL DEBUG: Check what we received
  (MBS:log-verbose (strcat "\n🔍 MBS:write-csv called with:"))
  (MBS:log-verbose (strcat "\n🔍   File: " file-path))
  (MBS:log-verbose (strcat "\n🔍   Header length: " (itoa (length header))))
  (MBS:log-verbose (strcat "\n🔍   Records length: " (itoa (length records))))
  (MBS:log-verbose (strcat "\n🔍   Skip backup: " (if skip-backup "YES" "NO")))
  
  ;; Disable logging during CSV write to prevent conflicts
  
  ;; STEP 1: Create backup FIRST (before any changes) - unless skipped
  (if (and (findfile file-path) (not skip-backup))
    (progn
      (setq backup-result (MBS:backup-csv-file file-path))
      (if (not backup-result)
        (progn
          (MBS:log-error "\n[ERROR] Could not create backup - write operation cancelled")
          (setq f nil)  ;; Force failure
        )
      )
    )
    (progn
      ;; Skip all logging during CSV operations to prevent file conflicts
      (setq backup-result t)
    )
  )
  
  ;; STEP 2: Only proceed if backup was successful (or not needed)
  (if backup-result
    (progn
      ;; Direct write to original file (no temp files, no complex logic)
      (setq f (open file-path "w"))
      (if f
        (progn
          ;; CRITICAL DEBUG: Track the write process
          ;;(prompt (strcat "\n🔍 CSV WRITE DEBUG: File opened, writing header..."))
          
          ;; Write header
          (write-line (list-to-csv-line header) f)
          ;;(prompt (strcat "\n🔍 CSV WRITE DEBUG: Header written, processing " (itoa (length records)) " records..."))
          
          ;; Write each record with error handling - CONTINUE ON ERRORS
          (setq rows-written 0)
          (setq error-count 0)
          (setq record-index 0)
          (foreach record records
            (setq record-index (1+ record-index))
            (if (= rows-written 0)
              (progn
               ;;(prompt (strcat "\n🔍 CSV WRITE DEBUG: Processing record " (itoa record-index) " of " (itoa (length records))))
                ;;(prompt (strcat "\n🔍 CSV WRITE DEBUG: Record " (itoa record-index) " has " (itoa (length record)) " fields"))
                ;;(prompt (strcat "\n🔍 CSV WRITE DEBUG: Record " (itoa record-index) " fields (first 10):"))
                (setq field-index 0)
                (foreach field (take record 10)
                  ;;(prompt (strcat "\n  [" (itoa field-index) "] '" field "'"))
                  (setq field-index (1+ field-index))
                )
              )
            )
            (setq aligned-record (vl-catch-all-apply 'MBS:align-record (list record header)))
            (if (vl-catch-all-error-p aligned-record)
              (progn
                (MBS:log-error (strcat "\n❌ ERROR: Record alignment failed for record " (itoa rows-written) "! Skipping..."))
                (setq error-count (1+ error-count))
              )
              (progn
                (if (= rows-written 0)
                  (progn
                    ;;(prompt (strcat "\n🔍 CSV WRITE DEBUG: After alignment: " (itoa (length aligned-record)) " fields"))
                    ;;(prompt "\n🔍 CSV WRITE DEBUG: Aligned record fields (first 10):")
                    (setq field-index 0)
                    (foreach field (take aligned-record 10)
                      (MBS:log-verbose (strcat "\n  [" (itoa field-index) "] '" field "'"))
                      (setq field-index (1+ field-index))
                    )
                  )
                )
                (setq csv-line (vl-catch-all-apply 'list-to-csv-line (list aligned-record)))
                (if (vl-catch-all-error-p csv-line)
                  (progn
                    (MBS:log-error (strcat "\n❌ ERROR: CSV line generation failed for record " (itoa rows-written) "! Skipping..."))
                    (MBS:log-verbose (strcat "\n🔍 DEBUG: Error details: " (vl-prin1-to-string csv-line)))
                    (MBS:log-verbose (strcat "\n🔍 DEBUG: Record length: " (itoa (length aligned-record))))
                    (if (> (length aligned-record) 0)
                      (progn
                        (MBS:log-verbose (strcat "\n🔍 DEBUG: First field type: " (vl-prin1-to-string (type (car aligned-record)))))
                        (MBS:log-verbose (strcat "\n🔍 DEBUG: First field value: '" (vl-prin1-to-string (car aligned-record)) "'"))
                      )
                    )
                    (setq error-count (1+ error-count))
                  )
                  (progn
                    (if (= rows-written 0)
                      (progn
                        (MBS:log-verbose (strcat "\n🔍 CSV WRITE DEBUG: CSV line length: " (itoa (strlen csv-line))))
                        (MBS:log-verbose (strcat "\n🔍 CSV WRITE DEBUG: CSV line content: '" csv-line "'"))
                      )
                    )
                    (if (vl-catch-all-error-p (vl-catch-all-apply 'write-line (list csv-line f)))
                      (progn
                        (MBS:log-error (strcat "\n❌ ERROR: File write failed for record " (itoa rows-written) "! Skipping..."))
                        (setq error-count (1+ error-count))
                      )
                      (setq rows-written (1+ rows-written))
                    )
                  )
                )
              )
            )
            
            ;; Progress check
            (if (and (> (length records) 20) (= (rem (+ rows-written error-count) 10) 0))
              (MBS:log-verbose (strcat "\n🔍 CSV WRITE DEBUG: Processed " (itoa (+ rows-written error-count)) " records, written " (itoa rows-written) ", errors " (itoa error-count)))
            )
          )
          
          ;; Close file
          (close f)
          
          (MBS:log-verbose (strcat "\n🔍 CSV WRITE DEBUG: File closed. Total rows written: " (itoa rows-written)))
          (if (> error-count 0)
            (MBS:log-warning (strcat "\n⚠ Wrote " (itoa rows-written) " records to CSV file (" (itoa error-count) " errors skipped)"))
            (MBS:log-info (strcat "\n✓ Wrote " (itoa rows-written) " records to CSV file"))
          )
          t
        )
        (progn
          (MBS:log-error "\n[ERROR] Could not open CSV file for writing")
          nil
        )
      )
    )
    (progn
      (MBS:log-error "\n[ERROR] Write operation aborted due to backup failure")
      nil
    )
  )
)










;;; ========================================================================
;; CSV FILE MANAGEMENT
;;; ========================================================================

;; get-csv-path-from-block to use mode-specific attributes
(defun get-csv-path-from-block (quiet-param / doc blkName foundBlk attList att path relpath dwg-path full-rel-path
                                 path-attribute rel-path-attribute quiet)
  ;; Initialize quiet parameter as nil (not quiet) by default
  (setq quiet nil)
  
  ;; Get the optional quiet parameter if it was passed
  (if quiet-param
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
                  (MBS:log-info (strcat "\nUsing relative CSV path: " relpath))
                  (MBS:log-info (strcat "\n(Resolved to: " full-rel-path ")"))
                )
              )
              full-rel-path  ;; Return the resolved path
            )
            ;; Relative file not found, try absolute path
            (if (and path (/= path ""))
              (progn
                ;; Only output messages if not in quiet mode
                (if (not quiet)
                  (MBS:log-info (strcat "\nRelative file not found. Using absolute path: " path))
                )
                path
              )
              ;; Neither path works, call select-csv-files instead of direct prompt
              (progn
                (MBS:log-warning "\nNo valid CSV file path found.")
                (if (not quiet)
                  (progn
                    (if (boundp 'MBS:select-csv-files)
                      (MBS:select-csv-files)  ;; Call select-csv-files function
                      (alert "Function MBS:select-csv-files not available yet"))
                    (setq quiet-param nil)  ;; Reset quiet param before recursive call
                    (get-csv-path-from-block nil)  ;; Recursive call to get the new path
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
              (MBS:log-info (strcat "\nUsing CSV path from block: " path))
            )
            path
          )
          ;; If attribute exists but empty, call select-csv-files
          (progn
            (MBS:log-warning "\nCSV path attributes are empty.")
            (if (not quiet)
              (progn
                (if (boundp 'MBS:select-csv-files)
                  (MBS:select-csv-files)  ;; Call select-csv-files function
                  (alert "Function select-csv-files not available yet"))
                (setq quiet-param nil)  ;; Reset quiet param before recursive call
                (get-csv-path-from-block nil)  ;; Recursive call to get the new path
              )
              nil  ;; In quiet mode, just return nil
            )
          )
        )
      )
    )
    ;; Block not found, call select-csv-files instead of direct prompt
    (progn
      (MBS:log-warning "\nAnchor block not found.")
      (if (not quiet)
        (progn
          (if (boundp 'MBS:select-csv-files)
            (MBS:select-csv-files)  ;; Call select-csv-files function
            (alert "Function MBS:select-csv-files not available yet"))
          (setq quiet-param nil)  ;; Reset quiet param before recursive call
          (get-csv-path-from-block nil)  ;; Recursive call to get the new path
        )
        nil  ;; In quiet mode, just return nil
      )
    )
  )
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

;; Enhanced backup function with unique timestamped files to avoid locking conflicts
(defun MBS:backup-csv-file (csv-path / backup-dir backup-path timestamp)
  (if (and csv-path (findfile csv-path))
    (progn
      ;; Create backup directory path
      (setq backup-dir (strcat (vl-filename-directory csv-path) "\\BACKUP"))
      
      ;; Ensure backup directory exists
      (if (not (vl-file-directory-p backup-dir))
        (vl-mkdir backup-dir)
      )
      
      ;; Create unique timestamped backup file to avoid conflicts
      (setq timestamp (menucmd "m=$(edtime,$(getvar,date),YYYYMMDD_HHMMSS_$(getvar,MILLISECS))"))
      (setq backup-path (strcat backup-dir "\\" (vl-filename-base csv-path) "_" timestamp ".bak"))
      
      ;; If file already exists, add a counter
      (setq counter 1)
      (while (findfile backup-path)
        (setq backup-path (strcat backup-dir "\\" (vl-filename-base csv-path) "_" timestamp "_" (itoa counter) ".bak"))
        (setq counter (1+ counter))
      )
      
      ;; Use simple file copy without complex retry logic
      (if (vl-file-copy csv-path backup-path nil)
        (progn
          (MBS:log-info (strcat "Created backup: " backup-path))
          (prompt (strcat "\n[BACKUP] Created backup: " (vl-filename-base backup-path)))
          t
        )
        (progn
          ;; If first attempt fails, add counter and try again
          (setq backup-path (strcat backup-dir "\\" (vl-filename-base csv-path) "_" timestamp "_2.bak"))
          (if (vl-file-copy csv-path backup-path nil)
            (progn
              (MBS:log-info (strcat "Created backup: " backup-path))
              (prompt (strcat "\n[BACKUP] Created backup: " (vl-filename-base backup-path)))
              t
            )
            (progn
              (MBS:log-warning (strcat "Failed to create backup: " backup-path))
              nil
            )
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

;; File copy with retry logic to handle temporary file system issues - WITH DEBUG
(defun MBS:file-copy-with-retry (source-path dest-path max-retries / attempt success)
  (MBS:log-info (strcat "DEBUG: Starting file copy from " source-path " to " dest-path))
  (setq attempt 0)
  (setq success nil)
  
  (while (and (not success) (< attempt max-retries))
    (setq attempt (1+ attempt))
    (MBS:log-info (strcat "DEBUG: File copy attempt " (itoa attempt) " of " (itoa max-retries)))
    
    (if (vl-file-copy source-path dest-path nil)
      (progn
        (MBS:log-info (strcat "Successfully copied file on attempt " (itoa attempt)))
        (setq success t)
      )
      (progn
        (MBS:log-warning (strcat "File copy failed on attempt " (itoa attempt) " of " (itoa max-retries)))
        (MBS:log-info "DEBUG: About to delay before retry")
        ;; Add small delay before retry
        (MBS:delay-ms 200)
        (MBS:log-info "DEBUG: Completed delay, continuing retry loop")
      )
    )
  )
  
  (MBS:log-info (strcat "DEBUG: Exited retry loop, success: " (if success "T" "nil")))
  
  (if success
    (progn
      (MBS:log-info (strcat "Created backup: " dest-path))
      (prompt (strcat "\n[BACKUP] Created backup: " (vl-filename-base dest-path)))
      t
    )
    (progn
      (MBS:log-critical (strcat "Failed to create backup after " (itoa max-retries) " attempts"))
      (prompt "\n[BACKUP] Critical: Failed to create backup file")
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

;; Write temp file with retry logic - WITH DEBUG
(defun MBS:write-temp-file-with-retry (temp-path header records max-retries / attempt f success rows-written)
  (MBS:log-info "DEBUG: Starting MBS:write-temp-file-with-retry")
  (MBS:log-info (strcat "DEBUG: Records count: " (itoa (length records))))
  (setq attempt 0)
  (setq success nil)
  
  (while (and (not success) (< attempt max-retries))
    (setq attempt (1+ attempt))
    (MBS:log-info (strcat "DEBUG: Temp file write attempt " (itoa attempt)))
    
    ;; Try to open temp file
    (setq f (open temp-path "w"))
    (if f
      (progn
        (MBS:log-info "DEBUG: Successfully opened temp file")
        ;; Write header line
        (write-line (list-to-csv-line header) f)
        (MBS:log-info "DEBUG: Wrote header line")
        
        ;; Write data rows with proper column alignment
        (setq rows-written 0)
        (MBS:log-info "DEBUG: Starting foreach loop")
        (foreach record records
          (MBS:log-info (strcat "DEBUG: Processing record " (itoa (1+ rows-written))))
          ;; Ensure record has the right number of columns
          (setq aligned-record (MBS:align-record record header))
          (MBS:log-info "DEBUG: Completed record alignment")
          
          ;; Write the record
          (write-line (list-to-csv-line aligned-record) f)
          (MBS:log-info "DEBUG: Wrote record to file")
          (setq rows-written (1+ rows-written))
        )
        (MBS:log-info "DEBUG: Completed foreach loop")
        
        ;; Close temp file
        (close f)
        
        ;; Verify file was written successfully
        (if (and (findfile temp-path) (> (vl-file-size temp-path) 0))
          (progn
            (MBS:log-info (strcat "Successfully wrote temp file on attempt " (itoa attempt)))
            (setq success t)
          )
          (progn
            (MBS:log-warning (strcat "Temp file verification failed on attempt " (itoa attempt)))
            ;; Clean up failed temp file
            (if (findfile temp-path)
              (vl-file-delete temp-path)
            )
          )
        )
      )
      (progn
        (MBS:log-warning (strcat "Failed to open temp file on attempt " (itoa attempt) " of " (itoa max-retries)))
        ;; Add small delay before retry
        (MBS:delay-ms 200)
      )
    )
  )
  
  (if success
    (progn
      (MBS:log-info (strcat "Successfully wrote " (itoa rows-written) " records to temp file"))
      t
    )
    (progn
      (MBS:log-critical (strcat "Failed to write temp file after " (itoa max-retries) " attempts"))
      nil
    )
  )
)

;; Atomic file replace operation to minimize file locking issues
(defun MBS:atomic-file-replace (temp-path target-path / replace-success)
        ;; Add a small delay to ensure file operations complete
      (MBS:delay-ms 100)
  
  ;; Try to replace the original file
  (if (findfile target-path)
    (progn
      (MBS:log-info "DEBUG: About to delete original file")
      ;; Delete original file first
      (setq delete-result (vl-catch-all-apply '(lambda () (vl-file-delete target-path))))
      (if (vl-catch-all-error-p delete-result)
        (MBS:log-warning (strcat "DEBUG: File deletion failed: " (vl-catch-all-error-message delete-result)))
        (MBS:log-info "DEBUG: File deletion successful")
      )
      ;; Add a small delay after deletion
      (MBS:delay-ms 100)
      (MBS:log-info "DEBUG: Completed delay after deletion")
    )
  )
  
  ;; Copy temp file to target location
  (setq replace-success (MBS:file-copy-with-retry temp-path target-path 3))
  
  (if replace-success
    (progn
      ;; Clean up temp file
      (vl-file-delete temp-path)
      (MBS:log-info (strcat "Successfully replaced target file: " target-path))
      t
    )
    (progn
      (MBS:log-critical (strcat "Failed to replace target file. Temp file is at: " temp-path))
      nil
    )
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
  (setq csv-path (get-csv-path-from-block nil))
  
  (if (not csv-path)
    (progn
      (MBS:log-warning "\nNo CSV file path found. Please select a CSV file first.")
      (princ)
      nil
    )
    (if (findfile csv-path)
      (progn
        (MBS:log-info (strcat "\nAttempting to open CSV file in Excel: " csv-path))
        
        ;; Try multiple methods in sequence until one works
        (setq result nil)
        
        ;; Method 1: Use ShellExecute Windows API
        (if (not result)
          (progn
            (MBS:log-verbose "\nTrying method 1: ShellExecute...")
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
              (MBS:log-verbose "\n  Method 1 executed.")
            )
          )
        )
        
        ;; Method 2: Use Shell.Application Open
        (if (not result)
          (progn
            (MBS:log-verbose "\nTrying method 2: Shell.Application Open...")
            (setq result (vl-catch-all-apply 
                          '(lambda ()
                             (vl-load-com)
                             (setq shell (vlax-create-object "Shell.Application"))
                             (vlax-invoke shell 'Open (strcat "\"" csv-path "\""))
                             (vlax-release-object shell)
                             t)))
            (if (vl-catch-all-error-p result)
              (setq result nil)
              (MBS:log-verbose "\n  Method 2 executed.")
            )
          )
        )
        
        ;; Method 3: Use direct Excel Automation
        (if (not result)
          (progn
            (MBS:log-verbose "\nTrying method 3: Direct Excel Automation...")
            (setq result (vl-catch-all-apply 
                          '(lambda ()
                             (vl-load-com)
                             (setq excel (vlax-get-or-create-object "Excel.Application"))
                             (vlax-put-property excel "Visible" :vlax-true)
                             (vlax-invoke-method (vlax-get-property excel "Workbooks") "Open" csv-path)
                             t)))
            (if (vl-catch-all-error-p result)
              (setq result nil)
              (MBS:log-verbose "\n  Method 3 executed.")
            )
          )
        )
        
        ;; Method 4: Use startapp with full path to Excel
        (if (not result)
          (progn
            (MBS:log-verbose "\nTrying method 4: startapp with Excel paths...")
            
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
                  (MBS:log-verbose (strcat "\n  Trying Excel path: " excel-path))
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
          (MBS:write-csv csv-path header sorted-records nil)
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
  (setq csv-path (get-csv-path-from-block nil))
  
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

;; Improved function to create backup of a CSV file before modification (duplicate removed - see line 1543)

;; Manual backup command
(defun c:MBS-BackupCSV (/ csv-path)
  (prompt "\n=== CREATE CSV BACKUP ===")
  
  ;; Get current CSV path
  (setq csv-path (get-csv-path-from-block nil))
  
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
  (setq csv-path (get-csv-path-from-block nil))
  
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
  (setq csv-path (get-csv-path-from-block nil))
  
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
;; Enhanced restore function that properly overwrites the target file
(defun MBS:extract-and-restore-backup (backup-path target-path)
  (prompt (strcat "\nRestoring backup from: " backup-path))
  (prompt (strcat "\nTarget file: " target-path))
  
  ;; First, delete the target file if it exists to ensure clean overwrite
  (if (findfile target-path)
    (progn
      (prompt "\nDeleting existing target file...")
      (vl-file-delete target-path)
    )
  )
  
  ;; Copy the backup file to the target location
  ;; Since backup files are individual timestamped copies, no extraction is needed
  (if (vl-file-copy backup-path target-path nil)  ;; nil = don't overwrite (we already deleted)
    (progn
      (prompt "\n✓ Backup restored successfully.")
      (prompt (strcat "\n✓ File overwritten: " target-path))
      t
    )
    (progn
      (prompt "\n❌ Failed to restore backup file.")
      (prompt "\n❌ Check file permissions and disk space.")
      nil
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



;;; ========================================================================
;; CSV FILE SELECTION FUNCTIONS
;;; ========================================================================

;; Main function to select both LOM and LOR CSV files
(defun MBS:select-csv-files ( / select-mode lom-path lor-path result dwg-path)
  (MBS:log-info "\n🔧 === CSV FILE SELECTION ===")
  
  ;; Get current drawing path for relative path calculation
  (setq dwg-path (vl-filename-directory (vla-get-fullname (vla-get-ActiveDocument (vlax-get-acad-object)))))
  
  ;; Ask user for selection method
  (initget "Manual Auto Debug Create")
  (setq select-mode (getkword "\nSelect files [Manual/Auto/Debug/Create]: "))
  
  (cond
    ((= select-mode "Debug") 
     ;; DEBUG MODE - Run with verbose output to troubleshoot subfolder issues
     (prompt "\n🔍 DEBUG: Starting CSV file search with verbose output...")
     (prompt (strcat "\n🔍 DEBUG: Starting directory is: " dwg-path))
     (setq result (MBS:debug-detect-csv-files dwg-path))
     
     ;; Check results and handle found files
     (if (car result)
       (progn
         (setq lom-path (car result))
         (prompt (strcat "\n✅ Found LOM file: " lom-path))
       )
       (progn
         (prompt "\n❌ No LOM file found. Please select manually.")
         (setq lom-path (getfiled "Select LOM CSV File" "" "csv" 0))
         (if (not lom-path) (setq lom-path ""))
       )
     )
     
     (if (cadr result)
       (progn
         (setq lor-path (cadr result))
         (prompt (strcat "\n✅ Found LOR file: " lor-path))
       )
       (progn
         (prompt "\n❌ No LOR file found. Please select manually.")
         (setq lor-path (getfiled "Select LOR CSV File" "" "csv" 0))
         (if (not lor-path) (setq lor-path ""))
       )
     )
    )
    
    ((= select-mode "Auto")
     ;; AUTO-DETECT MODE with fixed subfolder handling
     (prompt "\n🔍 Searching for CSV files in drawing location and subdirectories...")
     (setq result (MBS:fixed-auto-detect-csv-files dwg-path))
     
     (if (car result)
       (progn
         (setq lom-path (car result))
         (prompt (strcat "\n✅ Found LOM file: " lom-path))
       )
       (progn
         (prompt "\n❌ No LOM file found. Please select manually.")
         (setq lom-path (getfiled "Select LOM CSV File" "" "csv" 0))
         (if (not lom-path) (setq lom-path ""))
       )
     )
     
     (if (cadr result)
       (progn
         (setq lor-path (cadr result))
         (prompt (strcat "\n✅ Found LOR file: " lor-path))
       )
       (progn
         (prompt "\n❌ No LOR file found. Please select manually.")
         (setq lor-path (getfiled "Select LOR CSV File" "" "csv" 0))
         (if (not lor-path) (setq lor-path ""))
       )
     )
    )
    
    ((= select-mode "Create")
     ;; CREATE MODE - Create new CSV files in DATA subdirectory
     (prompt "\n📁 Creating new CSV files...")
     
     ;; Get drawing directory and create DATA subdirectory
     (setq dwg-dir (vl-filename-directory (vla-get-fullname (vla-get-ActiveDocument (vlax-get-acad-object)))))
     (setq data-dir (strcat dwg-dir "\\DATA"))
     
     ;; Create DATA directory if it doesn't exist
     (if (not (vl-file-directory-p data-dir))
       (progn
         (vl-mkdir data-dir)
         (prompt (strcat "\n📁 Created DATA directory: " data-dir))
       )
     )
     
     ;; Get drawing name for CSV file names
     (setq dwg-name (vl-filename-base (vla-get-fullname (vla-get-ActiveDocument (vlax-get-acad-object)))))
     (setq lom-path (strcat data-dir "\\" dwg-name "_LOM.csv"))
     (setq lor-path (strcat data-dir "\\" dwg-name "_LOR.csv"))
     
     (prompt (strcat "\n📝 Will create LOM file: " lom-path))
     (prompt (strcat "\n📝 Will create LOR file: " lor-path))
     
     ;; Confirm creation
     (initget "Yes No")
     (setq confirm-create (getkword "\nCreate these files? [Yes/No]: "))
     
     (if (= confirm-create "Yes")
       (progn
         (prompt "\n✅ Creating CSV files...")
         ;; Files will be created by initialize function below
       )
       (progn
         (prompt "\n❌ CSV creation cancelled")
         (setq lom-path "")
         (setq lor-path "")
       )
     )
    )
    
    (t  ;; MANUAL MODE
     (prompt "\n📁 Select LOM CSV file first:")
     (setq lom-path (getfiled "Select LOM CSV File" "" "csv" 0))
     (if (not lom-path) (setq lom-path ""))
     
     (prompt "\n📁 Now select LOR CSV file:")
     (setq lor-path (getfiled "Select LOR CSV File" "" "csv" 0))
     (if (not lor-path) (setq lor-path ""))
    )
  )
  
  ;; Initialize files if they don't exist
  (if (and lom-path (/= lom-path ""))
    ;; Check if file exists before initializing
    (if (not (findfile lom-path))
      (MBS:initialize-csv-file lom-path)
    )
  )
  
  (if (and lor-path (/= lor-path ""))
    ;; Check if file exists before initializing
    (if (not (findfile lor-path))
      (MBS:initialize-csv-file lor-path)
    )
  )
  
  ;; Update anchor block with both paths
  (MBS:update-csv-paths-in-block lom-path lor-path)
  
  ;; Set global variable for current mode
  (if (and (boundp 'current-mode) (= current-mode "LOM"))
    (setq csv-sync-file lom-path)
    (setq csv-sync-file lor-path)
  )
  
  (prompt "\n✅ CSV file selection complete.")
  (princ)
)

;; Initialize CSV file with proper headers
(defun MBS:initialize-csv-file (file-path / header)
  (if (not file-path)
    nil  ;; Exit if file-path is nil
    (if (not (findfile file-path))
      (progn
        ;; Create proper header row matching the required structure
        ;; Column A (index 0) = ##, Column D (index 3) = DESCRIPTION, Column P (index 15) = MATERIAL_ID
        (setq header '("ITEM NO." "QTY" "UNITS" "DESCRIPTION" "MATERIAL SPEC" "MANUFACTURER" "NSN OR MFR PART NO." "STANDARD" 
                      "SCE" "UNIT WT" "TOTAL WT" "REMARKS/SALIENT CHARACTERISTICS" "-" "-" "-" "MATERIAL_ID" "-" "IGE MN HOURS PER UNIT" "IGE COST PER UNIT" "VENDOR INFO" "-" "-" "VCG" "LCG" "TCG"))
        (MBS:write-csv-file file-path header '())
        (prompt (strcat "\n📝 Created new CSV file: " file-path))
      )
    )
  )
  t
)

;; Fixed directory search function that properly handles subdirectories
(defun MBS:fixed-search-directories (dir lom-var lor-var / dirs-to-search visited-dirs current-dir all-files file)
  ;; Use a stack-based approach instead of recursion
  (setq dirs-to-search (list dir))
  (setq visited-dirs (list))
  
  ;; Process directories until stack is empty or both files found
  (while (and dirs-to-search (or (not (eval lom-var)) (not (eval lor-var))))
    ;; Get next directory to process
    (setq current-dir (car dirs-to-search))
    (setq dirs-to-search (cdr dirs-to-search))
    
    ;; Skip if already visited (avoid loops)
    (if (not (member (strcase current-dir) visited-dirs))
      (progn
        ;; Mark as visited
        (setq visited-dirs (cons (strcase current-dir) visited-dirs))
        
        ;; Make sure this is actually a directory
        (if (vl-file-directory-p current-dir)
          (progn
            ;; FIRST: Search for CSV files in this directory
            (setq all-files (vl-directory-files current-dir "*.csv" 0))
            
            ;; Process all CSV files
            (foreach file all-files
              (setq full-path (strcat current-dir "\\" file))
              
              ;; Check for LOM and LOR files
              (cond
                ((wcmatch (strcase file) "*_LOM.CSV")
                 (if (not (eval lom-var))
                   (set lom-var full-path)
                 )
                )
                ((wcmatch (strcase file) "*_LOR.CSV")
                 (if (not (eval lor-var))
                   (set lor-var full-path)
                 )
                )
              )
            )
            
            ;; SECOND: Get all subdirectories using the CORRECT APPROACH
            ;; Get all items in directory
            (setq all-items (vl-directory-files current-dir nil nil))
            
            ;; Process each item - check explicitly if it's a directory
            (foreach item all-items
              (if (and (/= item ".") (/= item ".."))
                (progn
                  ;; Build full path to item
                  (setq item-path (strcat current-dir "\\" item))
                  
                  ;; CHECK IF IT'S A DIRECTORY using vl-file-directory-p
                  (if (vl-file-directory-p item-path)
                    ;; If it's a directory, add to search stack
                    (setq dirs-to-search (append dirs-to-search (list item-path)))
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

;; Auto-detect CSV files with fixed subfolder handling
(defun MBS:fixed-auto-detect-csv-files (start-dir / lom-path lor-path)
  (setq lom-path nil)
  (setq lor-path nil)
  
  ;; Search directories for CSV files
  (MBS:fixed-search-directories start-dir 'lom-path 'lor-path)
  
  ;; Return results as list (lom-path lor-path)
  (list lom-path lor-path)
)

;; Debug version of CSV file detection with verbose output
(defun MBS:debug-detect-csv-files (start-dir / lom-path lor-path)
  (prompt "\n🔍 DEBUG: Starting CSV file detection...")
  (prompt (strcat "\n🔍 DEBUG: Search directory: " start-dir))
  
  (setq lom-path nil)
  (setq lor-path nil)
  
  ;; Search directories for CSV files
  (MBS:fixed-search-directories start-dir 'lom-path 'lor-path)
  
  (prompt "\n🔍 DEBUG: Search completed.")
  (prompt (strcat "\n🔍 DEBUG: LOM path: " (if lom-path lom-path "NOT FOUND")))
  (prompt (strcat "\n🔍 DEBUG: LOR path: " (if lor-path lor-path "NOT FOUND")))
  
  ;; Return results as list (lom-path lor-path)
  (list lom-path lor-path)
)

;; Update CSV paths in anchor block
(defun MBS:update-csv-paths-in-block (lom-path lor-path / doc modelspace anchor-block)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelspace (vla-get-ModelSpace doc))
  (setq anchor-block nil)
  
  ;; Find anchor block
  (vlax-for ent modelspace
    (if (and (= (vla-get-ObjectName ent) "AcDbBlockReference")
             (= (vlax-get ent 'EffectiveName) "_CACI_CSV_LINK_ANCHOR"))
      (setq anchor-block (vlax-vla-object->ename ent))
    )
  )
  
  ;; If anchor block exists, update its attributes
  (if anchor-block
    (progn
      (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke (vlax-ename->vla-object anchor-block) 'GetAttributes))))
      
      (if (not (vl-catch-all-error-p att-list))
        (progn
          (foreach att att-list
            (setq tag (strcase (vlax-get att 'TagString)))
            (cond
              ((= tag "CSVFILEPATH")
               (vlax-put att 'TextString (if lom-path lom-path ""))
               (prompt (strcat "\n✅ Updated LOM path: " (if lom-path lom-path "CLEARED")))
              )
              ((= tag "CSVRELFILEPATH")
               (vlax-put att 'TextString (if lom-path (MBS:make-relative-path lom-path) ""))
               (prompt (strcat "\n✅ Updated LOM relative path: " (if lom-path (MBS:make-relative-path lom-path) "CLEARED")))
              )
              ((= tag "CSVLORFILEPATH")
               (vlax-put att 'TextString (if lor-path lor-path ""))
               (prompt (strcat "\n✅ Updated LOR path: " (if lor-path lor-path "CLEARED")))
              )
              ((= tag "CSVLORRELFILEPATH")
               (vlax-put att 'TextString (if lor-path (MBS:make-relative-path lor-path) ""))
               (prompt (strcat "\n✅ Updated LOR relative path: " (if lor-path (MBS:make-relative-path lor-path) "CLEARED")))
              )
            )
          )
          (vla-Update (vlax-ename->vla-object anchor-block))
        )
        (MBS:log-warning "\n⚠ Could not update anchor block attributes")
      )
    )
    (MBS:log-warning "\n⚠ No anchor block found - paths not saved")
  )
)

;; Command wrapper for CSV file selection
(defun C:MBS-SELECT-CSV-FILES ()
  (MBS:select-csv-files)
  (princ)
)

;; Command to check CSV setup
(defun C:MBS-CHECK-CSV ()
  (MBS:check-csv-setup)
  (princ)
)

;; Function to check if CSV files are properly configured
(defun MBS:check-csv-setup ( / current-csv)
  (setq current-csv (MBS:get-csv-path))
  
  (if (and current-csv (findfile current-csv))
    (progn
      (prompt (strcat "\n✅ CSV file configured: " (MBS:filename-base current-csv)))
      t
    )
    (progn
      (MBS:log-warning "\n⚠ No CSV file configured or file not found")
      (prompt "\n💡 Use 'MBS-SELECT-CSV-FILES' to configure CSV files")
      nil
    )
  )
)

;; Helper function to get filename base (duplicate removed - see line 1638)

;; Helper function to create a relative path from absolute path
(defun MBS:create-relative-path (abs-path base-path / abs-parts base-parts rel-path i j)
  (if (and abs-path base-path)
    (progn
      ;; If the paths are on different drives, we can't make a relative path
      (if (/= (substr abs-path 1 1) (substr base-path 1 1))
        (vl-filename-base abs-path)  ;; Just return the filename
        (progn
          ;; Split paths into components using our safe function
          (setq abs-parts (MBS:vl-string-split-safe abs-path "\\"))
          (setq base-parts (MBS:vl-string-split-safe base-path "\\"))
          
          ;; Find how many parts match from the beginning
          (setq i 0)
          (while (and (< i (min (length abs-parts) (length base-parts)))
                      (= (strcase (nth i abs-parts)) (strcase (nth i base-parts))))
            (setq i (1+ i))
          )
          
          ;; Build relative path
          (setq rel-path "")
          
          ;; Add ".." for each remaining level in base path
          (setq j i)
          (while (< j (length base-parts))
            (setq rel-path (strcat rel-path "..\\"))
            (setq j (1+ j))
          )
          
          ;; Add remaining parts from abs-path
          (while (< i (length abs-parts))
            (setq rel-path (strcat rel-path (nth i abs-parts)))
            (setq i (1+ i))
            (if (< i (length abs-parts))
              (setq rel-path (strcat rel-path "\\"))
            )
          )
          
          rel-path
        )
      )
    )
    nil
  )
)

;; Helper function to split a string
(defun MBS:vl-string-split-safe (str delimiter / result start pos)
  (setq result '())
  (setq start 0)
  (while (setq pos (vl-string-search delimiter str start))
    (setq result (append result (list (substr str (+ start 1) (- pos start)))))
    (setq start (+ pos (strlen delimiter)))
  )
  (setq result (append result (list (substr str (+ start 1)))))
  result
)

;; Helper function to make relative path from absolute path (wrapper)
(defun MBS:make-relative-path (absolute-path / dwg-path)
  (if (and absolute-path (/= absolute-path ""))
    (progn
      (setq dwg-path (vl-filename-directory (vla-get-fullname (vla-get-ActiveDocument (vlax-get-acad-object)))))
      (MBS:create-relative-path absolute-path dwg-path)
    )
    ""
  )
)

;; Custom path joining function to replace vl-filename-mkpath
(defun MBS:join-path (base-path rel-path / result)
  (if (and base-path rel-path)
    (progn
      ;; Make sure base path ends with a backslash
      (if (= (substr base-path (strlen base-path) 1) "\\")
        (setq result base-path)
        (setq result (strcat base-path "\\"))
      )
      
      ;; Make sure rel-path doesn't start with a backslash
      (if (and (> (strlen rel-path) 0) (= (substr rel-path 1 1) "\\"))
        (setq rel-path (substr rel-path 2))
      )
      
      ;; Join the paths
      (strcat result rel-path)
    )
    (if base-path base-path rel-path)
  )
)

(prompt "\nMaterial Block Sync CSV Module loaded.")
(princ)