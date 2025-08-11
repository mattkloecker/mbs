;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - Block Operations Module
;; VERSION 6.0 - UNIFIED ARCHITECTURE
;;; ========================================================================
;; This module implements the unified block operations for Material Block Sync:
;; - Unified INSERT with batch/single/text/prefix/filter options
;; - Unified UPDATE with audit and orphan handling
;; - Common foundation functions to eliminate inconsistencies
;;
;; Dependencies: mbs_core.lsp, mbs_config.lsp, mbs_csv.lsp, mbs_transaction.lsp
;;; ========================================================================

;;; ========================================================================
;; HELPER FUNCTIONS
;;; ========================================================================

;; Parse comma-separated selection like "1,2,3" to return selected records - WITH DEBUG
(defun MBS:parse-selection (selection matches / selected-records selected-indices tokens token index token-num)
  "Parse comma-separated selection and return matching records"
  
  (prompt (strcat "\n🔍 DEBUG: Input selection = '" selection "'"))
  
  (setq selected-records '())
  (setq selected-indices '())
  
  ;; Split selection by commas
  (setq tokens (MBS:string-split selection ","))
  (prompt (strcat "\n🔍 DEBUG: Split into " (itoa (length tokens)) " tokens"))
  
  ;; Convert each token to index and validate
  (setq token-num 0)
  (foreach token tokens
    (prompt (strcat "\n🔍 DEBUG: Token[" (itoa token-num) "] = '" token "'"))
    (setq token (MBS:string-trim token))  ;; Remove spaces
    (prompt (strcat "\n🔍 DEBUG: After trim = '" token "'"))
    (if (and token (/= token ""))
      (progn
        (setq index (atoi token))
        (prompt (strcat "\n🔍 DEBUG: Parsed index = " (itoa index)))
        (if (and (> index 0) (<= index (length matches)))
          (progn
            (setq selected-indices (append selected-indices (list (1- index))))  ;; Convert to 0-based
            (prompt (strcat "\n🔍 DEBUG: Added 0-based index " (itoa (1- index))))
          )
          (prompt (strcat "\nWarning: Invalid selection '" token "' - skipping"))
        )
      )
    )
    (setq token-num (1+ token-num))
  )
  
  (prompt (strcat "\n🔍 DEBUG: Selected indices: " (vl-princ-to-string selected-indices)))
  
  ;; Get selected records
  (foreach index selected-indices
    (if (< index (length matches))
      (setq selected-records (append selected-records (list (nth index matches))))
    )
  )
  
  (prompt (strcat "\nSelected " (itoa (length selected-records)) " records from " (itoa (length matches)) " matches"))
  selected-records
)

;; Helper function to split string by delimiter - FIXED
(defun MBS:string-split (str delimiter / result pos next-pos token)
  "Split string by delimiter"
  (setq result '())
  (setq pos 1)
  
  (while (<= pos (strlen str))
    (setq next-pos (vl-string-search delimiter str (1- pos)))
    (if next-pos
      (progn
        ;; Extract token before delimiter
        (if (>= next-pos pos)
          (setq token (substr str pos (- (1+ next-pos) pos)))
          (setq token "")  ;; Empty token
        )
        (setq result (append result (list token)))
        (setq pos (+ next-pos 2))  ;; Move past delimiter
      )
      (progn
        ;; Last token (no more delimiters)
        (setq token (substr str pos))
        (setq result (append result (list token)))
        (setq pos (1+ (strlen str)))  ;; Exit loop
      )
    )
  )
  
  result
)

;; Helper function to trim spaces
(defun MBS:string-trim (str / start-pos end-pos)
  "Remove leading and trailing spaces"
  (if (not str)
    ""
    (progn
      ;; Find first non-space
      (setq start-pos 1)
      (while (and (<= start-pos (strlen str)) (= (substr str start-pos 1) " "))
        (setq start-pos (1+ start-pos))
      )
      
      ;; Find last non-space
      (setq end-pos (strlen str))
      (while (and (> end-pos 0) (= (substr str end-pos 1) " "))
        (setq end-pos (1- end-pos))
      )
      
      ;; Extract trimmed string
      (if (> start-pos end-pos)
        ""
        (substr str start-pos (1+ (- end-pos start-pos)))
      )
    )
  )
)

;; Sort matches by ITEM NO. for intuitive display (R1, R2, R3... instead of R7, R6, R5...)
(defun MBS:sort-matches-by-item-no (matches num-index / sorted-list match record item-no prefix number i result item)
  "Sort matches by ITEM NO. - handles both numeric and prefixed items"
  
  ;; Extract sort keys and create tuples
  (setq sorted-list '())
  (foreach match matches
    (setq record (cdr match))
    (setq item-no (if (>= (length record) (1+ num-index)) (nth num-index record) ""))
    
    ;; Extract prefix and number for sorting
    (if (and item-no (/= item-no ""))
      (progn
        ;; Find where digits start
        (setq i 0)
        (setq prefix "")
        (while (and (< i (strlen item-no)) 
                   (not (and (>= (ascii (substr item-no (1+ i) 1)) 48)  ;; '0'
                            (<= (ascii (substr item-no (1+ i) 1)) 57)))) ;; '9'
          (setq prefix (strcat prefix (substr item-no (1+ i) 1)))
          (setq i (1+ i))
        )
        
        ;; Extract number part
        (if (< i (strlen item-no))
          (setq number (atoi (substr item-no (1+ i))))
          (setq number 0)
        )
        
        ;; Add to sort list: (prefix number original-match)
        (setq sorted-list (append sorted-list (list (list prefix number match))))
      )
      ;; No item number - put at end
      (setq sorted-list (append sorted-list (list (list "ZZZ" 9999 match))))
    )
  )
  
  ;; Simple bubble sort by prefix then number
  (setq sorted-list (MBS:bubble-sort-matches sorted-list))
  
  ;; Extract just the matches
  (setq result '())
  (foreach item sorted-list
    (setq result (append result (list (nth 2 item))))
  )
  
  result
)

;; Simple bubble sort for match sorting
(defun MBS:bubble-sort-matches (lst / n i j temp swapped item1 item2 prefix1 number1 prefix2 number2)
  "Bubble sort for (prefix number match) tuples"
  (setq n (length lst))
  (setq i 0)
  
  (while (< i (1- n))
    (setq swapped nil)
    (setq j 0)
    
    (while (< j (- n i 1))
      ;; Compare (prefix1 number1) with (prefix2 number2)
      (setq item1 (nth j lst))
      (setq item2 (nth (1+ j) lst))
      
      (setq prefix1 (nth 0 item1))
      (setq number1 (nth 1 item1))
      (setq prefix2 (nth 0 item2))
      (setq number2 (nth 1 item2))
      
      ;; Sort by prefix first, then by number
      (if (or (> (strcase prefix1) (strcase prefix2))
              (and (= (strcase prefix1) (strcase prefix2)) (> number1 number2)))
        (progn
          ;; Swap
          (setq temp (nth j lst))
          (setq lst (MBS:replace-nth lst j (nth (1+ j) lst)))
          (setq lst (MBS:replace-nth lst (1+ j) temp))
          (setq swapped t)
        )
      )
      
      (setq j (1+ j))
    )
    
    (if (not swapped)
      (setq i n)  ;; Exit early if no swaps
      (setq i (1+ i))
    )
  )
  
  lst
)

;; Helper to replace nth element in list
(defun MBS:replace-nth (lst n new-item / result i)
  "Replace nth element in list"
  (setq result '())
  (setq i 0)
  
  (foreach item lst
    (if (= i n)
      (setq result (append result (list new-item)))
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  
  result
)

(prompt "\nHelper functions loaded!")

;;; ========================================================================
;; UNIFIED CORE ENGINE FUNCTIONS  
;;; ========================================================================

;; Session context structure for maintaining state during operations
;; (csv-path header records block-map operation-type current-mode)
(defun MBS:create-session-context (csv-path header records block-map operation-type)
  "Creates a session context to maintain state during operations"
  (list csv-path header records block-map operation-type (MBS:get-mode))
)

;; Session Manager - Initialize environment and build block map
(defun MBS:start-session (csv-path operation-type / csv-data header records block-map validation-result)
  "Initialize session with validation, CSV reading, and block mapping"
  
  ;; Validate environment
  (if (not (MBS:init-environment))
    (progn
      (prompt "\n❌ Failed to initialize environment")
      nil
    )
    (progn
      ;; Validate CSV structure
      (setq validation-result (MBS:validate-csv-structure csv-path))
      (if (not (car validation-result))
        (progn
          (prompt (strcat "\n❌ CSV validation failed: " (cadr validation-result)))
          nil
        )
        (progn
          ;; Read CSV data using NEW CSV ENGINE
          (prompt "\n📖 Reading CSV data...")
          (setq csv-data (CSV:read-file csv-path))
          (if (not csv-data)
            (progn
              (prompt "\n❌ Failed to read CSV data")
              nil
            )
            (progn
              (setq header (car csv-data))
              (setq records (cadr csv-data))
              (prompt (strcat "\n✓ Successfully read " (itoa (length records)) " data rows"))
              
              ;; Build block map for this session using current mode
              (prompt "\n🔍 Building block map...")
              (prompt (strcat "\n[MODE] Using " (MBS:get-mode) " mode for block detection"))
              (setq block-map (MBS:build-material-id-map))
              (if block-map
                (prompt (strcat "\n✓ Found " (itoa (length (car block-map))) " blocks with IDs"))
                (MBS:log-warning "\n⚠ No blocks found or failed to build block map")
              )
              
              ;; Clear any existing transactions
              (MBS:clear-transactions)
              (prompt "\n✓ Session initialized successfully")
              
              ;; Return session context
              (MBS:create-session-context csv-path header records block-map operation-type)
            )
          )
        )
      )
    )
  )
)

;; Selection Engine - Unified record selection with all methods
(defun MBS:select-records (records header selection-mode / num-index desc-index selected-records)
  "Unified record selection supporting batch, single, text, prefix, and filter modes"
  
  ;; Get column indices using NEW CSV ENGINE
  (setq num-index (CSV:get-column-index header "ITEM NO."))
  (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
  
  (if (or (not num-index) (not desc-index))
    (progn
      (prompt "\n❌ Required columns (ITEM NO., DESCRIPTION) not found")
      nil
    )
    (progn
      (cond
        ;; Batch mode - select all records
        ((= selection-mode "Batch")
         (prompt (strcat "\n📦 Batch mode: Auto-selecting all " (itoa (length records)) " records"))
         (setq selected-records '())
         (setq row-index 0)
         (foreach record records
           (setq selected-records (append selected-records (list (cons row-index record))))
           (setq row-index (1+ row-index))
         )
         selected-records
        )
        
        ;; Single row selection
        ((= selection-mode "Single")
         (MBS:select-single-row records header num-index desc-index)
        )
        
        ;; Text search in descriptions
        ((= selection-mode "Text")
         (MBS:select-by-text-search records header num-index desc-index)
        )
        
        ;; Prefix filter
        ((= selection-mode "Prefix")
         (MBS:select-by-prefix-filter records header num-index desc-index)
        )
        
        ;; Advanced filter
        ((= selection-mode "Filter")
         (MBS:select-by-advanced-filter records header num-index desc-index)
        )
        
        ;; Invalid mode
        (t
         (prompt (strcat "\n❌ Invalid selection mode: " selection-mode))
         nil
        )
      )
    )
  )
)

;; Insertion Engine - Unified block insertion with layout options
(defun MBS:insert-blocks (selected-records session-context layout-mode / csv-path header success-count generated-ids)
  "Unified block insertion engine supporting grid and individual layouts"
  
  (if (not selected-records)
    (progn
      (prompt "\n❌ No records selected for insertion")
      nil
    )
    (progn
      ;; Extract session context
      (setq csv-path (nth 0 session-context))
      (setq header (nth 1 session-context))
      
      ;; Get column indices using NEW CSV ENGINE
      (setq num-index (CSV:get-column-index header "ITEM NO."))
      (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
      (setq mat-id-index (CSV:get-column-index header (get-id-column)))
      
      (if (or (not num-index) (not desc-index) (not mat-id-index))
        (progn
          (prompt "\n❌ Required columns not found for insertion")
          nil
        )
        (progn
          (prompt (strcat "\n🏗️ Inserting " (itoa (length selected-records)) " blocks in " layout-mode " mode"))
          
          ;; Get current space
          (setq space-result (MBS:get-current-space))
          (setq target-space (car space-result))
          (setq space-name (cadr space-result))
          (prompt (strcat "\n📍 Target space: " space-name))
          
          ;; Call appropriate layout function
          (prompt (strcat "\n🔍 DEBUG: Layout mode = '" layout-mode "'"))
          
          (cond
            ((= layout-mode "grid")
             (setq result (MBS:insert-blocks-grid selected-records target-space header num-index desc-index mat-id-index))
            )
            ((= layout-mode "individual")
             (setq result (MBS:insert-blocks-individual selected-records target-space header num-index desc-index mat-id-index))
            )
            (t
             (prompt (strcat "\n❌ Invalid layout mode: '" layout-mode "' (expected 'grid' or 'individual')"))
             nil
            )
          )
          
          ;; Return results
          (if result
            (progn
              (setq success-count (car result))
              (MBS:log-info (strcat "\n✅ Successfully inserted " (itoa success-count) " blocks"))
              result
            )
            (progn
              (prompt "\n❌ Block insertion failed")
              nil
            )
          )
        )
      )
    )
  )
)

;; Transaction Finalizer - Execute Stage 1 transactions
(defun MBS:finalize-stage1 (session-context / csv-path)
  "Execute all queued transactions for Stage 1 (ID generation, row operations)"
  
  (setq csv-path (nth 0 session-context))
  
  (if (> (length MBS:transaction-queue) 0)
    (progn
      (prompt (strcat "\n💾 Executing " (itoa (length MBS:transaction-queue)) " Stage 1 transactions..."))
      
      ;; Use NEW CSV ENGINE for transactions with current mode
      (if (CSV:execute-transactions csv-path MBS:transaction-queue (nth 5 session-context))
        (progn
          (MBS:log-info "\n✅ Stage 1 transactions completed successfully")
          ;; Clear transaction queue after successful execution
          (MBS:clear-transactions)
          t
        )
        (progn
          (prompt "\n❌ Stage 1 transactions failed")
          nil
        )
      )
    )
    (progn
      (prompt "\n📝 No Stage 1 transactions to execute")
      t
    )
  )
)

;; Item Numbering Engine - Unified Stage 2 processing
(defun MBS:process-item-numbers (session-context mode / csv-path)
  "Process item numbering (Stage 2) with preview or auto-apply modes"
  
  (setq csv-path (nth 0 session-context))
  
  (prompt "\n🔢 === STAGE 2: ITEM NUMBER UPDATE ===")
  
  ;; Use existing item numbering logic
  (if (= mode "preview")
    (MBS:update-all-item-numbers csv-path)
    (MBS:update-all-item-numbers csv-path)  ;; For now, same logic - can enhance later
  )
)

;; Block Synchronization - Final sync with CSV data
(defun MBS:sync-blocks-final (session-context / csv-path)
  "Final synchronization of blocks with updated CSV data"
  
  (setq csv-path (nth 0 session-context))
  
  (prompt "\n🔄 Synchronizing blocks with updated CSV data...")
  (setq sync-result (MBS:sync-blocks-with-csv csv-path))
  
  (if sync-result
    (MBS:log-info "\n✅ Block synchronization completed")
    (MBS:log-warning "\n⚠ Block synchronization had issues")
  )
  
  sync-result
)

(prompt "\nCore engine functions loaded!")

;;; ========================================================================
;; UNIFIED MAIN COMMANDS
;;; ========================================================================

;; Unified INSERT command with batch/single/text/prefix/filter options
(defun C:MBS-INSERT3 (/ csv-path session-context selection-mode layout-mode selected-records result)
  "Unified INSERT command supporting all selection and layout modes"
  
  ;; Get CSV path
  (if (boundp 'get-csv-path-from-block)
    (setq csv-path (get-csv-path-from-block nil))
    (setq csv-path nil)
  )
  (if (not csv-path)
    (progn
      (prompt "\n❌ No CSV file found or specified")
      (princ)
    )
    (progn
      (prompt "\n🚀 [UNIFIED INSERT - Multi-mode support]")
      (prompt (strcat "\nUsing CSV: " csv-path))
      
      ;; Start session
      (setq session-context (MBS:start-session csv-path "INSERT"))
      (if (not session-context)
        (progn
          (prompt "\n❌ Failed to start session")
          (princ)
        )
        (progn
          ;; Get selection method with batch option
          (initget "Batch Single Text Prefix Filter")
          (setq selection-mode (getkword "\nSelection method: (B)atch, (S)ingle row, (T)ext search, (P)refix filter, or advanced (F)ilter: "))
          
          (if (not selection-mode)
            (progn
              (prompt "\n❌ No selection method specified")
              (princ)
            )
            (progn
              ;; === STAGE 1: RECORD SELECTION & BLOCK INSERTION ===
              (prompt "\n📋 === STAGE 1: MATERIAL DATA INSERTION ===")
              
              ;; Select records
              (setq selected-records (MBS:select-records (nth 2 session-context) (nth 1 session-context) selection-mode))
              
              (if (not selected-records)
                (progn
                  (prompt "\n❌ No records selected")
                  (princ)
                )
                (progn
                  ;; For batch mode, use grid layout automatically
                  ;; For other modes, ask user
                  (if (= selection-mode "Batch")
                    (setq layout-mode "grid")
                    (progn
                      (initget "Grid Individual")
                      (setq layout-mode (getkword "\nLayout mode: (G)rid or (I)ndividual points? "))
                      (if (not layout-mode)
                        (setq layout-mode "grid")  ;; Default to grid
                        (setq layout-mode (strcase layout-mode t))  ;; Convert to lowercase
                      )
                    )
                  )
                  
                  ;; Insert blocks
                  (setq result (MBS:insert-blocks selected-records session-context layout-mode))
                  
                  (if result
                    (progn
                      ;; Finalize Stage 1 transactions
                      (if (MBS:finalize-stage1 session-context)
                        (progn
                          ;; === STAGE 2: ITEM NUMBERING ===
                          (if (MBS:process-item-numbers session-context "preview")
                            (progn
                              ;; === STAGE 3: FINAL SYNCHRONIZATION ===
                              (MBS:sync-blocks-final session-context)
                              (prompt "\n🎉 INSERT operation completed successfully!")
                            )
                            (MBS:log-warning "\n⚠ Stage 2 failed, but Stage 1 completed. Blocks inserted but item numbers may need manual update.")
                          )
                        )
                        (prompt "\n❌ Stage 1 transaction execution failed")
                      )
                    )
                    (prompt "\n❌ Block insertion failed")
                  )
                )
              )
            )
          )
        )
      )
      (princ)
    )
  )
)

(prompt "\nMain commmands loaded!")

;;; ========================================================================
;; SELECTION ENGINE HELPER FUNCTIONS  
;;; ========================================================================

;; Selection helper functions that delegate to existing working functions
(defun MBS:select-single-row (records header num-index desc-index)
  "Select a single row - delegates to existing function"
  ;; This will call the existing single row selection logic
  ;; For now, return a simple implementation
  (prompt "\n🎯 Single row selection mode")
  (prompt "\n⚠ Single row selection - using first record for testing")
  (if (> (length records) 0)
    (list (cons 0 (car records)))
    nil
  )
)

(defun MBS:select-by-text-search (records header num-index desc-index / search-text matches)
  "Text search in descriptions - delegates to existing function"
  (prompt "\nEnter search text to find in descriptions: ")
  (setq search-text (getstring))
  
  (if (not search-text)
    nil
    (progn
      (setq matches '())
      (setq row-index 0)
      
      ;; Search through records
      (foreach record records
        (if (>= (length record) (1+ desc-index))
          (progn
            (setq desc (nth desc-index record))
            (if (and desc (vl-string-search (strcase search-text) (strcase desc)))
              (setq matches (append matches (list (cons row-index record))))
            )
          )
        )
        (setq row-index (1+ row-index))
      )
      
      (if matches
        (progn
          ;; Sort matches by ITEM NO. for more intuitive display (R1, R2, R3...)
          (setq sorted-matches (MBS:sort-matches-by-item-no matches num-index))
          
          (prompt (strcat "\nFound " (itoa (length sorted-matches)) " matching items:"))
          (setq j 1)
          (foreach match sorted-matches
            (setq record (cdr match))
            (setq item-no (if (>= (length record) (1+ num-index)) (nth num-index record) ""))
            (setq desc (if (>= (length record) (1+ desc-index)) (nth desc-index record) ""))
            (prompt (strcat "\n" (itoa j) ": " item-no " - " desc))
            (setq j (1+ j))
          )
          
          (prompt "\nEnter item number(s) to insert (comma-separated, or A for all): ")
          (setq selection (getstring))
          
          (if (= (strcase selection) "A")
            sorted-matches
            ;; Parse comma-separated selection
            (MBS:parse-selection selection sorted-matches)
          )
        )
        (progn
          (prompt "\nNo matching records found")
          nil
        )
      )
    )
  )
)

(defun MBS:select-by-prefix-filter (records header num-index desc-index)
  "Prefix filter selection - delegates to existing function"
  (prompt "\n🏷️ Prefix filter selection mode")
  (prompt "\n⚠ Prefix filter - using all records for testing")
  ;; For now, return all records - will implement proper prefix filtering
  (setq selected-records '())
  (setq row-index 0)
  (foreach record records
    (setq selected-records (append selected-records (list (cons row-index record))))
    (setq row-index (1+ row-index))
  )
  selected-records
)

(defun MBS:select-by-advanced-filter (records header num-index desc-index)
  "Advanced filter selection - delegates to existing function"
  (prompt "\n🔍 Advanced filter selection mode")
  (prompt "\n⚠ Advanced filter - using all records for testing")
  ;; For now, return all records - will implement proper advanced filtering
  (setq selected-records '())
  (setq row-index 0)
  (foreach record records
    (setq selected-records (append selected-records (list (cons row-index record))))
    (setq row-index (1+ row-index))
  )
  selected-records
)

(prompt "\nSelection functions loaded!")

;;; ========================================================================
;; INSERTION ENGINE HELPER FUNCTIONS
;;; ========================================================================

;; Block insertion helper functions - these will use existing working logic
(defun MBS:insert-blocks-grid (selected-records target-space header num-index desc-index mat-id-index)
  "Grid layout block insertion - uses existing MBS:insert-multiple-grid logic"
  
  ;; Clear transactions before starting
  (MBS:clear-transactions)
  
  ;; Call existing grid insertion function but without internal transaction execution
  (setq result (MBS:insert-multiple-grid selected-records target-space header num-index desc-index mat-id-index))
  
  ;; Return result (count will be processed by calling function)
  result
)

(defun MBS:insert-blocks-individual (selected-records target-space header num-index desc-index mat-id-index)
  "Individual point block insertion - uses existing MBS:insert-multiple-single logic"
  
  ;; Clear transactions before starting  
  (MBS:clear-transactions)
  
  ;; Call existing individual insertion function
  (setq result (MBS:insert-multiple-single selected-records target-space header num-index desc-index mat-id-index))
  
  ;; Return result
  result
)

(prompt "\nInsertion engine helper functions loaded!")

;;; ========================================================================
;; UNIFIED UPDATE3 AUDIT ENGINE
;;; ========================================================================

;; Main UPDATE3 command - comprehensive audit system
(defun C:MBS-UPDATE3 (/ csv-path session-context audit-results resolution-results)
  "Unified audit system for orphaned blocks and rows"
  
  (prompt "\n🔍 [UNIFIED UPDATE - Comprehensive Audit System]")
  
  ;; Initialize session using proven INSERT3 architecture
  (setq csv-path (MBS:get-csv-path))
  (if (not csv-path)
    (prompt "\n❌ No CSV file found or specified")
    (progn
      (prompt (strcat "\nUsing CSV: " csv-path))
      
      ;; Start audit session
      (setq session-context (MBS:start-audit-session csv-path))
      
      (if session-context
        (progn
          ;; Run comprehensive audit
          (prompt "\n🔍 === COMPREHENSIVE AUDIT ===")
          (setq audit-results (MBS:run-comprehensive-audit session-context))
          
          ;; Always run resolution (including PHASE 6) regardless of audit results
          (prompt "\n🛠️ === INTERACTIVE RESOLUTION ===")
          (setq resolution-results (MBS:resolve-audit-issues session-context audit-results))
          
          (if (and (not audit-results) (not resolution-results))
            (MBS:log-info "\n✅ No issues found - everything is properly synchronized!")
            (if resolution-results
              (prompt "\n🎉 Audit and resolution completed successfully!")
              (MBS:log-warning "\n⚠ Audit completed with some unresolved issues")
            )
          )
        )
        (prompt "\n❌ Failed to initialize audit session")
      )
    )
  )
  
  (princ)
)

;; Audit Session Manager - extends INSERT3 session pattern
(defun MBS:start-audit-session (csv-path / csv-data header records block-map validation-result detected-mode)
  "Initialize audit session with comprehensive data loading"
  
  (prompt "\n🔄 Initializing audit session...")
  
  ;; Use global mode configuration (don't override with CSV detection)
  (setq current-mode (MBS:get-mode))
  (prompt (strcat "\n[MODE] Using " current-mode " mode (from global config)"))
  
  ;; Clear any existing transactions
  (MBS:clear-transactions)
  
  ;; Read CSV data using proven CSV engine
  (prompt "\n📖 Reading CSV data...")
  (setq csv-data (CSV:read-file csv-path))
  
  (if (not csv-data)
    (progn
      (prompt "\n❌ Failed to read CSV file")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; (prompt (strcat "\n✅ Read " (itoa (length records)) " records with " (itoa (length header)) " columns"))
      
      ;; Build comprehensive block map (pass current mode)
      (prompt "\n🔍 Building comprehensive block map...")
      (setq block-map (MBS:build-comprehensive-block-map current-mode))
      
            ;; Allow empty block maps for audit - this is valid when no blocks exist in drawing
      (if (listp block-map)  ; Check if block-map is a list (empty list is fine, nil is not)
        (progn
          ;; Audit session ready
          ;; (if (> (length block-map) 0)
          ;;   (prompt "\n✅ Audit session initialized successfully")
          ;;   (prompt "\n✅ Audit session initialized successfully (no blocks found - this is normal for empty drawings)")
          ;; )
          ;; Store session context globally for comprehensive sync
          (setq MBS:session-context (list csv-path header records block-map "audit" current-mode))
          ;; Return session context: (csv-path header records block-map operation-type current-mode)        
          MBS:session-context
        )
        (progn
          (prompt "\n❌ Failed to build block map")
          nil
        )
      )
    )
  )
)

;; Get current mode (LOM or LOR) - enhanced detection with persistent storage
(defun MBS:get-mode ()
  "Get current mode (LOM or LOR) using enhanced detection with persistent storage"
  
  ;; Method 1: Check global current-mode variable (highest priority)
  (if (and (boundp 'current-mode) current-mode (/= current-mode ""))
    (progn
      (MBS:log-verbose "\n🔍 DEBUG: Using global current-mode variable")
      current-mode
    )
    (progn
      ;; Method 2: Check _MBS_SETTINGS_MARKER block attribute (persistent)
      (setq settings-block (MBS:find-settings-marker))
      (if settings-block
        (progn
          (setq stored-mode (MBS:get-settings-attribute settings-block "MODE"))
          (if (and stored-mode (/= stored-mode ""))
            (progn
              (prompt "\n🔍 DEBUG: Using mode from _MBS_SETTINGS_MARKER block")
              (setq current-mode stored-mode)  ;; Set global variable for future use
              stored-mode
            )
            (progn
              ;; Method 3: Auto-detect from CSV path (fallback)
              (prompt "\n🔍 DEBUG: Using CSV path auto-detection")
              (if (boundp 'get-csv-path-from-block)
                (progn
                  (setq csv-path (get-csv-path-from-block nil))
                  (if csv-path
                    (progn
                      (setq detected-mode (MBS:auto-detect-mode csv-path))
                      (setq current-mode detected-mode)  ;; Set global variable for future use
                      detected-mode
                    )
                    (progn
                      ;; Default to LOM if no method works
                      (prompt "\n🔍 DEBUG: No CSV path available, defaulting to LOM")
                      (setq current-mode "LOM")
                      "LOM"
                    )
                  )
                )
                (progn
                  ;; Default to LOM if function not available
                  (prompt "\n🔍 DEBUG: get-csv-path-from-block not available, defaulting to LOM")
                  (setq current-mode "LOM")
                  "LOM"
                )
              )
            )
          )
        )
        (progn
          ;; Method 3: Auto-detect from CSV path (fallback)
          (prompt "\n🔍 DEBUG: No settings marker found, using CSV path auto-detection")
          (if (boundp 'get-csv-path-from-block)
            (progn
              (setq csv-path (get-csv-path-from-block nil))
              (if csv-path
                (progn
                  (setq detected-mode (MBS:auto-detect-mode csv-path))
                  (setq current-mode detected-mode)  ;; Set global variable for future use
                  detected-mode
                )
                (progn
                  ;; Default to LOM if no method works
                  (prompt "\n🔍 DEBUG: No CSV path available, defaulting to LOM")
                  (setq current-mode "LOM")
                  "LOM"
                )
              )
            )
            (progn
              ;; Default to LOM if function not available
              (prompt "\n🔍 DEBUG: get-csv-path-from-block not available, defaulting to LOM")
              (setq current-mode "LOM")
              "LOM"
            )
          )
        )
      )
    )
  )
)

;; Auto-detect mode from CSV path
(defun MBS:auto-detect-mode (csv-path)
  "Auto-detect LOR/LOM mode from CSV file path"
  (if (and csv-path (vl-string-search "_LOR" (strcase csv-path)))
    "LOR"
    "LOM"
  )
)

;; Helper function to find settings marker block
(defun MBS:find-settings-marker ( / doc spaceList obj)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq spaceList (list (vla-get-ModelSpace doc)))
  
  ;; Add all Paper Spaces
  (setq layoutDict (vla-get-Layouts doc))
  (if layoutDict
    (progn
      (vlax-for layout layoutDict
        (setq layoutBlock (vl-catch-all-apply '(lambda () (vla-get-Block layout))))
        (if (and (not (vl-catch-all-error-p layoutBlock)) layoutBlock)
          (progn
            (setq layoutName (vl-catch-all-apply '(lambda () (vlax-get layout 'Name))))
            (if (and (not (vl-catch-all-error-p layoutName)) (/= layoutName "Model"))
              (setq spaceList (append spaceList (list layoutBlock)))
            )
          )
        )
      )
    )
  )
  
  ;; Search for _MBS_SETTINGS_MARKER block
  (foreach space spaceList
    (vlax-for obj space
      (if (and (eq (vla-get-ObjectName obj) "AcDbBlockReference")
               (= (vlax-get obj 'EffectiveName) "_MBS_SETTINGS_MARKER")
               (not (vlax-erased-p obj)))
        (setq result obj)
      )
    )
  )
  result
)

;; Helper function to get attribute from settings block
(defun MBS:get-settings-attribute (block-ref attr-name / att-list result)
  (setq result nil)
  (if (and block-ref attr-name)
    (progn
      (setq att-list (vlax-invoke block-ref 'GetAttributes))
      (foreach att att-list
        (if (= (strcase (vlax-get att 'TagString)) (strcase attr-name))
          (setq result (vlax-get att 'TextString))
        )
      )
    )
  )
  result
)

;; Clean AutoCAD formatting codes from description - FIXED
(defun MBS:clean-description (desc / start-pos end-pos)
  "Remove AutoCAD formatting codes like \\W0.8000; from description"
  (if (and desc (/= desc ""))
    (progn
      ;; Remove \W formatting codes (e.g., \W0.8000;) - safer version
      (while (vl-string-search "\\W" desc)
        (setq start-pos (vl-string-search "\\W" desc))
        (setq end-pos (vl-string-search ";" desc start-pos))
        (if (and start-pos end-pos (> end-pos start-pos))
          (setq desc (vl-string-subst "" (substr desc (1+ start-pos) (+ (- end-pos start-pos) 1)) desc))
          (setq desc (vl-string-subst "" "\\W" desc))  ; Fallback: just remove \W
        )
      )
      desc
    )
    desc
  )
)

;; Comprehensive Block Map Builder - enhanced version of INSERT3 logic
(defun MBS:build-comprehensive-block-map (passed-mode / all-blocks block-map block-name entity-name entity-obj material-id description item-no attributes total-count i block-info current-mode id-attribute all-inserts matching-blocks ent ent-obj this-name effective-name)
  "Build comprehensive map of ALL blocks in drawing for audit - WITH DEBUG"
  
  ;; Use passed mode and determine block name directly
  (setq current-mode (if passed-mode passed-mode (MBS:get-mode)))
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
  (setq block-name (if (= current-mode "LOR") "_CACI_REMOVALLEADER" "_CACI_ITEMLEADER"))
  
  ;; (prompt (strcat "\n🔍 DEBUG: Building block map for '" block-name "' blocks"))
  ;; (prompt (strcat "\n🔍 DEBUG: Mode = " current-mode ", ID attribute = " id-attribute))
  
  ;; Find ALL blocks of this type, including dynamic blocks (anonymous references)
  ;; Always search comprehensively for both direct and dynamic blocks
  ;; (prompt (strcat "\n🔍 DEBUG: Searching for ALL '" block-name "' blocks (direct + dynamic)..."))
  (setq all-inserts (ssget "_X" '((0 . "INSERT"))))
  (setq all-blocks nil)
  
  (if all-inserts
    (progn
      (setq matching-blocks (ssadd))
      (setq i 0)
      (while (< i (sslength all-inserts))
        (setq ent (ssname all-inserts i))
        (setq ent-obj (vlax-ename->vla-object ent))
        
        ;; Check both Name and EffectiveName for dynamic blocks
        (setq this-name (vla-get-Name ent-obj))
        (setq effective-name 
          (if (vlax-property-available-p ent-obj 'EffectiveName)
            (vla-get-EffectiveName ent-obj)
            this-name
          )
        )
        
        ;; Debug: show first few blocks being checked
        ;; (if (< i 5)
        ;;   (prompt (strcat "\n🔍 DEBUG: Block " (itoa i) " - Name: '" this-name "', EffectiveName: '" effective-name "'"))
        ;; )
        
        ;; If either name matches our target, add to selection
        (if (or (= this-name block-name) (= effective-name block-name))
          (progn
            ;; (prompt (strcat "\n🔍 DEBUG: MATCH found! Adding block: " this-name " (effective: " effective-name ")"))
            (ssadd ent matching-blocks)
          )
        )
        
        (setq i (1+ i))
      )
      
      ;; Use the matching blocks if any found
      (if (> (sslength matching-blocks) 0)
        (setq all-blocks matching-blocks)
      )
    )
  )
  
  ;; (prompt (strcat "\n🔍 DEBUG: Final ssget result = " (if all-blocks (strcat "FOUND " (itoa (sslength all-blocks)) " blocks") "NIL")))
  
  (setq block-map '())
  (setq total-count 0)
  
  (if all-blocks
    (progn
      (setq total-count (sslength all-blocks))
      (prompt (strcat "\n[MAP] Found " (itoa total-count) " total blocks"))
      
      ;; Process each block
      (setq i 0)
      (while (< i total-count)
        (setq entity-name (ssname all-blocks i))
        (setq entity-obj (vlax-ename->vla-object entity-name))
        
        ;; Extract all attributes for comprehensive analysis
        (setq attributes (MBS:extract-all-block-attributes entity-obj))
        
        ;; Extract key attributes for audit
        (setq material-id (cdr (assoc (strcase id-attribute) attributes)))
        (setq description (MBS:clean-description (cdr (assoc "DESCRIPTION" attributes))))  ; Clean formatting
        (setq item-no (cdr (assoc "##" attributes)))  ; Fixed: ## not "ITEM NO."
        
        ;; Add to map with ALL blocks, not just ones with IDs
        (setq block-info (list entity-name entity-obj material-id description item-no attributes))
        (setq block-map (append block-map (list block-info)))
        
        (setq i (1+ i))
      )
      
      ;; (prompt (strcat "\n✅ Mapped " (itoa (length block-map)) " blocks for audit"))
      block-map
    )
    (progn
      (MBS:log-warning "\n⚠ No blocks found for audit")
      '()  ;; Return empty list, not nil
    )
  )
)

;; Extract all block attributes for comprehensive analysis - DYNAMIC BLOCK COMPATIBLE
(defun MBS:extract-all-block-attributes (block-obj / attributes att-list att-ref tag value)
  "Extract all attributes from block for comprehensive audit - handles dynamic blocks"
  
  (setq attributes '())
  
  (if (and block-obj (vlax-property-available-p block-obj 'HasAttributes))
    (if (= (vla-get-HasAttributes block-obj) :vlax-true)
      (progn
        ;; Get attributes using safe error handling (dynamic block compatible)
        (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke block-obj 'GetAttributes))))
        
        ;; Check if we got attributes successfully
        (if (not (vl-catch-all-error-p att-list))
          (progn
            ;; Process attributes using foreach (works with dynamic blocks)
            (foreach att-ref att-list
              (if att-ref
                (progn
                  (setq tag (vlax-get att-ref 'TagString))
                  (setq value (vlax-get att-ref 'TextString))
                  (setq attributes (append attributes (list (cons (strcase tag) value))))
                )
              )
            )
          )
        )
      )
    )
  )
  
  attributes
)

;; Comprehensive Audit Engine - detects all orphan types
(defun MBS:run-comprehensive-audit (session-context / csv-path header records block-map current-mode audit-results orphaned-rows orphaned-blocks semi-orphaned-identical semi-orphaned-fuzzy missing-attributes)
  "Run comprehensive audit to detect all types of orphans and issues"
  
  (setq csv-path (nth 0 session-context))
  (setq header (nth 1 session-context))
  (setq records (nth 2 session-context))
  (setq block-map (nth 3 session-context))
  (setq current-mode (nth 5 session-context))  ; Extract mode from session
  
  (setq audit-results '())
  
  ;; 1. Detect Orphaned Rows (CSV rows without matching blocks)
  (prompt "\n🔍 Scanning for orphaned CSV rows...")
  (setq orphaned-rows (MBS:detect-orphaned-rows records header block-map current-mode))
  (if orphaned-rows
    (setq audit-results (append audit-results (list (cons "orphaned-rows" orphaned-rows))))
  )
  
  ;; 2. Detect Orphaned Blocks (blocks without matching CSV rows)
  (prompt "\n🔍 Scanning for orphaned blocks...")
  (setq orphaned-blocks (MBS:detect-orphaned-blocks records header block-map current-mode))
  (if orphaned-blocks
    (setq audit-results (append audit-results (list (cons "orphaned-blocks" orphaned-blocks))))
  )
  
  ;; 3. Detect Semi-Orphaned Blocks (identical description matches)
  (prompt "\n🔍 Scanning for semi-orphaned blocks (identical matches)...")
  (setq semi-orphaned-identical (MBS:detect-semi-orphaned-identical records header block-map current-mode))
  (if semi-orphaned-identical
    (setq audit-results (append audit-results (list (cons "semi-orphaned-identical" semi-orphaned-identical))))
  )
  
  ;; 4. Detect Semi-Orphaned Blocks (fuzzy description matches)
  (prompt "\n🔍 Scanning for semi-orphaned blocks (fuzzy matches)...")
  (setq semi-orphaned-fuzzy (MBS:detect-semi-orphaned-fuzzy records header block-map current-mode))
  (if semi-orphaned-fuzzy
    (setq audit-results (append audit-results (list (cons "semi-orphaned-fuzzy" semi-orphaned-fuzzy))))
  )
  
  ;; 5. Detect blocks with missing critical attributes
  (prompt "\n🔍 Scanning for blocks with missing attributes...")
  (setq missing-attributes (MBS:detect-missing-attributes block-map current-mode))
  (if missing-attributes
    (setq audit-results (append audit-results (list (cons "missing-attributes" missing-attributes))))
  )
  
  ;; Report summary
  (MBS:report-audit-summary audit-results)
  
  audit-results
)

;; Detect orphaned CSV rows (no matching blocks)
(defun MBS:detect-orphaned-rows (records header block-map current-mode / orphaned-rows mat-id-index desc-index row-index record material-id description matching-block id-attribute)
  "Find CSV rows that have no corresponding blocks"
  
  (setq orphaned-rows '())
  ;; Set mode-specific ID attribute
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
  (setq mat-id-index (CSV:get-column-index header (strcase id-attribute)))
  (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
  
  (if (or (not mat-id-index) (not desc-index))
    (progn
      (MBS:log-warning "\n⚠ Cannot check orphaned rows - missing required columns")
      nil
    )
    (progn
      (setq row-index 0)
      (foreach record records
        (setq material-id (if (>= (length record) (1+ mat-id-index)) (nth mat-id-index record) ""))
        (setq description (if (>= (length record) (1+ desc-index)) (nth desc-index record) ""))
        
        ;; Check if this row has a matching block
        (setq matching-block nil)
        (if (and material-id (/= material-id ""))
          ;; Has ID - look for exact ID match
          (setq matching-block (MBS:find-block-by-id block-map material-id))
          ;; No ID - look for description match
          (if (and description (/= description ""))
            (setq matching-block (MBS:find-block-by-description block-map description))
          )
        )
        
        ;; If no matching block found, it's orphaned
        (if (not matching-block)
          (setq orphaned-rows (append orphaned-rows (list (cons row-index record))))
        )
        
        (setq row-index (1+ row-index))
      )
      
      (if orphaned-rows
        (prompt (strcat "\n📋 Found " (itoa (length orphaned-rows)) " orphaned CSV rows"))
        (prompt "\n✅ No orphaned CSV rows found")
      )
      
      orphaned-rows
    )
  )
)

;; Detect orphaned blocks (no matching CSV rows)
(defun MBS:detect-orphaned-blocks (records header block-map current-mode / orphaned-blocks mat-id-index desc-index block-info entity-name material-id description matching-record id-attribute)
  "Find blocks that have no corresponding CSV rows"
  
  (setq orphaned-blocks '())
  ;; Set mode-specific ID attribute
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
  (setq mat-id-index (CSV:get-column-index header (strcase id-attribute)))
  (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
  
  (if (or (not mat-id-index) (not desc-index))
    (progn
      (MBS:log-warning "\n⚠ Cannot check orphaned blocks - missing required columns")
      nil
    )
    (progn
      (foreach block-info block-map
        (setq entity-name (nth 0 block-info))
        (setq material-id (nth 2 block-info))
        (setq description (nth 3 block-info))
        
        ;; Check if this block has a matching CSV row
        (setq matching-record nil)
        (if (and material-id (/= material-id ""))
          ;; Has ID - look for exact ID match in CSV
          (setq matching-record (MBS:find-csv-record-by-id records header material-id current-mode))
          ;; No ID - look for description match in CSV
          (if (and description (/= description ""))
            (setq matching-record (MBS:find-csv-record-by-description records desc-index description))
          )
        )
        
        ;; If no matching record found, it's orphaned
        (if (not matching-record)
          (setq orphaned-blocks (append orphaned-blocks (list block-info)))
        )
      )
      
      (if orphaned-blocks
        (prompt (strcat "\n🧱 Found " (itoa (length orphaned-blocks)) " orphaned blocks"))
        (prompt "\n✅ No orphaned blocks found")
      )
      
      orphaned-blocks
    )
  )
)

;; Helper Functions for Audit Engine
(defun MBS:find-block-by-id (block-map material-id / block-info)
  "Find block in block-map by material ID"
  
  (setq result nil)
  (foreach block-info block-map
    (if (and (not result) (= (nth 2 block-info) material-id))
      (setq result block-info)
    )
  )
  result
)

(defun MBS:find-block-by-description (block-map description / block-info)
  "Find block in block-map by exact description match"
  
  (setq result nil)
  (foreach block-info block-map
    (if (and (not result) (= (strcase (nth 3 block-info)) (strcase description)))
      (setq result block-info)
    )
  )
  result
)

;; OLD FUNCTION - DISABLED to avoid conflicts with new version at line 2023
;; OLD FUNCTION REMOVED - conflicts with new version at line 2023

(defun MBS:find-csv-record-by-description (records desc-index description / row-index record record-desc)
  "Find CSV record by exact description match"
  
  (setq result nil)
  (setq row-index 0)
  (foreach record records
    (if (not result)
      (progn
        (setq record-desc (if (>= (length record) (1+ desc-index)) (nth desc-index record) ""))
        (if (= (strcase record-desc) (strcase description))
          (setq result (cons row-index record))
        )
      )
    )
    (setq row-index (1+ row-index))
  )
  result
)

;; Detect semi-orphaned blocks with identical description matches
(defun MBS:detect-semi-orphaned-identical (records header block-map current-mode / semi-orphaned mat-id-index desc-index block-info entity-name material-id description matching-record id-attribute)
  "Find blocks with empty/invalid IDs but identical description matches in CSV"
  
  (setq semi-orphaned '())
  ;; Set mode-specific ID attribute
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
  (setq mat-id-index (CSV:get-column-index header (strcase id-attribute)))
  (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
  
  (if (or (not mat-id-index) (not desc-index))
    (progn
      (MBS:log-warning "\n⚠ Cannot check semi-orphaned blocks - missing required columns")
      nil
    )
    (progn
      (foreach block-info block-map
        (setq entity-name (nth 0 block-info))
        (setq material-id (nth 2 block-info))
        (setq description (nth 3 block-info))
        
        ;; Check if block has empty/invalid ID but valid description
        (if (and (or (not material-id) (= material-id ""))
                 description (/= description ""))
          (progn
            ;; Look for exact description match in CSV
            (setq matching-record (MBS:find-csv-record-by-description records desc-index description))
            (if matching-record
              ;; Found identical match - this is semi-orphaned
              (setq semi-orphaned (append semi-orphaned (list (list block-info matching-record "identical"))))
            )
          )
        )
      )
      
      (if semi-orphaned
        (prompt (strcat "\n🔗 Found " (itoa (length semi-orphaned)) " semi-orphaned blocks (identical matches)"))
        ;; (prompt "\n✅ No semi-orphaned blocks with identical matches found")
      )
      
      semi-orphaned
    )
  )
)

;; Detect semi-orphaned blocks with fuzzy description matches
(defun MBS:detect-semi-orphaned-fuzzy (records header block-map current-mode / semi-orphaned mat-id-index desc-index block-info entity-name material-id description fuzzy-matches id-attribute error-result)
  "Find blocks with empty/invalid IDs but fuzzy description matches in CSV"
  
  (setq semi-orphaned '())
  ;; Set mode-specific ID attribute
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
  (setq mat-id-index (CSV:get-column-index header (strcase id-attribute)))
  (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
  
  (if (or (not mat-id-index) (not desc-index))
    (progn
      (MBS:log-warning "\n⚠ Cannot check fuzzy semi-orphaned blocks - missing required columns")
      nil
    )
    (progn
      (foreach block-info block-map
        (setq entity-name (nth 0 block-info))
        (setq material-id (nth 2 block-info))
        (setq description (nth 3 block-info))
        
        ;; Minimal debugging - only for orphaned blocks with empty IDs
        (if (and (or (not material-id) (= material-id "")) description (/= description ""))
          (prompt (strcat "\n🔍 Processing orphaned block: " description))
        )
        
        ;; Check if block has empty/invalid ID but valid description
        (if (and (or (not material-id) (= material-id ""))
                 description 
                 (if (listp description)
                   ;; If it's a list, convert to string
                   (progn
                     (setq description (if (= (length description) 1) (car description) (princ-to-string description)))
                     (/= description "")
                   )
                   ;; If it's already a string, check if not empty
                   (/= description "")
                 ))
          (progn
            ;; Look for fuzzy description matches in CSV with error handling
            (setq error-result (vl-catch-all-apply 'MBS:find-fuzzy-description-matches (list records desc-index description)))
            (if (vl-catch-all-error-p error-result)
              (progn
                (prompt (strcat "\n⚠ ERROR in fuzzy matching for description: " (if (listp description) (princ-to-string description) description)))
                (prompt (strcat "\n⚠ Error details: " (vl-catch-all-error-message error-result)))
              )
              (progn
                (setq fuzzy-matches error-result)
                (if fuzzy-matches
                  ;; Found fuzzy matches - this is semi-orphaned
                  (setq semi-orphaned (append semi-orphaned (list (list block-info fuzzy-matches "fuzzy"))))
                )
              )
            )
          )
        )
      )
      
      (if semi-orphaned
        (prompt (strcat "\n🔗 Found " (itoa (length semi-orphaned)) " semi-orphaned blocks (fuzzy matches)"))
        ;; (prompt "\n✅ No semi-orphaned blocks with fuzzy matches found")
      )
      
      semi-orphaned
    )
  )
)

;; Find fuzzy description matches using smart string comparison
(defun MBS:find-fuzzy-description-matches (records desc-index target-description / matches row-index record record-desc similarity enhanced-similarity final-similarity error-result)
  "Find CSV records with fuzzy description matches"
  
  (setq matches '())
  (setq row-index 0)
  
  (foreach record records
    (setq record-desc (if (>= (length record) (1+ desc-index)) (nth desc-index record) ""))
    
    (if (and record-desc (/= record-desc ""))
      (progn
        ;; Calculate similarity score with error handling
        (setq error-result (vl-catch-all-apply 'MBS:calculate-string-similarity (list target-description record-desc)))
        (if (vl-catch-all-error-p error-result)
          (progn
            (prompt (strcat "\n⚠ ERROR in string similarity: " (vl-catch-all-error-message error-result)))
            (setq similarity 0.0)
          )
          (setq similarity error-result)
        )
        
        ;; Enhanced fuzzy matching with error handling
        (setq error-result (vl-catch-all-apply 'MBS:calculate-enhanced-similarity (list target-description record-desc)))
        (if (vl-catch-all-error-p error-result)
          (progn
            (prompt (strcat "\n⚠ ERROR in enhanced similarity: " (vl-catch-all-error-message error-result)))
            (setq enhanced-similarity 0.0)
          )
          (setq enhanced-similarity error-result)
        )
        
        (setq final-similarity (max similarity enhanced-similarity))
        
        ;; Show debug info only for matches above threshold
        (if (> final-similarity 0.6)
          (prompt (strcat "\n🔍 FUZZY MATCH: '" target-description "' vs '" record-desc "' = " (rtos final-similarity 2 3) " (base=" (rtos similarity 2 3) " enhanced=" (rtos enhanced-similarity 2 3) ")"))
        )
        
        ;; Consider it a fuzzy match if similarity > 60% (lowered threshold)
        (if (> final-similarity 0.6)
          (setq matches (append matches (list (list (cons row-index record) final-similarity))))
        )
      )
    )
    
    (setq row-index (1+ row-index))
  )
  
  ;; Sort matches by similarity (best matches first)
  (if matches
    (setq matches (MBS:sort-fuzzy-matches matches))
  )
  
  matches
)

;; Enhanced similarity calculation for numbered items
(defun MBS:calculate-enhanced-similarity (str1 str2 / base1 base2 num1 num2 error-result)
  "Calculate enhanced similarity for numbered items like 'NEW ITEM 3' vs 'NEW ITEM 1'"
  
  ;; Extract base text (everything before the last number) with error handling
  (setq error-result (vl-catch-all-apply 'MBS:extract-base-text (list str1)))
  (if (vl-catch-all-error-p error-result)
    (progn
      (prompt (strcat "\n⚠ ERROR extracting base text from '" str1 "': " (vl-catch-all-error-message error-result)))
      (setq base1 str1)
    )
    (setq base1 error-result)
  )
  
  (setq error-result (vl-catch-all-apply 'MBS:extract-base-text (list str2)))
  (if (vl-catch-all-error-p error-result)
    (progn
      (prompt (strcat "\n⚠ ERROR extracting base text from '" str2 "': " (vl-catch-all-error-message error-result)))
      (setq base2 str2)
    )
    (setq base2 error-result)
  )
  
  ;; If base texts are identical, give high similarity even if numbers differ
  (if (and base1 base2 (= (strcase base1) (strcase base2)))
    0.85  ; High similarity for same base text with different numbers
    0.0   ; No enhanced similarity if bases don't match
  )
)

;; Helper function to extract base text from numbered items - SIMPLIFIED VERSION
(defun MBS:extract-base-text (str / words last-word base-words)
  "Extract base text by removing trailing numbers: 'NEW ITEM 3' -> 'NEW ITEM'"
  
  ;; Disable enhanced similarity for now to avoid errors
  ;; Just return the original string
  str
)

;; Helper function to check if a string is numeric
(defun MBS:is-numeric (str / result)
  "Check if string contains only digits"
  
  (if (and str (/= str ""))
    (progn
      (setq result T)
      (setq i 0)
      (while (and result (< i (strlen str)))
        (setq char (substr str (1+ i) 1))
        (if (not (and (>= (ascii char) 48) (<= (ascii char) 57)))  ; ASCII 48-57 are digits 0-9
          (setq result nil)
        )
        (setq i (1+ i))
      )
      result
    )
    nil
  )
)

;; Helper function to join words with delimiter
(defun MBS:join-words (word-list delimiter / result)
  "Join list of words with delimiter"
  
  (setq result "")
  (foreach word word-list
    (if (= result "")
      (setq result word)
      (setq result (strcat result delimiter word))
    )
  )
  result
)

;; Calculate string similarity using improved algorithm   
(defun MBS:calculate-string-similarity (str1 str2 / str1-upper str2-upper words1 words2 word-matches total-words word all-words)
  "Calculate similarity between two strings (0.0 to 1.0) - IMPROVED VERSION"
  
  (if (or (not str1) (not str2) (= str1 "") (= str2 ""))
    0.0
    (progn
      (setq str1-upper (strcase str1))
      (setq str2-upper (strcase str2))

      ;; Simple exact match
      (if (= str1-upper str2-upper)
        1.0
        (progn
          ;; Word-based similarity - IMPROVED to use union of all words
          (setq words1 (MBS:split-into-words str1-upper))
          (setq words2 (MBS:split-into-words str2-upper))

          ;; Create union of all unique words from both strings
          (setq all-words words1)
          (foreach word words2
            (if (not (member word all-words))
              (setq all-words (append all-words (list word)))
            )
          )
          
          (setq word-matches 0)
          (setq total-words (length all-words))

          ;; Count words that appear in BOTH strings
          (foreach word all-words
            (if (and (member word words1) (member word words2))
              (setq word-matches (1+ word-matches))
            )
          )

          ;; Return similarity ratio based on shared words vs total unique words
          (if (> total-words 0)
            (/ (float word-matches) (float total-words))
            0.0
          )
        )
      )
    )
  )
)

;; Split string into words for fuzzy matching
(defun MBS:split-into-words (str / words current-word i char)
  "Split string into words, removing punctuation"
  
  (setq words '())
  (setq current-word "")
  (setq i 1)
  
  (while (<= i (strlen str))
    (setq char (substr str i 1))
    
    ;; Check if character is alphanumeric
    (if (or (and (>= (ascii char) 65) (<= (ascii char) 90))    ;; A-Z
            (and (>= (ascii char) 48) (<= (ascii char) 57)))   ;; 0-9
      ;; Add to current word
      (setq current-word (strcat current-word char))
      ;; End current word
      (if (/= current-word "")
        (progn
          (setq words (append words (list current-word)))
          (setq current-word "")
        )
      )
    )
    
    (setq i (1+ i))
  )
  
  ;; Add final word
  (if (/= current-word "")
    (setq words (append words (list current-word)))
  )
  
  words
)

;; Sort fuzzy matches by similarity score
(defun MBS:sort-fuzzy-matches (matches / sorted i j temp)
  "Sort fuzzy matches by similarity score (highest first)"
  
  (setq sorted matches)
  (setq i 0)
  
  ;; Simple bubble sort by similarity score
  (while (< i (1- (length sorted)))
    (setq j 0)
    (while (< j (- (length sorted) i 1))
      (if (< (cadr (nth j sorted)) (cadr (nth (1+ j) sorted)))
        (progn
          ;; Swap
          (setq temp (nth j sorted))
          (setq sorted (MBS:replace-nth sorted j (nth (1+ j) sorted)))
          (setq sorted (MBS:replace-nth sorted (1+ j) temp))
        )
      )
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  
  sorted
)

;; Detect blocks with missing critical attributes - WITH DETAILED DEBUG
(defun MBS:detect-missing-attributes (block-map passed-mode / missing-attrs block-info entity-name material-id description item-no issues block-num current-mode id-attribute)
  "Find blocks with missing critical attributes"
  
  (setq missing-attrs '())
  (setq block-num 0)
  (setq current-mode (if passed-mode passed-mode (MBS:get-mode)))
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
  
  ;; (prompt (strcat "\n🔍 DEBUG: Checking " (itoa (length block-map)) " blocks for missing attributes"))
  ;; (prompt (strcat "\n🔍 DEBUG: Current mode = " current-mode ", ID attribute = " id-attribute))
  
  (foreach block-info block-map
    (setq block-num (1+ block-num))
    (setq entity-name (nth 0 block-info))
    (setq material-id (nth 2 block-info))
    (setq description (nth 3 block-info))
    (setq item-no (nth 4 block-info))
    
    ;; Check for missing critical attributes
    
    (setq issues '())
    
    ;; Check for missing critical attributes (ITEM NO. handled in Phase 6)
    (if (or (not material-id) (= material-id ""))
      (setq issues (append issues (list "missing-id")))
    )
    (if (or (not description) (= description ""))
      (setq issues (append issues (list "missing-description")))
    )
    ;; NOTE: Missing ITEM NO. removed - automatically handled in Phase 6 renumbering
    
    ;; If any issues found, add to missing attributes list
    (if issues
      (setq missing-attrs (append missing-attrs (list (list block-info issues))))
    )
  )
  
  (if missing-attrs
    (MBS:log-warning (strcat "\n⚠ Found " (itoa (length missing-attrs)) " blocks with missing attributes"))
    (prompt "\n✅ No blocks with missing attributes found")
  )
  
  missing-attrs
)

;; Report audit summary
(defun MBS:report-audit-summary (audit-results / result-type result-data count)
  "Generate comprehensive audit summary report"
  
  (prompt "\n" )
  (prompt "\n📊 === AUDIT SUMMARY ===")
  
  (if (not audit-results)
    (prompt "\n🎉 Perfect! No issues found - everything is properly synchronized!")
    (progn
      (MBS:log-warning "\n⚠ Issues detected:")
      
      (foreach result audit-results
        (setq result-type (car result))
        (setq result-data (cdr result))
        (setq count (length result-data))
        
        (cond
          ((= result-type "orphaned-rows")
           (prompt (strcat "\n  📋 " (itoa count) " orphaned CSV rows (no matching blocks)"))
          )
          ((= result-type "orphaned-blocks") 
           (prompt (strcat "\n  🧱 " (itoa count) " orphaned blocks (no matching CSV rows)"))
          )
          ((= result-type "semi-orphaned-identical")
           (prompt (strcat "\n  🔗 " (itoa count) " semi-orphaned blocks (identical description matches)"))
          )
          ((= result-type "semi-orphaned-fuzzy")
           (prompt (strcat "\n  🔗 " (itoa count) " semi-orphaned blocks (fuzzy description matches)"))
          )
          ((= result-type "missing-attributes")
           (MBS:log-warning (strcat "\n  ⚠ " (itoa count) " blocks with missing critical attributes"))
          )
        )
      )
      
      (prompt "\n")
    )
  )
)

;; Interactive Resolution Engine - PROGRESSIVE WORKFLOW
(defun MBS:resolve-audit-issues (session-context audit-results / total-resolved changes-made)
  "Progressive resolution system - re-scan after each issue type"
  
  (setq total-resolved 0)
  (setq changes-made t)  ; Start with true to begin the cycle
  
  (prompt "\n🛠️ === PROGRESSIVE RESOLUTION SYSTEM ===")
  ;; (prompt "\n🔄 Will re-scan after each resolution type to ensure accuracy")
  
  ;; Continue processing until no more changes are made
  (while changes-made
    (setq changes-made nil)
    
    ;; Phase 1: Semi-Orphaned Blocks (Identical Matches) - HIGHEST PRIORITY
    (prompt "\n\n🔗 === PHASE 1: SEMI-ORPHANED BLOCKS (IDENTICAL MATCHES) ===")
    (setq phase-resolved (MBS:resolve-phase-semi-orphaned-identical session-context))
    (if (> phase-resolved 0)
      (progn
        (setq total-resolved (+ total-resolved phase-resolved))
        (setq changes-made t)
        (prompt (strcat "\n🔄 Resolved " (itoa phase-resolved) " identical matches - re-scanning..."))
      )
      (prompt "\n✅ No identical semi-orphaned blocks found")
    )
    
    ;; Phase 2: Semi-Orphaned Blocks (Fuzzy Matches) - if changes made, re-scan
    (if (not changes-made)
      (progn
        (prompt "\n\n🔗 === PHASE 2: SEMI-ORPHANED BLOCKS (FUZZY MATCHES) ===")
        (setq phase-resolved (MBS:resolve-phase-semi-orphaned-fuzzy session-context))
        (if (> phase-resolved 0)
          (progn
            (setq total-resolved (+ total-resolved phase-resolved))
            (setq changes-made t)
            (prompt (strcat "\n🔄 Resolved " (itoa phase-resolved) " fuzzy matches - re-scanning..."))
          )
          (prompt "\n✅ No fuzzy semi-orphaned blocks found")
        )
      )
    )
  )
  
  ;; Phase 3: Orphaned Blocks (after all semi-orphaned are resolved)
  (prompt "\n\n🧱 === PHASE 3: ORPHANED BLOCKS ===")
  (setq phase-resolved (MBS:resolve-phase-orphaned-blocks session-context))
  (setq total-resolved (+ total-resolved phase-resolved))
  
  ;; Phase 4: Orphaned CSV Rows  
  (prompt "\n\n📋 === PHASE 4: ORPHANED CSV ROWS ===")
  (setq phase-resolved (MBS:resolve-phase-orphaned-rows session-context))
  (setq total-resolved (+ total-resolved phase-resolved))
  
  ;; Phase 5: Skipped - Missing attributes handled by Phase 6 (for valid IDs) or orphaned logic (for invalid IDs)
  ;; (prompt "\n\n⏭ === PHASE 5: SKIPPED (REDUNDANT) ===")
  ;; (prompt "✅ Missing attributes will be handled by Phase 6 comprehensive synchronization")
  (setq phase-resolved 0)
  (setq total-resolved (+ total-resolved phase-resolved))
  
  ;; Execute any pending transactions from resolution actions
  (if (> (length MBS:transaction-queue) 0)
    (progn
      (prompt (strcat "\n\n💾 === EXECUTING " (itoa (length MBS:transaction-queue)) " PENDING TRANSACTIONS ==="))
      (setq csv-path (nth 0 session-context))
      (if (CSV:execute-transactions csv-path MBS:transaction-queue (nth 5 session-context))
        (progn
          (MBS:log-info "\n✅ All transactions executed successfully")
          (MBS:clear-transactions)
        )
        (prompt "\n❌ Some transactions failed")
      )
    )
    (prompt "\n📝 No pending transactions to execute")
  )
  
  ;; Final Summary
  (prompt "\n\n📊 === FINAL RESOLUTION SUMMARY ===")
  (prompt (strcat "\n🎉 Total issues resolved: " (itoa total-resolved)))
  
  ;; Phase 6: Clean Implementation
  (prompt "\n\n🎯 === PHASE 6: ITEM NO & DESCRIPTION SYNCHRONIZATION ===")
  (setq phase-resolved (MBS:phase6-clean session-context))
  (setq total-resolved (+ total-resolved phase-resolved))
  
  (> total-resolved 0)
)

(prompt "\nUpdate3 main functions loaded!")

;; ========================================================================
;; PROGRESSIVE PHASE FUNCTIONS - Real-time detection and resolution
;; ========================================================================

;; Phase function: Semi-Orphaned Identical - detect and resolve in real-time
(defun MBS:resolve-phase-semi-orphaned-identical (session-context / csv-path header records current-mode block-map semi-orphaned-blocks resolved-count)
  "Phase 1: Detect and resolve semi-orphaned blocks with identical matches"
  
  ;; Extract session data
  (setq csv-path (nth 0 session-context))
  (setq header (nth 1 session-context))
  (setq records (nth 2 session-context))
  (setq current-mode (nth 5 session-context))
  
  ;; Re-scan blocks in real-time
  (setq block-map (MBS:build-comprehensive-block-map current-mode))
  
  ;; Detect semi-orphaned identical matches
  (setq semi-orphaned-blocks (MBS:detect-semi-orphaned-identical records header block-map current-mode))
  
  ;; Resolve if any found
  (if semi-orphaned-blocks
    (setq resolved-count (MBS:resolve-semi-orphaned-identical semi-orphaned-blocks session-context))
    (setq resolved-count 0)
  )
  
  resolved-count
)

;; Phase function: Semi-Orphaned Fuzzy - detect and resolve in real-time
(defun MBS:resolve-phase-semi-orphaned-fuzzy (session-context / csv-path header records current-mode block-map semi-orphaned-blocks resolved-count)
  "Phase 2: Detect and resolve semi-orphaned blocks with fuzzy matches"
  
  ;; Extract session data
  (setq csv-path (nth 0 session-context))
  (setq header (nth 1 session-context))
  (setq records (nth 2 session-context))
  (setq current-mode (nth 5 session-context))
  
  ;; Re-scan blocks in real-time
  (setq block-map (MBS:build-comprehensive-block-map current-mode))
  
  ;; Detect semi-orphaned fuzzy matches
  (setq semi-orphaned-blocks (MBS:detect-semi-orphaned-fuzzy records header block-map current-mode))
  
  ;; Resolve if any found
  (if semi-orphaned-blocks
    (setq resolved-count (MBS:resolve-semi-orphaned-fuzzy semi-orphaned-blocks session-context))
    (setq resolved-count 0)
  )
  
  resolved-count
)

;; Phase function: Orphaned Blocks - detect and resolve in real-time
(defun MBS:resolve-phase-orphaned-blocks (session-context / csv-path header records current-mode block-map orphaned-blocks resolved-count)
  "Phase 3: Detect and resolve orphaned blocks"
  
  ;; Extract session data
  (setq csv-path (nth 0 session-context))
  (setq header (nth 1 session-context))
  (setq records (nth 2 session-context))
  (setq current-mode (nth 5 session-context))
  
  ;; Re-scan blocks in real-time
  (setq block-map (MBS:build-comprehensive-block-map current-mode))
  
  ;; Detect orphaned blocks
  (setq orphaned-blocks (MBS:detect-orphaned-blocks records header block-map current-mode))
  
  ;; Resolve if any found
  (if orphaned-blocks
    (setq resolved-count (MBS:resolve-orphaned-blocks orphaned-blocks session-context))
    (setq resolved-count 0)
  )
  
  resolved-count
)

;; Phase function: Orphaned CSV Rows - detect and resolve in real-time
(defun MBS:resolve-phase-orphaned-rows (session-context / csv-path header records current-mode block-map orphaned-rows resolved-count)
  "Phase 4: Detect and resolve orphaned CSV rows"
  
  ;; Extract session data
  (setq csv-path (nth 0 session-context))
  (setq header (nth 1 session-context))
  (setq records (nth 2 session-context))
  (setq current-mode (nth 5 session-context))
  
  ;; Re-scan blocks in real-time
  (setq block-map (MBS:build-comprehensive-block-map current-mode))
  
  ;; Detect orphaned CSV rows
  (setq orphaned-rows (MBS:detect-orphaned-rows records header block-map current-mode))
  
  ;; Resolve if any found
  (if orphaned-rows
    (setq resolved-count (MBS:resolve-orphaned-rows orphaned-rows session-context))
    (setq resolved-count 0)
  )
  
  resolved-count
)

;; Phase function: Missing Attributes - detect and resolve in real-time
(defun MBS:resolve-phase-missing-attributes (session-context / csv-path header records current-mode block-map missing-attrs resolved-count)
  "Phase 5: Detect and resolve missing attributes"
  
  ;; Extract session data
  (setq csv-path (nth 0 session-context))
  (setq header (nth 1 session-context))
  (setq records (nth 2 session-context))
  (setq current-mode (nth 5 session-context))
  
  ;; Re-scan blocks in real-time
  (setq block-map (MBS:build-comprehensive-block-map current-mode))
  
  ;; Detect missing attributes
  (setq missing-attrs (MBS:detect-missing-attributes block-map current-mode))
  
  ;; Resolve if any found
  (if missing-attrs
    (setq resolved-count (MBS:resolve-missing-attributes missing-attrs session-context))
    (setq resolved-count 0)
  )
  
  resolved-count
)

;; Phase function: ITEM NO. Renumbering - final stage
(defun MBS:renumber-all-item-numbers (session-context / csv-path current-mode csv-data header records num-index desc-index prefix-groups changes-list total-changes user-choice)
  "Phase 6: Renumber all ITEM NOs with contiguous prefix-based sequences"
  
  ;; Extract session data and RE-READ CSV to get updated records after transactions
  (setq csv-path (nth 0 session-context))
  (setq current-mode (nth 5 session-context))
  
  ;; Re-read CSV to get the current state after all transactions
  (prompt "\n📖 Re-reading CSV to get current state after transactions...")
  (setq csv-data (CSV:read-file csv-path))
  (setq header (car csv-data))
  (setq records (cadr csv-data))
  (MBS:log-info (strcat "\n✅ Loaded " (itoa (length records)) " current records for renumbering"))
  
  ;; Get column indices
  (setq num-index (CSV:get-column-index header "ITEM NO."))
  (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
  
  (if (or (not num-index) (not desc-index))
    (progn
      (prompt "\n❌ Cannot find required columns for renumbering")
      0
    )
    (progn
      ;; Check prefix mode setting
      (MBS:log-info (strcat "\n🔧 Prefix mode: " (if (boundp 'prefix-mode) prefix-mode "AUTO")))
      
      (if (and (boundp 'prefix-mode) (= prefix-mode "MANUAL"))
        ;; MANUAL MODE: Use CSV ITEM NO as-is, no renumbering
        (progn
          (prompt "\n📋 Manual mode: Using ITEM NO values directly from CSV")
          (MBS:log-info "\n✅ No renumbering needed in manual mode")
          0
        )
        ;; AUTO MODE: Extract prefixes and renumber sequences
        (MBS:auto-renumber-sequences records header num-index desc-index current-mode)
      )
    )
  )
)

;; Helper function for auto-renumbering logic
(defun MBS:auto-renumber-sequences (records header num-index desc-index current-mode / prefix-groups changes-list total-changes user-choice)
  "Handle the auto-renumbering logic in a separate function for cleaner structure"
  
  (prompt "\n🔢 Auto mode: Analyzing ITEM NO. sequences by prefix...")
  
  ;; Group all records by prefix
  (setq prefix-groups (MBS:group-records-by-prefix records num-index desc-index))
  (prompt (strcat "\n📊 Found " (itoa (length prefix-groups)) " prefix groups"))
  
  ;; Build change list
  (setq changes-list (MBS:build-renumber-changes prefix-groups num-index desc-index))
  (setq total-changes (length changes-list))
  
  (if (= total-changes 0)
    (progn
      (MBS:log-info "\n✅ All ITEM NOs are already properly sequenced")
      0
    )
    (progn
      ;; Show preview
      (MBS:show-renumber-preview changes-list total-changes)
      
      ;; Get user confirmation
      (setq user-choice (strcase (getstring "\n❓ Proceed with renumbering? (Y/N): ")))
      
      (if (= user-choice "Y")
        (progn
          (setq applied-changes (MBS:apply-renumber-changes changes-list current-mode))
          ;; Execute the queued transactions to save changes to CSV
          (if (> (length MBS:transaction-queue) 0)
            (progn
              (prompt "\n💾 Executing Phase 6 transactions to save ITEM NO changes...")
              (MBS:execute-transactions (nth 0 session-context))
              (MBS:log-info "\n✅ ITEM NO changes saved to CSV")
            )
          )
          applied-changes
        )
        (progn
          (MBS:log-warning "\n⚠ ITEM NO. renumbering skipped")
          0
        )
      )
    )
  )
)

;; Helper function to build change list
(defun MBS:build-renumber-changes (prefix-groups num-index desc-index / changes-list prefix group-records current-num record-pair record-index record current-item-no expected-item-no description change-info)
  "Build list of changes needed for renumbering"
  
  (setq changes-list '())
  
  (foreach prefix-group prefix-groups
    (setq prefix (car prefix-group))
    (setq group-records (cdr prefix-group))
    (setq current-num 1)
    
    (foreach record-pair group-records
      (setq record-index (car record-pair))
      (setq record (cdr record-pair))
      (setq current-item-no (nth num-index record))
      (setq expected-item-no (strcat prefix (itoa current-num)))
      (setq description (nth desc-index record))
      
      (if (not (= current-item-no expected-item-no))
        (progn
          (setq change-info (list record-index current-item-no expected-item-no description))
          (setq changes-list (append changes-list (list change-info)))
        )
      )
      (setq current-num (1+ current-num))
    )
  )
  
  changes-list
)

;; Helper function to show preview
(defun MBS:show-renumber-preview (changes-list total-changes / change-info old-item-no new-item-no description old-display)
  "Show preview of renumbering changes"
  
  (prompt "\n🔍 === PREVIEW OF ITEM NO. CHANGES ===")
  (foreach change-info changes-list
    (setq old-item-no (nth 1 change-info))
    (setq new-item-no (nth 2 change-info))
    (setq description (nth 3 change-info))
    (setq old-display (if (or (not old-item-no) (= old-item-no "")) "[blank]" old-item-no))
    (prompt (strcat "\n" description ": " old-display " → " new-item-no))
  )
  (prompt (strcat "\n📊 Total changes: " (itoa total-changes)))
)

;; Helper function to apply changes
(defun MBS:apply-renumber-changes (changes-list current-mode / applied-changes change-info record-index new-item-no block-updates)
  "Apply the renumbering changes to CSV and blocks"
  
  (prompt "\n🔄 Applying ITEM NO. changes...")
  (setq applied-changes 0)
  
  ;; Apply changes to CSV via transactions
  (foreach change-info changes-list
    (setq record-index (nth 0 change-info))
    (setq new-item-no (nth 2 change-info))
    (MBS:queue-transaction (MBS:tx-update-field record-index "ITEM NO." new-item-no))
    (setq applied-changes (1+ applied-changes))
  )
  
  ;; Update all matching blocks
  (MBS:log-info "\n🔧 Updating block attributes...")
  (setq block-updates (MBS:update-block-item-numbers changes-list current-mode))
  
  (MBS:log-info (strcat "\n✅ Queued " (itoa applied-changes) " CSV changes"))
  (MBS:log-info (strcat "\n✅ Updated " (itoa block-updates) " block attributes"))
  
  applied-changes
)

;; Helper function to update block attributes
(defun MBS:update-block-item-numbers (changes-list current-mode / block-updates block-map change-info description new-item-no block-info block-desc entity-obj att-list att-ref csv-data header records all-block-updates)
  "Update block attributes with new ITEM NO values - COMPREHENSIVE VERSION"
  
  (setq block-updates 0)
  (setq block-map (MBS:build-comprehensive-block-map current-mode))
  
  ;; First, update blocks according to the change list
  (foreach change-info changes-list
    (setq description (nth 3 change-info))
    (setq new-item-no (nth 2 change-info))
    
    ;; Find blocks with this description and update their ITEM NO
    (foreach block-info block-map
      (setq block-desc (nth 3 block-info))
      (if (= block-desc description)
        (progn
          (setq entity-obj (nth 1 block-info))
          (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke entity-obj 'GetAttributes))))
          (if (not (vl-catch-all-error-p att-list))
            (progn
              (foreach att-ref att-list
                (if (= (strcase (vlax-get att-ref 'TagString)) "##")
                  (progn
                    (vlax-put att-ref 'TextString new-item-no)
                    (setq block-updates (1+ block-updates))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  
  ;; COMPREHENSIVE PHASE 6: Update ALL blocks to sync with current CSV state
  (MBS:log-info "\n🔧 Phase 6: Syncing ALL blocks with CSV for comprehensive update...")
  (setq all-block-updates (MBS:sync-all-blocks-with-csv current-mode))
  (MBS:log-info (strcat "\n✅ Synced " (itoa (length all-block-updates)) " blocks with CSV"))
  (setq block-updates (+ block-updates all-block-updates))
  
  block-updates
)

;; Helper function to sync ALL blocks with CSV - COMPREHENSIVE PHASE 6
(defun MBS:sync-all-blocks-with-csv (current-mode / csv-path block-map csv-data header records block-info entity-obj att-list att-ref material-id description current-item-no matching-record csv-item-no synced-updates)
  "Sync ALL blocks in drawing with current CSV state - comprehensive update"
  
  (prompt "\n🔍 DEBUG: Starting comprehensive block-CSV sync...")
  
  ;; Get current CSV path from session context
  (if MBS:session-context
    (progn
      (setq csv-path (nth 0 MBS:session-context))
      (prompt (strcat "\n🔍 DEBUG: CSV path from session: " csv-path))
    )
    (progn
      (prompt "\n⚠ DEBUG: No session context found, detecting CSV path...")
      (setq csv-path (MBS:detect-csv-path))
      (prompt (strcat "\n🔍 DEBUG: Detected CSV path: " csv-path))
    )
  )
  
  (setq synced-updates 0)
  (setq block-map (MBS:build-comprehensive-block-map current-mode))
  
  ;; Get CSV path from global session context - fallback to detection
  (setq csv-path 
    (if (and (boundp 'MBS:session-context) MBS:session-context)
      (nth 0 MBS:session-context)
      (MBS:detect-csv-path)  ; Fallback detection
    )
  )
  
  (if csv-path
    (progn
      ;; Re-read CSV to get current state after all transactions
      (setq csv-data (CSV:read-file csv-path))
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      (prompt (strcat "\n🔄 Syncing ALL " (itoa (length block-map)) " blocks with current CSV state..."))
      
      (foreach block-info block-map
        (setq entity-obj (nth 1 block-info))
        (setq material-id (nth 2 block-info))
        (setq description (nth 3 block-info))
        (setq current-item-no (nth 4 block-info))
        
        ;; If block has a material ID, find its CSV counterpart
        (if (and material-id (/= material-id ""))
          (progn
            ;; Find matching CSV record by material ID
            (setq matching-record (MBS:find-csv-record-by-id records header material-id current-mode))
            (if matching-record
              (progn
                (setq csv-item-no (nth 0 matching-record))  ; ITEM NO is at index 0
                ;; Update block if CSV has different ITEM NO
                (if (and csv-item-no (/= csv-item-no "") (/= csv-item-no current-item-no))
                  (progn
                    (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke entity-obj 'GetAttributes))))
                    (if (not (vl-catch-all-error-p att-list))
                      (progn
                        (foreach att-ref att-list
                          (if (= (strcase (vlax-get att-ref 'TagString)) "##")
                            (progn
                              (vlax-put att-ref 'TextString csv-item-no)
                              (setq synced-updates (1+ synced-updates))
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
        )
      )
      
      (if (> synced-updates 0)
        (MBS:log-info (strcat "\n🔧 Comprehensively synced " (itoa synced-updates) " blocks with CSV ITEM NO"))
        (MBS:log-info "\n✅ All blocks already match CSV ITEM NO values")
      )
    )
    (prompt "\n⚠ Could not find CSV path for comprehensive sync")
  )
  
  synced-updates
)

;; Helper function to find CSV record by ID - DEBUG VERSION
(defun MBS:find-csv-record-by-id (records header target-id current-mode / id-attribute id-index record result record-count checked-count csv-id debug-first-few)
  "Find CSV record by material/removal ID - DEBUG VERSION"
  
  ;; Use global config to determine correct ID column name
  (setq id-attribute (get-id-column))
  (setq id-index (CSV:get-column-index header (strcase id-attribute)))
  (setq result nil)
  (setq record-count 0)
  (setq checked-count 0)
  (setq debug-first-few 3)
  
  ;; Only show deep debug for first few calls to avoid spam
  (if (not MBS:deep-debug-count) (setq MBS:deep-debug-count 0))
  (setq MBS:deep-debug-count (1+ MBS:deep-debug-count))
  
  (if id-index
    (progn
      (if (<= MBS:deep-debug-count 3)
        (progn
          (prompt (strcat "\n🔍 DEEP DEBUG: Searching for target ID: '" target-id "'"))
          (prompt (strcat "\n🔍 DEEP DEBUG: Using column '" id-attribute "' at index " (itoa id-index)))
          (prompt (strcat "\n🔍 DEEP DEBUG: Have " (itoa (length records)) " records to search"))
        )
      )
      
      (foreach record records
        (setq record-count (1+ record-count))
        (if (>= (length record) (1+ id-index))
          (progn
            (setq checked-count (1+ checked-count))
            (setq csv-id (nth id-index record))
            
            ;; Debug first few records (only for first few searches)
            (if (and (<= MBS:deep-debug-count 3) (<= record-count debug-first-few))
              (prompt (strcat "\n🔍 DEEP DEBUG: Record " (itoa record-count) " - CSV ID: '" 
                             (if csv-id csv-id "NIL") "' vs Target: '" target-id "'"))
            )
            
            ;; Check for match
            (if (and (not result) (= csv-id target-id))
              (progn
                (setq result record)
                (if (<= MBS:deep-debug-count 3)
                  (prompt (strcat "\n✅ DEEP DEBUG: MATCH FOUND at record " (itoa record-count)))
                )
              )
            )
          )
          ;; Debug short records (only for first few searches)
          (if (and (<= MBS:deep-debug-count 3) (<= record-count debug-first-few))
            (prompt (strcat "\n⚠ DEEP DEBUG: Record " (itoa record-count) " too short: " 
                           (itoa (length record)) " fields"))
          )
        )
      )
      
      (if (<= MBS:deep-debug-count 3)
        (progn
          (prompt (strcat "\n🔍 DEEP DEBUG: Checked " (itoa checked-count) "/" (itoa record-count) " valid records"))
          (if (not result)
            (prompt (strcat "\n❌ DEEP DEBUG: No match found for ID: '" target-id "'"))
          )
        )
      )
    )
    (prompt (strcat "\n⚠ ID column '" id-attribute "' not found in CSV"))
  )
  
  result  ; Return found record or nil
)

(prompt "\nProgressive phase functions loaded!")

;; ========================================================================
;; RESOLUTION FUNCTIONS - Handle each issue type interactively
;; ========================================================================

;; Resolve orphaned CSV rows (no matching blocks)
(defun MBS:resolve-orphaned-rows (orphaned-rows session-context / resolved-count row-data record material-id description user-choice user-choices deletions-list current-mode id-attribute final-choice)
  "Handle CSV rows that have no corresponding blocks"
  
  (setq resolved-count 0)
  (setq user-choices '())
  (setq deletions-list '())
  
  (prompt "\n📋 === RESOLVING ORPHANED CSV ROWS ===")
  (prompt (strcat "\n Found " (itoa (length orphaned-rows)) " CSV rows without matching blocks"))
  
  ;; First pass: collect user choices
  (foreach row-data orphaned-rows
    (setq row-index (car row-data))
    (setq record (cdr row-data))
    (setq current-mode (nth 5 session-context))
    (setq material-id (nth (if (= current-mode "LOR") 13 15) record))
    (setq description (nth 3 record))
    
    (prompt "\n")
    (prompt "📋 Orphaned CSV Row:")
    (prompt (strcat "   ID: " (if material-id material-id "N/A")))
    (prompt (strcat "   Description: " (if description description "N/A")))
    
    (prompt "\nChoose resolution:")
    (prompt "  [I] Insert Block for this row")
    (prompt "  [D] Delete this CSV row")
    (prompt "  [S] Skip (leave as-is)")
    
    (setq user-choice (strcase (getstring "\nEnter choice (I/D/S): ")))
    
    ;; Store the choice with the data
    (setq user-choices (append user-choices (list (list user-choice row-index record material-id description))))
    
    ;; If deletion was chosen, add to deletions list for preview
    (if (= user-choice "D")
      (setq deletions-list (append deletions-list (list (list material-id description))))
    )
  )
  
  ;; Show deletion preview and get confirmation if any deletions were requested
  (if (> (length deletions-list) 0)
    (progn
      (setq current-mode (nth 5 session-context))
      (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
      
      (prompt "\n\n🔍 === PREVIEW OF CHANGES ===")
      (prompt "\n⚠ Warning! Will delete CSV rows:")
      
      (foreach deletion-item deletions-list
        (setq material-id (car deletion-item))
        (setq description (cadr deletion-item))
        (prompt (strcat "\n   " id-attribute ": " (if material-id material-id "N/A")))
        (prompt (strcat "\n   DESCRIPTION: " (if description description "N/A")))
      )
      
      (prompt "\n\n❓ Proceed with this action? (Y/N): ")
      (setq final-choice (strcase (getstring)))
      
      (if (/= final-choice "Y")
        (progn
          (prompt "\n⏭ Deletion cancelled by user")
          (setq deletions-list '())  ; Clear deletions list
        )
      )
    )
  )
  
  ;; Second pass: execute the choices
  (foreach choice-data user-choices
    (setq user-choice (nth 0 choice-data))
    (setq row-index (nth 1 choice-data))
    (setq record (nth 2 choice-data))
    (setq material-id (nth 3 choice-data))
    (setq description (nth 4 choice-data))
    
    (cond
      ((= user-choice "I")
       (if (MBS:insert-block-for-row record row-index session-context)
         (progn
           (prompt "\n✅ Block inserted successfully")
           (setq resolved-count (1+ resolved-count))
         )
         (prompt "\n❌ Failed to insert block")
       )
      )
      ((= user-choice "D")
       ;; Only execute deletion if user confirmed in preview
       (if (> (length deletions-list) 0)
         (if (MBS:delete-csv-row record session-context)
           (setq resolved-count (1+ resolved-count))
           (prompt "\n❌ Failed to queue CSV row deletion")
         )
         (prompt "\n⏭ Deletion skipped (cancelled in preview)")
       )
      )
      ((= user-choice "S")
       (prompt "\n⏭ Skipped - row left as-is")
      )
      (t
       (prompt "\n❌ Invalid choice - skipping")
      )
    )
  )
  
  resolved-count
)

;; Resolve orphaned blocks (no matching CSV rows)
(defun MBS:resolve-orphaned-blocks (orphaned-blocks session-context / resolved-count grouped-blocks group-desc group-blocks first-block group-deleted user-choice)
  "Handle blocks that have no corresponding CSV rows"
  
  (setq resolved-count 0)
  
  (prompt "\n🧱 === RESOLVING ORPHANED BLOCKS ===")
  (prompt (strcat "\n Found " (itoa (length orphaned-blocks)) " blocks without matching CSV rows"))
  
  ;; Debug: Show orphaned block info
  (setq i 0)
  (foreach block-info orphaned-blocks
    (setq description (nth 3 block-info))
    (prompt (strcat "\n🔍 DEBUG: Block " (itoa i) " desc: '" (if description description "NIL") "'"))
    (setq i (1+ i))
  )
  
  ;; Group blocks by description - PROPER GROUPING VERSION
  (prompt "\n🔍 DEBUG: Starting inline grouping logic...")
  (setq grouped-blocks '())
  
  ;; Group blocks by exact description match
  (if (> (length orphaned-blocks) 0)
    (progn
      (prompt "\n🔍 DEBUG: Grouping blocks by exact description...")
      (foreach block-info orphaned-blocks
        (setq description (nth 3 block-info))
        (prompt (strcat "\n🔍 DEBUG: Processing block with desc: '" description "'"))
        
        ;; Find existing group with this description
        (setq existing-group nil)
        (foreach group grouped-blocks
          (if (equal (car group) description)
            (setq existing-group group)
          )
        )
        
        (if existing-group
          ;; Add to existing group
          (progn
            (prompt (strcat "\n🔍 DEBUG: Adding to existing group: '" description "'"))
            (setq grouped-blocks (vl-remove existing-group grouped-blocks))
            (setq grouped-blocks (append grouped-blocks 
                                         (list (cons description (append (cdr existing-group) (list block-info))))))
          )
          ;; Create new group
          (progn
            (prompt (strcat "\n🔍 DEBUG: Creating new group: '" description "'"))
            (setq grouped-blocks (append grouped-blocks (list (cons description (list block-info)))))
          )
        )
      )
      (prompt (strcat "\n🔍 DEBUG: Created " (itoa (length grouped-blocks)) " groups"))
    )
    (prompt "\n🔍 DEBUG: No blocks to group")
  )
  
  (prompt (strcat "\n🔍 DEBUG: Grouped into " (itoa (length grouped-blocks)) " groups"))
  
  (prompt "\n🔍 DEBUG: Starting foreach loop for groups...")
  (foreach group grouped-blocks
    (setq group-desc (car group))
    (setq group-blocks (cdr group))
    
    (prompt (strcat "\n🔍 DEBUG: Processing group: '" (if group-desc group-desc "NIL") "'"))
    (prompt "\n")
    (prompt "🧱 Orphaned Block Group:")
    (prompt (strcat "   Description: " (if group-desc group-desc "N/A")))
    (prompt (strcat "   Count: " (itoa (length group-blocks)) " identical blocks"))
    
    (prompt (strcat "\nChoose resolution for ALL " (itoa (length group-blocks)) " blocks:"))
    (prompt "  [G] Generate CSV row + ID for this block group")
    (prompt "  [D] Delete all blocks in this group")
    (prompt "  [S] Skip (leave as-is)")
    
    (setq user-choice (strcase (getstring "\nEnter choice (G/D/S): ")))
    
    (cond
      ((= user-choice "G")
       ;; Show preview of what will be generated
       (setq first-block (nth 0 group-blocks))
       (setq description (nth 3 first-block))
       (setq preview-id (generate-unique-id description))
       
       (prompt "\n🔍 === PREVIEW OF CHANGES ===")
       (prompt (strcat "\n📝 Will create CSV row:"))
       (prompt (strcat "\n   " (if (= (nth 5 session-context) "LOR") "REMOVAL_ID" "MATERIAL_ID") ": " preview-id))
       (prompt (strcat "\n   DESCRIPTION: " description))
       (prompt (strcat "\n   ITEM NO.: [empty - will be set in Phase 6]"))
       (MBS:log-info (strcat "\n🔧 Will update " (itoa (length group-blocks)) " blocks with new ID"))
       
       ;; Ask for confirmation
       (setq confirmation (strcase (getstring "\n❓ Proceed with this action? (Y/N): ")))
       
       (if (= confirmation "Y")
         (progn
           ;; Create CSV row with the first block using the SAME preview-id
           (if (MBS:insert-csv-row-for-block-with-id first-block session-context preview-id)
             (progn
               ;; Use the same ID that was shown in preview
               (setq generated-id preview-id)
               
               ;; Update ALL remaining blocks in the group with the same ID (INCLUDING the first block again to ensure consistency)
               (setq blocks-updated 0)  ; Count all blocks we update
               (setq i 0)  ; Start from block 0 (first block) to ensure all get updated
               (while (< i (length group-blocks))
                 (setq block-info (nth i group-blocks))
                 (setq entity-obj (nth 1 block-info))
                 (setq id-attribute (if (= (nth 5 session-context) "LOR") "REMOVAL_ID" "MATERIAL_ID"))
                 
                 (MBS:log-info (strcat "\n🔧 Updating block " (itoa (1+ i)) " with " id-attribute ": " generated-id))
                 
                 ;; Update this block's attributes with enhanced debugging
                 (prompt (strcat "\n🔍 DEBUG: Processing block " (itoa (1+ i)) " - Entity type: " (vla-get-ObjectName entity-obj)))
                 (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke entity-obj 'GetAttributes))))
                 (if (not (vl-catch-all-error-p att-list))
                   (progn
                     (prompt (strcat "\n🔍 DEBUG: Found " (itoa (length att-list)) " attributes"))
                     (setq id-updated nil)
                     (setq item-updated nil)
                     
                     ;; Debug: Show all attribute tags
                     (foreach att-ref att-list
                       (prompt (strcat "\n🔍 DEBUG: Attribute tag: '" (vlax-get att-ref 'TagString) "' = '" (vlax-get att-ref 'TextString) "'"))
                     )
                     
                     (foreach att-ref att-list
                       (setq current-tag (strcase (vlax-get att-ref 'TagString)))
                       (cond
                         ;; Update ID attribute
                         ((= current-tag (strcase id-attribute))
                          (vlax-put att-ref 'TextString generated-id)
                          (setq id-updated t)
                          (prompt (strcat "\n  ✅ Updated " id-attribute " to: " generated-id))
                         )
                         ;; Update ITEM NO attribute to TBD
                         ((= current-tag "##")
                          (vlax-put att-ref 'TextString "TBD")
                          (setq item-updated t)
                          (prompt "\n  ✅ Updated ITEM NO to: TBD")
                         )
                       )
                     )
                     ;; Force update the block display
                     (vla-Update entity-obj)
                     (if id-updated
                       (progn
                         (setq blocks-updated (1+ blocks-updated))
                         (prompt (strcat "\n  ✅ Block " (itoa (1+ i)) " successfully updated"))
                       )
                       (prompt (strcat "\n  ❌ Block " (itoa (1+ i)) " - " id-attribute " attribute not found or not updated"))
                     )
                     (if (not item-updated)
                       (prompt (strcat "\n  ⚠ Block " (itoa (1+ i)) " - ITEM NO (##) attribute not found"))
                     )
                   )
                   (progn
                     (prompt (strcat "\n  ❌ Failed to get attributes for block " (itoa (1+ i))))
                     (prompt (strcat "\n  🔍 DEBUG: Error details: " (vl-prin1-to-string att-list)))
                   )
                 )
                 (setq i (1+ i))
               )
               
               (prompt (strcat "\n✅ CSV row + ID generated and applied to " (itoa blocks-updated) "/" (itoa (length group-blocks)) " blocks"))
               (setq resolved-count (+ resolved-count blocks-updated))
             )
             (prompt "\n❌ Failed to generate CSV row + ID")
           )
         )
         (prompt "\n⏭ Action cancelled by user")
       )
      )
      ((= user-choice "D")
       ;; Delete all blocks in group
       (setq group-deleted 0)
       (foreach block-info group-blocks
         (if (MBS:delete-block block-info session-context)
           (setq group-deleted (1+ group-deleted))
         )
       )
       (prompt (strcat "✅ Deleted " (itoa group-deleted) "/" (itoa (length group-blocks)) " blocks"))
       (setq resolved-count (+ resolved-count group-deleted))
      )
      ((= user-choice "S")
       (prompt (strcat "⏭ Skipped " (itoa (length group-blocks)) " blocks - left as-is"))
      )
      (t
       (prompt "❌ Invalid choice - skipping group")
      )
    )
  )
  
  (prompt (strcat "\n🔍 DEBUG: Finished orphaned blocks resolution, resolved: " (itoa resolved-count)))
  resolved-count
)

;; Helper function to group blocks by description - SIMPLIFIED TEST VERSION
(defun MBS:group-blocks-by-description (blocks)
  (prompt "\n🔍 DEBUG GROUP: Function called!")
  (prompt (strcat "\n🔍 DEBUG GROUP: Received " (itoa (length blocks)) " blocks"))
  
  ;; Very simple test - just create one group with all blocks
  (if (> (length blocks) 0)
    (progn
      (prompt "\n🔍 DEBUG GROUP: Creating single group with all blocks")
      (setq first-block (nth 0 blocks))
      (setq description (nth 3 first-block))
      (prompt (strcat "\n🔍 DEBUG GROUP: Using description: '" description "'"))
      (list (cons description blocks))
    )
    (progn
      (prompt "\n🔍 DEBUG GROUP: No blocks to group")
      '()
    )
  )
)

;; Resolve semi-orphaned blocks with identical description matches
(defun MBS:resolve-semi-orphaned-identical (semi-orphaned-blocks session-context / resolved-count block-data block-info csv-match entity-obj description matching-id user-choice)
  "Handle blocks with empty IDs but identical description matches in CSV"
  
  (setq resolved-count 0)
  
  (prompt "\n🔗 === RESOLVING SEMI-ORPHANED BLOCKS (IDENTICAL MATCHES) ===")
  (prompt (strcat "\n Found " (itoa (length semi-orphaned-blocks)) " blocks with empty IDs but matching descriptions"))
  
  (foreach block-data semi-orphaned-blocks
    (setq block-info (nth 0 block-data))
    (setq csv-match (nth 1 block-data))
    (setq entity-obj (nth 1 block-info))
    (setq description (nth 3 block-info))
    (setq csv-record (cdr csv-match))  ; Extract actual CSV record
    
    ;; Get correct column index based on mode
    (setq current-mode (nth 5 session-context))
    (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
    (setq id-index (if (= current-mode "LOR") 13 15))  ; REMOVAL_ID at 13, MATERIAL_ID at 15
    (setq matching-id (nth id-index csv-record))
    
    (prompt "\n")
    (prompt "🔗 Semi-Orphaned Block (Identical Match):")
    (prompt (strcat "   Block Description: " (if description description "N/A")))
    (prompt (strcat "   Matching CSV ID: " (if matching-id matching-id "N/A")))
    
    (prompt "\nChoose resolution:")
    (prompt "  [R] Re-associate block with CSV ID")
    (prompt "  [D] Delete this block")
    (prompt "  [S] Skip (leave as-is)")
    
    (setq user-choice (strcase (getstring "\nEnter choice (R/D/S): ")))
    
    (cond
      ((= user-choice "R")
       (if (MBS:reassociate-block block-info matching-id session-context)
         (progn
           (prompt "✅ Block re-associated successfully")
           (setq resolved-count (1+ resolved-count))
         )
         (prompt "❌ Failed to re-associate block")
       )
      )
      ((= user-choice "D")
       (if (MBS:delete-block block-info session-context)
         (progn
           (prompt "✅ Block deleted successfully")
           (setq resolved-count (1+ resolved-count))
         )
         (prompt "❌ Failed to delete block")
       )
      )
      ((= user-choice "S")
       (prompt "⏭ Skipped - block left as-is")
      )
      (t
       (prompt "❌ Invalid choice - skipping")
      )
    )
  )
  
  resolved-count
)

;; Resolve semi-orphaned blocks with fuzzy description matches - WITH GROUPING
(defun MBS:resolve-semi-orphaned-fuzzy (semi-orphaned-blocks session-context / resolved-count block-groups group-description block-group fuzzy-matches group-choice blocks-in-group user-choice block-data block-info entity-obj description selected-match matching-id match-num)
  "Handle blocks with empty IDs but fuzzy description matches in CSV - GROUPED BY DESCRIPTION"
  
  (setq resolved-count 0)
  
  (prompt "\n🔗 === RESOLVING SEMI-ORPHANED BLOCKS (FUZZY MATCHES) ===")
  (prompt (strcat "\n Found " (itoa (length semi-orphaned-blocks)) " blocks with empty IDs but similar descriptions"))
  
  ;; Group blocks by exact description (like orphaned blocks)
  (setq block-groups (MBS:group-semi-orphaned-by-description semi-orphaned-blocks))
  (prompt (strcat "\n🔍 Grouped into " (itoa (length block-groups)) " description groups"))
  
  ;; Process each group
  (foreach block-group block-groups
    (setq group-description (car block-group))
    (setq blocks-in-group (cdr block-group))
    (setq fuzzy-matches (nth 1 (nth 0 blocks-in-group)))  ; Get fuzzy matches from first block (all have same matches)
    
    (prompt "\n")
    (prompt "🔗 Semi-Orphaned Block Group (Fuzzy Matches):")
    (prompt (strcat "   Group Description: " group-description))
    (prompt (strcat "   Count: " (itoa (length blocks-in-group)) " identical blocks"))
    (prompt "\n   Possible CSV matches:")
    
    ;; Display fuzzy matches with numbers
    (setq match-num 1)
    
    ;; Get correct column index for display based on mode
    (setq current-mode (nth 5 session-context))
    (setq display-id-index (if (= current-mode "LOR") 13 15))  ; REMOVAL_ID at 13, MATERIAL_ID at 15
    
    (foreach match fuzzy-matches
      (setq match-record (car (nth 0 match)))  ; Get the CSV record from (cons row-index record)
      (setq match-csv-record (cdr (nth 0 match)))  ; Get actual CSV record
      (setq match-id (nth display-id-index match-csv-record))  ; Use mode-specific index
      (setq match-desc (nth 3 match-csv-record))  ; DESCRIPTION at index 3
      (setq similarity (nth 1 match))  ; Similarity score
      (prompt (strcat "   [" (itoa match-num) "] ID: " match-id " - " match-desc " (Similarity: " (rtos (* similarity 100) 2 1) "%)"))
      (setq match-num (1+ match-num))
    )
    
    (prompt (strcat "\nChoose resolution for ALL " (itoa (length blocks-in-group)) " blocks:"))
    (prompt (strcat "  [1-" (itoa (length fuzzy-matches)) "] Re-associate with selected match"))
    (prompt "  [D] Delete all blocks in this group")
    (prompt "  [S] Skip (leave as-is)")
    
    (setq user-choice (strcase (getstring "\nEnter choice: ")))
    
    (cond
      ((and (>= (atoi user-choice) 1) (<= (atoi user-choice) (length fuzzy-matches)))
       (setq selected-match (nth (1- (atoi user-choice)) fuzzy-matches))
       (setq selected-csv-record (cdr (nth 0 selected-match)))  ; Get CSV record
       
       ;; Get correct column index based on mode
       (setq current-mode (nth 5 session-context))
       (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
       (setq id-index (if (= current-mode "LOR") 13 15))  ; REMOVAL_ID at 13, MATERIAL_ID at 15
       (setq matching-id (nth id-index selected-csv-record))
       
       ;; Apply to ALL blocks in group
       (prompt (strcat "\n🔗 Re-associating " (itoa (length blocks-in-group)) " blocks with " id-attribute ": " matching-id))
       (foreach block-data blocks-in-group
         (setq block-info (nth 0 block-data))
         (if (MBS:reassociate-block block-info matching-id session-context)
           (setq resolved-count (1+ resolved-count))
         )
       )
       (prompt (strcat "✅ Group re-associated successfully (" (itoa (length blocks-in-group)) " blocks)"))
      )
      ((= user-choice "D")
       ;; Delete all blocks in group
       (prompt (strcat "\n🗑️ Deleting " (itoa (length blocks-in-group)) " blocks in group"))
       (foreach block-data blocks-in-group
         (setq block-info (nth 0 block-data))
         (if (MBS:delete-block block-info session-context)
           (setq resolved-count (1+ resolved-count))
         )
       )
       (prompt (strcat "✅ Group deleted successfully (" (itoa (length blocks-in-group)) " blocks)"))
      )
      ((= user-choice "S")
       (prompt (strcat "⏭ Skipped group - " (itoa (length blocks-in-group)) " blocks left as-is"))
      )
      (t
       (prompt "❌ Invalid choice - skipping group")
      )
    )
  )
  
  resolved-count
)

;; Helper function to group semi-orphaned blocks by description
(defun MBS:group-semi-orphaned-by-description (semi-orphaned-blocks / groups existing-group block-data block-info description)
  "Group semi-orphaned blocks by exact description match"
  
  (setq groups '())
  
  (foreach block-data semi-orphaned-blocks
    (setq block-info (nth 0 block-data))
    (setq description (nth 3 block-info))
    
    ;; Find existing group or create new one
    (setq existing-group (assoc description groups))
    (if existing-group
      ;; Add to existing group using compatible method
      (setq groups (MBS:update-assoc-value groups description (append (cdr existing-group) (list block-data))))
      ;; Create new group
      (setq groups (append groups (list (cons description (list block-data)))))
    )
  )
  
  groups
)

;; Original single-block function for backward compatibility
(defun MBS:resolve-semi-orphaned-fuzzy-single (semi-orphaned-blocks session-context / resolved-count block-data block-info fuzzy-matches entity-obj description selected-match matching-id user-choice match-num)
  "Handle blocks with empty IDs but fuzzy description matches in CSV - SINGLE BLOCK MODE"
  
  (setq resolved-count 0)
  
  (prompt "\n🔗 === RESOLVING SEMI-ORPHANED BLOCKS (FUZZY MATCHES) ===")
  (prompt (strcat "\n Found " (itoa (length semi-orphaned-blocks)) " blocks with empty IDs but similar descriptions"))
  
  (foreach block-data semi-orphaned-blocks
    (setq block-info (nth 0 block-data))
    (setq fuzzy-matches (nth 1 block-data))
    (setq entity-obj (nth 1 block-info))
    (setq description (nth 3 block-info))
    
    (prompt "\n")
    (prompt "🔗 Semi-Orphaned Block (Fuzzy Matches):")
    (prompt (strcat "   Block Description: " (if description description "N/A")))
    (prompt "\n   Possible CSV matches:")
    
    ;; Display fuzzy matches with numbers
    (setq match-num 1)
    (foreach match fuzzy-matches
      (setq match-record (car (nth 0 match)))  ; Get the CSV record from (cons row-index record)
      (setq match-csv-record (cdr (nth 0 match)))  ; Get actual CSV record
      (setq match-id (nth 13 match-csv-record))  ; REMOVAL_ID at index 13
      (setq match-desc (nth 3 match-csv-record))  ; DESCRIPTION at index 3
      (setq similarity (nth 1 match))  ; Similarity score
      (prompt (strcat "   [" (itoa match-num) "] ID: " match-id " - " match-desc " (Similarity: " (rtos (* similarity 100) 2 1) "%)"))
      (setq match-num (1+ match-num))
    )
    
    (prompt "\nChoose resolution:")
    (prompt (strcat "  [1-" (itoa (length fuzzy-matches)) "] Re-associate with selected match"))
    (prompt "  [D] Delete this block")
    (prompt "  [S] Skip (leave as-is)")
    
    (setq user-choice (strcase (getstring "\nEnter choice: ")))
    
    (cond
      ((and (>= (atoi user-choice) 1) (<= (atoi user-choice) (length fuzzy-matches)))
       (setq selected-match (nth (1- (atoi user-choice)) fuzzy-matches))
       (setq selected-csv-record (cdr (nth 0 selected-match)))  ; Get CSV record
       (setq matching-id (nth 13 selected-csv-record))  ; REMOVAL_ID at index 13
       (if (MBS:reassociate-block block-info matching-id session-context)
         (progn
           (prompt "✅ Block re-associated successfully")
           (setq resolved-count (1+ resolved-count))
         )
         (prompt "❌ Failed to re-associate block")
       )
      )
      ((= user-choice "D")
       (if (MBS:delete-block block-info session-context)
         (progn
           (prompt "✅ Block deleted successfully")
           (setq resolved-count (1+ resolved-count))
         )
         (prompt "❌ Failed to delete block")
       )
      )
      ((= user-choice "S")
       (prompt "⏭ Skipped - block left as-is")
      )
      (t
       (prompt "❌ Invalid choice - skipping")
      )
    )
  )
  
  resolved-count
)

;; Resolve missing attributes
(defun MBS:resolve-missing-attributes (missing-attrs session-context / resolved-count attr-data block-info issues entity-obj material-id description item-no user-choice)
  "Handle blocks with missing critical attributes"
  
  (setq resolved-count 0)
  
  (prompt "\n⚠ === RESOLVING MISSING ATTRIBUTES ===")
  (prompt (strcat "\n Found " (itoa (length missing-attrs)) " blocks with missing critical attributes"))
  
  (foreach attr-data missing-attrs
    (setq block-info (nth 0 attr-data))
    (setq issues (nth 1 attr-data))
    (setq entity-obj (nth 1 block-info))
    (setq material-id (nth 2 block-info))
    (setq description (nth 3 block-info))
    (setq item-no (nth 4 block-info))
    
    (prompt "\n")
    (prompt "⚠ Block with Missing Attributes:")
    (prompt (strcat "   Current ID: " (if material-id material-id "MISSING")))
    (prompt (strcat "   Current Description: " (if description description "MISSING")))
    (prompt (strcat "   Current Item No: " (if item-no item-no "MISSING")))
    (prompt (strcat "   Issues: " (vl-princ-to-string issues)))
    
    (prompt "\nChoose resolution:")
    (prompt "  [F] Fix missing attributes (generate IDs, etc.)")
    (prompt "  [D] Delete this block")
    (prompt "  [S] Skip (leave as-is)")
    
    (setq user-choice (strcase (getstring "\nEnter choice (F/D/S): ")))
    
    (cond
      ((= user-choice "F")
       ;; NOTE: Fix missing attributes removed - handled in other phases
       (prompt "\n⚠ Fix missing attributes not implemented - use other resolution phases")
      )
      ((= user-choice "D")
       (if (MBS:delete-block block-info session-context)
         (progn
           (prompt "✅ Block deleted successfully")
           (setq resolved-count (1+ resolved-count))
         )
         (prompt "❌ Failed to delete block")
       )
      )
      ((= user-choice "S")
       (prompt "⏭ Skipped - block left as-is")
      )
      (t
       (prompt "❌ Invalid choice - skipping")
      )
    )
  )
  
  resolved-count
)

(prompt "\nResolution functions loaded!")

;; ========================================================================
;; RESOLUTION ACTION FUNCTIONS - Perform actual changes
;; ========================================================================


(defun MBS:insert-block-for-row (record row-index session-context / material-id description item-no target-space space-result ins-pt block current-mode id-attribute original-material-id)
  "Insert a new block for an orphaned CSV row"
  
  (setq current-mode (nth 5 session-context))
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
  
  ;; Extract data from CSV record
  (setq material-id (nth (if (= current-mode "LOR") 13 15) record))  ; REMOVAL_ID at 13, MATERIAL_ID at 15
  (setq description (nth 3 record))  ; DESCRIPTION at index 3
  (setq item-no (nth 0 record))      ; ITEM NO. at index 0 (corrected from index 1)
  (setq original-material-id material-id)  ; Store original for comparison
  
  (prompt (strcat "\n🏗️ Inserting block for orphaned row:"))
  (prompt (strcat "\n   " id-attribute ": " (if material-id material-id "N/A")))
  (prompt (strcat "\n   DESCRIPTION: " (if description description "N/A")))
  (prompt (strcat "\n   ITEM NO.: " (if item-no item-no "N/A")))
  
  ;; Get current drawing space
  (setq space-result (MBS:get-current-space))
  (setq target-space (car space-result))
  
  ;; Prompt for insertion point
  (prompt "\n📍 Specify insertion point for new block: ")
  (setq ins-pt (getpoint))
  
  (if ins-pt
    (progn
      ;; Generate ID if missing
      (if (or (not material-id) (= material-id ""))
        (progn
          (setq material-id (generate-unique-id description))
          (MBS:log-info (strcat "\n🔧 Generated new " id-attribute ": " material-id))
        )
      )
      
      ;; Use actual item number from CSV or TBD if empty
      (if (or (not item-no) (= item-no ""))
        (progn
          (setq item-no "TBD")
          (MBS:log-info "\n🔧 Using temporary ITEM NO.: TBD (will be renumbered)")
        )
                  (MBS:log-info (strcat "\n🔧 Using CSV ITEM NO.: " item-no))
      )
      
      ;; Insert block using existing function
      (setq block (MBS:insert-block-with-attributes target-space ins-pt material-id description item-no))
      
      (if block
        (progn
          (prompt "\n✅ Block inserted successfully")
          
          ;; Update CSV row with generated MATERIAL_ID if it was generated
          (if (and (or (not original-material-id) (= original-material-id ""))
                   material-id (/= material-id ""))
            (progn
              (prompt (strcat "\n📝 Updating CSV row with generated " id-attribute ": " material-id))
              (MBS:queue-transaction (MBS:tx-update-field row-index id-attribute material-id))
            )
          )
          
          t  ; Return success
        )
        (progn
          (prompt "\n❌ Failed to insert block")
          nil
        )
      )
    )
    (progn
      (prompt "\n⚠ No insertion point specified - skipping")
      nil
    )
  )
)

(defun MBS:delete-csv-row (record session-context / current-mode id-attribute material-id description)
  "Delete a CSV row by marking it for deletion in transaction queue"
  
  (setq current-mode (nth 5 session-context))
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
  
  ;; Extract identifying data from CSV record
  (setq material-id (nth (if (= current-mode "LOR") 13 15) record))
  (setq description (nth 3 record))
  
  (prompt (strcat "\n🗑️ Marking CSV row for deletion:"))
  (prompt (strcat "\n   " id-attribute ": " (if material-id material-id "N/A")))
  (prompt (strcat "\n   DESCRIPTION: " (if description description "N/A")))
  
  ;; Queue transaction to delete this row by material ID
  (if (and material-id (/= material-id ""))
    (progn
      (MBS:queue-transaction (MBS:tx-delete-row-by-material-id material-id))
      (prompt "\n📝 Row deletion queued for transaction processing")
      t  ; Return success
    )
    (progn
      (prompt "\n❌ Cannot delete row - no valid ID found")
      nil
    )
  )
)

;; New function that accepts pre-generated ID (for consistent preview/execution)
(defun MBS:insert-csv-row-for-block-with-id (block-info session-context provided-id / header current-mode material-id description item-no new-record)
  "Insert a new CSV row for an orphaned block using provided ID"
  
  (setq header (nth 1 session-context))
  (setq current-mode (nth 5 session-context))
  
  ;; Extract data from block-info (format: entity-name entity-obj material-id description item-no attributes)
  (setq material-id provided-id)  ; Use the provided ID instead of generating
  (setq description (nth 3 block-info))
  (setq item-no (nth 4 block-info))
  
  (prompt (strcat "\n📝 Creating CSV row for orphaned block:"))
  (prompt (strcat "\n   " (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID") ": " material-id))
  (prompt (strcat "\n   DESCRIPTION: " (if description description "N/A")))
  (prompt (strcat "\n   ITEM NO.: " (if item-no item-no "N/A")))
  
  ;; Update the block with the provided ID AND set ITEM NO to TBD
  (setq entity-obj (nth 1 block-info))
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
  (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke entity-obj 'GetAttributes))))
  (if (not (vl-catch-all-error-p att-list))
    (progn
      (foreach att-ref att-list
        (cond
          ;; Update ID attribute
          ((= (strcase (vlax-get att-ref 'TagString)) (strcase id-attribute))
           (vlax-put att-ref 'TextString material-id)
           (prompt (strcat "\n🔗 Updated block " id-attribute " to: " material-id))
          )
          ;; Update ITEM NO attribute to TBD for user clarity
          ((= (strcase (vlax-get att-ref 'TagString)) "##")
           (vlax-put att-ref 'TextString "TBD")
           (prompt "\n🔢 Set block ITEM NO. to: TBD (will be renumbered)")
          )
        )
      )
    )
  )
  
  ;; Create and queue CSV record
  (MBS:create-and-queue-csv-record header current-mode material-id description item-no)
)

;; Helper function to create and queue CSV record
(defun MBS:create-and-queue-csv-record (header current-mode material-id description item-no / new-record)
  "Create empty CSV record and queue it for insertion"
  
  ;; Create empty CSV record with proper column count
  (setq new-record (MBS:create-empty-record header))
  
  ;; Set the generated or existing material ID
  (setq new-record (MBS:set-record-value-enhanced new-record header 
                     (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID") material-id))
  
  ;; Set description
  (if description
    (setq new-record (MBS:set-record-value-enhanced new-record header "DESCRIPTION" description))
  )
  
  ;; ITEM NO will be handled in Phase 6 (renumbering) - leave empty for now
  ;; Queue transaction for CSV insertion
  (MBS:queue-transaction (MBS:tx-insert-row new-record))
  (prompt "\n📝 Row insertion queued for transaction processing")
  
  t  ; Return success
)

;; Original function - keep for backward compatibility  
(defun MBS:insert-csv-row-for-block (block-info session-context / header current-mode material-id description item-no new-record)
  "Insert a new CSV row for an orphaned block"
  
  (setq header (nth 1 session-context))
  (setq current-mode (nth 5 session-context))
  
  ;; Extract data from block-info (format: entity-name entity-obj material-id description item-no attributes)
  (setq material-id (nth 2 block-info))
  (setq description (nth 3 block-info))
  (setq item-no (nth 4 block-info))
  
  (prompt (strcat "\n📝 Creating CSV row for orphaned block:"))
  (prompt (strcat "\n   " (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID") ": " (if material-id material-id "N/A")))
  (prompt (strcat "\n   DESCRIPTION: " (if description description "N/A")))
  (prompt (strcat "\n   ITEM NO.: " (if item-no item-no "N/A")))
  
  ;; Generate material ID if missing (orphaned blocks typically have no ID)
  (if (or (not material-id) (= material-id ""))
    (progn
      (setq material-id (generate-unique-id description))
      (MBS:log-info (strcat "\n🔧 Generated new " (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID") ": " material-id))
      
      ;; Update the block with the new ID AND set ITEM NO to TBD
      (setq entity-obj (nth 1 block-info))
      (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
      (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke entity-obj 'GetAttributes))))
      (if (not (vl-catch-all-error-p att-list))
        (progn
          (foreach att-ref att-list
            (cond
              ;; Update ID attribute
              ((= (strcase (vlax-get att-ref 'TagString)) (strcase id-attribute))
               (vlax-put att-ref 'TextString material-id)
               (prompt (strcat "\n🔗 Updated block " id-attribute " to: " material-id))
              )
              ;; Update ITEM NO attribute to TBD for user clarity
              ((= (strcase (vlax-get att-ref 'TagString)) "##")
               (vlax-put att-ref 'TextString "TBD")
               (prompt "\n🔢 Set block ITEM NO. to: TBD (will be renumbered)")
              )
            )
          )
        )
      )
    )
  )
  
  ;; Create empty CSV record with proper column count
  (setq new-record (MBS:create-empty-record header))
  
  ;; Set the generated or existing material ID
  (setq new-record (MBS:set-record-value-enhanced new-record header 
                     (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID") material-id))
  
  ;; Set description
  (if description
    (setq new-record (MBS:set-record-value-enhanced new-record header "DESCRIPTION" description))
  )
  
  ;; ITEM NO will be handled in Phase 6 (renumbering) - leave empty for now
  
  ;; Queue transaction to insert the row
  (MBS:queue-transaction (MBS:tx-insert-row new-record))
  (prompt "\n📝 Row insertion queued for transaction processing")
  
  t  ; Return success
)

(defun MBS:delete-block (block-info session-context / entity-name entity-obj success)
  "Delete a block from the drawing"
  
  (setq entity-name (nth 0 block-info))
  (setq entity-obj (nth 1 block-info))
  (setq success nil)
  
  (prompt "\n🗑️ Deleting block from drawing...")
  
  ;; Try to delete the block entity
  (if (and entity-name (not (eq entity-name nil)))
    (progn
      (if (vl-catch-all-error-p (vl-catch-all-apply 'entdel (list entity-name)))
        (prompt "\n❌ Failed to delete block")
        (progn
          (setq success t)
          (prompt "\n✅ Block deleted successfully")
        )
      )
    )
    (prompt "\n❌ Invalid block entity")
  )
  
  success
)

(defun MBS:reassociate-block (block-info matching-id session-context / entity-obj current-mode id-attribute att-list att-ref success)
  "Re-associate a block with a CSV row by updating its ID"
  
  (setq entity-obj (nth 1 block-info))
  (setq current-mode (nth 5 session-context))
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
  (setq success nil)
  
  (prompt (strcat "\n🔗 Re-associating block with " id-attribute ": " matching-id))
  
  ;; Get block attributes safely
  (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke entity-obj 'GetAttributes))))
  
  (if (not (vl-catch-all-error-p att-list))
    (progn
      ;; Find and update the ID attribute
      (foreach att-ref att-list
        (if (= (strcase (vlax-get att-ref 'TagString)) (strcase id-attribute))
          (progn
            (vlax-put att-ref 'TextString matching-id)
            (setq success t)
            (prompt (strcat "\n✅ Updated " id-attribute " to: " matching-id))
          )
        )
      )
      (if (not success)
        (prompt (strcat "\n❌ Could not find " id-attribute " attribute in block"))
      )
    )
    (prompt "\n❌ Failed to get block attributes")
  )
  
  success
)

;; NOTE: MBS:fix-missing-attributes removed - not needed per user analysis
;; Missing MATERIAL_ID/REMOVAL_ID handled in orphaned block phase
;; Missing ITEM NO handled in renumbering stage
;; Missing DESCRIPTION should be user-corrected manually

(prompt "\nResolution action functions loaded!")

;;; ========================================================================
;; TEST COMMAND FOR NEW CSV ENGINE
;;; ========================================================================

;; Test command to validate new CSV engine
(defun C:MBS-TEST-CSV (/ csv-path)
  "Test the new CSV engine with a simple operation"
  
  (setq csv-path (get-csv-path-from-block nil))
  (if (not csv-path)
    (prompt "\n❌ No CSV file found")
    (progn
      (prompt "\n🧪 [CSV ENGINE TEST]")
      (prompt (strcat "\nTesting with: " csv-path))
      
      ;; Test reading
      (prompt "\n📖 Testing CSV read...")
      (setq csv-data (CSV:read-file csv-path))
      
      (if csv-data
        (progn
          (setq header (car csv-data))
          (setq records (cadr csv-data))
          
          (prompt (strcat "\n✅ Read successful: " (itoa (length records)) " records"))
          (prompt (strcat "\n📋 Header: " (CSV:list-to-string header)))
          
          ;; Test column finding
          (prompt "\n🔍 Testing column index lookup...")
          (setq removal-id-index (CSV:get-column-index header "REMOVAL_ID"))
          (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
          
          (if (and removal-id-index desc-index)
            (progn
              (prompt "\n✅ Column lookup successful")
              
              ;; Test field access
              (if (> (length records) 0)
                (progn
                  (setq first-record (car records))
                  (setq removal-id (CSV:get-field first-record removal-id-index))
                  (setq description (CSV:get-field first-record desc-index))
                  
                  (prompt (strcat "\n📝 First record:"))
                  (prompt (strcat "\n   REMOVAL_ID[" (itoa removal-id-index) "] = '" removal-id "'"))
                  (prompt (strcat "\n   DESCRIPTION[" (itoa desc-index) "] = '" description "'"))
                  
                  (prompt "\n🧪 CSV Engine test completed successfully!")
                )
                (prompt "\n⚠ No data records to test")
              )
            )
            (prompt "\n❌ Column lookup failed")
          )
        )
        (prompt "\n❌ CSV read failed")
      )
    )
  )
  (princ)
)

(prompt "\nMBS-TEST-CSV functions loaded!")

;; Set loading flag
(setq MBS:blocks-loaded T)

;;; ========================================================================
;; MAIN ENTRY POINTS - TWO-STAGE TRANSACTIONS
;;; ========================================================================

     ;; Two-stage BATCHINSERT (using INSERT2 logic)
(defun C:MBS-BATCHINSERT3 (/ csv-path sync-blocks)
    (if (not (MBS:init-environment))
    (progn
      (prompt "\nFailed to initialize environment.")
      (princ)
    )
    (progn
      ;; Debug: Show current mode and configuration
      (prompt (strcat "\n[DEBUG] Current mode: " current-mode))
      (prompt (strcat "\n[DEBUG] ID attribute: " id-attribute))
      (prompt (strcat "\n[DEBUG] Block name: " (get-block-name)))
      
      ;; Get CSV path using your existing functions
      (setq csv-path (get-csv-path-from-block nil))
      (if (not csv-path)
      (progn
        (prompt "\nNo CSV file specified or selected.")
        (princ)
      )
      (progn
        (prompt (strcat "\n📊 [BATCH INSERT MODE – Two-stage process]"))
        (prompt (strcat "\nUsing CSV path: " csv-path))
        
        ;; STAGE 1: Material Data Insertion (using INSERT2 logic)
        (prompt "\n=== STAGE 1: MATERIAL DATA INSERTION ===")
        
        ;; Perform batch insertion using the working INSERT2 pattern
        (setq result (MBS:perform-batch-insert-v3 csv-path))
        
        ;; If successful, execute any queued transactions and move to stage 2
        (if result
        (progn
          ;; Execute any queued transactions (for generated IDs)
          (if (> (length MBS:transaction-queue) 0)
            (progn
              (prompt "\n💾 Executing transactions to save generated IDs...")
              (MBS:execute-transactions csv-path)
              (prompt "\n✅ Generated IDs saved to CSV")
            )
          )
          
          ;; STAGE 2: Item Number Update
          (prompt "\n=== STAGE 2: ITEM NUMBER UPDATE ===")
          (setq result (MBS:update-all-item-numbers csv-path))
          
          ;; Sync blocks with updated data
          (if result
          (progn
            (prompt "\nSynchronizing blocks with updated data...")
            (setq sync-blocks (MBS:sync-blocks-with-csv csv-path))
            (if sync-blocks
            (prompt "\n✓ Update process complete.")
            (prompt "\n❌ Failed to sync blocks with updated data.")
            )
          )
          )
        )
        )
        
        (princ)
      )
      )
    )
    )
  )
  (prompt "\nBatchInsert3 function loaded!")

    
    
    ;;; ========================================================================
    ;; STAGE 1 FUNCTIONS - MATERIAL DATA OPERATIONS
    ;;; ========================================================================
    
    ;; Perform single block insertion (Stage 1)
(defun MBS:perform-single-insert (csv-path / header records)
    (prompt "\nReading CSV data...")
    
    ;; Read CSV data
    (setq csv-data (MBS:read-csv csv-path))
    (if (not csv-data)
    (progn
      (prompt "\nFailed to read CSV data")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Get column indices
      (setq num-index (get-column-index header "ITEM NO."))
      (setq desc-index (get-column-index header "DESCRIPTION"))
      (setq mat-id-index (get-column-index header (get-id-column)))
      
      (if (or (not num-index) (not desc-index) (not mat-id-index))
      (progn
        (prompt "\nRequired columns not found in CSV")
        nil
      )
      (progn
        ;; Ask for selection method - WITH UNIQUE FIRST LETTERS
        (initget "Single Text Prefix Filter")
        (setq select-method (getkword "\nSelection method: (S)ingle row, (T)ext search, (P)refix filter, or advanced (F)ilter: "))
        
        (cond
        ;; Basic row selection
        ((= select-method "Single")
         (MBS:insert-single-row csv-path records header num-index desc-index mat-id-index))
        
        ;; Search by description - now using "Text" instead of "Search"
        ((= select-method "Text")
         (MBS:insert-by-search csv-path records header num-index desc-index mat-id-index))
        
        ;; Filter by prefix
        ((= select-method "Prefix")
         (MBS:insert-by-prefix csv-path records header num-index desc-index mat-id-index))
        
        ;; Advanced filtering
        ((= select-method "Filter")
         (MBS:insert-by-filter csv-path records header num-index desc-index mat-id-index))
        )
      )
      )
    )
    )
  )

  ;; Perform batch insertion using INSERT2 logic (Stage 1)
(defun MBS:perform-batch-insert-v3 (csv-path / csv-data header records result insert-count record-pairs)
    (prompt "\nReading CSV data...")
    
    ;; Read CSV data
    (setq csv-data (MBS:read-csv csv-path))
    (if (not csv-data)
    (progn
      (prompt "\nFailed to read CSV data")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Get column indices
      (setq num-index (get-column-index header "ITEM NO."))
      (setq desc-index (get-column-index header "DESCRIPTION"))
      (setq mat-id-index (get-column-index header (get-id-column)))
      
      (prompt (strcat "\n[DEBUG] Looking for ID column: " (get-id-column) " at index: " (if mat-id-index (itoa mat-id-index) "NOT FOUND")))
      
      (if (or (not num-index) (not desc-index) (not mat-id-index))
      (progn
        (prompt "\nRequired columns not found in CSV")
        nil
      )
      (progn
        ;; Create record pairs for all records (auto-select all)
        (setq record-pairs '())
        (setq row-index 0)
        (foreach record records
        (setq record-pairs (append record-pairs (list (cons row-index record))))
        (setq row-index (1+ row-index))
        )
        
        (prompt (strcat "\nAuto-selecting all " (itoa (length record-pairs)) " records for batch insertion"))
        
        ;; Use the working INSERT2 grid insertion logic
        (setq space-result (MBS:get-current-space))
        (setq target-space (car space-result))
        
        ;; Call the working grid insertion function
        (setq result (MBS:insert-multiple-grid record-pairs target-space header num-index desc-index mat-id-index))
        
        ;; Return result
        (if result
        (progn
          (setq insert-count (car result))
          (prompt (strcat "\n✓ Inserted " (itoa insert-count) " blocks"))
          t
        )
        (progn
          (prompt "\n❌ Batch insertion failed or was cancelled")
          nil
        )
        )
      )
      )
    )
    )
  )
    
    ;; Perform batch insertion (Stage 1)
(defun MBS:perform-batch-insert (csv-path / csv-data header records result insert-count)
    (prompt "\nReading CSV data...")
    
    ;; Read CSV data
    (setq csv-data (MBS:read-csv csv-path))
    (if (not csv-data)
    (progn
      (prompt "\nFailed to read CSV data")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Get column indices
      (setq num-index (get-column-index header "ITEM NO."))
      (setq desc-index (get-column-index header "DESCRIPTION"))
      (setq mat-id-index (get-column-index header (get-id-column)))
      
      (prompt (strcat "\n[DEBUG] Looking for ID column: " (get-id-column) " at index: " (if mat-id-index (itoa mat-id-index) "NOT FOUND")))
      
      (if (or (not num-index) (not desc-index) (not mat-id-index))
      (progn
        (prompt "\nRequired columns not found in CSV")
        nil
      )
      (progn
        ;; Prompt for insertion method
        (initget "All Selected Filter")
        (setq method (getkword "\nInsert (A)ll, (S)elected rows, or (F)ilter: "))
        
        (cond
        ;; Insert all records
        ((= method "All")
         (setq result (MBS:insert-material-blocks-with-fixed-spacing records header num-index desc-index mat-id-index csv-path))
        )
        
        ;; Insert selected records
        ((= method "Selected")
         (setq selected-records (MBS:prompt-for-row-selection records header))
         (if selected-records
           (setq result (MBS:insert-material-blocks-with-fixed-spacing selected-records header num-index desc-index mat-id-index csv-path))
           (setq result nil)
         )
        )
        
        ;; Filter records
        ((= method "Filter")
         (setq filtered-records (MBS:prompt-for-filtered-rows records header))
         (if filtered-records
           (setq result (MBS:insert-material-blocks-with-fixed-spacing filtered-records header num-index desc-index mat-id-index csv-path))
           (setq result nil)
         )
        )
        )
        
        ;; Return result
        (if result
        (progn
          (setq insert-count (car result))
          (prompt (strcat "\n✓ Inserted " (itoa insert-count) " blocks"))
          t
        )
        (progn
          (prompt "\n❌ Batch insertion failed or was cancelled")
          nil
        )
        )
      )
      )
    )
    )
  )
    
  ;; Process material updates (Stage 1 of update)
(defun MBS:process-material-updates (csv-path / csv-data header records blocks-inserted error-occurred changes-made)
    (MBS:clear-transactions)
    (prompt "\nProcessing material data updates...")
    
    ;; Error tracking
    (setq error-occurred nil)
    (setq changes-made nil)
    
    ;; Read CSV data
    (setq csv-data (MBS:read-csv csv-path))
    (if (not csv-data)
    (progn
      (prompt "\nFailed to read CSV data")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Get column indices
      (setq num-index (get-column-index header "ITEM NO."))
      (setq desc-index (get-column-index header "DESCRIPTION"))
      (setq mat-id-index (get-column-index header (get-id-column)))
      
      (prompt (strcat "\nFound Material ID column at index: " (itoa mat-id-index)))
      
      ;; Handle orphaned rows (CSV rows with no matching blocks) - with error handling
      (vl-catch-all-apply
      '(lambda ()
        (setq map-result (MBS:build-material-id-map))
        (setq id-map (car map-result))
        (setq empty-id-blocks (cadr map-result))
        
        (setq orphan-rows (MBS:find-orphaned-rows id-map records header mat-id-index))
        (if (> (length orphan-rows) 0)
        (progn
          (setq blocks-inserted (MBS:process-orphaned-rows-simplified orphan-rows records header))
          
          ;; Check if there are transactions to execute after handling orphaned rows
          (if (> (length MBS:transaction-queue) 0)
          (progn
            (prompt "\n\nMaterial deletions requested. Applying changes...")
            (prompt (strcat "\nExecuting " (itoa (length MBS:transaction-queue)) " deletion transactions"))
            (setq csv-updated (MBS:execute-material-transactions csv-path))
            (if csv-updated
            (progn
              (prompt "\n✓ Material data changes applied successfully.")
              (setq changes-made t)
            )
            (prompt "\n❌ Failed to apply material data changes.")
            )
          )
          )
        )
        )
      )
      )
      
      ;; Handle orphaned blocks with error handling
      (vl-catch-all-apply
      '(lambda ()
        (setq orphan-blocks (MBS:find-orphaned-blocks id-map records header mat-id-index))
        (if (> (length orphan-blocks) 0)
        (MBS:process-orphaned-blocks-simplified orphan-blocks records header)
        )
      )
      )
      
      ;; Handle blocks with empty Material IDs with error handling
      (setq empty-result
      (vl-catch-all-apply
        '(lambda ()
        (if (and (boundp 'empty-id-blocks) (> (length empty-id-blocks) 0))
          (MBS:process-empty-id-blocks-simplified empty-id-blocks records header)
        )
        )
      )
      )
      
      ;; Check if error occurred
      (if (vl-catch-all-error-p empty-result)
      (progn
        (prompt "\nWARNING: Error occurred while processing blocks with empty IDs")
        (prompt (strcat "\nError: " (vl-catch-all-error-message empty-result)))
        (setq error-occurred t)
      )
      )
      
      ;; Only continue with other operations if no errors occurred
      (if (not error-occurred)
      (progn
        ;; Build update transaction queue for normal blocks
        (setq blocks-to-update (MBS:build-material-updates id-map records header))
        
        ;; Check if we have additional transactions to execute
        (if (> (length MBS:transaction-queue) 0)
        (progn
          ;; Show transaction preview 
          (if (MBS:preview-material-transactions csv-path)
          (progn
            ;; Execute transactions
            (prompt "\nApplying data updates...")
            (setq csv-updated (MBS:execute-material-transactions csv-path))
            (if csv-updated
            (setq changes-made t)
            )
            (or csv-updated blocks-inserted)  ;; Return success if either worked
          )
          (progn
            (prompt "\n❌ Operation cancelled by user.")
            nil
          )
          )
        )
        (progn
          ;; No transactions but we might have inserted blocks directly or made changes earlier
          (if (or blocks-inserted changes-made)
          (progn
            (prompt "\nNo additional material data updates needed.")
            t  ;; Return success - we inserted blocks directly or made changes
          )
          (progn
            (prompt "\nNo material data updates needed.")
            t  ;; Return success - continue to Stage 2
          )
          )
        )
        )
      )
      (progn
        (prompt "\n❌ Error occurred during material updates. See warnings above.")
        nil
      )
      )
    )
    )
  )
    
    ;;; ========================================================================
    ;; STAGE 2 FUNCTIONS - ITEM NUMBER OPERATIONS
    ;;; ========================================================================
    
  ;; Update the function that builds and displays the preview text for item number changes
(defun MBS:update-all-item-numbers (csv-path / csv-data header records modified)
    (prompt "\nUpdating item numbers...")
    
    ;; Check if auto-numbering is enabled
    (if (not auto-increment-item-numbers)
    (progn
      (prompt "\nAuto-numbering is disabled. Keeping existing item numbers.")
      nil  ;; Return - no changes made
    )
    (progn
      ;; Read current CSV data
      (setq csv-data (MBS:read-csv csv-path))
      (if (not csv-data)
      (progn
        (prompt "\nFailed to read CSV for item number updates.")
        nil
      )
      (progn
        (setq header (car csv-data))
        (setq records (cadr csv-data))
        (setq modified nil)
        
        ;; Get column indices with error checking
        (setq num-index (get-column-index header "ITEM NO."))
        (setq desc-index (get-column-index header "DESCRIPTION"))
        
        (if (or (not num-index) (not desc-index))
        (progn
          (prompt "\nERROR: Required columns not found. Cannot update item numbers.")
          nil
        )
        (progn
          ;; Group records by prefix with error handling
          (setq prefix-groups (vl-catch-all-apply 
                    '(lambda () (MBS:group-records-by-prefix records num-index desc-index))))
          
          ;; Check for error in grouping
          (if (vl-catch-all-error-p prefix-groups)
          (progn
            (prompt "\nERROR: Failed to group records by prefix.")
            nil
          )
          (progn
            ;; SIMPLIFIED: Just count items that will be renumbered (no detailed preview)
            (setq total-items-to-update 0)
            (setq prefixes-with-changes '())
            
            ;; Count items per prefix group
            (foreach prefix-group prefix-groups
            (setq prefix (car prefix-group))
            (setq group-records (cdr prefix-group))
            (setq group-count (length group-records))
            
            (if (> group-count 0)
              (progn
                (setq total-items-to-update (+ total-items-to-update group-count))
                (setq prefixes-with-changes (append prefixes-with-changes (list (cons prefix group-count))))
              )
            )
            )
            
            ;; Show simple summary
            (prompt (strcat "\n📋 Item numbering summary: " (itoa total-items-to-update) " items will be updated"))
            (foreach prefix-info prefixes-with-changes
              (setq prefix (car prefix-info))
              (setq count (cdr prefix-info))
              (prompt (strcat "  • Prefix '" prefix "': " (itoa count) " items → " prefix "1, " prefix "2, ... " prefix (itoa count)))
            )
            
            (setq any-changes (> total-items-to-update 0))
            
            ;; Check AUTO ACCEPT setting for item number changes
            (if any-changes
              (progn
                (if MBS:auto-accept-item-number-changes
                  (progn
                    (prompt "\n✅ Auto-applying item number updates...")
                    (setq response "Y")
                  )
                  (progn
                    (prompt "\n🔍 === PREVIEW OF ITEM NUMBER CHANGES ===")
                    (prompt "\nThe following item numbers will be updated:")
                    
                    ;; Show detailed preview of each record that will be changed
                    (setq preview-count 0)
                    (setq changed-records 0)
                    
                    ;; First pass: determine what the new item numbers will be
                    (setq new-item-numbers '())
                    (setq record-index 1)
                    (foreach prefix-group prefix-groups
                      (setq prefix (car prefix-group))
                      (setq group-records (cdr prefix-group))
                      (setq group-count 1)
                      
                      (foreach record group-records
                        (setq current-item-no (nth num-index record))
                        (setq new-item-no (strcat prefix (itoa group-count)))
                        
                        ;; Store the new item number for this record
                        (setq new-item-numbers (append new-item-numbers (list (cons record-index new-item-no))))
                        (setq group-count (1+ group-count))
                        (setq record-index (1+ record-index))
                      )
                    )
                    
                    ;; Second pass: show only records that will actually change
                    (setq record-index 1)
                    (foreach record records
                      (setq current-item-no (nth num-index record))
                      (setq description (nth desc-index record))
                      (setq new-item-no (cdr (assoc record-index new-item-numbers)))
                      
                      ;; Only show records where ITEM NO will actually change
                      (if (and description new-item-no)
                        (if (or (null current-item-no) 
                                (= current-item-no "") 
                                (/= current-item-no new-item-no))
                          (progn
                            (setq changed-records (1+ changed-records))
                            (prompt (strcat "\n  Record " (itoa changed-records) ":"))
                            (prompt (strcat "\n    DESCRIPTION: " description))
                            (if (or (null current-item-no) (= current-item-no ""))
                              (prompt (strcat "\n    ITEM NO: [blank] → " new-item-no))
                              (prompt (strcat "\n    ITEM NO: " current-item-no " → " new-item-no))
                            )
                            (prompt "\n")
                          )
                        )
                      )
                      (setq record-index (1+ record-index))
                    )
                    
                    (if (> changed-records 0)
                      (progn
                        (prompt (strcat "\n📋 Summary: " (itoa changed-records) " records will have ITEM NO changes"))
                        (foreach prefix-info prefix-summary
                          (setq prefix (car prefix-info))
                          (setq count (cdr prefix-info))
                          (prompt (strcat "\n  • Prefix '" prefix "': " (itoa count) " items → " prefix "1, " prefix "2, ... " prefix (itoa count)))
                        )
                        
                        (initget "Yes No")
                        (setq response (getkword "\nProceed with item number updates? (Y)es/(N)o: "))
                      )
                      (progn
                        (prompt "\n✅ No ITEM NO changes needed - all items are already correctly numbered")
                        (setq response "Y")  ;; Auto-proceed when no changes needed
                      )
                    )
                  )
                )
              )
              (progn
                (prompt "\n✅ No item number updates needed")
                (setq response "Y")
              )
            )
            
            ;; Process the response
            (prompt (strcat "\n🔍 DEBUG: Response received: '" response "'"))
            (if (or (= response "Y") (= response "Yes"))
            (progn
              (prompt "\n✅ Proceeding with item number updates...")
              ;; Apply all renumbering
              ;; (prompt (strcat "\n🔍 DEBUG: Starting renumbering with " (itoa (length records)) " records"))
              (foreach prefix-group prefix-groups
                (setq prefix (car prefix-group))
                (setq group-records (cdr prefix-group))
                
                ;; Apply renumbering (will always modify since we're forcing it)
                ;; (prompt (strcat "\n🔍 DEBUG: Before renumbering prefix '" prefix "': " (itoa (length records)) " total records"))
                (setq result (MBS:apply-force-renumber-prefix-group 
                        group-records records header num-index prefix))
                (setq records (car result))
                ;; (prompt (strcat "\n🔍 DEBUG: After renumbering prefix '" prefix "': " (itoa (length records)) " total records"))
                (if (cadr result) (setq modified t))
              )
              ;; (prompt (strcat "\n🔍 DEBUG: Renumbering complete. Final record count: " (itoa (length records))))
              
              ;; Sort the CSV
              ;; (prompt (strcat "\n🔍 DEBUG: Before sorting: " (itoa (length records)) " records"))
              (setq records (MBS:sort-records-by-item-no records num-index))
              ;; (prompt (strcat "\n🔍 DEBUG: After sorting: " (itoa (length records)) " records"))
              
              ;; CRITICAL DEBUGGING: Check records before final write
              ;; (prompt (strcat "\n🔍 DEBUG: About to write " (itoa (length records)) " records to CSV"))
              ;; (prompt (strcat "\n🔍 DEBUG: Header length: " (itoa (length header))))
              (if (> (length records) 0)
                ;; (prompt (strcat "\n🔍 DEBUG: First record length: " (itoa (length (car records)))))
                 ;;(prompt "\n✅ Records ready for CSV write")
                (MBS:log-error "\n⚠ CRITICAL: No records to write!")
              )
                
              ;; FINAL VALIDATION: Ensure no data loss before writing CSV
              (if (< (length records) 1)
                (progn
                  (MBS:log-error "\n⚠ CRITICAL WARNING: No records remaining! Aborting CSV write to prevent data loss.")
                  (setq modified nil)  ;; Prevent write
                )
              )
              
              ;; Write changes back to CSV (skip backup since Stage 1 already created one)
              (if modified
                (progn
                  ;;(prompt "\n🔄 Starting CSV write operation...")
                  (if (MBS:write-csv csv-path header records t)  ;; t = skip backup
                    (progn
                      (prompt (strcat "\n✅ " (itoa total-items-to-update) " item numbers updated successfully!"))
                      t  ;; Return success
                    )
                    (progn
                      (prompt "\n❌ Failed to write updated item numbers to CSV.")
                      nil
                    )
                  )
                )
                (progn
                  (prompt "\n✅ No item number changes needed.")
                  t  ;; Return success - no changes needed
                )
              )
            )
            (progn
              (prompt "\n❌ Item number updates cancelled.")
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
    )
  )
  
  (prompt "\nTwo stage functions loaded!")
    
    ;;; ========================================================================
    ;; BLOCK SYNCHRONIZATION FUNCTIONS
    ;;; ========================================================================
    
    ;; Sync blocks with CSV data
(defun MBS:sync-blocks-with-csv (csv-path / csv-data header records block-map)
    (prompt "\nSynchronizing blocks with updated CSV data...")
    
    ;; Read CSV data using MBS:read-csv
    (setq csv-data (MBS:read-csv csv-path))
    (if (not csv-data)
    (progn
      (prompt "\nFailed to read CSV data")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Get column indices
      (setq num-index (get-column-index header "ITEM NO."))
      (setq desc-index (get-column-index header "DESCRIPTION"))
      (setq mat-id-index (get-column-index header (get-id-column)))
      
      (if (or (not num-index) (not desc-index) (not mat-id-index))
      (progn
        (prompt "\nRequired columns not found in CSV")
        nil
      )
      (progn
        ;; Get all blocks with material IDs
        (setq block-map (MBS:get-blocks-with-ids))
        
        ;; Update blocks with CSV data
        (MBS:update-blocks-from-csv block-map records header num-index desc-index mat-id-index)
        
        t
      )
      )
    )
    )
  )
    
    ;; Update blocks safely after changes
(defun MBS:update-blocks-safely (csv-path / csv-data header records)
    (prompt "\n⏳ Auto-update is ON. Syncing block attributes with CSV...")
    (command "_.regen")
    
    ;; Read latest CSV data
    (setq csv-data (MBS:read-csv csv-path))
    (if csv-data
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Get column indices
      (setq num-index (get-column-index header "ITEM NO."))
      (setq desc-index (get-column-index header "DESCRIPTION"))
      (setq mat-id-index (get-column-index header (get-id-column)))
      
      (if (not mat-id-index)
      (progn
        (MBS:log-warning (strcat "Material ID column '" id-attribute "' not found"))
        nil
      )
      (progn
        ;; Build material ID map from blocks - FIXED: Get only the map part
        (setq map-result (MBS:build-material-id-map))
        (setq id-map (car map-result))  ;; Get just the map, not empty-id-blocks
        
        ;; Update blocks with latest values
        (setq blocks-updated 0)
        
        ;; For each record with a material ID, update the corresponding blocks
        (foreach record records
        (if (and mat-id-index 
            (>= (length record) (1+ mat-id-index)) 
            (/= (nth mat-id-index record) ""))
          (progn
          (setq material-id (nth mat-id-index record))
          
          ;; Find blocks with this material ID - FIXED: Added safety check
          (setq blocks-entry (assoc material-id id-map))
          (setq blocks (if blocks-entry (cdr blocks-entry) nil))
          
          (if blocks
            (progn
            ;; Get values to update
            (setq item-no (if (and num-index (>= (length record) (1+ num-index)))
                    (nth num-index record)
                    ""))
                    
            (setq desc (if (and desc-index (>= (length record) (1+ desc-index)))
                   (nth desc-index record)
                   ""))
            
            ;; Update each block
            (foreach blk blocks
              (if (and blk (= (type blk) 'VLA-OBJECT))
              (vl-catch-all-apply 
                '(lambda () 
                ;; Check if object is valid
                (vlax-get-property blk 'ObjectName)
                
                ;; Update attributes
                (setq att-list (vlax-invoke blk 'GetAttributes))
                (foreach att att-list
                  (setq tag (strcase (vlax-get att 'TagString)))
                  (cond
                  ((= tag "##")
                   (if (/= item-no "")
                     (vlax-put att 'TextString item-no)))
                  ((= tag "DESCRIPTION")
                   (if (/= desc "")
                     (vlax-put att 'TextString (shorten-description desc))))
                  )
                )
                (vla-Update blk)
                (setq blocks-updated (1+ blocks-updated))
                )
              )
              )
            )
            )
          )
          )
        )
        )
        
        (MBS:log-info (strcat "Updated " (itoa blocks-updated) " blocks with latest CSV data"))
        (prompt (strcat "\n✓ Updated " (itoa blocks-updated) " blocks with new values"))
      )
      )
    )
    (MBS:log-critical "Failed to read CSV for block updates")
    )
  )
    
    ;;; ========================================================================
    ;; SUPPORT FUNCTIONS FOR SINGLE INSERTION
    ;;; ========================================================================
    
    ;; Insert by searching description
(defun MBS:insert-by-search (csv-path records header num-index desc-index mat-id-index)
    ;; Prompt for search term
    (prompt "\nEnter search text to find in descriptions: ")
    (setq search-text (strcase (getstring)))
    
    (if (= search-text "")
    (progn
      (prompt "\nNo search text entered.")
      nil
    )
    (progn
      ;; Find matching records
      (setq matches '())
      (setq i 0)
      (foreach record records
      (if (and (>= (length record) (1+ desc-index))
          (vl-string-search search-text (strcase (nth desc-index record))))
        (setq matches (append matches (list (cons i record))))
      )
      (setq i (1+ i))
      )
      
      ;; Show matches
      (if (= (length matches) 0)
      (progn
        (prompt "\nNo matches found.")
        nil
      )
      (progn
        (prompt (strcat "\nFound " (itoa (length matches)) " matching items:"))
        (setq j 1)
        (foreach match matches
        (setq record (cdr match))
        (prompt (strcat "\n" (itoa j) ": " 
                 (nth num-index record) " - " 
                 (nth desc-index record)))
        (setq j (1+ j))
        )
        
        ;; Prompt for selection
        (prompt "\nEnter item number(s) to insert (comma-separated, or A for all): ")
        (setq selection (getstring))
        
        ;; Process selection
        (if (= (strcase selection) "A")
        ;; Insert all matches
        (MBS:insert-multiple-records matches header num-index desc-index mat-id-index)
        ;; Insert selected matches
        (progn
          (setq indices (MBS:parse-number-list selection))
          (if (not indices)
          (progn
            (prompt "\nInvalid selection.")
            nil
          )
          (progn
            ;; Extract selected records
            (setq selected-matches '())
            (foreach idx indices
            (if (and (> idx 0) (<= idx (length matches)))
              (setq selected-matches (append selected-matches (list (nth (1- idx) matches))))
            )
            )
            
            ;; Insert selected matches
            (MBS:insert-multiple-records selected-matches header num-index desc-index mat-id-index)
          )
          )
        )
        )
      )
      )
    )
    )
  )
    
    ;; Insert by prefix filter
(defun MBS:insert-by-prefix (csv-path records header num-index desc-index mat-id-index)
    ;; Get available prefixes
    (setq prefixes (MBS:get-available-prefixes-from-records records header num-index))
    
    (if (= (length prefixes) 0)
    (progn
      (prompt "\nNo prefixes found in the CSV data.")
      nil
    )
    (progn
      ;; Show available prefixes
      (prompt "\nAvailable prefixes:")
      (setq i 1)
      (foreach prefix prefixes
      (setq prefix-desc (MBS:get-prefix-descriptor prefix))
      (prompt (strcat "\n" (itoa i) ": " prefix " - " prefix-desc))
      (setq i (1+ i))
      )
      
      ;; Prompt for selection
      (prompt "\nSelect prefix number: ")
      (setq prefix-num (getint))
      
      (if (or (not prefix-num) (< prefix-num 1) (> prefix-num (length prefixes)))
      (progn
        (prompt "\nInvalid selection.")
        nil
      )
      (progn
        ;; Get selected prefix
        (setq selected-prefix (nth (1- prefix-num) prefixes))
        
        ;; Find matching records
        (setq matches '())
        (setq i 0)
        (foreach record records
        (if (>= (length record) (1+ num-index))
          (progn
          (setq item-no (nth num-index record))
          (if (and item-no (/= item-no "")
               (>= (strlen item-no) (strlen selected-prefix))
               (= (substr item-no 1 (strlen selected-prefix)) selected-prefix))
            (setq matches (append matches (list (cons i record))))
          )
          )
        )
        (setq i (1+ i))
        )
        
        ;; Show matches
        (if (= (length matches) 0)
        (progn
          (prompt (strcat "\nNo items found with prefix '" selected-prefix "'."))
          nil
        )
        (progn
          (prompt (strcat "\nFound " (itoa (length matches)) " items with prefix '" selected-prefix "':"))
          (setq j 1)
          (foreach match matches
          (setq record (cdr match))
          (prompt (strcat "\n" (itoa j) ": " 
                   (nth num-index record) " - " 
                   (nth desc-index record)))
          (setq j (1+ j))
          )
          
          ;; Prompt for selection
          (prompt "\nEnter item number(s) to insert (comma-separated, or A for all): ")
          (setq selection (getstring))
          
          ;; Process selection
          (if (= (strcase selection) "A")
          ;; Insert all matches
          (MBS:insert-multiple-records matches header num-index desc-index mat-id-index)
          ;; Insert selected matches
          (progn
            (setq indices (MBS:parse-number-list selection))
            (if (not indices)
            (progn
              (prompt "\nInvalid selection.")
              nil
            )
            (progn
              ;; Extract selected records
              (setq selected-matches '())
              (foreach idx indices
              (if (and (> idx 0) (<= idx (length matches)))
                (setq selected-matches (append selected-matches (list (nth (1- idx) matches))))
              )
              )
              
              ;; Insert selected matches
              (MBS:insert-multiple-records selected-matches header num-index desc-index mat-id-index)
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
  )

    
    ;; Insert by advanced filtering
(defun MBS:insert-by-filter (csv-path records header num-index desc-index mat-id-index)
    ;; Show available columns
    (prompt "\nAvailable columns for filtering:")
    (setq i 1)
    (foreach col header
    (prompt (strcat "\n" (itoa i) ": " col))
    (setq i (1+ i))
    )
    
    ;; Prompt for column selection
    (prompt "\nSelect column number to filter on: ")
    (setq col-num (getint))
    
    (if (or (not col-num) (< col-num 1) (> col-num (length header)))
    (progn
      (prompt "\nInvalid column selection.")
      nil
    )
    (progn
      ;; Get column index
      (setq filter-col-idx (1- col-num))
      (setq filter-col-name (nth filter-col-idx header))
      
      ;; Prompt for filter value
      (prompt (strcat "\nEnter filter value for column '" filter-col-name "' (empty for any): "))
      (setq filter-val (getstring))
      
      ;; Find matching records
      (setq matches '())
      (setq i 0)
      (foreach record records
      (if (>= (length record) (1+ filter-col-idx))
        (progn
        (setq record-val (nth filter-col-idx record))
        (if (or (= filter-val "")
            (= (strcase record-val) (strcase filter-val)))
          (setq matches (append matches (list (cons i record))))
        )
        )
      )
      (setq i (1+ i))
      )
      
      ;; Show matches
      (if (= (length matches) 0)
      (progn
        (prompt "\nNo matching records found.")
        nil
      )
      (progn
        (prompt (strcat "\nFound " (itoa (length matches)) " matching items:"))
        (setq j 1)
        (foreach match matches
        (setq record (cdr match))
        (prompt (strcat "\n" (itoa j) ": " 
                 (nth num-index record) " - " 
                 (nth desc-index record)))
        (setq j (1+ j))
        )
        
        ;; Prompt for selection
        (prompt "\nEnter item number(s) to insert (comma-separated, or A for all): ")
        (setq selection (getstring))
        
        ;; Process selection
        (if (= (strcase selection) "A")
        ;; Insert all matches
        (MBS:insert-multiple-records matches header num-index desc-index mat-id-index)
        ;; Insert selected matches
        (progn
          (setq indices (MBS:parse-number-list selection))
          (if (not indices)
          (progn
            (prompt "\nInvalid selection.")
            nil
          )
          (progn
            ;; Extract selected records
            (setq selected-matches '())
            (foreach idx indices
            (if (and (> idx 0) (<= idx (length matches)))
              (setq selected-matches (append selected-matches (list (nth (1- idx) matches))))
            )
            )
            
            ;; Insert selected matches
            (MBS:insert-multiple-records selected-matches header num-index desc-index mat-id-index)
          )
          )
        )
        )
      )
      )
    )
    )
  )
    
    ;; Insert single row
(defun MBS:insert-single-row (csv-path records header num-index desc-index mat-id-index)
    ;; Prompt for row selection
    (setq selected-row (MBS:prompt-for-single-row-selection records header))
    (if (not selected-row)
    (progn
      (prompt "\nNo row selected.")
      nil
    )
    (progn
      ;; Extract data from selected row
      (setq material-id (nth mat-id-index selected-row))
      (setq desc (nth desc-index selected-row))
      (setq item-no (nth num-index selected-row))
      
      ;; Insert block
      (prompt (strcat "\nInserting block for item: " item-no " - " desc))
      
      ;; Automatically determine the current space
      (setq space-result (MBS:get-current-space))
      (setq target-space (car space-result))
      (setq space-name (cadr space-result))
      
      ;; Inform user of the current space being used
      (prompt (strcat "\nBlock will be inserted into Current Layout (" space-name "space)"))
      
      ;; Prompt for insertion point
      (prompt "\nSpecify insertion point: ")
      (setq ins-pt (getpoint))
      
      (if ins-pt
      (progn
        ;; Insert block
        (setq block (MBS:insert-block-with-attributes 
               target-space ins-pt material-id desc item-no))
        
        (if block
        (progn
          (prompt "\n✓ Block inserted successfully.")
          t
        )
        (progn
          (prompt "\n❌ Failed to insert block.")
          nil
        )
        )
      )
      (progn
        (prompt "\nNo insertion point specified.")
        nil
      )
      )
    )
    )
  )
    
    ;;; ========================================================================
    ;; SUPPORT FUNCTIONS FOR BATCH INSERTION
    ;;; ========================================================================
    
  ;; Parse space-separated numbers
(defun MBS:parse-numbers (input / result num start i)
    (setq result '())
    (setq input (strcat input " ")) ;; Add space at end to simplify parsing
    
    (setq start 1)
    (setq i 1)
    (while (<= i (strlen input))
    (if (= (substr input i 1) " ")
      (progn
      (if (< start i)
        (progn
        (setq num (atoi (substr input start (- i start))))
        (if (> num 0)
          (setq result (append result (list num)))
        )
        )
      )
      (setq start (1+ i))
      )
    )
    (setq i (1+ i))
    )
    
    result
  )
  
  ;; Prompt for multiple row selection
(defun MBS:prompt-for-row-selection (records header / num-index desc-index mat-id-index selected-rows)
    ;; Get indices
    (setq num-index (get-column-index header "ITEM NO."))
    (setq desc-index (get-column-index header "DESCRIPTION"))
    (setq mat-id-index (get-column-index header (strcase id-attribute)))
    
    ;; Display rows
    (prompt "\n\n=== SELECT ROWS TO INSERT ===")
    (setq i 1)
    (foreach record records
    (prompt (strcat "\n" (itoa i) ": " 
             (nth num-index record) " - " 
             (nth desc-index record) " - " 
             (nth mat-id-index record)))
    (setq i (1+ i))
    )
    
    ;; Prompt for selection
    (prompt "\n\nEnter row numbers to insert, separated by spaces (or 0 to cancel): ")
    (setq input (getstring))
    
    (if (= input "0")
    nil
    (progn
      ;; Parse input
      (setq row-nums (MBS:parse-numbers input))
      
      ;; Select rows
      (setq selected-rows '())
      (foreach row-num row-nums
      (if (and (> row-num 0) (<= row-num (length records)))
        (setq selected-rows (append selected-rows (list (nth (1- row-num) records))))
      )
      )
      
      selected-rows
    )
    )
  )
  
  ;; Get all blocks with material IDs - filtered by current mode's block type
(defun MBS:get-blocks-with-ids (/ doc block-map block-name)
    (prompt "\nFinding blocks with material IDs...")
    
    (setq block-map '())
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    (setq block-name (get-block-name))  ;; Get the correct block type for current mode
    
    (prompt (strcat "\n[DEBUG] Looking for blocks of type: " block-name))
    
    ;; Check modelspace
    (vlax-for ent (vla-get-ModelSpace doc)
    (if (and (= (vla-get-ObjectName ent) "AcDbBlockReference")
         (= (vlax-get ent 'EffectiveName) block-name))  ;; Filter by block type
      (progn
      ;; Try to get attributes
      (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke ent 'GetAttributes))))
      
      ;; Check if we got attributes successfully
      (if (not (vl-catch-all-error-p att-list))
        (progn
        ;; Check for material ID attribute
        (setq found-id nil)
        (foreach att att-list
          (if (= (strcase (vlax-get att 'TagString)) (strcase id-attribute))
          (progn
            (setq material-id (vlax-get att 'TextString))
            (if (and material-id (/= material-id ""))
            (progn
              (setq block-map (cons (cons material-id ent) block-map))
              (setq found-id t)
            )
            )
          )
          )
        )
        
        ;; If not found, check alternate ID
        (if (not found-id)
          (foreach att att-list
          (if (= (strcase (vlax-get att 'TagString)) (strcase alternate-id-attribute))
            (progn
            (setq removal-id (vlax-get att 'TextString))
            (if (and removal-id (/= removal-id ""))
              (setq block-map (cons (cons removal-id ent) block-map))
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
    
    ;; Check paperspace
    (vlax-for ent (vla-get-PaperSpace doc)
    (if (and (= (vla-get-ObjectName ent) "AcDbBlockReference")
         (= (vlax-get ent 'EffectiveName) block-name))  ;; Filter by block type
      (progn
      ;; Try to get attributes
      (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke ent 'GetAttributes))))
      
      ;; Check if we got attributes successfully
      (if (not (vl-catch-all-error-p att-list))
        (progn
        ;; Check for material ID attribute
        (setq found-id nil)
        (foreach att att-list
          (if (= (strcase (vlax-get att 'TagString)) (strcase id-attribute))
          (progn
            (setq material-id (vlax-get att 'TextString))
            (if (and material-id (/= material-id ""))
            (progn
              (setq block-map (cons (cons material-id ent) block-map))
              (setq found-id t)
            )
            )
          )
          )
        )
        
        ;; If not found, check alternate ID
        (if (not found-id)
          (foreach att att-list
          (if (= (strcase (vlax-get att 'TagString)) (strcase alternate-id-attribute))
            (progn
            (setq removal-id (vlax-get att 'TextString))
            (if (and removal-id (/= removal-id ""))
              (setq block-map (cons (cons removal-id ent) block-map))
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
    
    (prompt (strcat "\nFound " (itoa (length block-map)) " blocks with material IDs."))
    block-map
  )
  
  
    ;; Insert material blocks with fixed spacing
(defun MBS:insert-material-blocks-with-fixed-spacing (rows header num-index desc-index mat-id-index csv-path / count)
    (setq count 0)
    
    ;; Clear any existing transactions from previous runs
    (MBS:clear-transactions)
    
    ;; Automatically determine the current space
    (setq space-result (MBS:get-current-space))
    (setq target-space (car space-result))
    (setq space-name (cadr space-result))
    
    ;; Inform user of the current space being used
    (prompt (strcat "\nBlocks will be inserted into Current Layout (" space-name "space)"))
    
    ;; Determine if we're in metric or imperial units
    (setq drawing-units (getvar "INSUNITS"))
    (setq is-metric nil)
    (if (member drawing-units '(4 5 6 7 12 13 14 15 16 17))
    (setq is-metric T)
    )
    
    ;; Set fixed spacing based on units
    (if is-metric
    (progn
      (setq x-spacing 110.0)  ;; 110mm for metric
      (setq y-spacing 20.0)   ;; 20mm for metric
      (prompt "\n[METRIC] Using spacing: 110mm x 20mm")
    )
    (progn
      (setq x-spacing 4.0)    ;; 4" for imperial
      (setq y-spacing 0.75)   ;; 0.75" for imperial
      (prompt "\n[IMPERIAL] Using spacing: 4\" x 0.75\"")
    )
    )
    
    ;; Prompt for base point
    (prompt "\nSpecify base insertion point: ")
    (setq base-pt (getpoint))
    
    ;; Prompt for grid arrangement
    (prompt "\nEnter number of blocks per row: ")
    (setq blocks-per-row (getint))
    (if (or (not blocks-per-row) (< blocks-per-row 1))
    (setq blocks-per-row 5)  ;; Default to 5 per row
    )
    
    (if base-pt
    (progn
      ;; Start insertion
      (setq row-num 0)
      (setq col-num 0)
      
      (setq row-index 0)  ;; Track row index for CSV updates
      
      (foreach record rows
      ;; Extract data
      (setq material-id (nth mat-id-index record))
      (setq desc (nth desc-index record))
      (setq item-no (nth num-index record))
      
      ;; Generate new ID if empty (for batch insert) 
      (setq id-was-generated nil)
      (if (or (not material-id) (= material-id "") (= (strcase material-id) "NIL"))
        (progn
        (setq material-id (generate-unique-id desc))
        (setq id-was-generated t)
        )
      )
      
      ;; Calculate insertion point
      (setq ins-x (+ (car base-pt) (* col-num x-spacing)))
      (setq ins-y (- (cadr base-pt) (* row-num y-spacing)))
      (setq ins-pt (list ins-x ins-y))
      
      ;; Insert block
      (setq block (MBS:insert-block-with-attributes 
            target-space ins-pt material-id desc item-no))
      
      (if block
        (progn
        (setq count (1+ count))
        ;; If we generated a new ID, queue a transaction to update the CSV
        (if id-was-generated
          (MBS:queue-transaction (MBS:tx-update-field row-index (get-id-column) material-id))
        )
        )
      )
      
      (setq row-index (1+ row-index))  ;; Increment row index
      
      ;; Update grid position
      (setq col-num (1+ col-num))
      (if (>= col-num blocks-per-row)
        (progn
        (setq col-num 0)
        (setq row-num (1+ row-num))
        )
      )
      )
      
      ;; Execute queued transactions to update CSV with generated IDs
      (if (> (length MBS:transaction-queue) 0)
      (progn
        (prompt (strcat "\nUpdating CSV with " (itoa (length MBS:transaction-queue)) " generated " id-attribute "s..."))
        (if (MBS:execute-material-transactions csv-path)
        (prompt "\n✓ CSV updated with generated IDs")
        (prompt "\n❌ Failed to update CSV with generated IDs")
        )
      )
      )
      
      (list count)
    )
    nil
    )
  )
    
    ;; Insert multiple records
(defun MBS:insert-multiple-records (record-pairs header num-index desc-index mat-id-index / result csv-path)
    ;; Automatically determine the current space
    (setq space-result (MBS:get-current-space))
    (setq target-space (car space-result))
    (setq space-name (cadr space-result))
    
    ;; Inform user of the current space being used
    (prompt (strcat "\nBlock(s) will be inserted into Current Layout (" space-name "space)"))
    
    ;; Determine insert mode
    (initget "Single Grid")
    (setq insert-mode (getkword "\nInsert as (S)ingle points or (G)rid layout? "))
    
    (if (= insert-mode "Single")
    ;; Insert each block individually
    (setq result (MBS:insert-multiple-single record-pairs target-space header num-index desc-index mat-id-index))
    ;; Insert as grid
    (setq result (MBS:insert-multiple-grid record-pairs target-space header num-index desc-index mat-id-index))
    )
    
    ;; Execute queued transactions if any blocks were inserted
    (if (and result (> (length MBS:transaction-queue) 0))
    (progn
      ;; Get CSV path for transaction execution
      (setq csv-path (get-csv-path-from-block nil))
      (if csv-path
      (progn
        (prompt (strcat "\nUpdating CSV with " (itoa (length MBS:transaction-queue)) " generated " id-attribute "s..."))
        (prompt (strcat "\n[DEBUG] Queue contents before execution:"))
        (foreach tx MBS:transaction-queue
        (prompt (strcat "\n[DEBUG] Transaction: " (vl-princ-to-string tx)))
        )
        (if (MBS:execute-material-transactions csv-path)
        (prompt "\n✓ CSV updated with generated IDs")
        (prompt "\n❌ Failed to update CSV with generated IDs")
        )
      )
      (prompt "\n⚠ No CSV path found for transaction execution")
      )
    )
    )
    
    result
  )
    
    ;; Insert multiple blocks at individual points
(defun MBS:insert-multiple-single (record-pairs target-space header num-index desc-index mat-id-index / count)
    (setq count 0)
    (setq total (length record-pairs))
    
    (foreach rec-pair record-pairs
    (setq record (cdr rec-pair))
    
    ;; Extract data
    (setq material-id (if (>= (length record) (1+ mat-id-index))
              (nth mat-id-index record)
              ""))
    (setq desc (if (>= (length record) (1+ desc-index))
           (nth desc-index record)
           ""))
    (setq item-no (if (>= (length record) (1+ num-index))
            (nth num-index record)
            ""))
    
    ;; Prompt for insertion point
    (prompt (strcat "\n[" (itoa (1+ count)) "/" (itoa total) "] Inserting: " 
             item-no " - " desc))
    (prompt "\nSpecify insertion point: ")
    (setq ins-pt (getpoint))
    
    (if ins-pt
      (progn
      ;; Insert the block
      (setq block (MBS:insert-block-with-attributes 
             target-space ins-pt material-id desc item-no))
      
      (if block
        (setq count (1+ count))
      )
      )
      (prompt "\nInsertion point not specified, skipping this block.")
    )
    )
    
    (if (> count 0)
    (progn
      (prompt (strcat "\n✓ Successfully inserted " (itoa count) " blocks."))
      (list count)  ;; Return list with count for consistency
    )
    (progn
      (prompt "\nNo blocks inserted.")
      (list 0)  ;; Return list with 0 count
    )
    )
  )
    
    ;; Insert multiple blocks in a grid layout
(defun MBS:insert-multiple-grid (record-pairs target-space header num-index desc-index mat-id-index / count)
    ;; Determine if we're in metric or imperial units
    (setq drawing-units (getvar "INSUNITS"))
    (setq is-metric nil)
    (if (member drawing-units '(4 5 6 7 12 13 14 15 16 17))
    (setq is-metric T)
    )
    
    ;; Set fixed spacing based on units
    (if is-metric
    (progn
      (setq x-spacing 110.0)  ;; 110mm for metric
      (setq y-spacing 20.0)   ;; 20mm for metric
      (prompt "\n[METRIC] Using spacing: 110mm x 20mm")
    )
    (progn
      (setq x-spacing 4.0)    ;; 4" for imperial
      (setq y-spacing 0.75)   ;; 0.75" for imperial
      (prompt "\n[IMPERIAL] Using spacing: 4\" x 0.75\"")
    )
    )
    
    ;; Prompt for base point
    (prompt "\nSpecify base insertion point: ")
    (setq base-pt (getpoint))
    
    ;; Prompt for grid arrangement
    (prompt "\nEnter number of blocks per row: ")
    (setq blocks-per-row (getint))
    (if (or (not blocks-per-row) (< blocks-per-row 1))
    (setq blocks-per-row 5)  ;; Default to 5 per row
    )
    
    (if base-pt
    (progn
      ;; Start insertion
      (setq count 0)
      (setq row-num 0)
      (setq col-num 0)
      (setq updated-records '())  ;; Track records that need CSV updates
      
      (foreach rec-pair record-pairs
      (setq record (cdr rec-pair))
      (setq rec-index (car rec-pair))
      
      ;; Extract data
      (setq material-id (if (>= (length record) (1+ mat-id-index))
                (nth mat-id-index record)
                ""))
      (setq desc (if (>= (length record) (1+ desc-index))
             (nth desc-index record)
             ""))
      (setq item-no (if (>= (length record) (1+ num-index))
              (nth num-index record)
              ""))
      
      ;; Debug: Show what we found in the ID column (commented for cleaner output)
      ;(prompt (strcat "\n[DEBUG] Row " (itoa rec-index) " - ID field: '" (if material-id material-id "NIL") "' (type: " (vl-princ-to-string (type material-id)) ")"))
      
      ;; Generate new ID if empty (for batch insert) 
      (setq id-was-generated nil)
      (if (or (not material-id) (= material-id "") (= (strcase material-id) "NIL"))
        (progn
        (setq material-id (generate-unique-id desc))
        (setq id-was-generated t)
        ;; Commented for cleaner output
        ;(prompt (strcat "\n[DEBUG] Generated new ID: " material-id " for row " (itoa rec-index)))
        )
        ;; Commented for cleaner output
        ;(prompt (strcat "\n[DEBUG] Using existing ID: " material-id))
      )
      
      ;; Calculate insertion point
      (setq ins-x (+ (car base-pt) (* col-num x-spacing)))
      (setq ins-y (- (cadr base-pt) (* row-num y-spacing)))
      (setq ins-pt (list ins-x ins-y))
      
      ;; Insert block
      (setq block (MBS:insert-block-with-attributes 
            target-space ins-pt material-id desc item-no))
      
      (if block
        (progn
        (setq count (1+ count))
        ;; If we generated a new ID, queue a transaction to update the CSV
        (if id-was-generated
          (progn
          ;; Commented for cleaner output
          ;(prompt (strcat "\n[DEBUG] Queueing transaction to update " (get-id-column) " for row " (itoa rec-index)))
          (MBS:queue-transaction (MBS:tx-update-field rec-index (get-id-column) material-id))
          ;(prompt (strcat "\n[DEBUG] Transaction queued successfully"))
          )
        )
        )
      )
      
      ;; Update grid position
      (setq col-num (1+ col-num))
      (if (>= col-num blocks-per-row)
        (progn
        (setq col-num 0)
        (setq row-num (1+ row-num))
        )
      )
      )
      
      (if (> count 0)
      (progn
        (prompt (strcat "\n✓ Successfully inserted " (itoa count) " blocks."))
        
        ;; Note: Transactions will be executed by the calling function
        
        (list count)  ;; Return count in list format for consistency
      )
      (progn
        (prompt "\nNo blocks inserted.")
        nil
      )
      )
    )
    (progn
      (prompt "\nNo base point specified.")
      nil
    )
    )
  )
    
    ;;; ========================================================================
    ;; SUPPORT FUNCTIONS FOR UPDATES
    ;;; ========================================================================
    
  ;; Update blocks from CSV data with enhanced debugging
(defun MBS:update-blocks-from-csv (block-map records header num-index desc-index mat-id-index / updates total-records processed-records)
    (setq updates 0)
    (setq total-records (length records))
    (setq processed-records 0)
    
    ;; (prompt (strcat "\n🔍 DEBUG: Starting block sync - " (itoa total-records) " CSV records, " (itoa (length block-map)) " blocks available"))
    
    ;; Process each record
    (foreach record records
      (setq processed-records (1+ processed-records))
      ;; (prompt (strcat "\n🔍 DEBUG: Processing CSV record " (itoa processed-records) "/" (itoa total-records)))
      
    (if (>= (length record) (1+ mat-id-index))
      (progn
      (setq material-id (nth mat-id-index record))
      (setq item-no (nth num-index record))
      (setq desc (nth desc-index record))
      
      ;; (prompt (strcat "\n🔍 DEBUG: CSV Record - ID: '" material-id "', ITEM NO: '" item-no "', DESC: '" desc "'"))
      
      ;; Find ALL matching blocks with this ID (not just the first one)
      (setq matching-blocks '())
      (foreach block-entry block-map
        (if (= (car block-entry) material-id)
          (setq matching-blocks (append matching-blocks (list (cdr block-entry))))
        )
      )
      
      (if (> (length matching-blocks) 0)
        (progn
          ;; (prompt (strcat "\n✅ DEBUG: Found " (itoa (length matching-blocks)) " matching block(s) for ID: " material-id))
          
          ;; Update ALL blocks with this ID
          (setq block-num 0)
          (foreach block matching-blocks
            (setq block-num (1+ block-num))
            (MBS:log-info (strcat "\n🔧 Updating block " (itoa block-num) "/" (itoa (length matching-blocks)) " for ID: " material-id))
            
          ;; Update attributes
          (setq att-list (vlax-invoke block 'GetAttributes))
          ;; (prompt (strcat "\n🔍 DEBUG: Block " (itoa block-num) " has " (itoa (length att-list)) " attributes"))
          
          (setq item-updated nil)
          (setq desc-updated nil)
          (foreach att att-list
            (setq tag (strcase (vlax-get att 'TagString)))
            ;; (prompt (strcat "\n🔍 DEBUG: Attribute '" tag "' = '" (vlax-get att 'TextString) "'"))
            (cond
            ((= tag "##")
             (vlax-put att 'TextString item-no)
             (setq item-updated t)
             (prompt (strcat "\n  ✅ Updated ITEM NO to: " item-no)))
            ((= tag "DESCRIPTION")
             (if (boundp 'shorten-description)
               (vlax-put att 'TextString (shorten-description desc))
               (vlax-put att 'TextString desc))
             (setq desc-updated t)
             (prompt (strcat "\n  ✅ Updated DESCRIPTION to: " desc)))
            )
          )
          
          (vla-Update block)
          (if item-updated
            (progn
              (setq updates (1+ updates))
              (prompt (strcat "\n✅ Successfully updated block " (itoa block-num) " (total: " (itoa updates) ") for ID: " material-id))
            )
            (prompt (strcat "\n⚠ Block " (itoa block-num) " found but ITEM NO not updated for ID: " material-id))
          )
          )
        )
        (prompt (strcat "\n❌ DEBUG: No matching blocks found for ID: '" material-id "'"))
      )
      )
      (prompt (strcat "\n⚠ DEBUG: Record " (itoa processed-records) " has insufficient columns"))
    )
    )
    
    ;; (prompt (strcat "\n🔍 DEBUG: Block sync complete - Updated " (itoa updates) "/" (itoa total-records) " blocks"))
    
      (prompt (strcat "\nUpdated " (itoa updates) " blocks with new item numbers and descriptions."))
  )
  
  ;; Get all blocks with material IDs - filtered by current mode's block type
(defun MBS:get-blocks-with-ids (/ doc block-map block-name)
    (prompt "\nFinding blocks with material IDs...")
    
    (setq block-map '())
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    (setq block-name (get-block-name))  ;; Get the correct block type for current mode
    
    (prompt (strcat "\n[DEBUG] Looking for blocks of type: " block-name))
    
    ;; Check modelspace
    (vlax-for ent (vla-get-ModelSpace doc)
    (if (and (= (vla-get-ObjectName ent) "AcDbBlockReference")
         (= (vlax-get ent 'EffectiveName) block-name))  ;; Filter by block type
      (progn
      ;; Try to get attributes
      (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke ent 'GetAttributes))))
      
      ;; Check if we got attributes successfully
      (if (not (vl-catch-all-error-p att-list))
        (progn
        ;; Check for material ID attribute
        (setq found-id nil)
        (foreach att att-list
          (if (= (strcase (vlax-get att 'TagString)) (strcase id-attribute))
          (progn
            (setq material-id (vlax-get att 'TextString))
            (if (and material-id (/= material-id ""))
            (progn
              (setq block-map (cons (cons material-id ent) block-map))
              (setq found-id t)
            )
            )
          )
          )
        )
        
        ;; If not found, check alternate ID
        (if (not found-id)
          (foreach att att-list
          (if (= (strcase (vlax-get att 'TagString)) (strcase alternate-id-attribute))
            (progn
            (setq removal-id (vlax-get att 'TextString))
            (if (and removal-id (/= removal-id ""))
              (setq block-map (cons (cons removal-id ent) block-map))
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
    
    ;; Check paperspace
    (vlax-for ent (vla-get-PaperSpace doc)
    (if (and (= (vla-get-ObjectName ent) "AcDbBlockReference")
         (= (vlax-get ent 'EffectiveName) block-name))  ;; Filter by block type
      (progn
      ;; Try to get attributes
      (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke ent 'GetAttributes))))
      
      ;; Check if we got attributes successfully
      (if (not (vl-catch-all-error-p att-list))
        (progn
        ;; Check for material ID attribute
        (setq found-id nil)
        (foreach att att-list
          (if (= (strcase (vlax-get att 'TagString)) (strcase id-attribute))
          (progn
            (setq material-id (vlax-get att 'TextString))
            (if (and material-id (/= material-id ""))
            (progn
              (setq block-map (cons (cons material-id ent) block-map))
              (setq found-id t)
            )
            )
          )
          )
        )
        
        ;; If not found, check alternate ID
        (if (not found-id)
          (foreach att att-list
          (if (= (strcase (vlax-get att 'TagString)) (strcase alternate-id-attribute))
            (progn
            (setq removal-id (vlax-get att 'TextString))
            (if (and removal-id (/= removal-id ""))
              (setq block-map (cons (cons removal-id ent) block-map))
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
    
    (prompt (strcat "\nFound " (itoa (length block-map)) " blocks with material IDs."))
    block-map
  )
  
    ;; Find orphaned rows (CSV rows with no matching blocks)
(defun MBS:find-orphaned-rows (id-map records header mat-id-index / orphans)
    (setq orphans '())
    (setq row-index 0)
    
    (foreach record records
    (if (>= (length record) (1+ mat-id-index))
      (progn
      (setq material-id (nth mat-id-index record))
      (if (and (/= material-id "") (not (assoc material-id id-map)))
        ;; This row has no matching block
        (setq orphans (append orphans (list (cons row-index record))))
      )
      )
    )
    (setq row-index (1+ row-index))
    )
    
    orphans
  )
    
    ;; Find orphaned blocks (blocks with no matching CSV row)
(defun MBS:find-orphaned-blocks (id-map records header mat-id-index / orphans)
    (setq orphans '())
    
    ;; For each material ID mapped to blocks
    (foreach id-pair id-map
    (setq material-id (car id-pair))
    (setq blocks (cdr id-pair))
    
    ;; Check if this material ID is in CSV
    (setq found nil)
    (foreach record records
      (if (and (>= (length record) (1+ mat-id-index))
           (= (strcase (nth mat-id-index record)) (strcase material-id)))
      (setq found t)
      )
    )
    
    ;; If not found in CSV, it's an orphaned block
    (if (not found)
      (setq orphans (append orphans (list id-pair)))
    )
    )
    
    orphans
  )
  
  ;; Process orphaned rows using MATERIAL_ID as the key identifier
(defun MBS:process-orphaned-rows-simplified (orphan-rows records header / choice material-ids-to-delete
                         mat-id-index item-num-index desc-index blocks-inserted)
    ;; Find relevant column indices - IMPORTANT: Use the actual column names from your CSV
    (setq mat-id-index (get-column-index header (strcase id-attribute)))
    (setq item-num-index (get-column-index header "ITEM NO."))
    (setq desc-index (get-column-index header "DESCRIPTION"))
    
    ;; Debug output for column indices
    (prompt (strcat "\nColumn indices - Material ID: " (itoa mat-id-index) 
           ", Item No: " (itoa item-num-index)
           ", Description: " (itoa desc-index)))
    
    ;; Initialize tracking variables
    (setq material-ids-to-delete '())
    (setq blocks-inserted nil)
    
    (prompt "\n=== ORPHANED ROWS DETECTED ===")
    (prompt "\nThe following rows in CSV have no matching blocks:")
    
    ;; Process each orphaned row
    (foreach orphan-pair orphan-rows
    (setq row-index (car orphan-pair))
    (setq record (cdr orphan-pair))
    
    ;; Get item description
    (setq desc (if (and desc-index (>= (length record) (1+ desc-index)))
            (nth desc-index record)
            "Unknown"))
    
    ;; Get item number
    (setq item-no (if (and item-num-index (>= (length record) (1+ item-num-index)))
             (nth item-num-index record)
             "TBD"))
    
    ;; Get material ID
    (setq material-id (if (and mat-id-index (>= (length record) (1+ mat-id-index)))
              (nth mat-id-index record)
              "?"))
    
    ;; Show row information
    (prompt (strcat "\nRow " (itoa (1+ row-index)) ": " 
             item-no " - " desc))
    (prompt (strcat "\nMaterial ID: " material-id))
    
    ;; Prompt for action
    (initget "Insert Delete Skip")
    (setq choice (getkword "\nOptions: (I)nsert Block, (D)elete Row, (S)kip: "))
    
    (cond
      ;; Insert Block option
      ((= choice "Insert")
       ;; Get target space
       (setq space-result (MBS:get-current-space))
       (setq target-space (car space-result))
       
       ;; Get insertion point
       (prompt "\nSpecify insertion point: ")
       (setq ins-pt (getpoint))
       
       ;; Insert the block
       (if ins-pt
       (progn
         (setq block (MBS:insert-block-with-attributes 
               target-space ins-pt material-id desc item-no))
         
         (if block
         (progn
           (prompt "\nBlock inserted successfully.")
           (setq blocks-inserted t)
         )
         (prompt "\nFailed to insert block.")
         )
       )
       (prompt "\nNo insertion point specified. Skipping block insertion.")
       )
      )
      
      ;; Delete Row option - Now collect Material IDs
      ((= choice "Delete")
       (if (and mat-id-index material-id (/= material-id ""))
       (progn
         ;; Add to list of material IDs to delete
         (setq material-ids-to-delete (cons material-id material-ids-to-delete))
         (prompt (strcat "\nQueuing deletion of material ID: " material-id))
       )
       (prompt "\nMaterial ID not found or empty. Cannot delete this row.")
       )
      )
      
      ;; Skip option
      ((= choice "Skip")
       (prompt "\nSkipping this row.")
      )
    )
    )
    
    ;; Process all material IDs marked for deletion
    (if (> (length material-ids-to-delete) 0)
    (progn
      ;; Queue transactions to delete by material ID
      (prompt (strcat "\nQueuing " (itoa (length material-ids-to-delete)) " material ID deletions"))
      (foreach mat-id material-ids-to-delete
      (prompt (strcat "\n - Queuing deletion of: " mat-id))
      (MBS:queue-transaction (list 'delete-row-by-material-id mat-id))
      )
    )
    )
    
    ;; Return whether any blocks were inserted
    blocks-inserted
  )
    
    ; ; ;; Process orphaned rows
  ; ; (defun MBS:process-orphaned-rows-simplified (orphan-rows records header / choice blocks-inserted)
    ; ; (prompt "\n=== ORPHANED ROWS DETECTED ===")
    ; ; (prompt "\nThe following rows in CSV have no matching blocks:")
    
    ; ; ;; Initialize tracking flag
    ; ; (setq blocks-inserted nil)
    
    ; ; ;; Get target space once for all insertions
    ; ; (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    ; ; (setq target-space nil)
    
    ; ; ;; Process each orphaned row
    ; ; (foreach orphan-pair orphan-rows
    ; ; (setq row-index (car orphan-pair))
    ; ; (setq record (cdr orphan-pair))
    
    ; ; ;; Get item description
    ; ; (setq desc-index (get-column-index header "DESCRIPTION"))
    ; ; (setq desc (if (and desc-index (>= (length record) (1+ desc-index)))
            ; ; (nth desc-index record)
            ; ; "Unknown"))
    
    ; ; ;; Get item number
    ; ; (setq num-index (get-column-index header "ITEM NO."))
    ; ; (setq item-no (if (and num-index (>= (length record) (1+ num-index)))
             ; ; (nth num-index record)
             ; ; "TBD"))
    
    ; ; ;; Get material ID
    ; ; (setq mat-id-index (get-column-index header (strcase id-attribute)))
    ; ; (setq material-id (if (and mat-id-index (>= (length record) (1+ mat-id-index)))
               ; ; (nth mat-id-index record)
               ; ; "?"))
    
    ; ; ;; Show row information
    ; ; (prompt (strcat "\n\nRow " (itoa (1+ row-index)) ": " item-no " - " desc))
    ; ; (prompt (strcat "\nMaterial ID: " material-id))
    
    ; ; ;; Prompt for action
    ; ; (initget "Insert Delete Skip")
    ; ; (setq choice (getkword "\nOptions: (I)nsert Block, (D)elete Row, (S)kip: "))
    
    ; ; (cond
      ; ; ;; Insert Block option - DIRECT insertion without transaction
      ; ; ((= choice "Insert")
       ; ; ;; Get target space if not already determined
       ; ; (if (not target-space)
       ; ; (progn
         ; ; (initget "Model Paper")
         ; ; (setq space-choice (getkword "\nInsert into (M)odel or (P)aper space? "))
         ; ; (setq target-space 
         ; ; (if (= space-choice "Paper")
           ; ; (vla-get-PaperSpace doc)
           ; ; (vla-get-ModelSpace doc)))
       ; ; )
       ; ; )
       
       ; ; ;; Get insertion point
       ; ; (prompt (strcat "\nInserting block for item: " item-no " - " desc))
       ; ; (prompt "\nSpecify insertion point: ")
       ; ; (setq ins-pt (getpoint))
       
       ; ; ;; Insert the block immediately
       ; ; (if ins-pt
       ; ; (progn
         ; ; (setq blkRef nil)
         ; ; (vl-catch-all-apply
         ; ; '(lambda ()
           ; ; (setq blkRef (vla-InsertBlock 
                  ; ; target-space
                  ; ; (vlax-3d-point (car ins-pt) (cadr ins-pt) 0.0)
                  ; ; (get-block-name)
                  ; ; 1.0
                  ; ; 1.0
                  ; ; 1.0
                  ; ; 0.0))
         ; ; )
         ; ; )
         
         ; ; (if blkRef
         ; ; (progn
           ; ; ;; Set the attributes
           ; ; (setq attList (vlax-invoke blkRef 'GetAttributes))
    ; ; (foreach att attList
    ; ; (setq tag (strcase (vlax-get att 'TagString)))
    ; ; (cond
      ; ; ((= tag (strcase id-attribute))
       ; ; (vlax-put att 'TextString material-id))
      ; ; ((= tag "##") 
       ; ; (vlax-put att 'TextString item-no))
      ; ; ((= tag "DESCRIPTION") 
       ; ; (if (boundp 'shorten-description)
       ; ; (vlax-put att 'TextString (shorten-description (MBS:clean-attribute-text desc)))
       ; ; (vlax-put att 'TextString (MBS:clean-attribute-text desc))))
    ; ; )
     ; ; )
           
           ; ; ;; Update the block
           ; ; (vla-Update blkRef)
           ; ; (prompt "\n✓ Block inserted successfully.")
           ; ; (setq blocks-inserted t)  ;; Mark that we've inserted a block
         ; ; )
         ; ; (prompt "\n❌ Failed to insert block.")
         ; ; )
       ; ; )
       ; ; (prompt "\nNo insertion point specified. Skipping block insertion.")
       ; ; )
      ; ; )
      
      ; ; ;; Delete Row option - Use transaction
      ; ; ((= choice "Delete")
       ; ; (MBS:queue-transaction (MBS:tx-delete-row row-index))
      ; ; )
      
      ; ; ;; Skip option - do nothing
      ; ; ((= choice "Skip")
       ; ; (prompt "\nSkipping this row.")
      ; ; )
    ; ; )
    ; ; )
    
    ; ; ;; Return whether any blocks were inserted
    ; ; blocks-inserted
  ; ; )
    
    ;; Helper function to group blocks by description
(defun MBS:group-blocks-by-description (empty-blocks / grouped-blocks)
    (setq grouped-blocks '())
    
    ;; Process each block to extract descriptions and group them
    (foreach block empty-blocks
    ;; Check if block is valid
    (if (and block (= (type block) 'VLA-OBJECT))
      (progn
      ;; Use safer attribute extraction with error handling
      (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke block 'GetAttributes))))
      
      ;; Check if we got attributes successfully
      (if (not (vl-catch-all-error-p att-list))
        (progn
        ;; Extract info from attributes
        (setq item-no "")
        (setq desc "")
        
        (foreach att att-list
          ;; Use safe attribute access
          (setq tag (vl-catch-all-apply '(lambda () (strcase (vlax-get att 'TagString)))))
          
          (if (not (vl-catch-all-error-p tag))
          (cond
            ((= tag "##") 
             (setq item-no (MBS:clean-attribute-text-safe (vlax-get att 'TextString))))
            ((= tag "DESCRIPTION") 
             (setq desc (MBS:clean-attribute-text-safe (vlax-get att 'TextString))))
          )
          )
        )
        
        ;; Add to grouped blocks by description
        (if (> (strlen desc) 0)
          (progn
          (setq group (assoc desc grouped-blocks))
          (if group
            ;; Add to existing group
            (setq grouped-blocks (MBS:update-assoc-value 
                       grouped-blocks 
                       desc 
                       (append (cdr group) (list (cons block item-no)))))
            ;; Create new group
            (setq grouped-blocks (append grouped-blocks 
                         (list (cons desc (list (cons block item-no))))))
          )
          )
        )
        )
      )
      )
    )
    )
    grouped-blocks
  )

  ;; Helper function to get block location information
(defun MBS:get-block-location-info (block / space-name layout-name)
    (setq space-name "Model Space")
    (setq layout-name "")
    
    ;; Try to get layout information if available
    (vl-catch-all-apply
    '(lambda ()
      (if (= (vla-get-ObjectName block) "AcDbBlockReference")
      (progn
        (setq parent (vla-get-Parent block))
        (if parent
        (progn
          (setq parent-name (vla-get-ObjectName parent))
          (if (= parent-name "AcDbLayout")
          (progn
            (setq space-name "Paper Space")
            (setq layout-name (vla-get-Name parent))
          )
          )
        )
        )
      )
      )
    )
    )
    
    (if (> (strlen layout-name) 0)
    (strcat space-name " - " layout-name)
    space-name
    )
  )

  ;; Helper function to display block group information
(defun MBS:display-block-group (group / desc block-list first-block item-no location-info)
    (setq desc (car group))
    (setq block-list (cdr group))
    (setq first-block (car (car block-list)))
    
    ;; Display group info
    (prompt (strcat "\n\nDescription: " desc))
    (prompt (strcat "\nFound " (itoa (length block-list)) " block(s) with this description"))
    
    ;; Get location information for the first block
    (setq location-info (MBS:get-block-location-info first-block))
    (prompt (strcat "\nLocation: " location-info))
    
    ;; Get the first item number if any are non-empty
    (setq item-no "")
    (foreach block-pair block-list
    (if (and (/= (cdr block-pair) "") (= item-no ""))
      (setq item-no (cdr block-pair))
    )
    )
    
    ;; Show info
    (prompt (strcat "\nItem: " (if (/= item-no "") item-no "TBD") " - " desc))
    
    ;; Return the extracted information
    (list desc block-list item-no location-info)
  )

  ;; Helper function to handle reassociate choice
(defun MBS:handle-reassociate-choice (match-list desc / material-id row-index match-num)
    (if (= (length match-list) 1)
    ;; Only one match - use it automatically
    (progn
      (setq material-id (cadr (car match-list)))
      ;; CHECK IF MATERIAL_ID IS EMPTY
      (if (or (not material-id) (= material-id ""))
      (progn
        ;; Generate new MATERIAL_ID and update CSV
        (setq material-id (generate-unique-id desc))
        (setq row-index (car (car match-list)))
        (MBS:queue-transaction (MBS:tx-update-field row-index (strcase id-attribute) material-id))
        (prompt (strcat "\nGenerated new Material ID: " material-id " for CSV row"))
      )
      )
      (prompt (strcat "\nRe-associating with Material ID: " material-id))
    )
    ;; Multiple matches - ask which one to use
    (progn
      (prompt "\nEnter match number to use: ")
      (setq match-num (atoi (getstring)))
      (if (and (> match-num 0) (<= match-num (length match-list)))
      (progn
        (setq material-id (cadr (nth (1- match-num) match-list)))
        ;; Check if the selected match has empty material ID
        (if (or (not material-id) (= material-id ""))
        (progn
          ;; Generate new MATERIAL_ID and update CSV
          (setq material-id (generate-unique-id desc))
          (setq row-index (car (nth (1- match-num) match-list)))
          (MBS:queue-transaction (MBS:tx-update-field row-index (strcase id-attribute) material-id))
          (prompt (strcat "\nGenerated new Material ID: " material-id " for CSV row"))
        )
        )
      )
      (setq material-id nil)
      )
    )
    )
    material-id
  )

  ;; Helper function to handle insert choice
(defun MBS:handle-insert-choice (desc item-no header / new-record new-material-id)
    (MBS:log-info "DEBUG: Starting MBS:handle-insert-choice")
    (MBS:log-info (strcat "DEBUG: Header length: " (itoa (length header))))
    
    ;; Create new row
    (setq new-record (MBS:create-empty-record header))
    (MBS:log-info (strcat "DEBUG: Empty record created, length: " (itoa (length new-record))))
    
    ;; Generate a new material ID
    (setq new-material-id (generate-unique-id desc))
    (MBS:log-info (strcat "DEBUG: Generated material ID: " new-material-id))
    
    ;; Set material ID using enhanced function
    (setq new-record (MBS:set-record-value-enhanced new-record header (get-id-column) new-material-id))
    (MBS:log-info (strcat "DEBUG: After setting material ID, record length: " (itoa (length new-record))))
    
    ;; Set description using enhanced function
    (setq new-record (MBS:set-record-value-enhanced new-record header "DESCRIPTION" desc))
    (MBS:log-info (strcat "DEBUG: After setting description, record length: " (itoa (length new-record))))
    
    ;; Use existing item number or TBD using enhanced function
    (if (and item-no (/= item-no ""))
    (progn
      (setq new-record (MBS:set-record-value-enhanced new-record header "ITEM NO." item-no))
      (MBS:log-info (strcat "DEBUG: After setting item number '" item-no "', record length: " (itoa (length new-record))))
    )
    (progn
      (setq new-record (MBS:set-record-value-enhanced new-record header "ITEM NO." "TBD"))
      (MBS:log-info (strcat "DEBUG: After setting TBD item number, record length: " (itoa (length new-record))))
    )
    )
    
    (MBS:log-info "DEBUG: About to queue transaction")
    ;; Queue transaction to insert the row
    (MBS:queue-transaction (MBS:tx-insert-row new-record))
    (MBS:log-info "DEBUG: Transaction queued successfully")
    
    new-material-id
  )

  ;; Helper function to update blocks with material ID
(defun MBS:update-blocks-with-material-id (block-list material-id / updated-count)
    (setq updated-count 0)
    (foreach block-pair block-list
    (setq block (car block-pair))
    (setq att-list (vlax-invoke block 'GetAttributes))
    (foreach att att-list
      (if (= (strcase (vlax-get att 'TagString)) (strcase id-attribute))
      (vlax-put att 'TextString material-id)
      )
    )
    (vla-Update block)
    (setq updated-count (1+ updated-count))
    )
    updated-count
  )

    ;; Process blocks with empty Material IDs
  (defun MBS:process-empty-id-blocks-simplified (empty-blocks records header / choice grouped-blocks group-info total-groups current-group)
    (prompt "\n=== BLOCKS WITH EMPTY MATERIAL IDs DETECTED ===")
    (prompt "\nThe following blocks have empty or missing Material IDs:")
    
    ;; First, group blocks by description using helper function
    (setq grouped-blocks (MBS:group-blocks-by-description empty-blocks))
    (setq total-groups (length grouped-blocks))
    (setq current-group 0)
    
    ;; Now process each group
    (foreach group grouped-blocks
    (setq current-group (1+ current-group))
    
    ;; Show progress if enabled
    (MBS:show-progress current-group total-groups "Processing block groups")
    
    ;; Display group info using helper function
    (setq group-info (MBS:display-block-group group))
    (setq desc (car group-info))
    (setq block-list (cadr group-info))
    (setq item-no (caddr group-info))
    
    ;; NEW: Look for potential CSV matches
    (setq first-block (car (car block-list)))
    (setq matches (MBS:find-potential-csv-matches first-block records header))
    
    (if matches
      (progn
      (setq match-type (car matches))
      (setq match-list (cdr matches))
      
      (prompt (strcat "\n\nFound " (itoa (length match-list)) " potential " 
               (if (eq match-type 'exact) "exact" "fuzzy") 
               " match(es) in CSV:"))
      
      ;; Show matches
      (setq i 1)
      (foreach match match-list
        (setq row-index (car match))
        (setq material-id (cadr match))
        (setq csv-desc (caddr match))
        
        (prompt (strcat "\n" (itoa i) ": " material-id " - " csv-desc))
        (setq i (1+ i))
      )
      
      ;; Prompt for action with re-association option
      (initget "Reassociate Insert Delete Skip")
      (setq choice (getkword "\nOptions: (R)eassociate with CSV, (I)nsert to CSV, (D)elete Block(s), (S)kip: "))
      
      (cond
        ;; Re-associate option - NEW
        ((= choice "Reassociate")
         (setq material-id (MBS:handle-reassociate-choice match-list desc))
         
         ;; Update all blocks with the selected material ID
         (if material-id
         (progn
           (setq updated-count (MBS:update-blocks-with-material-id block-list material-id))
           (prompt (strcat "\n✓ Updated " (itoa updated-count) " block(s) with Material ID: " material-id))
         )
         (prompt "\n❌ Invalid match number. No updates made.")
         )
        )
        
        ;; Insert to CSV option using helper function
        ((= choice "Insert")
         (setq new-material-id (MBS:handle-insert-choice desc item-no header))
         
         ;; Update ALL blocks in this group with the new material ID
         (setq updated-count (MBS:update-blocks-with-material-id block-list new-material-id))
         
         (prompt (strcat "\n✓ Updated " (itoa updated-count) " block(s) with new material ID"))
        )
        
        ;; Delete Block(s) option
        ((= choice "Delete")
         ;; Queue transactions to delete all blocks in this group
         (foreach block-pair block-list
         (MBS:queue-transaction (MBS:tx-delete-block (car block-pair)))
         )
        )
        
        ;; Skip option - do nothing
        ((= choice "Skip")
         (prompt "\nSkipping these blocks.")
        )
      )
      )
      ;; No matches found - proceed with regular options
      (progn
      ;; Prompt for action without re-association option
      (initget "Insert Delete Skip")
      (setq choice (getkword "\nOptions: (I)nsert to CSV, (D)elete Block(s), (S)kip: "))
      
      (cond
        ;; Insert to CSV option using helper function
        ((= choice "Insert")
         (setq new-material-id (MBS:handle-insert-choice desc item-no header))
         
         ;; Update ALL blocks in this group with the new material ID
         (setq updated-count (MBS:update-blocks-with-material-id block-list new-material-id))
         
         (prompt (strcat "\n✓ Updated " (itoa updated-count) " block(s) with new material ID"))
        )
        
        ;; Delete Block(s) option
        ((= choice "Delete")
         ;; Queue transactions to delete all blocks in this group
         (foreach block-pair block-list
         (MBS:queue-transaction (MBS:tx-delete-block (car block-pair)))
         )
        )
        
        ;; Skip option - do nothing
        ((= choice "Skip")
         (prompt "\nSkipping these blocks.")
        )
      )
      )
    )
    )
  )
    
    ;; Build material updates
(defun MBS:build-material-updates (id-map records header / num-index desc-index blocks-to-update)
    ;; Get column indices
    (setq desc-index (get-column-index header "DESCRIPTION"))
    (setq mat-id-index (get-column-index header (strcase id-attribute)))
    
    (setq blocks-to-update 0)
    
    ;; For each mapped material ID
    (foreach id-pair id-map
    (setq material-id (car id-pair))
    (setq blocks (cdr id-pair))
    
    ;; Find matching record in CSV
    (setq matched-record (MBS:find-record-by-material-id material-id records header))
    
    (if matched-record
      (progn
      ;; Get CSV description
      (setq csv-desc (if (and desc-index (>= (length matched-record) (1+ desc-index)))
               (nth desc-index matched-record)
               ""))
      
      ;; Process each block with this material ID
      (foreach blk blocks
        (if (and blk (= (type blk) 'VLA-OBJECT))
        (progn
          (setq needs-update nil)
          (setq field-updates '())
          
          ;; Get current block attributes
          (setq att-list (vlax-invoke blk 'GetAttributes))
          
          ;; Check description attribute only in Stage 1
    (foreach att att-list
    (if (= (strcase (vlax-get att 'TagString)) "DESCRIPTION")
      (progn
      (setq current-val (MBS:clean-attribute-text (vlax-get att 'TextString)))
      (setq expected-val (if (boundp 'shorten-description)
                 (shorten-description csv-desc)
                 csv-desc))
       
      (if (and (/= csv-desc "") (/= current-val expected-val))
        (progn
        (setq needs-update t)
        (setq field-updates 
            (append field-updates 
               (list (list "DESCRIPTION" 
                    current-val 
                    expected-val))))
        )
      )
      )
    )
    )
          
          ;; If block needs update, queue transaction with detailed info
          (if (and needs-update (> (length field-updates) 0))
          (progn
            ;; Include full item info for better preview
            (setq item-info (list csv-desc material-id))
            (MBS:queue-transaction (MBS:tx-update-block-detailed blk field-updates item-info))
            (setq blocks-to-update (1+ blocks-to-update))
          )
          )
        )
        )
      )
      )
    )
    )
    
    blocks-to-update
  )
  
  ;; Find a record by MATERIAL_ID
(defun MBS:find-record-by-material-id (material-id records header / material-id-index)
    (setq material-id-index (get-column-index header (strcase id-attribute)))
    (if (and material-id-index (>= material-id-index 0))
    (progn
      (setq result nil)
      (foreach record records
      (if (and (>= (length record) (1+ material-id-index))
          (= (strcase (nth material-id-index record)) (strcase material-id)))
        (setq result record)
      )
      )
      result
    )
    nil
    )
  )
  
    (prompt "\nBlock support functions loaded!")
  
  ;; ===============================================
  ;;; REASSOCATION LOGIC FUNCTIONS
  ;; ===============================================

  ;; Find potential CSV matches for blocks with missing Material IDs
(defun MBS:find-potential-csv-matches (block records header / desc exact-matches fuzzy-matches)
    ;; Get block attributes
    (setq att-list (vlax-invoke block 'GetAttributes))
    
    ;; Extract description from block
    (setq desc "")
    (foreach att att-list
    (if (= (strcase (vlax-get att 'TagString)) "DESCRIPTION")
      (setq desc (MBS:clean-attribute-text (vlax-get att 'TextString)))
    )
    )
    
    ;; If no description, return nil
    (if (= desc "") 
    nil
    (progn
      ;; Find description column index
      (setq desc-index (get-column-index header "DESCRIPTION"))
      (if (not desc-index)
      nil
      (progn
        (setq exact-matches '())
        (setq fuzzy-matches '())
        
        ;; Find material ID column index
        (setq mat-id-index (get-column-index header (get-id-column)))
        
        ;; Loop through records to find matches
        (setq i 0)
        (foreach record records
        (if (and (>= (length record) (1+ desc-index))
            (>= (length record) (1+ mat-id-index)))
          (progn
          (setq csv-desc (nth desc-index record))
          (setq material-id (nth mat-id-index record))
          
          ;; Check for exact match
          (if (= (strcase csv-desc) (strcase desc))
            (setq exact-matches (append exact-matches (list (list i material-id csv-desc))))
            ;; Check for fuzzy match (block desc is shortened CSV desc)
            (if (MBS:is-shortened-description desc csv-desc)
            (setq fuzzy-matches (append fuzzy-matches (list (list i material-id csv-desc))))
            )
          )
          )
        )
        (setq i (1+ i))
        )
        
        ;; Return matches, prioritizing exact matches
        (if (> (length exact-matches) 0)
        (cons 'exact exact-matches)
        (if (> (length fuzzy-matches) 0)
          (cons 'fuzzy fuzzy-matches)
          nil
        )
        )
      )
      )
    )
    )
  )
  
      (prompt "\nReassociation support functions loaded!")

  ;;; ========================================================================
  ;; DESCRIPTION SHORTENING FUNCTIONS
  ;;; ========================================================================
  ;; Global variable for description shortening 
  (if (not (boundp 'description-comma-limit))
    (setq description-comma-limit 2)  ;; Default to keep only text before SECOND comma
  )

  ;; Function to toggle description shortening
(defun toggle-description-shortening ()
    (prompt "\nEnter number of commas to keep in descriptions (0 for no shortening): ")
    (setq new-limit (getint))
    (if new-limit
    (progn
      (setq description-comma-limit new-limit)
      (prompt (strcat "\nDescription shortening set to keep " 
            (itoa description-comma-limit) " comma" 
            (if (= description-comma-limit 1) "" "s")))
    )
    (prompt "\nInvalid input. Description shortening unchanged.")
    )
  )

  ;; Function to shorten description based on comma limit
(defun shorten-description (description / count pos result last-pos)
    ;; Safety check to handle nil or empty strings
    (if (or (not description) (= description ""))
    ""  ;; Return empty string for nil/empty inputs
    (progn
      ;; If comma limit is 0 or negative, return the full description
      (if (<= description-comma-limit 0)
      description
      (progn
        (setq count 0)
        (setq result description)
        (setq last-pos 0)
        
        ;; Find the position after the specified number of commas
        (while (and (< count description-comma-limit)
              (setq pos (vl-string-search "," description last-pos)))
        (setq count (1+ count))
        (setq last-pos (1+ pos))
        )
        
        ;; If we found the target number of commas, truncate the string (don't include the comma itself)
        (if (= count description-comma-limit)
        (progn
          ;; Take everything up to but not including the last comma
          (setq result (substr description 1 (- last-pos 1)))
          ;; Trim any trailing whitespace
          (vl-string-right-trim " " result)
        )
        ;; Otherwise, return the full string (not enough commas found)
        description
        )
      )
      )
    )
    )
  )
  
  ;; Check if desc1 is a shortened version of desc2
(defun MBS:is-shortened-description (desc1 desc2 / shortened)
  ;; Try to shorten desc2 in the same way our system would
  (setq shortened 
    (if (boundp 'shorten-description)
      (shorten-description desc2)
      desc2
    ))

  ;; Compare shortened version with desc1
  (= (strcase desc1) (strcase shortened))
)

;; Calculate actual similarity percentage between two strings
(defun MBS:calculate-similarity (str1 str2 / len1 len2 max-len min-len matches i char1 char2)
  "Calculate similarity percentage between two strings based on character matching"
  
  (setq str1 (strcase str1))
  (setq str2 (strcase str2))
  (setq len1 (strlen str1))
  (setq len2 (strlen str2))
  (setq max-len (max len1 len2))
  (setq min-len (min len1 len2))
  
  (if (= max-len 0)
    1.0  ; Both empty strings = 100% match
    (progn
      (setq matches 0)
      (setq i 0)
      
      ;; Count matching characters at same positions
      (while (< i min-len)
        (setq char1 (substr str1 (1+ i) 1))
        (setq char2 (substr str2 (1+ i) 1))
        (if (= char1 char2)
          (setq matches (1+ matches))
        )
        (setq i (1+ i))
      )
      
      ;; Calculate similarity as percentage of matching characters vs max length
      (/ (float matches) (float max-len))
    )
  )
)

(prompt "\nDescription shortening support functions loaded!")
    
    ;;; ========================================================================
    ;; ITEM NUMBERING SUPPORT FUNCTIONS
    ;;; ========================================================================
    
    ;; Force renumbering for a prefix group
(defun MBS:force-renumber-prefix-group (group-records original-records num-index prefix / changes)
    (setq changes '())
    
    ;; Get header from original records
    (setq header (car original-records))
    
    ;; Find description column index in header
    (setq desc-index (get-column-index header "DESCRIPTION"))
    
    ;; Sort records by existing number if available
    (setq sorted-records 
      (vl-sort group-records
          '(lambda (a b) 
             (< (MBS:get-sort-value (cdr a) num-index)
              (MBS:get-sort-value (cdr b) num-index)))))
    
    ;; Generate new numbers
    (setq new-index 1)
    (foreach rec sorted-records
    (setq row-index (car rec))
    (setq record (cdr rec))
    
    ;; Get current item number
    (setq current-item-no 
        (if (and (>= (length record) (1+ num-index)))
        (nth num-index record)
        ""))
    
    ;; Get description directly from record
    (setq description 
      (if (and desc-index (>= (length record) (1+ desc-index)))
        (MBS:clean-attribute-text (nth desc-index record))
        ""))
    
    ;; Generate new item number
    (setq new-item-no (strcat prefix (itoa new-index)))
    
    ;; Always add to changes list with description
    (setq changes (append changes (list (list current-item-no new-item-no description))))
    
    ;; Increment for next item
    (setq new-index (1+ new-index))
    )
    
    (list changes)
  )
  
    ;; Apply forced renumbering to a prefix group
(defun MBS:apply-force-renumber-prefix-group (group-records original-records header num-index prefix / modified)
    (setq modified nil)
    (prompt (strcat "\n  🔄 Renumbering " (itoa (length group-records)) " items with prefix '" prefix "'"))
    
    ;; Sort records by existing number if available
    (setq sorted-records 
      (vl-sort group-records
          '(lambda (a b) 
             (< (MBS:get-sort-value (cdr a) num-index)
              (MBS:get-sort-value (cdr b) num-index)))))
    
    ;; Apply new numbers
    (setq new-index 1)
    (foreach rec sorted-records
    (setq row-index (car rec))
    (setq record (cdr rec))
    
    ;; Get current item number
    (setq current-item-no 
        (if (and (>= (length record) (1+ num-index))
            (nth num-index record))
        (nth num-index record)
        ""))
    
    ;; Generate new item number
    (setq new-item-no (strcat prefix (itoa new-index)))
    
    ;; Always update the record
    (progn
      ;; Update the record in the original records list
      (if (< row-index (length original-records))
      (progn
        (setq updated-record (MBS:replace-at-index 
                   (nth row-index original-records)
                   num-index
                   new-item-no))
        (setq original-records (MBS:replace-at-index 
                  original-records
                  row-index
                  updated-record))
        (setq modified t)
      )
      )
    )
    
    ;; Increment for next item
    (setq new-index (1+ new-index))
    )
    
    ;; Simple completion message
    (if modified
      (prompt (strcat "\n  ✅ " prefix " prefix numbering completed"))
    )
    
    (list original-records modified)
  )

  
(defun MBS:prompt-for-filtered-rows (records header / num-index desc-index mat-id-index filter-col filter-val filtered-rows)
    ;; Get column to filter on
    (prompt "\n\n=== FILTER ROWS ===")
    (prompt "\nAvailable columns:")
    (setq i 1)
    (foreach col header
    (prompt (strcat "\n" (itoa i) ": " col))
    (setq i (1+ i))
    )
    
    ;; Prompt for column selection
    (prompt "\n\nEnter column number to filter on: ")
    (setq col-num (getint))
    
    (if (and (> col-num 0) (<= col-num (length header)))
    (progn
      (setq filter-col (1- col-num))
      
      ;; Prompt for filter value
      (prompt (strcat "\nEnter value to filter on column '" (nth filter-col header) "': "))
      (setq filter-val (getstring))
      
      ;; Apply filter
      (setq filtered-rows '())
      (foreach record records
      (if (and (>= (length record) (1+ filter-col))
          (= (strcase (nth filter-col record)) (strcase filter-val)))
        (setq filtered-rows (append filtered-rows (list record)))
      )
      )
      
      ;; Show results
      (if (> (length filtered-rows) 0)
      (progn
        (prompt (strcat "\n\nFound " (itoa (length filtered-rows)) " matching rows:"))
        
        ;; Get indices for display
        (setq num-index (get-column-index header "ITEM NO."))
        (setq desc-index (get-column-index header "DESCRIPTION"))
        
        ;; Display filtered rows
        (setq i 1)
        (foreach record filtered-rows
        (prompt (strcat "\n" (itoa i) ": " 
                 (nth num-index record) " - " 
                 (nth desc-index record)))
        (setq i (1+ i))
        )
        
        filtered-rows
      )
      (progn
        (prompt "\nNo matching rows found.")
        nil
      )
      )
    )
    nil
    )
  )
  
  ;; FIXED Sort a group of records numerically - handles both raw records and pairs
(defun MBS:sort-group-numerically (records num-index prefix / sorted records-with-values record item-no num-part num-val sorted-with-values i)
    (if (= (length records) 0)
    '()  ;; Empty group
    (progn
      ;; Add numeric value for sorting
      (setq records-with-values '())
      (foreach record-item records
        ;; Handle both raw records and (row-index . record) pairs
        (if (and (listp record-item) (numberp (car record-item)))
          ;; It's a (row-index . record) pair
          (setq record (cdr record-item))
          ;; It's a raw record
          (setq record record-item)
        )
        
      (setq item-no (if (>= (length record) (1+ num-index))
              (nth num-index record)
              ""))
      ;; Ensure item-no is a string
      (if item-no 
        (setq item-no (vl-princ-to-string item-no))
        (setq item-no "")
      )
      
      ;; Extract numeric part after prefix
      (setq num-part item-no)
      (if (and (> (strlen item-no) 0) 
           (> (strlen prefix) 0)
           (= (strcase (substr item-no 1 (strlen prefix))) (strcase prefix)))
        (setq num-part (substr item-no (1+ (strlen prefix))))
        ;; If no prefix match, extract all digits from the end
        (if (> (strlen item-no) 0)
        (progn
          (setq i (strlen item-no))
          (while (and (> i 0) (is-digit (substr item-no i 1)))
          (setq i (1- i))
          )
          (if (< i (strlen item-no))
          (setq num-part (substr item-no (1+ i)))
          )
        )
        )
      )
      
      ;; Convert to number (default to 999999 if can't parse)
      (setq num-val (atoi num-part))
      (if (= num-val 0) (setq num-val 999999))  ;; Put invalid numbers at end
      
      ;; Add to list with numeric value
      (setq records-with-values (append records-with-values (list (list num-val record))))
      )
      
      ;; Sort by numeric value
      (setq sorted-with-values (vl-sort records-with-values 
                      '(lambda (a b) (< (car a) (car b)))))
      
      ;; Extract just the records
      (setq sorted '())
      (foreach item sorted-with-values
      (setq sorted (append sorted (list (cadr item))))
      )
      
      sorted
    )
    )
  )
  
  ;; FIXED Sort records by ITEM NO with special handling for prefixes
(defun MBS:sort-records-by-item-no (records num-index / grouped-records sorted-groups result item-no prefix i group sorted-group)
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
      
      ;; Add record to group
      (setq grouped-records 
          (subst (cons (car group) (append (cdr group) (list record)))
             group
             grouped-records))
      )
      ;; If record is too short, skip it
      (prompt (strcat "\n⚠ Record too short for sorting: " (vl-princ-to-string record)))
    )
    )
    
    ;; Sort each group numerically with debugging
    (setq sorted-groups '())
    (foreach group grouped-records
    (setq prefix (car group))
    (setq group-records (cdr group))
    
    (if (> (length group-records) 0)
      (progn
      (prompt (strcat "\n🔄 Sorting " (itoa (length group-records)) " records with prefix '" prefix "'"))
      ;; Sort this group
      (setq sorted-group (MBS:sort-group-numerically group-records num-index prefix))
      (prompt (strcat "\n✅ Sorted " (itoa (length sorted-group)) " records with prefix '" prefix "'"))
      )
      (setq sorted-group '())
    )
    
    ;; Add to sorted groups
    (setq sorted-groups (append sorted-groups (list (cons prefix sorted-group))))
    )
    
    ;; Combine groups in correct order: numeric first, then F, P, V, OTHER
    (setq result '())
    (setq result (append result (cdr (assoc "" sorted-groups))))   ;; No prefix first  
    (setq result (append result (cdr (assoc "F" sorted-groups))))   ;; F items next
    (setq result (append result (cdr (assoc "P" sorted-groups))))   ;; P items 
    (setq result (append result (cdr (assoc "V" sorted-groups))))   ;; V items
    (setq result (append result (cdr (assoc "OTHER" sorted-groups)))) ;; Other items last
    
    (prompt (strcat "\n✅ Final sorted result: " (itoa (length result)) " records"))
    result
  )
  
    ;; Helper function to get available prefixes from records
(defun MBS:get-available-prefixes-from-records (records header num-index / prefixes)
    (setq prefixes '())
    
    ;; Loop through records to find all prefixes
    (foreach record records
    (if (>= (length record) (1+ num-index))
      (progn
      (setq item-no (nth num-index record))
      (if (and item-no (/= item-no ""))
        (progn
        ;; Extract prefix (non-numeric chars at start)
        (setq prefix "")
        (setq i 0)
        (while (and (< i (strlen item-no)) 
               (not (is-digit (substr item-no (1+ i) 1))))
          (setq prefix (strcat prefix (substr item-no (1+ i) 1)))
          (setq i (1+ i))
        )
        
        ;; Add prefix to list if not already there
        (if (and prefix (not (member prefix prefixes)))
          (setq prefixes (cons prefix prefixes))
        )
        )
      )
      )
    )
    )
    
    ;; Sort and return the prefixes
    (vl-sort prefixes '<)
  )
  
    ;; Extract numeric portion of item number
(defun MBS:extract-item-number (item-no)
    (setq num-part 0)
    
    ;; Find first digit
    (setq i 0)
    (while (and (< i (strlen item-no)) 
         (not (is-digit (substr item-no (1+ i) 1))))
    (setq i (1+ i))
    )
    
    ;; Extract numeric part
    (if (< i (strlen item-no))
    (setq num-part (atoi (substr item-no (1+ i))))
    )
    
    num-part
  )
  
  ;; Function to get a descriptor for a prefix
(defun MBS:get-prefix-descriptor (prefix)
    (setq descriptor (cdr (assoc prefix MBS:prefix-descriptors)))
    (if descriptor
    descriptor
    (strcat prefix " Items")  ;; Default descriptor if not found
    )
  )

  ;; Function to add or update a prefix descriptor
(defun MBS:add-prefix-descriptor (prefix descriptor)
    (setq existing (assoc prefix MBS:prefix-descriptors))
    (if existing
    (setq MBS:prefix-descriptors (subst (cons prefix descriptor) existing MBS:prefix-descriptors))
    (setq MBS:prefix-descriptors (cons (cons prefix descriptor) MBS:prefix-descriptors))
    )
  )
  
  ;; Prefix descriptor mapping
  (if (not (boundp 'MBS:prefix-descriptors))
    (setq MBS:prefix-descriptors
    '(
      ("" . "Normal Items")
      ("P" . "Piping")
      ("V" . "Valves")
      ("F" . "Fittings")
      ("R" . "Removals")
      ("E" . "Equipment")
      ("A" . "Assembly")
    )
    )
  )
  
    ;; Group records by prefix - WITH DEBUG
(defun MBS:group-records-by-prefix (records num-index desc-index / result row-index record item-no description prefix i existing-group found-group)
    (prompt "\n🔍 Grouping records by prefix...")
    (setq result '())
    ;; Skip detailed debug output during normal workflow
    (setq MBS:debug-prefix-first-call t)  ;; Reset debug flag for new grouping session
    
    ;; Handle nil indices safely
    (if (or (not num-index) (not desc-index))
    (progn
      (prompt "\nWARNING: Missing required column indices for grouping records")
      result  ;; Return empty result
    )
    (progn
      ;; Index of each record in original list
      (setq row-index 0)
      
      ;; Process each record
      (foreach record records
      (if (>= (length record) (1+ num-index))
        (progn
        ;; Get existing item number or determine from description
        (setq item-no (nth num-index record))
        ;; Ensure item-no is a string for processing
        (if item-no 
          (setq item-no (vl-princ-to-string item-no))
          (setq item-no "")
        )
        (setq description (if (>= (length record) (1+ desc-index)) (nth desc-index record) ""))
        ;; Ensure description is a string
        (if (not (= (type description) 'STR))
          (setq description (if description (vl-princ-to-string description) ""))
        )
        

        
              ;; Extract or detect prefix
      (if (and item-no (/= item-no "") (/= item-no "TBD"))
        ;; Extract from existing item number
        (progn
        (setq prefix "")
        (setq i 0)
        (while (and (< i (strlen item-no)) 
               (not (is-digit (substr item-no (1+ i) 1))))
          (setq prefix (strcat prefix (substr item-no (1+ i) 1)))
          (setq i (1+ i))
        )

        )
        ;; Detect from description if possible
        (progn
        (if (and description (/= description ""))
          (progn
            (setq prefix (detect-prefix-from-description description))
            ;; Prefix detection completed (debug output reduced for performance)
          )
          (setq prefix "")  ;; Default if no description
        )
        )
      )
        
        ;; Add to the corresponding prefix group with record index
        (setq prefix-pair (assoc prefix result))
        (if prefix-pair
          ;; Add to existing group
          (setq result (MBS:update-assoc-value 
                 result 
                 prefix 
                 (append (cdr prefix-pair) (list (cons row-index record)))))
          ;; Create new group
          (setq result (append result (list (cons prefix (list (cons row-index record))))))
        )
        )
      )
      
      (setq row-index (1+ row-index))
      )
      
      ;; VALIDATION: Ensure no records were lost during grouping
      (setq total-grouped 0)
      (foreach prefix-group result
        (setq total-grouped (+ total-grouped (length (cdr prefix-group))))
      )
      (prompt (strcat "\n✓ Grouped " (itoa total-grouped) " records into " (itoa (length result)) " prefix groups"))
      (if (/= total-grouped (length records))
        (prompt (strcat "\n⚠ WARNING: Record count mismatch! Input: " (itoa (length records)) " Grouped: " (itoa total-grouped)))
      )
      
      result
    )
    )
  )
    
    ;; Improved preview function for item number changes that only shows real changes
(defun MBS:preview-renumber-prefix-group (group-records original-records num-index prefix / changes)
    (setq changes '())
    
    ;; Get header from original records
    (setq header (car original-records))
    
    ;; Find description column index in header
    (setq desc-index (get-column-index header "DESCRIPTION"))
    
    ;; Sort records by existing number if available
    (setq sorted-records 
      (vl-sort group-records
          '(lambda (a b) 
             (< (MBS:get-sort-value (cdr a) num-index)
              (MBS:get-sort-value (cdr b) num-index)))))
    
    ;; Generate new numbers
    (setq new-index 1)
    (foreach rec sorted-records
    (setq row-index (car rec))
    (setq record (cdr rec))
    
    ;; Get current item number
    (setq current-item-no 
        (if (and (>= (length record) (1+ num-index)))
        (nth num-index record)
        ""))
    
    ;; Get description directly from record
    (setq description 
        (if (and desc-index (>= (length record) (1+ desc-index)))
        (MBS:clean-attribute-text (nth desc-index record))
        ""))
    
    ;; Generate new item number
    (setq new-item-no (strcat prefix (itoa new-index)))
    
    ;; Only add to changes list if there's an actual change
    (if (or (= current-item-no "") 
        (= current-item-no "TBD")
        (/= current-item-no new-item-no))
      (setq changes (append changes (list (list current-item-no new-item-no description))))
    )
    
    ;; Increment for next item
    (setq new-index (1+ new-index))
    )
    
    (list changes)
  )
    
    ;; Apply renumbering to a prefix group
(defun MBS:apply-renumber-prefix-group (group-records original-records header num-index prefix / modified)
    (setq modified nil)
    
    ;; Sort records by existing number if available
    (setq sorted-records 
      (vl-sort group-records
          '(lambda (a b) 
             (< (MBS:get-sort-value (cdr a) num-index)
              (MBS:get-sort-value (cdr b) num-index)))))
    
    ;; Apply new numbers
    (setq new-index 1)
    (foreach rec sorted-records
    (setq row-index (car rec))
    (setq record (cdr rec))
    
    ;; Get current item number
    (setq current-item-no 
        (if (and (>= (length record) (1+ num-index))
            (nth num-index record))
        (nth num-index record)
        ""))
    
    ;; Generate new item number
    (setq new-item-no (strcat prefix (itoa new-index)))
    
    ;; If different, update the record
    (if (or (= current-item-no "") 
        (= current-item-no "TBD")
        (/= current-item-no new-item-no))
      (progn
      ;; Update the record in the original records list
      (if (< row-index (length original-records))
        (progn
        (setq updated-record (MBS:replace-at-index 
                   (nth row-index original-records)
                   num-index
                   new-item-no))
        (setq original-records (MBS:replace-at-index 
                    original-records
                    row-index
                    updated-record))
        (setq modified t)
        )
      )
      )
    )
    
    ;; Increment for next item
    (setq new-index (1+ new-index))
    )
    
    (list original-records modified)
  )
  
  (prompt "\nItem prefix shortening support functions loaded!")
    
    ;;; ========================================================================
    ;; BLOCK OPERATION UTILITIES
    ;;; ========================================================================
    
    ;; Insert block with attributes
(defun MBS:insert-block-with-attributes (space point material-id desc item-no / blkRef)
    (vl-catch-all-apply
    '(lambda ()
      (setq blkRef (vla-InsertBlock 
             space
             (vlax-3d-point (car point) (cadr point) 0.0)
             (get-block-name)
             1.0
             1.0
             1.0
             0.0))
             
      ;; Set attributes
      (setq attList (vlax-invoke blkRef 'GetAttributes))
      (foreach att attList
      (setq tag (strcase (vlax-get att 'TagString)))
      (cond
        ((= tag (strcase id-attribute))
         (vlax-put att 'TextString material-id))
        ((= tag "##") 
         (vlax-put att 'TextString item-no))
        ((= tag "DESCRIPTION") 
         (if (boundp 'shorten-description)
         (vlax-put att 'TextString (shorten-description desc))
         (vlax-put att 'TextString desc)))
      )
      )
      
      ;; Update block
      (vla-Update blkRef)
      blkRef
    )
    )
  )
    
    ;; Build material ID map
(defun MBS:build-material-id-map ( / doc spaceList blockMap empty-id-blocks space obj id-value valA valD att tag attList
                    block-name id-attribute total-blocks progress-counter)
    ;; Get the configuration for current mode
    (setq block-name (get-block-name))
    (setq id-attribute (get-id-attribute))
    
    (setq blockMap '())
    (setq empty-id-blocks '()) ;; NEW: Track blocks with empty/missing IDs separately
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    
    ;; Start with Model Space
    (setq spaceList (list (vla-get-ModelSpace doc)))
    
    ;; Add all Paper Spaces (layouts) using a more compatible approach
    (setq layoutDict (vla-get-Layouts doc))
    (if layoutDict
    (progn
      (vlax-for layout layoutDict
      ;; Get the block associated with each layout
      (setq layoutBlock (vl-catch-all-apply '(lambda () (vla-get-Block layout))))
      (if (and (not (vl-catch-all-error-p layoutBlock))
           layoutBlock)
        ;; Add to space list if it's not the model space
        (progn
        ;; Try to get layout name safely
        (setq layoutName (vl-catch-all-apply '(lambda () (vlax-get layout 'Name))))
        (if (and (not (vl-catch-all-error-p layoutName))
             (/= layoutName "Model"))
          (setq spaceList (append spaceList (list layoutBlock)))
        )
        )
      )
      )
    )
    )
    
    ;; Progress counter for feedback
    (setq progress-counter 0)
    (setq total-blocks 0)
    
    ;; First pass to count total blocks
    (foreach space spaceList
    (vlax-for obj space
      (if (and (eq (vla-get-ObjectName obj) "AcDbBlockReference")
           (= (vlax-get obj 'EffectiveName) block-name)
           (not (vlax-erased-p obj)))
      (setq total-blocks (1+ total-blocks))
      )
    )
    )
    
    (prompt (strcat "\n[MAP] Found " (itoa total-blocks) " total blocks to process across "
           (itoa (length spaceList)) " spaces"))
    
    ;; Process all spaces
    (foreach space spaceList
    (vlax-for obj space
      (if (and (eq (vla-get-ObjectName obj) "AcDbBlockReference")
           (= (vlax-get obj 'EffectiveName) block-name)
           (not (vlax-erased-p obj)))
      (progn
        (setq id-value nil valA "" valD "")
        (setq attList (vlax-invoke obj 'GetAttributes))
        
        ;; Show progress occasionally
        (setq progress-counter (1+ progress-counter))
        (if (= (rem progress-counter 10) 0)
        (prompt (strcat "\r[MAP] Processing block " (itoa progress-counter) 
                 " of " (itoa total-blocks) "... "))
        )
        
        ;; Extract attribute values
        (foreach att attList
        (setq tag (strcase (vlax-get att 'TagString)))
        (cond
          ((= tag (strcase id-attribute)) (setq id-value (vlax-get att 'TextString)))
          ((= tag "##") (setq valA (clean-attribute-text (vlax-get att 'TextString))))
          ((= tag "DESCRIPTION") (setq valD (clean-attribute-text (vlax-get att 'TextString))))
        )
        )
        
        ;; Check if it has a valid non-empty ID
        (if (and id-value (/= id-value ""))
        (progn
          ;; If this ID exists in map, append to list, otherwise create new list
          (setq existing-refs (assoc id-value blockMap))
          (if existing-refs
          (setq blockMap (subst (cons id-value (append (cdr existing-refs) (list obj))) 
                     existing-refs blockMap))
          (setq blockMap (cons (cons id-value (list obj)) blockMap))
          )
        )
        ;; NEW: Track blocks with empty IDs separately
        (setq empty-id-blocks (append empty-id-blocks (list obj)))
        )
      )
      )
    )
    )
    
    (prompt (strcat "\n[MAP] Found " (itoa (length blockMap)) " unique Material IDs"))
    
    ;; NEW: Add information about empty ID blocks
    (if (> (length empty-id-blocks) 0)
    (prompt (strcat "\n[MAP] Also found " (itoa (length empty-id-blocks)) " blocks with empty Material IDs"))
    )
    
    ;; Return both the map and empty ID blocks
    (list blockMap empty-id-blocks)
  )
    
    ;; Find potential CSV matches for blocks
(defun MBS:find-potential-csv-matches (block records header / desc exact-matches fuzzy-matches)
    ;; Get block attributes
    (setq att-list (vlax-invoke block 'GetAttributes))
    
    ;; Extract description from block
    (setq desc "")
    (foreach att att-list
    (if (= (strcase (vlax-get att 'TagString)) "DESCRIPTION")
      (setq desc (MBS:clean-attribute-text (vlax-get att 'TextString)))
    )
    )
    
    ;; If no description, return nil
    (if (= desc "") 
    nil
    (progn
      ;; Find description column index
      (setq desc-index (get-column-index header "DESCRIPTION"))
      (if (not desc-index)
      nil
      (progn
        (setq exact-matches '())
        (setq fuzzy-matches '())
        
        ;; Find material ID column index
        (setq mat-id-index (get-column-index header (get-id-column)))
        
        ;; Loop through records to find matches
        (setq i 0)
        (foreach record records
        (if (and (>= (length record) (1+ desc-index))
            (>= (length record) (1+ mat-id-index)))
          (progn
          (setq csv-desc (nth desc-index record))
          (setq material-id (nth mat-id-index record))
          
          ;; Check for exact match
          (if (= (strcase csv-desc) (strcase desc))
            (setq exact-matches (append exact-matches (list (list i material-id csv-desc))))
            ;; Check for fuzzy match (block desc is shortened CSV desc)
            (if (MBS:is-shortened-description desc csv-desc)
            (setq fuzzy-matches (append fuzzy-matches (list (list i material-id csv-desc))))
            )
          )
          )
        )
        (setq i (1+ i))
        )
        
        ;; Return matches, prioritizing exact matches
        (if (> (length exact-matches) 0)
        (cons 'exact exact-matches)
        (if (> (length fuzzy-matches) 0)
          (cons 'fuzzy fuzzy-matches)
          nil
        )
        )
      )
      )
    )
    )
  )
  
    (prompt "\nBlock operations support functions loaded!")
    
    ;;; ========================================================================
    ;; HELPER FUNCTIONS
    ;;; ========================================================================
    
    ;; Set the value in a record at a specific column name
(defun MBS:set-record-value (record header column-name value / index new-record)
    (setq index (get-column-index header column-name))
    (if (and index (>= index 0))
    (progn
      (setq new-record '())
      (setq i 0)
      
      ;; Make sure record is at least as long as the index we need to set
      (while (< (length record) (1+ index))
      (setq record (append record (list "")))
      )
      
      ;; Build new record with the value at the right index
      (foreach item record
      (if (= i index)
        (setq new-record (append new-record (list value)))
        (setq new-record (append new-record (list item)))
      )
      (setq i (1+ i))
      )
      new-record
    )
    record  ;; Return unchanged if column not found
    )
  )

;; Preview ITEM NO changes (for when auto-accept is disabled)
(defun MBS:preview-item-number-changes (csv-path / csv-data header records prefix-groups changes-needed)
  "Generate and show preview of ITEM NO changes"
  
  ;; Read current CSV data
  (setq csv-data (MBS:read-csv csv-path))
  (if (not csv-data)
    (progn
      (prompt "\n❌ Failed to read CSV for preview")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Get column indices
      (setq num-index (get-column-index header "ITEM NO."))
      (setq desc-index (get-column-index header "DESCRIPTION"))
      
      (if (and num-index desc-index)
        (progn
          ;; Group records by prefix
          (setq prefix-groups (MBS:group-records-by-prefix records num-index desc-index))
          (setq changes-needed nil)
          
          (prompt "\n🔍 === ITEM NO CHANGE PREVIEW ===")
          
          ;; Check each prefix group for changes
          (foreach prefix-group prefix-groups
            (setq prefix (car prefix-group))
            (setq group-records (cdr prefix-group))
            (setq group-count (length group-records))
            
            (if (> group-count 0)
              (progn
                (prompt (strcat "\n📋 Prefix '" prefix "': " (itoa group-count) " items → " prefix "1, " prefix "2, ... " prefix (itoa group-count)))
                (setq changes-needed t)
              )
            )
          )
          
          (if changes-needed
            (prompt (strcat "\n📊 Total: " (itoa (length records)) " records will be renumbered"))
            (prompt "\n✅ No ITEM NO changes needed")
          )
          
          changes-needed
        )
        (progn
          (prompt "\n❌ Required columns not found for preview")
          nil
        )
      )
    )
  )
)

;; Helper function to replace element at index in list
(defun MBS:replace-nth (lst n new-val / result i)
  "Replace the nth element in a list with a new value"
  (setq result '())
  (setq i 0)
  
  (foreach item lst
    (if (= i n)
      (setq result (append result (list new-val)))
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  
  result
)

;; Helper function for integer division with ceiling result
(defun MBS:ceiling-div (a b)
  (if (= (rem a b) 0)
    (/ a b)
    (1+ (/ (- a (rem a b)) b))
  )
)

(prompt "\n✅ Helper functions loaded!")

;;; ========================================================================
;; ENHANCED PREFIX MAPPING SYSTEM
;;; ========================================================================

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

;; Drawing Type H prefix map (HVAC)
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
    (t prefix-keyword-map-P) ;; Default to P (Piping) for fitting-heavy drawings
  )
)

;; Enhanced function to detect prefix from description with LOR mode support
(defun detect-prefix-from-description (description / description-upper keyword-part keyword pair prefix-map found-prefix comma-pos)
  ;; Global debug flag for first call only
  (if (not MBS:debug-prefix-first-call) (setq MBS:debug-prefix-first-call t))
  
  ;; If in LOR mode, always use "R" prefix
  (if (= current-mode "LOR")
    "R"
    ;; Otherwise use LOM mode logic with enhanced mapping
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
        
        ;; DEBUG: Show detection details for first call only (commented for cleaner output)
        ;(if MBS:debug-prefix-first-call
        ;  (progn
        ;    (prompt (strcat "\n[DEBUG] Drawing type: '" (if current-drawing-type current-drawing-type "UNDEFINED") "'"))
        ;    (prompt (strcat "\n[DEBUG] Keyword part: '" keyword-part "' → Upper: '" description-upper "'"))
        ;    (prompt (strcat "\n[DEBUG] Prefix map length: " (itoa (length prefix-map))))
        ;    (setq MBS:debug-prefix-first-call nil)
        ;  )
        ;)
        
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
)

;; generate unique material id or removal id based on current mode
(defun generate-unique-id (description / hash-value i char timestamp)
  (setq hash-value 0)
  (setq i 1)
  
  ;; Create a hash from the description
  (while (<= i (strlen description))
    (setq char (ascii (substr description i 1)))
    (setq hash-value (+ hash-value (* char i)))
    (setq i (1+ i))
  )
  
  ;; Add timestamp component for uniqueness
  (setq timestamp (getvar "DATE"))  ;; Current date/time as Julian value
  (setq hash-value (+ hash-value (* (- timestamp (fix timestamp)) 1000000)))
  
  ;; Format as a unique ID with appropriate prefix based on current mode
  (if (= current-mode "LOR")
    (strcat "REM-" (rtos hash-value 2 0))  ;; LOR mode: REM- prefix
    (strcat "MAT-" (rtos hash-value 2 0))  ;; LOM mode: MAT- prefix
  )
)

;; Clean attribute text function
(defun clean-attribute-text (text / clean-text)
  (if (and text (> (strlen text) 0))
    (progn
      (setq clean-text text)
      
      ;; Check if text starts with a backslash
      (if (= (substr clean-text 1 1) "\\")
        (progn
          ;; Find the position of the semicolon that terminates the formatting code
          (setq pos (vl-string-search ";" clean-text))
          (if pos
            ;; Return everything after the semicolon
            (setq clean-text (substr clean-text (+ pos 2)))
          )
        )
      )
      
      ;; Return the cleaned text
      (vl-string-trim " \t\n\r" clean-text)
    )
    ;; Return empty string if text is nil or empty
    ""
  )
)

;; Helper function to determine the current active space (model or paper)
(defun MBS:get-current-space (/ doc)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  
  ;; Check if we're in paper space
  (if (= (getvar "TILEMODE") 0)
    ;; We're in paper space
    (list (vla-get-PaperSpace doc) "Paper")
    ;; We're in model space
    (list (vla-get-ModelSpace doc) "Model")
  )
)

;; Prompt for row selection
(defun MBS:prompt-for-single-row-selection (records header / num-index desc-index mat-id-index)
    ;; Get indices
    (setq num-index (get-column-index header "ITEM NO."))
    (setq desc-index (get-column-index header "DESCRIPTION"))
    (setq mat-id-index (get-column-index header (strcase id-attribute)))
    
    ;; Display rows
    (prompt "\n\n=== SELECT ROW TO INSERT ===")
    (setq i 1)
    (foreach record records
    (prompt (strcat "\n" (itoa i) ": " 
             (nth num-index record) " - " 
             (nth desc-index record) " - " 
             (nth mat-id-index record)))
    (setq i (1+ i))
    )
    
    ;; Prompt for selection
    (prompt "\n\nEnter row number to insert (or 0 to cancel): ")
    (setq row-num (getint))
    
    (if (and (> row-num 0) (<= row-num (length records)))
    (nth (1- row-num) records)
    nil
    )
  )
    
    ;; Parse number list
(defun MBS:parse-number-list (input-str / result number current char)
    (setq result '())
    (setq current "")
    (setq input-str (strcat input-str ","))  ;; Add trailing comma for parsing
    
    (setq i 0)
    (while (< i (strlen input-str))
    (setq char (substr input-str (1+ i) 1))
    
    (if (= char ",")
      (progn
      ;; Process current number
      (if (/= current "")
        (progn
        (setq number (atoi current))
        (if (> number 0)
          (setq result (append result (list number)))
        )
        (setq current "")
        )
      )
      )
      ;; Add character to current if it's a digit
      (if (is-digit char)
      (setq current (strcat current char))
      )
    )
    
    (setq i (1+ i))
    )
    
    result
  )
    
  ;; Helper for sorting - get numeric value from item number or description
(defun MBS:get-sort-value (record num-index / item-no num-part)
    (if (and (>= (length record) (1+ num-index))
         (nth num-index record)
         (/= (nth num-index record) ""))
    ;; Extract number part if item number exists
    (progn
      (setq item-no (nth num-index record))
      (if (= item-no "TBD")
      9999  ;; Large value for TBD
      (MBS:extract-item-number item-no)
      )
    )
    ;; Default high value for empty/missing
    9999
    )
  )
  
  (prompt "\n✅ Prefix mapping functions loaded!")
  
  ;;; ========================================================================
  ;; IMPROVED ITEM NUMBERING SYSTEM
  ;;; ========================================================================
  
  ;; Function to detect and handle empty ITEM NO. fields - WITH DEBUG
(defun MBS:detect-empty-item-numbers (id-map records header / needs-numbering num-index desc-index mat-id-index prefix-records row-index empty-count block-match-count item-no material-id desc detected-prefix prefix-pair)
    (prompt "\n🔍 DEBUG: === EMPTY ITEM NUMBER DETECTION ===")
    (prompt (strcat "\n🔍 DEBUG: Current mode: " current-mode))
    (prompt (strcat "\n🔍 DEBUG: ID attribute: " id-attribute))
    (prompt (strcat "\n🔍 DEBUG: Records count: " (itoa (length records))))
    (prompt (strcat "\n🔍 DEBUG: ID map count: " (itoa (length id-map))))
    
    ;; Get column indices
    (setq num-index (CSV:get-column-index header "ITEM NO."))
    (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
    (setq mat-id-index (CSV:get-column-index header (strcase id-attribute)))
    
    (prompt (strcat "\n🔍 DEBUG: Column indices - ITEM NO.: " (if num-index (itoa num-index) "NIL")))
    (prompt (strcat "\n🔍 DEBUG: Column indices - DESCRIPTION: " (if desc-index (itoa desc-index) "NIL")))
    (prompt (strcat "\n🔍 DEBUG: Column indices - " (strcase id-attribute) ": " (if mat-id-index (itoa mat-id-index) "NIL")))
    
    (if (or (not num-index) (not desc-index) (not mat-id-index))
    (progn
      (prompt "\n❌ DEBUG: Missing required columns for item number check")
      nil
    )
    (progn
      ;; Group by prefix for consistent numbering
      (setq prefix-records '())
      
      ;; Find records with empty item numbers but with matching blocks
      (setq row-index 0)
      (setq empty-count 0)
      (setq block-match-count 0)
      (foreach record records
      (setq item-no (if (and (>= (length record) (1+ num-index)))
              (nth num-index record)
              ""))
      
      (setq material-id (if (and (>= (length record) (1+ mat-id-index)))
                (nth mat-id-index record)
                ""))
      
      ;; Debug first 3 records
      (if (< row-index 3)
        (progn
        (prompt (strcat "\n🔍 DEBUG: Row " (itoa row-index) " - ITEM NO.: '" item-no "'"))
        (prompt (strcat "\n🔍 DEBUG: Row " (itoa row-index) " - " (strcase id-attribute) ": '" material-id "'"))
        )
      )
      
      ;; Count empty item numbers
      (if (or (= item-no "") (= item-no nil))
        (setq empty-count (1+ empty-count))
      )
      
      ;; Check if this record has a matching block
      (if (and (/= material-id "") (assoc material-id id-map))
        (progn
        (setq block-match-count (1+ block-match-count))
        (if (< row-index 3)
          (prompt (strcat "\n🔍 DEBUG: Row " (itoa row-index) " has matching block"))
        )
        ;; This record has a matching block
        (if (or (= item-no "") (= item-no nil))
          (progn
          ;; Empty item number - needs renumbering
          (setq desc (if (and (>= (length record) (1+ desc-index)))
                 (nth desc-index record)
                 "Unknown"))
          
          ;; Detect prefix
          (setq detected-prefix (detect-prefix-from-description desc))
          
          ;; Add to prefix records list
          (setq prefix-pair (assoc detected-prefix prefix-records))
          (if prefix-pair
            ;; Add to existing group
            (setq prefix-records (MBS:update-assoc-value 
                       prefix-records 
                       detected-prefix 
                       (append (cdr prefix-pair) 
                          (list (cons row-index record)))))
            ;; Create new group
            (setq prefix-records 
              (append prefix-records 
                   (list (cons detected-prefix 
                        (list (cons row-index record))))))
          )
          )
        )
        )
      )
      
      (setq row-index (1+ row-index))
      )
      
      ;; Process each prefix group
      (setq needs-numbering nil)
      (foreach prefix-pair prefix-records
      (setq prefix (car prefix-pair))
      (setq records-to-number (cdr prefix-pair))
      
      (if (> (length records-to-number) 0)
        (progn
        (setq needs-numbering t)
        (prompt (strcat "\nDetected " (itoa (length records-to-number)) 
                 " items with empty ITEM NO. fields (prefix: " prefix ")"))
        
        ;; Queue renumbering for this prefix
        (MBS:queue-transaction (MBS:tx-renumber-empty-items prefix records-to-number))
        )
      )
      )
      
      ;; Debug summary
      (prompt (strcat "\n🔍 DEBUG: === SUMMARY ==="))
      (prompt (strcat "\n🔍 DEBUG: Empty item numbers found: " (itoa empty-count)))
      (prompt (strcat "\n🔍 DEBUG: Records with matching blocks: " (itoa block-match-count)))
      (prompt (strcat "\n🔍 DEBUG: Prefix groups created: " (itoa (length prefix-records))))
      (prompt (strcat "\n🔍 DEBUG: Needs numbering: " (if needs-numbering "YES" "NO")))
      
      ;; Return whether numbering is needed
      needs-numbering
    )
    )
  )
  
  ;; Display loading message
(prompt "\ndetect-empty-item-numbers function loaded.")
  
  ;; Reset the global prefix map when analyzing numbers
(defun MBS:analyze-item-numbers (records header / num-index prefix-map)
    (MBS:log-info "Analyzing existing item numbers in CSV")
    
    ;; Find the ITEM NO column
    (setq num-index (get-column-index header "ITEM NO."))
    (if (not num-index)
    (progn
      (MBS:log-warning "ITEM NO. column not found during analysis")
      (list (cons "" 0) (cons "P" 0) (cons "V" 0) (cons "F" 0))  ;; Default empty map
    )
    (progn
      ;; Initialize prefix map with common prefixes
      (setq prefix-map (list (cons "" 0) (cons "P" 0) (cons "V" 0) (cons "F" 0)))
      
      ;; Scan all records to find highest number for each prefix
      (foreach record records
      (if (and (>= (length record) (1+ num-index))
           (nth num-index record)
           (/= (nth num-index record) ""))
        (progn
        (setq item-no (nth num-index record))
        
        ;; Extract prefix and number
        (setq prefix "")
        (setq i 0)
        
        ;; Extract prefix (non-digit characters at start)
        (while (and (< i (strlen item-no)) 
               (not (is-digit (substr item-no (1+ i) 1))))
          (setq prefix (strcat prefix (substr item-no (1+ i) 1)))
          (setq i (1+ i))
        )
        
        ;; Extract number part (digits after prefix)
        (setq num-part (substr item-no (1+ i)))
        (setq num-val (atoi num-part))
        (if (= num-val 0) (setq num-val 1))  ;; Ensure at least 1 for invalid numbers
        
        ;; Update max for this prefix
        (setq current-max (cdr (assoc prefix prefix-map)))
        (if (not current-max) 
          (setq prefix-map (cons (cons prefix num-val) prefix-map))
          (if (> num-val current-max)
          (setq prefix-map 
              (subst (cons prefix num-val) 
                 (cons prefix current-max) 
                 prefix-map))
          )
        )
        )
      )
      )
      
      ;; Reset global map with our new map
      (setq MBS:global-prefix-map prefix-map)
      
      (MBS:log-info "Updated item number counters from CSV analysis")
      (foreach pm prefix-map
      (MBS:log-verbose (strcat "  Prefix '" (car pm) "' max number: " (itoa (cdr pm))))
      )
      prefix-map
    )
    )
)
	
	  ;; Display loading message
(prompt "\nanalyze-item-numbers function loaded.")

;; Display loading message
(prompt "\nMaterial Block Sync Block Operations module loaded.")

;; Reload the blocks module
(defun c:MBS-ReloadBlocks ()
  (setq MBS:blocks-loaded nil)  ;; Reset the loading flag
  (load "mbs_blocks")          ;; Reload the file
  (prompt "\nMaterial Block Sync Block Operations module reloaded.")
  (princ)
)
(princ)

;; Enhanced error handling function
(defun MBS:handle-operation-error (operation error-msg / choice)
  (prompt (strcat "\n⚠ ERROR during " operation " operation"))
  (prompt (strcat "\nError details: " error-msg))
  (prompt "\n\nWhat would you like to do?")
  (initget "Continue Skip Retry")
  (setq choice (getkword "\nOptions: (C)ontinue with next item, (S)kip all remaining, (R)etry this operation: "))
  
  (cond
  ((= choice "Continue") 'continue)
  ((= choice "Skip") 'skip-all)
  ((= choice "Retry") 'retry)
  (t 'continue)  ;; Default to continue
  )
)

;; Safe block attribute extraction with error handling
(defun MBS:get-block-attributes-safe (block / att-list)
  (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke block 'GetAttributes))))
  (if (vl-catch-all-error-p att-list)
  (progn
    (MBS:handle-operation-error "block attribute extraction" (vl-catch-all-error-message att-list))
    nil
  )
  att-list
  )
)

;; Safe attribute value setting with error handling
(defun MBS:set-attribute-value-safe (att value / result)
  (setq result (vl-catch-all-apply '(lambda () (vlax-put att 'TextString value))))
  (if (vl-catch-all-error-p result)
  (progn
    (MBS:handle-operation-error "attribute value setting" (vl-catch-all-error-message result))
    nil
  )
  t
  )
)

;; Safe block update with error handling
(defun MBS:update-block-safe (block / result)
  (setq result (vl-catch-all-apply '(lambda () (vla-Update block))))
  (if (vl-catch-all-error-p result)
  (progn
    (MBS:handle-operation-error "block update" (vl-catch-all-error-message result))
    nil
  )
  t
  )
)

  ;; Enhanced CSV operation with error recovery
(defun MBS:execute-csv-operation-with-recovery (csv-path operation-fn / result error-occurred)
    (setq error-occurred nil)
    (setq result (vl-catch-all-apply operation-fn))
    
    (if (vl-catch-all-error-p result)
    (progn
      (setq error-occurred t)
      (setq error-msg (vl-catch-all-error-message result))
      (prompt (strcat "\n⚠ CSV operation failed: " error-msg))
      
      ;; Offer recovery options
      (initget "Retry Skip Abort")
      (setq choice (getkword "\nOptions: (R)etry operation, (S)kip this step, (A)bort entire process: "))
      
      (cond
      ((= choice "Retry")
       (prompt "\nRetrying CSV operation...")
       (MBS:execute-csv-operation-with-recovery csv-path operation-fn)
      )
      ((= choice "Skip")
       (prompt "\nSkipping this CSV operation. Some data may be inconsistent.")
       nil
      )
      ((= choice "Abort")
       (prompt "\nAborting entire process due to CSV error.")
       nil
      )
      (t nil)
      )
    )
    (cadr result)  ;; Return the result if successful
    )
  )

  ;; Performance optimization: Batch processing function
(defun MBS:process-blocks-in-batches (blocks process-fn batch-size / batch results)
    (setq results '())
    (setq batch '())
    (setq batch-count 0)
    
    (foreach block blocks
    (setq batch (append batch (list block)))
    (setq batch-count (1+ batch-count))
    
    ;; Process batch when it reaches the specified size
    (if (>= batch-count batch-size)
      (progn
      (setq batch-result (vl-catch-all-apply process-fn (list batch)))
      (if (not (vl-catch-all-error-p batch-result))
        (setq results (append results batch-result))
      )
      (setq batch '())
      (setq batch-count 0)
      )
    )
    )
    
    ;; Process remaining blocks
    (if (> batch-count 0)
    (progn
      (setq batch-result (vl-catch-all-apply process-fn (list batch)))
      (if (not (vl-catch-all-error-p batch-result))
      (setq results (append results batch-result))
      )
    )
    )
    
    results
  )

  ;; Performance optimization: Caching system
(defun MBS:get-cached-value (cache-key / cached-value)
    (if (and (boundp 'MBS:cache) (assoc cache-key MBS:cache))
    (cdr (assoc cache-key MBS:cache))
    nil
    )
  )

(defun MBS:set-cached-value (cache-key value / cache-size)
    ;; Initialize cache if not exists
    (if (not (boundp 'MBS:cache))
    (setq MBS:cache '())
    )
    
    ;; Add or update cache entry
    (setq MBS:cache (MBS:update-assoc-value MBS:cache cache-key value))
    
    ;; Limit cache size to prevent memory issues
    (setq cache-size (length MBS:cache))
    (if (> cache-size 100)  ;; Limit to 100 entries
    (setq MBS:cache (cdr MBS:cache))  ;; Remove oldest entry
    )
  )

  ;; Performance optimization: Cached column index lookup
(defun get-column-index-cached (header column-name / cache-key cached-index)
    (setq cache-key (strcat "col_" column-name))
    (setq cached-index (MBS:get-cached-value cache-key))
    
    (if cached-index
    cached-index
    (progn
      (setq index (get-column-index header column-name))
      (MBS:set-cached-value cache-key index)
      index
    )
    )
  )

  ;; Performance optimization: Batch CSV operations
(defun MBS:batch-csv-operations (csv-path operations / csv-data header records)
    ;; Read CSV once for all operations
    (setq csv-data (MBS:read-csv csv-path))
    (if (not csv-data)
    (progn
      (prompt "\n❌ Failed to read CSV for batch operations")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Apply all operations
      (foreach operation operations
      (setq result (vl-catch-all-apply operation (list records header)))
      (if (not (vl-catch-all-error-p result))
        (setq records (car result))
      )
      )
      
      ;; Write back to CSV once
            (MBS:write-csv csv-path header records nil)
    )
    )
  )

  ;; Performance optimization: Progress indicator
(defun MBS:show-progress (current total operation / percentage)
    (if (and (boundp 'MBS:show-progress-indicators) MBS:show-progress-indicators)
    (progn
      (setq percentage (fix (* (/ current total) 100)))
      (prompt (strcat "\r" operation ": " (itoa current) "/" (itoa total) " (" (itoa percentage) "%)"))
    )
    )
  )

;; ========================================================================
;; PHASE 6: CLEAN ARCHITECTURE - Item Number & Description Synchronization
;; ========================================================================

;; PHASE 6: Enhanced with user settings support
(defun MBS:phase6-clean (session-context / csv-path current-mode result)
  "Phase 6: Enhanced update process using user configuration settings"
  
  (prompt "\n📖 Reading current CSV state...")
  
  ;; Get session data  
  (setq csv-path (nth 0 session-context))
  (setq current-mode (nth 5 session-context))
  
  (prompt "\n🎯 === PHASE 6: ITEM NO & DESCRIPTION SYNCHRONIZATION ===")
  
  ;; Use the enhanced simple update process
  (setq result (MBS:execute-simple-update csv-path))
  
  (if result
    (progn
      (prompt "\n✅ Phase 6 completed successfully: Enhanced update process finished")
      1  ;; Return success count
    )
    (progn
      (prompt "\n⚠ Phase 6 had issues - check results")
      0
    )
  )
)

;; ========================================================================
;; USER CONFIGURATION SETTINGS
;; ========================================================================

;; Configuration variables for user control with synchronization to existing settings
(if (not (boundp 'MBS:enable-weight-calculation))
  (progn
    ;; Sync with existing total-weight-calculation variable if it exists
    (if (boundp 'total-weight-calculation)
      (setq MBS:enable-weight-calculation total-weight-calculation)
      (setq MBS:enable-weight-calculation t)  ;; Default: enabled
    )
  )
)

(if (not (boundp 'MBS:enable-auto-increment-item-numbers))
  (progn
    ;; Sync with existing auto-increment-item-numbers variable if it exists
    (if (boundp 'auto-increment-item-numbers)
      (setq MBS:enable-auto-increment-item-numbers auto-increment-item-numbers)
      (setq MBS:enable-auto-increment-item-numbers t)  ;; Default: enabled (smart update)
    )
  )
)

(if (not (boundp 'MBS:auto-accept-item-number-changes))
  (progn
    ;; Sync with existing auto-accept-item-number-changes variable if it exists
    (if (boundp 'auto-accept-item-number-changes)
      (setq MBS:auto-accept-item-number-changes auto-accept-item-number-changes)
      (setq MBS:auto-accept-item-number-changes t)  ;; Default: enabled (no preview)
    )
  )
)

;; Ensure reverse synchronization - update old variables to match new ones for compatibility
(if (boundp 'total-weight-calculation)
  (setq total-weight-calculation MBS:enable-weight-calculation)
)
(if (boundp 'auto-increment-item-numbers)
  (setq auto-increment-item-numbers MBS:enable-auto-increment-item-numbers)
)
(if (boundp 'auto-accept-item-number-changes)
  (setq auto-accept-item-number-changes MBS:auto-accept-item-number-changes)
)

;; Configuration commands for users
(defun C:MBS-CONFIG (/ choice)
  "Show and modify MBS configuration settings"
  
  (MBS:log-info "\n🔧 [MBS CONFIGURATION SETTINGS]")
  (prompt "\n")
  (prompt (strcat "\n1. Weight Calculation: " (if MBS:enable-weight-calculation "ENABLED" "DISABLED")))
  (prompt (strcat "\n2. Auto-Increment ITEM NO: " (if MBS:enable-auto-increment-item-numbers "ENABLED" "DISABLED")))
  (prompt (strcat "\n3. Auto-Accept Changes: " (if MBS:auto-accept-item-number-changes "ENABLED" "DISABLED")))
  (prompt "\n")
  (prompt "\nEnter number to toggle setting (1-3), or press Enter to exit:")
  
  (setq choice (getstring))
  
  (cond
    ((= choice "1")
     (setq MBS:enable-weight-calculation (not MBS:enable-weight-calculation))
     ;; Sync with old variable
     (if (boundp 'total-weight-calculation)
       (setq total-weight-calculation MBS:enable-weight-calculation)
     )
     (prompt (strcat "\n✅ Weight Calculation: " (if MBS:enable-weight-calculation "ENABLED" "DISABLED")))
    )
    ((= choice "2")
     (setq MBS:enable-auto-increment-item-numbers (not MBS:enable-auto-increment-item-numbers))
     ;; Sync with old variable
     (if (boundp 'auto-increment-item-numbers)
       (setq auto-increment-item-numbers MBS:enable-auto-increment-item-numbers)
     )
     (prompt (strcat "\n✅ Auto-Increment ITEM NO: " (if MBS:enable-auto-increment-item-numbers "ENABLED" "DISABLED")))
    )
    ((= choice "3")
     (setq MBS:auto-accept-item-number-changes (not MBS:auto-accept-item-number-changes))
     ;; Sync with old variable
     (if (boundp 'auto-accept-item-number-changes)
       (setq auto-accept-item-number-changes MBS:auto-accept-item-number-changes)
     )
     (prompt (strcat "\n✅ Auto-Accept Changes: " (if MBS:auto-accept-item-number-changes "ENABLED" "DISABLED")))
    )
    ((= choice "")
     (prompt "\n⏭ Configuration unchanged")
    )
    (t
     (prompt "\n❌ Invalid choice")
    )
  )
  
  (princ)
)

;; Quick toggle commands with synchronization
(defun C:MBS-TOGGLE-WEIGHTS ()
  "Quick toggle for weight calculation"
  (setq MBS:enable-weight-calculation (not MBS:enable-weight-calculation))
  ;; Sync with old variable
  (if (boundp 'total-weight-calculation)
    (setq total-weight-calculation MBS:enable-weight-calculation)
  )
  (prompt (strcat "\n✅ Weight Calculation: " (if MBS:enable-weight-calculation "ENABLED" "DISABLED")))
  (princ)
)

(defun C:MBS-TOGGLE-ITEMNO ()
  "Quick toggle for auto-increment item numbers"
  (setq MBS:enable-auto-increment-item-numbers (not MBS:enable-auto-increment-item-numbers))
  ;; Sync with old variable
  (if (boundp 'auto-increment-item-numbers)
    (setq auto-increment-item-numbers MBS:enable-auto-increment-item-numbers)
  )
  (prompt (strcat "\n✅ Auto-Increment ITEM NO: " (if MBS:enable-auto-increment-item-numbers "ENABLED" "DISABLED")))
  (princ)
)

(defun C:MBS-TOGGLE-PREVIEW ()
  "Quick toggle for auto-accept changes"
  (setq MBS:auto-accept-item-number-changes (not MBS:auto-accept-item-number-changes))
  ;; Sync with old variable
  (if (boundp 'auto-accept-item-number-changes)
    (setq auto-accept-item-number-changes MBS:auto-accept-item-number-changes)
  )
  (prompt (strcat "\n✅ Auto-Accept Changes: " (if MBS:auto-accept-item-number-changes "ENABLED" "DISABLED")))
  (princ)
)

;; ========================================================================
;; SIMPLE UPDATE COMMAND - FORMER ITEMNO-UPDATE WITH ENHANCED FEATURES
;; ========================================================================

;; Modular function for ITEM NO generation and CSV writing  
(defun MBS:update-item-numbers-modular (csv-path &optional skip-backup / result)
  "Modular function to update ITEM NO fields in CSV - can be called standalone or from other functions"
  
  (if (not skip-backup) (setq skip-backup nil))
  
  (prompt "\n🔢 [MODULAR ITEM NO UPDATE]")
  (prompt (strcat "\nProcessing: " csv-path))
  
  ;; Use the proven working function from INSERT3
  (setq result (MBS:update-all-item-numbers csv-path))
  
  (if result
    (prompt "\n✅ ITEM NO update completed successfully")
    (prompt "\n⏭ No ITEM NO changes were needed")
  )
  
  result
)

;; Modular function for block synchronization
(defun MBS:sync-blocks-modular (csv-path / result)
  "Modular function to sync blocks with CSV data - can be called standalone or from other functions"
  
  (prompt "\n🔄 [MODULAR BLOCK SYNC]")
  (prompt (strcat "\nSynchronizing blocks with: " csv-path))
  
  ;; Use the proven working function from INSERT3  
  (setq result (MBS:sync-blocks-with-csv csv-path))
  
  (if result
    (prompt "\n✅ Block synchronization completed successfully")
    (prompt "\n❌ Block synchronization failed")
  )
  
  result
)

;; Combined modular function for both ITEM NO and block updates
(defun MBS:update-itemno-and-sync-blocks (csv-path &optional skip-backup / itemno-result sync-result)
  "Combined modular function for ITEM NO updates and block synchronization"
  
  (prompt "\n📋 [COMBINED ITEM NO & BLOCK UPDATE]")
  (prompt (strcat "\nProcessing: " csv-path))
  
  ;; Step 1: Update ITEM NO fields in CSV
  (setq itemno-result (MBS:update-item-numbers-modular csv-path skip-backup))
  
  ;; Step 2: Sync blocks with updated CSV (always run, even if no ITEM NO changes)
  (setq sync-result (MBS:sync-blocks-modular csv-path))
  
  ;; Return combined result
  (and itemno-result sync-result)
)

;; Enhanced simple update function that performs comprehensive CSV and block updates
(defun MBS:execute-simple-update (csv-path / csv-data header records qty-index unit-wt-index total-wt-index itemno-result weight-result sync-result modified record qty unit-wt total-wt new-total-wt i)
  "Execute comprehensive simple update: item numbering, weight calculation, and block sync"
  
  (MBS:log-info "\n🔧 === EXECUTING SIMPLE UPDATE ===")
  
  ;; Step 1: Update item numbers with sorting if enabled
  (prompt "\n📋 Step 1: Updating item numbers and sorting...")
  (setq itemno-result (MBS:update-all-item-numbers csv-path))
  
  (if itemno-result
    (prompt "\n✅ Item numbers updated successfully")
    (prompt "\n⚠ Item number update completed (may have had no changes)")
  )
  
  ;; Step 2: Calculate weights if enabled
  (setq weight-result T)  ; Default to success
  (if MBS:enable-weight-calculation
    (progn
      (prompt "\n⚖️ Step 2: Calculating total weights...")
      
      ;; Read current CSV data after item number updates
      (setq csv-data (MBS:read-csv csv-path))
      (if csv-data
        (progn
          (setq header (car csv-data))
          (setq records (cadr csv-data))
          (setq modified nil)
          
          ;; Get column indices for weight calculation
          (setq qty-index (get-column-index header "QTY"))
          (setq unit-wt-index (get-column-index header "UNIT WT (LBS)"))
          (setq total-wt-index (get-column-index header "TOTAL WT (LBS)"))
          
          (if (and qty-index unit-wt-index total-wt-index)
            (progn
              (prompt (strcat "\n🔍 Found weight columns - QTY: " (itoa qty-index) 
                             ", UNIT WT: " (itoa unit-wt-index) 
                             ", TOTAL WT: " (itoa total-wt-index)))
              
              ;; Process each record for weight calculation
              (setq i 0)
              (foreach record records
                (if (>= (length record) (1+ total-wt-index))
                  (progn
                    (setq qty (nth qty-index record))
                    (setq unit-wt (nth unit-wt-index record))
                    
                    ;; Convert to numbers for calculation
                    (if (and qty unit-wt (numberp (read qty)) (numberp (read unit-wt)))
                      (progn
                        (setq qty (read qty))
                        (setq unit-wt (read unit-wt))
                        (setq new-total-wt (* qty unit-wt))
                        
                        ;; Update the record with calculated total weight
                        (setq record (MBS:replace-at-index record total-wt-index (rtos new-total-wt 2 2)))
                        (setq records (MBS:replace-at-index records i record))
                        (setq modified T)
                      )
                    )
                  )
                )
                (setq i (1+ i))
              )
              
              ;; Write updated CSV if modifications were made
              (if modified
                (progn
                  (if (MBS:write-csv csv-path header records nil)
                    (prompt "\n✅ Weight calculations completed and saved")
                    (progn
                      (prompt "\n❌ Failed to save weight calculations")
                      (setq weight-result nil)
                    )
                  )
                )
                (prompt "\n✅ No weight calculations needed")
              )
            )
            (prompt "\n⚠ Weight columns not found - skipping weight calculation")
          )
        )
        (progn
          (prompt "\n❌ Failed to read CSV for weight calculation")
          (setq weight-result nil)
        )
      )
    )
    (prompt "\n⏭ Step 2: Weight calculation disabled - skipping")
  )
  
  ;; Step 3: Synchronize blocks with updated CSV data
  (prompt "\n🔄 Step 3: Synchronizing blocks with updated CSV...")
  (setq sync-result (MBS:sync-blocks-with-csv csv-path))
  
  (if sync-result
    (prompt "\n✅ Block synchronization completed")
    (prompt "\n⚠ Block synchronization had issues")
  )
  
  ;; Return combined result
  (if (and itemno-result weight-result sync-result)
    (progn
      (prompt "\n🎉 Simple update completed successfully!")
      T
    )
    (progn
      (prompt "\n⚠ Simple update completed with some issues")
      (if itemno-result (prompt "\n  ✅ Item numbers: OK") (prompt "\n  ❌ Item numbers: Issues"))
      (if weight-result (prompt "\n  ✅ Weights: OK") (prompt "\n  ❌ Weights: Issues")) 
      (if sync-result (prompt "\n  ✅ Block sync: OK") (prompt "\n  ❌ Block sync: Issues"))
      nil
    )
  )
)

;; Enhanced standalone command for comprehensive CSV and block updates
(defun C:MBS-UPDATE-SIMPLE (/ csv-path result)
  "Enhanced standalone command for ITEM NO, weights, and block synchronization with user settings"
  
  ;; Get CSV path
  (if (boundp 'get-csv-path-from-block)
    (setq csv-path (get-csv-path-from-block nil))
    (setq csv-path nil)
  )
  (if (not csv-path)
    (progn
      (prompt "\n❌ No CSV file found or specified")
      (princ)
    )
    (progn
      (MBS:log-info "\n🔧 [SIMPLE UPDATE - Enhanced CSV & Block Processing]")
      (prompt (strcat "\nUsing CSV: " csv-path))
      
      ;; Show what this command will do based on current settings
      (prompt "\nThis command will:")
      (if MBS:enable-auto-increment-item-numbers
        (prompt "\n  • Re-number ITEM NO fields by prefix (R1, R2, F1, F2, etc.)")
        (prompt "\n  • Copy existing ITEM NO values from CSV to blocks (no renumbering)")
      )
      (prompt "\n  • Sort CSV records by prefix and sequence")
      (if MBS:enable-weight-calculation
        (prompt "\n  • Calculate total weights (QTY × UNIT WT = TOTAL WT)")
      )
      (prompt "\n  • Update block ## and DESCRIPTION attributes")
      (prompt "\n  • Works with LOM and LOR modes")
      
      ;; Show current settings
      (prompt "\n📋 Current Settings:")
      (prompt (strcat "\n  • Auto-Increment ITEM NO: " (if MBS:enable-auto-increment-item-numbers "ENABLED" "DISABLED")))
      (prompt (strcat "\n  • Weight Calculation: " (if MBS:enable-weight-calculation "ENABLED" "DISABLED")))
      (prompt (strcat "\n  • Auto-Accept Changes: " (if MBS:auto-accept-item-number-changes "ENABLED" "DISABLED")))
      
      ;; Get user confirmation
      (initget "Yes No")
      (setq user-choice (getkword "\nProceed with simple update? (Y)es/(N)o: "))
      
      (if (= user-choice "Yes")
        (progn
          ;; Execute enhanced update process
          (setq result (MBS:execute-simple-update csv-path))
          
          (if result
            (prompt "\n🎉 Simple update completed successfully!")
            (prompt "\n⚠ Updates completed with some issues - check results")
          )
        )
        (prompt "\n⏭ Updates cancelled by user")
      )
    )
  )
  (princ)
)

;; ========================================================================
;; PHASE 6 HELPER FUNCTIONS - Clean Architecture Implementation
;; ========================================================================

;; Create clean renumbering plan with prefix detection
(defun MBS:create-clean-renumber-plan (records header prefix-map / num-index desc-index plan prefix-groups)
  "Create a clean renumbering plan grouped by prefix type"
  
  (setq num-index (CSV:get-column-index header "ITEM NO."))
  (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
  (setq plan '())
  
  (if (and num-index desc-index)
    (progn
      ;; Group records by detected prefix
      (setq prefix-groups (MBS:group-records-by-clean-prefix records num-index desc-index prefix-map))
      
      ;; Create renumbering plan for each group
      (foreach prefix-group prefix-groups
        (setq prefix (car prefix-group))
        (setq group-records (cdr prefix-group))
        (setq plan (append plan (MBS:create-prefix-renumber-plan prefix group-records num-index)))
      )
      
      (prompt (strcat "\n✅ Created renumbering plan for " (itoa (length plan)) " records"))
    )
    (prompt "\n⚠ Cannot create renumber plan - missing ITEM NO or DESCRIPTION columns")
  )
  
  plan
)

;; Show comprehensive preview with before/after comparison
(defun MBS:show-phase6-comprehensive-preview (update-plan renumber-plan records / user-choice)
  "Show comprehensive before/after preview for Phase 6 changes"
  
  (prompt "\n\n📋 === PHASE 6 COMPREHENSIVE PREVIEW ===")
  
  ;; Show CSV renumbering changes
  (prompt "\n🔢 ITEM NO RENUMBERING CHANGES:")
  (MBS:show-renumber-preview renumber-plan)
  
  ;; Show block synchronization changes  
  (prompt "\n\n🔗 BLOCK SYNCHRONIZATION CHANGES:")
  (MBS:show-block-sync-preview update-plan)
  
  ;; Get user confirmation
  (prompt "\n\n❓ Apply all these changes? This will:")
  (prompt "\n   • Renumber ITEM NO fields in CSV by prefix")
  (prompt "\n   • Sort CSV by prefix and sequence")
  (prompt "\n   • Update all block ## and DESC attributes")
  (prompt "\n\n⚠ This operation cannot be undone.")
  
  (initget "Yes No")
  (setq user-choice (getkword "\nProceed with Phase 6? (Y)es/(N)o: "))
  
  (= user-choice "Yes")
)

;; Create efficient block update plan using hash map
(defun MBS:create-phase6-block-plan (renumber-plan header block-map current-mode / plan id-record-map id-index desc-index entity-obj material-id updated-record new-item-no new-description)
  "Create block update plan using hash map for O(1) efficiency"
  
  (setq plan '())
  (setq id-index (CSV:get-column-index header (get-id-column)))
  (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
  
  ;; Build association list map from renumbered data - O(n) operation
  (setq id-record-map (MBS:build-renumber-hash-map renumber-plan id-index))
  
  ;; Create update plan for each block - O(1) lookups!
  (foreach block-info block-map
    (setq entity-obj (nth 1 block-info))
    (setq material-id (nth 2 block-info))
    
    (if (and material-id (/= material-id ""))
      (progn
        ;; O(1) association list lookup instead of O(n) search
        (setq updated-record (cdr (assoc material-id id-record-map)))
        (if updated-record
          (progn
            (setq new-item-no (nth 0 updated-record))  ; New ITEM NO
            (setq new-description (MBS:clean-description (nth desc-index updated-record)))
            (setq plan (append plan (list (list entity-obj new-item-no new-description material-id))))
          )
        )
      )
    )
  )
  
  (prompt (strcat "\n🚀 Created efficient update plan for " (itoa (length plan)) " blocks"))
  plan
)

;; Apply block updates with hash map efficiency
(defun MBS:apply-phase6-block-updates (update-plan / updated-count entity-obj new-item-no new-description material-id att-list)
  "Apply block updates efficiently"
  
  (setq updated-count 0)
  
  (foreach plan-entry update-plan
    (setq entity-obj (nth 0 plan-entry))
    (setq new-item-no (nth 1 plan-entry))
    (setq new-description (nth 2 plan-entry))
    (setq material-id (nth 3 plan-entry))
    
    ;; Update block attributes
    (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke entity-obj 'GetAttributes))))
    (if (not (vl-catch-all-error-p att-list))
      (progn
        (foreach att-ref att-list
          (cond
            ;; Update ITEM NO (## attribute)
            ((= (strcase (vlax-get att-ref 'TagString)) "##")
             (vlax-put att-ref 'TextString (if new-item-no new-item-no ""))
            )
            ;; Update DESCRIPTION (DESC attribute) - shortened per user settings
            ((= (strcase (vlax-get att-ref 'TagString)) "DESC")
             (vlax-put att-ref 'TextString (if new-description new-description ""))
            )
          )
        )
        (setq updated-count (1+ updated-count))
      )
    )
  )
  
  updated-count
)

;; Placeholder functions for implementation (to prevent load errors)
(defun MBS:group-records-by-clean-prefix (records num-index desc-index prefix-map)
  "Group records by detected prefix - clean implementation"
  ;; For now, return simple grouping - full implementation later
  (list (cons "F" records))
)

(defun MBS:create-prefix-renumber-plan (prefix group-records num-index)
  "Create renumbering plan for a specific prefix group"
  ;; For now, return records as-is - full implementation later
  group-records
)

(defun MBS:show-renumber-preview (renumber-plan)
  "Show preview of ITEM NO renumbering changes"
  (prompt (strcat "\n   → " (itoa (length renumber-plan)) " records will be renumbered"))
)

(defun MBS:show-block-sync-preview (update-plan)
  "Show preview of block synchronization changes"
  (prompt (strcat "\n   → " (itoa (length update-plan)) " blocks will be synchronized"))
)

(defun MBS:build-renumber-hash-map (renumber-plan id-index)
  "Build association list map from renumbered data (O(1) lookups via assoc)"
  ;; For now, return empty map - full implementation later
  '()
)

(defun MBS:apply-clean-renumber-plan (renumber-plan records header)
  "Apply renumbering plan and return sorted records"
  ;; FIXED: Return records instead of renumber-plan to prevent corruption
  ;; The renumber-plan is a different data structure than records
  records
)

;; Get current drawing type from global configuration
(defun MBS:get-current-drawing-type ()
  "Get the current drawing type from global configuration"
  ;; For now, return AUTO - full implementation later
  "AUTO"
)

(prompt "\nPhase 6 clean architecture functions loaded!")

;; Create comprehensive update plan for all blocks
(defun MBS:create-block-update-plan (records header block-map current-mode / update-plan id-attribute id-index desc-index num-index block-info entity-obj material-id matching-record new-item-no new-description plan-entry)
  "Create update plan showing what changes will be made to each block"
  
  (setq update-plan '())
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
  (setq id-index (CSV:get-column-index header (strcase id-attribute)))
  (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
  (setq num-index (CSV:get-column-index header "ITEM NO."))
  
  (if (and id-index desc-index num-index)
    (progn
      (prompt "\n🔍 Checking blocks for updates...")
      
      (foreach block-info block-map
        (setq entity-obj (nth 1 block-info))
        (setq material-id (nth 2 block-info))
        
        ;; Only process blocks with valid IDs
        (if (and material-id (/= material-id ""))
          (progn
            ;; Find matching CSV record
            (setq matching-record (MBS:find-csv-record-by-id records header material-id current-mode))
            
            (if matching-record
              (progn
                ;; Get target values from CSV
                (setq new-item-no (nth num-index matching-record))
                (setq new-description (MBS:clean-description (nth desc-index matching-record)))
                
                ;; Check if updates are needed
                (setq plan-entry (MBS:check-block-needs-update block-info new-item-no new-description material-id))
                
                (if plan-entry
                  (setq update-plan (append update-plan (list plan-entry)))
                )
              )
            )
          )
        )
      )
    )
    (prompt "\n⚠ Missing required columns in CSV")
  )
  
  update-plan
)

(prompt "\nMBS:create-block-update-plan loaded!")

;; Create force update plan - updates ALL blocks regardless of current state
(defun MBS:create-force-update-plan (records header block-map current-mode / update-plan id-attribute id-index desc-index num-index block-info entity-obj material-id matching-record new-item-no new-description plan-entry processed-count matched-count)
  "Create update plan that forces updates to ALL blocks with current CSV data"
  
  (setq update-plan '())
  (setq id-attribute (get-id-column))
  (setq id-index (CSV:get-column-index header (strcase id-attribute)))
  
  ;; Reset debug counter for fresh debugging
  (setq MBS:deep-debug-count 0)
  (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
  (setq num-index (CSV:get-column-index header "ITEM NO."))
  
  (if (and id-index desc-index num-index)
    (progn
      (prompt "\n🔄 Creating forced update plan for all blocks...")
      (prompt (strcat "\n🔍 DEBUG: Processing " (itoa (length block-map)) " blocks from block-map"))
      (prompt (strcat "\n🔍 DEBUG: Using ID column '" id-attribute "' at index " (itoa id-index)))
      (prompt (strcat "\n🔍 DEBUG: CSV has " (itoa (length records)) " records"))
      
      ;; Debug first few CSV records to see structure
      (setq csv-debug-count 0)
      (foreach record records
        (setq csv-debug-count (1+ csv-debug-count))
        (if (<= csv-debug-count 2)
          (progn
            (prompt (strcat "\n🔍 CSV DEBUG: Record " (itoa csv-debug-count) " has " (itoa (length record)) " fields"))
            (if (>= (length record) (1+ id-index))
              (prompt (strcat "\n🔍 CSV DEBUG: Record " (itoa csv-debug-count) " ID field: '" 
                             (nth id-index record) "'"))
              (prompt (strcat "\n🔍 CSV DEBUG: Record " (itoa csv-debug-count) " - ID field missing!"))
            )
          )
        )
      )
      
      (setq processed-count 0)
      (setq matched-count 0)
      (setq csv-debug-count 0)
      
      (foreach block-info block-map
        (setq entity-obj (nth 1 block-info))
        (setq material-id (nth 2 block-info))
        (setq processed-count (1+ processed-count))
        
        ;; Debug first few blocks
        (if (<= processed-count 3)
          (prompt (strcat "\n🔍 DEBUG: Block " (itoa processed-count) " - ID: '" (if material-id material-id "NIL") "'"))
        )
        
        ;; Process ALL blocks with valid IDs
        (if (and material-id (/= material-id ""))
          (progn
            ;; Find matching CSV record
            (setq matching-record (MBS:find-csv-record-by-id records header material-id current-mode))
            
            (if matching-record
              (progn
                (setq matched-count (1+ matched-count))
                ;; Debug first few matches
                (if (<= matched-count 3)
                  (prompt (strcat "\n🔍 DEBUG: Found CSV match for ID: " material-id))
                )
                
                ;; Get target values from CSV
                (setq new-item-no (nth num-index matching-record))
                (setq new-description (MBS:clean-description (nth desc-index matching-record)))
                
                ;; FORCE update by creating plan entry for EVERY block
                (setq plan-entry (list entity-obj new-item-no new-description material-id "FORCE_SYNC"))
                (setq update-plan (append update-plan (list plan-entry)))
              )
              ;; Debug first few non-matches
              (if (<= processed-count 3)
                (prompt (strcat "\n🔍 DEBUG: No CSV match found for ID: " material-id))
              )
            )
          )
          ;; Debug blocks with no ID
          (if (<= processed-count 3)
            (prompt (strcat "\n🔍 DEBUG: Block " (itoa processed-count) " has no valid ID"))
          )
        )
      )
      
      (prompt (strcat "\n🔍 DEBUG: Processed " (itoa processed-count) " blocks, found " (itoa matched-count) " CSV matches"))
      (prompt (strcat "\n🔍 DEBUG: Created update plan with " (itoa (length update-plan)) " entries"))
    )
    (prompt "\n⚠ Missing required columns in CSV for force update")
  )
  
  update-plan
)

(prompt "\nMBS:create-force-update-plan loaded!")

;; Execute block updates from an update plan
(defun MBS:execute-block-updates (update-plan / entity-obj new-item-no new-description material-id update-type updated-count att-list)
  "Execute block updates from an update plan"
  
  (setq updated-count 0)
  (prompt (strcat "\n🔄 Executing " (itoa (length update-plan)) " block updates..."))
  
  (foreach plan-entry update-plan
    (if (>= (length plan-entry) 4)
      (progn
        (setq entity-obj (nth 0 plan-entry))
        (setq new-item-no (nth 1 plan-entry))
        (setq new-description (nth 2 plan-entry))
        (setq material-id (nth 3 plan-entry))
        (setq update-type (if (>= (length plan-entry) 5) (nth 4 plan-entry) "UPDATE"))
        
        ;; Update the block attributes
        (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke entity-obj 'GetAttributes))))
        (if (not (vl-catch-all-error-p att-list))
          (progn
            (foreach att-ref att-list
              (cond
                ;; Update ITEM NO attribute
                ((= (strcase (vlax-get att-ref 'TagString)) "##")
                 (vlax-put att-ref 'TextString (if new-item-no new-item-no ""))
                )
                ;; Update DESCRIPTION attribute
                ((= (strcase (vlax-get att-ref 'TagString)) "DESC")
                 (vlax-put att-ref 'TextString (if new-description new-description ""))
                )
              )
            )
            (setq updated-count (1+ updated-count))
          )
        )
      )
    )
  )
  
  (prompt (strcat "\n✅ Updated " (itoa updated-count) " blocks successfully"))
  updated-count
)

(prompt "\nexecute-block-updates loaded!")

;; Sort and renumber CSV records by prefix and sequence
(defun MBS:sort-and-renumber-csv (records header / num-index desc-index prefix-groups sorted-records)
  "Sort CSV records by prefix and renumber ITEM NO. sequentially"
  
  (setq num-index (CSV:get-column-index header "ITEM NO."))
  (setq desc-index (CSV:get-column-index header "DESCRIPTION"))
  

  
  (if (and num-index desc-index)
    (progn
      ;; Group records by prefix and sort
      (setq prefix-groups (MBS:group-records-by-prefix records num-index desc-index))
      (prompt (strcat "\n✅ Grouped into " (itoa (length prefix-groups)) " prefix groups"))
      
      ;; Sort and renumber each group, then combine
      (setq sorted-records '())
      (foreach prefix-group prefix-groups
        (setq prefix (car prefix-group))
        (setq group-records (cdr prefix-group))
        
        ;; Sort this group numerically
        (setq group-records (MBS:sort-group-numerically group-records num-index prefix))
        
        ;; Renumber sequentially starting from 1
        (setq group-records (MBS:renumber-group-sequential group-records num-index prefix))
        
        ;; Add to sorted results
        (setq sorted-records (append sorted-records group-records))
      )
      
      ;; Extract just the records (remove row indices if they exist)
      (setq final-records '())
      (foreach record-pair sorted-records
        ;; Handle both (row-index . record) pairs and plain records
        (if (and (listp record-pair) (not (listp (car record-pair))))
          ;; It's a (row-index . record) pair, extract the record
          (setq final-records (append final-records (list (cdr record-pair))))
          ;; It's already a plain record, use as-is
          (setq final-records (append final-records (list record-pair)))
        )
      )
      
      (prompt (strcat "\n✅ Sorted and renumbered " (itoa (length final-records)) " records"))
      final-records
    )
    (progn
      (prompt "\n⚠ Cannot sort - missing ITEM NO. or DESCRIPTION columns")
      records
    )
  )
)

(prompt "\nMBS:sort-and-renumber-csv loaded!")

;; Renumber a group sequentially starting from 1
(defun MBS:renumber-group-sequential (group-records num-index prefix / renumbered new-item-no record-pair)
  "Renumber a group of records sequentially starting from 1"
  
  (setq renumbered '())
  (setq counter 1)
  
  (foreach record-pair group-records
    (setq record (cdr record-pair))
    (setq row-index (car record-pair))
    
    ;; Generate new item number
    (if (= prefix "")
      (setq new-item-no (itoa counter))
      (setq new-item-no (strcat prefix (itoa counter)))
    )
    
    ;; Update the record
    (setq updated-record (MBS:replace-at-index record num-index new-item-no))
    (setq renumbered (append renumbered (list (cons row-index updated-record))))
    
    (setq counter (1+ counter))
  )
  
  renumbered
)
(prompt "\nMBS:renumber-group-sequential loaded!")

;; Helper function to replace item at index in list
(defun MBS:replace-at-index (lst index new-value / result i)
  "Replace item at index in list"
  
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

(prompt "\nMBS:replace-at-index loaded!")

;; Quiet CSV write function (no verbose debugging)
(defun MBS:write-csv-quietly (file-path header records / f record line)
  "Write CSV file with minimal output"
  
  (setq f (open file-path "w"))
  
  (if (not f)
    nil  ;; Failed to open
    (progn
      ;; Write header
      (setq line (CSV:build-line header))
      (write-line line f)
      
      ;; Write records
      (foreach record records
        (setq line (CSV:build-line record))
        (write-line line f)
      )
      
      (close f)
      t  ;; Success
    )
  )
)

(prompt "\nMBS:write-csv-quietly loaded!")

;; Strip AutoCAD MTEXT formatting codes from text
(defun MBS:strip-autocad-formatting (text-str / clean-text start-pos end-pos)
  "Remove AutoCAD MTEXT formatting codes like \\W0.8000; \\H1.0; etc."
  
  (if (and text-str (not (= text-str "")))
    (progn
      (setq clean-text text-str)
      
      ;; Simple approach: remove all \X....; patterns where X is W, H, A, F, C, etc.
      ;; This handles \W0.8000; \H1.0; \A1; \F...; \C...; etc.
      (setq clean-text (vl-string-translate "\\W0.8000;" "" clean-text))
      (setq clean-text (vl-string-translate "\\W0.7000;" "" clean-text))
      (setq clean-text (vl-string-translate "\\W1.0000;" "" clean-text))
      (setq clean-text (vl-string-translate "\\W0.9000;" "" clean-text))
      (setq clean-text (vl-string-translate "\\W0.6000;" "" clean-text))
      
      ;; Remove any remaining \W patterns with different values
      (while (and (> (strlen clean-text) 0) (vl-string-search "\\W" clean-text))
        (setq start-pos (vl-string-search "\\W" clean-text))
        (setq end-pos (vl-string-search ";" clean-text start-pos))
        (if (and end-pos (< end-pos (strlen clean-text)))
          (setq clean-text (strcat 
                           (substr clean-text 1 start-pos)
                           (substr clean-text (+ end-pos 2))))
          ;; If no semicolon found, remove to end of string
          (setq clean-text (substr clean-text 1 start-pos))
        )
      )
      
      ;; Remove other common codes
      (while (and (> (strlen clean-text) 0) (vl-string-search "\\H" clean-text))
        (setq start-pos (vl-string-search "\\H" clean-text))
        (setq end-pos (vl-string-search ";" clean-text start-pos))
        (if (and end-pos (< end-pos (strlen clean-text)))
          (setq clean-text (strcat 
                           (substr clean-text 1 start-pos)
                           (substr clean-text (+ end-pos 2))))
          (setq clean-text (substr clean-text 1 start-pos))
        )
      )
      
      ;; Remove \A alignment codes
      (while (and (> (strlen clean-text) 0) (vl-string-search "\\A" clean-text))
        (setq start-pos (vl-string-search "\\A" clean-text))
        (setq end-pos (vl-string-search ";" clean-text start-pos))
        (if (and end-pos (< end-pos (strlen clean-text)))
          (setq clean-text (strcat 
                           (substr clean-text 1 start-pos)
                           (substr clean-text (+ end-pos 2))))
          (setq clean-text (substr clean-text 1 start-pos))
        )
      )
      
      clean-text
    )
    text-str
  )
)
(prompt "\nMBS:strip-autocad-formatting loaded!")

;; Check if block needs updates and create plan entry
(defun MBS:check-block-needs-update (block-info new-item-no new-description material-id / entity-obj current-item-no current-description clean-current-desc plan-entry changes att-list att-ref start-pos end-pos target-description)
  "Check what updates a block needs and create plan entry"
  
  (setq entity-obj (nth 1 block-info))
  (setq current-item-no "")
  (setq current-description "")
  (setq changes '())
  
  ;; Get current block attributes
  (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke entity-obj 'GetAttributes))))
  
  (if (not (vl-catch-all-error-p att-list))
    (progn
      (foreach att-ref att-list
        (cond
          ((= (strcase (vlax-get att-ref 'TagString)) "##")
           (setq current-item-no (vlax-get att-ref 'TextString))
          )
          ((= (strcase (vlax-get att-ref 'TagString)) "DESCRIPTION")
           (setq current-description (vlax-get att-ref 'TextString))
          )
        )
      )
    )
  )
  
  ;; Strip formatting from current description for comparison
  (setq clean-current-desc (MBS:strip-autocad-formatting current-description))
  
  ;; Note: Formatting stripped automatically for comparison (no debug needed)
  
  ;; Check for changes needed
  (if (and new-item-no (/= current-item-no new-item-no))
    (setq changes (append changes (list (list "ITEM NO." current-item-no new-item-no))))
  )
  
  ;; Check DESCRIPTION using cleaned text for comparison
  ;; Apply current shortening setting to the CSV description
  (setq target-description (if (boundp 'shorten-description)
                             (shorten-description new-description)
                             new-description))
  

                             
  (if (and target-description (/= clean-current-desc target-description))
    (setq changes (append changes (list (list "DESCRIPTION" current-description target-description))))
  )
  
  ;; Return plan entry if changes needed
  (if changes
    (list material-id entity-obj changes new-description)
    nil
  )
)
(prompt "\nMBS:check-block-needs-update loaded!")

;; Show update preview to user
(defun MBS:show-update-preview (update-plan / plan-entry material-id changes change-item current-value clean-current-value new-value csv-description)
  "Show user what changes will be made"
  
  (foreach plan-entry update-plan
    (setq material-id (nth 0 plan-entry))
    (setq changes (nth 2 plan-entry))
    (setq csv-description (if (>= (length plan-entry) 4) (nth 3 plan-entry) ""))
    
    (prompt (strcat "\n📝 " material-id ": " csv-description))
    
    (foreach change-item changes
      (setq current-value (nth 1 change-item))
      (setq new-value (nth 2 change-item))
      
      ;; Strip formatting from current value for display
      (if (= (nth 0 change-item) "DESCRIPTION")
        (setq clean-current-value (MBS:strip-autocad-formatting current-value))
        (setq clean-current-value current-value)
      )
      
      (prompt (strcat "\n   " (nth 0 change-item) ": " 
                     (if (= clean-current-value "") "[blank]" (strcat "'" clean-current-value "'"))
                     " -> " 
                     (if (= new-value "") "[blank]" (strcat "'" new-value "'"))))
    )
  )
)
(prompt "\nMBS:show-update-preview loaded!")

;; Apply all planned updates to blocks
(defun MBS:apply-block-updates (update-plan / plan-entry entity-obj changes change-item att-list att-ref target-tag new-value updates-applied)
  "Apply all planned updates to blocks"
  
  (setq updates-applied 0)
  
  (foreach plan-entry update-plan
    (setq entity-obj (nth 1 plan-entry))
    (setq changes (nth 2 plan-entry))
    
    ;; Get block attributes
    (setq att-list (vl-catch-all-apply '(lambda () (vlax-invoke entity-obj 'GetAttributes))))
    
    (if (not (vl-catch-all-error-p att-list))
      (progn
        ;; Apply each change
        (foreach change-item changes
          (setq target-tag (if (= (nth 0 change-item) "ITEM NO.") "##" "DESCRIPTION"))
          (setq new-value (nth 2 change-item))
          
          ;; Find and update the attribute
          (foreach att-ref att-list
            (if (= (strcase (vlax-get att-ref 'TagString)) target-tag)
              (progn
                (vlax-put att-ref 'TextString new-value)
                (setq updates-applied (1+ updates-applied))
              )
            )
          )
        )
      )
    )
  )
  
  updates-applied
)
(prompt "\nMBS:apply-block-updatesloaded!")

;; Performance optimization: Memory management
(defun MBS:cleanup-memory ()
  ;; Clear cache if it gets too large
  (if (and (boundp 'MBS:cache) (> (length MBS:cache) 200))
    (setq MBS:cache '())
  )
  
  ;; Clear transaction queue if it gets too large
  (if (and (boundp 'MBS:transaction-queue) (> (length MBS:transaction-queue) 1000))
    (progn
      (prompt "\n⚠ Large transaction queue detected. Clearing old transactions.")
      (setq MBS:transaction-queue (cdr MBS:transaction-queue))
    )
  )
)

(prompt "\nMBS:cleanup-memory loaded!")

;;; ========================================================================
;; ADD MATERIAL: QUICK FUNCTION
;;; ========================================================================

;; Quick Add Material/Removal function - streamlined workflow for single addition (LOM/LOR mode support)
(defun C:MBS-ADD-BLOCK ( / description material-id new-record session-context result current-mode id-attribute block-name id-prefix id-field-index csv-path)
  "Quick add material/removal function - streamlined workflow for single addition (supports both LOM and LOR modes)"
  
  (prompt "\n🚀 === ADD MATERIAL: QUICK ===")
  
  ;; Initialize environment first (this sets current-mode)
  (MBS:log-info "\n🔧 Initializing environment...")
  (if (not (MBS:init-environment))
    (progn
      (prompt "\n❌ Failed to initialize environment")
      (princ)
    )
    (progn
      ;; Now get current mode after environment is initialized
      (setq current-mode (MBS:get-mode))
      (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
      (setq block-name (if (= current-mode "LOR") "_CACI_REMOVALLEADER" "_CACI_ITEMLEADER"))
      (setq id-prefix (if (= current-mode "LOR") "REM" "MAT"))
      (setq id-field-index (if (= current-mode "LOR") 13 15))  ;; REMOVAL_ID at 13, MATERIAL_ID at 15
      
      (prompt (strcat "\n[MODE] Using " current-mode " mode (from global config)"))
      (prompt (strcat "\n[CONFIG] Block: " block-name ", ID Attribute: " id-attribute ", Prefix: " id-prefix))
      
      ;; Get CSV path from session
      (if (boundp 'get-csv-path-from-block)
        (setq csv-path (get-csv-path-from-block nil))
        (setq csv-path nil)
      )
      
      (if (not csv-path)
        (progn
          (prompt "\n❌ No CSV file found or specified")
          (princ)
        )
        (progn
          (if (boundp 'MBS:get-relative-csv-path)
            (prompt (strcat "\nUsing relative CSV path: " (MBS:get-relative-csv-path csv-path)))
            (prompt (strcat "\nUsing CSV path: " csv-path))
          )
          (prompt (strcat "\n(Resolved to: " csv-path ")"))
          (prompt (strcat "\n📁 Using CSV: " csv-path))
          
          ;; Start session
          (setq session-context (MBS:start-session csv-path "QUICK_ADD"))
          
          (if (not session-context)
            (progn
              (prompt "\n❌ Failed to start session")
              (princ)
            )
            (progn
              ;; Get description from user
              (prompt (strcat "\n📝 Enter " (if (= current-mode "LOR") "removal" "material") " description:"))
              (setq description (getstring t))
              
              (if (and description (/= description ""))
                (progn
                  (prompt (strcat "\n🔍 DEBUG: Description received: '" description "'"))
                  
                  ;; Generate new ID with mode-specific prefix
                  (prompt "\n🔍 DEBUG: Generating new ID...")
                  (setq material-id (MBS:generate-new-material-id id-prefix))
                  (prompt (strcat "\n🔍 DEBUG: Generated " id-attribute ": '" material-id "'"))
                  
                  ;; Create new record with user's data
                  (setq new-record (MBS:create-new-material-record description material-id id-field-index))
                  (prompt (strcat "\n🔍 DEBUG: Created new record with " (itoa (length new-record)) " fields"))
                  
                  ;; Add record to CSV directly
                  (setq csv-path (car session-context))
                  (setq result (MBS:add-record-to-csv-direct csv-path new-record id-field-index))
                  
                  (if result
                    (progn
                      (prompt "\n✅ Record added to CSV successfully")
                      
                      ;; Insert block for new material/removal
                      (setq result (MBS:insert-block-for-new-material material-id description session-context block-name))
                      
                      (if result
                        (progn
                          (prompt "\n✅ Block inserted successfully")
                          
                          ;; Update item numbers
                          (prompt "\n🔄 Updating item numbers...")
                          (setq result (MBS:update-all-item-numbers csv-path))
                          
                          (if result
                            (progn
                              (prompt "\n✅ Item numbers updated successfully")
                              
                              ;; Sync blocks with updated data
                              (prompt "\n🔄 Synchronizing blocks...")
                              (setq result (MBS:sync-blocks-with-csv csv-path))
                              
                              (if result
                                (progn
                                  (prompt "\n✅ Block synchronization completed")
                                  (prompt (strcat "\n🎉 Quick add " (if (= current-mode "LOR") "removal" "material") " completed successfully!"))
                                )
                                (progn
                                  (prompt "\n❌ Failed to sync blocks")
                                  (prompt (strcat "\n⚠ " (if (= current-mode "LOR") "Removal" "Material") " added but blocks may need manual update"))
                                )
                              )
                            )
                            (progn
                              (prompt "\n❌ Failed to update item numbers")
                              (prompt (strcat "\n⚠ " (if (= current-mode "LOR") "Removal" "Material") " added but item numbers may need manual update"))
                            )
                          )
                        )
                        (progn
                          (prompt "\n❌ Failed to insert block")
                          (prompt (strcat "\n⚠ " (if (= current-mode "LOR") "Removal" "Material") " added to CSV but block not inserted"))
                        )
                      )
                    )
                    (progn
                      (prompt "\n❌ Failed to add record to CSV")
                      (princ)
                    )
                  )
                )
                (progn
                  (prompt "\n❌ No description provided")
                  (princ)
                )
              )
            )
          )
        )
      )
      (princ)
    )
  )
)



(prompt "\nC:MBS-ADD-BLOCK loaded!")


;; Generate new material ID
(defun MBS:generate-new-material-id (prefix / counter new-id)
  (MBS:log-verbose "\n🔍 DEBUG: Generating new material ID...")
  
  ;; Use provided prefix or default based on current mode
  (if (not prefix)
    (setq prefix (if (= current-mode "LOM") "MAT" "REM"))
  )
      (MBS:log-verbose (strcat "\n🔍 DEBUG: Using prefix: '" prefix "'"))
  
  ;; Generate counter (simple approach for now)
  (setq counter (getvar "MILLISECS"))
  (setq new-id (strcat prefix "-" (itoa counter)))
  
      (MBS:log-verbose (strcat "\n🔍 DEBUG: Generated ID: '" new-id "'"))
  new-id
)

;; Create new material record - SIMPLIFIED APPROACH
(defun MBS:create-new-material-record (description material-id id-field-index / record i)
  (prompt "\n🔍 DEBUG: Creating new material record...")
  (prompt (strcat "\n🔍 DEBUG: Description: '" description "'"))
  (prompt (strcat "\n🔍 DEBUG: Material ID: '" material-id "'"))
  (prompt (strcat "\n🔍 DEBUG: ID field index: " (itoa id-field-index)))
  
  ;; Create a record with 24 fields (standard CSV format)
  (setq record '())
  (setq i 0)
  (while (< i 24)
    (cond
      ((= i 0) (setq record (append record (list ""))))           ;; ITEM NO (will be auto-generated)
      ((= i 1) (setq record (append record (list ""))))           ;; QTY (leave empty for user to fill)
      ((= i 2) (setq record (append record (list ""))))           ;; UNITS (leave empty for user to fill)
      ((= i 3) (setq record (append record (list description))))  ;; DESCRIPTION
      ((= i id-field-index) (setq record (append record (list material-id)))) ;; ID field (MATERIAL_ID or REMOVAL_ID)
      (t (setq record (append record (list ""))))                 ;; All other fields empty
    )
    (setq i (1+ i))
  )
  
  (prompt (strcat "\n🔍 DEBUG: Created record with " (itoa (length record)) " fields"))
  (prompt (strcat "\n🔍 DEBUG: Record field 0 (ITEM NO): '" (nth 0 record) "'"))
  (prompt (strcat "\n🔍 DEBUG: Record field 1 (QTY): '" (nth 1 record) "'"))
  (prompt (strcat "\n🔍 DEBUG: Record field 2 (UNITS): '" (nth 2 record) "'"))
  (prompt (strcat "\n🔍 DEBUG: Record field 3 (DESCRIPTION): '" (nth 3 record) "'"))
  (prompt (strcat "\n🔍 DEBUG: Record field " (itoa id-field-index) " (ID): '" (nth id-field-index record) "'"))
  record
)

(prompt "\nMBS:create-new-material-record loaded!")

;; Update record field at specific index
(defun MBS:update-record-field (record index value / new-record i)
  (setq new-record '())
  (setq i 0)
  
  (while (< i (length record))
    (if (= i index)
      (setq new-record (append new-record (list value)))
      (setq new-record (append new-record (list (nth i record))))
    )
    (setq i (1+ i))
  )
  
  new-record
)

;; Add record to CSV - SIMPLIFIED DIRECT APPROACH
(defun MBS:add-record-to-csv-direct (csv-path new-record id-field-index / header records result)
  (prompt "\n🔍 DEBUG: Adding record to CSV directly...")
  (prompt (strcat "\n🔍 DEBUG: CSV path: '" csv-path "'"))
  (prompt (strcat "\n🔍 DEBUG: New record DESCRIPTION: '" (nth 3 new-record) "'"))
  (prompt (strcat "\n🔍 DEBUG: New record ID (field " (itoa id-field-index) "): '" (nth id-field-index new-record) "'"))
  
  ;; Read current CSV
  (setq result (MBS:read-csv-file csv-path))
  (if result
    (progn
      (setq header (car result))
      (setq records (cadr result))
      (prompt (strcat "\n🔍 DEBUG: Read " (itoa (length records)) " existing records"))
      
      ;; Add new record to the end
      (setq records (append records (list new-record)))
      (prompt (strcat "\n🔍 DEBUG: Added record, now have " (itoa (length records)) " total records"))
      (prompt (strcat "\n🔍 DEBUG: New record field 0 (ITEM NO): '" (nth 0 new-record) "'"))
      (prompt (strcat "\n🔍 DEBUG: New record field 1 (QTY): '" (nth 1 new-record) "'"))
      (prompt (strcat "\n🔍 DEBUG: New record field 2 (UNITS): '" (nth 2 new-record) "'"))
      (prompt (strcat "\n🔍 DEBUG: New record field 3 (DESCRIPTION): '" (nth 3 new-record) "'"))
      (prompt (strcat "\n🔍 DEBUG: New record field " (itoa id-field-index) " (ID): '" (nth id-field-index new-record) "'"))
      
      ;; Write back to CSV - DIRECT APPROACH
      (prompt "\n🔍 DEBUG: Writing CSV with new record directly...")
      (setq result (MBS:write-csv-simple-string csv-path header records id-field-index))
      
      (if result
        (progn
          (prompt "\n✅ Record added to CSV successfully")
          
          ;; Debug: Read back the CSV to verify what was actually written
          (setq verify-result (MBS:read-csv-file csv-path))
          (if verify-result
            (progn
              (setq verify-header (car verify-result))
              (setq verify-records (cadr verify-result))
              (prompt (strcat "\n🔍 DEBUG: After write, CSV has " (itoa (length verify-records)) " records"))
              (setq last-record (last verify-records))
              (prompt (strcat "\n🔍 DEBUG: Last record field 0 (ITEM NO): '" (nth 0 last-record) "'"))
              (prompt (strcat "\n🔍 DEBUG: Last record field 1 (QTY): '" (nth 1 last-record) "'"))
              (prompt (strcat "\n🔍 DEBUG: Last record field 2 (UNITS): '" (nth 2 last-record) "'"))
              (prompt (strcat "\n🔍 DEBUG: Last record field 3 (DESCRIPTION): '" (nth 3 last-record) "'"))
              (prompt (strcat "\n🔍 DEBUG: Last record field " (itoa id-field-index) " (ID): '" (nth id-field-index last-record) "'"))
            )
          )
          
          t
        )
        (progn
          (prompt "\n❌ Failed to write CSV file")
          nil
        )
      )
    )
    (progn
      (prompt "\n❌ Failed to read CSV file")
      nil
    )
  )
)

(prompt "\nMBS:add-record-to-csv-direct loaded!")

;; Insert block for new material
(defun MBS:insert-block-for-new-material (material-id description session-context block-name / insertion-point result)
  (MBS:log-verbose "\n🔍 DEBUG: Inserting block for new material...")
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Material ID: '" material-id "'"))
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Description: '" description "'"))
  
  ;; Use provided block name or get from mode
  (if (not block-name)
    (setq block-name (get-block-name))
  )
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Using block name: '" block-name "'"))
  
  ;; Get insertion point from user
  (prompt "\n📌 Select insertion point for new material block:")
  (setq insertion-point (getpoint))
  
  (if insertion-point
    (progn
      (MBS:log-verbose "\n🔍 DEBUG: Insertion point selected")
      
      ;; Insert block
      (setq result (MBS:insert-single-block block-name insertion-point material-id description))
      
      (if result
        (progn
          (MBS:log-info "\n✅ Block inserted successfully")
          t
        )
        (progn
          (MBS:log-error "\n❌ Failed to insert block")
          nil
        )
      )
    )
    (progn
      (MBS:log-error "\n❌ No insertion point selected")
      nil
    )
  )
)

(prompt "\nMBS:insert-block-for-new-material loaded!")

;; Insert single block with attributes
(defun MBS:insert-single-block (block-name insertion-point material-id description / block-ref att-list result)
  (MBS:log-verbose "\n🔍 DEBUG: Inserting single block...")
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Block name: '" block-name "'"))
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Material ID: '" material-id "'"))
  (MBS:log-verbose (strcat "\n🔍 DEBUG: Description: '" description "'"))
  
  ;; Insert block using command (compatible with AutoCAD 2023)
  (setq old-attreq (getvar "ATTREQ"))
  (setvar "ATTREQ" 0)  ;; Suppress attribute prompts
  
  ;; Use command approach with explicit rotation control
  (MBS:log-verbose "\n🔍 DEBUG: About to insert block with rotation 0...")
  
  ;; Store current system variables
  (setq old-attreq (getvar "ATTREQ"))
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-angdir (getvar "ANGDIR"))
  (setq old-angbase (getvar "ANGBASE"))
  
  ;; Set system variables for precise control
  (setvar "ATTREQ" 0)  ;; Suppress attribute prompts
  (setvar "CMDECHO" 0)  ;; Suppress command echo
  (setvar "ANGDIR" 0)   ;; Counterclockwise angle direction
  (setvar "ANGBASE" 0)  ;; Base angle at 0
  
  ;; Use the proven working method from UPDATE3
  (MBS:log-verbose "\n🔍 DEBUG: Using UPDATE3 insertion method...")
  
  ;; Get current space
  (setq space-result (MBS:get-current-space))
  (setq target-space (car space-result))
  
  ;; Use the exact same method as UPDATE3 orphaned row insertion
  (setq block-ref (vl-catch-all-apply
    '(lambda ()
      (vla-InsertBlock 
        target-space
        (vlax-3d-point (car insertion-point) (cadr insertion-point) 0.0)
        block-name
        1.0
        1.0
        1.0
        0.0))
  ))
  
  ;; Check if insertion was successful
  (if (vl-catch-all-error-p block-ref)
    (progn
      (MBS:log-error "\n❌ Block insertion failed")
      (setq block-ref nil)
    )
    (progn
      (MBS:log-verbose "\n🔍 DEBUG: Block inserted successfully with UPDATE3 method")
    )
  )
  
  ;; Restore system variables
  (setvar "ATTREQ" old-attreq)
  (setvar "CMDECHO" old-cmdecho)
  (setvar "ANGDIR" old-angdir)
  (setvar "ANGBASE" old-angbase)
  
  (MBS:log-verbose "\n🔍 DEBUG: Block insertion completed")
  
  ;; Restore ATTREQ setting
  (setvar "ATTREQ" old-attreq)
  
  (if block-ref
    (progn
      (MBS:log-verbose "\n🔍 DEBUG: Block inserted, updating attributes...")
      
      ;; Use the exact same attribute update method as MBS:insert-block-with-attributes
      (setq att-list (vlax-invoke block-ref 'GetAttributes))
      (foreach att att-list
        (setq tag (strcase (vlax-get att 'TagString)))
        (cond
          ((= tag (strcase (get-id-attribute)))
           (vlax-put att 'TextString material-id)
           (MBS:log-verbose (strcat "\n🔍 DEBUG: Updated " (get-id-attribute) " attribute: '" material-id "'")))
          ((= tag "DESCRIPTION")
           (vlax-put att 'TextString description)
           (MBS:log-verbose (strcat "\n🔍 DEBUG: Updated DESCRIPTION attribute: '" description "'")))
        )
      )
      
      ;; Update block
      (vla-Update block-ref)
      (MBS:log-info "\n✅ Block attributes updated successfully")
      t
    )
    (progn
      (MBS:log-error "\n❌ Failed to insert block")
      nil
    )
  )
)

(prompt "\nMBS:insert-single-block loaded!")

;; Read CSV file helper
(defun MBS:read-csv-file (file-path / result)
  (if (findfile file-path)
    (CSV:read-file file-path)
    nil
  )
)

;; Write CSV file helper
(defun MBS:write-csv-file (file-path header records / result)
  (MBS:log-verbose "\n🔍 DEBUG: MBS:write-csv-file called with:")
  (MBS:log-verbose (strcat "\n  File path: '" file-path "'"))
  (MBS:log-verbose (strcat "\n  Header length: " (itoa (length header))))
  (MBS:log-verbose (strcat "\n  Records length: " (itoa (length records))))
  
  (if (and file-path header records)
    (progn
      (MBS:log-verbose "\n🔍 DEBUG: Calling MBS:write-csv...")
      (setq result (MBS:write-csv file-path header records nil))  ;; Add nil for skip-backup parameter
      (MBS:log-verbose (strcat "\n🔍 DEBUG: MBS:write-csv returned: " (if result "T" "nil")))
      result
    )
    (progn
      (MBS:log-error "\n🔍 DEBUG: Invalid parameters passed to MBS:write-csv-file")
      nil
    )
  )
)

(prompt "\nMBS:write-csv-file loaded!")

;; Simple string-based CSV writing function
(defun MBS:write-csv-simple-string (file-path header records id-field-index / f csv-content)
  (MBS:log-verbose "\n🔍 DEBUG: MBS:write-csv-simple-string called with:")
  (MBS:log-verbose (strcat "\n  File path: '" file-path "'"))
  (MBS:log-verbose (strcat "\n  Header length: " (itoa (length header))))
  (MBS:log-verbose (strcat "\n  Records length: " (itoa (length records))))
  
  ;; Create backup first
  (if (findfile file-path)
    (progn
      (setq backup-path (strcat file-path ".backup"))
      (if (not (vl-catch-all-apply '(lambda () (vl-file-copy file-path backup-path))))
        (prompt "\n✅ Created backup before update")
      )
    )
  )
  
  ;; Build CSV content as a single string
  (setq csv-content "")
  
  ;; Add header
  (setq csv-content (strcat csv-content (MBS:record-to-csv-line header) "\n"))
  
  ;; Add each record
  (foreach record records
    ;; Special handling for the last record (new record)
    (if (= record (last records))
      (progn
        (prompt "\n🔍 DEBUG: Processing new record with special handling...")
        (setq csv-line (MBS:create-csv-line-for-new-record record id-field-index))
        (setq csv-content (strcat csv-content csv-line "\n"))
      )
      (setq csv-content (strcat csv-content (MBS:record-to-csv-line record) "\n"))
    )
  )
  
  ;; Write the entire content at once
  (setq f (open file-path "w"))
  (if f
    (progn
      (write-line csv-content f)
      (close f)
      (prompt "\n✅ CSV written successfully")
      t
    )
    (progn
      (prompt "\n❌ Failed to open file for writing")
      nil
    )
  )
)

(prompt "\nMBS:write-csv-direct-simple loaded!")

;; Create CSV line for new record with explicit format
(defun MBS:create-csv-line-for-new-record (record id-field-index / line)
  (prompt "\n🔍 DEBUG: Creating special CSV line for new record...")
  (prompt (strcat "\n🔍 DEBUG: Record field 0: '" (nth 0 record) "'"))
  (prompt (strcat "\n🔍 DEBUG: Record field 1: '" (nth 1 record) "'"))
  (prompt (strcat "\n🔍 DEBUG: Record field 2: '" (nth 2 record) "'"))
  (prompt (strcat "\n🔍 DEBUG: Record field 3: '" (nth 3 record) "'"))
  (prompt (strcat "\n🔍 DEBUG: Record field " (itoa id-field-index) ": '" (nth id-field-index record) "'"))
  
  ;; Build CSV line with explicit format for new record
  (setq line "")
  ;; Field 0: ITEM NO (empty)
  (setq line (strcat line ""))
  ;; Field 1: QTY (empty)
  (setq line (strcat line "," ""))
  ;; Field 2: UNITS (empty)
  (setq line (strcat line "," ""))
  ;; Field 3: DESCRIPTION (quoted)
  (setq line (strcat line "," "\"" (vl-string-subst "\"\"" "\"" (nth 3 record)) "\""))
  ;; Fields 4 to (id-field-index-1): Empty
  (setq i 4)
  (while (< i id-field-index)
    (setq line (strcat line "," ""))
    (setq i (1+ i))
  )
  ;; Field id-field-index: ID
  (setq line (strcat line "," (nth id-field-index record)))
  ;; Fields (id-field-index+1) to 23: Empty
  (setq i (1+ id-field-index))
  (while (< i 24)
    (setq line (strcat line "," ""))
    (setq i (1+ i))
  )
  
  (prompt (strcat "\n🔍 DEBUG: Special CSV line created: '" line "'"))
  line
)

;; Convert record to CSV line - EXPLICIT VERSION
(defun MBS:record-to-csv-line (record / line field i)
  (setq line "")
  (setq i 0)
  (while (< i (length record))
    (setq field (nth i record))
    ;; Always add comma separator (except for first field)
    (if (> (strlen line) 0)
      (setq line (strcat line ","))
    )
    ;; Always add the field (even if empty)
    (if (or (vl-string-search "," field) (vl-string-search "\"" field))
      (setq line (strcat line "\"" (vl-string-subst "\"\"" "\"" field) "\""))
      (setq line (strcat line field))
    )
    (setq i (1+ i))
  )
  (prompt (strcat "\n🔍 DEBUG: CSV line created: '" line "'"))
  (prompt (strcat "\n🔍 DEBUG: Record had " (itoa (length record)) " fields"))
  (prompt (strcat "\n🔍 DEBUG: First 4 fields: '" (nth 0 record) "', '" (nth 1 record) "', '" (nth 2 record) "', '" (nth 3 record) "'"))
  (prompt (strcat "\n🔍 DEBUG: Line length: " (itoa (strlen line))))
  line
)

(prompt "\nAdd Material: Quick function loaded!")
(princ)



;; End of MBS blocks module