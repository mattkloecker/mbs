;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - Transaction System Module
;; VERSION 5.5
;;; ========================================================================
;; This module implements a two-stage transaction system for Material Block Sync
;; to ensure reliability when updating blocks and CSV files.
;; Dependencies: mbs_core.lsp
;;; ========================================================================

;; Set transaction system flag if not already defined
(if (not (boundp 'use-two-stage-transactions))
  (setq use-two-stage-transactions T)
)

;; Global transaction status variables
(setq MBS:transaction-active nil)
(setq MBS:transaction-changes nil)
(setq MBS:transaction-rollbacks nil)
(setq MBS:transaction-data nil)

;;; ========================================================================
;; TRANSACTION MANAGEMENT FUNCTIONS
;;; ========================================================================

;; Begin a new transaction
(defun MBS:begin-transaction (name / transaction-id)
  (if (not use-two-stage-transactions)
    ;; If transactions are disabled, return a dummy ID
    (progn
      (MBS:log-info "Transactions disabled, using simple mode")
      "SIMPLE"
    )
    ;; Normal transaction processing
    (progn
      (if MBS:transaction-active
        (progn
          (MBS:log-warning "Transaction already in progress, committing first")
          (MBS:commit-transaction MBS:transaction-active)
        )
      )
      
      ;; Create a unique transaction ID
      (setq transaction-id (strcat name "-" (itoa (getvar "MILLISECS"))))
      
      ;; Initialize transaction data
      (setq MBS:transaction-active transaction-id)
      (setq MBS:transaction-changes (list))
      (setq MBS:transaction-rollbacks (list))
      (setq MBS:transaction-data (list))
      
      (MBS:log-info (strcat "Transaction started: " transaction-id))
      
      ;; Return the transaction ID
      transaction-id
    )
  )
)

;; Record a change in the current transaction
(defun MBS:record-change (transaction-id type entity-id data rollback-fn)
  (if (and use-two-stage-transactions
           (= transaction-id MBS:transaction-active))
    (progn
      (MBS:log-verbose (strcat "Recording " type " change for " (vl-princ-to-string entity-id)))
      
      ;; Add change to the transaction log
      (setq MBS:transaction-changes 
            (cons (list transaction-id type entity-id data) 
                  MBS:transaction-changes))
      
      ;; Add rollback function to the rollback list
      (if rollback-fn
        (setq MBS:transaction-rollbacks 
              (cons (list transaction-id entity-id rollback-fn) 
                    MBS:transaction-rollbacks))
      )
      
      T
    )
    ;; If simple mode or invalid transaction, just return T
    T
  )
)

;; Store transaction data that might be needed for rollback
(defun MBS:store-transaction-data (transaction-id key value)
  (if (and use-two-stage-transactions
           (= transaction-id MBS:transaction-active))
    (progn
      (MBS:log-verbose (strcat "Storing data for key: " key))
      
      ;; Add or update data in the transaction
      (setq found nil)
      (setq updated-data '())
      
      (foreach item MBS:transaction-data
        (if (and (= (car item) transaction-id)
                 (equal (cadr item) key))
          (progn
            (setq updated-data 
                  (cons (list transaction-id key value) updated-data))
            (setq found T)
          )
          (setq updated-data (cons item updated-data))
        )
      )
      
      ;; If key wasn't found, add it
      (if (not found)
        (setq updated-data 
              (cons (list transaction-id key value) updated-data))
      )
      
      ;; Update the data store
      (setq MBS:transaction-data updated-data)
      
      T
    )
    ;; If simple mode or invalid transaction, just return T
    T
  )
)

;; Retrieve transaction data
(defun MBS:get-transaction-data (transaction-id key)
  (if (and use-two-stage-transactions
           (or (= transaction-id MBS:transaction-active)
               (= transaction-id "LAST")))
    (progn
      ;; If LAST is specified, use the active transaction
      (if (= transaction-id "LAST")
        (setq transaction-id MBS:transaction-active)
      )
      
      ;; Search for the key in transaction data
      (setq result nil)
      (foreach item MBS:transaction-data
        (if (and (= (car item) transaction-id)
                 (equal (cadr item) key))
          (setq result (caddr item))
        )
      )
      
      result
    )
    nil
  )
)

;; Commit the transaction (make changes permanent)
(defun MBS:commit-transaction (transaction-id)
  (if (and use-two-stage-transactions
           (= transaction-id MBS:transaction-active))
    (progn
      (MBS:log-info (strcat "Committing transaction: " transaction-id))
      (MBS:log-info (strcat "Changes: " (itoa (length MBS:transaction-changes))))
      
      ;; Clear transaction data
      (setq MBS:transaction-active nil)
      (setq MBS:transaction-changes nil)
      (setq MBS:transaction-rollbacks nil)
      (setq MBS:transaction-data nil)
      
      T
    )
    ;; If simple mode or invalid transaction, just return T
    T
  )
)

;; Rollback the transaction (undo changes)
(defun MBS:rollback-transaction (transaction-id / rollback-result)
  (if (and use-two-stage-transactions
           (= transaction-id MBS:transaction-active))
    (progn
      (MBS:log-warning (strcat "Rolling back transaction: " transaction-id))
      (MBS:log-warning (strcat "Rollbacks to execute: " (itoa (length MBS:transaction-rollbacks))))
      
      ;; Execute rollback functions in reverse order (LIFO)
      (setq rollback-result T)
      (foreach rollback-item MBS:transaction-rollbacks
        (if (= (car rollback-item) transaction-id)
          (progn
            (MBS:log-verbose (strcat "Executing rollback for: " (vl-princ-to-string (cadr rollback-item))))
            (setq fn (caddr rollback-item))
            (if (not (fn))
              (progn
                (MBS:log-critical (strcat "Rollback failed for: " (vl-princ-to-string (cadr rollback-item))))
                (setq rollback-result nil)
              )
            )
          )
        )
      )
      
      ;; Clear transaction data
      (setq MBS:transaction-active nil)
      (setq MBS:transaction-changes nil)
      (setq MBS:transaction-rollbacks nil)
      (setq MBS:transaction-data nil)
      
      rollback-result
    )
    ;; If simple mode or invalid transaction, just return T
    T
  )
)

;;; ========================================================================
;; COMMON ROLLBACK FUNCTIONS
;;; ========================================================================

;; Generate a rollback function for a block attribute change
(defun MBS:create-attribute-rollback (block-ref att-tag old-value / rollback-fn)
  (setq rollback-fn 
    (lambda ( / atts att success)
      (if (MBS:valid-object-p block-ref)
        (progn
          (setq success nil)
          (setq atts (MBS:get-block-attributes block-ref))
          (foreach att atts
            (if (= (strcase (vlax-get att 'TagString)) (strcase att-tag))
              (progn
                (vlax-put att 'TextString old-value)
                (setq success T)
              )
            )
          )
          success
        )
        T  ;; Block no longer exists, consider rollback successful
      )
    )
  )
  
  rollback-fn
)

;; Generate a rollback function for a CSV record change
(defun MBS:create-csv-record-rollback (csv-path row-index old-record / rollback-fn)
  (setq rollback-fn 
    (lambda ( / csv-data header records)
      ;; Check if file exists
      (if (not (findfile csv-path))
        T  ;; File doesn't exist, can't rollback but return success
        (progn
          ;; Read the CSV
          (setq csv-data (MBS:read-csv csv-path))
          (if (not csv-data)
            nil  ;; Failed to read CSV
            (progn
              (setq header (car csv-data))
              (setq records (cadr csv-data))
              
              ;; Replace the record if index is valid
              (if (and (>= row-index 0) (< row-index (length records)))
                (progn
                  ;; Replace with old record
                  (setq records (MBS:replace-list-item records row-index old-record))
                  
                  ;; Write the updated CSV
                  (MBS:write-csv csv-path header records)
                )
                nil  ;; Invalid index
              )
            )
          )
        )
      )
    )
  )
  
  rollback-fn
)

;; Queue a transaction for later execution
(defun MBS:queue-transaction (tx)
  (if (not (boundp 'MBS:transaction-queue))
    (setq MBS:transaction-queue '())
  )
  
  ;; Handle different transaction types
  (cond
    ;; Delete row by material ID
    ((and (listp tx) (eq (car tx) 'delete-row-by-material-id))
     (setq MBS:transaction-queue (append MBS:transaction-queue (list tx)))
    )
    
    ;; Other transaction types (add as needed)
    (t
     (setq MBS:transaction-queue (append MBS:transaction-queue (list tx)))
    )
  )
  
  tx
)

;;; ========================================================================
;; TRANSACTION PREVIEW FUNCTIONS
;;; ========================================================================

;; Fixed preview function for material transactions with better description handling
(defun MBS:preview-material-transactions (csv-path / csv-data header records)
  ;; Read CSV to get column information
  (setq csv-data (MBS:read-csv csv-path))
  (if (not csv-data)
    (setq header nil records nil)
    (setq header (car csv-data)
          records (cadr csv-data))
  )
  
  ;; Find column indices
  (setq num-index (if header (MBS:get-column-index header "ITEM NO.") nil))
  (setq desc-index (if header (MBS:get-column-index header "DESCRIPTION") nil))
  (setq mat-id-index (if header (MBS:get-column-index header (strcase id-attribute)) nil))
  
  ;; Group transactions by type
  (setq insertions 0)
  (setq updates 0)
  (setq deletions 0)
  (setq insertion-details '())
  (setq update-details '())
  (setq deletion-details '())
  
  ;; Process each transaction
  (foreach tx MBS:transaction-queue
    (cond
      ;; Insert row
      ((eq (car tx) 'insert-row)
       (setq insertions (1+ insertions))
       (setq row-data (cadr tx))
       
       ;; Extract item info for preview with proper description handling
       (setq item-no (if (and num-index (>= (length row-data) (1+ num-index)))
                        (nth num-index row-data)
                        "TBD"))
       
       (setq desc (if (and desc-index (>= (length row-data) (1+ desc-index)))
                     (nth desc-index row-data)
                     (if (and (boundp 'MBS:temp-description) MBS:temp-description)
                       MBS:temp-description
                       "Unknown")))
                     
       ;; Add to details list
       (setq insertion-details 
             (append insertion-details
                    (list (strcat "Item: ITEM NO: (" item-no ") - " desc))))
      )
      
      ;; Update field
      ((eq (car tx) 'update-field)
       (setq updates (1+ updates))
       (setq row-index (cadr tx))
       (setq field (caddr tx))
       (setq value (cadddr tx))
       
       ;; If we have records, get more info
       (if (and records (< row-index (length records)))
         (progn
           (setq record (nth row-index records))
           (setq item-no (if (and num-index (>= (length record) (1+ num-index)))
                           (nth num-index record)
                           "?"))
                           
           (setq desc (if (and desc-index (>= (length record) (1+ desc-index)))
                        (nth desc-index record)
                        "Unknown"))
                        
           ;; Add to details list
           (setq update-details 
                 (append update-details
                        (list (strcat "Item: " item-no " - " desc ": " field " = " value))))
         )
       )
      )
      
      ;; Delete row by index
      ((eq (car tx) 'delete-row)
       (setq deletions (1+ deletions))
       (setq row-index (cadr tx))
       
       ;; If we have records, get more info
       (if (and records (< row-index (length records)))
         (progn
           (setq record (nth row-index records))
           (setq item-no (if (and num-index (>= (length record) (1+ num-index)))
                           (nth num-index record)
                           "?"))
                           
           (setq desc (if (and desc-index (>= (length record) (1+ desc-index)))
                        (nth desc-index record)
                        "Unknown"))
                        
           ;; Add to details list
           (setq deletion-details 
                 (append deletion-details
                        (list (strcat "Item: " item-no " - " desc))))
         )
       )
      )
      
      ;; Delete row by material ID
      ((eq (car tx) 'delete-row-by-material-id)
       (setq material-id (cadr tx))
       (setq deletions (1+ deletions))
       
       ;; Find record with this material ID
       (setq found-record nil)
       (foreach record records
         (if (and mat-id-index 
                 (>= (length record) (1+ mat-id-index))
                 (= (nth mat-id-index record) material-id))
           (setq found-record record)
         )
       )
       
       ;; If found, add details
       (if found-record
         (progn
           (setq item-no (if (and num-index (>= (length found-record) (1+ num-index)))
                           (nth num-index found-record)
                           "?"))
                           
           (setq desc (if (and desc-index (>= (length found-record) (1+ desc-index)))
                        (nth desc-index found-record)
                        "Unknown"))
                        
           ;; Add to details list
           (setq deletion-details 
                 (append deletion-details
                        (list (strcat "Item: " item-no " - " desc))))
         )
         ;; If not found, just show material ID
         (setq deletion-details 
               (append deletion-details
                      (list (strcat "Material ID: " material-id))))
       )
      )
      
      ;; Block updates
      ((eq (car tx) 'update-block-detailed)
       (setq updates (1+ updates))
      )
    )
  )
  
  ;; Display summary header
  (prompt "\n\n=== MATERIAL DATA CHANGES PREVIEW ===")
  (prompt "\nCSV Changes:")
  
  ;; Show insertions with details
  (prompt (strcat "\n  - Rows to insert: " (itoa insertions)))
  (foreach detail insertion-details
    (prompt (strcat "\n    " detail))
  )
  
  ;; Show updates with details (if not too many)
  (prompt (strcat "\n  - Rows to update: " (itoa updates)))
  (if (and (> updates 0) (<= updates 5))  ;; Only show details if 5 or fewer
    (foreach detail update-details
      (prompt (strcat "\n    " detail))
    )
  )
  
  ;; Show deletions with details
  (prompt (strcat "\n  - Rows to delete: " (itoa deletions)))
  (foreach detail deletion-details
    (prompt (strcat "\n    " detail))
  )
  
  ;; Summary section
  (prompt "\nSummary:")
  (if (> insertions 0)
    (prompt (strcat "\n  - Insert " (itoa insertions) " new rows to CSV"))
  )
  (if (> updates 0)
    (prompt (strcat "\n  - Update " (itoa updates) " existing rows/blocks"))
  )
  (if (> deletions 0)
    (prompt (strcat "\n  - Delete " (itoa deletions) " rows from CSV"))
  )
  
  ;; Ask for confirmation if there are any changes to preview
  (if (or (> insertions 0) (> updates 0) (> deletions 0))
    (progn
      (prompt "\n\nDo you want to proceed with these changes? [Y/N]: ")
      (setq response (strcase (getstring)))
      (= response "Y")
    )
    (progn
      ;; No changes to preview, return true to continue
      t
    )
  )
)

;; Modify the execute-material-transactions function to handle block insertion properly
(defun MBS:execute-material-transactions (csv-path)
  (prompt "\nExecuting material data transactions...")
  
  ;; Execute standard transactions (excluding any block insertions we already handled)
  (setq csv-updated (MBS:execute-transactions csv-path))
  
  ;; Return success/failure
  csv-updated
)

;; Execute update-field transaction
(defun MBS:exec-update-field (tx-data records header / row-index field-name value updated-record)
  (setq row-index (nth 0 tx-data))
  (setq field-name (nth 1 tx-data))
  (setq value (nth 2 tx-data))
  
  ;; Find field index in header
  (setq field-index (MBS:get-column-index header field-name))
  
  (if (and field-index (>= row-index 0) (< row-index (length records)))
    (progn
      ;; Get record to update
      (setq record (nth row-index records))
      
      ;; Ensure record is long enough
      (while (< (length record) (1+ field-index))
        (setq record (append record (list "")))
      )
      
      ;; Update the field
      (setq updated-record (MBS:replace-at-index record field-index value))
      
      ;; Replace in records list
      (setq records (MBS:replace-at-index records row-index updated-record))
      
      (list records header)
    )
    (progn
      (MBS:log-error (strcat "Invalid update-field parameters: row=" 
                            (itoa row-index) ", field=" field-name))
      (list records header)  ;; Return unchanged
    )
  )
)

;; Execute insert-row transaction
(defun MBS:exec-insert-row (row-data records header)
  ;; Simply append the new row to records
  (setq records (append records (list row-data)))
  (list records header)
)

;; Execute delete-row transaction
(defun MBS:exec-delete-row (row-index records header / new-records i)
  (if (and (>= row-index 0) (< row-index (length records)))
    (progn
      ;; Create new records list excluding the specified index
      (setq new-records '())
      (setq i 0)
      (foreach record records
        (if (/= i row-index)
          (setq new-records (append new-records (list record)))
        )
        (setq i (1+ i))
      )
      (list new-records header)
    )
    (progn
      (MBS:log-error (strcat "Invalid row index for deletion: " (itoa row-index)))
      (list records header)  ;; Return unchanged
    )
  )
)

;; Execute delete-row-by-material-id transaction
(defun MBS:exec-delete-row-by-material-id (material-id records header mat-id-index / new-records)
  (setq new-records '())
  
  (if mat-id-index
    (progn
      ;; Loop through all records
      (foreach record records
        ;; Only keep records that don't match the material ID
        (if (or (< (length record) (1+ mat-id-index))
                (/= (nth mat-id-index record) material-id))
          (setq new-records (append new-records (list record)))
        )
      )
      (list new-records header)
    )
    (progn
      (MBS:log-error "Material ID column not found for deletion by material ID")
      (list records header)  ;; Return unchanged
    )
  )
)

;; Execute a single transaction
(defun MBS:execute-transaction (tx records header / tx-type tx-data mat-id-index result)
  (setq tx-type (car tx))
  (setq tx-data (cdr tx))
  
  (MBS:log-info (strcat "Executing transaction type: " (vl-princ-to-string tx-type)))
  
  (cond
    ;; Update a field value in a record
    ((eq tx-type 'update-field)
      (setq result (MBS:exec-update-field tx-data records header))
    )
    
    ;; Insert a new row
    ((eq tx-type 'insert-row)
      (setq result (MBS:exec-insert-row tx-data records header))
    )
    
    ;; Delete a row by index
    ((eq tx-type 'delete-row)
      (setq result (MBS:exec-delete-row tx-data records header))
    )
    
    ;; Delete a row by material ID
    ((eq tx-type 'delete-row-by-material-id)
      (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
      (if (>= mat-id-index 0)
        (progn
          (MBS:log-info (strcat "Deleting row with material ID: " (car tx-data)))
          (MBS:log-info (strcat "Material ID column index: " (itoa mat-id-index)))
          (setq result (MBS:exec-delete-row-by-material-id (car tx-data) records header mat-id-index))
        )
        (progn
          (MBS:log-error (strcat "Material ID column not found in header: " (strcase id-attribute)))
          (setq result (list records header))  ;; Return unchanged
        )
      )
    )
    
    ;; Insert a block
    ((eq tx-type 'insert-block)
      ;; This is executed separately by MBS:sync-blocks-with-csv
      (setq result (list records header))
    )
    
    ;; Unknown transaction type
    (t
      (MBS:log-error (strcat "Unknown transaction type: " (vl-princ-to-string tx-type)))
      (setq result (list records header))
    )
  )
  
  result
)

;; Execute all transactions in the queue
(defun MBS:execute-transactions (csv-path / csv-data header records changes tx result old-count new-count)
  (MBS:log-info "Starting transaction execution")
  
  ;; Read current CSV data
  (MBS:log-info (strcat "Reading CSV file: " csv-path))
  (setq csv-data (MBS:read-csv csv-path))
  (if (not csv-data)
    (progn
      (MBS:log-error "Failed to read CSV for transaction execution")
      nil
    )
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      (setq old-count (length records))
      (setq changes nil)
      
      (MBS:log-info (strcat "Successfully read " (itoa old-count) " data rows"))
      (MBS:log-info (strcat "Applying " (itoa (length MBS:transaction-queue)) " transactions"))
      
      ;; Process each transaction
      (foreach tx MBS:transaction-queue
        (MBS:log-info (strcat "Processing transaction type: " (vl-princ-to-string (car tx))))
        
        ;; Execute the transaction
        (setq result (MBS:execute-transaction tx records header))
        
        ;; Check result
        (if result
          (progn
            (setq records (car result))
            (setq header (cadr result))
            (setq new-count (length records))
            
            ;; Check if row count changed
            (if (/= old-count new-count)
              (progn
                (MBS:log-info (strcat "Row count changed from " (itoa old-count) 
                                    " to " (itoa new-count)))
                (setq changes T)
                (setq old-count new-count)
              )
            )
          )
        )
      )
      
      ;; Write changes back to CSV if any were made
      (if changes
        (progn
          (MBS:log-info "Changes detected, writing to CSV")
          (MBS:log-info (strcat "Writing " (itoa (length records)) " records to " csv-path))
          (if (MBS:write-csv csv-path header records)
            (progn
              (MBS:log-info "Successfully wrote changes to CSV")
              T
            )
            (progn
              (MBS:log-error "Failed to write changes to CSV")
              nil
            )
          )
        )
        (progn
          (MBS:log-info "No changes needed to CSV")
          T
        )
      )
    )
  )
)

;; Enhanced execute block transactions function
(defun MBS:execute-block-update-transactions (/ blocks-updated)
  (setq blocks-updated 0)
  
  ;; Process each block transaction
  (foreach tx MBS:transaction-queue
    (cond
      ;; Update block with detailed info
      ((eq (car tx) 'update-block-detailed)
       (setq blk-ref (nth 1 tx))
       (setq field-updates (nth 2 tx))
       
       ;; Check if block reference is valid
       (if (and blk-ref 
                (= (type blk-ref) 'VLA-OBJECT)
                (MBS:is-object-alive blk-ref))
         (progn
           ;; Get attributes
           (setq att-list (vlax-invoke blk-ref 'GetAttributes))
           
           ;; Update each attribute
           (foreach update field-updates
             (setq tag (car update))
             (setq new-val (nth 2 update))
             
             ;; Find and update matching attribute
             (foreach att att-list
               (if (= (strcase (vlax-get att 'TagString)) (strcase tag))
                 (vlax-put att 'TextString new-val)
               )
             )
           )
           
           ;; Update the block
           (vla-Update blk-ref)
           (setq blocks-updated (1+ blocks-updated))
         )
       )
      )
      
      ;; Insert block at specific point
      ((eq (car tx) 'insert-block-at-point)
       (setq row-index (nth 1 tx))
       (setq material-id (nth 2 tx))
       (setq desc (nth 3 tx))
       (setq item-no (nth 4 tx))
       (setq target-space (nth 5 tx))
       (setq ins-pt (nth 6 tx))
       
       ;; Insert the block
       (setq blkRef nil)
       (vl-catch-all-apply
         '(lambda ()
           (setq blkRef (vla-InsertBlock 
                          target-space
                          (vlax-3d-point (car ins-pt) (cadr ins-pt) 0.0)
                          (get-block-name)
                          1.0
                          1.0
                          1.0
                          0.0))
         )
       )
       
       (if blkRef
         (progn
           ;; Set the attributes
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
           
           ;; Update the block
           (vla-Update blkRef)
           (setq blocks-updated (1+ blocks-updated))
         )
       )
      )
      
      ;; Delete block
      ((eq (car tx) 'delete-block)
       (setq blk-ref (nth 1 tx))
       
       ;; Check if block reference is valid
       (if (and blk-ref 
                (= (type blk-ref) 'VLA-OBJECT)
                (MBS:is-object-alive blk-ref))
         (progn
           ;; Delete the block
           (vla-Delete blk-ref)
           (setq blocks-updated (1+ blocks-updated))
         )
       )
      )
    )
  )
  
  blocks-updated
)

;; Helper function to replace an element at a specific index in a list
(defun MBS:replace-at-index (lst index new-val / result i)
  (setq result '())
  (setq i 0)
  (foreach item lst
    (if (= i index)
      (setq result (append result (list new-val)))
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  result
)

;; Transaction to renumber empty items
(defun MBS:tx-renumber-empty-items (prefix records-to-number)
  (list 'renumber-empty-items prefix records-to-number)
)

;; Execute delete-row-by-material-id transaction (CLEAN VERSION)
(defun MBS:exec-delete-row-by-material-id (material-id records header mat-id-index / new-records kept-count deleted-count row-index)
  (setq new-records '())
  (setq kept-count 0)
  (setq deleted-count 0)
  (setq row-index 0)
  
  (if mat-id-index
    (progn
      ;; Log the material ID we're trying to delete
      (MBS:log-info (strcat "Attempting to delete row with material ID: " material-id))
      
      ;; Loop through all records
      (foreach record records
        ;; Check if this record has the material ID we want to delete
        (if (and (>= (length record) (1+ mat-id-index))
                 (MBS:material-id-match (nth mat-id-index record) material-id))
          (progn
            ;; Skip this record (delete it)
            (setq deleted-count (1+ deleted-count))
            (MBS:log-info (strcat "Deleting record with material ID: " material-id 
                                " at row " (itoa row-index)))
          )
          (progn
            ;; Keep this record
            (setq new-records (append new-records (list record)))
            (setq kept-count (1+ kept-count))
          )
        )
        (setq row-index (1+ row-index))
      )
      
      ;; If no records were deleted, log an error
      (if (= deleted-count 0)
        (progn
          (MBS:log-warning (strcat "No records found with Material ID: " material-id))
          (list records header)  ;; Return unchanged records
        )
        (progn
          (MBS:log-info (strcat "Deleted " (itoa deleted-count) 
                              " records with Material ID: " material-id))
          (list new-records header)
        )
      )
    )
    (progn
      (MBS:log-error (strcat "Material ID column not found for deletion by Material ID"))
      (list records header)  ;; Return unchanged
    )
  )
)

;; Helper to replace an item in a list
(defun MBS:replace-list-item (lst index new-item / result i)
  (setq result '())
  (setq i 0)
  (foreach item lst
    (if (= i index)
      (setq result (append result (list new-item)))
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  result
)

;; Helper function to safely compare material IDs
(defun MBS:material-id-match (id1 id2 / cleaned-id1 cleaned-id2)
  ;; Clean and normalize both IDs
  (setq cleaned-id1 (vl-string-trim " \t\n\r" id1))
  (setq cleaned-id2 (vl-string-trim " \t\n\r" id2))
  
  ;; Try different comparison methods
  (or 
    ;; Exact match
    (= id1 id2)
    ;; Case-insensitive match
    (= (strcase id1) (strcase id2))
    ;; Trimmed match
    (= cleaned-id1 cleaned-id2)
    ;; Trimmed case-insensitive match
    (= (strcase cleaned-id1) (strcase cleaned-id2))
  )
)





(prompt "\nMaterial Block Sync Transaction System loaded.")
(princ)