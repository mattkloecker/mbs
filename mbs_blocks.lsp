;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - Block Operations Module
;; VERSION 5.5
;;; ========================================================================
;; This module implements the block operations for Material Block Sync:
;; - Single block insertion (MBS-Insert2)
;; - Batch block insertion (MBS-BATCHINSERT2)
;; - Block updates (MBS-UpdateBlocks2)
;;
;; Dependencies: mbs_core.lsp, mbs_config.lsp, mbs_csv.lsp, mbs_transaction.lsp
;;; ========================================================================

	;; Check if already loaded to prevent redefinition
	(if (not (boundp 'MBS:blocks-loaded))
	  (progn  ;; Start of the true branch
		(setq MBS:blocks-loaded T)
    
    ;;; ========================================================================
    ;; MAIN ENTRY POINTS - TWO-STAGE TRANSACTIONS
    ;;; ========================================================================
    
    ;; Two-stage INSERT implementation
	(defun C:MBS-INSERT2 (/ csv-path)
	  (if (not (MBS:init-environment))
		(progn
		  (prompt "\nFailed to initialize environment.")
		  (princ)
		)
		(progn
		  ;; Get CSV path using your existing functions
		  (setq csv-path (get-csv-path-from-block))
		  (if (not csv-path)
			(progn
			  (prompt "\nNo CSV file specified or selected.")
			  (princ)
			)
			(progn
			  (prompt (strcat "\n📊 [SINGLE INSERT MODE – Two-stage process]"))
			  (prompt (strcat "\nUsing CSV path: " csv-path))
			  
			  ;; STAGE 1: Material Data Insertion
			  (prompt "\n=== STAGE 1: MATERIAL DATA INSERTION ===")
			  
			  ;; Perform single insertion
			  (setq result (MBS:perform-single-insert csv-path))
			  
			  ;; If successful, move to stage 2
			  (if result
				(progn
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
    
	;; Two-stage BATCHINSERT 
	(defun C:MBS-BATCHINSERT2 (/ csv-path sync-blocks)
	  (if (not (MBS:init-environment))
		(progn
		  (prompt "\nFailed to initialize environment.")
		  (princ)
		)
		(progn
		  ;; Get CSV path using your existing functions
		  (setq csv-path (get-csv-path-from-block))
		  (if (not csv-path)
			(progn
			  (prompt "\nNo CSV file specified or selected.")
			  (princ)
			)
			(progn
			  (prompt (strcat "\n📊 [BATCH INSERT MODE – Two-stage process]"))
			  (prompt (strcat "\nUsing CSV path: " csv-path))
			  
			  ;; STAGE 1: Material Data Insertion
			  (prompt "\n=== STAGE 1: MATERIAL DATA INSERTION ===")
			  
			  ;; Perform batch insertion
			  (setq result (MBS:perform-batch-insert csv-path))
			  
			  ;; If successful, move to stage 2
			  (if result
				(progn
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
    
	;; Two-stage UPDATE implementation
	(defun c:MBS-UpdateBlocks2 ( / csv-path)
	  (prompt "\n📊 [UPDATE MODE – Two-stage update process]")
	  
	  ;; Get CSV file path
	  (setq csv-path (get-csv-path-from-block))
	  (if (not csv-path) 
		(progn 
		  (prompt "\nNo CSV path found. Operation cancelled.")
		  (princ)
		)
		(progn
		  ;; STAGE 1: Perform material data operations
		  (prompt "\n=== STAGE 1: MATERIAL DATA UPDATE ===")
		  (setq result (MBS:process-material-updates csv-path))
		  
		  ;; STAGE 2: Update item numbers if Stage 1 was successful
		  (if result
			(progn
			  (prompt "\n\n=== STAGE 2: ITEM NUMBER UPDATE ===")
			  (if (MBS:update-all-item-numbers csv-path)
				(progn
				  ;; Final block sync
				  (prompt "\n\nSynchronizing blocks with updated data...")
				  (MBS:update-blocks-safely csv-path)
				  (prompt "\n\n✓ Update process complete.")
				)
				(prompt "\n\n⚠ Item number update cancelled or failed. Some blocks may need manual updates.")
			  )
			)
			(prompt "\n\n⚠ Material data update cancelled or failed.")
		  )
		)
	  )
	  (princ)
	)
    
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
		  (setq num-index (MBS:get-column-index header "ITEM NO."))
		  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
		  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
		  
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
		  (setq num-index (MBS:get-column-index header "ITEM NO."))
		  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
		  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
		  
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
				 (setq result (MBS:insert-material-blocks-with-fixed-spacing records header num-index desc-index mat-id-index))
				)
				
				;; Insert selected records
				((= method "Selected")
				 (setq selected-records (MBS:prompt-for-row-selection records header))
				 (if selected-records
				   (setq result (MBS:insert-material-blocks-with-fixed-spacing selected-records header num-index desc-index mat-id-index))
				   (setq result nil)
				 )
				)
				
				;; Filter records
				((= method "Filter")
				 (setq filtered-records (MBS:prompt-for-filtered-rows records header))
				 (if filtered-records
				   (setq result (MBS:insert-material-blocks-with-fixed-spacing filtered-records header num-index desc-index mat-id-index))
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
		  (setq num-index (MBS:get-column-index header "ITEM NO."))
		  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
		  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
		  
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
			  (setq num-index (MBS:get-column-index header "ITEM NO."))
			  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
			  
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
					  ;; Create preview of changes
					  (setq preview-text "\n=== ITEM NUMBER CHANGES PREVIEW ===\n")
					  (setq changes-by-prefix '())
					  (setq any-changes nil)
					  
					  ;; Process each prefix group
					  (foreach prefix-group prefix-groups
						(setq prefix (car prefix-group))
						(setq group-records (cdr prefix-group))
						
						;; Force renumbering this group
						(setq renumber-result 
							  (vl-catch-all-apply
								'(lambda () 
								  (MBS:preview-renumber-prefix-group 
								   group-records (cons header records) num-index prefix))))
						
						(if (vl-catch-all-error-p renumber-result)
						  (prompt (strcat "\nWARNING: Error processing prefix '" prefix "'"))
						  (progn
							(setq changes (car renumber-result))
							
							;; Add to changes list if any changes found
							(if (> (length changes) 0)
							  (progn
								(setq changes-by-prefix 
									  (append changes-by-prefix (list (cons prefix changes))))
								(setq any-changes t)
							  )
							)
						  )
						)
					  )
					  
					  ;; Build enhanced preview text for all changes
					  (foreach prefix-change changes-by-prefix
						(setq prefix (car prefix-change))
						(setq changes (cdr prefix-change))
						
						(setq preview-text (strcat preview-text 
												 "\nPrefix '" prefix "' changes:"))
						
						(foreach change changes
						  (setq old-val (car change))
						  (setq new-val (cadr change))
						  (setq description (caddr change))
						  
						  (setq preview-text (strcat preview-text 
													"\nItem: " 
													(if (= description "") "Unknown" description)
													": " 
													(if (= old-val "") "(empty)" old-val) 
													" → " new-val))
						)
					  )
					  
					  ;; Add prefix sections with "None" if no changes for a prefix
					  (setq all-prefixes (list "" "P" "V" "F"))
					  (foreach prefix all-prefixes
						(if (not (assoc prefix changes-by-prefix))
						  (setq preview-text (strcat preview-text 
													"\nPrefix '" prefix "' changes: None"))
						)
					  )
					  
					  ;; Show preview
					  (prompt preview-text)
					  
					  ;; Decide whether to ask for confirmation based on auto-accept setting
					  (if (and any-changes (not auto-accept-item-number-changes))
						(progn
						  ;; Manual confirmation
						  (prompt "\n\nDo you want to apply these item number changes? [Y/N]: ")
						  (setq response (strcase (getstring)))
						)
						(progn
						  ;; Auto-accept
						  (if auto-accept-item-number-changes
							(prompt "\n\n[Auto-accept is ON - automatically applying changes]")
						  )
						  (setq response "Y")
						)
					  )
					  
					  ;; Process the response
					  (if (= response "Y")
						(progn
						  ;; Apply all renumbering
						  (foreach prefix-group prefix-groups
							(setq prefix (car prefix-group))
							(setq group-records (cdr prefix-group))
							
							;; Apply renumbering (will always modify since we're forcing it)
							(setq result (MBS:apply-force-renumber-prefix-group 
										  group-records records header num-index prefix))
							(setq records (car result))
							(if (cadr result) (setq modified t))
						  )
						  
						  ;; Sort the CSV
						  (setq records (MBS:sort-records-by-item-no records num-index))
						  
						  ;; Write changes back to CSV
						  (if modified
							(progn
							  (if (MBS:write-csv csv-path header records)
								(progn
								  (prompt "\n✓ Item numbers updated successfully.")
								  t  ;; Return success
								)
								(progn
								  (prompt "\n❌ Failed to write updated item numbers to CSV.")
								  nil
								)
							  )
							)
							(progn
							  (prompt "\nNo item number changes needed.")
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
		  (setq num-index (MBS:get-column-index header "ITEM NO."))
		  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
		  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
		  
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
		  (setq num-index (MBS:get-column-index header "ITEM NO."))
		  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
		  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
		  
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
	  (setq num-index (MBS:get-column-index header "ITEM NO."))
	  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
	  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
	  
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
	
	;; Get all blocks with material IDs
	(defun MBS:get-blocks-with-ids (/ doc block-map)
	  (prompt "\nFinding blocks with material IDs...")
	  
	  (setq block-map '())
	  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
	  
	  ;; Check modelspace
	  (vlax-for ent (vla-get-ModelSpace doc)
		(if (= (vla-get-ObjectName ent) "AcDbBlockReference")
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
		(if (= (vla-get-ObjectName ent) "AcDbBlockReference")
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
	(defun MBS:insert-material-blocks-with-fixed-spacing (rows header num-index desc-index mat-id-index / count)
	  (setq count 0)
	  
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
		  
		  (foreach record rows
			;; Extract data
			(setq material-id (nth mat-id-index record))
			(setq desc (nth desc-index record))
			(setq item-no (nth num-index record))
			
			;; Calculate insertion point
			(setq ins-x (+ (car base-pt) (* col-num x-spacing)))
			(setq ins-y (- (cadr base-pt) (* row-num y-spacing)))
			(setq ins-pt (list ins-x ins-y))
			
			;; Insert block
			(setq block (MBS:insert-block-with-attributes 
						target-space ins-pt material-id desc item-no))
			
			(if block
			  (setq count (1+ count))
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
		  
		  (list count)
		)
		nil
	  )
	)
    
    ;; Insert multiple records
	(defun MBS:insert-multiple-records (record-pairs header num-index desc-index mat-id-index)
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
		(MBS:insert-multiple-single record-pairs target-space header num-index desc-index mat-id-index)
		;; Insert as grid
		(MBS:insert-multiple-grid record-pairs target-space header num-index desc-index mat-id-index)
	  )
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
		  t
		)
		(progn
		  (prompt "\nNo blocks inserted.")
		  nil
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
			
			;; Calculate insertion point
			(setq ins-x (+ (car base-pt) (* col-num x-spacing)))
			(setq ins-y (- (cadr base-pt) (* row-num y-spacing)))
			(setq ins-pt (list ins-x ins-y))
			
			;; Insert block
			(setq block (MBS:insert-block-with-attributes 
						target-space ins-pt material-id desc item-no))
			
			(if block
			  (setq count (1+ count))
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
			  t
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
    
	;; Update blocks from CSV data
	(defun MBS:update-blocks-from-csv (block-map records header num-index desc-index mat-id-index / updates)
	  (setq updates 0)
	  
	  ;; Process each record
	  (foreach record records
		(if (>= (length record) (1+ mat-id-index))
		  (progn
			(setq material-id (nth mat-id-index record))
			(setq item-no (nth num-index record))
			(setq desc (nth desc-index record))
			
			;; Find matching block
			(setq block-pair (assoc material-id block-map))
			(if block-pair
			  (progn
				(setq block (cdr block-pair))
				
				;; Update attributes
				(setq att-list (vlax-invoke block 'GetAttributes))
				(foreach att att-list
				  (cond
					((= (strcase (vlax-get att 'TagString)) "##")
					 (vlax-put att 'TextString item-no))
					((= (strcase (vlax-get att 'TagString)) "DESCRIPTION")
					 (if (boundp 'shorten-description)
					   (vlax-put att 'TextString (shorten-description desc))
					   (vlax-put att 'TextString desc)))
				  )
				)
				
				(vla-Update block)
				(setq updates (1+ updates))
			  )
			)
		  )
		)
	  )
	  
	  (prompt (strcat "\nUpdated " (itoa updates) " blocks with new item numbers and descriptions."))
	)
	
	;; Get all blocks with material IDs
	(defun MBS:get-blocks-with-ids (/ doc block-map)
	  (prompt "\nFinding blocks with material IDs...")
	  
	  (setq block-map '())
	  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
	  
	  ;; Check modelspace
	  (vlax-for ent (vla-get-ModelSpace doc)
		(if (= (vla-get-ObjectName ent) "AcDbBlockReference")
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
		(if (= (vla-get-ObjectName ent) "AcDbBlockReference")
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
	  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
	  (setq item-num-index (MBS:get-column-index header "ITEM NO."))
	  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
	  
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
		; ; (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
		; ; (setq desc (if (and desc-index (>= (length record) (1+ desc-index)))
					  ; ; (nth desc-index record)
					  ; ; "Unknown"))
		
		; ; ;; Get item number
		; ; (setq num-index (MBS:get-column-index header "ITEM NO."))
		; ; (setq item-no (if (and num-index (>= (length record) (1+ num-index)))
						 ; ; (nth num-index record)
						 ; ; "TBD"))
		
		; ; ;; Get material ID
		; ; (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
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
    
    ;; Process blocks with empty Material IDs
	(defun MBS:process-empty-id-blocks-simplified (empty-blocks records header / choice grouped-blocks)
	  (prompt "\n=== BLOCKS WITH EMPTY MATERIAL IDs DETECTED ===")
	  (prompt "\nThe following blocks have empty or missing Material IDs:")
	  
	  ;; First, group blocks by description
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
	  
	  ;; Now process each group
	  (foreach group grouped-blocks
		(setq desc (car group))
		(setq block-list (cdr group))
		(setq first-block (car (car block-list)))
		
		;; Display group info
		(prompt (strcat "\n\nDescription: " desc))
		(prompt (strcat "\nFound " (itoa (length block-list)) " block(s) with this description"))
		
		;; Get the first item number if any are non-empty
		(setq item-no "")
		(foreach block-pair block-list
		  (if (and (/= (cdr block-pair) "") (= item-no ""))
			(setq item-no (cdr block-pair))
		  )
		)
		
		;; Show info
		(prompt (strcat "\nItem: " (if (/= item-no "") item-no "TBD") " - " desc))
		
		;; NEW: Look for potential CSV matches
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
						 (MBS:queue-transaction (list 'update-field row-index (strcase id-attribute) material-id))
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
					 (setq material-id (cadr (nth (1- match-num) match-list)))
					 (setq material-id nil)
				   )
				 )
			   )
			   
			   ;; Update all blocks with the selected material ID
			   (if material-id
				 (progn
				   (foreach block-pair block-list
					 (setq block (car block-pair))
					 (setq att-list (vlax-invoke block 'GetAttributes))
					 (foreach att att-list
					   (if (= (strcase (vlax-get att 'TagString)) (strcase id-attribute))
						 (vlax-put att 'TextString material-id)
					   )
					 )
					 (vla-Update block)
				   )
				   (prompt (strcat "\n✓ Updated " (itoa (length block-list)) " block(s) with Material ID: " material-id))
				 )
				 (prompt "\n❌ Invalid match number. No updates made.")
			   )
			  )
			  
			  ;; Insert to CSV option (same as before)
			  ((= choice "Insert")
			   ;; Create new row
			   (setq new-record (MBS:create-empty-record header))
			   
			   ;; Generate a new material ID
			   (setq new-material-id (generate-unique-id desc))
			   
			   ;; Set material ID
			   (setq new-record (MBS:set-record-value new-record header (strcase id-attribute) new-material-id))
			   
			   ;; Set description
			   (setq new-record (MBS:set-record-value new-record header "DESCRIPTION" desc))
			   
			   ;; Use existing item number or TBD
			   (if (and item-no (/= item-no ""))
				 (setq new-record (MBS:set-record-value new-record header "ITEM NO." item-no))
				 (setq new-record (MBS:set-record-value new-record header "ITEM NO." "TBD"))
			   )
			   
			   ;; Queue transaction to insert the row
			   (MBS:queue-transaction (MBS:tx-insert-row new-record))
			   
			   ;; Update ALL blocks in this group with the new material ID
			   (foreach block-pair block-list
				 (setq block (car block-pair))
				 (setq att-list (vlax-invoke block 'GetAttributes))
				 (foreach att att-list
				   (if (= (strcase (vlax-get att 'TagString)) (strcase id-attribute))
					 (vlax-put att 'TextString new-material-id)
				   )
				 )
				 (vla-Update block)
			   )
			   
			   (prompt (strcat "\n✓ Updated " (itoa (length block-list)) " block(s) with new material ID"))
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
			
			;; [Rest of function identical to the original]
			(cond
			  ;; Insert to CSV option
		  ;; Insert to CSV option
		  ((= choice "Insert")
		   ;; Create new row
		   (setq new-record (MBS:create-empty-record header))
		   
		   ;; Generate a new material ID
		   (setq new-material-id (generate-unique-id desc))
		   
		   ;; Set material ID
		   (setq new-record (MBS:set-record-value new-record header (strcase id-attribute) new-material-id))
		   
		   ;; Set description - CLEANED
		   (setq new-record (MBS:set-record-value new-record header "DESCRIPTION" desc))
		   
		   ;; Use existing item number or TBD
		   (if (and item-no (/= item-no ""))
			 (setq new-record (MBS:set-record-value new-record header "ITEM NO." item-no))
			 (setq new-record (MBS:set-record-value new-record header "ITEM NO." "TBD"))
		   )
		   
		   ;; Queue transaction to insert the row - JUST ONCE PER GROUP
		   (MBS:queue-transaction (MBS:tx-insert-row new-record))
		   
		   ;; Update ALL blocks in this group with the new material ID
		   (foreach block-pair block-list
			 (setq block (car block-pair))
			 (setq att-list (vlax-invoke block 'GetAttributes))
			 (foreach att att-list
			   (if (= (strcase (vlax-get att 'TagString)) (strcase id-attribute))
				 (vlax-put att 'TextString new-material-id)
			   )
			 )
			 (vla-Update block)
		   )
		   
		   (prompt (strcat "\n✓ Updated " (itoa (length block-list)) " block(s) with new material ID"))
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
	  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
	  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
	  
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
	  (setq material-id-index (MBS:get-column-index header (strcase id-attribute)))
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
		  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
		  (if (not desc-index)
			nil
			(progn
			  (setq exact-matches '())
			  (setq fuzzy-matches '())
			  
			  ;; Find material ID column index
			  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
			  
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
    
    ;;; ========================================================================
    ;; ITEM NUMBERING SUPPORT FUNCTIONS
    ;;; ========================================================================
    
		;; Force renumbering for a prefix group
	(defun MBS:force-renumber-prefix-group (group-records original-records num-index prefix / changes)
	  (setq changes '())
	  
	  ;; Get header from original records
	  (setq header (car original-records))
	  
	  ;; Find description column index in header
	  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
	  
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
			  (setq num-index (MBS:get-column-index header "ITEM NO."))
			  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
			  
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
	
	;; Sort a group of records numerically
	(defun MBS:sort-group-numerically (records num-index prefix / sorted)
	  (if (= (length records) 0)
		'()  ;; Empty group
		(progn
		  ;; Add numeric value for sorting
		  (setq records-with-values '())
		  (foreach record records
			(setq item-no (if (>= (length record) (1+ num-index))
							(nth num-index record)
							""))
			
			;; Extract numeric part
			(setq num-part item-no)
			(if (and (> (strlen item-no) 0) 
					 (> (strlen prefix) 0)
					 (= (strcase (substr item-no 1 (strlen prefix))) (strcase prefix)))
			  (setq num-part (substr item-no (1+ (strlen prefix))))
			)
			
			;; Convert to number (default to 999999 if can't parse)
			(setq num-val (atoi num-part))
			(if (= num-val 0) (setq num-val 999999))  ;; Put invalid numbers at end
			
			;; Add to list with numeric value
			(setq records-with-values (cons (list num-val record) records-with-values))
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
			
			;; Add record to group
			(setq grouped-records 
				  (subst (cons (car group) (append (cdr group) (list record)))
						 group
						 grouped-records))
		  )
		  ;; If record is too short, skip it
		  (MBS:log-warning (strcat "Record too short for sorting: " (vl-princ-to-string record)))
		)
	  )
	  
	  ;; Sort each group numerically
	  (setq sorted-groups '())
	  (foreach group grouped-records
		(setq prefix (car group))
		(setq group-records (cdr group))
		
		;; Sort this group
		(setq sorted-group (MBS:sort-group-numerically group-records num-index prefix))
		
		;; Add to sorted groups
		(setq sorted-groups (append sorted-groups (list (cons prefix sorted-group))))
	  )
	  
	  ;; Combine groups in correct order: numeric first, then P, V, F, OTHER
	  (setq result '())
	  (setq result (append result (cdr (assoc "" sorted-groups))))
	  (setq result (append result (cdr (assoc "P" sorted-groups))))
	  (setq result (append result (cdr (assoc "V" sorted-groups))))
	  (setq result (append result (cdr (assoc "F" sorted-groups))))
	  (setq result (append result (cdr (assoc "OTHER" sorted-groups))))
	  
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
	
    ;; Group records by prefix
	(defun MBS:group-records-by-prefix (records num-index desc-index / result)
	  (setq result '())
	  
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
				  (if (and (>= (length record) (1+ desc-index))
						  (nth desc-index record)
						  (/= (nth desc-index record) ""))
					(setq prefix (detect-prefix-from-description (nth desc-index record)))
					(setq prefix "")  ;; Default if no description
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
	  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
	  
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
		  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
		  (if (not desc-index)
			nil
			(progn
			  (setq exact-matches '())
			  (setq fuzzy-matches '())
			  
			  ;; Find material ID column index
			  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
			  
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
    
    ;;; ========================================================================
    ;; HELPER FUNCTIONS
    ;;; ========================================================================
    
		;; Set the value in a record at a specific column name
	(defun MBS:set-record-value (record header column-name value / index new-record)
	  (setq index (MBS:get-column-index header column-name))
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

	;; Helper function for integer division with ceiling result
	(defun MBS:ceiling-div (a b)
	  (if (= (rem a b) 0)
		(/ a b)
		(1+ (/ (- a (rem a b)) b))
	  )
	)
	
	;; generate unique material id or removal id
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
	  
	  ;; Format as a unique ID with prefix
	  (strcat "MAT-" (rtos hash-value 2 0))
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
	  (setq num-index (MBS:get-column-index header "ITEM NO."))
	  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
	  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
	  
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
	
	
	;;; ========================================================================
	;; IMPROVED ITEM NUMBERING SYSTEM
	;;; ========================================================================
	
	;; Function to detect and handle empty ITEM NO. fields
	(defun MBS:detect-empty-item-numbers (id-map records header / needs-numbering)
	  (MBS:log-info "Checking for empty item numbers in CSV")
	  
	  ;; Get column indices
	  (setq num-index (MBS:get-column-index header "ITEM NO."))
	  (setq desc-index (MBS:get-column-index header "DESCRIPTION"))
	  (setq mat-id-index (MBS:get-column-index header (strcase id-attribute)))
	  
	  (if (or (not num-index) (not desc-index) (not mat-id-index))
		(progn
		  (MBS:log-warning "Missing required columns for item number check")
		  nil
		)
		(progn
		  ;; Group by prefix for consistent numbering
		  (setq prefix-records '())
		  
		  ;; Find records with empty item numbers but with matching blocks
		  (setq row-index 0)
		  (foreach record records
			(setq item-no (if (and (>= (length record) (1+ num-index)))
							(nth num-index record)
							""))
			
			(setq material-id (if (and (>= (length record) (1+ mat-id-index)))
								(nth mat-id-index record)
								""))
			
			;; Check if this record has a matching block
			(if (and (/= material-id "") (assoc material-id id-map))
			  (progn
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
		  
		  ;; Return whether numbering is needed
		  needs-numbering
		)
	  )
	)
	
	;; Reset the global prefix map when analyzing numbers
	(defun MBS:analyze-item-numbers (records header / num-index prefix-map)
	  (MBS:log-info "Analyzing existing item numbers in CSV")
	  
	  ;; Find the ITEM NO column
	  (setq num-index (MBS:get-column-index header "ITEM NO."))
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
	
	
 
  ) ;; End of progn (true branch of if)
) ;; End of if

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