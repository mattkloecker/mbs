;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - Debug Module
;; VERSION 5.5
;;; ========================================================================
;; This module implements the debug logging and management functionality
;;; ========================================================================

;; Check if already loaded to prevent redefinition
(if (not (boundp 'MBS:debug-loaded))
  (progn
    (setq MBS:debug-loaded T)
    
    ;;; ========================================================================
    ;; DEBUG SYSTEM INITIALIZATION
    ;;; ========================================================================
    
    ;; Set default debug level (0=Off, 1=Error, 2=Warning, 3=Info, 4=Verbose)
    (defun MBS:initialize-debug-system ()
      ;; Set default level to INFO (3) if not already set
      (if (not (boundp 'MBS:debug-level))
        (setq MBS:debug-level 3)  ;; Default to INFO level (numeric)
      )
      
      ;; Set default output to console if not set
      (if (not (boundp 'MBS:debug-output))
        (setq MBS:debug-output 'console)  ;; Default to console
      )
      
      ;; Set default log file path if not set
      (if (not (boundp 'MBS:debug-log-file))
        (setq MBS:debug-log-file (strcat (getenv "TEMP") "\\mbs_debug.log"))
      )
      
      ;; Create an initial log entry to confirm system is working
      (MBS:log-info "Debug system initialized")
      (prompt (strcat "\nDebug system initialized with level: " 
                     (cond
                       ((= MBS:debug-level 0) "OFF")
                       ((= MBS:debug-level 1) "ERROR")
                       ((= MBS:debug-level 2) "WARNING")
                       ((= MBS:debug-level 3) "INFO")
                       ((= MBS:debug-level 4) "VERBOSE")
                       (t "UNKNOWN")
                     )))
    )
    
    ;;; ========================================================================
    ;; DEBUG LEVEL MANAGEMENT
    ;;; ========================================================================
    
    ;; Fix for mixed string/numeric debug levels
    (defun MBS:normalize-debug-level (level)
      "Converts any debug level format to a consistent numeric value"
      (cond
        ;; If already a number, return it
        ((numberp level) level)
        
        ;; String representation of a number
        ((and (= (type level) 'STR) (vl-string-search "1" level)) 1)
        ((and (= (type level) 'STR) (vl-string-search "2" level)) 2)
        ((and (= (type level) 'STR) (vl-string-search "3" level)) 3)
        ((and (= (type level) 'STR) (vl-string-search "4" level)) 4)
        ((and (= (type level) 'STR) (vl-string-search "0" level)) 0)
        
        ;; Named levels as strings
        ((= level "VERBOSE") 4)
        ((= level "INFO") 3)
        ((= level "WARNING") 2)
        ((= level "ERROR") 1)
        ((= level "OFF") 0)
        
        ;; Default to INFO level
        (t 3)
      )
    )
    
    ;; Set debug level - will override existing level
    (defun MBS:set-debug-level (level)
      (setq MBS:debug-level (MBS:normalize-debug-level level))
      (prompt (strcat "\nDebug level set to " 
                      (cond
                        ((= MBS:debug-level 0) "OFF")
                        ((= MBS:debug-level 1) "ERROR")
                        ((= MBS:debug-level 2) "WARNING")
                        ((= MBS:debug-level 3) "INFO")
                        ((= MBS:debug-level 4) "VERBOSE")
                        (t "UNKNOWN")
                      )))
    )
    
    ;; Check if a message at the given level should be logged
    (defun MBS:should-log (level)
      (>= (MBS:normalize-debug-level MBS:debug-level) 
          (MBS:normalize-debug-level level))
    )
    
    ;; Command to set debug level
    (defun c:MBS-SetDebugLevel ()
      (initget "Off Error Warning Info Verbose")
      (setq level-choice (getkword "\nSelect debug level [Off/Error/Warning/Info/Verbose]: "))
      
      (cond
        ((= level-choice "Off") (setq MBS:debug-level 0))
        ((= level-choice "Error") (setq MBS:debug-level 1))
        ((= level-choice "Warning") (setq MBS:debug-level 2))
        ((= level-choice "Info") (setq MBS:debug-level 3))
        ((= level-choice "Verbose") (setq MBS:debug-level 4))
      )
      
      (prompt (strcat "\nDebug level set to " level-choice))
      (MBS:log-info (strcat "Debug level changed to " level-choice))
      
      ;; If we have settings persistence, save the change
      (if (boundp 'MBS:save-settings-to-drawing)
        (MBS:save-settings-to-drawing)
      )
      
      (princ)
    )
    
    ;;; ========================================================================
    ;; LOGGING FUNCTIONS 
    ;;; ========================================================================
    
    ;; Write log entry
    (defun MBS:write-log (prefix message / full-message)
      (setq full-message (strcat prefix message))
      
      (cond
        ;; Console output
        ((eq MBS:debug-output 'console)
          (prompt (strcat "\n" full-message))
        )
        
        ;; File output
        ((eq MBS:debug-output 'file)
          (MBS:log-to-file-safe full-message)
        )
        
        ;; Both console and file
        ((eq MBS:debug-output 'both)
          (prompt (strcat "\n" full-message))
          (MBS:log-to-file-safe full-message)
        )
      )
    )
    
    ;; More robust file logging with error handling
    (defun MBS:log-to-file-safe (message / f dir-path)
      (vl-catch-all-apply
        '(lambda ()
          ;; Ensure directory exists
          (setq dir-path (vl-filename-directory MBS:debug-log-file))
          (if (and dir-path (not (vl-file-directory-p dir-path)))
            (vl-mkdir dir-path)
          )
          
          ;; Try to open the file
          (setq f (open MBS:debug-log-file "a"))
          (if f
            (progn
              (write-line (strcat (MBS:timestamp) " " message) f)
              (close f)
              t
            )
            nil
          )
        )
      )
    )
    
    ;; Generate timestamp
    (defun MBS:timestamp (/ date time)
      (setq date (rtos (getvar "CDATE") 2 6))
      (strcat "[" date "]")
    )
    
    ;; Different log level functions
    (defun MBS:log-critical (message)
      (if (MBS:should-log "ERROR")
        (MBS:write-log "[CRITICAL] " message)
      )
    )
    
    (defun MBS:log-error (message)
      (if (MBS:should-log "ERROR")
        (MBS:write-log "[ERROR] " message)
      )
    )
    
    (defun MBS:log-warning (message)
      (if (MBS:should-log "WARNING")
        (MBS:write-log "[WARNING] " message)
      )
    )
    
    (defun MBS:log-info (message)
      (if (MBS:should-log "INFO")
        (MBS:write-log "[INFO] " message)
      )
    )
    
    (defun MBS:log-verbose (message)
      (if (MBS:should-log "VERBOSE")
        (MBS:write-log "[VERBOSE] " message)
      )
    )
    
    (defun MBS:log-data (label data / depth)
      (if (MBS:should-log "VERBOSE")
        (progn
          (MBS:write-log "[DATA] " (strcat label ": "))
          (MBS:log-lisp-object data 0)
        )
      )
    )
    
    ;; Helper for pretty-printing lisp objects with indentation
    (defun MBS:log-lisp-object (obj depth / indent i)
      (setq indent "")
      (setq i 0)
      (while (< i depth)
        (setq indent (strcat indent "  "))
        (setq i (1+ i))
      )
      
      (cond
        ;; Lists
        ((listp obj)
          (if (= (length obj) 0)
            (MBS:write-log indent "()")
            (progn
              (MBS:write-log indent "(")
              (foreach item obj
                (MBS:log-lisp-object item (1+ depth))
              )
              (MBS:write-log indent ")")
            )
          )
        )
        
        ;; Strings
        ((= (type obj) 'STR)
          (MBS:write-log indent (strcat "\"" obj "\"")))
        
        ;; Other types
        (t
          (MBS:write-log indent (vl-princ-to-string obj)))
      )
    )
    
    ;;; ========================================================================
    ;; LOG MANAGEMENT FUNCTIONS
    ;;; ========================================================================
    
    ;; Clear log file
    (defun MBS:clear-log ()
      (if (findfile MBS:debug-log-file)
        (vl-file-delete MBS:debug-log-file)
      )
      (setq f (open MBS:debug-log-file "w"))
      (if f
        (progn
          (write-line "=== Material Block Sync Log ===" f)
          (write-line (strcat "[" (MBS:timestamp) "] Log cleared") f)
          (close f)
        )
      )
      (prompt "\nLog file cleared")
    )
    
    ;; Get log content for display
    (defun MBS:get-log (/ log-content f line)
      (setq log-content "")
      
      (if (findfile MBS:debug-log-file)
        (progn
          (setq f (open MBS:debug-log-file "r"))
          (if f
            (progn
              (while (setq line (read-line f))
                (setq log-content (strcat log-content line "\n"))
              )
              (close f)
            )
          )
        )
        (setq log-content "Log file not found or empty.")
      )
      
      log-content
    )
    
    ;; Function to limit log file size
    (defun MBS:manage-log-file-size (max-size-kb / f-size)
      ;; Check if log file exists
      (if (findfile MBS:debug-log-file)
        (progn
          ;; Get file size in KB
          (setq f-size (/ (vl-file-size MBS:debug-log-file) 1024.0))
          
          ;; If file exceeds maximum size, archive and start new log
          (if (> f-size max-size-kb)
            (progn
              (MBS:log-warning (strcat "Log file exceeded size limit (" 
                                      (rtos f-size 2 1) "KB). Archiving."))
              (c:MBS-ArchiveLog)
            )
          )
        )
      )
    )
    
    ;; Debug menu commands
    (defun c:MBS-TestLogging ()
      (prompt "\nTesting logging at all levels...")
      
      ;; Save current debug level
      (setq current-level MBS:debug-level)
      
      ;; Temporarily set to VERBOSE to show all messages
      (setq MBS:debug-level 4)
      
      ;; Test all message types
      (MBS:log-critical "This is a CRITICAL test message")
      (MBS:log-warning "This is a WARNING test message")
      (MBS:log-info "This is an INFO test message")
      (MBS:log-verbose "This is a VERBOSE test message")
      
      ;; Also test data logging
      (MBS:log-data "Test data" '("item1" "item2" (cons "key" "value")))
      
      ;; Restore original level
      (setq MBS:debug-level current-level)
      
      ;; View the log file
      (c:MBS-ViewLog)
      (princ)
    )
    
    ;; Direct debug on/off commands
    (defun MBS:debug-on () (setq MBS:debug-level 4) (prompt "\nDebug logging enabled at verbose level"))
    (defun MBS:debug-info () (setq MBS:debug-level 3) (prompt "\nDebug logging set to info level"))
    (defun MBS:debug-warn () (setq MBS:debug-level 2) (prompt "\nDebug logging set to warning level"))
    (defun MBS:debug-off () (setq MBS:debug-level 0) (prompt "\nDebug logging disabled"))
    
    ;; Toggle log output mode
    (defun c:MBS-ToggleLogOutput ()
      (initget "Console File Both")
      (setq output-choice (getkword "\nSelect log output mode [Console/File/Both]: "))
      
      (cond
        ((= output-choice "Console") (setq MBS:debug-output 'console))
        ((= output-choice "File") (setq MBS:debug-output 'file))
        ((= output-choice "Both") (setq MBS:debug-output 'both))
      )
      
      (prompt (strcat "\nLog output set to " output-choice))
      (MBS:log-info (strcat "Log output changed to " output-choice))
      (princ)
    )
    
    ;; Command to view the log file in Notepad
    (defun c:MBS-OpenLogInNotepad ()
      (if (findfile MBS:debug-log-file)
        (progn
          (startapp "notepad.exe" MBS:debug-log-file)
          (prompt (strcat "\nOpened log file: " MBS:debug-log-file))
        )
        (prompt "\nNo log file found.")
      )
      (princ)
    )
    
    ;; Command to view log
    (defun c:MBS-ViewLog ()
      (if (new_dialog "matblock_log" (load_dialog (MBS:create-dcl-file-with-debug)))
        (progn
          (set_tile "log_text" (MBS:get-log))
          (action_tile "export" "(MBS:export-log-action)")
          (action_tile "close" "(done_dialog 1)")
          (start_dialog)
        )
      )
      (princ)
    )
    
    ;; Command to open debug panel
    (defun c:MBS-Debug ()
      (MBS:show-debug-dialog)
      (princ)
    )
    
    ;;; ========================================================================
    ;; DEBUG DIALOG FUNCTIONALITY
    ;;; ========================================================================
    
    ;; Create a temporary DCL file that includes debug dialog definitions
    (defun MBS:create-dcl-file-with-debug (/ main-dcl temp-path f)
      (setq temp-path (strcat (getenv "TEMP") "\\mbs_combined_" (itoa (getvar "MILLISECS")) ".dcl"))
      
      ;; Create new combined DCL file
      (setq f (open temp-path "w"))
      
      ;; Write debug dialog definitions
      (write-line "// Debug Dialog Definitions for Material Block Sync" f)
      (write-line "matblock_debug : dialog {" f)
      (write-line "  label = \"Material Block Sync Debug Panel\";" f)
      (write-line "  : boxed_column {" f)
      (write-line "    label = \"Debug Level\";" f)
      (write-line "    : radio_row {" f)
      (write-line "      : radio_button {" f)
      (write-line "        key = \"error\";" f)
      (write-line "        label = \"Errors Only\";" f)
      (write-line "        mnemonic = \"E\";" f)
      (write-line "      }" f)
      (write-line "      : radio_button {" f)
      (write-line "        key = \"warning\";" f)
      (write-line "        label = \"Warnings\";" f)
      (write-line "        mnemonic = \"W\";" f)
      (write-line "      }" f)
      (write-line "      : radio_button {" f)
      (write-line "        key = \"info\";" f)
      (write-line "        label = \"Info\";" f)
      (write-line "        mnemonic = \"I\";" f)
      (write-line "      }" f)
      (write-line "      : radio_button {" f)
      (write-line "        key = \"verbose\";" f)
      (write-line "        label = \"Verbose\";" f)
      (write-line "        mnemonic = \"V\";" f)
      (write-line "      }" f)
      (write-line "    }" f)
      
      (write-line "    spacer;" f)
      
      (write-line "    : text {" f)
      (write-line "      key = \"tx_count\";" f)
      (write-line "      label = \"Pending transactions: 0\";" f)
      (write-line "    }" f)
      
      (write-line "    : text {" f)
      (write-line "      key = \"last_action\";" f)
      (write-line "      label = \"Last action:\";" f)
      (write-line "    }" f)
      
      (write-line "    spacer;" f)
      
      (write-line "    : row {" f)
      (write-line "      : button {" f)
      (write-line "        key = \"view_log\";" f)
      (write-line "        label = \"View Log\";" f)
      (write-line "        mnemonic = \"L\";" f)
      (write-line "      }" f)
      (write-line "      : button {" f)
      (write-line "        key = \"clear_log\";" f)
      (write-line "        label = \"Clear Log\";" f)
      (write-line "        mnemonic = \"C\";" f)
      (write-line "      }" f)
      (write-line "      : button {" f)
      (write-line "        key = \"clear_tx\";" f)
      (write-line "        label = \"Clear Transactions\";" f)
      (write-line "        mnemonic = \"T\";" f)
      (write-line "      }" f)
      (write-line "      : button {" f)
      (write-line "        key = \"dump_tx\";" f)
      (write-line "        label = \"View Transactions\";" f)
      (write-line "        mnemonic = \"V\";" f)
      (write-line "      }" f)
      (write-line "    }" f)
      (write-line "  }" f)
      
      (write-line "  ok_cancel;" f)
      (write-line "}" f)

      ;; Add Log Viewer Dialog
      (write-line "" f)
      (write-line "matblock_log : dialog {" f)
      (write-line "  label = \"Material Block Sync Log Viewer\";" f)
      (write-line "  width = 60;" f)
      (write-line "  height = 20;" f)
      
      (write-line "  : edit_box {" f)
      (write-line "    key = \"log_text\";" f)
      (write-line "    width = 58;" f)
      (write-line "    height = 18;" f)
      (write-line "    multiple_lines = true;" f)
      (write-line "    read_only = true;" f)
      (write-line "    fixed_width_font = true;" f)
      (write-line "  }" f)
      
      (write-line "  : row {" f)
      (write-line "    fixed_width = true;" f)
      (write-line "    alignment = centered;" f)
        
      (write-line "    : button {" f)
      (write-line "      key = \"export\";" f)
      (write-line "      label = \"Export Log\";" f)
      (write-line "      mnemonic = \"E\";" f)
      (write-line "    }" f)
        
      (write-line "    : button {" f)
      (write-line "      key = \"close\";" f)
      (write-line "      label = \"Close\";" f)
      (write-line "      mnemonic = \"C\";" f)
      (write-line "      is_default = true;" f)
      (write-line "    }" f)
      (write-line "  }" f)
      (write-line "}" f)
      
      ;; Close file
      (close f)
      
      ;; Return the path to the combined DCL file
      temp-path
    )
    
    ;; Show debug dialog
    (defun MBS:show-debug-dialog ()
      ;; Use the main DCL file that already contains debug dialog definition
      (setq dcl-file (MBS:create-dcl-file-with-debug))
      
      (setq dcl-id (load_dialog dcl-file))
      (if (< dcl-id 0)
        (progn
          (alert "Could not load debug dialog definition.")
          nil
        )
        (progn
          (if (new_dialog "matblock_debug" dcl-id)
            (progn
              ;; Set current debug level
              (cond
                ((= MBS:debug-level 4) (set_tile "verbose" "1"))
                ((= MBS:debug-level 3) (set_tile "info" "1"))
                ((= MBS:debug-level 2) (set_tile "warning" "1"))
                ((= MBS:debug-level 1) (set_tile "error" "1"))
                (t (set_tile "info" "1"))  ;; Default to INFO
              )
              
              ;; Show transaction information
              (if (boundp 'MBS:transaction-queue)
                (set_tile "tx_count" (strcat "Pending transactions: " 
                                            (itoa (length MBS:transaction-queue))))
                (set_tile "tx_count" "Transaction system not loaded")
              )
              
              ;; Show last action if available
              (if (boundp 'MBS:last-action)
                (set_tile "last_action" (strcat "Last action: " MBS:last-action))
                (set_tile "last_action" "")
              )
              
              ;; Define actions - UPDATED to use Notepad for viewing logs
              (action_tile "view_log" "(MBS:view-log-in-notepad)")
              (action_tile "clear_log" "(MBS:clear-log-action)")
              (action_tile "clear_tx" "(MBS:clear-transactions-action)")
              (action_tile "dump_tx" "(MBS:dump-transactions-action)")
              (action_tile "accept" "(MBS:save-debug-settings) (done_dialog 1)")
              (action_tile "cancel" "(done_dialog 0)")
              
              ;; Display dialog
              (setq result (start_dialog))
              (unload_dialog dcl-id)
              
              ;; Process result
              (= result 1)
            )
            (progn
              (alert "Could not create debug dialog.")
              (unload_dialog dcl-id)
              nil
            )
          )
        )
      )
    )
    
    ;; Function to open log file in Notepad (used by view_log button)
    (defun MBS:view-log-in-notepad ()
      (if (findfile MBS:debug-log-file)
        (progn
          (prompt (strcat "\nOpening log file in Notepad: " MBS:debug-log-file))
          (startapp "notepad.exe" MBS:debug-log-file)
          t
        )
        (progn
          (prompt "\nLog file not found or has not been created yet.")
          (alert "Log file not found. Try generating some log entries first.")
          nil
        )
      )
    )
    
    ;; Clear log action
    (defun MBS:clear-log-action ()
      (MBS:clear-log)
      (alert "Log has been cleared")
    )
    
    ;; Clear transactions action
    (defun MBS:clear-transactions-action ()
      (if (boundp 'MBS:clear-transactions)
        (progn
          (MBS:clear-transactions)
          (set_tile "tx_count" "Pending transactions: 0")
          (alert "All pending transactions have been cleared")
        )
        (alert "Transaction system not loaded")
      )
    )
    
    ;; Dump transactions action
    (defun MBS:dump-transactions-action ()
      (if (boundp 'MBS:transaction-queue)
        (progn
          (setq tx-dump "")
          (setq tx-index 1)
          (foreach tx MBS:transaction-queue
            (setq tx-dump (strcat tx-dump 
                                 "\n" (itoa tx-index) ": " 
                                 (MBS:format-transaction tx)))
            (setq tx-index (1+ tx-index))
          )
          
          ;; If no transactions, say so
          (if (= tx-dump "")
            (setq tx-dump "No pending transactions")
          )
          
          (alert tx-dump)
        )
        (alert "Transaction system not loaded")
      )
    )
    
    ;; Format transaction for display
    (defun MBS:format-transaction (tx)
      (setq tx-type (car tx))
      (cond
        ((eq tx-type 'update-field)
         (strcat "Update field: " (nth 2 tx) " = " (vl-princ-to-string (nth 3 tx))))
        
        ((eq tx-type 'insert-row)
         "Insert row")
        
        ((eq tx-type 'delete-row)
         (strcat "Delete row " (itoa (nth 1 tx))))
        
        ((eq tx-type 'insert-block)
         (strcat "Insert block (row " (itoa (nth 1 tx)) ")"))
        
        (t (vl-princ-to-string tx))
      )
    )
    
    ;; Export log action
    (defun MBS:export-log-action ()
      (setq log-content (MBS:get-log))
      (setq file-path (getfiled "Export Log" "" "txt" 1))
      (if file-path
        (progn
          (setq f (open file-path "w"))
          (write-line "MATERIAL BLOCK SYNC LOG" f)
          (write-line "======================" f)
          (write-line (strcat "Exported: " (menucmd "m=$(edtime,$(getvar,date),YYYY-MM-DD HH:MM:SS)")) f)
          (write-line "" f)
          (write-line log-content f)
          (close f)
          (alert (strcat "Log exported to:\n" file-path))
        )
      )
    )
    
    ;; Save debug settings
    (defun MBS:save-debug-settings ()
      (cond
        ((= (get_tile "verbose") "1") (setq MBS:debug-level 4))
        ((= (get_tile "info") "1") (setq MBS:debug-level 3))
        ((= (get_tile "warning") "1") (setq MBS:debug-level 2))
        ((= (get_tile "error") "1") (setq MBS:debug-level 1))
      )
      
      (if (boundp 'MBS:save-settings-to-drawing)
        (MBS:save-settings-to-drawing)
        (alert "Settings saved in memory but could not save to drawing")
      )
    )
    
    ;; Function to archive the current log file
    (defun c:MBS-ArchiveLog ()
      (if (findfile MBS:debug-log-file)
        (progn
          ;; Create archive filename with timestamp
          (setq timestamp (menucmd "m=$(edtime,$(getvar,date),YYYYMMDD_HHMMSS)"))
          (setq archive-path (strcat (vl-filename-directory MBS:debug-log-file) 
                                    "\\mbs_log_" timestamp ".txt"))
          
          ;; Copy current log to archive
          (if (vl-file-copy MBS:debug-log-file archive-path)
            (progn
              (prompt (strcat "\nLog file archived to: " archive-path))
              
              ;; Ask if user wants to clear current log
              (initget "Yes No")
              (if (= (getkword "\nClear current log file? [Yes/No]: ") "Yes")
                (MBS:clear-log)
              )
              t
            )
            (progn
              (prompt "\nFailed to archive log file.")
              nil
            )
          )
        )
        (prompt "\nNo log file found to archive.")
      )
      (princ)
    )
    
    ;; Add this to your initialization
    (defun MBS:setup-log-management ()
      ;; Check log size once at startup
      (MBS:manage-log-file-size 2048)  ;; 2MB limit
      
      ;; Ensure the log file exists
      (if (not (findfile MBS:debug-log-file))
        (progn
          (setq f (open MBS:debug-log-file "w"))
          (if f
            (progn
              (write-line "=== Material Block Sync Log ===" f)
              (write-line (strcat (MBS:timestamp) " Log file created") f)
              (close f)
            )
          )
        )
      )
    )
    
    ;; Run initialization
    (MBS:initialize-debug-system)
    (MBS:setup-log-management)
    
  ) ;; End of progn
) ;; End of if

;; Display loading message
(prompt "\nMaterial Block Sync Debug module loaded.")
(princ)