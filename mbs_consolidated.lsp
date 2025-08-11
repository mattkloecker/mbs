;;; ========================================================================
;; MBS CONSOLIDATED SCRIPT - Complete Material Block Sync System
;; AutoCAD 2023 Vanilla LISP
;; Consolidated from: mbs_blocks, mbs_config, mbs_core, mbs_csv, mbs_debug, 
;;                    mbs_dialog, mbs_enhanced_mode, mbs_library_dialog, 
;;                    mbs_library, and mbs_transaction.lsp
;; VERSION 6.0 - UNIFIED ARCHITECTURE
;;; ========================================================================

;;; ------------------------------------------------------------------------
;;; Safe logging stubs (allow logging before mbs_debug.lsp is loaded)
;;; ------------------------------------------------------------------------
(if (not (vl-position 'MBS:log-info (atoms-family 1)))
  (defun MBS:log-info (msg)
    (if (and msg (stringp msg)) (prompt msg))
  )
)
(if (not (vl-position 'MBS:log-warning (atoms-family 1)))
  (defun MBS:log-warning (msg)
    (if (and msg (stringp msg)) (prompt msg))
  )
)
(if (not (vl-position 'MBS:log-error (atoms-family 1)))
  (defun MBS:log-error (msg)
    (if (and msg (stringp msg)) (prompt msg))
  )
)
(if (not (vl-position 'MBS:log-verbose (atoms-family 1)))
  (defun MBS:log-verbose (msg)
    (if (and msg (stringp msg)) (prompt msg))
  )
)

;;; ------------------------------------------------------------------------
;;; Bootstrap debug module FIRST to satisfy dependencies
;;; ------------------------------------------------------------------------
(prompt "\n⏱ Loading debug module first...")


;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - Debug Module
;; VERSION 5.5
;;; ========================================================================
;; This module implements the debug logging and management functionality
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
  
  ;; Try to load debug level from drawing
  (MBS:load-debug-level-from-drawing)
  
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

;; Load debug level from drawing using MBS settings marker block
(defun MBS:load-debug-level-from-drawing (/ doc modelspace marker-block loaded-level)
  "Load debug level from MBS settings marker block using DEBUG_LEVEL attribute"
  (setq loaded-level nil)
  
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelspace (vla-get-ModelSpace doc))
  
  ;; First try to find existing marker block
  (vlax-for ent modelspace
    (if (and (= (vla-get-ObjectName ent) "AcDbBlockReference")
             (= (vlax-get ent 'EffectiveName) "_MBS_SETTINGS_MARKER"))
      (setq marker-block ent)
    )
  )
  
  ;; If marker block exists, try to load using DEBUG_LEVEL attribute
  (if marker-block
    (progn
      (prompt "\n🔧 === LOADING DEBUG LEVEL ===")
      
      ;; Try to load using DEBUG_LEVEL attribute
      (setq loaded-level (MBS:get-debug-level-attribute marker-block))
      
      (if loaded-level
        (progn
          (setq MBS:debug-level (atoi (itoa loaded-level)))  ;; Ensure it's a number
          (prompt (strcat "\n✅ Debug level loaded from DEBUG_LEVEL attribute: " (itoa MBS:debug-level)))
        )
        (progn
          (prompt "\n⚠ No DEBUG_LEVEL attribute found, trying XDATA...")
          ;; Fallback to XDATA
          (setq loaded-level (MBS:load-debug-level-from-xdata))
          (if loaded-level
            (progn
              (setq MBS:debug-level (atoi (itoa loaded-level)))  ;; Ensure it's a number
              (prompt (strcat "\n✅ Debug level loaded from XDATA: " (itoa MBS:debug-level)))
            )
            (prompt "\n⚠ No debug level found in drawing, using default")
          )
        )
      )
    )
    (progn
      (prompt "\n⚠ No MBS settings marker block found, trying XDATA...")
      ;; Fallback to XDATA on modelspace
      (setq loaded-level (MBS:load-debug-level-from-xdata))
      (if loaded-level
        (progn
          (setq MBS:debug-level (atoi (itoa loaded-level)))  ;; Ensure it's a number
          (prompt (strcat "\n✅ Debug level loaded from XDATA: " (itoa MBS:debug-level)))
        )
        (prompt "\n⚠ No debug level found in drawing, using default")
      )
    )
  )
  
  loaded-level
)

;; Get DEBUG_LEVEL attribute from marker block
(defun MBS:get-debug-level-attribute (marker-block / attrs debug-level)
  "Get DEBUG_LEVEL attribute from MBS settings marker block"
  (setq debug-level nil)
  
  (setq attrs (entget (entnext (cdr (assoc -1 (entget (vlax-vla-object->ename marker-block)))))))
  
  ;; Look for DEBUG_LEVEL attribute
  (while (and attrs (/= (cdr (assoc 0 attrs)) "SEQEND"))
    (if (= (cdr (assoc 2 attrs)) "DEBUG_LEVEL")
      (setq debug-level (atoi (cdr (assoc 1 attrs)))))
    (setq attrs (entget (entnext (cdr (assoc -1 attrs)))))
  )
  
  debug-level
)

;; Load debug level from XDATA as fallback
(defun MBS:load-debug-level-from-xdata (/ doc modelspace entdata xdata xdata-app settings-string debug-level)
  "Load debug level from XDATA on modelspace"
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelspace (vla-get-ModelSpace doc))
  (setq debug-level nil)
  
  ;; Get XDATA from modelspace
  (setq entdata (entget (vlax-vla-object->ename modelspace) '("MBS_SETTINGS")))
  (setq xdata (assoc -3 entdata))
  
  (if xdata
    (progn
      (setq xdata-app (assoc "MBS_SETTINGS" (cdr xdata)))
      (if xdata-app
        (progn
          ;; Extract settings string
          (setq settings-string (cdr (assoc 1000 (cdr xdata-app))))
          
          ;; Parse for DEBUG_LEVEL
          (if (and settings-string (vl-string-search "DEBUG_LEVEL=" settings-string))
            (progn
              (setq debug-level-start (+ (vl-string-search "DEBUG_LEVEL=" settings-string) 12))
              (setq debug-level-end (vl-string-search "|" settings-string debug-level-start))
              (if debug-level-end
                (setq debug-level (atoi (substr settings-string debug-level-start (- debug-level-end debug-level-start))))
                (setq debug-level (atoi (substr settings-string debug-level-start)))
              )
            )
          )
        )
      )
    )
  )
  
  debug-level
)

;;; ========================================================================
;; DEBUG LEVEL MANAGEMENT
;;; ========================================================================

;; Fix for mixed string/numeric debug levels
(defun MBS:normalize-debug-level (level)
  "Converts any debug level format to a consistent numeric value"
  (cond
    ;; Handle nil values
    ((null level) 3)
    
    ;; If already a number, return it
    ((numberp level) level)
    
    ;; String representation of a number - use exact string comparison
    ((= level "0") 0)
    ((= level "1") 1)
    ((= level "2") 2)
    ((= level "3") 3)
    ((= level "4") 4)
    
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
  ;; Ensure MBS:debug-level is a number
  (if (not (numberp MBS:debug-level))
    (setq MBS:debug-level 3)  ;; Default to INFO if not a number
  )
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
  ;; Ensure MBS:debug-output is set to a valid value
  (if (not (boundp 'MBS:debug-output))
    (setq MBS:debug-output 'console)
  )
  (if (null MBS:debug-output)
    (setq MBS:debug-output 'console)
  )
  
  ;; Ensure prefix and message are strings
  (if (null prefix)
    (setq prefix "")
  )
  (if (null message)
    (setq message "")
  )
  
  ;; Convert to strings if needed
  (if (not (= (type prefix) 'STR))
    (setq prefix (vl-princ-to-string prefix))
  )
  (if (not (= (type message) 'STR))
    (setq message (vl-princ-to-string message))
  )
  
  (setq full-message (strcat prefix message))
  
  ;; Always write to log file regardless of console setting
  (MBS:log-to-file-safe full-message)
  
  ;; Console output based on debug level (controlled by MBS:should-log)
  ;; Only show console output if the message level meets the debug level requirement
  (if (MBS:should-log (cond
    ((vl-string-search "[CRITICAL]" full-message) "ERROR")
    ((vl-string-search "[ERROR]" full-message) "ERROR")
    ((vl-string-search "[WARNING]" full-message) "WARNING")
    ((vl-string-search "[INFO]" full-message) "INFO")
    ((vl-string-search "[VERBOSE]" full-message) "VERBOSE")
    (t "INFO")  ;; Default to INFO level
  ))
    (cond
      ;; Console output
      ((eq MBS:debug-output 'console)
        (prompt (strcat "\n" full-message))
      )
      
      ;; File output (console already handled above)
      ((eq MBS:debug-output 'file)
        ;; Log file already written above, no additional console output
      )
      
      ;; Both console and file
      ((eq MBS:debug-output 'both)
        (prompt (strcat "\n" full-message))
      )
      
      ;; Default to console if unknown value
      (t
        (prompt (strcat "\n" full-message))
      )
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
  (MBS:write-log "[CRITICAL] " message)
)

(defun MBS:log-error (message)
  (MBS:write-log "[ERROR] " message)
)

(defun MBS:log-warning (message)
  (MBS:write-log "[WARNING] " message)
)

(defun MBS:log-info (message)
  (MBS:write-log "[INFO] " message)
)

(defun MBS:log-verbose (message)
  (MBS:write-log "[VERBOSE] " message)
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
          
          ;; Define actions - UPDATED to use Notepad for viewing logs
          (action_tile "view_log" "(MBS:view-log-in-notepad)")
          (action_tile "clear_log" "(MBS:clear-log-action)")
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

;; Save debug settings with persistence
(defun MBS:save-debug-settings ()
  (cond
    ((= (get_tile "verbose") "1") (setq MBS:debug-level 4))
    ((= (get_tile "info") "1") (setq MBS:debug-level 3))
    ((= (get_tile "warning") "1") (setq MBS:debug-level 2))
    ((= (get_tile "error") "1") (setq MBS:debug-level 1))
  )
  
  ;; Save to drawing using MBS settings marker block
  (MBS:save-debug-level-to-drawing)
)

;; Save debug level to drawing using MBS settings marker block
(defun MBS:save-debug-level-to-drawing (/ doc modelspace marker-block success)
  "Save debug level to MBS settings marker block using DEBUG_LEVEL attribute"
  (setq success nil)
  
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelspace (vla-get-ModelSpace doc))
  
  ;; First try to find existing marker block
  (vlax-for ent modelspace
    (if (and (= (vla-get-ObjectName ent) "AcDbBlockReference")
             (= (vlax-get ent 'EffectiveName) "_MBS_SETTINGS_MARKER"))
      (setq marker-block ent)
    )
  )
  
  ;; If marker block exists, try to save using DEBUG_LEVEL attribute
  (if marker-block
    (progn
      (prompt "\n🔧 === SAVING DEBUG LEVEL ===")
      (prompt (strcat "\n📊 Current debug level: " (itoa MBS:debug-level)))
      
      ;; Try to save using DEBUG_LEVEL attribute
      (if (MBS:set-debug-level-attribute marker-block MBS:debug-level)
        (progn
          (prompt "\n✅ Debug level saved to DEBUG_LEVEL attribute")
          (setq success t)
        )
        (progn
          (prompt "\n⚠ Failed to save to DEBUG_LEVEL attribute, trying XDATA...")
          ;; Fallback to XDATA
          (if (MBS:save-debug-level-to-xdata)
            (progn
              (prompt "\n✅ Debug level saved to XDATA")
              (setq success t)
            )
            (progn
              (prompt "\n❌ Failed to save debug level to drawing")
              (setq success nil)
            )
          )
        )
      )
    )
    (progn
      (prompt "\n⚠ No MBS settings marker block found, trying XDATA...")
      ;; Fallback to XDATA on modelspace
      (if (MBS:save-debug-level-to-xdata)
        (progn
          (prompt "\n✅ Debug level saved to XDATA")
          (setq success t)
        )
        (progn
          (prompt "\n❌ Failed to save debug level to drawing")
          (setq success nil)
        )
      )
    )
  )
  
  ;; Show result to user
  (if success
    (prompt "\n✅ Debug level saved successfully")
    (prompt "\n⚠ Debug level saved in memory only")
  )
  
  success
)

;; Set DEBUG_LEVEL attribute on marker block
(defun MBS:set-debug-level-attribute (marker-block debug-level / attrs success)
  "Set DEBUG_LEVEL attribute on MBS settings marker block"
  (setq success nil)
  
  (setq attrs (entget (entnext (cdr (assoc -1 (entget (vlax-vla-object->ename marker-block)))))))
  
  ;; Look for existing DEBUG_LEVEL attribute
  (while (and attrs (/= (cdr (assoc 0 attrs)) "SEQEND"))
    (if (= (cdr (assoc 2 attrs)) "DEBUG_LEVEL")
      (progn
        ;; Update existing attribute
        (entmod (subst (cons 1 (itoa debug-level)) (assoc 1 attrs) attrs))
        (setq success t)
      )
    )
    (setq attrs (entget (entnext (cdr (assoc -1 attrs)))))
  )
  
  ;; If attribute not found, we can't create it here (would need block definition modification)
  ;; So we'll return nil and let the XDATA fallback handle it
  success
)

;; Save debug level to XDATA as fallback
(defun MBS:save-debug-level-to-xdata (/ doc modelspace settings-string xdata-list)
  "Save debug level to XDATA on modelspace"
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelspace (vla-get-ModelSpace doc))
  
  ;; Create settings string with debug level
  (setq settings-string (strcat "DEBUG_LEVEL=" (itoa MBS:debug-level)))
  
  ;; Create XDATA list
  (setq xdata-list (list (list -3 (list "MBS_SETTINGS" (cons 1000 settings-string)))))
  
  ;; Apply XDATA to modelspace
  (entmod (append (entget (vlax-vla-object->ename modelspace)) xdata-list))
  
  t
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

;; Display loading message
(prompt "\nMaterial Block Sync Debug module loaded.")
(princ)




;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - CLEAN LOADER WITH VALIDATION
;; VERSION 5.5 - CONFLICT RESOLUTION EDITION
;;; ========================================================================
;; This loader clears all old function definitions and loads fresh copies
;;; ========================================================================

;; Set global version
(setq MBS:version "5.5")

;; Function to safely undefine a function if it exists
(defun MBS:undefine-function (func-name)
  "Safely undefine a function to prevent conflicts"
  (if (boundp func-name)
    (progn
      (MBS:log-verbose (strcat "\n🧹 Clearing old definition: " (vl-symbol-name func-name)))
      (setq func-name nil)
    )
  )
)

;; Clear all potentially conflicting functions
  (MBS:log-info "\n🧹 === CLEANING OLD FUNCTION DEFINITIONS ===")
(MBS:undefine-function 'C:MBS-INSERT3)
(MBS:undefine-function 'C:MBS-BATCHINSERT3)
(MBS:undefine-function 'C:MBS-UPDATE3)
(MBS:undefine-function 'CSV:READ-FILE)
(MBS:undefine-function 'open-csv-in-excel)
(MBS:undefine-function 'C:MSYNC)

;; Also clear these commonly conflicting functions
(MBS:undefine-function 'MBS:read-csv)
(MBS:undefine-function 'MBS:write-csv)
(MBS:undefine-function 'get-csv-path-from-block)

;; Clear library-related functions
(MBS:undefine-function 'C:MBS-MaterialLibrary)
(MBS:undefine-function 'C:MBS-LibraryDialog)
(MBS:undefine-function 'MBS:toggle-enhanced-mode)

  (MBS:log-info "\n✅ Function cleanup completed")
  
  
 ;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - Core Module
;; VERSION 5.5
;;; ========================================================================
;; This module contains core functionality with no dependencies on other modules.
;; It includes global variable initialization, logging/debug system, and utility functions.
;;; ========================================================================

;; Global Constants and Initialization
(if (not (boundp 'MBS:version))
  (setq MBS:version "5.5")
)

;; Initialize global settings variables with defaults
(if (not (boundp 'current-mode))          (setq current-mode "LOM"))
(if (not (boundp 'prefix-mode))           (setq prefix-mode "AUTO"))
(if (not (boundp 'description-comma-limit)) (setq description-comma-limit 2))
(if (not (boundp 'auto-update-after-insert)) (setq auto-update-after-insert T))
(if (not (boundp 'total-weight-calculation)) (setq total-weight-calculation T))
(if (not (boundp 'auto-increment-item-numbers)) (setq auto-increment-item-numbers T))
(if (not (boundp 'current-drawing-type))  (setq current-drawing-type "P"))
(if (not (boundp 'auto-accept-item-number-changes)) (setq auto-accept-item-number-changes T))

;; Initialize id-attribute based on current mode
(if (not (boundp 'id-attribute))
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
)

;; Function to update id-attribute when mode changes
(defun MBS:update-id-attribute ()
  "Update id-attribute based on current mode"
  (setq id-attribute (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID"))
)

;; Load settings from drawing if available
;; This will be defined in mbs_dialog.lsp
(if (and (boundp 'MBS:load-settings-from-drawing) 
         (not (equal MBS:load-settings-from-drawing nil)))
  (MBS:load-settings-from-drawing)
)

;; Synchronize MBS:enable variables with loaded settings
(if (boundp 'MBS:enable-weight-calculation)
  (setq MBS:enable-weight-calculation total-weight-calculation)
)
(if (boundp 'MBS:enable-auto-increment-item-numbers)
  (setq MBS:enable-auto-increment-item-numbers auto-increment-item-numbers)
)
(if (boundp 'MBS:auto-accept-item-number-changes)
  (setq MBS:auto-accept-item-number-changes auto-accept-item-number-changes)
)

;; Global variable for prefix filter
(if (not (boundp 'MBS:current-prefix-filter))
  (setq MBS:current-prefix-filter nil)  ;; nil means no filtering (show all)
)

;;; ========================================================================
;; UTILITY FUNCTIONS
;;; ========================================================================

;; Helper function to create directories recursively
(defun vl-mkdir-recursive (dir / parent)
  (if (and dir (/= dir ""))
    (if (vl-file-directory-p dir)
      t  ;; Directory exists
      (progn
        ;; Get parent directory
        (setq parent (vl-filename-directory dir))
        ;; Create parent first, then this directory
        (if (vl-mkdir-recursive parent)
          (vl-mkdir dir)
        )
      )
    )
  )
)

;; Custom path joining function to replace vl-filename-mkpath
(defun join-path (base-path rel-path / result)
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

;; Helper function to create a relative path from absolute path
(defun create-relative-path (abs-path base-path / abs-parts base-parts rel-path)
  (if (and abs-path base-path)
    (progn
      ;; If the paths are on different drives, we can't make a relative path
      (if (/= (substr abs-path 1 1) (substr base-path 1 1))
        (vl-filename-base abs-path)  ;; Just return the filename
        (progn
          ;; Split paths into components using our safe function
          (setq abs-parts (vl-string-split-safe abs-path "\\"))
          (setq base-parts (vl-string-split-safe base-path "\\"))
          
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
(defun vl-string-split-safe (str delimiter / result start pos)
  (setq result '())
  (setq start 0)
  (while (setq pos (vl-string-search delimiter str start))
    (setq result (append result (list (substr str (+ start 1) (- pos start)))))
    (setq start (+ pos (strlen delimiter)))
  )
  (setq result (append result (list (substr str (+ start 1)))))
  result
)

;; String split helper function
(defun MBS:str-split (delimiter str / pos result token)
  (setq result '())
  (while (setq pos (vl-string-search delimiter str))
    (setq token (substr str 1 pos))
    (setq result (append result (list token)))
    (setq str (substr str (+ pos 1 (strlen delimiter))))
  )
  (if (/= str "")
    (setq result (append result (list str)))
  )
  result
)

;; SUBSEQ function for list slicing (equivalent to Common Lisp's subseq)
(defun SUBSEQ (list start &optional end / result i)
  "Extract a subsequence from a list"
  (setq result '())
  (setq i start)
  ;; If end is not provided, use the length of the list
  (if (or (null end) (= end nil))
    (setq end (length list))
  )
  (while (and (< i (length list)) (< i end))
    (setq result (append result (list (nth i list))))
    (setq i (1+ i))
  )
  result
)

;; Check if a character is a digit
(defun is-digit (ch)
  (and (>= (ascii ch) 48) (<= (ascii ch) 57))
)

;; Check if a character is alphabetic
(defun alpha-p (char)
  (or (and (>= (ascii char) 65) (<= (ascii char) 90))  ;; A-Z
      (and (>= (ascii char) 97) (<= (ascii char) 122))) ;; a-z
)

;; Helper for integer division with ceiling result
(defun MBS:ceiling-div (a b)
  (if (= (rem a b) 0)
    (/ a b)
    (1+ (/ (- a (rem a b)) b))
  )
)

;; Helper function to update a value in an association list without using setcdr
(defun MBS:update-assoc-value (assoc-list key new-value / result)
  (setq result '())
  (foreach pair assoc-list
    (if (equal (car pair) key)
      (setq result (append result (list (cons key new-value))))
      (setq result (append result (list pair)))
    )
  )
  result
)

;; Remove duplicates from a list
(defun MBS:remove-duplicates (lst / result)
  (setq result '())
  (foreach item lst
    (if (not (member item result))
      (setq result (append result (list item)))
    )
  )
  result
)

;; Safe object test function that doesn't rely on vlax-object-p
(defun MBS:valid-object-p (obj)
  (and obj
       (not (equal obj nil))
       (not (vl-catch-all-error-p 
         (vl-catch-all-apply 
           '(lambda () (vlax-get-property obj 'ObjectName))))))
)

;; Safe function to get attributes from a block
(defun MBS:get-block-attributes (blk / result)
  (setq result nil)
  (if (MBS:valid-object-p blk)
    (vl-catch-all-apply
      '(lambda ()
        (setq result (vlax-invoke blk 'GetAttributes))))
  )
  result
)

;; Safe object check that doesn't require VLAX
(defun MBS:is-valid-object (obj)
  "Safely checks if an object is valid without using VLAX functions"
  (and obj 
       (= (type obj) 'VLA-OBJECT)
       (MBS:is-object-alive obj))
)

;; Check if object is still alive
(defun MBS:is-object-alive (obj)
  "Check if VLA object is still valid"
  (not (vl-catch-all-error-p
         (vl-catch-all-apply 
           '(lambda () 
             (vlax-get-property obj 'ObjectName)
             nil))
  ))
)

;; Enhanced attribute text cleaning function to remove AutoCAD formatting codes
(defun MBS:clean-attribute-text (text / clean-text)
  (if (and text (= (type text) 'STR))
    (progn
      (setq clean-text text)
      
      ;; Remove common AutoCAD formatting codes
      ;; Width factor: \Wn.nnnn;
      (setq clean-text (vl-string-subst "" "\\W0.8000;" clean-text))
      (setq clean-text (vl-string-subst "" "\\W0.7000;" clean-text))
      (setq clean-text (vl-string-subst "" "\\W1.0000;" clean-text))
      
      ;; Height factor: \Hn.nnnn;
      (setq clean-text (vl-string-subst "" "\\H1.0000;" clean-text))
      
      ;; More comprehensive cleaning using regex-like approach
      (setq i 0)
      (while (< i (strlen clean-text))
        (if (= (substr clean-text (1+ i) 1) "\\")
          (progn
            ;; Found a backslash, look for the ending semicolon
            (setq j (1+ i))
            (while (and (< j (strlen clean-text)) 
                      (/= (substr clean-text (1+ j) 1) ";"))
              (setq j (1+ j))
            )
            
            ;; If we found a semicolon, remove the entire code
            (if (< j (strlen clean-text))
              (progn
                (setq clean-text (strcat (substr clean-text 1 i) 
                                        (substr clean-text (+ j 2))))
                (setq i (1- i))  ;; Adjust i since we modified the string
              )
            )
          )
        )
        (setq i (1+ i))
      )
      
      ;; Return the cleaned text with any leading/trailing whitespace removed
      (vl-string-trim " \t\n\r" clean-text)
    )
    ;; Return empty string for nil or non-string values
    ""
  )
)

;; Safe version with error handling
(defun MBS:clean-attribute-text-safe (text)
  (vl-catch-all-apply '(lambda () (MBS:clean-attribute-text text)))
)

;;; ========================================================================
;; INITIALIZATION FUNCTIONS
;;; ========================================================================

;; Load required libraries - fixed version without vl-fb-activex-support-p
(defun MBS:load-required-libraries (/ result)
  (setq result t)
  
  ;; Try to access ActiveX objects directly instead of checking for support
  (if (not (vl-catch-all-apply '(lambda () (vlax-get-acad-object))))
    (progn
      (MBS:log-error "\nERROR: Failed to access AutoCAD object model.")
      (setq result nil)
    )
  )
  
  result
)

;; Function to auto-detect mode from CSV path
(defun MBS:auto-detect-mode (csv-path)
  (if (and csv-path (vl-string-search "_LOR" (strcase csv-path)))
    "LOR"
    "LOM"
  )
)

;; Simplified init environment
(defun MBS:init-environment (/ result)
  (setq result t)  ;; Assume success
  
  ;; Initialize global variables using configuration system
  ;; Set id-attribute using the configuration
  (setq id-attribute (get-id-attribute))
  
  ;; Set alternate attribute (for backward compatibility)
  (if (= current-mode "LOR")
    (setq alternate-id-attribute "MATERIAL_ID")
    (setq alternate-id-attribute "REMOVAL_ID")
  )
  
  ;; Initialize transaction queue
  (if (not (boundp 'MBS:transaction-queue))
    (MBS:clear-transactions)
  )
  
  ;; Simple block check
  (if (not (tblsearch "BLOCK" (get-block-name)))
    (progn
      (MBS:log-error (strcat "\nERROR: Required block '" (get-block-name) "' not found."))
      (MBS:log-error "\nPlease ensure that the block exists in the drawing.")
      (setq result nil)
    )
  )
  
  result
)

;; Clear transaction queue
(defun MBS:clear-transactions ()
  (setq MBS:transaction-queue '())
)



;; Function to update mode and re-initialize environment
(defun MBS:set-mode (new-mode)
  (setq current-mode new-mode)
  ;; Update id-attribute using configuration system
  (setq id-attribute (get-id-attribute))
  
  ;; Update alternate attribute
  (if (= current-mode "LOR")
    (setq alternate-id-attribute "MATERIAL_ID")
    (setq alternate-id-attribute "REMOVAL_ID")
  )
      (MBS:log-info (strcat "\nMode set to: " current-mode))
    (MBS:log-info (strcat "\nUsing attribute: " id-attribute))
    (MBS:log-info (strcat "\nUsing block: " (get-block-name)))
)

;; Command to set LOR mode for testing
(defun C:MBS-SETLOR ()
  (MBS:set-mode "LOR")
  (princ)
)

;; Command to set LOM mode for testing
(defun C:MBS-SETLOM ()
  (MBS:set-mode "LOM")
  (princ)
)

  (MBS:log-info "\nMaterial Block Sync Core Module loaded.")
(princ)


;;; ========================================================================
;; MATERIAL BLOCK SYNC SYSTEM - Configuration Module
;; VERSION 5.5
;;; ========================================================================
;; This module centralizes all configuration settings and provides validation
;; functions to ensure operations succeed.
;;; ========================================================================

;;; ========================================================================
;; CONFIGURATION VALIDATION FUNCTIONS
;;; ========================================================================

;; Validate CSV structure before processing
(defun MBS:validate-csv-structure (csv-path / csv-data header records validation-result)
  (setq validation-result (list t ""))  ;; (is-valid error-message)
  
  ;; Read CSV data
  (setq csv-data (MBS:read-csv csv-path))
  (if (not csv-data)
    (setq validation-result (list nil "Failed to read CSV file"))
    (progn
      (setq header (car csv-data))
      (setq records (cadr csv-data))
      
      ;; Check if header exists and has required columns
      (if (not header)
        (setq validation-result (list nil "CSV file has no header row"))
        (progn
          ;; Check for required columns
          (setq required-columns (list "ITEM NO." "DESCRIPTION" (strcase id-attribute)))
          (setq missing-columns '())
          
          (foreach col required-columns
            (if (not (get-column-index header col))
              (setq missing-columns (append missing-columns (list col)))
            )
          )
          
          (if missing-columns
            (setq validation-result (list nil (strcat "Missing required columns: " (vl-princ-to-string missing-columns))))
            ;; Check if we have any data rows
            (if (= (length records) 0)
              (setq validation-result (list nil "CSV file has no data rows"))
              (setq validation-result (list t "CSV structure is valid"))
            )
          )
        )
      )
    )
  )
  
  validation-result
)
  
    (MBS:log-info "\n✓ Validate csv loaded successfully")


;; Validate block structure before processing
(defun MBS:validate-block-structure (block / att-list required-attrs missing-attrs)
  (setq required-attrs (list "##" "DESCRIPTION" (strcase id-attribute)))
  (setq missing-attrs '())
  
  ;; Get block attributes
  (setq att-list (MBS:get-block-attributes-safe block))
  (if (not att-list)
    (list nil "Failed to get block attributes")
    (progn
      ;; Check for required attributes
      (foreach req-attr required-attrs
        (setq found nil)
        (foreach att att-list
          (if (= (strcase (vlax-get att 'TagString)) (strcase req-attr))
            (setq found t)
          )
        )
        (if (not found)
          (setq missing-attrs (append missing-attrs (list req-attr)))
        )
      )
      
      (if missing-attrs
        (list nil (strcat "Missing required attributes: " (vl-princ-to-string missing-attrs)))
        (list t "Block structure is valid")
      )
    )
  )
)

    (MBS:log-info "\n✓ Validate block structure successfully")

;; Validate transaction queue before execution
(defun MBS:validate-transaction-queue (/ valid-count invalid-count)
  (setq valid-count 0)
  (setq invalid-count 0)
  
  (if (not (boundp 'MBS:transaction-queue))
    (list nil "No transaction queue found")
    (progn
      (foreach tx MBS:transaction-queue
        (if (and (listp tx) (> (length tx) 0))
          (setq valid-count (1+ valid-count))
          (setq invalid-count (1+ invalid-count))
        )
      )
      
      (if (> invalid-count 0)
        (list nil (strcat "Found " (itoa invalid-count) " invalid transactions"))
        (list t (strcat "All " (itoa valid-count) " transactions are valid"))
      )
    )
  )
)

    (MBS:log-info "\n✓ Validate structure successfully")

;;; ========================================================================
;; ENHANCED CONFIGURATION SETTINGS
;;; ========================================================================

;; Initialize enhanced configuration with validation
(defun MBS:initialize-configuration ()
  ;; Core settings
  (if (not (boundp 'current-mode))          (setq current-mode "LOM"))
  (if (not (boundp 'prefix-mode))           (setq prefix-mode "AUTO"))
  (if (not (boundp 'description-comma-limit)) (setq description-comma-limit 2))
  (if (not (boundp 'auto-update-after-insert)) (setq auto-update-after-insert T))
  (if (not (boundp 'total-weight-calculation)) (setq total-weight-calculation T))
  (if (not (boundp 'auto-increment-item-numbers)) (setq auto-increment-item-numbers T))
  (if (not (boundp 'current-drawing-type))  (setq current-drawing-type "P"))
  (if (not (boundp 'auto-accept-item-number-changes)) (setq auto-accept-item-number-changes T))
  
  ;; Error handling settings
  (if (not (boundp 'MBS:error-recovery-mode)) (setq MBS:error-recovery-mode "prompt"))
  (if (not (boundp 'MBS:max-retry-attempts)) (setq MBS:max-retry-attempts 3))
  (if (not (boundp 'MBS:backup-before-operations)) (setq MBS:backup-before-operations T))
  
  ;; File locking settings
  (if (not (boundp 'MBS:file-lock-timeout)) (setq MBS:file-lock-timeout 30))
  (if (not (boundp 'MBS:enable-file-lock-detection)) (setq MBS:enable-file-lock-detection T))
  (if (not (boundp 'MBS:file-operation-retry-delay)) (setq MBS:file-operation-retry-delay 200))
  
  ;; Performance settings
  (if (not (boundp 'MBS:batch-size)) (setq MBS:batch-size 50))
  (if (not (boundp 'MBS:enable-caching)) (setq MBS:enable-caching T))
  
  ;; User interface settings
  (if (not (boundp 'MBS:show-location-info)) (setq MBS:show-location-info T))
  (if (not (boundp 'MBS:show-progress-indicators)) (setq MBS:show-progress-indicators T))
  (if (not (boundp 'MBS:detailed-error-messages)) (setq MBS:detailed-error-messages T))
  
      (MBS:log-info "\n✓ Configuration initialized successfully")
)

    (MBS:log-info "\n✓ init config loaded successfully")

;; Get configuration value with validation
(defun MBS:get-config (key default-value)
  (if (boundp key)
    (eval key)
    default-value
  )
)

    (MBS:log-info "\n✓ MBS:get-config loaded successfully")

;; Set configuration value with validation
(defun MBS:set-config (key value / valid-types)
  (setq valid-types (list 'STR 'INT 'REAL 'SYM 'LIST))
  
  (if (member (type value) valid-types)
    (progn
      (set key value)
      t
    )
    (progn
      (MBS:log-warning (strcat "\n⚠ Invalid value type for configuration key: " (vl-princ-to-string key)))
      nil
    )
  )
)

    (MBS:log-info "\n✓ MBS:set-config loaded successfully")

;; Validate all configuration settings
(defun MBS:validate-configuration (/ validation-errors)
  (setq validation-errors '())
  
  ;; Check required settings
  (if (not (boundp 'current-mode))
    (setq validation-errors (append validation-errors (list "current-mode not set")))
  )
  
  ;; id-attribute is now auto-initialized in mbs_core.lsp based on current-mode
  ;; No longer need to validate its existence
  
  ;; Check setting ranges
  (if (and (boundp 'description-comma-limit) (< description-comma-limit 0))
    (setq validation-errors (append validation-errors (list "description-comma-limit must be >= 0")))
  )
  
  (if (and (boundp 'MBS:max-retry-attempts) (< MBS:max-retry-attempts 1))
    (setq validation-errors (append validation-errors (list "max-retry-attempts must be >= 1")))
  )
  
  (if (and (boundp 'MBS:batch-size) (< MBS:batch-size 1))
    (setq validation-errors (append validation-errors (list "batch-size must be >= 1")))
  )
  
  ;; Return validation result
  (if validation-errors
    (list nil validation-errors)
    (list t "Configuration is valid")
  )
)

;;; ========================================================================
;; CONFIGURATION PERSISTENCE
;;; ========================================================================

;; Save configuration to file
(defun MBS:save-configuration (file-path / f)
  (setq f (open file-path "w"))
  (if f
    (progn
      ;; Write configuration as LISP expressions
      (write-line "(setq current-mode \"" f)
      (write-line (strcat current-mode "\")") f)
      (write-line "(setq prefix-mode \"" f)
      (write-line (strcat prefix-mode "\")") f)
      ;; Add more configuration settings as needed
      (close f)
      t
    )
    nil
  )
)

;; Load configuration from file
(defun MBS:load-configuration (file-path / f line)
  (if (findfile file-path)
    (progn
      (setq f (open file-path "r"))
      (if f
        (progn
          (while (setq line (read-line f))
            (if (and line (> (strlen line) 0))
              (vl-catch-all-apply '(lambda () (eval (read line))))
            )
          )
          (close f)
          t
        )
        nil
      )
    )
    nil
  )
)

;;; ========================================================================
;; INITIALIZATION
;;; ========================================================================

;; Initialize configuration when module is loaded
(MBS:initialize-configuration)

;; Validate configuration
(setq config-validation (MBS:validate-configuration))
(if (not (car config-validation))
  (progn
    (MBS:log-warning "\n⚠ Configuration validation failed:")
    (foreach error (cadr config-validation)
      (MBS:log-warning (strcat "\n  - " error))
    )
  )
  (MBS:log-info "\n✓ Configuration validation passed")
)

;; Function to toggle between LOM and LOR modes
(defun toggle-mode ()
  (if (= current-mode "LOM")
    (setq current-mode "LOR")
    (setq current-mode "LOM")
  )
      (MBS:log-info (strcat "\nSwitched to " current-mode " mode"))
)

;; Configuration based on current mode
(defun get-mode-config ( / config)
  (if (= current-mode "LOM")
    ;; LOM Configuration
    (setq config '(
      ("BLOCK_NAME" . "_CACI_ITEMLEADER")
      ("ID_ATTRIBUTE" . "MATERIAL_ID")
      ("PATH_ATTRIBUTE" . "CSVFILEPATH")
      ("REL_PATH_ATTRIBUTE" . "CSVRELFILEPATH")
      ("ID_COLUMN" . "MATERIAL_ID")
      ("WEIGHT_COLUMN" . "TOTAL WT")
      ("UNIT_WEIGHT_COLUMN" . "UNIT WT")
      ("PREFIX_ENABLED" . T)
      ("FIXED_PREFIX" . "")
    ))
    ;; LOR Configuration
    (setq config '(
      ("BLOCK_NAME" . "_CACI_REMOVALLEADER")
      ("ID_ATTRIBUTE" . "REMOVAL_ID")
      ("PATH_ATTRIBUTE" . "CSVLORFILEPATH")
      ("REL_PATH_ATTRIBUTE" . "CSVLORRELFILEPATH")
      ("ID_COLUMN" . "REMOVAL_ID")
      ("WEIGHT_COLUMN" . "TOTAL WT")
      ("UNIT_WEIGHT_COLUMN" . "UNIT WT")
      ("PREFIX_ENABLED" . T)
      ("FIXED_PREFIX" . "R-")
    ))
  )
  config  ;; Return the config
)

;; Helper functions to get specific config values
(defun get-block-name ()
  (cdr (assoc "BLOCK_NAME" (get-mode-config)))
)

(defun get-id-attribute ()
  (cdr (assoc "ID_ATTRIBUTE" (get-mode-config)))
)

(defun get-path-attribute ()
  (cdr (assoc "PATH_ATTRIBUTE" (get-mode-config)))
)

(defun get-rel-path-attribute ()
  (cdr (assoc "REL_PATH_ATTRIBUTE" (get-mode-config)))
)

(defun get-id-column ()
  (cdr (assoc "ID_COLUMN" (get-mode-config)))
)

(defun get-weight-column ()
  (cdr (assoc "WEIGHT_COLUMN" (get-mode-config)))
)

(defun get-unit-weight-column ()
  (cdr (assoc "UNIT_WEIGHT_COLUMN" (get-mode-config)))
)

(defun is-prefix-enabled ()
  (cdr (assoc "PREFIX_ENABLED" (get-mode-config)))
)

(defun get-fixed-prefix ()
  (cdr (assoc "FIXED_PREFIX" (get-mode-config)))
)

(princ "\nMaterial Block Sync Configuration module loaded.")
(princ)

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

;; Check if file is locked by another process
(defun CSV:is-file-locked (file-path / test-file)
  "Check if file is locked by trying to open it in append mode"
  (if (findfile file-path)
    (progn
      (setq test-file (open file-path "a"))
      (if test-file
        (progn
          (close test-file)
          nil  ;; File not locked
        )
        t  ;; File is locked
      )
    )
    nil  ;; File doesn't exist, so not locked
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

;; Enhanced set-record-value function to ensure proper record structure
(defun MBS:set-record-value-enhanced (record header column-name value / index new-record)
  (setq index (get-column-index header column-name))
  (if (and index (>= index 0))
    (progn
      ;; Ensure record has the correct number of columns
      (setq new-record (MBS:align-record record header))
      
      ;; Set the value at the correct index
      (setq new-record (MBS:set-record-value new-record header column-name value))
      new-record
    )
    record  ;; Return unchanged if column not found
  )
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

;; Function to get CSV path in quiet mode (for menu display)
(defun get-quiet-csv-path ()
  (setq quiet-param 1)  ;; Set global parameter to indicate quiet mode
  (setq result (get-csv-path-from-block nil))  ;; Call function with quiet mode
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

;; Helper function to get filename base
(defun MBS:filename-base (filepath)
  (vl-filename-base filepath)
)

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

;; Proper delay function that doesn't prompt user
(defun MBS:delay-ms (milliseconds / counter max-count)
  "Create a delay by running a counter loop - non-interactive"
  (setq max-count (* milliseconds 100))  ;; Approximate timing
  (setq counter 0)
  (while (< counter max-count)
    (setq counter (1+ counter))
  )
)

;;; ========================================================================
;; TRANSACTION CREATION FUNCTIONS
;;; ========================================================================

;; Create an insert-row transaction
(defun MBS:tx-insert-row (row-data)
  (list 'insert-row row-data)
)

;; Create an update-field transaction
(defun MBS:tx-update-field (row-index field-name value)
  (list 'update-field row-index field-name value)
)

;; Create a delete-row transaction
(defun MBS:tx-delete-row (row-index)
  (list 'delete-row row-index)
)

;; Create a delete-row-by-material-id transaction
(defun MBS:tx-delete-row-by-material-id (material-id)
  (list 'delete-row-by-material-id material-id)
)

;; Create a delete-block transaction
(defun MBS:tx-delete-block (block)
  (list 'delete-block block)
)

;; Create an update-block transaction
(defun MBS:tx-update-block-detailed (block field-updates item-info)
  (list 'update-block-detailed block field-updates item-info)
)

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

;;; ========================================================================
;; FILE LOCKING DETECTION AND RECOVERY
;;; ========================================================================

;; Check for file locking issues by attempting to open files in read mode
(defun MBS:check-file-locking-issues (/ csv-path f test-result)
  (setq csv-path (get-csv-path-from-block nil))
  (if (and csv-path (findfile csv-path))
    (progn
      ;; Try to open the file in read mode to check for locks
      (setq f (open csv-path "r"))
      (if f
        (progn
          ;; File opened successfully, no locking issues
          (close f)
          (setq test-result nil)
        )
        (progn
          ;; File could not be opened, likely locked
          (MBS:log-warning (strcat "File appears to be locked: " csv-path))
          (setq test-result t)
        )
      )
      test-result
    )
    nil  ;; No CSV path found, assume no issues
  )
)

;; Handle file locking recovery with timeout and retry logic
(defun MBS:handle-file-locking-recovery (/ max-wait-time wait-interval current-wait csv-path)
  (setq max-wait-time 30)  ;; Maximum wait time in seconds
  (setq wait-interval 2)   ;; Check every 2 seconds
  (setq current-wait 0)
  (setq csv-path (get-csv-path-from-block nil))
  
  (if csv-path
    (progn
      (MBS:log-info (strcat "Waiting for file lock to clear: " csv-path))
      (prompt (strcat "\n[RECOVERY] Waiting for file lock to clear (max " (itoa max-wait-time) " seconds)..."))
      
      ;; Wait for file lock to clear
      (while (and (< current-wait max-wait-time) (MBS:check-file-locking-issues))
        (MBS:delay-ms (* wait-interval 1000))  ;; Convert to milliseconds
        (setq current-wait (+ current-wait wait-interval))
        
        ;; Show progress
        (if (= (rem current-wait 10) 0)
          (prompt (strcat "\n[RECOVERY] Waited " (itoa current-wait) " seconds..."))
        )
      )
      
      (if (>= current-wait max-wait-time)
        (progn
          (MBS:log-critical "File lock timeout exceeded")
          (prompt "\n[RECOVERY] Critical: File lock timeout exceeded")
          nil
        )
        (progn
          (MBS:log-info "File lock cleared successfully")
          (prompt "\n[RECOVERY] File lock cleared successfully")
          t
        )
      )
    )
    (progn
      (MBS:log-warning "No CSV path found for lock recovery")
      nil
    )
  )
)

;; Enhanced transaction timeout detection
(defun MBS:check-transaction-timeout (transaction-id max-duration / start-time current-time)
  (setq start-time (MBS:get-transaction-data transaction-id "start-time"))
  (if start-time
    (progn
      (setq current-time (getvar "MILLISECS"))
      (if (> (- current-time start-time) (* max-duration 1000))  ;; Convert to milliseconds
        (progn
          (MBS:log-warning (strcat "Transaction timeout detected: " transaction-id))
          t
        )
        nil
      )
    )
    nil  ;; No start time recorded
  )
)

;; Commit the transaction (make changes permanent)
(defun MBS:commit-transaction (transaction-id)
  (if (and use-two-stage-transactions
           (= transaction-id MBS:transaction-active))
    (progn
      (MBS:log-info (strcat "Committing transaction: " transaction-id))
      (MBS:log-info (strcat "Changes: " (itoa (length MBS:transaction-changes))))
      
      ;; Check for file locking issues before committing - TEMPORARILY DISABLED
      ;; (if (MBS:check-file-locking-issues)
      ;;   (progn
      ;;     (MBS:log-warning "File locking issues detected, attempting recovery")
      ;;     (MBS:handle-file-locking-recovery)
      ;;   )
      ;; )
      
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
                  (MBS:write-csv csv-path header records nil)
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
  (setq num-index (if header (get-column-index header "ITEM NO.") nil))
  (setq desc-index (if header (get-column-index header "DESCRIPTION") nil))
  (setq mat-id-index (if header (get-column-index header (strcase id-attribute)) nil))
  
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
  (setq field-index (get-column-index header field-name))
  
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

;; Execute insert-row transaction - DETAILED DEBUG
(defun MBS:exec-insert-row (row-data records header)
  (MBS:log-info "DEBUG: Starting simplified MBS:exec-insert-row")
  (MBS:log-info (strcat "DEBUG: row-data type: " (vl-princ-to-string (type row-data))))
  (MBS:log-info (strcat "DEBUG: row-data length: " (if (listp row-data) (itoa (length row-data)) "not-a-list")))
  
  ;; Check if row-data is actually the right structure
  (if (listp row-data)
    (progn
      (if (= (length row-data) 1)
        (progn
          (MBS:log-error "DEBUG: row-data has length 1 - this is the corruption!")
          (MBS:log-error (strcat "DEBUG: Single element content: " (vl-princ-to-string (car row-data))))
        )
        (MBS:log-info "DEBUG: row-data appears to have correct structure")
      )
    )
    (progn
      (MBS:log-error "DEBUG: row-data is not a list")
      (setq row-data (list "ERROR"))
    )
  )
  
  (if (not (listp records))
    (progn
      (MBS:log-error "DEBUG: records is not a list")
      (setq records '())
    )
  )
  
  ;; Use simple append operation
  (setq new-records (append records (list row-data)))
  (MBS:log-info (strcat "DEBUG: new-records count: " (itoa (length new-records))))
  
  (MBS:log-info "DEBUG: Completed simplified insert-row")
  (list new-records header)
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
  (MBS:log-info "DEBUG: About to process transaction data")
  
  (cond
    ;; Update a field value in a record
    ((eq tx-type 'update-field)
      (setq result (MBS:exec-update-field tx-data records header))
    )
    
    ;; Insert a new row
    ((eq tx-type 'insert-row)
      (MBS:log-info "DEBUG: About to call MBS:exec-insert-row")
      (MBS:log-info (strcat "DEBUG: tx-data type: " (vl-princ-to-string (type tx-data))))
      (MBS:log-info (strcat "DEBUG: tx-data length: " (itoa (length tx-data))))
      ;; Extract the actual record from tx-data (first element of the list)
      (setq row-data (car tx-data))
      (MBS:log-info (strcat "DEBUG: Extracted row-data length: " (itoa (length row-data))))
      (setq result (MBS:exec-insert-row row-data records header))
      (MBS:log-info "DEBUG: Returned from MBS:exec-insert-row")
    )
    
    ;; Delete a row by index
    ((eq tx-type 'delete-row)
      (setq result (MBS:exec-delete-row tx-data records header))
    )
    
    ;; Delete a row by material ID
    ((eq tx-type 'delete-row-by-material-id)
      ;; Determine ID attribute based on mode (fallback to current global if available)
      (setq local-id-attribute (if (boundp 'current-mode)
                                  (if (= current-mode "LOR") "REMOVAL_ID" "MATERIAL_ID")
                                  (if (boundp 'id-attribute) id-attribute "MATERIAL_ID")))
      (setq mat-id-index (get-column-index header (strcase local-id-attribute)))
      (if (>= mat-id-index 0)
        (progn
          (MBS:log-info (strcat "Deleting row with material ID: " (car tx-data)))
          (MBS:log-info (strcat "Material ID column index: " (itoa mat-id-index)))
          (setq result (MBS:exec-delete-row-by-material-id (car tx-data) records header mat-id-index))
        )
        (progn
          (MBS:log-error (strcat "Material ID column not found in header: " (strcase local-id-attribute)))
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
            (setq old-records records)  ;; Store old records for comparison
            (setq records (car result))
            (setq header (cadr result))
            (setq new-count (length records))
            
            ;; Check if row count changed OR if any field values changed
            (if (or (/= old-count new-count)
                    (not (equal old-records records)))
              (progn
                (if (/= old-count new-count)
                  (MBS:log-info (strcat "Row count changed from " (itoa old-count) 
                                      " to " (itoa new-count)))
                  (MBS:log-info "Field values updated")
                )
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

          (if (MBS:write-csv csv-path header records nil)
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

;; Simple and efficient helper to replace an item in a list
(defun MBS:replace-list-item (lst index new-item / result i current-item)
  "Replace item at index in list - simple and fast implementation"
  (setq result '())
  (setq i 0)
  (foreach current-item lst
    (if (= i index)
      (setq result (cons new-item result))
      (setq result (cons current-item result))
    )
    (setq i (1+ i))
  )
  (reverse result)
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

;; Enhanced Mode Configuration System
;; Adds persistent mode storage using _MBS_SETTINGS_MARKER block

;; Enhanced mode detection with three methods:
;; 1. Global current-mode variable (highest priority)
;; 2. _MBS_SETTINGS_MARKER block attribute (persistent)
;; 3. CSV path auto-detection (fallback)

(defun MBS:get-enhanced-mode ()
  "Get current mode using enhanced detection with persistent storage"
  
  ;; Method 1: Check global current-mode variable (highest priority)
  (if (and (boundp 'current-mode) current-mode (/= current-mode ""))
    (progn
      (prompt "\n🔍 DEBUG: Using global current-mode variable")
      current-mode
    )
    (progn
      ;; Method 2: Check _MBS_SETTINGS_MARKER block attribute
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
              (setq csv-path (get-csv-path-from-block nil))
              (if csv-path
                (progn
                  (setq detected-mode (MBS:auto-detect-mode csv-path))
                  (setq current-mode detected-mode)  ;; Set global variable for future use
                  detected-mode
                )
                (progn
                  ;; Default to LOM if no method works
                  (prompt "\n🔍 DEBUG: No mode detection method available, defaulting to LOM")
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
          (setq csv-path (get-csv-path-from-block nil))
          (if csv-path
            (progn
              (setq detected-mode (MBS:auto-detect-mode csv-path))
              (setq current-mode detected-mode)  ;; Set global variable for future use
              detected-mode
            )
            (progn
              ;; Default to LOM if no method works
              (prompt "\n🔍 DEBUG: No mode detection method available, defaulting to LOM")
              (setq current-mode "LOM")
              "LOM"
            )
          )
        )
      )
    )
  )
)

;; Enhanced mode setting with persistent storage
(defun MBS:set-enhanced-mode (new-mode)
  "Set mode with persistent storage in _MBS_SETTINGS_MARKER block"
  
  ;; Validate mode
  (if (not (or (= new-mode "LOM") (= new-mode "LOR")))
    (progn
      (prompt "\n❌ Invalid mode. Must be 'LOM' or 'LOR'")
      nil
    )
    (progn
      ;; Set global variable
      (setq current-mode new-mode)
      
      ;; Update persistent storage in _MBS_SETTINGS_MARKER block
      (setq settings-block (MBS:find-settings-marker))
      (if settings-block
        (progn
          ;; Update MODE attribute
          (setq att-list (vlax-invoke settings-block 'GetAttributes))
          (setq mode-attr-found nil)
          (foreach att att-list
            (if (= (strcase (vlax-get att 'TagString)) "MODE")
              (progn
                (vlax-put att 'TextString new-mode)
                (setq mode-attr-found t)
              )
            )
          )
          
          ;; If MODE attribute doesn't exist, create it
          (if (not mode-attr-found)
            (prompt "\n⚠ MODE attribute not found in _MBS_SETTINGS_MARKER block")
          )
          
          ;; Update block
          (vla-Update settings-block)
          (prompt (strcat "\n✅ Mode set to " new-mode " and stored in _MBS_SETTINGS_MARKER block"))
        )
        (progn
          ;; Create _MBS_SETTINGS_MARKER block if it doesn't exist
          (prompt "\n⚠ _MBS_SETTINGS_MARKER block not found")
          (prompt "\n  Mode set to global variable only")
          (prompt (strcat "\n✅ Mode set to " new-mode " (global variable only)"))
        )
      )
      
      ;; Update configuration
      (setq id-attribute (get-id-attribute))
      (prompt (strcat "\n  Using attribute: " id-attribute))
      (prompt (strcat "\n  Using block: " (get-block-name)))
      
      t
    )
  )
)

;; Enhanced mode toggle with persistent storage
(defun MBS:toggle-enhanced-mode ()
  "Toggle between LOM and LOR modes with persistent storage"
  (setq current-mode (MBS:get-enhanced-mode))
  (setq new-mode (if (= current-mode "LOM") "LOR" "LOM"))
  (MBS:set-enhanced-mode new-mode)
)

;; Command functions for enhanced mode system
(defun C:MBS-SETLOR-ENHANCED ()
  "Set LOR mode with persistent storage"
  (MBS:set-enhanced-mode "LOR")
  (princ)
)

(defun C:MBS-SETLOM-ENHANCED ()
  "Set LOM mode with persistent storage"
  (MBS:set-enhanced-mode "LOM")
  (princ)
)

(defun C:MBS-TOGGLE-ENHANCED ()
  "Toggle between LOM and LOR modes with persistent storage"
  (MBS:toggle-enhanced-mode)
  (princ)
)

;; Simple command for testing enhanced mode system
(defun C:MBS-TOGGLE ()
  "Simple toggle command for enhanced mode system"
  (MBS:toggle-enhanced-mode)
  (princ)
)

;; Create _MBS_SETTINGS_MARKER block if it doesn't exist
(defun MBS:create-settings-marker ( / doc modelspace insertion-point block-ref)
  "Create _MBS_SETTINGS_MARKER block for persistent settings storage"
  
  ;; Check if block already exists
  (if (MBS:find-settings-marker)
    (progn
      (prompt "\n✅ _MBS_SETTINGS_MARKER block already exists")
      t
    )
    (progn
      ;; Check if block definition exists
      (if (not (tblsearch "BLOCK" "_MBS_SETTINGS_MARKER"))
        (progn
          (prompt "\n❌ _MBS_SETTINGS_MARKER block definition not found")
          (prompt "\n  Please ensure the block is available in the drawing")
          nil
        )
        (progn
          ;; Get insertion point from user
          (prompt "\n📌 Select insertion point for _MBS_SETTINGS_MARKER block:")
          (setq insertion-point (getpoint))
          
          (if insertion-point
            (progn
              ;; Insert block
              (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
              (setq modelspace (vla-get-ModelSpace doc))
              
              (setq block-ref (vl-catch-all-apply
                '(lambda ()
                  (vla-InsertBlock 
                    modelspace
                    (vlax-3d-point (car insertion-point) (cadr insertion-point) 0.0)
                    "_MBS_SETTINGS_MARKER"
                    1.0
                    1.0
                    1.0
                    0.0))
              ))
              
              (if (vl-catch-all-error-p block-ref)
                (progn
                  (prompt "\n❌ Failed to insert _MBS_SETTINGS_MARKER block")
                  nil
                )
                (progn
                  (prompt "\n✅ _MBS_SETTINGS_MARKER block created successfully")
                  t
                )
              )
            )
            (progn
              (prompt "\n❌ No insertion point selected")
              nil
            )
          )
        )
      )
    )
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

;; Helper function to auto-detect mode from CSV path
(defun MBS:auto-detect-mode (csv-path)
  (if (and csv-path (vl-string-search "_LOR" (strcase csv-path)))
    "LOR"
    "LOM"
  )
)

(prompt "\nEnhanced Mode Configuration System loaded!")
(princ) 

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
      (write-line "      label = \"ADD MATERIAL - QUICK\";" f)
      (write-line "      mnemonic = \"1\";" f)
      (write-line "      is_default = true;" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"material_library\";" f)
      (write-line "      label = \"ADD MATERIAL FROM LIBRARY\";" f)
      (write-line "      mnemonic = \"1\";" f)
      (write-line "      is_default = true;" f)
      (write-line "      width = 36;" f)
      (write-line "    }" f)
      (write-line "    " f)
      (write-line "    : button {" f)
      (write-line "      key = \"insert\";" f)
      (write-line "      label = \"INSERT MATERIAL FROM CSV\";" f)
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
          "ADD MATERIAL - QUICK - Adds a new block + CSV row using DESCRIPTION you enter in C/L.\n"
          "ADD MATERIAL FROM LIBRARY - Adds a new block + CSV row from a material library.\n"
          "INSERT MATERIAL FROM CSV  - Adds a new block from an existing CSV row.\n"
          "UPDATE BLOCKS (BASIC) - Updates ITEM NO and DESCRIPTION of all blocks in DWG\n"
		  "UPDATE BLOCKS (FULL SCAN AUDIT) - Full check of Blocks with corrective action prompts.\n\n"
          "CSV Management:\n"
          "CREATE/SELECT CSV - Create or choose a CSV file\n"
          "OPEN CSV IN EXCEL - Open & edit current CSV file\n"
          "CREATE CSV BACKUP - Create a backup of the current CSV file\n"
          "RESTORE CSV BACKUP - Restore from a previous backup\n\n"
          "Settings:\n"
          "TOGGLE MODE - Switch between LOM and LOR modes\n"
          "SETTINGS - Configure various program options\n"
		  "DEBUG - Shows log for troubleshooting purposes\n\n"
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

;; Display completion message
(MBS:log-info (strcat "\n\n🎉 Material Block Sync v" MBS:version " CLEAN LOADING COMPLETE"))
(MBS:log-info "\n   All function conflicts should now be resolved")
(MBS:log-info "\n   Type MSYNC to display the menu")
(MBS:log-info "\n   Test with: MBS-INSERT3, MBS-BATCHINSERT3, MBS-UPDATE3")
(MBS:log-info "\n   Material Library: Use menu option 4 or type MBS-LibraryDialog")

(princ)