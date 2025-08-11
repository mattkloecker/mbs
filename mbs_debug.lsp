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