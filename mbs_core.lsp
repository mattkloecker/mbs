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
      (prompt "\nERROR: Failed to access AutoCAD object model.")
      (setq result nil)
    )
  )
  
  result
)

;; Simplified init environment
(defun MBS:init-environment (/ result)
  (setq result t)  ;; Assume success
  
  ;; Initialize global variables if not already defined
  (if (not (boundp 'id-attribute))
    (setq id-attribute "MATERIAL_ID")
  )
  
  (if (not (boundp 'alternate-id-attribute))
    (setq alternate-id-attribute "REMOVAL_ID")
  )
  
  
  ;; Initialize transaction queue
  (if (not (boundp 'MBS:transaction-queue))
    (MBS:clear-transactions)
  )
  
  ;; Simple block check
  (if (not (tblsearch "BLOCK" (get-block-name)))
    (progn
      (prompt (strcat "\nERROR: Required block '" (get-block-name) "' not found."))
      (prompt "\nPlease ensure that the block exists in the drawing.")
      (setq result nil)
    )
  )
  
  result
)

;; Clear transaction queue
(defun MBS:clear-transactions ()
  (setq MBS:transaction-queue '())
)

(prompt "\nMaterial Block Sync Core Module loaded.")
(princ)