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