;; (set-face-attribute
;;  'tabbar-default nil
;;  :background "gray20"
;;  :foreground "gray20"
;;  :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :underline nil
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-modified nil
 :background "gray30"
 :foreground "red"
 :underline nil
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))

;; highlight is hover behavior
(set-face-attribute
 'tabbar-highlight nil
 :background "DarkCyan"
 :foreground "green"
 :underline nil
 :box '(:color "DarkCyan" :style nil))

;; defaults for button
(set-face-attribute
 'tabbar-button nil
 :underline nil
 :box '(:line-width 1 :color "white" :style nil))
;; (set-face-attribute
;;  'tabbar-separator nil
;;  :background "gray20"
;;  :height 0.6)




;; example tabbar coloring code...
;; (set-face-attribute
;;  'tabbar-default nil   
;; :background "gray24")
(set-face-attribute
 'tabbar-unselected nil
 :background "gray34"
 :foreground "white"
 :box '(:line-width 1 :color "white" :style released-button)
 )
(set-face-attribute
 'tabbar-modified nil
 :background "gray34"
 :foreground "pink"
 :inherit 'tabbar-unselected
 :box '(:line-width 1 :color "white" :style released-button)
 )
(set-face-attribute
 'tabbar-selected nil
 :background "#bcbcbc"
 :foreground "black"
 :box nil)
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute
 'tabbar-separator nil
 :height 0.7)



;; set big fonts
(set-face-attribute
 'tabbar-button nil
 :inherit 'tabbar-default-face
 :box '(:line-width 1 :color "gray30"))

(set-face-attribute 'tabbar-default  nil
 ;;:family "Courier"
 :height 1.1)

(set-face-attribute
 'tabbar-selected nil
 :inherit 'tabbar-default-face
 :foreground "blue3"
 :background "LightGoldenrod"
 :box '(:line-width 1 :color "DarkGoldenrod")
 ;;:overline "black" :underline "black"
 :weight 'bold)

(set-face-attribute
 'tabbar-unselected nil
 :inherit 'tabbar-default-face
 :box '(:line-width 1 :color "gray70"))
;;end set big fonts



;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))
;; adding spaces
(defun tabbar-buffer-tab-label (tab)
    "Return a label for TAB.
That is, a string used to represent it on the tab bar."
    (let ((label  (if tabbar--buffer-show-groups
		      (format " [%s] " (tabbar-tab-tabset tab))
		    (format " %s " (tabbar-tab-value tab)))))
      ;; Unless the tab bar auto scrolls to keep the selected tab
      ;; visible, shorten the tab label to keep as many tabs as possible
      ;; in the visible area of the tab bar.
      (if tabbar-auto-scroll-flag
	  label
	(tabbar-shorten
	 label (max 1 (/ (window-width)
			 (length (tabbar-view
                                  (tabbar-current-tabset)))))))))




;; add a marker to all modified buffers to help identify what has changed
(progn
  
  ;; add a buffer modification state indicator in the tab label,
  (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
    (setq ad-return-value
          (if (and (buffer-modified-p (tabbar-tab-value tab))
                   (buffer-file-name (tabbar-tab-value tab)))
              (concat "" (concat ad-return-value "[*]"))
            (concat "" (concat ad-return-value "")))))
  ;; called each time the modification state of the buffer changed
  (defun ztl-modification-state-change ()
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))
  ;; first-change-hook is called BEFORE the change is made
  (defun ztl-on-buffer-modification ()
    (set-buffer-modified-p t)
    (ztl-modification-state-change))
  (add-hook 'after-save-hook 'ztl-modification-state-change)
  ;; this doesn't work for revert, I don't know
  ;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
  (add-hook 'first-change-hook 'ztl-on-buffer-modification))




