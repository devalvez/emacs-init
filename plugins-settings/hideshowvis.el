;; ----------------- SHOWVIS --------------------
;;Hideshow
;; (add-to-list 'load-path "~/.emacs.d/elpa/hideshowvis/")
(defun hideshowvis-enable ()
  "Will enable hideshowvis minor mode"
  (interactive)
  (hideshowvis-minor-mode 1))

;; HedeShow interaction
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'hideshowvis-hideable-marker [0 0 0 126 126 0 0 0]))

(defconst hideshowvis-version "v0.5" "Version of hideshowvis minor mode")

(defface hideshowvis-hidable-face
  '((t (:foreground "#ccc" :box t)))
  "Face to highlight foldable regions"
  :group 'hideshow)

(defcustom hideshowvis-ignore-same-line t
  "Do not display foldable regions in the fringe if the matching
  closing parenthesis is on the same line. Set this to nil if
  enabling the minor mode is slow on your machine"
  :group 'hideshow)

(defun hideshowvis-highlight-hs-regions-in-fringe (&optional start end old-text-length)
  (when hs-minor-mode
    (save-excursion
      (save-restriction
        (when (and start end)
          (narrow-to-region start end))
        (goto-char (point-min))
        (remove-overlays (point-min) (point-max) 'hideshowvis-hs t)
        (while (search-forward-regexp hs-block-start-regexp nil t)
          (let* ((ovl (make-overlay (match-beginning 0) (match-end 0)))
                 (marker-string "*hideshowvis*")
                 (doit
                  (if hideshowvis-ignore-same-line
                      (let (begin-line)
                        (setq begin-line
                              (save-excursion
                                (goto-char (match-beginning 0))
                                (line-number-at-pos (point))))
                        (save-excursion
                          (goto-char (match-beginning 0))
                          (ignore-errors
                            (progn
                              (funcall hs-forward-sexp-func 1)
                              (> (line-number-at-pos (point)) begin-line)))))
                    t)))
            (when doit
              (put-text-property 0
                                 (length marker-string)
                                 'display
                                 (list 'left-fringe
                                       'hideshowvis-hideable-marker
                                       'hideshowvis-hidable-face)
                                 marker-string)
              (overlay-put ovl 'before-string marker-string)
              (overlay-put ovl 'hideshowvis-hs t))))))))

;;;###autoload
(defun hideshowvis-click-fringe (event)
  (interactive "e")
  (mouse-set-point event)
  (end-of-line)
  (if (save-excursion
        (end-of-line 1)
        (or (hs-already-hidden-p)
            (progn
              (forward-char 1)
              (hs-already-hidden-p))))
      (hs-show-block)
    (hs-hide-block)
    (beginning-of-line)))

(defvar hideshowvis-mode-map
  (let ((hideshowvis-mode-map (make-sparse-keymap)))
    (define-key hideshowvis-mode-map [left-fringe mouse-1]
		            'hideshowvis-click-fringe)
    hideshowvis-mode-map)
  "Keymap for hideshowvis mode")

;;;###autoload
(define-minor-mode hideshowvis-minor-mode ()
  "Will indicate regions foldable with hideshow in the fringe."
  :init-value nil
  :require 'hideshow
  :group 'hideshow
  :keymap hideshowvis-mode-map
  (condition-case nil
      (if hideshowvis-minor-mode
          (progn
            (hs-minor-mode 1)
            (hideshowvis-highlight-hs-regions-in-fringe (point-min) (point-max) 0)
            (add-to-list 'after-change-functions
                         'hideshowvis-highlight-hs-regions-in-fringe))
        (remove-overlays (point-min) (point-max) 'hideshowvis-hs t)
        (setq after-change-functions
              (remove 'hideshowvis-highlight-hs-regions-in-fringe
                      after-change-functions)))
    (error
     (message "Failed to toggle hideshowvis-minor-mode")
     )))

;;;###autoload
(defun hideshowvis-enable ()
  "Will enable hideshowvis minor mode"
  (interactive)
  (hideshowvis-minor-mode 1))

;;;###autoload
(defun hideshowvis-symbols ()
  "Defines the things necessary to get a + symbol in the fringe
and a yellow marker indicating the number of hidden lines at
the end of the line for hidden regions."
  (interactive)
  
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0]))
  
  (defcustom hs-fringe-face 'hs-fringe-face
    "*Specify face used to highlight the fringe on hidden regions."
    :type 'face
    :group 'hideshow)
  
  (defface hs-fringe-face
    '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
    "Face used to highlight the fringe on folded regions"
    :group 'hideshow)
  
  (defcustom hs-face 'hs-face
    "*Specify the face to to use for the hidden region indicator"
    :type 'face
    :group 'hideshow)
  
  (defface hs-face
    '((t (:background "#ff8" :box t)))
    "Face to hightlight the ... area of hidden regions"
    :group 'hideshow)
  
  (defun display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((marker-string "*fringe-dummy*")
             (marker-length (length marker-string))
             (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov))))
             )
        (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
        (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
        (overlay-put ov 'before-string marker-string)
        (put-text-property 0 (length display-string) 'face 'hs-face display-string)
        (overlay-put ov 'display display-string)
        )))
  
  (setq hs-set-up-overlay 'display-code-line-counts))
(provide 'hideshowvis)
;; ---------------------- ENDHIDESHOW -------------------------
