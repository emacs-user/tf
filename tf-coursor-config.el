(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (super) (control meta super))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))
;;
(if (is-osx) (setq modifierKey "s")) ; Default modifier key
(if (is-linux) (setq modifierKey "M"))
;;
;; cursor up
(global-unset-key (kbd (concat modifierKey "-i")))
(global-set-key (kbd (concat modifierKey "-i")) 'previous-line)
;; cursor down
(global-unset-key (kbd (concat modifierKey "-k")))
(global-set-key (kbd (concat modifierKey "-k")) 'next-line)
;; cursor left
(global-unset-key (kbd (concat modifierKey "-j")))
(global-set-key (kbd (concat modifierKey "-j")) 'backward-char)
;; cursor right
(global-unset-key (kbd (concat modifierKey "-l")))
(global-set-key (kbd (concat modifierKey "-l")) 'forward-char)
;; page up
(global-unset-key (kbd (concat modifierKey "-I")))
(global-set-key (kbd (concat modifierKey "-I")) 'scroll-down-command)
;; page down
(global-unset-key (kbd (concat modifierKey "-K")))
(global-set-key (kbd (concat modifierKey "-K")) 'scroll-up-command)
;; one word forward
(global-unset-key (kbd (concat modifierKey "-L")))
(global-set-key (kbd (concat modifierKey "-L")) 'forward-word)
;; one word back
(global-unset-key (kbd (concat modifierKey "-J")))
(global-set-key (kbd (concat modifierKey "-J")) 'backward-word)
;; end of line
(global-unset-key (kbd (concat modifierKey "-;")))
(global-set-key (kbd (concat modifierKey "-;")) 'move-end-of-line)
;; beginning of line
(global-unset-key (kbd (concat modifierKey "-h")))
(global-set-key (kbd (concat modifierKey "-h")) 'move-beginning-of-line)
;; end of buffer
(global-unset-key (kbd (concat modifierKey "-:")))
(global-set-key (kbd (concat modifierKey "-:")) 'end-of-buffer)
;; beginning of buffer
(global-unset-key (kbd (concat modifierKey "-H")))
(global-set-key (kbd (concat modifierKey "-H")) 'beginning-of-buffer)
;; delete character to the right of cursor
(global-unset-key (kbd (concat modifierKey "-o")))
(global-set-key (kbd (concat modifierKey "-o")) 'delete-forward-char)
;; delete character to the left of cursor
(global-unset-key (kbd (concat modifierKey "-u")))
(global-set-key (kbd (concat modifierKey "-u")) 'delete-backward-char)
;; line feed with alignment (two identical combinations)
(global-unset-key (kbd (concat modifierKey "-n")))
(global-set-key (kbd (concat modifierKey "-n")) 'reindent-then-newline-and-indent)
(global-unset-key (kbd (concat modifierKey "-m")))
(global-set-key (kbd (concat modifierKey "-m")) 'reindent-then-newline-and-indent)
;;
;;
;; set mark
(global-unset-key (kbd (concat modifierKey "-SPC")))
(global-set-key (kbd (concat modifierKey "-SPC")) 'set-mark-command)
;; copy
(global-unset-key (kbd (concat modifierKey "-c")))
(global-set-key (kbd (concat modifierKey "-c")) 'copy-region-as-kill)
;; cut
(global-unset-key (kbd (concat modifierKey "-x")))
(global-set-key (kbd (concat modifierKey "-x")) 'kill-region)
;; paste
(global-unset-key (kbd (concat modifierKey "-v")))
(global-set-key (kbd (concat modifierKey "-v")) 'yank)
;; undo
(global-unset-key (kbd (concat modifierKey "-z")))
(global-set-key (kbd (concat modifierKey "-z")) 'undo)
;;
;;
(if (is-osx)
    (progn
        ;; move to window up
        (global-unset-key (kbd "M-i"))
        (global-set-key (kbd "M-i") 'windmove-up)
        ;; move to window up
        (global-unset-key (kbd "M-k"))
        (global-set-key (kbd "M-k") 'windmove-down)
        ;; move to window left
        (global-unset-key (kbd "M-j"))
        (global-set-key (kbd "M-j") 'windmove-left)
        ;; move to window right
        (global-unset-key (kbd "M-l"))
        (global-set-key (kbd "M-l") 'windmove-right)))
(if (is-linux) ;; with PC keyboard
    (progn
        (setq w32-pass-lwindow-to-system nil)
        (setq w32-lwindow-modifier 'super)
        ;; move to window up
        (global-unset-key (kbd "s-i"))
        (global-set-key (kbd "s-i") 'windmove-up)
        ;; move to window up
        (global-unset-key (kbd "s-k"))
        (global-set-key (kbd "s-k") 'windmove-down)
        ;; move to window left
        (global-unset-key (kbd "s-j"))
        (global-set-key (kbd "s-j") 'windmove-left)
        ;; move to window right
        (global-unset-key (kbd "s-l"))
        (global-set-key (kbd "s-l") 'windmove-right)))
;;
;; ???????????????????? ????????????????
;; ===================
;;
;; ?????????????????????? ?????????? ??????????
(global-unset-key (kbd "C-I"))
(global-set-key (kbd "C-I") 'buf-move-up)
;; ?????????????????????? ?????????? ????????
(global-unset-key (kbd "C-K"))
(global-set-key (kbd "C-K") 'buf-move-down)
;; ?????????????????????? ?????????? ??????????
(global-unset-key (kbd "C-J"))
(global-set-key (kbd "C-J") 'buf-move-left)
;; ?????????????????????? ?????????? ????????????
(global-unset-key (kbd "C-L"))
(global-set-key (kbd "C-L") 'buf-move-right)
;; ?????????????????? ???????????? ????????????
(global-unset-key (kbd "C-("))
(global-set-key (kbd "C-(") 'my-shrink-vert)
;; ?????????????????? ???????????? ????????????
(global-unset-key (kbd "C-)"))
(global-set-key (kbd "C-)") 'my-enlarge-vert)
;; ?????????????????? ???????????? ????????????
(global-unset-key (kbd "C-9"))
(global-set-key (kbd "C-9") 'my-shrink-horz)
;; ?????????????????? ???????????? ????????????
(global-unset-key (kbd "C-0"))
(global-set-key (kbd "C-0") 'my-enlarge-horz)
;; ???????????????????? ???????????? ???????????? ?? ?????????????????? 30/70
(global-unset-key (kbd (concat modifierKey "-(")))
(global-set-key (kbd (concat modifierKey "-(")) 'my-super-shrink-vert)
;; ???????????????????? ???????????? ???????????? ?? ?????????????????? 70/30
(global-unset-key (kbd (concat modifierKey "-)")))
(global-set-key (kbd (concat modifierKey "-)")) 'my-super-enlarge-vert)
;; ???????????????????? ???????????? ???????????? ?? ?????????????????? 30/70
(global-unset-key (kbd (concat modifierKey "-9")))
(global-set-key (kbd (concat modifierKey "-9")) 'my-super-shrink-horz)
;; ???????????????????? ???????????? ???????????? ?? ?????????????????? 70/30
(global-unset-key (kbd (concat modifierKey "-0")))
(global-set-key (kbd (concat modifierKey "-0")) 'my-super-enlarge-horz)
;; ???????????????????? ???????????? ?????????????? ?? ?????????????????? 50/50
(global-unset-key (kbd (concat modifierKey "-8")))
(global-set-key (kbd (concat modifierKey "-8")) 'my-50%-horz)
;; ???????????????????? ???????????? ?????????????? ?? ?????????????????? 50/50
(global-unset-key (kbd (concat modifierKey "-*")))
(global-set-key (kbd (concat modifierKey "-*")) 'my-50%-vert)
;;
(defun my-enlarge-vert ()
  (interactive)
  (enlarge-window 2))
(defun my-shrink-vert ()
  (interactive)
  (enlarge-window -2))
(defun my-enlarge-horz ()
  (interactive)
  (enlarge-window-horizontally 2))
(defun my-shrink-horz ()
  (interactive)
  (enlarge-window-horizontally -2))
(defun my-50%-horz ()
  (interactive)
  (let* ((width (round (* (frame-width) 0.5)))
         (cur-width (window-width))
         (delta (- width (+ cur-width 5))))
    (enlarge-window-horizontally delta)))
(defun my-50%-vert ()
  (interactive)
  (let* ((height (round (* (frame-height) 0.5)))
         (cur-height (window-height))
         (delta (- height (+ cur-height 5))))
    (enlarge-window delta)))
(defvar *larg-window-size-percent* 0.7)
(defun my-50%-horz ()
  (interactive)
  (let* ((width (round (* (frame-width) 0.5)))
         (cur-width (window-width))
         (delta (- width (+ cur-width 5))))
    (enlarge-window-horizontally delta)))
(defun my-50%-vert ()
  (interactive)
  (let* ((height (round (* (frame-height) 0.5)))
         (cur-height (window-height))
         (delta (- height (+ cur-height 5))))
    (enlarge-window delta)))
(defun my-super-enlarge-horz ()
  (interactive)
  (let* ((width (round (* (frame-width) *larg-window-size-percent*)))
         (cur-width (window-width))
         (delta (- width cur-width)))
    (enlarge-window-horizontally delta)))
(defun my-super-enlarge-vert ()
  (interactive)
  (let* ((height (round (* (frame-height) *larg-window-size-percent*)))
         (cur-height (window-height))
         (delta (- height cur-height)))
    (enlarge-window delta)))
(defun my-super-shrink-horz ()
  (interactive)
  (let* ((width (round (* (frame-width) (- 1 *larg-window-size-percent*))))
         (cur-width (window-width))
         (delta (- width cur-width)))
    (enlarge-window-horizontally delta)))
(defun my-super-shrink-vert ()
  (interactive)
  (let* ((height (round (* (frame-height) (- 1 *larg-window-size-percent*))))
         (cur-height (window-height))
         (delta (- height cur-height)))
    (enlarge-window delta)))

(reverse-input-method 'russian-computer)
