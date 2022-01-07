;;; tf-file-capture.el --- Create Org entry from Dired buffer and save files as attachments

;; Copyright 2022 Sergey Ievlev

;; Author: Sergey Ievlev <ievlev.sergey@gmail.com>
;; URL: http://github.com/emacs-user/task-factory
;; Version: 1.0
;; Package-Requires: ((org "9.4.6"))

;; Code goes here

(defun tf--helm-get-org-id ()
    "Returns the org-id of an entry selected from agenda files using helm"
    (save-window-excursion
        (save-restriction
            (helm-org-agenda-files-headings)
            (org-id-copy))))

(defvar tf--dired-file-list nil)

(defun tf-dired-org-create ()
    "Create new Org entry from Dired buffer and save files as attachments"
    (interactive)
    (if (eq major-mode 'dired-mode)
        (progn
            (setq tf--dired-file-list (dired-get-marked-files))
            (add-hook 'org-capture-before-finalize-hook
                      'tf--dired-org-capture-finalize)
            (helm-org-capture-templates))
        (message "You are not in the Dired buffer!")))

(defun tf--dired-org-capture-finalize ()
    "Finish editing a new Org entry"
    (org-id-copy)
    (dolist (file tf--dired-file-list)
            (org-attach-attach file nil 'mv))
    (setq tf--dired-file-list nil)
    (remove-hook 'org-capture-before-finalize-hook
                 'tf--dired-org-capture-finalize))

(define-key dired-mode-map (kbd "<f9> <f9>") 'tf-dired-org-create)

(defun tf-dired-org-attach ()
    "Adds the selected Dired files to the Org entry specified by the interactive selection"
    (interactive)
    (if (eq major-mode 'dired-mode)
        (progn
            (setq tf--dired-file-list (dired-get-marked-files))
            (save-window-excursion
                (save-restriction
                    (org-id-goto (tf--helm-get-org-id))
                    (org-id-copy)
                    (dolist (file tf--dired-file-list)
                        (org-attach-attach file nil 'mv))))
            (revert-buffer)
            (setq tf--dired-file-list nil))
        (message "You are not in the Dired buffer!")))

(define-key dired-mode-map (kbd "<f9> a") 'tf-dired-org-attach)

;;; tf-file-capture.el ends here
