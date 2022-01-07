;;; tf-mu4e-org-integration.el --- create Org entry from mu4e buffer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022, Sergey Ievlev, all rights reserved.
;;
;; Author: Sergey Ievlev <ievlev.sergey@gmail.com>
;; Created:  4 January 2022
;; Keywords: Org mu4e
;; URL: https://github.com/emacs-user/tf.git
;; Package-Requires: ((org "9.4.4") (mu4e "1.6.10"))
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(defvar tf--mu4e-data-list nil
  "Variable for staging data for creating a task.
List with the following sequence of elements:
0. subject
1. org-link
2. from
3. to
4. copy
5. date
6. body
7. attachments")

(define-key mu4e-headers-mode-map (kbd "<f9> <f9>") 'tf-mu4e-org-new-entry-create)
(define-key mu4e-view-mode-map (kbd "<f9> <f9>") 'tf-mu4e-org-new-entry-create)
(define-key mu4e-headers-mode-map (kbd "<f9> a") 'tf-mu4e-org-attach)
(define-key mu4e-view-mode-map (kbd "<f9> a") 'tf-mu4e-org-attach)

(defun tf-mu4e-org-attach ()
    "Adds e-mail on point to the Org entry specified by the interactive selection"
    (interactive)
    (setq tf--mu4e-data-list (tf--mu4e-message-buffer-grab))
    (save-window-excursion
        (save-restriction
            (org-id-goto (tf--helm-get-org-id))
            (ignore-errors
                (tf--mu4e-org-add-note (org-id-copy) (tf--mu4e-org-note-str-compose tf--mu4e-data-list)))
            (org-indent-indent-buffer)))
    (setq tf--mu4e-data-list nil))

(defun tf-mu4e-org-new-entry-create ()
    "Creates a new task"
    (interactive)
    (setq tf--mu4e-data-list (tf--mu4e-message-buffer-grab))
    (add-hook 'org-capture-after-finalize-hook
              'tf--mu4e-org-capture-finalize)
    (helm-org-capture-templates))

(defun tf--mu4e-org-capture-finalize ()
    "Finish editing a new Org entry"
    (ignore-errors
        (tf--mu4e-org-add-note (org-id-copy) (tf--mu4e-org-note-str-compose tf--mu4e-data-list)))
    (org-indent-indent-buffer)
    (setq tf--mu4e-data-list nil)
    (remove-hook 'org-capture-after-finalize-hook
                 'tf--mu4e-org-capture-finalize))

(defun tf--mu4e-message-buffer-grab ()
    "Returns a list of data to fill in the task fields"
    (setq tf--mu4e-data-list
          (list
           (mu4e-message-field-at-point :subject)
           (org-store-link nil nil)
           (tf--mu4e-convert-mailaddr-to-string (mu4e-message-field-at-point :from))
           (tf--mu4e-convert-mailaddr-to-string (mu4e-message-field-at-point :to))
           (tf--mu4e-convert-mailaddr-to-string (mu4e-message-field-at-point :cc))
           (format-time-string "%d.%m.%y %a %T" (mu4e-message-field-at-point :date))
           (ignore-errors (concat (substring (mu4e-message-field-at-point :body-txt) 0 70) "..."))
           (mu4e-message-field-at-point :attachments))))

(defun tf--mu4e-convert-mailaddr-to-string (list)
    "Converts a `list' in mu4e address format to a string"
    (let ((str))
        (dolist (address list)
            (setq str (concat str
                              (car address) " <"
                              (cdr address) "> ")))
        str))

(defun tf--mu4e-org-add-note (org-id note-str)
    "Insert note `note-str' in drawer LOGBOOK to entry with `org-id'"
    (let ((str-ins (concat "- "
                           (format-time-string (org-time-stamp-format t t))
                           ": Заметка \\\\\n")))
        (setq str-ins (concat str-ins note-str))
        (save-window-excursion
            (org-id-goto org-id)
            (search-forward ":LOGBOOK:")
            (newline)
            (insert str-ins)
            (org-indent-drawer))))

(defun tf--test ()
    (interactive)
    (tf--mu4e-org-add-note "BB326347-A708-494F-A415-843A55E843DA" "new note"))

(defun tf--mu4e-org-note-str-compose (data-list)
    "Forms a string to be inserted into a note"
    (let ((str nil))
        (setq str (concat "e-mail: " (nth 1 data-list) "\n"
                          "From:   " (nth 2 data-list) "\n"
                          "To:     " (nth 3 data-list) "\n"
                          "CC:     " (nth 4 data-list) "\n"
                          "Date:   " (nth 5 data-list) "\n"
                          "Attach: " (nth 7 data-list) "\n"
                          (nth 6 data-list)))
        str))

  ;;   e-mail  : "О новом порядке установления з/п"
  ;;   From:   Сергей Романтеев <romanteev.s.v@cniiag.local>
  ;;   To:     Сергей Иевлев <ievlev.s.a@cniiag.local>
  ;;   CC:     Александр Станкевич <stankevich.a.a@cniiag.local>
  ;;   Date:   [2022-01-06 чт 22:40]
  ;;   Attach: Пояснительная записка.docx
  ;;   Добрый день! Предлагаю новый порядок начисления з/п для отдела...


;; 0. subject
;; 1. org-link
;; 2. from
;; 3. to
;; 4. copy
;; 5. date
;; 6. body
;; 7. attachments

;;; tf-mu4e-org-integration.el ends here
