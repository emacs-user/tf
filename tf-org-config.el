(defun tf--org-todo-state-change-property ()
  (pcase (org-get-todo-state)
    ("NEXT" (tf--org-NEXT-state-property))
    ("TODO" (tf-org-TODO-state-property))
    ("DONE" (tf--org-DONE-state-property))
    ("CONTROL" (tf--org-CONTROL-state-property))
    ("WAIT" (tf--org-WAIT-state-property))
    ("HOLD" (tf--org-HOLD-state-property))
    ("CANCEL" (tf--org-CANCEL-state-property))
    ("MEET" (tf--org-MEET-state-property))))

(defun tf--org-TODO-state-property ()
  (org-set-property "Effort" "0:15")
  (org-set-property "CLOCK_MODELINE_TOTAL" "auto")
  (org-delete-property "Участники"))

(defun tf--org-NEXT-state-property ()
  (org-set-property "Effort" "0:30")
  (org-set-property "CLOCK_MODELINE_TOTAL" "auto")
  (org-delete-property "Участники"))

(defun tf--org-DONE-state-property ()
  (org-delete-property "Участники"))

(defun tf--org-CONTROL-state-property ()
  (org-set-property "Effort" "0:15")
  (org-set-property "CLOCK_MODELINE_TOTAL" "today")
  (org-delete-property "Участники"))

(defun tf--org-WAIT-state-property ()
  (org-set-property "Effort" "0:15")
  (org-set-property "CLOCK_MODELINE_TOTAL" "today")
  (org-delete-property "Участники"))

(defun tf--org-HOLD-state-property ()
  (org-set-property "Effort" "0:15")
  (org-set-property "CLOCK_MODELINE_TOTAL" "today")
  (org-delete-property "Участники"))

(defun tf--org-CANCEL-state-property ()
  (org-delete-property "Участники"))

(defun tf--org-MEET-state-property ()
  (org-set-property "Effort" "1:00")
  (org-set-property "CLOCK_MODELINE_TOTAL" "auto")
  (call-interactively 'org-schedule))

(defun tf-remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))


(defun tf--skip-if-not-today ()
  "If this function returns nil, the current match should not be skipped.
        Otherwitse, the function must return a position from where the search
        should be continued."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (deadline-day
         (or (ignore-errors (time-to-days
                             (org-time-string-to-time
                              (org-entry-get nil "DEADLINE"))))
             0))
        (scheduled-day
         (or (ignore-errors (time-to-days
                             (org-time-string-to-time
                              (org-entry-get nil "SCHEDULED"))))
             0))
        (now (time-to-days (current-time))))
    (and (and (not (= deadline-day now)) (not (= scheduled-day now)))
         subtree-end)))

(defun tf--agenda-sort (a b)
  (let* ((aPos (get-text-property 0 'org-marker a))
         (bPos (get-text-property 0 'org-marker b))
         (aPriorityValue (string-to-number (or (org-entry-get aPos "FPRIORITY") "")))
         (bPriorityValue (string-to-number (or (org-entry-get bPos "FPRIORITY") ""))))
    (if (eq aPriorityValue bPriorityValue)
        nil
      (cl-signum (- aPriorityValue bPriorityValue)))))

(defun tf--clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
    Skips capture tasks, projects, and subprojects.
    Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (tf--is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (tf--is-project-p))
      "TODO"))))

(defun tf--find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun tf-punch-in (arg)
  "Start continuous clocking and set the default task to the
    selected task.  If no task is selected set the Organization task
    as the default task."
  (interactive "p")
  (setq tf--keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (tf-clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (tf-clock-in-organization-task-as-default)))))

(defun tf-punch-out ()
  (interactive)
  (setq tf--keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun tf--clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun tf--clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when tf--keep-clock-running
            (tf--clock-in-default-task)))))))

(defvar tf--organization-task-id "0d967179-06e6-45a4-a9e8-33f97cfaee7b")

(defun tf-clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find tf--organization-task-id 'marker)
    (org-clock-in '(16))))

(defun tf--clock-out-maybe ()
  (when (and tf--keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (tf--clock-in-parent-task)))


(defun tf--clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun tf-clock-in-last-task (&optional arg)
  "Clock in the interrupted task if there is one
    Skip the default task and get the next one.
    A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defun tf--is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun tf--is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
    Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (tf--find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun tf--is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun tf--is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun tf--list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
      This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun tf--list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
      This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar tf--hide-scheduled-and-waiting-next-tasks t)

(defun tf-toggle-next-task-display ()
  (interactive)
  (setq tf--hide-scheduled-and-waiting-next-tasks (not tf--hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if tf--hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun tf--skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (tf--is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun tf--skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (tf--list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (tf--is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun tf--skip-non-projects ()
  "Skip trees that are not projects"
  ;; (tf--list-sublevels-for-projects-indented)
  (if (save-excursion (tf--skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((tf--is-project-p)
            nil)
           ((and (tf--is-project-subtree-p) (not (tf--is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun tf--skip-non-tasks ()
  "Show non-project tasks.
    Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((tf--is-task-p)
        nil)
       (t
        next-headline)))))

(defun tf--skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((tf--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun tf--skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and tf--hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((tf--is-project-p)
        next-headline)
       ((and (tf--is-task-p) (not (tf--is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun tf--skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
    When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
    When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((tf--is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (tf--is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (tf--is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun tf--skip-project-tasks ()
  "Show non-project tasks.
    Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((tf--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((tf--is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun tf--skip-non-project-tasks ()
  "Show project tasks.
    Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((tf--is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (tf--is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (tf--is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun tf--skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((tf--is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun tf--skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (tf--is-subproject-p)
        nil
      next-headline)))


(defun tf-interrupt (arg)
  "Unscheduled interruption: switching to the scratch buffer with timing enabled for other tasks"
  (interactive "p")
  (tf-punch-in arg)
  (switch-to-buffer "*scratch*")
  (delete-other-windows))

(defun tf-org-set-tags ()
  (interactive)
  (save-excursion
    ;(outline-previous-visible-heading 1)
    (org-set-tags-command)))


(defun tf-helm-org-agenda-files-headings ()
  "Модифицированная функция 'helm-org-agenda-files-headings' без проверки временных файлов"
  (interactive)
  (let ((autosaves nil))
    (when (or (null autosaves)
              helm-org-ignore-autosaves
              (y-or-n-p (format "%s have auto save data, continue?"
                                (mapconcat 'identity autosaves ", "))))
      (helm :sources (helm-source-org-headings-for-files (org-agenda-files))
            :candidate-number-limit 99999
            :truncate-lines helm-org-truncate-lines
            :buffer "*helm org headings*"))))


(defun tf-org-insert-extension-link ()
  "Вставка ссылки с интерактивным поиском"
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (buffer-modified-p))
        (save-buffer))))
  (let (link
        description)
    (save-excursion
      (call-interactively 'tf-helm-org-agenda-files-headings)
      (setq link (org-id-copy))
      (setq description (org-get-heading t t t t)))
    (org-insert-link nil (concatenate 'string "id:" link) description)))




(defcustom org-e1c-command 'e1c
  "Команда для отображения ссылки 1С."
  :group 'org-link
  :type 'e1c)

(defun org-1с-open (path)
  "Переход по ссылке 1С к PATH."
  (funcall org-1с-command path))


(defun tf--display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))


(when (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
  (setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin")))  

(defvar tf--export-tmp-dir "~/tmp/"
  "Путь к временной директории для PDF и служебных файлов")

(defvar tf--export-template-dir "~/.template/"
  "Путь к директории с шаблонами")

(defvar tf--export-template-TeX "orgTaskTemplate.tex"
  "Имя файла с шаблоном TeX по умолчанию")

(defvar tf--export-template-pdf "orgTemplate1.pdf"
  "Имя файла с шаблоном PDF по умолчанию")

(defun tf--export-task (title create status id priority link shedule deadline effort tags content notes resolution filename &optional open)
  "Экспорт данных задачи в формат PDF
    title - заголовок задачи
    create - дата создания
    status - текущий статус
    id - идентификатор
    priority - приоритет
    link - ссылка
    shedule - напоминание
    deadline - крайний срок
    effort - длительность
    tags - теги
    content - содержание
    notes - заметка
    resolution - решение
    filename - имя выходного файла без расширения
    open - если не nil, то созданный файл будет открыт"
  (copy-file (concat tf--export-template-dir tf--export-template-TeX)
             (concat tf--export-tmp-dir tf--export-template-TeX) t)
  (copy-file (concat tf--export-template-dir tf--export-template-pdf)
             (concat tf--export-tmp-dir tf--export-template-pdf) t)
  (with-temp-buffer
    (insert-file-contents (concat tf--export-tmp-dir tf--export-template-TeX))
    (tf--export-serch-and-replace "{Title}" (concat "{" title "}"))
    (tf--export-serch-and-replace "{CreateDate}" (concat "{" create "}"))
    (tf--export-serch-and-replace "{Status}" (concat "{" status "}"))
    (tf--export-serch-and-replace "{ID}" (concat "{" id "}"))
    (tf--export-serch-and-replace "{Priority}" (concat "{" priority "}"))
    (tf--export-serch-and-replace "{Link}" (concat "{" link "}"))
    (tf--export-serch-and-replace "{Shedule}" (concat "{" shedule "}"))
    (tf--export-serch-and-replace "{Deadline}" (concat "{" deadline "}"))
    (tf--export-serch-and-replace "{Effort}" (concat "{" effort "}"))
    (tf--export-serch-and-replace "{Tags}" (concat "{" tags "}"))
    (tf--export-serch-and-replace "{Content}" (concat "{" content "}"))
    (tf--export-serch-and-replace "{Notes}" (concat "{" notes "}"))
    (tf--export-serch-and-replace "{Resolution}" (concat "{" resolution "}"))
    (write-file (concat tf--export-tmp-dir tf--export-template-TeX) nil))
  (shell-command (concat "cd " tf--export-tmp-dir " && xelatex -jobname=" (or filename "tmpfile") " " tf--export-template-TeX))
  (if open
      (shell-command (concat "cd " tf--export-tmp-dir " && open " (or filename "tmpfile") ".pdf"))))

(defun tf--export-serch-and-replace (search replace)
  "Находит в буфере первое вхождение строки SEARCH и заменяет ее на REPLACE"
  (goto-char (point-min))
  (replace-string search replace))

(defun tf-export-search-logbook-content ()
  "Находит и возвращает содержимое LOGBOOK DRAWER в формате, приемлемом для экспорта"
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (let ((start (search-forward-regexp ":LOGBOOK:"))
          (end (- (search-forward-regexp ":END:") 5)))
      (replace-regexp-in-string "CLOCK:.*" "" (buffer-substring start end)))))

(defun tf--export-convert-org-to-latex (orgString)
  "Возвращает `orgString' в формате LaTeX"
  (with-temp-buffer
    (insert orgString)
    (set-mark (point-max))
    (goto-char (point-min))
    (org-latex-convert-region-to-latex)
    (buffer-string)))

(defun tf-org-export-current-task (&optional file-name open)
  "Экспорт текущей задачи в формат PDF
    Экспортирует задачу в которой находится точка"
  (interactive)
  (let ((title (org-entry-get (point) "ITEM"))
        (create (replace-regexp-in-string "\\(\\[\\|\\]\\)" "" (or (org-entry-get (point) "CREATED") "")))
        (status (org-entry-get (point) "TODO"))
        (id (concat (ignore-errors (substring (org-entry-get (point) "ID") 0 2)) "..."
                    (ignore-errors (substring (org-entry-get (point) "ID") 30 36))))
        (priority (or (ignore-errors (substring (org-entry-get (point) "FPRIORITY") 0 10)) ""))
        (link (replace-regexp-in-string "]]" ""
                                        (replace-regexp-in-string "\\[\\[.*\\]\\[" ""
                                                                  (or (org-entry-get (point) "CONTEXT") ""))))
        (shedule (replace-regexp-in-string "\\(<\\|>\\)" "" (or (org-entry-get (point) "SCHEDULED") "")))
        (deadline (replace-regexp-in-string "\\(<\\|>\\)" "" (or (org-entry-get (point) "DEADLINE") "")))
        (effort (concat (or (org-entry-get (point) "Effort") "0:00") "/" (number-to-string (/ (org-clock-sum-current-item) 60)) ":" (format "%02d" (% (org-clock-sum-current-item) 60))))
        (tags (replace-regexp-in-string ":" " " (or (org-entry-get (point) "ALLTAGS") "")))
        (content nil)
        (notes (tf--export-convert-org-to-latex (substring-no-properties (tf-export-search-logbook-content)
                                                                        nil (if (< (length (tf-export-search-logbook-content)) 1500)
                                                                                (length (tf-export-search-logbook-content))
                                                                              1500))))
        (resolution (or (org-entry-get (point) "RESOLVE") "")))
    (tf--export-task title create status id priority link shedule deadline effort tags content notes resolution file-name open)))

(defun tf-export-active-task-tags (tag &optional end-date)
  "Возвращает PDF из активных задач, имеющих тег TAG
    TAG - строка имени тега для поиска
    END-DATE - дата ограничивающая поиск задач справа"
  (interactive "sВведите тег для экспорта задач: ")
  (save-restriction
    (save-excursion
      (let ((count 1)
            (cmd-string ""))
        (delete-directory tf--export-tmp-dir t nil)
        (make-directory tf--export-tmp-dir)
        (org-todo-list)
        (let ((line-num (count-lines (point-min) (point-max))))
          (while (not (eq (line-number-at-pos) line-num))
            (org-agenda-next-item 1)
            (save-excursion
              (when (string-match tag (or (org-entry-get (org-agenda-get-any-marker) "TAGS" t nil) ""))
                (org-agenda-goto)
                (tf-org-export-current-task (concat "export-" (number-to-string count)) nil)
                (setq count (1+ count))))))
        (org-agenda-Quit)
        (setq count (1- count))
        (while (not (eq count 0))
          (setq cmd-string (concat (concat "export-" (number-to-string count) ".pdf ") cmd-string))
          (setq count (1- count)))
        (if (not (eq cmd-string ""))
            (shell-command (concat "cd " tf--export-tmp-dir " && pdftk " cmd-string " cat output export.pdf && open export.pdf"))
          (message "Задач для экспорта не найдено"))))))


(defun tf--find-shorten-long-word (string long short)
  "Возвращает сроку STRING в которой все слова длиннее LONG укорочены до длины SHORT"
  (let ((words (split-string string))
        number
        (index 0))
    (setq number  (length words))
    (while (< index number)
      (if (> (length (nth index words)) long) (setcar (nthcdr index words) (s-truncate short (nth index words))))
      (setq index (1+ index)))
    (s-join " " words)))


(defun tf-normalize-contact-item ()
  (interactive)
  (outline-next-heading)
  (let ((end-line (line-number-at-pos)))
    (outline-previous-heading)
    (while (< (line-number-at-pos) end-line)
      (next-line)
      (let ((begin (search-forward-regexp "." nil t (line-end-position)))
            (end (line-end-position)))
        ))))

(defun tf-clock-in-and-narrow ()
    "Clock in and narrow task on point"
    (interactive)
    (org-clock-in)
    (org-narrow-to-subtree))
