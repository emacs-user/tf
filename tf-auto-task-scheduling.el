;;; tf-auto-task-scheduling.el --- Automatic task prioritization and scheduling

;; Copyright 2021 Sergey Ievlev

;; Author: Sergey Ievlev <ievlev.sergey@gmail.com>
;; URL: http://github.com/emacs-user/task-factory
;; Version: 1.0
;; Package-Requires: ((org "9.4.6"))

;; Code goes here

(defun tf-auto-task-scheduling ()
  "Calculates the actual priorities of tasks and performs automatic scheduling of their execution"
  (interactive)
  (tf-task-priority-set)
  (tf-task-schedule-set))

(defun tf-task-schedule-set ()
  "Automatic scheduling of tasks according to their actual priority"
  (interactive)
  (save-restriction
    (save-excursion
      (let (id
	    effort
	    plan-mode
	    (dayeffort (tf-get-efficiency-time-today))
	    (plan-date (current-time)))
	(with-temp-buffer (insert-file-contents tf-id-to-sort-file-path)
			  (sort-numeric-fields 1 (point-min) (point-max))
			  (reverse-region (point-min) (point-max))
			  (goto-char 1)
			  (while (not (eq (line-number-at-pos) (+ (count-lines (point-min) (point-max)) 1)))
			    (setq id (nth 1 (split-string (thing-at-point 'line t))))
			    (setq effort (or (nth 3 (split-string (thing-at-point 'line t))) "00:15"))
			    (setq plan-mode (or (nth 4 (split-string (thing-at-point 'line t))) "auto"))
			    (if (not (and (tf--planning-today-p plan-date) (> dayeffort 0)))
				    (progn (setq plan-date (tf--move-plan-date-tomorrow plan-date))
				           (setq dayeffort (tf-get-efficiency-time-today plan-date))))
			    (if (not (member "manual" (split-string plan-mode ":")))
				    (progn (with-temp-buffer (org-id-goto id)
							                 (org-entry-put (point)
									                        "SCHEDULED"
									                        (tf--org-get-time-stamp plan-date)))
				           (setq dayeffort (- dayeffort (tf--convert-org-time-string-to-second effort)))))
			    (next-line)))))))

(defun tf--move-plan-date-tomorrow (plan-date)
  "Returns the `plan-date' extended to the next business day"
  (let ((time (time-add (time-to-seconds plan-date) (tf--to-next-day-second plan-date))))
    (while (member (format-time-string "%u" time) '("6" "7"))
      (setq time (time-add (time-to-seconds time) (tf--to-next-day-second time))))
    time))

(defun tf--to-next-day-second (time)
  "Returns the number of seconds from `time' to the
     start time of the day `tf-work-day-begin' of the
     next CALENDAR day"
  (let ((hour (nth 2 (decode-time time)))
	(min (nth 1 (decode-time time)))
	(sec (nth 0 (decode-time time))))
    (+ (+ (- 60 sec) (* 60 (- 60 min)) (* 3600 (- 23 hour)))
       (* 3600 (string-to-number (nth 0 (split-string tf-work-day-begin ":"))))
       (* 60 (string-to-number (nth 1 (split-string tf-work-day-begin ":")))))))

(defun tf--planning-today-p (time)
  "Returns t if today's work day has not yet expired and there is time to complete the task"
  (let (begin-sec
	end-sec
	time-sec)
    (setq begin-sec (tf--convert-org-time-string-to-second tf-work-day-begin))
    (setq end-sec (tf--convert-org-time-string-to-second tf-work-day-end))
    (setq time-sec (+ (nth 0 (decode-time time))
		      (* 60 (nth 1 (decode-time time)))
		      (* 3600 (nth 2 (decode-time time)))))
    (and (> time-sec begin-sec) (< time-sec end-sec))))

(defun tf--convert-org-time-string-to-time (org-time-string &optional today)
  "Converts `org-time-string' (`hh:mm' format string) to an emacs time value.
     If `today' is not nil, the time with the current date is returned"
  (if today
      (encode-time 0
		   (string-to-number (nth 1 (split-string org-time-string ":")))
		   (string-to-number (nth 0 (split-string org-time-string ":")))
		   (nth 3 (decode-time (current-time)))
		   (nth 4 (decode-time (current-time)))
		   (nth 5 (decode-time (current-time))))
    (encode-time 0 (string-to-number (nth 1 (split-string org-time-string ":"))) (string-to-number (nth 0 (split-string org-time-string ":"))) 0 0 0)))

(defun tf--convert-org-time-string-to-second (org-time-string)
  "Returns the number of seconds in time in a string of type HH: MM"
  (+ (* 3600 (string-to-number (nth 0 (split-string org-time-string ":"))))
     (* 60 (string-to-number (nth 1 (split-string org-time-string ":"))))))

(defun tf--org-get-time-stamp (time)
  "Returns a date string in Org format for insertion into SCHEDULE / DEADLINE properties"
  (with-temp-buffer (replace-regexp-in-string "\[<>\]" "" (org-insert-time-stamp time))))

(defun tf-get-efficiency-time-today (&optional date)
  "Returns the value of the efficiently spent working time today in second.
     If the `date' value is set - returns the remaining effective time for this date."
  (if date
      (- (tf--convert-org-time-string-to-second tf-work-day-duration)
	     (tf--get-manual-time-today date))
      (- (tf--convert-org-time-string-to-second tf-work-day-duration)
         (tf--get-manual-time-today)
         (tf--get-other-time-today)
         (tf--org-clock-sum-today))))

(defun tf--get-other-time-today ()
  "Returns the time in seconds spent today on other tasks"
  (with-temp-buffer (org-id-goto tf--organization-task-id)
		            (org-clock-sum-today)
		            (tf--convert-org-time-string-to-second
		             (or (org-entry-get (point) "CLOCKSUM_T") "00:00"))))

(defun tf--get-manual-time-today (&optional date)
  "Returns the time in seconds scheduled today (on `date', if set) for manually scheduled tasks (tag - `manual`)"
  (save-restriction
      (save-excursion
          (let ((total 0)
	            match)
	          (if date
	              (setq match (format-time-string "<%Y-%m-%d>" date))
	              (setq match (format-time-string "<%Y-%m-%d>" (current-time))))
	          (with-temp-buffer (org-tags-view t (concat "+manual+SCHEDULED=\"" match "\""))
			                    (if (> (count-lines (point-min) (point-max)) 2)
			                        (while (not (eq (line-number-at-pos) (count-lines (point-min) (point-max))))
				                        (org-agenda-next-item 1)
				                        (setq total (+ total
					                                   (tf--convert-org-time-string-to-second
						                                (or (org-entry-get (org-agenda-get-any-marker) "EFFORT") "0:15")))))))
	          total))))

(defun tf--org-clock-sum-today ()
    "Visit each file in `org-agenda-files' and return the total time of today's
     clocked tasks in second."
    (let ((files (org-agenda-files))
	      (total 0))
        (org-agenda-prepare-buffers files)
        (dolist (file files)
            (with-current-buffer (find-buffer-visiting file)
	            (setq total (+ total (org-clock-sum-today)))))
        (* 60 total)))

(defun tf-task-priority-set ()
  "Sets actual priorities for all tasks with a TODO"
  (interactive)
  (save-restriction
    (save-excursion
      (org-todo-list)
      (let (priority
	    deadline
	    effort
	    id)
	(with-temp-buffer (find-file tf-id-to-sort-file-path)
			  (delete-region (point-min) (point-max))
			  (save-buffer))
	(while (not (eq (line-number-at-pos) (count-lines (point-min) (point-max))))
	  (org-agenda-next-item 1)
	  (setq priority (or (org-entry-get (org-agenda-get-any-marker) "PRIORITY" t nil) ""))
	  (setq deadline (org-entry-get (org-agenda-get-any-marker) "DEADLINE" t nil))
	  (setq effort (org-entry-get (org-agenda-get-any-marker) "EFFORT" t nil))
	  (if (or (string= deadline "") (eq deadline nil))
	      (setq deadline (current-time-string)))
	  (if (or (string= effort "") (eq effort nil))
	      (setq effort "01:01"))
	  (org-entry-put
	   (org-agenda-get-any-marker)
	   "FPRIORITY"
	   (number-to-string (tf--calc-fact-priority
			      priority
			      deadline
			      effort)))
	  (org-entry-put
	   (org-agenda-get-any-marker)
	   "BUFFER"
	   (tf--buffer-calc 
	    effort
	    (current-time-string)
	    deadline))
	  (append-to-file (concat (org-entry-get (org-agenda-get-any-marker) "FPRIORITY" t nil) " "
				  (org-id-get (org-agenda-get-any-marker) t nil) " "
				  (org-entry-get (org-agenda-get-any-marker) "TODO" t nil) " "
				  (org-entry-get (org-agenda-get-any-marker) "SCHEDULE" t nil) " "
				  (org-entry-get (org-agenda-get-any-marker) "EFFORT" t nil) " "
				  (org-entry-get (org-agenda-get-any-marker) "TAGS" t nil) " "
				  "\n")
			  nil
			  tf-id-to-sort-file-path)))
      (kill-buffer (get-file-buffer tf-id-to-sort-file-path))
      (org-agenda-Quit))))

(defvar tf-id-to-sort-file-path "~/pro/tmp/id-to-sort"
  "Path to a temporary file for storing intermediate calculation results")

(defun tf-org-schedule-force-note ()
  "Call org-schedule but make sure it prompts for re-scheduling note."
  (interactive)
  (let ((org-log-reschedule 'note))
    (call-interactively 'org-schedule)))

(defun tf-org-deadline-force-note ()
  "Call org-deadline but make sure it prompts for re-deadlining note."
  (interactive)
  (let ((org-log-redeadline 'note))
    (call-interactively 'org-deadline)))

(defun tf--calc-fact-priority (priority deadline effort &optional current)
  "Calculating the actual priority of the task"
  (let ((E (tf--convert-time-to-work-day effort))
	(x (tf--work-days-calc (or current (current-time-string)) deadline))
	(P (tf--priority-calc priority)))
    (/ P (exp (- x E)))))

(defun tf--convert-time-to-work-day (time)
  "Convert time string of format 'HH:MM' on weekdays"
  (let ((tm (parse-time-string time))
	(dr (parse-time-string tf-work-day-duration)))
    (/ (tf--convert-time-list-to-hours tm) (tf--convert-time-list-to-hours dr))))

(defun tf--convert-time-list-to-hours (time-list)
  "Convert a list of the form '(SS MM HH)' to a value in hours"
  (+ (/ (nth 0 time-list) 3600.0)
     (/ (nth 1 time-list) 60.0)
     (nth 2 time-list)))

(defun tf--work-days-calc (begin end)
  "Returns the time difference between BEGIN and END in working days.
If BEGIN is before END then the return value is positive or equal to zero.
Otherwise, the return value is negative."
  (let ((work-day-begin (parse-time-string tf-work-day-begin))
	(work-day-end (parse-time-string tf-work-day-end))
	(work-day-duration (parse-time-string tf-work-day-duration))
	(beginPoint (parse-time-string begin))
	(endPoint (parse-time-string end))
	(workDays 0)
	(deltaBegin 0)
	(deltaEnd 0)
	forwardPlan)
    (setq beginPoint (tf--test-time-correct beginPoint))
    (setq endPoint (tf--test-time-correct endPoint))
    (setq beginPoint (tf--shift-timePoint-to-work-time beginPoint))
    (setq endPoint (tf--shift-timePoint-to-work-time endPoint))
    (if (tf--day-is-same beginPoint endPoint)
	(tf--time-difference beginPoint endPoint)
      (setq forwardPlan (tf--plan-forward-p beginPoint endPoint))
      (setq deltaBegin (tf--time-delta beginPoint forwardPlan))
      (setq deltaEnd (tf--time-delta endPoint forwardPlan))
      (setq workDays (tf--work-days-dif beginPoint endPoint forwardPlan))
      (if forwardPlan
	  (+ workDays deltaBegin deltaEnd)
	(* (+ workDays deltaBegin deltaEnd) -1.0)))))

(defun tf--test-time-correct (time)
  "If no time is specified, it returns a time value with a `time' less than one minute to midnight"
  (if (or (eq (nth 0 time) nil) (eq (nth 1 time) nil) (eq (nth 2 time) nil))
      (append '(59 59 23) (nthcdr 3 time))
    time))

(defun tf--shift-timePoint-to-work-time (time)
  "Returns the closest working time to `time'"
  (let ((work-day-begin (parse-time-string tf-work-day-begin))
	(work-day-end (parse-time-string tf-work-day-end))
	(current-time time))
    (if (< (tf--convert-time-list-to-hours current-time) (tf--convert-time-list-to-hours work-day-begin))
	(setq current-time (append (seq-take work-day-begin 3) (nthcdr 3 current-time))))
    (if (> (tf--convert-time-list-to-hours current-time) (tf--convert-time-list-to-hours work-day-end))
	(setq current-time (append (seq-take work-day-end 3) (nthcdr 3 current-time))))
    (if (= (calendar-day-of-week (list (nth 4 current-time) (nth 3 current-time) (nth 5 current-time))) 6)
	(setq current-time (decode-time (time-add (encode-time (nth 0 current-time) (nth 1 current-time)
							       (nth 2 current-time) (nth 3 current-time)
							       (nth 4 current-time) (nth 5 current-time))
						  (* 86400 2))))) ; + 2 day
    (if (= (calendar-day-of-week (list (nth 4 current-time) (nth 3 current-time) (nth 5 current-time))) 0)
	(setq current-time (decode-time (time-add (encode-time (nth 0 current-time) (nth 1 current-time)
							       (nth 2 current-time) (nth 3 current-time)
							       (nth 4 current-time) (nth 5 current-time))
						  (* 86400 1))))) ; + 1 day
    current-time))

(defun tf--day-is-same (time1 time2)
  "Returns t if the dates `time1' and `time2' are the same"
  (and (= (nth 3 time1) (nth 3 time2))
       (= (nth 4 time1) (nth 4 time2))
       (= (nth 5 time1) (nth 5 time2))))

(defun tf--time-difference (time1 time2)
  "Returns the difference in working time between `time1' and `time2' ignoring the date"
  (let ((work-day-duration (parse-time-string tf-work-day-duration)))
    (if (>= (abs (- (tf--convert-time-list-to-hours time1) (tf--convert-time-list-to-hours time2)))
	    (tf--convert-time-list-to-hours work-day-duration))
	1.0
      (/ (abs (- (tf--convert-time-list-to-hours time1) (tf--convert-time-list-to-hours time2)))
	 (tf--convert-time-list-to-hours work-day-duration)))))

(defun tf--time-delta (time forward)
  "Returns the difference between `time' and the start (`forward' = nil) or end (`forward' = t) of the work day.
If the difference is more than the duration of the working day, then the duration is returned"
  (let ((work-day-begin (parse-time-string tf-work-day-begin))
	(work-day-end (parse-time-string tf-work-day-end))
	(work-day-duration (parse-time-string tf-work-day-duration))
	(delta 0))
    (if forward
	(setq delta (/ (abs (- (tf--convert-time-list-to-hours time) (tf--convert-time-list-to-hours work-day-end)))
		       (tf--convert-time-list-to-hours work-day-duration)))
      (setq delta (/ (abs (- (tf--convert-time-list-to-hours time) (tf--convert-time-list-to-hours work-day-begin)))
		     (tf--convert-time-list-to-hours work-day-duration))))
    (if (> delta 1.0) (setq delta 1.0))
    delta))

(defun tf--plan-forward-p (time1 time2)
  "Returns t if `time1' < `time2' and nil otherwise"
  (< (time-to-seconds (encode-time (nth 0 time1) (nth 1 time1)
				   (nth 2 time1) (nth 3 time1)
				   (nth 4 time1) (nth 5 time1)))
     (time-to-seconds (encode-time (nth 0 time2) (nth 1 time2)
				   (nth 2 time2) (nth 3 time2)
				   (nth 4 time2) (nth 5 time2)))))

(defun tf--work-days-dif (start end forward)
  "Returns the number of full business days between `start' and `end' times"
  (let ((dif nil)
	(count 0)
	day-w)
    (setq dif (abs (- (time-to-days (encode-time (nth 0 start) (nth 1 start)
						 (nth 2 start) (nth 3 start)
						 (nth 4 start) (nth 5 start)))
		      (time-to-days (encode-time (nth 0 end) (nth 1 end)
						 (nth 2 end) (nth 3 end)
						 (nth 4 end) (nth 5 end))))))
    (if (> dif 0) (setq dif (1- dif)))
    (if forward
	(setq day-w (calendar-day-of-week (list (nth 4 start)
						(nth 3 start)
						(nth 5 start))))
      (setq day-w (calendar-day-of-week (list (nth 4 end)
					      (nth 3 end)
					      (nth 5 end)))))
    (while (> dif 0)
      (if (= day-w 6)
	  (setq day-w 1
		dif (- dif 2))
	(setq day-w (1+ day-w)))
      (setq count (1+ count))
      (setq dif (1- dif)))
    count))

(defvar tf-work-day-begin    "09:00" "Start time of the working day. The default is 09:00")
(defvar tf-work-day-end      "18:00" "The end time of the working day. The default is 18:00")
(defvar tf-work-day-duration "06:00" "The length of the working day in hours. Default 06:00")

(defun tf--priority-calc (priority)
  "Convert letter-to-number `priority'"
  (cond ((string= priority "A") 5)
	((string= priority "B") 4)
	((string= priority "C") 3)
	((string= priority "D") 2)
	((string= priority "")  1)))

(defun tf--three-point-effort (min norm max)
  "Three-point estimate '(MIN + 4xNORM + MAX) / 6'"
  (/ (+ min (* norm 4.0) max) 6.0))

(defun tf-effort-set (norm min max)
  "Setting the complexity of the task by the three-point estimation method"
  (interactive "nNormal estimate (hours): \nnOptimistic estimate (hours): \nnPessimistic estimate (hours): ")
  (let ((ef (tf--three-point-effort min norm max))
	hh
	mm)
    (setq hh (floor ef))
    (setq mm (floor (* 60 (- ef hh))))
    (org-set-property "EFFORT" (concat (number-to-string hh) ":" (number-to-string mm)))))

(defun tf--buffer-calc (effort begin end)
  "Calculates the current consumption of the buffer.
Returns the consumption value as a string (as a percentage)."
  (let ((bf (/ (tf--convert-time-list-to-hours (parse-time-string effort)) 2.0))
	(work-day-duration (tf--convert-time-list-to-hours (parse-time-string tf-work-day-duration)))
	delta
	result)
    (setq delta (* (tf--work-days-calc begin end) work-day-duration))
    (if (>= delta bf) "100%"
      (setq result (* (/ delta bf) 100.0))
      (concat (number-to-string (round result)) "%"))))

;;; tf-auto-task-scheduling.el ends here
