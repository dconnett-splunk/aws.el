;;; aws-log-streams.el --- Emacs major modes wrapping the AWS CLI

;; Copyright (C) 2022, Marcel Patzwahl

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Author: Marcel Patzwahl

;;; Commentary:

;; Emacs major modes wrapping the AWS CLI

;;; Code:
(require 'json)
(require 'subr-x)
(require 'transient)

(defvar-local aws-log-streams-current-group-name nil)
(defvar-local aws-log-streams-current-prefix nil)

(defun aws-log-streams--quote (value)
  "Return VALUE quoted for use in an AWS CLI shell command."
  (shell-quote-argument value))

(defun aws-log-streams-get-latest-logs-command (log-group-name &optional count prefix)
  "Return the aws command to retrieve the latest logs for LOG-GROUP-NAME.
An optional COUNT can be passed to limit the maximum amount of log events.
An optional PREFIX can be passed to filter log streams by name."
  (let* ((has-prefix (and prefix (not (string-empty-p prefix))))
         (max-items-string (if count (concat " --max-items " count) ""))
         (prefix-string (if has-prefix
                            (concat " --log-stream-name-prefix "
                                    (aws-log-streams--quote prefix))
                          ""))
         (order-string (unless has-prefix
                         " --order-by LastEventTime --descending")))
    (concat "logs describe-log-streams --log-group-name "
            (aws-log-streams--quote log-group-name)
            " --output text"
            " --query 'logStreams[*].[logStreamName,lastEventTimestamp,storedBytes]'"
            order-string
            prefix-string
            max-items-string)))

(defun aws-log-streams-describe-log-streams (log-group-name &optional prefix)
  "Tabulated-list-view of the log streams for a given LOG-GROUP-NAME."
  (aws-core--tabulated-list-from-command-multi-column
   (aws-log-streams-get-latest-logs-command log-group-name nil prefix)
   [("Log Streams" 90) ("Last Event" 18) ("Stored Bytes" 12)]))

(defun aws-log-streams-refresh ()
  "Refresh the current log streams buffer."
  (interactive)
  (unless aws-log-streams-current-group-name
    (user-error "No CloudWatch Logs group is active"))
  (let ((current-line (aws-core--get-current-line)))
    (message "Refreshing buffer...")
    (aws-log-streams-describe-log-streams
     aws-log-streams-current-group-name
     aws-log-streams-current-prefix)
    (forward-line current-line)
    (message "Buffer refreshed")))

(defun aws-log-streams-set-prefix (prefix)
  "Filter the current log stream list by PREFIX."
  (interactive "sLog stream prefix: ")
  (unless aws-log-streams-current-group-name
    (user-error "No CloudWatch Logs group is active"))
  (setq-local aws-log-streams-current-prefix
              (unless (string-empty-p prefix) prefix))
  (aws-log-streams-describe-log-streams
   aws-log-streams-current-group-name
   aws-log-streams-current-prefix))

(defun aws-log-streams-get-log-event-in-view ()
  "Get the log events for the current log stream."
  (interactive)
  (let ((current-log-stream-name (tabulated-list-get-id)))
    (aws-log-streams-get-log-event aws-log-streams-current-group-name current-log-stream-name)))

(defun aws-log-streams--event-timestamp (millis)
  "Format CloudWatch Logs MILLIS as a UTC timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (seconds-to-time (/ millis 1000))
                      t))

(defun aws-log-streams--insert-events (output)
  "Insert CloudWatch Logs events from JSON OUTPUT into the current buffer."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (events (alist-get 'events (json-read-from-string output))))
    (dolist (event events)
      (insert
       (format "%s\t%s\n"
               (aws-log-streams--event-timestamp
                (alist-get 'timestamp event))
               (or (alist-get 'message event) ""))))))

(defun aws-log-streams-get-log-event (log-group log-stream)
  "Get the log events for the LOG-GROUPs LOG-STREAM."
  (let ((buffer (concat "*" log-group ": " log-stream "*"))
        (cmd (concat
              (aws-cmd)
              "logs get-log-events --log-group-name "
              (aws-log-streams--quote log-group)
              " --log-stream-name "
              (aws-log-streams--quote log-stream)
              " --start-from-head --output json")))
    (setq aws--last-command cmd)
    (with-current-buffer (get-buffer-create buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Log group:  %s\nLog stream: %s\n\n" log-group log-stream))
        (let ((output (shell-command-to-string (concat cmd " 2>&1"))))
          (condition-case err
              (aws-log-streams--insert-events output)
            (error
             (insert (format "Failed to parse log events: %s\n\nCommand:\n%s\n\nOutput:\n%s"
                             err
                             cmd
                             output))))))
      (goto-char (point-min))
      (view-mode 1))
    (switch-to-buffer buffer)
    (setq buffer-read-only t)))

(transient-define-prefix aws-log-streams-help-popup ()
  "AWS Log Streams Menu"
  ["Actions"
   ("RET" "Get Log Events" aws-log-streams-get-log-event-in-view)
   ("/" "Filter Stream Prefix" aws-log-streams-set-prefix)
   ("g" "Refresh Buffer" aws-log-streams-refresh)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Log Groups" aws-logs)])

(defvar aws-log-streams-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-log-streams-get-log-event-in-view)
    (define-key map (kbd "?") 'aws-log-streams-help-popup)
    (define-key map (kbd "/") 'aws-log-streams-set-prefix)
    (define-key map (kbd "g") 'aws-log-streams-refresh)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws-logs)
    map))

(defun aws-log-streams-from-line-under-cursor ()
  "Get the Log Streams for the Log Group under the cursor.
Used from the aws-logs mode."
  (interactive)
  (let ((log-group-name (tabulated-list-get-id)))
    (aws-log-streams log-group-name)))

(defun aws-log-streams (log-group-name)
  "List the LOG-GROUP-NAMEs log streams."
  (interactive "slog-group name: ")
  (aws--pop-to-buffer (aws--buffer-name "log-streams"))
  (aws-log-streams-mode)
  (setq-local aws-log-streams-current-group-name log-group-name)
  (setq-local aws-log-streams-current-prefix nil)
  (aws-log-streams-describe-log-streams log-group-name))

(define-derived-mode aws-log-streams-mode tabulated-list-mode "aws-log-streams"
  "AWS Log Stream Mode"
  (setq major-mode 'aws-log-streams-mode)
  (use-local-map aws-log-streams-mode-map))

(provide 'aws-log-streams)
;;; aws-log-streams.el ends here
