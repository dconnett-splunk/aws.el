;;; aws-logs.el --- Emacs major modes wrapping the AWS CLI

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
(require 'subr-x)
(require 'transient)
(require 'url-util)
(require 'aws-log-streams)

(defun aws-logs-describe-log-groups ()
  "List CloudWatch Logs log groups."
  (aws-core--tabulated-list-from-command-multi-column
   "logs describe-log-groups --output=text --query 'logGroups[*].[logGroupName,storedBytes,retentionInDays,metricFilterCount]' --output text"
   [("LogGroupName" 85) ("Stored Bytes" 15) ("Retention" 10) ("Metric Filters" 5)]))

(defun aws-logs-describe-log-groups-refresh ()
  "Refresh the CloudWatch Logs log group overview."
  (interactive)
  (aws-core--refresh-list-view 'aws-logs-describe-log-groups))

(defun aws-logs-describe-log-group ()
  "Describe the log group under the cursor."
  (interactive)
  (let ((cmd (concat "logs describe-log-groups"
                    " --query 'logGroups[0]'"
                    " --log-group-name-prefix")))
    (aws-core--describe-current-resource cmd)))

(defun aws-logs--decode-console-component (value)
  "Decode CloudWatch Logs URL component VALUE."
  (let ((decoded (url-unhex-string
                  (replace-regexp-in-string "\\$" "%" value))))
    (url-unhex-string decoded)))

(defun aws-logs--parse-console-url (url)
  "Parse a CloudWatch Logs console URL into a list of log group and stream.
The returned list is (LOG-GROUP LOG-STREAM).  LOG-STREAM may be nil."
  (let ((target (or (cadr (split-string url "#")) url)))
    (cond
     ((string-match "log-group/\\([^/?#]+\\)/log-events/\\([^?#]+\\)"
                    target)
      (let ((log-group (match-string 1 target))
            (log-stream (match-string 2 target)))
        (list
         (aws-logs--decode-console-component log-group)
         (aws-logs--decode-console-component log-stream))))
     ((string-match "log-group/\\([^/?#]+\\)" target)
      (let ((log-group (match-string 1 target)))
        (list
         (aws-logs--decode-console-component log-group)
         nil)))
     (t
      (user-error "Could not find a CloudWatch Logs group in URL")))))

(defun aws-logs-open-console-url (url)
  "Open CloudWatch Logs group or stream from console URL."
  (interactive "sCloudWatch Logs URL: ")
  (pcase-let ((`(,log-group ,log-stream)
               (aws-logs--parse-console-url url)))
    (if log-stream
        (aws-log-streams-get-log-event log-group log-stream)
      (aws-log-streams log-group))))

(defun aws-logs-get-log-events (log-group log-stream)
  "Open CloudWatch Logs events for LOG-GROUP and LOG-STREAM."
  (interactive "sLog group: \nsLog stream: ")
  (aws-log-streams-get-log-event log-group log-stream))

(transient-define-prefix aws-logs-help-popup ()
  "AWS Logs Menu"
  ["Actions"
   ("RET" "Describe Log Group" aws-logs-describe-log-group)
   ("u" "Open Console URL" aws-logs-open-console-url)
   ("e" "Get Log Events" aws-logs-get-log-events)
   ("g" "Refresh Buffer" aws-logs-describe-log-groups-refresh)
   ("P" "Set AWS Profile" aws-set-profile)
   ("s" "Get Log Streams" aws-log-streams-from-line-under-cursor)
   ("q" "Service Overview" aws)])

(defvar aws-logs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-logs-describe-log-group)
    (define-key map (kbd "?")   'aws-logs-help-popup)
    (define-key map (kbd "u")   'aws-logs-open-console-url)
    (define-key map (kbd "e")   'aws-logs-get-log-events)
    (define-key map (kbd "g")   'aws-logs-describe-log-groups-refresh)
    (define-key map (kbd "P")   'aws-set-profile)
    (define-key map (kbd "q")   'aws)
    (define-key map (kbd "s")   'aws-log-streams-from-line-under-cursor)
    map))

;;;###autoload
(defun aws-logs ()
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "logs"))
  (aws-logs-mode))

(define-derived-mode aws-logs-mode tabulated-list-mode "aws-logs"
  "AWS mode"
  (setq major-mode 'aws-logs-mode)
  (use-local-map aws-logs-mode-map)
  (aws-logs-describe-log-groups))

(provide 'aws-logs)
;;; aws-logs.el ends here
