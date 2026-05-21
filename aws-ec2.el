;;; aws-ec2.el --- Emacs major modes wrapping the AWS CLI

;; Copyright (C) 2022-2025, Marcel Patzwahl

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
(require 'term)
(require 'transient)

(defconst aws-ec2--instance-query
  "Reservations[].Instances[].[InstanceId,Tags[?Key==`Name`]|[0].Value,State.Name,InstanceType,PrivateIpAddress,PublicIpAddress,Placement.AvailabilityZone]"
  "JMESPath query used to render EC2 instances.")

(defun aws-ec2--list-instances ()
  "List all EC2 instances."
  (fset 'aws--last-view 'aws-ec2)
  (aws-core--tabulated-list-from-command-multi-column
   (concat "ec2 describe-instances --output=text --query '"
           aws-ec2--instance-query
           "'")
   [("Instance ID" 24)
    ("Name" 32)
    ("State" 14)
    ("Type" 14)
    ("Private IP" 16)
    ("Public IP" 16)
    ("AZ" 16)]))

(defun aws-ec2-list-instances-refresh ()
  "Refresh the EC2 Instances Overview and jump to the last position."
  (interactive)
  (aws-core--refresh-list-view 'aws-ec2--list-instances))

(defun aws-ec2--current-instance-id ()
  "Return the EC2 instance id under the cursor."
  (or (tabulated-list-get-id)
      (user-error "No EC2 instance on this row")))

(defun aws-ec2-describe-instance ()
  "Describe the EC2 instance under the cursor."
  (interactive)
  (aws-core--describe-current-resource "ec2 describe-instances --instance-ids"))

(defun aws-ec2--run-instance-command (command instance-id)
  "Run EC2 COMMAND for INSTANCE-ID and refresh the instances list."
  (let ((output (shell-command-to-string
                 (concat (aws-cmd)
                         "ec2 "
                         command
                         "-instances --instance-ids "
                         instance-id))))
    (aws-ec2-list-instances-refresh)
    (message (string-trim output))))

(defun aws-ec2-start-instance ()
  "Start the EC2 instance under the cursor."
  (interactive)
  (aws-ec2--run-instance-command "start" (aws-ec2--current-instance-id)))

(defun aws-ec2-stop-instance ()
  "Stop the EC2 instance under the cursor."
  (interactive)
  (let ((instance-id (aws-ec2--current-instance-id)))
    (when (yes-or-no-p (format "Stop EC2 instance %s? " instance-id))
      (aws-ec2--run-instance-command "stop" instance-id))))

(defun aws-ec2-reboot-instance ()
  "Reboot the EC2 instance under the cursor."
  (interactive)
  (let ((instance-id (aws-ec2--current-instance-id)))
    (when (yes-or-no-p (format "Reboot EC2 instance %s? " instance-id))
      (aws-ec2--run-instance-command "reboot" instance-id))))

(defun aws-ec2-terminate-instance ()
  "Terminate the EC2 instance under the cursor."
  (interactive)
  (let ((instance-id (aws-ec2--current-instance-id)))
    (when (yes-or-no-p (format "Terminate EC2 instance %s? " instance-id))
      (aws-ec2--run-instance-command "terminate" instance-id))))

(defun aws-ec2-start-ssm-session ()
  "Start an SSM session to the EC2 instance under the cursor."
  (interactive)
  (let* ((instance-id (aws-ec2--current-instance-id))
         (command (concat (aws-cmd)
                          "ssm start-session --target "
                          instance-id)))
    (ansi-term (or (getenv "SHELL") "/bin/sh")
               (format "aws-ssm:%s" instance-id))
    (term-send-raw-string (concat command "\n"))))

(transient-define-prefix aws-ec2-help-popup ()
  "AWS EC2 Menu"
  ["Actions"
   ("RET" "Describe Instance" aws-ec2-describe-instance)
   ("S" "Start Instance" aws-ec2-start-instance)
   ("s" "Stop Instance" aws-ec2-stop-instance)
   ("r" "Reboot Instance" aws-ec2-reboot-instance)
   ("T" "Terminate Instance" aws-ec2-terminate-instance)
   ("m" "Start SSM Session" aws-ec2-start-ssm-session)
   ("g" "Refresh Buffer" aws-ec2-list-instances-refresh)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws)])

(defvar aws-ec2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-ec2-describe-instance)
    (define-key map (kbd "?") 'aws-ec2-help-popup)
    (define-key map (kbd "S") 'aws-ec2-start-instance)
    (define-key map (kbd "s") 'aws-ec2-stop-instance)
    (define-key map (kbd "r") 'aws-ec2-reboot-instance)
    (define-key map (kbd "T") 'aws-ec2-terminate-instance)
    (define-key map (kbd "m") 'aws-ec2-start-ssm-session)
    (define-key map (kbd "g") 'aws-ec2-list-instances-refresh)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

;;;###autoload
(defun aws-ec2 ()
  "Open the EC2 Mode."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "ec2"))
  (aws-ec2-mode))

(define-derived-mode aws-ec2-mode tabulated-list-mode "aws-ec2"
  "AWS EC2 mode"
  (setq major-mode 'aws-ec2-mode)
  (use-local-map aws-ec2-mode-map)
  (aws-ec2--list-instances))

(provide 'aws-ec2)
;;; aws-ec2.el ends here
