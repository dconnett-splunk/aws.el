;;; aws-ecr.el --- Emacs major modes wrapping the AWS CLI

;; Copyright (C) 2022-2025, Marcel Patzwahl

;; This file is NOT part of Emacs.

;; This  program is  free  software; you can redistribute it  and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Author: Marcel Patzwahl

;;; Commentary:

;; Emacs major modes wrapping the AWS CLI

;;; Code:
(require 'subr-x)
(require 'transient)

(defvar-local aws-ecr-images-current-repository nil)

(defconst aws-ecr--repositories-query
  "repositories[*].[repositoryName,repositoryUri,createdAt,imageTagMutability,imageScanningConfiguration.scanOnPush,encryptionConfiguration.encryptionType]"
  "JMESPath query used to render ECR repositories.")

(defconst aws-ecr--images-query
  "imageDetails[*].[imageDigest,imageTags[0],imagePushedAt,imageSizeInBytes,imageScanStatus.status]"
  "JMESPath query used to render ECR image details.")

(defun aws-ecr--quote (value)
  "Return VALUE quoted for use in an AWS CLI shell command."
  (shell-quote-argument value))

(defun aws-ecr--current-repository-name ()
  "Return the ECR repository name under the cursor."
  (or (tabulated-list-get-id)
      (user-error "No ECR repository on this row")))

(defun aws-ecr--current-image-digest ()
  "Return the ECR image digest under the cursor."
  (or (tabulated-list-get-id)
      (user-error "No ECR image on this row")))

(defun aws-ecr--list-repositories ()
  "List all ECR repositories."
  (fset 'aws--last-view 'aws-ecr)
  (aws-core--tabulated-list-from-command-multi-column
   (concat "ecr describe-repositories --output text --query '"
           aws-ecr--repositories-query
           "'")
   [("Repository" 36)
    ("URI" 72)
    ("Created" 24)
    ("Tag Mutability" 16)
    ("Scan On Push" 14)
    ("Encryption" 14)]))

(defun aws-ecr-list-repositories-refresh ()
  "Refresh the ECR repositories overview."
  (interactive)
  (aws-core--refresh-list-view 'aws-ecr--list-repositories))

(defun aws-ecr-describe-repository ()
  "Describe the ECR repository under the cursor."
  (interactive)
  (aws-core--describe-current-resource "ecr describe-repositories --repository-names"))

(defun aws-ecr--list-images (repository-name)
  "List image details for ECR REPOSITORY-NAME."
  (fset 'aws--last-view 'aws-ecr)
  (aws-core--tabulated-list-from-command-multi-column
   (concat "ecr describe-images --repository-name "
           (aws-ecr--quote repository-name)
           " --output text --query '"
           aws-ecr--images-query
           "'")
   [("Digest" 72)
    ("Tag" 32)
    ("Pushed" 24)
    ("Size" 14)
    ("Scan Status" 14)]))

(defun aws-ecr-list-images-refresh ()
  "Refresh the current ECR images buffer."
  (interactive)
  (unless aws-ecr-images-current-repository
    (user-error "No ECR repository is active"))
  (let ((current-line (aws-core--get-current-line)))
    (message "Refreshing buffer...")
    (aws-ecr--list-images aws-ecr-images-current-repository)
    (forward-line current-line)
    (message "Buffer refreshed")))

(defun aws-ecr-list-images ()
  "List image details for the ECR repository under the cursor."
  (interactive)
  (aws-ecr-images (aws-ecr--current-repository-name)))

(defun aws-ecr-describe-image ()
  "Describe the ECR image under the cursor."
  (interactive)
  (unless aws-ecr-images-current-repository
    (user-error "No ECR repository is active"))
  (let* ((image-digest (aws-ecr--current-image-digest))
         (buffer (concat (aws--buffer-name "ecr describe-images")
                         ": "
                         aws-ecr-images-current-repository
                         " "
                         image-digest
                         "*"))
         (cmd (concat (aws-cmd)
                      "--output "
                      aws-output
                      " ecr describe-images --repository-name "
                      (aws-ecr--quote aws-ecr-images-current-repository)
                      " --image-ids imageDigest="
                      (aws-ecr--quote image-digest))))
    (call-process-shell-command cmd nil buffer)
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (aws--get-view-mode))
    (goto-line 1)))

(transient-define-prefix aws-ecr-help-popup ()
  "AWS ECR Menu"
  ["Actions"
   ("RET" "Describe Repository" aws-ecr-describe-repository)
   ("i" "List Images" aws-ecr-list-images)
   ("g" "Refresh Buffer" aws-ecr-list-repositories-refresh)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws)])

(transient-define-prefix aws-ecr-images-help-popup ()
  "AWS ECR Images Menu"
  ["Actions"
   ("RET" "Describe Image" aws-ecr-describe-image)
   ("g" "Refresh Buffer" aws-ecr-list-images-refresh)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Repositories" aws-ecr)])

(defvar aws-ecr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-ecr-describe-repository)
    (define-key map (kbd "?") 'aws-ecr-help-popup)
    (define-key map (kbd "i") 'aws-ecr-list-images)
    (define-key map (kbd "g") 'aws-ecr-list-repositories-refresh)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

(defvar aws-ecr-images-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-ecr-describe-image)
    (define-key map (kbd "?") 'aws-ecr-images-help-popup)
    (define-key map (kbd "g") 'aws-ecr-list-images-refresh)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws-ecr)
    map))

;;;###autoload
(defun aws-ecr ()
  "Open ECR Mode."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "ecr"))
  (aws-ecr-mode))

(defun aws-ecr-images (repository-name)
  "Open ECR Images Mode for REPOSITORY-NAME."
  (aws--pop-to-buffer (aws--buffer-name (concat "ecr: " repository-name)))
  (aws-ecr-images-mode)
  (setq-local aws-ecr-images-current-repository repository-name)
  (aws-ecr--list-images repository-name))

(define-derived-mode aws-ecr-mode tabulated-list-mode "aws-ecr"
  "AWS ECR mode."
  (setq major-mode 'aws-ecr-mode)
  (use-local-map aws-ecr-mode-map)
  (aws-ecr--list-repositories))

(define-derived-mode aws-ecr-images-mode tabulated-list-mode "aws-ecr-images"
  "AWS ECR images mode."
  (setq major-mode 'aws-ecr-images-mode)
  (use-local-map aws-ecr-images-mode-map))

(provide 'aws-ecr)
;;; aws-ecr.el ends here
