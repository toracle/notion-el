(defvar *notion-workspace-conf*
  nil)


(defvar *notion-current-workspace-name*
  nil)


(defvar *notion-workspace-conf-path*
  (concat user-emacs-directory "notion-conf.el"))


(defun notion-list-workspace-names (&optional conf)
  "Return list of registered notion workspaces."
  (let ((workspaces (if conf conf
                      *notion-workspace-conf*)))
    (mapcar 'car workspaces)))


(defun notion-select-workspace (workspace-name)
  "Use a workspace that has WORKSPACE-NAME."
  (setq *notion-current-workspace-name* workspace-name))


(defun notion-current-workspace-conf ()
  (when *notion-current-workspace-name*
    (cdr (assoc *notion-current-workspace-name* *notion-workspace-conf*))))


(defun notion-add-workspace-conf (workspace-name secret-token)
  "Create a new workspace with WORKSPACE-NAME and SECRET-TOKEN."
  (add-to-list '*notion-workspace-conf* (cons workspace-name
                                              (list (cons :secret-key secret-token)
                                                    (cons :workspace-name workspace-name)))))


(defun notion-save-workspace-conf (&optional path)
  "Save notion workspace configuration to a config file."
  (with-current-buffer (find-file-noselect (if path
                                               path
                                             *notion-workspace-conf-path*))
    (erase-buffer)
    (insert (format "(setq *notion-workspace-conf* %s)" *notion-workspace-conf*))
    (newline)
    (save-buffer)))


(defun notion-load-workspace-conf (&optional path)
  "Load notion workspace configuration from a config file."
  (let ((p (if path path
             *notion-workspace-conf-path*)))
    (when (file-readable-p p)
      (load p))))
