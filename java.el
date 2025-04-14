;; JAVA START
;; (defvar dt/local-dir (concat user-emacs-directory ".local/") "Local state directory")
;; (defvar lsp-java-workspace-dir (expand-file-name "lsp/workspace/data" dt/local-dir) "LSP data directory for Java")
;; (defvar java-home (format "%s/.sdkman/candidates/java/current" dt/home-dir) "The home dir of the jdk")
;; (defvar java-bin (format "%s/bin/java" java-home) "The path to the java binary")
;; (defvar jdtls-home "/opt/eclipse.jdt.ls" "The path to eclipse.jdt.ls installation")
;; (defvar jdtls-config (format "%s/config_linux" jdtls-home) "The path to eclipse.jdt.ls installation")

;; (defun dt/jdtls-start-command (arg)
;;   "Creates the command to start jdtls"
;;   (let ((jdtls-jar (replace-regexp-in-string "\n\\'" "" (shell-command-to-string (format "find %s/plugins -iname '*launcher_*.jar'" jdtls-home)) "The jar file that starts jdtls")))
;;     `(,java-bin "-jar" ,jdtls-jar "-data" ,(format "%s/.cache/lsp/project/%s" dt/home-dir (project-name (project-current))) "-configuration" ,jdtls-config
;;        "--add-modules=ALL-SYSTEM"
;;        "--add-opens java.base/java.util=ALL-UNNAMED"
;;        "--add-opens java.base/java.lang=ALL-UNNAMED"
;;        "-XX:+UseAdaptiveSizePolicy" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Xmx8G" "-Xms2G" "-Xverify:none")))

;; (defun dt/java-setup-project-workspace ()
;;   "Setup a local java workspace for the current project."
;;   (interactive)
;;   (let* ((project-root (project-root (project-current)))
;;           (file-path (concat project-root ".dir-locals.el"))
;;           (data-dir (concat (project-root (project-current)) ".lsp/workspace/data"))
;;           (cache-dir (concat (project-root (project-current)) ".lsp/workspace/cache"))
;;           (content '((java-mode
;;                        (eval . (progn
;;                                  (setq lsp-session-file (concat (project-root (project-current)) ".lsp/session")
;;                                    lsp-java-workspace-dir (concat (project-root (project-current)) ".lsp/workspace/data")
;;                                    lsp-java-workspace-cache-dir (concat (project-root (project-current)) ".lsp/workspace/cache"))))))))
;;     (make-directory data-dir t)
;;     (make-directory cache-dir t)
;;     (with-temp-buffer
;;       (setq-local enable-local-variables :all)
;;       (insert (format "%s\n" (pp-to-string content)))
;;       (write-file file-path))))

;; (defun dt/java-clear-project-workspace ()
;;   "Setup a local java workspace for the current project."
;;   (interactive)
;;   (let ((directory lsp-java-workspace-dir))
;;     (when (file-exists-p directory)
;;       (delete-directory directory 'recursive))
;;     (make-directory directory t)))

;; ;; mkdir -p ~/.emacs.d/.local/lsp/eclipse.jdt.ls
;; ;; pushd ~/.emacs.d/.local/lsp/eclipse.jdt.ls
;; ;; curl -s -L https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.37.0/jdt-language-server-1.37.0-202406271335.tar.gz | tar zxv
;; ;; mkdir bundles
;; ;; pushd bundles
;; ;; curl -O https://github.com/dgileadi/vscode-java-decompiler/raw/master/server/dg.jdt.ls.decompiler.cfr-0.0.3.jar
;; ;; curl -O https://github.com/dgileadi/vscode-java-decompiler/raw/master/server/dg.jdt.ls.decompiler.common-0.0.3.jar
;; ;; curl -O https://github.com/dgileadi/vscode-java-decompiler/raw/master/server/dg.jdt.ls.decompiler.fernflower-0.0.3.jar
;; ;; curl -O https://github.com/dgileadi/vscode-java-decompiler/raw/master/server/dg.jdt.ls.decompiler.procyon-0.0.3.jar
;; ;; popod
;; ;; popd

;; (use-package eglot-java
;;   :custom
;;   (eglot-java-eclipse-jdt-ls-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.37.0/jdt-language-server-1.37.0-202406271335.tar.gz")
;;   (eglot-java-server-install-dir (file-name-concat dt/local-dir "lsp" "eclipse.jdt.ls"))
;;   (eglot-java-eclipse-jdt-args '("-XX:+UseAdaptiveSizePolicy" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Xmx8G" "-Xms2G"))
;;   (eglot-java-eclipse-jdt-data-root-dir (file-name-concat dt/local-dir "lsp" "eclipse.jdt.ls" "data"))
;;   :init

;;   (defun jdtls-initialization-options ()
;;     (let ((setting-json-file (file-name-concat user-emacs-directory "config.json")))
;;       (with-temp-buffer
;;         (insert-file-contents setting-json-file)
;;         (json-parse-buffer :object-type 'plist :false-object :json-false))))

;;   :config
;;   ;; Override existing options
;;   (cl-defmethod eglot-initialization-options ((server eglot-java-eclipse-jdt))
;;     (jdtls-initialization-options))
;;   ;; eglot-java registers in 'project-find-functions a function that lookus up for .project
;;   (advice-add 'eglot-java--init :after (lambda() (remove-hook 'project-find-functions  #'eglot-java--project-try)))
;;   :hook (java-mode . eglot-java-mode))
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;     '(java-mode . ("jdtls"))))
;; (add-hook 'java-mode-hook 'eglot-ensure)
;; JAVA END
