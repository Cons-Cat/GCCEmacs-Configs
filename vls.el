(require 'lsp-mode)

(defgroup lsp-vlang nil
  "LSP for V"
  :group 'lsp-mode)

(defcustom lsp-v-server-path "vls/vls"
  "Executable path for the server."
  :type 'string
  :package-version '(lsp-mode . "7.1")
  )

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-v-server-path))
                  :major-modes '(vlang-mode)
                  :server-id 'vls))

(provide 'lsp-vlang)
