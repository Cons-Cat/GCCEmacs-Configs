(defgroup lsp-vlang
  "LSP for V"
  :group 'lsp-mode)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "~/sources/v/vlang_vls/cmd/vls/")
                  :major-modes '(vlang-mode)
                  :server-id 'vls))

(provide 'lsp-vlang)
