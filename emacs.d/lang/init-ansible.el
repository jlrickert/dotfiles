;;; init-ansible.el --- ansible support
;;; Commentary:

;;; Code:
(use-package ansible
  :config
  (setq ansible::vault-password-file "~/.vpass")
  (add-hook 'yaml-mode-hook '(lambda() (ansible 1))))

(use-package company-ansible
  :after ansible
  :config
  (add-to-list 'company-backends 'company-ansible))

(provide 'init-ansible)
;;; init-ansible.el ends here
