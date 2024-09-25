;;; x-env.el --- env -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq exec-path
      (delete-dups
       (append
        exec-path
        '("/Applications/kitty.app/Contents/MacOS"
          "/Users/kevin/.nix-profile/bin"
          "/Users/kevin/.yarn/bin"
          "/Users/kevin/Library/Android/sdk/platform-tools"
          "/Users/kevin/bin"
          "/bin"
          "/etc/profiles/per-user/kevin/bin"
          "/nix/var/nix/profiles/default/bin"
          "/opt/homebrew/Cellar/emacs-plus@30/30.0.50/libexec/emacs/30.0.50/aarch64-apple-darwin21.6.0"
          "/opt/homebrew/bin"
          "/run/current-system/sw/bin"
          "/sbin"
          "/usr/bin"
          "/usr/local/bin"
          "/usr/sbin"))))

(provide 'x-env)
;;; x-env.el ends here
