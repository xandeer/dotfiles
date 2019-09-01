;;; private/xandeer/+eshell.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "eshell"             "e"   nil
      (:prefix ("e" . "eshell")
        :desc "Create a new eshell window below the current one."             "b"   #'+eshell/split-below
        :desc "Create a new eshell window to the right of the current one."   "r"   #'+eshell/split-right
        :desc "Switch to eshell and change directory to DIR."                 "z"   #'eshell-z))
(map!
   :n "M-;" #'eshell-command)
