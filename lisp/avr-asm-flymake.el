;;; avr-asm-flymake.el --- flymake minor mode for AVR assembler (Atmel AVR assembler)

;; Copyright (C) 2013  Matthew Kennedy

;; Author: Matthew Kennedy <burnsidemk@gmail.com>
;; Keywords: languages, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'flymake)

(defvar avr-asm-flymake-command
  (if (eq window-system 'w32)
      "C:/Program Files (x86)/Atmel/Atmel Studio 6.0/extensions/Atmel/AVRAssembler/2.1.51.64/avrassembler/avrasm2.exe"
    "avra"))

(defvar avr-asm-flymake-command-arguments
  (if (eq window-system 'w32)
      (list "-f-")
    (list "-o" "/dev/null")))

(defun avr-asm-flymake-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
         (local-file  (file-relative-name temp-file (file-name-directory buffer-file-name))))
    (list avr-asm-flymake-command (append avr-asm-flymake-command-arguments (list local-file)))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.\\(asm\\|inc\\)\\'" avr-asm-flymake-init))
(add-to-list 'flymake-err-line-patterns '("^\\(.*\\)(\\([0-9]+\\))\\ +:\\ +Error\\ +:\\ +\\(.*\\)" 1 2 nil 3))

(provide 'avr-asm-flymake)

;;; avr-asm-flymake.el ends here
