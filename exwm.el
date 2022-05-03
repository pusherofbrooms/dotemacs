(require 'exwm)
(require 'exwm-config)
(exwm-config-example)

(start-process-shell-command "xmodmap" nil "xmodmap ~/.xmodmaprc")
