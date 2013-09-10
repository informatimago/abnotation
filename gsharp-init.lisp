(in-package :gsharp)


(set-key `(com-erase-element ,*numeric-argument-marker*)
         'global-gsharp-table
         '((#\Backspace)))


(set-key `(esa:com-describe-bindings ,*numeric-argument-marker*)
         'global-gsharp-table
         '((#\h :control) #\m))




(define-gsharp-command (com-pjb :name t)
    ((argument 't :prompt "prefix"))
  (format *trace-output* "~S argument = ~S~%" 'com-pjb argument))

(set-key `(com-pjb ,*numeric-argument-marker*)
         'global-gsharp-table
         '((#\p)))



(define-gsharp-command (com-set-key :name t)
    ((command 'command :prompt "Command")
     (key     'list    :prompt "Key"))
  (set-key `(,command)
           'global-gsharp-table
           key))




;;(clim:find-command-table 'gsharp::cluster-table :errorp t)


