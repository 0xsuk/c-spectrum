(pushnew (uiop:getcwd) ql:*local-project-directories*)
(ql:quickload :cl-spectrum)
(asdf:load-system :cl-spectrum)