(in-package #:cl-user)

(defpackage #:cl-emb.system
  (:use #:cl))

(in-package #:cl-emb.system)

(load (cl-user::loadfn "packages"))
(load (cl-user::loadfn "emb"))

#|
(defsystem #:cl-emb
    :version "0.4.3"
    :author "Stefan Scholl <stesch@no-spoon.de>"
    :licence "Lesser Lisp General Public License"
    :depends-on (#:cl-ppcre)
    :components ((:file "packages")
                 (:file "emb" :depends-on ("packages"))))
|#