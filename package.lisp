(cl:defpackage "CSS-LITE"
  (:use "COMMON-LISP"
        #:split-sequence)
  (:export #:*css-stream*
           #:*indent-css*
           #:css
           #:css-string
           #:inline-css
           #:css-id-name
           #:pt
           #:px
           #:make-css-var
           #:make-css-func
           #:comment
           #:%))
