(in-package css-lite)

;;simple usage
(css (("#foo") :bar "50px"))
"
#foo {
bar:50px;
}"

;; defining a css-variable manually

(setq my-css-var (list :inside-var "50px 30px"))
(setf (get 'my-css-var 'css-var) t)

;; using that css-variable
(css (("#foo") :bar "50px" my-css-var))
"
#foo {
bar:50px;
inside-var:50px 30px;
}"

;; now defining a css-variable with the make-css-var macro

(make-css-var my-macroed-css-var '(:border "1px solid red"))

;;using that variable
(css (("#foo") :bar "50px" my-css-var my-macroed-css-var))
"
#foo {
bar:50px;
inside-var:50px 30px;
border:1px solid red;
}"


;; now with cascading 
(css (
      ("#foo") (:bar "50px" my-css-var my-macroed-css-var)  
      (("#foo") (:bar "50px" my-css-var my-macroed-css-var))))
"
#foo {
bar:50px;
inside-var:50px 30px;
border:1px solid red;
}

#foo #foo {
bar:50px;
inside-var:50px 30px;
border:1px solid red;
}
"