#!/usr/local/bin/gosh

(use www.cgi)
(use text.html-lite)

(cgi-main
 (lambda (params)
   (list
    (cgi-header)
    (html-doctype)
    (html:html
     (html:body
      (html:p "hello, Gauche! 22222222")
      (html:p (html-escape-string params))
      (html:p (html-escape-string
               (cgi-get-parameter "y" params)))
      (html:p (html-escape-string
               (cgi-get-parameter "y" params :list #t)))
      (html:p (html-escape-string
               (cgi-get-parameter "y" params :convert x->integer))))))))

