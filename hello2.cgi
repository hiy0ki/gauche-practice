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
      (html:p "hello, Gauche! 22222222"))))))

