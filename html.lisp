(define-syntax tag
(syntax-rules ()
   ((tag . rest)
      (list ">" . rest))))

(define-syntax span
(syntax-rules (- id class)
   ((span - (id value) . rest)
      (list
         " id=\""value"\""
         (span - . rest)))
   ((span - (class value) . rest)
      (list
         " class=\""value"\""
         (span - . rest)))

   ((span - . rest)
      (list ">" . rest))
   ((span . rest)
      (list
         "<span"
            (span - . rest)
         "</span>"))))

(define-syntax fieldset
(syntax-rules ()
   ((fieldset legend rest...)
      (list
         "<fieldset>" "<legend>"legend"</legend>"
           rest...
         "</fieldset>"))))

(define-syntax section
(syntax-rules (- class)
   ((section - (class value) . rest)
      (list
         " class=\""value"\""
         (section - . rest)))

   ((section - . rest)
      (list ">" . rest))
   ((section . rest)
      (list
         "<section"
            (section - . rest)
         "</section>"))))


(define-syntax form
(syntax-rules (- id action method accept-charset onsubmit name)
   ((form - (id value...) . rest)
      (list
         " id=\""value..."\""
         (form - . rest)))
   ((form - (action value...) . rest)
      (list
         " action=\""value..."\""
         (form - . rest)))
   ((form - (name value...) . rest)
      (list
         " name=\""value..."\""
         (form - . rest)))
   ((form - (onsubmit value...) . rest)
      (list
         " onsubmit=\""value..."\""
         (form - . rest)))

   ((form - . rest)
      (list ">" . rest))
   ((form . rest)
      (list
         "<form"
         " method='get'"
         " accept-charset='utf-8'"
            (form - . rest)
         "</form>"))))

(define-syntax a
(syntax-rules (- id href style)
   ((a - (id value...) . rest)
      (list
         " id=\""value..."\""
         (a - . rest)))
   ((a - (href value...) . rest)
      (list
         " href=\""value..."\""
         (a - . rest)))
   ((a - (style value...) . rest)
      (list
         " style=\""value..."\""
         (a - . rest)))

   ((a - . rest)
      (list ">" . rest))
   ((a . rest)
      (list
         "<a"
            (a - . rest)
         "</a>"))))

(define-syntax ul
(syntax-rules ()
   ((ul rest...)
      (list
         "<ul>"
           rest...
         "</ul>"))))

(define-syntax br
(syntax-rules ()
   ((br)
      "<br>")))
(define-syntax hr
(syntax-rules ()
   ((hr)
      "<hr>")))

(define-syntax li
(syntax-rules (- style)
   ((li - (style value) rest...)
      (list
         " style=\""value"\""
         (li - rest...)))

   ((li - rest...)
      (list ">" rest...))
   ((li rest...)
      (list
         "<li"
           (li - rest...)
         "</li>"))))


(define-syntax select
(syntax-rules (- id name)
   ((select - (id value...) rest...)
      (list
         " id=\""value..."\""
         (select - rest...)))
   ((select - (name value...) rest...)
      (list
         " name=\""value..."\""
         (select - rest...)))

   ((select - rest...)
      (list ">" rest...))
   ((select rest...)
      (list
         "<select"
           (select - rest...)
         "</select>"))))
(define-syntax option
(syntax-rules (- value)
   ((option - (value arg...) rest...)
      (list
         " value=\""arg..."\""
         (option - rest...)))
   ((option - rest...)
      (list ">" rest...))
   ((option rest...)
      (list
         "<option"
            (option - rest...)
         "</option>"))))



(define-syntax label
(syntax-rules ()
   ((label rest...)
      (list
         "<label>"
            rest...
         "</label>"))))

(define-syntax input
(syntax-rules (- id type name placeholder required readonly value)
   ((input - (id arg) rest...)
      (list
         " id=\""arg"\""
         (input - rest...)))
   ((input - (type arg) rest...)
      (list
         " type=\""arg"\""
         (input - rest...)))
   ((input - (name arg) rest...)
      (list
         " name=\""arg"\""
         (input - rest...)))
   ((input - (placeholder arg) rest...)
      (list
         " placeholder=\""arg"\""
         (input - rest...)))
   ((input - (required) rest...)
      (list
         " required"
         (input - rest...)))
;   ((input - (readonly is) rest...)
;      (list
;         (if is " readonly")
;         (input - rest...)))
   ((input - (readonly) rest...)
      (list
         " readonly"
         (input - rest...)))
   ((input - (value arg) rest...)
      (list
         " value=\""arg"\""
         (input - rest...)))

   ((input - rest...)
      (list ">" rest...))
   ((input rest...)
      (list
         "<input"
           (input - rest...)))))



(define-syntax table
(syntax-rules ()
   ((table rest...)
      (list
         "<table>"
           rest...
         "</table>"))))
(define-syntax tr
(syntax-rules ()
   ((tr rest...)
      (list
         "<tr>"
           rest...
         "</tr>"))))
(define-syntax th
(syntax-rules ()
   ((th rest...)
      (list
         "<th>"
           rest...
         "</th>"))))
(define-syntax td
(syntax-rules (- colspan style)
   ((td - (style value) rest...)
      (list
         " style="value
         (td - rest...)))
   ((td - (colspan value) rest...)
      (list
         " colspan="value
         (td - rest...)))

   ((td - rest...)
      (list ">" rest...))
   ((td rest...)
      (list
         "<td"
           (td - rest...)
         "</td>"))))


(define-syntax script
(syntax-rules ()
   ((script rest...)
      (list
         "<script>"
           rest...
         "</script>"))))








; ------------------------------------------
(define-syntax html-200
(syntax-rules (- stylesheet javascript html)
   ((html-200 - (stylesheet value) rest...)
      (list
         "   <link rel='stylesheet' href='"value"'>\n"
         (html-200 - rest...)))
   ((html-200 - (javascript value) rest...)
      (list
         "   <script src='"value"'> </script>\n"
         (html-200 - rest...)))

   ((html-200 - rest...)
      (list
         "</head><body>"
         rest...
         "</body></html>"))
   ((html-200 title rest...)
      (html
         "HTTP/1.0 200 OK\n"
         "Connection: close\n"
         "Content-Type: text/html\n"
         "Server: " (car *version*) "/" (cdr *version*) "\n"
         "\n"
         "<!DOCTYPE html>\n"
         "<html>\n"
         "<head>\n"
         "   <title>" title "</title>\n"
         "   <link rel='stylesheet' href='/stylesheets/normalize.css'>\n"
         "   <script src='/javascripts/jquery-2.1.1.min.js'> </script>\n"
         "   <script src='/javascripts/stars!-1.0.0.js'> </script>\n"
         (html-200 - rest...)))))

;(define-syntax SCREEN
;(syntax-rules (al account session args)
;   ((SCREEN url argc rest...)
;      ((starts-with al url)
;         (let*((account session args (parse al argc)))
;            rest...)))))

(define-syntax head
(syntax-rules (- stylesheet javascript html)
   ((head - (stylesheet value) rest...)
      (list
         "   <link rel='stylesheet' href='"value"'>\n"
         (head - rest...)))
   ((head - (javascript value) rest...)
      (list
         "   <script src='"value"'> </script>\n"
         (head - rest...)))

   ((head - rest...)
      (list
         rest...
         "</head>"))
   ((head title rest...)
      (html
         "<!DOCTYPE html>\n"
         "<html>\n"
         "<head>\n"
         "   <title>" title "</title>\n"
         "   <link rel='stylesheet' href='/stylesheets/normalize.css'>\n"
         "   <script src='/javascripts/jquery-2.1.1.min.js'> </script>\n"
         "   <link rel='stylesheet' href='/stylesheets/form.css'>\n"
         "   <script src='/javascripts/stars!-1.0.0.js'></script>\n"
         (head - rest...)))))
(define-syntax body
(syntax-rules (html)
   ((body rest...)
      (html
         "<body>"
            rest...
         "</body></html>"))))
