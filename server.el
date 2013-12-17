(require 'elnode)
(require 'f)

(defgroup htcpcp nil
 "The Hypertext Coffee Pot Control Protocol."
 :group 'applications)

(defconst htcpcp-gpio-path "/home/mad/test/gpio"
  "The base path of the GPIO sys interface.")

(defcustom htcpcp-brew-start-gpio "17"
 "Which GPIO port is used to send the command."
 :group 'htcpcp)

(defcustom htcpcp-brew-brewstate-gpio "18"
 "Which GPIO port is used to get the brewing state."
 :group 'htcpcp)

(defcustom htcpcp-brew-readystate-gpio "4"
 "Which GPIO port shows the ready state."
 :group 'htcpcp)

(defun htcpcp--init-gpios ()
 (f-write-text htcpcp-brew-start-gpio 'utf-8 (format "%s/%s" htcpcp-gpio-path "export"))
 (f-write-text htcpcp-brew-brewstate-gpio 'utf-8 (format "%s/%s" htcpcp-gpio-path "export"))
 (f-write-text htcpcp-brew-readystate-gpio 'utf-8 (format "%s/%s" htcpcp-gpio-path "export"))
 (f-write-text "out" 'utf-8 (format "%s/gpio%s/%s" htcpcp-gpio-path htcpcp-brew-start-gpio "direction"))
 (f-write-text "1" 'utf-8 (format "%s/gpio%s/%s" htcpcp-gpio-path htcpcp-brew-start-gpio "active_low"))
 (f-write-text "in" 'utf-8 (format "%s/gpio%s/%s" htcpcp-gpio-path htcpcp-brew-readystate-gpio "direction"))
 (f-write-text "in" 'utf-8 (format "%s/gpio%s/%s" htcpcp-gpio-path htcpcp-brew-brewstate-gpio "direction"))
)

;; Fixing elnode for proper coffee
(defun elnode--http-parse-status (httpcon &optional property)
  "Parse the status line of HTTPCON.

If PROPERTY is non-nil, then return that property."
  (let ((http-line (process-get httpcon :elnode-http-status)))
    (string-match
     "\\(GET\\|POST\\|BREW\\|WHEN\\|HEAD\\) \\(.*\\) HTTP/\\(1.[01]\\)"
     http-line)
    (process-put httpcon :elnode-http-method (match-string 1 http-line))
    (process-put httpcon :elnode-http-resource (match-string 2 http-line))
    (process-put httpcon :elnode-http-version (match-string 3 http-line))
    (if property
        (process-get httpcon property))))


(defun htcpcp--send-teapot (httpcon)
  "Sends the I'm a Teapot message."
  (elnode-send-status httpcon 418 "I'm a teapot!")
)

(defun htcpcp--do-brew (httpconn)
  "Just does the brewing."
  (f-write-text "1" 'utf-8 (format "%s/gpio%s/%s" htcpcp-gpio-path htcpcp-brew-brewstate-gpio "value"))
  (sleep-for 0 200)
  (f-write-text "0" 'utf-8 (format "%s/gpio%s/%s" htcpcp-gpio-path htcpcp-brew-brewstate-gpio "value"))
  (elnode-send-status httpcon 200 "I'm a coffeepot, brewing!")
)

(defun htcpcp--handler (httpcon)
 " the brewing connection handler "
 (princ "U-uh!")
 (let ((m (elnode-http-method httpcon)))
 (princ "Aha.")
 (cond ((or (equal m "BREW") (equal m "POST"))
	(princ "Braue braue manche Strecke...")
	(htcpcp--do-brew httpcon)
       ((equal m "GET") 
	(princ "Der alte Hexenmeister ist hier!")
	(elnode-send-status httpcon 200 "I'm a coffeepot, short and stout."))
      (t 
	(htcpcp--send-teapot httpcon))
  )))
)

(defun* htcpcp-start (&key port (host "localhost"))
  " Start the htcpcp server."
  (interactive
   (let ((port (read-number "Port: " 8000))
	 (host (read-string "Host: " "localhost")))
     ))
  (htcpcp--init-gpios)
  (let ((port (or port 8000))
	(host (or host "localhost")))
    (elnode-start 'htcpcp--handler :port port :host host)))

(provide 'htcpcp)
