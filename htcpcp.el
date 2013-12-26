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

(defun htcpcp--send-status (httpcon status)
  "Translates the symbols into a http status and sends it."
  (pcase status 
    (`brewing ((elnode-send-status httpcon 200 "Brewing")))
    (`needrefill ((elnode-send-status httpcon 404 "Coffee or water not found, call operator under DECT 2788!")))
    (`ready ((elnode-send-status httpcon 200 "Coffeepot ready!")))
    (_ ((htcpcp--send-teapot httpcon)))
    )
)


(defun htcpcp--status ()
  "Gets the hardware state. Can be 'ready' 'brewing' or 'refill'.
   If it is 'teapot', something went wrong."
  (let ((r (htcpcp--get-ready-state))
	(b (htcpcp--get-brew-state)))
    (cond ((and (equal r "1") (equal b "0")) 'ready)
	  ((and (equal r "0") (equal b "0")) 'needrefill)
	  ((and (equal r "0") (equal b "1")) 'brewing)
	  (t 'teapot)
	  )
))

(defun htcpcp--get-ready-state ()
  "Get the hardware ready state. "
  (substring (f-read-bytes (format "%s/gpio%s/%s" htcpcp-gpio-path htcpcp-brew-readystate-gpio "value")) 0 1)
)

(defun htcpcp--get-brew-state ()
  "Get the hardware brew state. "
  (substring (f-read-bytes (format "%s/gpio%s/%s" htcpcp-gpio-path htcpcp-brew-brewstate-gpio "value")) 0 1)
)

(defun htcpcp--do-brew ()
  "Just does the brewing."
  ;; needs to pull the gpio to 1 for at least 100ms
  (f-write-text "1" 'utf-8 (format "%s/gpio%s/%s" htcpcp-gpio-path htcpcp-brew-brews-start-gpio "value"))
  (sleep-for 0 200)
  (f-write-text "0" 'utf-8 (format "%s/gpio%s/%s" htcpcp-gpio-path htcpcp-brew-brew-start-gpio "value"))
)

(defun htcpcp--handler (httpcon)
 " the brewing connection handler "
 (let ((m (elnode-http-method httpcon)))
   (cond ((or (equal m "BREW") (equal m "POST"))
	  (htcpcp--do-brew)
	  (htcpcp--send-status httpcon 200 (htcpcp--status)))
	 ((equal m "GET")
	  (let ((s (htcpcp--status)))
	  (htcpcp--send-status httpcon s)))
	 ((t 
	   (htcpcp--send-teapot httpcon)))
	 )))


(defun* htcpcp-start (&key port (host "localhost"))
  " Start the htcpcp server."
  (interactive
   (let ((port (read-number "Port: " 8000))
	 (host (read-string "Host: " "localhost")))
     (list :port port :host host)))
  (htcpcp--init-gpios)
  (let ((port (or port 8000))
	(host (or host "localhost")))
    (elnode-start 'htcpcp--handler :port port :host host)))

(provide 'htcpcp)
