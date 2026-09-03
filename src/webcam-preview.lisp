;;;; SPDX-FileCopyrightText: Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;; webcam-preview.lisp
;;;;
;;;; Pre-record webcam preview (green-screen-1lb). Before recording with a live
;;;; webcam, takesy serves a tiny local web page so the user can pick the input
;;;; camera and frame themselves (zoom/pan) in the PiP viewport. takesy owns the
;;;; camera via v4l2 (ffmpeg keeps one preview JPEG fresh with -update 1) and the
;;;; browser just displays it and posts the chosen device + framing back -- so no
;;;; getUserMedia, no camera permission, no browser<->/dev/video mapping problem.
;;;;
;;;; Minimal hand-rolled HTTP/1.1 over sb-bsd-sockets (matching takesy's low-dep
;;;; grain); form-encoded POSTs, no JSON. The browser <canvas> reproduces the PiP
;;;; shader's exact source-sampling math, so what you frame is what you record.

(defpackage #:takesy/webcam-preview
  (:use #:cl)
  (:local-nicknames (#:wc #:takesy/webcam))
  (:export #:run-preview))

(in-package #:takesy/webcam-preview)

;;; ------------------------------------------------------------------
;;; Byte/string + HTTP helpers.

(defun %s->bytes (s) (sb-ext:string-to-octets s :external-format :utf-8))
(defun %bytes->s (v) (sb-ext:octets-to-string (coerce v '(vector (unsigned-byte 8)))
                                              :external-format :utf-8))

(defun %read-line-bytes (stream)
  "Read a CRLF/LF-terminated line as a string (latin-1), or NIL at EOF."
  (let ((buf (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream nil nil) do
      (cond ((null b) (return (when (plusp (length buf)) (%bytes->s buf))))
            ((= b 10) (when (and (plusp (length buf))
                                 (= (aref buf (1- (length buf))) 13))
                        (decf (fill-pointer buf)))
                      (return (%bytes->s buf)))
            (t (vector-push-extend b buf))))))

(defun %read-n (stream n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (read-sequence v stream) v))

(defun %url-decode (s)
  "Decode application/x-www-form-urlencoded token."
  (with-output-to-string (out)
    (loop with i = 0 while (< i (length s)) do
      (let ((c (char s i)))
        (cond ((char= c #\+) (write-char #\Space out) (incf i))
              ((and (char= c #\%) (< (+ i 2) (length s)))
               (write-char (code-char (parse-integer s :start (1+ i) :end (+ i 3) :radix 16)) out)
               (incf i 3))
              (t (write-char c out) (incf i)))))))

(defun %parse-form (body)
  "Parse a form-encoded body string into an alist of (key . value)."
  (when body
    (loop for pair in (uiop:split-string body :separator '(#\&))
          for eq = (position #\= pair)
          when eq collect (cons (subseq pair 0 eq) (%url-decode (subseq pair (1+ eq)))))))

(defun %num (alist key default)
  (let ((c (assoc key alist :test #'string=)))
    (or (and c (ignore-errors (let ((*read-eval* nil)) (float (read-from-string (cdr c)) 1.0))))
        default)))

(defun %respond (stream status ctype body &optional extra)
  "Write an HTTP response. BODY is a byte vector or a string."
  (let ((b (if (stringp body) (%s->bytes body) body))
        (crlf (coerce (list (code-char 13) (code-char 10)) 'string)))
    (flet ((line (fmt &rest args) (write-sequence (%s->bytes (apply #'format nil fmt args)) stream)
             (write-sequence (%s->bytes crlf) stream)))
      (line "HTTP/1.1 ~A" status)
      (line "Content-Type: ~A" ctype)
      (line "Content-Length: ~D" (length b))
      (line "Connection: close")
      (dolist (h extra) (line "~A" h))
      (write-sequence (%s->bytes crlf) stream)
      (write-sequence b stream))))

;;; ------------------------------------------------------------------
;;; Preview state.

(defstruct pv cameras dir jpg device proc lock (result nil) (done nil) deadline)

(defun %preview-cmd (dev jpg)
  ;; Prefer MJPEG so the preview shows the real (full) framerate, matching capture.
  (append (list "ffmpeg" "-y" "-loglevel" "error" "-f" "v4l2")
          (wc:v4l2-input-args dev)
          (list "-framerate" "15" "-i" dev "-vf" "scale=480:-1"
                "-q:v" "6" "-update" "1" jpg)))

(defun %start-ffmpeg (pv dev)
  (sb-thread:with-mutex ((pv-lock pv))
    (when (pv-proc pv) (ignore-errors (uiop:terminate-process (pv-proc pv) :urgent t)))
    (setf (pv-device pv) dev
          (pv-proc pv) (ignore-errors
                        (uiop:launch-program (%preview-cmd dev (pv-jpg pv))
                                             :output nil :error-output nil)))))

(defun %stop-ffmpeg (pv)
  (sb-thread:with-mutex ((pv-lock pv))
    (when (pv-proc pv)
      (ignore-errors (uiop:terminate-process (pv-proc pv) :urgent t))
      (setf (pv-proc pv) nil))))

;;; ------------------------------------------------------------------
;;; The page. A <canvas> reproduces the PiP shader's cover-fit + zoom + pan so the
;;; preview is exactly what will be recorded. Drag = pan, wheel = zoom.

(defun %page (pv)
  (with-output-to-string (h)
    (format h "<!doctype html><html><head><meta charset=utf-8>~
<title>takesy webcam framing</title><style>~
body{background:#15171c;color:#e6e6e6;font:15px/1.4 system-ui,sans-serif;margin:0;~
display:flex;flex-direction:column;align-items:center;gap:14px;padding:22px}~
h1{font-size:17px;font-weight:600;margin:2px}~
#stage{width:360px;height:360px;border-radius:50%;overflow:hidden;background:#000;~
box-shadow:0 6px 30px #0008;cursor:grab;touch-action:none}~
#stage:active{cursor:grabbing}canvas{display:block}~
.row{display:flex;gap:10px;align-items:center}~
select,button{font:inherit;padding:8px 12px;border-radius:8px;border:1px solid #3a3f48;~
background:#232830;color:#e6e6e6}button{cursor:pointer}~
button.go{background:#2d6cdf;border-color:#2d6cdf;font-weight:600}~
.hint{color:#9aa3af;font-size:13px}</style></head><body>~
<h1>Frame your webcam</h1>~
<div class=row><label>Camera:</label><select id=cam>")
    (dolist (c (pv-cameras pv))
      (format h "<option value=\"~A\"~:[~; selected~]>~A (~A)</option>"
              (car c) (string= (car c) (pv-device pv)) (cdr c) (car c)))
    (format h "</select></div>~
<div id=stage><canvas id=cv width=360 height=360></canvas></div>~
<div class=hint>drag to pan &middot; scroll to zoom</div>~
<div class=row><button id=reset>Reset</button>~
<button class=go id=go>Start recording</button>~
<button id=cancel>Cancel</button></div>~
<script>
var cv=document.getElementById('cv'),ctx=cv.getContext('2d'),C=360;
var img=new Image(),loaded=false,zoom=1,pan={x:0,y:0},dev=document.getElementById('cam').value;
img.onload=function(){loaded=true;draw()};
function poll(){img.src='/frame?t='+Date.now()} setInterval(poll,100);poll();
function srcRect(){var iw=img.naturalWidth||16,ih=img.naturalHeight||9,a=iw/ih;
 var sw=(a>1?1/a:1)/zoom, sh=(a>1?1:a)/zoom;               // matches +fs-webcam+
 var cx=0.5-pan.x, cy=0.5-pan.y;
 return [(cx-sw/2)*iw,(cy-sh/2)*ih,sw*iw,sh*ih];}
function draw(){ctx.fillStyle='#000';ctx.fillRect(0,0,C,C);
 if(loaded){var r=srcRect();try{ctx.drawImage(img,r[0],r[1],r[2],r[3],0,0,C,C);}catch(e){}}}
var drag=null;
cv.addEventListener('pointerdown',function(e){drag={x:e.clientX,y:e.clientY};cv.setPointerCapture(e.pointerId)});
cv.addEventListener('pointermove',function(e){if(!drag)return;
 var iw=img.naturalWidth||16,ih=img.naturalHeight||9,a=iw/ih;
 var sw=(a>1?1/a:1)/zoom, sh=(a>1?1:a)/zoom;
 pan.x+=(e.clientX-drag.x)/C*sw; pan.y+=(e.clientY-drag.y)/C*sh;
 pan.x=Math.max(-0.5,Math.min(0.5,pan.x));pan.y=Math.max(-0.5,Math.min(0.5,pan.y));
 drag={x:e.clientX,y:e.clientY};draw()});
cv.addEventListener('pointerup',function(){drag=null});
cv.addEventListener('wheel',function(e){e.preventDefault();
 zoom*=(e.deltaY<0?1.08:1/1.08);zoom=Math.max(1,Math.min(8,zoom));draw()},{passive:false});
document.getElementById('cam').onchange=function(e){dev=e.target.value;
 fetch('/select',{method:'POST',headers:{'Content-Type':'application/x-www-form-urlencoded'},body:'dev='+encodeURIComponent(dev)});
 loaded=false};
document.getElementById('reset').onclick=function(){zoom=1;pan={x:0,y:0};draw()};
document.getElementById('cancel').onclick=function(){fetch('/cancel',{method:'POST'}).then(function(){msg('Cancelled - you can close this tab.')})};
document.getElementById('go').onclick=function(){
 var b='dev='+encodeURIComponent(dev)+'&zoom='+zoom.toFixed(4)+'&panx='+pan.x.toFixed(4)+'&pany='+pan.y.toFixed(4);
 fetch('/done',{method:'POST',headers:{'Content-Type':'application/x-www-form-urlencoded'},body:b}).then(function(){msg('Recording - you can close this tab.')})};
function msg(t){document.body.innerHTML='<h1 style=\"margin-top:40px\">'+t+'</h1>'}
</script></body></html>")))

;;; ------------------------------------------------------------------
;;; Request routing + server.

(defun %route (pv stream method path body)
  (let ((route (subseq path 0 (or (position #\? path) (length path)))))
    (cond
      ((and (string= method "GET") (string= route "/"))
       (%respond stream "200 OK" "text/html; charset=utf-8" (%page pv)))
      ((and (string= method "GET") (string= route "/frame"))
       (let ((bytes (sb-thread:with-mutex ((pv-lock pv))
                      (ignore-errors
                       (when (probe-file (pv-jpg pv))
                         (with-open-file (s (pv-jpg pv) :element-type '(unsigned-byte 8))
                           (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                             (read-sequence v s) v)))))))
         (if (and bytes (plusp (length bytes)))
             (%respond stream "200 OK" "image/jpeg" bytes '("Cache-Control: no-store"))
             (%respond stream "503 Service Unavailable" "text/plain" "no frame yet"))))
      ((and (string= method "POST") (string= route "/select"))
       (let ((dev (cdr (assoc "dev" (%parse-form body) :test #'string=))))
         (when dev (%start-ffmpeg pv dev)))
       (%respond stream "200 OK" "text/plain" "ok"))
      ((and (string= method "POST") (string= route "/done"))
       (let* ((f (%parse-form body))
              (dev (cdr (assoc "dev" f :test #'string=))))
         (setf (pv-result pv) (list :device dev
                                    :zoom (max 1.0 (%num f "zoom" 1.0))
                                    :pan-x (%num f "panx" 0.0)
                                    :pan-y (%num f "pany" 0.0))
               (pv-done pv) t))
       (%respond stream "200 OK" "text/plain" "ok"))
      ((and (string= method "POST") (string= route "/cancel"))
       (setf (pv-done pv) t)
       (%respond stream "200 OK" "text/plain" "ok"))
      (t (%respond stream "404 Not Found" "text/plain" "not found")))))

(defun %handle (pv conn)
  (unwind-protect
       (ignore-errors
        (let ((stream (sb-bsd-sockets:socket-make-stream
                       conn :input t :output t :element-type '(unsigned-byte 8)
                            :buffering :full)))
          (let ((req (%read-line-bytes stream)))
            (when req
              (let* ((parts (uiop:split-string req :separator '(#\Space)))
                     (method (first parts)) (path (second parts))
                     (clen 0))
                (loop for hl = (%read-line-bytes stream)
                      while (and hl (plusp (length hl)))
                      do (let ((c (position #\: hl)))
                           (when (and c (string-equal (subseq hl 0 c) "content-length"))
                             (setf clen (or (ignore-errors
                                             (parse-integer hl :start (1+ c) :junk-allowed t))
                                            0)))))
                (let ((bodystr (when (and (string= method "POST") (plusp clen))
                                 (%bytes->s (%read-n stream clen)))))
                  (%route pv stream method path bodystr)))))
          (ignore-errors (finish-output stream))))
    (ignore-errors (sb-bsd-sockets:socket-close conn))))

(defun %open-url (url)
  (ignore-errors (uiop:launch-program (list "xdg-open" url)
                                      :output nil :error-output nil)))

(defun run-preview (&key (cameras (wc:list-cameras)) initial-device (timeout 300))
  "Serve the framing page, open it in the browser, and block until the user starts
or cancels (or TIMEOUT seconds). Return a plist (:device :zoom :pan-x :pan-y) on
start, or NIL on cancel/timeout. CAMERAS is a list of (path . name)."
  (when (null cameras)
    (format t "  [preview] no cameras found; skipping preview~%")
    (return-from run-preview nil))
  (let* ((dir (format nil "/tmp/takesy-preview-~A" (sb-unix:unix-getpid)))
         (jpg (format nil "~A/frame.jpg" dir))
         (dev (or initial-device
                  (ignore-errors (wc:resolve-device "auto"))
                  (car (first cameras))))
         (pv (make-pv :cameras cameras :dir dir :jpg jpg :device dev
                      :lock (sb-thread:make-mutex :name "takesy-preview")
                      :deadline (+ (get-internal-real-time)
                                   (* timeout internal-time-units-per-second))))
         (sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (ensure-directories-exist (concatenate 'string dir "/"))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
    (sb-bsd-sockets:socket-bind sock #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen sock 16)
    (%start-ffmpeg pv dev)
    (unwind-protect
         (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name sock)
           (declare (ignore addr))
           (let ((url (format nil "http://127.0.0.1:~D/" port)))
             (format t "  [preview] open ~A to pick a camera and frame yourself~%" url)
             (%open-url url)
             (let ((accept
                     (sb-thread:make-thread
                      (lambda ()
                        (loop until (pv-done pv) do
                          (let ((conn (ignore-errors (sb-bsd-sockets:socket-accept sock))))
                            (when conn
                              (sb-thread:make-thread (lambda () (%handle pv conn))
                                                     :name "takesy-preview-conn")))))
                      :name "takesy-preview-accept")))
               (loop until (or (pv-done pv) (> (get-internal-real-time) (pv-deadline pv)))
                     do (sleep 0.2))
               (setf (pv-done pv) t)
               (ignore-errors (sb-bsd-sockets:socket-close sock))
               (ignore-errors (sb-thread:join-thread accept)))))
      (%stop-ffmpeg pv)
      (ignore-errors (sb-bsd-sockets:socket-close sock))
      (ignore-errors (uiop:delete-file-if-exists jpg))
      (ignore-errors (uiop:delete-empty-directory dir)))
    (let ((r (pv-result pv)))
      (if r
          (format t "  [preview] using ~A  zoom ~,2F pan ~,2F,~,2F~%"
                  (getf r :device) (getf r :zoom) (getf r :pan-x) (getf r :pan-y))
          (format t "  [preview] cancelled~%"))
      r)))
