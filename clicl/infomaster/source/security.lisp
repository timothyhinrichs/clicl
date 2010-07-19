;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; security.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *repository* *host* *port* *web*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; profile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'profile)) postlines)
  (declare (ignore postlines))
  (output-fastinspectpage s *client* 'person))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; signin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'signin)) postlines)
  (cond ((null postlines) (process-signin-start s nil))
        ((equalp (getf-post "Command" postlines) "Sign In")
	 (process-signin-signin s postlines))
        (t (process-signin-start s postlines))))

(defmethod process-signin-start (s postlines)
  (let (url)
    (when postlines (setq url (cdar postlines)))
    (output-prolog s 200)
    (format-html s) (crlf s)
    (output-head s "Sign In") (crlf s)
    (format s "<body leftmargin='0' topmargin='0' marginwidth='0' marginheight='0'
	             bgcolor='~A' onload='document.form1.Username.focus()'>" *bgcolor*) (crlf s)
    (output-header s)
    (output-signin-structure s url nil nil)
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)
    'done))

(defmethod process-signin-signin (s postlines)
  (let (username password url user nonce (*security* *repository*))
    (setq username (cdr (pop postlines)))
    (setq password (cdr (pop postlines)))
    (setq url (cdr (pop postlines)))
    (setq user (read-value-string username))
    (when (equal url "") (setq url "/"))
    (cond ((chkpassword user password)
           (unless (setq nonce (get user 'nonce)) ;(result 'nonce user *repository*))
             (setq nonce (generate-nonce))
             ;(kill `(nonce ,user ?y) *repository* #'matchp)
             ;(insert `(nonce ,user ,nonce) *repository*)
	     (setf (get user 'nonce) nonce))
           (setq *client* user)
           (cond ((not (eq user 'anonymous))
                  (format s "HTTP/1.0 200 OK") (crlf s)
                  (format s "Set-Cookie: user=~A; path=/;" user) (crlf s)
                  (format s "Set-Cookie: nonce=~A; path=/;" nonce) (crlf s)
                  (crlf s)
                  (process s 'toplevel nil))
                 (t (format s "HTTP/1.0 302 Moved temporarily") (crlf s)
                    (format s "Location: ~A" url) (crlf s)
                    (format s "Set-Cookie: user=~A; path=/;" user) (crlf s)
                    (format s "Set-Cookie: nonce=~A; path=/;" nonce) (crlf s)
                    (crlf s))))
          (t (output-prolog s 200)
             (format-html s) (crlf s)
             (format s "<head>")
             (format s "<title>~A - Sign In</title>" (prettify *gui*))
             (format s "</head>") (crlf s)
             (format s "<body onload='document.form1.Username.focus();' bgcolor='~A' leftmargin='0' topmargin='0' marginwidth='0' marginheight='0'>" *bgcolor*) (crlf s)
             (output-header s)
             (output-signin-structure s url username t)
             (output-footer s)
             (finish-body s) (crlf s)
             (finish-html s) (crlf s)))))

(defmethod process-signin-signin (s postlines)
  (let (username password url user nonce (*security* *repository*))
    (setq username (cdr (pop postlines)))
    (setq password (cdr (pop postlines)))
    (setq url (cdr (pop postlines)))
    (setq user (read-value-string username))
    (when (equal url "") (setq url "/"))
    (cond ((chkpassword user password)
           (unless (setq nonce (get user 'nonce)) ;(result 'nonce user *repository*))
             (setq nonce (generate-nonce))
             ;(kill `(nonce ,user ?y) *repository* #'matchp)
             ;(insert `(nonce ,user ,nonce) *repository*)
	     (setf (get user 'nonce) nonce))
           (setq *client* user)
           (format s "HTTP/1.0 302 Moved temporarily") (crlf s)
	   (format s "Location: ~A" url) (crlf s)
	   (format s "Set-Cookie: user=~A; path=/;" user) (crlf s)
	   (format s "Set-Cookie: nonce=~A; path=/;" nonce) (crlf s)
	   (crlf s))
          (t (output-prolog s 200)
             (format-html s) (crlf s)
             (format s "<head>")
             (format s "<title>~A - Sign In</title>" (prettify *gui*))
             (format s "</head>") (crlf s)
             (format s "<body onload='document.form1.Username.focus();' bgcolor='~A' leftmargin='0' topmargin='0' marginwidth='0' marginheight='0'>" *bgcolor*) (crlf s)
             (output-header s)
             (output-signin-structure s url username t)
             (output-footer s)
             (finish-body s) (crlf s)
             (finish-html s) (crlf s)))))

(defun output-signin-structure (s url user retry)
  (format s "<form name='form1' action='signin?' method='post'>")
  (format s "<div style='font-weight: bold; margin-left: 10px; margin-top:10px; font-size: large'>Sign in.</div>")
  (when retry
    (format s "<p style='color: red; margin-left: 10px'><b>Invalid ID or password.</b><br/>Please try again.</p>"))
  (format s "<p style='margin-left:10px'>")
  (format s "<table cellspacing='3'>")
  (format s "<tr><td><b>Your ID</b></td><td>")
  (format-text s "Username" (htmlify user) 17 32)
  (format s "</td></tr>")
  (format s "<tr><td><b>Password</b></td><td>")
  (format-password s "Password" "" 17 32)
  (format s "</td></tr>")
  (format s "</table><br/>")
  (format-hidden s "Location" (htmlify url))
  (format-button s "Command" "Sign In")
  (format s "<br/><br/><br/>")
  (format s "<span style='line-height: 20px;'>Forgot your ID or password?  <a href='forgotpassword?'>Click here</a>.</span><br/>")
  (format s "Don't have an ID?  <a href='signup?Location=~A'>Sign up</a>.<br/>" (htmlify url))
  (format s "</p>")
  (format s "</form>"))

(defun makeuserid (username)
  (intern (nstring-upcase username)))

(defun redirect (s url)
   (format s "HTTP/1.0 302 Moved temporarily") (crlf s)
   (format s "Location: ~A" url) (crlf s)
   (crlf s))

(defmethod generate-nonce ()
  (format nil "~X" (* (get-universal-time) (get-internal-real-time)
                      (get-internal-run-time) (random 1000000000000))))

(defun chknonce (client nonce)
  (triplep 'nonce client nonce *security*))

(defun chknonce (client nonce)
  (equalp nonce (get client 'nonce)))

(defun chkpermissionp (client command)
  (triplep 'permission client command *security*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; signout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'signout)) postlines)
  (declare (ignore postlines))
  ;(kill `(nonce ,*client* ?y) *repository* #'matchp)
  (remprop *client* 'nonce)
  (setq *client* 'anonymous)
  (setq *password* "anonymous")
  (format s "HTTP/1.0 302 Moved temporarily") (crlf s)
  (format s "Location: /") (crlf s)
  (format s "Set-Cookie: user=false; path=/; expires=Wednesday, 09-Nov-99 23:12:40 GMT" *client*) (crlf s)
  (format s "Set-Cookie: nonce=false; path=/; expires=Wednesday, 09-Nov-99 23:12:40 GMT" *client*) (crlf s)
  (crlf s)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>~A - Sign Out</title>" (prettify *gui*))
  (format s "</head>") (crlf s)
  (format s "<body bgcolor='~A' leftmargin='0' topmargin='0' marginwidth='0' marginheight='0'>" *bgcolor*) (crlf s)
  (output-header s)
  (format s "<center><p style='font-size: large'>You are now signed out.</p></center>")
  (format s "<center><p style='font-size: medium'><a href='/'>Click here<a> to return to the homepage.</p></center>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; signup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'signup)) postlines)
  (let (fn ln zipcode telephone email username pwd repeat url)
    (cond ((null postlines) (process-signup-start s "/"))
          ((null (cdr postlines)) (process-signup-start s (cdar postlines)))
          ((equalp (getf-post "Command" postlines) "Create")
           (setq fn (cdr (pop postlines)))
           (setq ln (cdr (pop postlines)))
           (setq zipcode (cdr (pop postlines)))
           (setq telephone (cdr (pop postlines)))
           (setq email (cdr (pop postlines)))
           (setq username (cdr (pop postlines)))
           (setq pwd (cdr (pop postlines)))
           (setq repeat (cdr (pop postlines)))
           (setq url (or (cdar postlines) "/"))
           (process-signup-change s fn ln zipcode telephone email username pwd repeat url))
          (t (process-signup-start s "/")))))

(defun process-signup-start (s url)
  (output-prolog s 200)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Create a User Account</title>")
  (format s "</head>") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (output-signup-structure s nil "" "" "" "" "" "" "" "" url)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-signup-change (s fn ln zipcode telephone email username pwd repeat url)
  (let (error id hash)
    (format-html s) (crlf s)
    (format s "<head>")
    (format s "<title>Create Account</title>")
    (format s "</head>") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (cond ((setq error (required-error-p fn "First Name")))
          ((setq error (required-error-p ln "Last Name")))
          ((setq error (required-error-p zipcode "ZIP Code")))
          ((setq error (required-error-p telephone "Telephone")))
          ((setq error (required-error-p email "Email address")))
          ((setq error (required-error-p username "ID")))
          ((setq error (required-error-p pwd "Password")))
          ((setq error (firstname-error-p fn)))
          ((setq error (lastname-error-p ln)))
          ((setq error (zipcode-error-p zipcode)))
          ((setq error (telephone-error-p telephone)))
          ((setq error (email-error-p email)))
          ((setq error (username-error-p username)))
          ((setq error (username-exists-p username)))
          ((not (equal pwd repeat)) (setq error "Passwords don't match."))
          ((setq error (password-error-p pwd)))
          (t (setq id (makeuserid username))
             (setq hash (hashpassword id pwd))
             (with-lock-grabbed (*lock*)
               (cond ((user-exists-p id) (setq error "ID already exists."))
                     (t (revise `(and (person.instance ,id)
                                      (person.firstname ,id ,fn)
                                      (person.lastname ,id ,ln)
                                      (person.zipcode ,id ,zipcode)
                                      (person.telephone ,id ,telephone)
                                      (person.email ,id ,email)
                                      (pwd ,id ,hash)) nil *repository*))))))
    (cond (error (output-signup-structure s error fn ln zipcode telephone email username pwd repeat url))
          (t (format s "<center><p style='font-size: large'>Your account has been created.</p></center>")
             (format s "<center><p style='font-size: medium'><a href='signin?Username=~A&Password=~A&location=~A&Command=Sign+In'>Click here</a> to sign in.</p></center>" username pwd
                     (htmlify url))))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun output-signup-structure (s error fn ln zipcode telephone email username pwd repeat url)
  (format s "<form action='signup?' method='post' name='form1'>")
  (format s "<div style='font-weight: bold; margin-left: 10px; margin-top:10px; font-size: large'>Create an account.</div>")
  (format s "<p style='margin-left: 10px'>All fields are required.</p>")
  (when error
    (format s "<p style='color: red; margin-left: 10px'><b>~A</b><br/>Please try again.</p>" error))
  (format s "<div style='margin-left:10px; margin-right:10px;'>")
  (format s "<table cellspacing='3'>")
  (format s "<tr><th align='left' valign='top'>First Name:</th><td>")
  (format-text s "person.firstname" fn 27 30)
  (format s "</td></tr>")
  (format s "<tr><th align='left' valign='top'>Last Name:</th><td>")
  (format-text s "person.lastname" ln 27 30)
  (format s "</td></tr>")
  (format s "<tr><th align='left' valign='top'>ZIP Code:</th><td>")
  (format-text s "person.zipcode" zipcode 27 5)
  (format s "</td></tr>")
  (format s "<tr><th align='left' valign='top'>Telephone:</th><td>")
  (format-text s "person.telephone" telephone 27 11)
  (format s "</td></tr>")
  (format s "<tr><th align='left' valign='top'>Email:</th><td>")
  (format-text s "person.email" email 27 120)
  (format s "</td></tr>")
  (format s "<tr><th align='left' valign='top'>Your ID:</th><td>")
  (format-text s "Username" username 27 32)
  (format s "<br/><div style='font-family:arial; font-size:11px; color:#808080;line-height:15px;'>ID may consist of letters, numbers, periods, underscores, and dashes. Capitalization does not matter.</div>")
  (format s "</td></tr>")
  (format s "<tr><th align='left' valign='top'>Password:</th><td>")
  (format-password s "pwd" pwd 27 32)
  (format s "<br/><div style='font-family:arial; font-size:11px; color:#808080;line-height:15px;'>6 characters or more.  Capitalization matters!</div>")
  (format s "</td></tr>")
  (format s "<tr><th align='left' valign='top'>Re-enter Password:</th><td>")
  (format-password s "Repeat" repeat 27 32)
  (format s "</td></tr>")
  (format s "</table><br/>")
  (format-hidden s "Location" url)
  (format-button s "Command" "Create")
  (format s " your account.")
  (format s "<br/><br/><br/>Already have an ID?  <a href='signin?'>Sign in</a>.")
  (format s "</div>")
  (format s "</form>"))

(defun required-error-p (x name)
  (when (or (null x) (equal "" x))
    (format nil "~A is required." name)))

(defun firstname-error-p (fn)
   (when (> (length fn) 30) "First name must be at most 30 characters."))

(defun lastname-error-p (ln)
   (when (> (length ln) 30) "Last name must be at most 30 characters."))

(defun zipcode-error-p (zipcode)
   (unless (every #'digitp zipcode) "ZIP Code is invalid."))

(defun telephone-error-p (telephone)
   (when (equalp telephone "") "Telephone is invalid."))

(defun email-error-p (email)
   (unless (emailp email) "Email address is invalid."))

(defparameter *email-regexp*
  "^[A-z0-9_\\-\\.]+\@(([A-z0-9_\\-]+\\.)+[A-z][A-z]+)|\\[([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])$")

(defun emailp (x)
  (regexec x *email-regexp*))

(defun user-exists-p (id)
  (doublep 'user.instance id *repository*))

(defun username-exists-p (id)
  (unless (eq (classify (read-user-string id) *repository*) 'thing)
    "ID already exists."))

(defun username-error-p (id)
   (cond ((> (length id) 32) "ID must be at most 32 characters.")
         ((notevery #'username-char id) "ID must consist of a-z, 0-9, periods (.), underscores (_) and dashes (-).")
         ((regexec id "--") "ID cannot contain two consecutive dashes (--)")
         ((regexec id "__") "ID cannot contain two consecutive underscores (__)")
         ((regexec id "\\.\\.") "ID cannot contain two consecutive periods (..)")
         ((regexec id "\\.-") "ID cannot contain a period followed by a dash (.-)")
         ((regexec id "-\\.") "ID cannot contain a dash followed by a period (-.)")
         ((regexec id "\\._") "ID cannot contain a period followed by an underscore (._)")
         ((regexec id "_\\.") "ID cannot contain an underscore followed by a period (_.)")
         ((regexec id "-_") "ID cannot contain a dash followed by an underscore (-_)")
         ((regexec id "_-") "ID cannot contain an underscore followed by a dash (_-)")
         ((not (eq (classify (read-user-string id) *repository*) 'thing)) "ID already exists.")))

(defun username-char (c)
   (or (alphanumericp c) (eq c #\-) (eq c #\.) (eq c #\_)))

(defun password-error-p (p)
   (cond ((< (length p) 6) "Password must be at least 6 characters.")
         ((> (length p) 32) "Password must be at most 32 characters.")
         (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; crypto
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sha1 (x)
  (let ((hash))
    (setq hash (sha:sha1sum-sequence x))
    (format nil "~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X" 
        (elt hash 0) (elt hash 1) (elt hash 2) (elt hash 3) (elt hash 4) (elt hash 5) (elt hash 6) (elt hash 7) (elt hash 8) (elt hash 9) (elt hash 10)
        (elt hash 11)  (elt hash 12)  (elt hash 13)  (elt hash 14)  (elt hash 15)  (elt hash 16)  (elt hash 17)  (elt hash 18)  (elt hash 19))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'password)) postlines)
  (cond ((null postlines) (process-password-start s))
        ((and (setq command (getf-post "Command" postlines)) nil))
        ((equalp command "Record") (process-password-change s postlines))
        (t (process-password-start s))))

(defmethod process-password-start (s)
    (format-html s) (crlf s)
    (format s "<head>")
    (format s "<title>~A - Change Password</title>" (prettify *gui*))
    (format s "</head>") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (output-password-structure s nil)
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s))

(defmethod process-password-change (s postlines)
  (let (old new repeat error)
    (setq old (cdr (pop postlines)))
    (setq new (cdr (pop postlines)))
    (setq repeat (cdr (pop postlines)))
    (format-html s) (crlf s)
    (format s "<head>")
    (format s "<title>~A - Change Password</title>" (prettify *gui*))
    (format s "</head>") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (cond ((not (chkpassword *client* old))
            (output-password-structure s "Old password incorrect."))
          ((not (equal new repeat))
            (output-password-structure s "New passwords don't match."))
          ((setq error (password-error-p new))
            (output-password-structure s error))
          (t     (format s "<center><p style='font-size: large'>Your password has been changed.</p></center>")
    (format s "<center><p style='font-size: medium'><a href='/'>Click here<a> to return to the homepage.</p></center>")
             (prorequest (cons 'update `(and (not (pwd ,*client* ,old)) (pwd ,*client* ,new))))))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defmethod output-password-structure (s error)
  (format s "<form name='form1' action='password?' method='post'>")
  (format s "<div style='font-weight: bold; margin-left: 10px; margin-top:10px; font-size: large'>Change your password.</div>")
  (when error
    (format s "<p style='color: red; margin-left: 10px'><b>~A</b><br/>Please try again.</p>" error))
  (format s "<p style='margin-left: 10px'>")
  (format s "<table cellspacing='3'>")
  (format s "<tr><td style='text-align: right'><b>Old Password:</b></td><td>")
  (format-password s "Old" "" 17 32)
  (format s "</td></tr><tr><td><br/></td></tr>")
  (format s "<tr><td style='text-align: right'><b>New Password:</b></td><td>")
  (format-password s "New" "" 17 32)
  (format s "</td></tr>")
  (format s "<tr><td style='text-align: right'><b>Re-enter New Password:</b></td><td>")
  (format-password s "Repeat" "" 17 32)
  (format s "</td></tr>")
  (format s "</table><br/>")
  (format-button s "Command" "Record")
  (format s " changes.")
  (format s "</p>")
  (format s "</form>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setpassword
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'setpassword)) postlines)
  (cond ((or (null postlines) (null (cdr postlines))) (http-problem s "Bad request."))
        ((and (setq command (getf-post "Command" postlines)) nil))
        ((equalp command "Record") (process-setpassword-change s postlines))
        (t (process-setpassword-start s (cdar postlines) (cdadr postlines)))))

(defmethod process-setpassword-start (s user key)
    (format-html s) (crlf s)
    (format s "<head>")
    (format s "<title>Set Your Passowrd</title>")
    (format s "</head>") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (cond ((not (checkkey (read-user-string user) key))
             (format s "<center><p style='font-size: large'>There is a problem with your request.  Please request a <a href='forgotpassword?'>new email</a> to be sent to you.</p></center>"))
          (t (output-setpassword-structure s user key nil)))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s))

(defmethod process-setpassword-change (s postlines)
  (let (user key new repeat error old)
    (setq user (makeuserid (cdr (pop postlines))))
    (setq key (cdr (pop postlines)))
    (setq new (cdr (pop postlines)))
    (setq repeat (cdr (pop postlines)))
    (setq old (find-password user))
    (format-html s) (crlf s)
    (format s "<head>")
    (format s "<title>Set Your Password</title>")
    (format s "</head>") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (cond ((not (checkkey user key))
           (format s "<center><p style='font-size: large'>There is a problem with your request.  Please request a <a href='forgotpassword?'>new email</a> to be sent to you.</p></center>"))
          ((not (equal new repeat))
           (output-setpassword-structure s user key "New passwords don't match."))
          ((setq error (password-error-p new))
           (output-setpassword-structure s user key error))
          (t (format s "<center><p style='font-size: large'>Your password has been set.</p></center>")
             (format s "<center><p style='font-size: medium'><a href='/'>Click here</a> to return to the homepage.</p></center>")
             (prorequest (cons 'update `(and (not (pwd ,user ,old)) (pwd ,user ,(hashpassword user new)))))))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defmethod output-setpassword-structure (s user key error)
  (format s "<form name='form1' action='setpassword?' method='post'>")
  (format-hidden s "User" user)
  (format-hidden s "Key" key)
  (format s "<div style='font-weight: bold; margin-left: 10px; margin-top:10px; font-size: large'>Set your password.</div>")
  (when error
    (format s "<p style='color: red; margin-left: 10px'><b>~A</b><br/>Please try again.</p>" error))
  (format s "<p style='margin-left: 10px'>")
  (format s "<table cellspacing='3'>")
  (format s "<tr><td style='text-align: right'><b>Password:</b></td><td>")
  (format-password s "New" "" 17 32)
  (format s "</td></tr>")
  (format s "<tr><td style='text-align: right'><b>Re-enter Password:</b></td><td>")
  (format-password s "Repeat" "" 17 32)
  (format s "</td></tr>")
  (format s "</table><br/>")
  (format-button s "Command" "Record")
  (format s " changes.")
  (format s "</p>")
  (format s "</form>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; resetpassword
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'resetpassword)) postlines)
  (cond ((or (null postlines) (null (cdr postlines))) (http-problem s "Bad request."))
        ((and (setq command (getf-post "Command" postlines)) nil))
        ((equalp command "Record") (process-resetpassword-change s postlines))
        (t (process-resetpassword-start s (cdar postlines) (cdadr postlines)))))

(defmethod process-resetpassword-start (s user key)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Reset Your Passowrd</title>")
  (format s "</head>") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (cond ((not (checkkey (read-user-string user) key))
         (format s "<center><p style='font-size: large'>There is a problem with your request.  Please request a <a href='forgotpassword?'>new email</a> to be sent to you.</p></center>"))
        (t (output-resetpassword-structure s user key nil)))
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defmethod process-resetpassword-change (s postlines)
  (let (user key new repeat error old)
    (setq user (makeuserid (cdr (pop postlines))))
    (setq key (cdr (pop postlines)))
    (setq new (cdr (pop postlines)))
    (setq repeat (cdr (pop postlines)))
    (setq old (find-password user))
    (format-html s) (crlf s)
    (format s "<head>")
    (format s "<title>Reset Your Password</title>")
    (format s "</head>") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (cond ((not (checkkey user key))
           (format s "<center><p style='font-size: large'>There is a problem with your request.  Please request a <a href='forgotpassword?'>new email</a> to be sent to you.</p></center>"))
          ((not (equal new repeat))
           (output-resetpassword-structure s user key "New passwords don't match."))
          ((setq error (password-error-p new))
           (output-resetpassword-structure s user key error))
          (t (format s "<center><p style='font-size: large'>Your password has been changed.</p></center>")
             (format s "<center><p style='font-size: medium'><a href='/'>Click here</a> to return to the homepage.</p></center>")
             (prorequest (cons 'update `(and (not (pwd ,user ,old)) (pwd ,user ,(hashpassword user new)))))))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defmethod output-resetpassword-structure (s user key error)
  (format s "<form action='resetpassword?' method='post' name='form1'>")
  (format-hidden s "User" user)
  (format-hidden s "Key" key)
  (format s "<div style='font-weight: bold; margin-left: 10px; margin-top:10px; font-size: large'>Change your password.</div>")
  (when error
    (format s "<p style='color: red; margin-left: 10px'><b>~A</b><br/>Please try again.</p>" error))
  (format s "<p style='margin-left: 10px'>")
  (format s "<table cellspacing='3'>")
  (format s "<tr><td style='text-align: right'><b>New Password:</b></td><td>")
  (format-password s "New" "" 17 32)
  (format s "</td></tr>")
  (format s "<tr><td style='text-align: right'><b>Re-enter New Password:</b></td><td>")
  (format-password s "Repeat" "" 17 32)
  (format s "</td></tr>")
  (format s "</table><br/>")
  (format-button s "Command" "Record")
  (format s " changes.")
  (format s "</p>")
  (format s "</form>"))

(defun hashpassword (user pwd)
  (sha1 (stringappend user pwd)))

(defun chkpassword (client pwd)
  (triplep 'pwd client (hashpassword client pwd) *security*))

(defun checkkey (client key)
  (let ((hash))
     (setq hash (request `(ask-one ?x (pwd ,client ?x)) *client* *security*))
       (when hash
         (equal (sha1 hash) key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; forgotpassword
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'forgotpassword)) postlines)
  (cond ((null postlines) (process-forgotpassword-start s))
        ((and (setq command (getf-post "Command" postlines)) nil))
        ((equalp command "Email") (process-forgotpassword-change s postlines))
        (t (process-forgotpassword-start s))))

(defmethod process-forgotpassword-start (s)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>~A - Forgot Your ID or Password</title>" (prettify *gui*))
  (format s "</head>") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (output-forgotpassword-structure s nil)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defmethod process-forgotpassword-change (s postlines)
  (let (x)
    (setq x (cdr (pop postlines)))
    (format-html s) (crlf s)
    (format s "<head>")
    (format s "<title>~A - Forgot Your ID or Password</title>" (prettify *gui*))
    (format s "</head>") (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (cond ((eq (length x) 0)
           (output-forgotpassword-structure s "You did not enter an ID or email address."))
          ((substringp "@" x)
           (cond ((not (emailp x))
                  (output-forgotpassword-structure s "Email address invalid."))
                 (t (email-forgotten-usernames x)
                    (format s "<center><p style='font-size: large'>Your ID has been sent to ~A.</p></center>" x)
                    (format s "<center><p style='font-size: medium'><a href='/'>Click here<a> to return to the homepage.</p></center>"))))
          ((username-error-p x)
           (output-forgotpassword-structure s "The ID or email address you entered was invalid."))
          (t (email-forgotten-password x)
             (format s "<center><p style='font-size: large'>Your password has been emailed to you.</p></center>")
             (format s "<center><p style='font-size: medium'><a href='/'>Click here<a> to return to the homepage.</p></center>")))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun find-usernames (email)
  (finds '?x `(person.email ?x ,email) *repository*))

(defun find-email (id)
  (viewfindx '?x `(person.email ,id ?x) *repository*))

(defun find-password (id)
  (findx '?x `(pwd ,id ?x) *repository*))

(defun email-forgotten-password (username)
  (let (id email password key)
    (setq id (read-user-string username))
    (setq email (find-email id))
    (setq password (find-password id))
    (when password 
      (setq key (sha1 password))
      (when (and (stringp email) (stringp password))
        (mail "noreply@stanford.edu" email
            "The Information You Requested"
            (format nil "To reset your password, copy and paste the following URL into the address bar of your browser:~A~A~A~A~A/resetpassword?user=~A&key=~A" #\return #\linefeed #\return #\linefeed *web* username key)
            "smtp-roam.stanford.edu")))))

(defun email-forgotten-usernames (email)
  (let ((ids (find-usernames email)))
    (cond ((null ids))
          ((null (cdr ids))
           (mail "noreply@stanford.edu" email
                 "The Information You Requested"
                 (format nil "Your username is: ~A" (car ids))
                 "smtp-roam.stanford.edu"))
          (t (mail "noreply@stanford.edu" email
                   "The Information You Requested"
                   (format nil "The following usernames have email address ~A: ~{ ~A~^,~}" email ids)
                   "smtp-roam.stanford.edu")))))

(defmethod output-forgotpassword-structure (s error)
  (format s "<form name='form1' action='forgotpassword?' method='post'>")
  (format s "<br/><div style='margin-left: 10px; font-size: normal; line-height: 25px;'><b>Forgot your password?</b> Enter your ID and we'll send your password to you via email.<br/>  <b>Forgot your ID?</b> Enter your email address and we'll send your ID to you.</div>")
  (when error
    (format s "<p style='color: red; margin-left: 10px'><b>~A</b><br/>Please try again.</p>" error))
  (format s "<p style='margin-left: 10px'>")
  (format s "<table cellspacing='3'>")
  (format s "<tr><td style='text-align: right'><b>IPX ID or Email:</b></td><td>")
  (format-text s "IDorEmail" "" 30 120)
  (format s "</td></tr><tr><td><br/></td></tr>")
  (format s "</table><br/>")
  (format-button s "Command" "Email")
  (format s " me my IPX ID or password.")
  (format s "</p>")
  (format s "</form>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
