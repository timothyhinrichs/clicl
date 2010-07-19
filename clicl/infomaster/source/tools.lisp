
(defun http-reader (s host &optional ip)
  (with-output-to-string (out)
    (do ((c))
        ((not (stream-listen s)))
        (setq c (read-char s))
        (write-char c s)
        (write-char c t))))

(defun http-identifier (s)
  (let ((host (ccl::tcp-addr-to-str (ccl::stream-remote-host s))))
    (format s "~A

" host)
    host))

(defun http-challenge (s)
  (print (with-output-to-string (out)
           (do ((c))
               ((not (stream-listen s)))
               (setq c (read-char s))
               (write-char c out))))
    (format s "HTTP/1.0 401 Unauthorized

WWW-Authenticate: Basic realm=\"Infoserver\"



")
    (finish-output s)
    (close s))

(defun http-ignorer (s)
  (with-output-to-string (out)
    (do ((c))
        ((not (stream-listen s)))
        (setq c (read-char s))
        (write-char c out))))

(defun http-test (s)
  (with-output-to-string (out)
    (with-input-from-string (in s)
      (let ((b (make-two-way-stream in out)))
        (http-handler b 4800)))))

(defun file-handler (handler fn)
  (with-output-to-string (out)
    (with-open-file (in fn :direction :input)
      (let ((b (make-two-way-stream in out)))
        (funcall handler b)))))

(defparameter *james*
"POST /FIND?Lite=yes? HTTP/1.0

Referer: http://0.0.0.0:4800/FIND?Class=cookware?Lite=yes

Connection: Keep-Alive

User-Agent: Mozilla/4.07 (Macintosh; I; PPC)

Host: 0.0.0.0:4800

Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, */*

Accept-Encoding: gzip

Accept-Language: en

Accept-Charset: iso-8859-1,*,utf-8

Cookie: STATE-TAG=3153928068; standard=3153938654

Content-type: application/x-www-form-urlencoded

Content-length: 868



ExplicitTarget=&Lite=yes&Class=Cookware&Aspect=%3Fcookware&Start=1&Style=Menu&Attribute=Cookware.Type&Command_1=**remove**&cell_1=Skillet&End=&Start=2&Style=Menu&Attribute=Cookware.Material&Command_2=**remove**&End=&Start=3&Style=Menu&Attribute=Cookware.Interior&Command_3=**remove**&End=&Start=4&Style=Menu&Attribute=Cookware.Exterior&Command_4=**remove**&End=&Start=5&Style=Menu&Attribute=Cookware.Color&Command_5=**remove**&End=&Start=6&Style=Interval&Attribute=Cookware.Capacity&Min=&Max=&End=&Start=7&Style=Interval&Attribute=Cookware.Diameter&Min=&Max=&End=&Start=8&Style=Menu&Attribute=Cookware.Status&cell_8=Stocked&End=&Start=9&Style=Menu&Attribute=Cookware.Manufacturer&Command_9=**remove**&End=&Start=10&Style=Interval&Attribute=Cookware.Price&Min=&Max=&End=&Start=11&Style=Interval&Attribute=Cookware.Delivery&Min=&Max=&End=&Concept-End=&AutoSubmitCommand=

")

(defparameter *get-top*
"GET / HTTP/1.0

Referer: file:///gullible/mrg/mrg.html

Connection: Keep-Alive

User-Agent: Mozilla/2.01 (Macintosh; I; 68K)

Pragma: no-cache

Host: 171.64.71.89:4800

Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*



")

(defparameter *get-bmw*
"GET /GENERAL/BMW HTTP/1.0

Referer: http://171.64.71.89:4800/

Connection: Keep-Alive

User-Agent: Mozilla/2.01 (Macintosh; I; 68K)

Host: 171.64.71.89:4800

Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*



")

(defparameter *display-bmw*
"POST /QUERY?BMW HTTP/1.0

Referer: http://171.64.71.89:4800/GENERAL/BMW

Connection: Keep-Alive

User-Agent: Mozilla/2.01 (Macintosh; I; 68K)

Host: 171.64.71.89:4800

Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*

Content-type: application/x-www-form-urlencoded

Content-length: 368



Group-Start=AND&Concept=BMW&Aspect=%3FBMW&Cell-Start=&Val=&Cell-End=&Cell-Start=&Val=&Cell-End=&Cell-Start=&Val=&Cell-End=&Concept-End=&Group-End=&Kif-Box=&%3D%3D+Commands+%3D%3D=++&Command=Display&Display-type=Class&Display-Slots=Bmw+Type&Display-Slots=Bmw+Range&Display-Slots=Bmw+Msrp&Agent=facilitator@gullible&Host=gullible&Port=4801&New-relation=Bmw&Old-Options=0

")

(defparameter *display-model*
  "POST /GENERAL/MODEL HTTP/1.0

Referer: http://171.64.71.89:4800/GENERAL/MODEL

Connection: Keep-Alive

User-Agent: Mozilla/2.01 (Macintosh; I; 68K)

Host: 171.64.71.89:4800

Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*

Content-type: application/x-www-form-urlencoded

Content-length: 458



Group-Start=AND&Concept=MODEL&Aspect=%3FMODEL&Cell-Start=&Val=&Cell-End=&Cell-Start=&Val=&Cell-End=&Cell-Start=&Val=&Cell-End=&Cell-Start=&Val=&Cell-End=&Cell-Start=&Val=&Cell-End=&Concept-End=&Group-End=&Kif-Box=&%3D%3D+Commands+%3D%3D=++&Command=Display&Display-type=Class&Display-Slots=Doors&Display-Slots=Seats&Display-Slots=Range&Display-Slots=Msrp&Display-Slots=Nation&Agent=facilitator@gullible&Host=gullible&Port=4801&New-relation=Model&Old-Options=0

")

(defparameter *display-model-reply*
"HTTP/1.0 200 OK

Content-type: text/html


<HTML>
<HEAD>
<TITLE>Model</TITLE>
</HEAD>
<BODY>
<H1>Model</H1>
<HR>
The following table contains 10 items:<P>
<TABLE BORDER>
<TR><TH></TH>
<TH>Doors</TH>
<TH>Seats</TH>
<TH><A HREF=\"/HELP/DESCRIBE?RANGE\">Range</A></TH>
<TH><A HREF=\"/HELP/DESCRIBE?MSRP\">Msrp</A></TH>
<TH>Nation</TH>
</TR>

<TR><TH><FONT SIZE=-1><A HREF=\"/SHOW-OBJ/JIMMY\">Jimmy</A></FONT></TH>
<TD ALIGN=\"RIGHT\">2</TD><TD ALIGN=\"RIGHT\">5</TD><TD ALIGN=\"RIGHT\">400</TD><TD ALIGN=\"RIGHT\">25500</TD><TD><A HREF=/SHOW-OBJ/AMERICAN>American</A></TD></TR>

<TR><TH><FONT SIZE=-1><A HREF=\"/SHOW-OBJ/NOVA\">Nova</A></FONT></TH>
<TD ALIGN=\"RIGHT\">4</TD><TD ALIGN=\"RIGHT\">6</TD><TD ALIGN=\"RIGHT\">467</TD><TD ALIGN=\"RIGHT\">14999</TD><TD><A HREF=/SHOW-OBJ/AMERICAN>American</A></TD></TR>

<TR><TH><FONT SIZE=-1><A HREF=\"/SHOW-OBJ/MUSTANG\">Mustang</A></FONT></TH>
<TD ALIGN=\"RIGHT\">2</TD><TD ALIGN=\"RIGHT\">5</TD><TD ALIGN=\"RIGHT\">482</TD><TD ALIGN=\"RIGHT\">25500</TD><TD><A HREF=/SHOW-OBJ/AMERICAN>American</A></TD></TR>

<TR><TH><FONT SIZE=-1><A HREF=\"/SHOW-OBJ/ACCORD\">Accord</A></FONT></TH>
<TD ALIGN=\"RIGHT\">4</TD><TD ALIGN=\"RIGHT\">5</TD><TD ALIGN=\"RIGHT\">382</TD><TD ALIGN=\"RIGHT\">13400</TD><TD><A HREF=/SHOW-OBJ/JAPANESE>Japanese</A></TD></TR>

<TR><TH><FONT SIZE=-1><A HREF=\"/SHOW-OBJ/COROLLA\">Corolla</A></FONT></TH>
<TD ALIGN=\"RIGHT\">2</TD><TD ALIGN=\"RIGHT\">4</TD><TD ALIGN=\"RIGHT\">364</TD><TD ALIGN=\"RIGHT\">9333</TD><TD><A HREF=/SHOW-OBJ/JAPANESE>Japanese</A></TD></TR>

<TR><TH><FONT SIZE=-1><A HREF=\"/SHOW-OBJ/FOX\">Fox</A></FONT></TH>
<TD ALIGN=\"RIGHT\">2</TD><TD ALIGN=\"RIGHT\">4</TD><TD ALIGN=\"RIGHT\">420</TD><TD ALIGN=\"RIGHT\">18900</TD><TD><A HREF=/SHOW-OBJ/GERMAN>German</A></TD></TR>

<TR><TH><FONT SIZE=-1><A HREF=\"/SHOW-OBJ/318I\">318i</A></FONT></TH>
<TD ALIGN=\"RIGHT\">222</TD><TD ALIGN=\"RIGHT\">4</TD><TD ALIGN=\"RIGHT\">640</TD><TD ALIGN=\"RIGHT\">25500</TD><TD><A HREF=/SHOW-OBJ/GERMAN>German</A></TD></TR>

<TR><TH><FONT SIZE=-1><A HREF=\"/SHOW-OBJ/325\">325</A></FONT></TH>
<TD ALIGN=\"RIGHT\">2</TD><TD ALIGN=\"RIGHT\">4</TD><TD ALIGN=\"RIGHT\">560</TD><TD ALIGN=\"RIGHT\">28500</TD><TD><A HREF=/SHOW-OBJ/GERMAN>German</A></TD></TR>

<TR><TH><FONT SIZE=-1><A HREF=\"/SHOW-OBJ/190E\">190e</A></FONT></TH>
<TD ALIGN=\"RIGHT\">2</TD><TD ALIGN=\"RIGHT\">5</TD><TD ALIGN=\"RIGHT\">640</TD><TD ALIGN=\"RIGHT\">35500</TD><TD><A HREF=/SHOW-OBJ/GERMAN>German</A></TD></TR>

<TR><TH><FONT SIZE=-1><A HREF=\"/SHOW-OBJ/450SL\">450sl</A></FONT></TH>
<TD ALIGN=\"RIGHT\">2</TD><TD ALIGN=\"RIGHT\">2</TD><TD ALIGN=\"RIGHT\">560</TD><TD ALIGN=\"RIGHT\">65500</TD><TD><A HREF=/SHOW-OBJ/GERMAN>German</A></TD></TR>
</TABLE>

<HR>
<ADDRESS><FONT SIZE=1>
<A HREF=\"http://infomaster.stanford.edu/doc/infomaster.html\"><STRONG>Infomaster</STRONG></A>
[version June 25 1996] is a trademark of
<A HREF=\"http://www.stanford.edu/\">Stanford University</A>.

This service is a product of the <A HREF=\"http://logic.stanford.edu/\">Logic
Group</A>.
Questions, comments, and suggestions are welcome at
<A HREF=\"mailto:help@infomaster.stanford.edu\"><EM>help@infomaster.stanford.edu</EM></A>.
Page layout was designed for <A HREF=\"http://www.netscape.com/\">Netscape</A>;
quality may vary with other web browsers.

</FONT></ADDRESS>

</BODY>
</HTML>
")

