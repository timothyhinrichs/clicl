;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Patches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process-fastinspectpage (s object class)
  (format-html s) (crlf s)
  (format s "<head>")
  (format s "<title>Create ~A</title>" (prettify (cadr structure))) (crlf s)
  (format s (fastscript))
  (format s (modalscript))
  (format s "</head>") (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<div id='result' style='margin-left:10px; margin-right:10px'>") (crlf s)
  (output-fastinspect s object class)
  (format s "</div>")
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defmethod output-fastinspect (s object class)
  (let (structure)
    (setq structure (reviseitem object class))   
    (format s "<BR/><B>~A</B> is " (prettyname object))
    (format s (article class))
    (format s " <B>" )
    (output-classlink s class)
    (format s "</B>.")
    (force-output s)
    (format s "<table cellspacing='8'>")
    (do ((l (cddr structure)) (slot) (values))
        ((null l))
        (setq slot (caar l))
        (multiple-value-setq (values l) (collectvalues slot l))
        (output-multiple-cell s slot values))
    (format s "</table>")
    (output-fastinspect-commands s object class)))

(defmethod process (s (command (eql 'fastchange)) postlines)
  (let (dum object class structure (*buttons* 0))
    (cond ((null postlines) (http-problem s "Bad request."))
          ((null (cdr postlines))
           (setq object (read-value-string (cdr (pop postlines))))
           (setq class (classify object *gui*))
           (process-fastchange s object class))
          ((setq dum (getf-post "Another" postlines))
           (setq structure (fastchangestructure postlines))
           (setq dum (read-user-string dum))
           (process-fastchange-another s structure dum))
          ((setq dum (getf-post "Removal" postlines))
           (setq structure (fastchangestructure postlines))
           (setq dum (read-user-string dum))
           (process-fastchange-removal s structure dum))
          ((setq dum (getf-post "Command" postlines))
           (setq structure (fastchangestructure postlines))
           (process-fastchange-record s structure))
          (t (http-problem s "Bad request.")))))

(defun process-fastchange-record (s structure)
  (let (delta result)
    (setq delta (differentiator structure))
    (unless (null delta) (setq result (prorequest (cons 'update delta))))
    (cond ((errorp result) (output-problems s result))
          (t (output-fastinspect s (car structure) (cadr structure))))))

(defun output-fastchange (s structure)
  (let ((*buttons* 0))
    (format s "<form name='form1'>")
    (output-fastchange-structure s structure)
    (format s "<span onClick='postRecord(form1)'>Record Changes</span>")
    (format s "</form>") (crlf s)
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastscript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fastscript ()
  "<script>

    var http_request = false;
    var http_result = false;
    var textchange = false;

    function doclick (obj)
     {var height = window.outerHeight-100;
      var width = window.outerWidth-100;
      var left = window.screenX+50;
      var top = window.screenY+50;
      if (window.event.shiftKey == 1)
         {if (window.event.altKey == 1)
             {window.open('fastchangepage?object=' + obj,null,'height='+height+',width='+width+',left='+left+',top='+top)}
          else {window.location=('fastchangepage?object=' + obj)}}
      else if (window.event.altKey == 1)
              {window.open('fastinspectpage?object=' + obj,null,'height='+height+',width='+width+',left='+left+',top='+top)}
           else {window.location=('fastinspectpage?object=' + obj)}}

    function postRequest (url,args)
       {http_request = false;
        if (window.XMLHttpRequest)
           {http_request = new XMLHttpRequest();
            if (http_request.overrideMimeType)
               {http_request.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_request = new ActiveXObject(\"Msxml2.XMLHTTP\");}
                 catch (e) {
                try {http_request = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                    catch (e) {} }}
        if (!http_request)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_request.onreadystatechange = alertQuery;
        http_request.open('POST', url, true);
        http_request.send(args);}

    function alertQuery()
       {if (http_request.readyState == 4)
           {if (http_request.responseText)
               {query.innerHTML = http_request.responseText;}
            else {alert('There was a problem with the request in alertQuery.');}}}

    function postResult (url,args)
       {http_result = false;
        if (window.XMLHttpRequest)
           {http_result = new XMLHttpRequest();
            if (http_result.overrideMimeType)
               {http_result.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_result = new ActiveXObject(\"Msxml2.XMLHTTP\");}
                 catch (e) {
                try {http_result = new ActiveXObject(\"Microsoft.XMLHTTP\");}
                    catch (e) {} }}
        if (!http_result)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_result.onreadystatechange = alertResult;
        http_result.open('POST', url, true);
        http_result.send(args);}

    function alertResult()
       {if (http_result.readyState == 4)
           {if (http_result.responseText)
               {result.innerHTML = http_result.responseText}
            else {alert('There was a problem with the request in alertResult.')}}}

    function textEdit(e)
       {var keynum;
        if (window.event)
           {keynum = e.keyCode}
        else if(e.which)
           {keynum = e.which};
        if (keynum == 13)
           {textchange = false;
            postResult('fastshow?',newFormValues(form1));
            return false}
        else {textchange = true;
              return true}}

    function textBlur(e)
      {if (textchange == true)
          {textchange = false;
           postResult('fastshow?',newFormValues(form1))}}

    function newFormValues (fobj)
      {var str = '';
       for (var i = 0; i < fobj.elements.length; i++)
         {if (fobj.elements[i].type == \"select-one\")
             {var val = fobj.elements[i].options[fobj.elements[i].selectedIndex].value;
              str += fobj.elements[i].name + \"=\" + val + \"&\";}
          else if (fobj.elements[i].type == \"radio\" || fobj.elements[i].type == \"checkbox\")
                  {if (fobj.elements[i].checked)
                      {str += fobj.elements[i].name + \"=\" + escape(fobj.elements[i].value) + \"&\"}}
          else str += fobj.elements[i].name + \"=\" + escape(fobj.elements[i].value) + \"&\";}
       str = str.substr(0,(str.length - 1));
       return str;}

  function postMessage (url,args)
    {http_request = false;
     http_request = new XMLHttpRequest();
     if (http_request.overrideMimeType)
        {http_request.overrideMimeType('text/xml')};
     if (!http_request)
        {alert('Giving up: Cannot create an XMLHTTP instance');
         return false};
     http_request.onreadystatechange = alertMessage;
     http_request.open('POST', url, true);
     http_request.send(args)}

  function alertMessage ()
    {if (http_request.readyState == 4)
        {if (http_request.responseText)
            {return true}
         else {alert('There was a problem with the request.')}}}

   </script>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastcreatescript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fastcreatescript ()
  "<script>
    var http_result = false;

    function mouseclick ()
     {var ev = window.event;
      if (ev.shiftKey == 1 && ev.target.id)
         {window.open('fastchangepage?object=' + ev.target.id,null,'height=480,width=480'); ev.preventDefault(); return false}}

    function click (obj)
     {var ev = window.event;
      var xpos = (screen.width-640)/2;
      var height = screen.height-200;
      if (ev.shiftKey == 1)
         {window.open('fastchangepage?object=' + obj,null,'height='+height+',width=640,left='+xpos+',top=100,scrollbars=1')}
      else {window.open('inspect?object=' + obj,null,'height=480,width=480')}}

    function postAnother (fobj,location)
     {postResult('fastcreate?',newFormValues(fobj) + '&Another=' + location)}

    function postRemoval (fobj,location)
     {postResult('fastcreate?',newFormValues(fobj) + '&Removal=' + location)}

    function postResult (url,args)
       {http_result = false;
        if (window.XMLHttpRequest)
           {http_result = new XMLHttpRequest();
            if (http_result.overrideMimeType)
               {http_result.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_result = new ActiveXObject('Msxml2.XMLHTTP');}
                 catch (e) {
                try {http_result = new ActiveXObject('Microsoft.XMLHTTP');}
                    catch (e) {} }}
        if (!http_result)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_result.onreadystatechange = alertResult;
        http_result.open('POST', url, true);
        http_result.send(args);}

    function alertResult()
       {if (http_result.readyState == 4)
           {if (http_result.responseText)
               {result.innerHTML = http_result.responseText}
            else {alert('There was a problem with the request in alertResult.')}}}

    function newFormValues (fobj)
      {var str = '';
       for (var i = 0; i < fobj.elements.length; i++)
         {if (fobj.elements[i].type == 'select-one')
             {var val = fobj.elements[i].options[fobj.elements[i].selectedIndex].value;
              str += fobj.elements[i].name + '=' + val + '&';}
          else if (fobj.elements[i].type == 'radio' || fobj.elements[i].type == 'checkbox')
                  {if (fobj.elements[i].checked)
                      {str += fobj.elements[i].name + '=' + escape(fobj.elements[i].value) + '&'}}
          else str += fobj.elements[i].name + '=' + escape(fobj.elements[i].value) + '&';}
       str = str.substr(0,(str.length - 1));
       return str;}

   </script>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fastchangescript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fastchangescript ()
  "<script>
    var http_result = false;

    function mouseclick ()
     {var ev = window.event;
      if (ev.shiftKey == 1 && ev.target.id)
         {window.open('fastchange?object=' + ev.target.id,null,'height=480,width=480'); ev.preventDefault(); return false}}

    function click (obj)
     {var ev = window.event;
      var xpos = (screen.width-640)/2;
      var height = screen.height-200;
      if (ev.shiftKey == 1)
         {window.open('fastchangepage?object=' + obj,null,'height='+height+',width=640,left='+xpos+',top=100,scrollbars=1')}
      else {window.open('inspect?object=' + obj,null,'height=480,width=480')}}

    function postAnother (fobj,location)
     {postResult('fastchange?',newFormValues(fobj) + '&Another=' + location)}

    function postRemoval (fobj,location)
     {postResult('fastchange?',newFormValues(fobj) + '&Removal=' + location)}

    function postRecord (fobj)
     {postResult('fastchange?',newFormValues(fobj) + '&Command=Record')}

    function postResult (url,args)
       {http_result = false;
        if (window.XMLHttpRequest)
           {http_result = new XMLHttpRequest();
            if (http_result.overrideMimeType)
               {http_result.overrideMimeType('text/xml');}}
        else if (window.ActiveXObject) { // IE
            try {http_result = new ActiveXObject('Msxml2.XMLHTTP');}
                 catch (e) {
                try {http_result = new ActiveXObject('Microsoft.XMLHTTP');}
                    catch (e) {} }}
        if (!http_result)
           {alert('Giving up: Cannot create an XMLHTTP instance');
            return false;}
        http_result.onreadystatechange = alertResult;
        http_result.open('POST', url, true);
        http_result.send(args);}

    function alertResult()
       {if (http_result.readyState == 4)
           {if (http_result.responseText)
               {result.innerHTML = http_result.responseText}
            else {alert('There was a problem with the request in alertResult.')}}}

    function newFormValues (fobj)
      {var str = '';
       for (var i = 0; i < fobj.elements.length; i++)
         {if (fobj.elements[i].type == 'select-one')
             {var val = fobj.elements[i].options[fobj.elements[i].selectedIndex].value;
              str += fobj.elements[i].name + '=' + val + '&';}
          else if (fobj.elements[i].type == 'radio' || fobj.elements[i].type == 'checkbox')
                  {if (fobj.elements[i].checked)
                      {str += fobj.elements[i].name + '=' + escape(fobj.elements[i].value) + '&'}}
          else str += fobj.elements[i].name + '=' + escape(fobj.elements[i].value) + '&';}
       str = str.substr(0,(str.length - 1));
       return str;}

   </script>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
