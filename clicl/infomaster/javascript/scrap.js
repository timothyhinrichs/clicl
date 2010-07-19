
function postRecord (fobj)
 {window.location = fobj.action + 'Structure=' + newFormStructure(fobj) + '&Command=Record'}

function record (node)
 {postRecord('fastchangepage?','Structure=' + transform(node.parentNode) + '&Command=Record')}

function postRecord (url,args)
 {http_result = false;
  if (window.XMLHttpRequest)
     {http_result = new XMLHttpRequest();
      if (http_result.overrideMimeType)
         {http_result.overrideMimeType('text/xml');}}
  else if (window.ActiveXObject)
          {try {http_result = new ActiveXObject('Msxml2.XMLHTTP')}
           catch (e) {try {http_result = new ActiveXObject('Microsoft.XMLHTTP')}
                      catch (e) {} }}
  http_result.onreadystatechange = alertRecord;
  http_result.open('POST', url, true);
  http_result.send(args);}

function alertRecord()
 {if (http_result.readyState == 4)
     {if (http_result.responseText)
         {showsaved()}
      else {alert('There was a problem with the request in alertResult.')}}}

//------------------------------------------------------------------------------

function newFormStructure (fobj)
 {var str = '';
  str += '(' + fobj.elements[0].value;
  str += ' ' + fobj.elements[1].value;
  for (var i = 2; i < fobj.elements.length; i++)
      {if (fobj.elements[i].getAttribute('qualifier') == 'skip') {str = str}
       else if (fobj.elements[i].type == 'select-one')
               {var val = fobj.elements[i].options[fobj.elements[i].selectedIndex].value;
                str += ' ' + listify(fobj.elements[i].name, escape(val))}
       else if (fobj.elements[i].type == 'select-multiple')
               {for (var j = 0; j < fobj.elements[i].options.length; j++)
                    {if (fobj.elements[i].options[j].selected)
                        {var val = fobj.elements[i].options[j].value;
                         str += ' ' + listify(fobj.elements[i].name, val)}}}
       else if (fobj.elements[i].type == 'radio')
               {if (fobj.elements[i].checked)
                   {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
       else if (fobj.elements[i].type == 'checkbox')
               {if (fobj.elements[i].checked)
                   {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
       else if (fobj.elements[i].type == 'button')
               {str = str}
       else if (fobj.elements[i].getAttribute('qualifier') == 'string')
               {if (fobj.elements[i].value == '')
                   {str +=  ' ' + listify(fobj.elements[i].name, '')}
                else {str += ' ' + listify(fobj.elements[i].name, stringize(fobj.elements[i].value))}}
       else {str += ' ' + listify(fobj.elements[i].name, escape(fobj.elements[i].value))}}
  str = str + ')';
  return str}

//------------------------------------------------------------------------------

