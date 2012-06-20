var PHP = new PHP_JS();
var TLH = new TLH_builtins();

var univ;
var useshortnames = false;

var component_username = new set("username");
var component_name = new set("name");
var component_pass = new set("pass");
var component_pass2 = new set("pass2");
var component_birthmonth = new set("birthmonth");
var component_birthday = new set("birthday");
var component_birthyear = new set("birthyear");
var component_address = new set("address");
var component_city = new set("city");
var component_state = new set("state");
var component_country = new set("country");
var component_zip = new set("zip");
var component_telephone = new set("telephone");
var component_newsletter = new set("newsletter");
var component_accttype = new set("accttype");
var dependent = new Object();
dependent["username"] = component_username;
dependent["name"] = component_name;
dependent["pass"] = component_pass;
dependent["pass2"] = component_pass2;
dependent["birthmonth"] = component_birthmonth;
dependent["birthday"] = component_birthday;
dependent["birthyear"] = component_birthyear;
dependent["address"] = component_address;
dependent["city"] = component_city;
dependent["state"] = component_state;
dependent["country"] = component_country;
dependent["zip"] = component_zip;
dependent["telephone"] = component_telephone;
dependent["newsletter"] = component_newsletter;
dependent["accttype"] = component_accttype;

var prettynames = new Object();
prettynames[undefined] = '';
prettynames[''] = '';
prettynames['1'] = '1';
prettynames['false'] = 'false';
prettynames['true'] = 'true';
prettynames['buyer'] = 'buyer';
prettynames['buyertoseller'] = 'buyertoseller';

var uglynames = new Object();
uglynames[''] = '';
uglynames['1'] = '1';
uglynames['false'] = 'false';
uglynames['true'] = 'true';
uglynames['buyer'] = 'buyer';
uglynames['buyertoseller'] = 'buyertoseller';

function init () { 
   completep = true;
   casesensitive = true;
   allowconflicts = true;
   debug = false;
   valuecount = 6;
   init_index();
   univ = ds.get(datastore, "univ");
   var mycellarray = new Array();
   mycellarray[0] = new cell('username', "string", "textbox", negs_username, undefined, negsupps_username, undefined, negsuppx_username, undefined, component_username, true, undefined, true);
   mycellarray[1] = new cell('name', "string", "textbox", negs_name, undefined, negsupps_name, undefined, negsuppx_name, undefined, component_name, true, undefined, true);
   mycellarray[2] = new cell('pass', "string", "textbox", negs_pass, undefined, negsupps_pass, undefined, negsuppx_pass, undefined, component_pass, true, undefined, true);
   mycellarray[3] = new cell('pass2', "string", "textbox", negs_pass2, undefined, negsupps_pass2, undefined, negsuppx_pass2, undefined, component_pass2, true, undefined, true);
   mycellarray[4] = new cell('birthmonth', "string", "textbox", negs_birthmonth, undefined, negsupps_birthmonth, undefined, negsuppx_birthmonth, undefined, component_birthmonth, true, undefined, true);
   mycellarray[5] = new cell('birthday', "string", "textbox", negs_birthday, undefined, negsupps_birthday, undefined, negsuppx_birthday, undefined, component_birthday, true, undefined, true);
   mycellarray[6] = new cell('birthyear', "string", "textbox", negs_birthyear, undefined, negsupps_birthyear, undefined, negsuppx_birthyear, undefined, component_birthyear, true, undefined, true);
   mycellarray[7] = new cell('address', "string", "textbox", negs_address, undefined, negsupps_address, undefined, negsuppx_address, undefined, component_address, true, undefined, true);
   mycellarray[8] = new cell('city', "string", "textbox", negs_city, undefined, negsupps_city, undefined, negsuppx_city, undefined, component_city, true, undefined, true);
   mycellarray[9] = new cell('state', "string", "textbox", negs_state, undefined, negsupps_state, undefined, negsuppx_state, undefined, component_state, true, undefined, true);
   mycellarray[10] = new cell('country', "string", "textbox", negs_country, undefined, negsupps_country, undefined, negsuppx_country, undefined, component_country, true, undefined, true);
   mycellarray[11] = new cell('zip', "string", "textbox", negs_zip, undefined, negsupps_zip, undefined, negsuppx_zip, undefined, component_zip, true, undefined, true);
   mycellarray[12] = new cell('telephone', "string", "textbox", negs_telephone, undefined, negsupps_telephone, undefined, negsuppx_telephone, undefined, component_telephone, true, undefined, true);
   mycellarray[13] = new cell('newsletter', "string", "textbox", negs_newsletter, undefined, negsupps_newsletter, undefined, negsuppx_newsletter, undefined, component_newsletter, true, undefined, true);
   mycellarray[14] = new cell('accttype', "string", "textbox", negs_accttype, undefined, negsupps_accttype, undefined, negsuppx_accttype, undefined, component_accttype, true, undefined, true);
   initspread(mycellarray); }

function external_init () {  }

function submitprep () { 
   var s = new set();
   if (!(findcell("username").conflictset.empty())) {
      s.adjoin("username");}
   if (!(findcell("name").conflictset.empty())) {
      s.adjoin("name");}
   if (!(findcell("pass").conflictset.empty())) {
      s.adjoin("pass");}
   if (!(findcell("pass2").conflictset.empty())) {
      s.adjoin("pass2");}
   if (!(findcell("birthmonth").conflictset.empty())) {
      s.adjoin("birthmonth");}
   if (!(findcell("birthday").conflictset.empty())) {
      s.adjoin("birthday");}
   if (!(findcell("birthyear").conflictset.empty())) {
      s.adjoin("birthyear");}
   if (!(findcell("address").conflictset.empty())) {
      s.adjoin("address");}
   if (!(findcell("city").conflictset.empty())) {
      s.adjoin("city");}
   if (!(findcell("state").conflictset.empty())) {
      s.adjoin("state");}
   if (!(findcell("country").conflictset.empty())) {
      s.adjoin("country");}
   if (!(findcell("zip").conflictset.empty())) {
      s.adjoin("zip");}
   if (!(findcell("telephone").conflictset.empty())) {
      s.adjoin("telephone");}
   if (!(findcell("newsletter").conflictset.empty())) {
      s.adjoin("newsletter");}
   if (!(findcell("accttype").conflictset.empty())) {
      s.adjoin("accttype");}
   if (!(s.empty())) {
      alert('Errors remain.  You must fix them before submitting.');
      return false;}
   clear_forced_gui();
   return true; }

var datastore;

function init_index () { 
   datastore = new dictionary();
   ds.set(datastore, "univ", new hashbag(new expr(''), new expr('1'), new expr('false'), new expr('true'), new expr('buyer'), new expr('buyertoseller')));
   ds.set(datastore, "boolean", new set(new expr('false'), new expr('true'))); }

var x_func = function (newval, support, sofar) { 

return new pair(true, newval); };

var s_func = function (newval, support, sofar) { 

return new pair(false, ds.adjoin(sofar, newval, equalp)); };

var suppx_func = function (newval, support, sofar) { 

return new pair(true, new pair(newval, support)); };

var supps_func = function (newval, support, sofar) { 

return new pair(false, ds.adjoin(sofar, new pair(newval, support), equalp)); };

function check_univ (x0) { return ds.member(ds.get(datastore, "univ"), new expr(x0), equalp); }

function check_boolean (x0) { return ds.member(ds.get(datastore, "boolean"), new expr(x0), equalp); }

function enum_univ (x0) { 
   
   return ds.get(datastore, "univ"); }

function enum_boolean (x0) { 
   
   return ds.get(datastore, "boolean"); }

function negx_accttype (x0) { 
   var v = neg_accttype(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_accttype (x0) { return neg_accttype(s_func, x0); }

function negsuppx_accttype (x0) { return neg_accttype(suppx_func, x0); }

function negsupps_accttype (x0) { return neg_accttype(supps_func, x0); }

function neg_accttype (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_accttype_f === undefined))) return neg_accttype_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_accttype_b === undefined))) return neg_accttype_b(onsuccess, x0);
      else return undefined;} }

function neg_accttype_b (onsuccess, nsh626) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh626;
   if (!((x0 === 'buyer'))) {
      if (!((x0 === 'buyertoseller'))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}
      x0 = nsh626;}
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh626;}
   return sofar; }

function neg_accttype_f (onsuccess, nsh627) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh627;
   var arh627 = enum_univ(x0);
   for (var key in ds.data(arh627)) {
      x0 = ds.element(ds.element(arh627, key), 0);
      if (!((x0 === 'buyer'))) {
         if (!((x0 === 'buyertoseller'))) {
            tmp = onsuccess(new expr(x0), new set(), sofar);
            if ((ds.first(tmp) === true)) return ds.second(tmp);
            else sofar = ds.second(tmp);}}}
   x0 = nsh627;
   var arh628 = enum_univ(x0);
   for (var key in ds.data(arh628)) {
      x0 = ds.element(ds.element(arh628, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh627;
   return sofar; }

function negx_newsletter (x0) { 
   var v = neg_newsletter(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_newsletter (x0) { return neg_newsletter(s_func, x0); }

function negsuppx_newsletter (x0) { return neg_newsletter(suppx_func, x0); }

function negsupps_newsletter (x0) { return neg_newsletter(supps_func, x0); }

function neg_newsletter (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_newsletter_f === undefined))) return neg_newsletter_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_newsletter_b === undefined))) return neg_newsletter_b(onsuccess, x0);
      else return undefined;} }

function neg_newsletter_b (onsuccess, nsh628) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh628;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh628;}
   return sofar; }

function neg_newsletter_f (onsuccess, nsh629) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh629;
   var arh629 = enum_univ(x0);
   for (var key in ds.data(arh629)) {
      x0 = ds.element(ds.element(arh629, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh629;
   return sofar; }

function negx_telephone (x0) { 
   var v = neg_telephone(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_telephone (x0) { return neg_telephone(s_func, x0); }

function negsuppx_telephone (x0) { return neg_telephone(suppx_func, x0); }

function negsupps_telephone (x0) { return neg_telephone(supps_func, x0); }

function neg_telephone (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_telephone_f === undefined))) return neg_telephone_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_telephone_b === undefined))) return neg_telephone_b(onsuccess, x0);
      else return undefined;} }

function neg_telephone_b (onsuccess, nsh630) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh630;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh630;}
   return sofar; }

function neg_telephone_f (onsuccess, nsh631) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh631;
   var arh631 = enum_univ(x0);
   for (var key in ds.data(arh631)) {
      x0 = ds.element(ds.element(arh631, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh631;
   return sofar; }

function negx_zip (x0) { 
   var v = neg_zip(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_zip (x0) { return neg_zip(s_func, x0); }

function negsuppx_zip (x0) { return neg_zip(suppx_func, x0); }

function negsupps_zip (x0) { return neg_zip(supps_func, x0); }

function neg_zip (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_zip_f === undefined))) return neg_zip_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_zip_b === undefined))) return neg_zip_b(onsuccess, x0);
      else return undefined;} }

function neg_zip_b (onsuccess, nsh632) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh632;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh632;}
   return sofar; }

function neg_zip_f (onsuccess, nsh633) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh633;
   var arh633 = enum_univ(x0);
   for (var key in ds.data(arh633)) {
      x0 = ds.element(ds.element(arh633, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh633;
   return sofar; }

function negx_country (x0) { 
   var v = neg_country(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_country (x0) { return neg_country(s_func, x0); }

function negsuppx_country (x0) { return neg_country(suppx_func, x0); }

function negsupps_country (x0) { return neg_country(supps_func, x0); }

function neg_country (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_country_f === undefined))) return neg_country_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_country_b === undefined))) return neg_country_b(onsuccess, x0);
      else return undefined;} }

function neg_country_b (onsuccess, nsh634) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh634;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh634;}
   return sofar; }

function neg_country_f (onsuccess, nsh635) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh635;
   var arh635 = enum_univ(x0);
   for (var key in ds.data(arh635)) {
      x0 = ds.element(ds.element(arh635, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh635;
   return sofar; }

function negx_state (x0) { 
   var v = neg_state(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_state (x0) { return neg_state(s_func, x0); }

function negsuppx_state (x0) { return neg_state(suppx_func, x0); }

function negsupps_state (x0) { return neg_state(supps_func, x0); }

function neg_state (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_state_f === undefined))) return neg_state_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_state_b === undefined))) return neg_state_b(onsuccess, x0);
      else return undefined;} }

function neg_state_b (onsuccess, nsh636) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh636;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh636;}
   return sofar; }

function neg_state_f (onsuccess, nsh637) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh637;
   var arh637 = enum_univ(x0);
   for (var key in ds.data(arh637)) {
      x0 = ds.element(ds.element(arh637, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh637;
   return sofar; }

function negx_city (x0) { 
   var v = neg_city(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_city (x0) { return neg_city(s_func, x0); }

function negsuppx_city (x0) { return neg_city(suppx_func, x0); }

function negsupps_city (x0) { return neg_city(supps_func, x0); }

function neg_city (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_city_f === undefined))) return neg_city_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_city_b === undefined))) return neg_city_b(onsuccess, x0);
      else return undefined;} }

function neg_city_b (onsuccess, nsh638) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh638;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh638;}
   return sofar; }

function neg_city_f (onsuccess, nsh639) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh639;
   var arh639 = enum_univ(x0);
   for (var key in ds.data(arh639)) {
      x0 = ds.element(ds.element(arh639, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh639;
   return sofar; }

function negx_address (x0) { 
   var v = neg_address(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_address (x0) { return neg_address(s_func, x0); }

function negsuppx_address (x0) { return neg_address(suppx_func, x0); }

function negsupps_address (x0) { return neg_address(supps_func, x0); }

function neg_address (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_address_f === undefined))) return neg_address_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_address_b === undefined))) return neg_address_b(onsuccess, x0);
      else return undefined;} }

function neg_address_b (onsuccess, nsh640) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh640;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh640;}
   return sofar; }

function neg_address_f (onsuccess, nsh641) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh641;
   var arh641 = enum_univ(x0);
   for (var key in ds.data(arh641)) {
      x0 = ds.element(ds.element(arh641, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh641;
   return sofar; }

function negx_birthyear (x0) { 
   var v = neg_birthyear(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_birthyear (x0) { return neg_birthyear(s_func, x0); }

function negsuppx_birthyear (x0) { return neg_birthyear(suppx_func, x0); }

function negsupps_birthyear (x0) { return neg_birthyear(supps_func, x0); }

function neg_birthyear (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_birthyear_f === undefined))) return neg_birthyear_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_birthyear_b === undefined))) return neg_birthyear_b(onsuccess, x0);
      else return undefined;} }

function neg_birthyear_b (onsuccess, nsh642) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh642;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh642;}
   return sofar; }

function neg_birthyear_f (onsuccess, nsh643) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh643;
   var arh643 = enum_univ(x0);
   for (var key in ds.data(arh643)) {
      x0 = ds.element(ds.element(arh643, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh643;
   return sofar; }

function negx_birthday (x0) { 
   var v = neg_birthday(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_birthday (x0) { return neg_birthday(s_func, x0); }

function negsuppx_birthday (x0) { return neg_birthday(suppx_func, x0); }

function negsupps_birthday (x0) { return neg_birthday(supps_func, x0); }

function neg_birthday (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_birthday_f === undefined))) return neg_birthday_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_birthday_b === undefined))) return neg_birthday_b(onsuccess, x0);
      else return undefined;} }

function neg_birthday_b (onsuccess, nsh644) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh644;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh644;}
   return sofar; }

function neg_birthday_f (onsuccess, nsh645) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh645;
   var arh645 = enum_univ(x0);
   for (var key in ds.data(arh645)) {
      x0 = ds.element(ds.element(arh645, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh645;
   return sofar; }

function negx_birthmonth (x0) { 
   var v = neg_birthmonth(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_birthmonth (x0) { return neg_birthmonth(s_func, x0); }

function negsuppx_birthmonth (x0) { return neg_birthmonth(suppx_func, x0); }

function negsupps_birthmonth (x0) { return neg_birthmonth(supps_func, x0); }

function neg_birthmonth (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_birthmonth_f === undefined))) return neg_birthmonth_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_birthmonth_b === undefined))) return neg_birthmonth_b(onsuccess, x0);
      else return undefined;} }

function neg_birthmonth_b (onsuccess, nsh646) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh646;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh646;}
   return sofar; }

function neg_birthmonth_f (onsuccess, nsh647) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh647;
   var arh647 = enum_univ(x0);
   for (var key in ds.data(arh647)) {
      x0 = ds.element(ds.element(arh647, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh647;
   return sofar; }

function negx_pass2 (x0) { 
   var v = neg_pass2(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_pass2 (x0) { return neg_pass2(s_func, x0); }

function negsuppx_pass2 (x0) { return neg_pass2(suppx_func, x0); }

function negsupps_pass2 (x0) { return neg_pass2(supps_func, x0); }

function neg_pass2 (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_pass2_f === undefined))) return neg_pass2_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_pass2_b === undefined))) return neg_pass2_b(onsuccess, x0);
      else return undefined;} }

function neg_pass2_b (onsuccess, nsh648) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh648;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh648;}
   return sofar; }

function neg_pass2_f (onsuccess, nsh649) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh649;
   var arh649 = enum_univ(x0);
   for (var key in ds.data(arh649)) {
      x0 = ds.element(ds.element(arh649, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh649;
   return sofar; }

function negx_pass (x0) { 
   var v = neg_pass(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_pass (x0) { return neg_pass(s_func, x0); }

function negsuppx_pass (x0) { return neg_pass(suppx_func, x0); }

function negsupps_pass (x0) { return neg_pass(supps_func, x0); }

function neg_pass (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_pass_f === undefined))) return neg_pass_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_pass_b === undefined))) return neg_pass_b(onsuccess, x0);
      else return undefined;} }

function neg_pass_b (onsuccess, nsh650) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh650;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh650;}
   return sofar; }

function neg_pass_f (onsuccess, nsh651) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh651;
   var arh651 = enum_univ(x0);
   for (var key in ds.data(arh651)) {
      x0 = ds.element(ds.element(arh651, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh651;
   return sofar; }

function negx_name (x0) { 
   var v = neg_name(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_name (x0) { return neg_name(s_func, x0); }

function negsuppx_name (x0) { return neg_name(suppx_func, x0); }

function negsupps_name (x0) { return neg_name(supps_func, x0); }

function neg_name (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_name_f === undefined))) return neg_name_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_name_b === undefined))) return neg_name_b(onsuccess, x0);
      else return undefined;} }

function neg_name_b (onsuccess, nsh652) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh652;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh652;}
   return sofar; }

function neg_name_f (onsuccess, nsh653) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh653;
   var arh653 = enum_univ(x0);
   for (var key in ds.data(arh653)) {
      x0 = ds.element(ds.element(arh653, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh653;
   return sofar; }

function negx_username (x0) { 
   var v = neg_username(x_func, x0);
   if ((v instanceof expr)) return v;
   else return false; }

function negs_username (x0) { return neg_username(s_func, x0); }

function negsuppx_username (x0) { return neg_username(suppx_func, x0); }

function negsupps_username (x0) { return neg_username(supps_func, x0); }

function neg_username (onsuccess, x0) { 
   if (varp(x0)) {
      if (!((neg_username_f === undefined))) return neg_username_f(onsuccess, x0);
      else return undefined;}
   else {
      if (!((neg_username_b === undefined))) return neg_username_b(onsuccess, x0);
      else return undefined;} }

function neg_username_b (onsuccess, nsh654) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh654;
   if (!((x0 === x0))) {
      tmp = onsuccess(new expr(x0), new set(), sofar);
      if ((ds.first(tmp) === true)) return ds.second(tmp);
      else sofar = ds.second(tmp);
      x0 = nsh654;}
   return sofar; }

function neg_username_f (onsuccess, nsh655) { 
   var sofar = new set();
   var tmp;
   var x0 = nsh655;
   var arh655 = enum_univ(x0);
   for (var key in ds.data(arh655)) {
      x0 = ds.element(ds.element(arh655, key), 0);
      if (!((x0 === x0))) {
         tmp = onsuccess(new expr(x0), new set(), sofar);
         if ((ds.first(tmp) === true)) return ds.second(tmp);
         else sofar = ds.second(tmp);}}
   x0 = nsh655;
   return sofar; }

function getBigBoxName (x) { return x + 'BIGBOX'; }
