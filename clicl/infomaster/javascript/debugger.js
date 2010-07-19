//------------------------------------------------------------------------------
// Debugger
//------------------------------------------------------------------------------

function tryit ()
 {var str = document.getElementById('scriptarea').value;
  document.getElementById('valuearea').innerHTML = eval(str)}

function ruletest ()
 {return grinddata(ruleclosure(definitions))}

function clausetest ()
 {clauses = clausify(closure(bodies(definitions)));
  return grinddata(clauses)}

function bodies (rules)
 {var ns = empty();
  for (var i = 0; i<rules.length; i++)
      {ns[i] = rules[i].slice(2)};
  return ns}

function clausify (clauses)
 {var ns = empty();
  for (var i = 0; i<clauses.length; i++)
      {ns[i] = seq('and').concat(clauses[i])};
  return ns}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
