;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (c) Copyright 1999-2005 by Michael Genesereth.  All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special *ancestry* *agent* *interface* *options*)))

(defparameter *version* 5.0)

(defparameter *standard* (make-instance 'interface :name 'standard))
(defparameter *integrator* (make-instance 'facilitator :name 'integrator))
(defparameter *repository* (make-instance 'dualserver :name 'repository))
(defparameter *warehouse* (make-instance 'dataserver :name 'warehouse))
(defparameter *library* (make-instance 'ruleserver :name 'library))
(defparameter *manager* (make-instance 'fastserver :name 'manager))
(defparameter *metalibrary* (make-instance 'ruleserver :name 'metalibrary))

(defparameter *myfactserver* (make-instance 'factserver :name 'myfactserver))
(defparameter *myviewserver* (make-instance 'viewserver :name 'myviewserver))
(defparameter *myfullserver* (make-instance 'fullserver :name 'myfullserver))
(defparameter *mydualserver* (make-instance 'dualserver :name 'mydualserver))
(defparameter *mydataserver* (make-instance 'dataserver :name 'mydataserver))
(defparameter *mydiffserver* (make-instance 'diffserver :name 'mydiffserver))
(defparameter *myfastserver* (make-instance 'fastserver :name 'myfastserver))
(defparameter *mytranslator* (make-instance 'translator :name 'mytranslator))
(defparameter *mytransformer* (make-instance 'transformer :name 'mytransformer))
(defparameter *myintegrator* (make-instance 'integrator :name 'myintegrator))
(defparameter *myfacilitator* (make-instance 'facilitator :name 'myfacilitator))

(setq *agent* *standard*)
(setq *interface* *manager*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sentences*
  '((isa           standard interface)
    (nameisglobal  standard no)

    (isa           integrator facilitator)
    (nameisglobal  integrator no)
    (rulebase      integrator library)
    (database      integrator warehouse)
    
    (isa           library ruleserver)
    (nameisglobal  library no)
    (nocommand     library apropos)

    (isa           warehouse dataserver)
    (nameisglobal  warehouse no)

    (isa           repository dualserver)
    (includee      repository library)
    (nameisglobal  repository no)

    (isa           myfactserver factserver)
    (isa           myviewserver viewserver)
    (isa           mydualserver dualserver)
    (isa           myfullserver fullserver)
    (isa           mydataserver dataserver)

    (isa           mydiffserver diffserver)
    (recipient     mydiffserver repository)

    (isa           myfastserver fastserver)
    (includee      myfastserver library)

    (isa           mytranslator translator)
    (rulebase      mytranslator library)
    (recipient     mytranslator repository)

    (isa           mytransformer transformer)
    (rulebase      mytransformer library)
    (recipient     mytransformer repository)

    (isa           myintegrator integrator)
    (rulebase      myintegrator library)
    (database      myintegrator repository)

    (isa           myfacilitator facilitator)
    (rulebase      myfacilitator library)
    (database      myfacilitator repository)

    (isa           manager fastserver)
    (nameisglobal  manager no)
    (includee      manager metalibrary)
    (nocreate      manager action)
    (nocreate      manager agent)
    (nocreate      manager parameter)
    (nocreate      manager relation)
    (capability    manager backwardrules)
    (capability    manager checkattribute)
    (capability    manager checkclass)
    (capability    manager checkdata)
    (capability    manager checkpredicate)
    (capability    manager checkrelation)
    (capability    manager checkrules)
    (capability    manager concordance)
    (capability    manager consolidaterules)
    (capability    manager consolidatedata)
    (capability    manager copyagentdata)
    (capability    manager copyclassdata)
    (capability    manager copytabledata)
    (capability    manager countrows)
    (capability    manager countcells)
    (capability    manager countbytes)
    (capability    manager dematerializeclass)
    (capability    manager dematerializeschema)
    (capability    manager dematerializetable)
    (capability    manager dematerializetree)
    (capability    manager dropagentdata)
    (capability    manager dropclassdata)
    (capability    manager droptabledata)
    (capability    manager dump)
    (capability    manager dumpclass)
    (capability    manager dumpagent)
    (capability    manager dumprelation)
    (capability    manager duplicate)
    (capability    manager evertrules)
    (capability    manager forwardrules)
    (capability    manager incorporaterules)
    (capability    manager initialize)
    (capability    manager integraterules)
    (capability    manager interrelaterules)
    (capability    manager introspect)
    (capability    manager load)
    (capability    manager loadfile)
    (capability    manager materializeclass)
    (capability    manager materializeschema)
    (capability    manager materializetable)
    (capability    manager materializetree)
    (capability    manager move)
    (capability    manager moveagentdata)
    (capability    manager moveclassdata)
    (capability    manager movetabledata)
    (capability    manager reifyrules)
    (capability    manager reifydata)
    (capability    manager removeduplicatedata)
    (capability    manager rename)
    (capability    manager resetagent)
    (capability    manager resetsystem)
    (capability    manager separaterules)
    (capability    manager separatedata)
    (capability    manager tabulaterules)
    (capability    manager triggerrules)
    (capability    manager unloadfile)

    (isa           metalibrary ruleserver)
    (nameisglobal  metalibrary no)
    (nocommand     metalibrary apropos)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (isa           aclserver class)
    (superclass    aclserver agent)
    (attribute     aclserver host)
    (attribute     aclserver port)
    (attribute     aclserver interest)
    (attribute     aclserver specialty)
    (attribute     aclserver responds)
    (attribute     aclserver performs)
    (attribute     aclserver classifier)
    (attribute     aclserver rootclass)
    (attribute     aclserver rootrelation)
    (attribute     aclserver capability)
    (attribute     aclserver nocreate)
    (attribute     aclserver nochange)
    (attribute     aclserver noupdate)
    (attribute     aclserver nocommand)
    (attribute     aclserver frontpage)
    (attribute     aclserver header)
    (attribute     aclserver footer)
    (attribute     aclserver stylesheet)
    (attribute     aclserver logfile)
    (attribute     aclserver security)

    (isa           action class)
    (superclass    action thing)
    (attribute     action argument)
    (attribute     action documentation)

    (isa           actioninvocation class)
    (superclass    actioninvocation parameter)

    (isa           agent class)
    (superclass    agent thing)
    (attribute     agent interest)
    (attribute     agent specialty)
    (attribute     agent responds)
    (attribute     agent performs)

    (isa           argumentrelation class)
    (superclass    argumentrelation relation)
    (attribute     argumentrelation argumenttype)
    (attribute     argumentrelation argumentstyle)
    (attribute     argumentrelation documentation)

    (isa           attributerelation class)
    (superclass    attributerelation relation)
    (attribute     attributerelation superrelation)
    (attribute     attributerelation domain)
    (attribute     attributerelation range)
    (attribute     attributerelation unique)
    (attribute     attributerelation total)
    (attribute     attributerelation createstyle)
    (attribute     attributerelation changestyle)
    (attribute     attributerelation searchstyle)
    (attribute     attributerelation comparestyle)
    (attribute     attributerelation inspectstyle)
    (attribute     attributerelation expander)
    (attribute     attributerelation option)
    (attribute     attributerelation searchdefault)
    (attribute     attributerelation changedefault)
    (attribute     attributerelation searchlabel)
    (attribute     attributerelation changelabel)
    (attribute     attributerelation documentation)

    (isa           authorizer class)
    (superclass    authorizer agent)
    (attribute     authorizer recipient)
    (attribute     authorizer interest)
    (attribute     authorizer specialty)
    (attribute     authorizer responds)
    (attribute     authorizer performs)
    (attribute     authorizer classifier)
    (attribute     authorizer rootclass)
    (attribute     authorizer nocreate)
    (attribute     authorizer nochange)
    (attribute     authorizer rootrelation)
    (attribute     authorizer noupdate)
    (attribute     authorizer capability)
    (attribute     authorizer frontpage)
    (attribute     authorizer header)
    (attribute     authorizer footer)
    (attribute     authorizer stylesheet)
    (attribute     authorizer nocommand)
    (attribute     authorizer logfile)
    (attribute     authorizer security)

    (isa           basket class)
    (superclass    basket agent)
    (attribute     basket interest)
    (attribute     basket specialty)
    (attribute     basket responds)
    (attribute     basket performs)
    (attribute     basket classifier)
    (attribute     basket rootclass)
    (attribute     basket nocreate)
    (attribute     basket nochange)
    (attribute     basket rootrelation)
    (attribute     basket noupdate)
    (attribute     basket capability)
    (attribute     basket frontpage)
    (attribute     basket header)
    (attribute     basket footer)
    (attribute     basket stylesheet)
    (attribute     basket nocommand)
    (attribute     basket logfile)
    (attribute     basket security)

    (isa           boolean class)
    (superclass    boolean parameter)

    (isa           character class)
    (superclass    character parameter)

    (isa           class class)
    (superclass    class thing)
    (attribute     class superclass)
    (attribute     class predicate)
    (attribute     class attribute)
    (attribute     class sorter)
    (attribute     class documentation)

    (isa           command class)
    (superclass    command parameter)

    (isa           counter class)
    (superclass    counter agent)
    (attribute     counter specialty)

    (isa           dataserver class)
    (superclass    dataserver agent)
    (attribute     dataserver interest)
    (attribute     dataserver specialty)
    (attribute     dataserver responds)
    (attribute     dataserver performs)
    (attribute     dataserver classifier)
    (attribute     dataserver rootclass)
    (attribute     dataserver rootrelation)
    (attribute     dataserver capability)
    (attribute     dataserver nocreate)
    (attribute     dataserver nochange)
    (attribute     dataserver noupdate)
    (attribute     dataserver nocommand)
    (attribute     dataserver frontpage)
    (attribute     dataserver header)
    (attribute     dataserver footer)
    (attribute     dataserver stylesheet)
    (attribute     dataserver logfile)
    (attribute     dataserver security)

    (isa           diskserver class)
    (superclass    diskserver agent)
    (attribute     diskserver interest)
    (attribute     diskserver specialty)
    (attribute     diskserver responds)
    (attribute     diskserver performs)
    (attribute     diskserver classifier)
    (attribute     diskserver rootclass)
    (attribute     diskserver rootrelation)
    (attribute     diskserver capability)
    (attribute     diskserver nocreate)
    (attribute     diskserver nochange)
    (attribute     diskserver noupdate)
    (attribute     diskserver nocommand)
    (attribute     diskserver frontpage)
    (attribute     diskserver header)
    (attribute     diskserver footer)
    (attribute     diskserver stylesheet)
    (attribute     diskserver logfile)
    (attribute     diskserver security)

    (isa           distributor class)
    (superclass    distributor agent)
    (attribute     distributor interest)
    (attribute     distributor specialty)
    (attribute     distributor responds)
    (attribute     distributor performs)
    (attribute     distributor classifier)
    (attribute     distributor rulebase)
    (attribute     distributor recipient)
    (attribute     distributor rootclass)
    (attribute     distributor nocreate)
    (attribute     distributor nochange)
    (attribute     distributor rootrelation)
    (attribute     distributor noupdate)
    (attribute     distributor capability)
    (attribute     distributor frontpage)
    (attribute     distributor header)
    (attribute     distributor footer)
    (attribute     distributor stylesheet)
    (attribute     distributor nocommand)
    (attribute     distributor logfile)
    (attribute     distributor security)

    (isa           dualserver class)
    (superclass    dualserver agent)
    (attribute     dualserver interest)
    (attribute     dualserver specialty)
    (attribute     dualserver responds)
    (attribute     dualserver performs)
    (attribute     dualserver classifier)
    (attribute     dualserver rootclass)
    (attribute     dualserver rootrelation)
    (attribute     dualserver capability)
    (attribute     dualserver nocreate)
    (attribute     dualserver nochange)
    (attribute     dualserver noupdate)
    (attribute     dualserver nocommand)
    (attribute     dualserver frontpage)
    (attribute     dualserver header)
    (attribute     dualserver footer)
    (attribute     dualserver stylesheet)
    (attribute     dualserver logfile)
    (attribute     dualserver security)

    (isa           evaluablerelation class)
    (superclass    evaluablerelation relation)
    (attribute     evaluablerelation superrelation)
    (attribute     evaluablerelation arity)
    (attribute     evaluablerelation column)
    (attribute     evaluablerelation key)
    (attribute     evaluablerelation documentation)

    (isa           facilitator class)
    (superclass    facilitator agent)
    (attribute     facilitator interest)
    (attribute     facilitator specialty)
    (attribute     facilitator responds)
    (attribute     facilitator performs)
    (attribute     facilitator classifier)
    (attribute     facilitator rulebase)
    (attribute     facilitator recipient)
    (attribute     facilitator rootclass)
    (attribute     facilitator rootrelation)
    (attribute     facilitator capability)
    (attribute     facilitator nocreate)
    (attribute     facilitator nochange)
    (attribute     facilitator noupdate)
    (attribute     facilitator nocommand)
    (attribute     facilitator frontpage)
    (attribute     facilitator header)
    (attribute     facilitator footer)
    (attribute     facilitator stylesheet)
    (attribute     facilitator logfile)
    (attribute     facilitator security)

    (isa           factserver class)
    (superclass    factserver agent)
    (attribute     factserver interest)
    (attribute     factserver specialty)
    (attribute     factserver responds)
    (attribute     factserver performs)
    (attribute     factserver classifier)
    (attribute     factserver rootclass)
    (attribute     factserver rootrelation)
    (attribute     factserver capability)
    (attribute     factserver nocreate)
    (attribute     factserver nochange)
    (attribute     factserver noupdate)
    (attribute     factserver nocommand)
    (attribute     factserver frontpage)
    (attribute     factserver header)
    (attribute     factserver footer)
    (attribute     factserver stylesheet)
    (attribute     factserver logfile)
    (attribute     factserver security)

    (isa           fastserver class)
    (superclass    fastserver agent)
    (attribute     fastserver interest)
    (attribute     fastserver specialty)
    (attribute     fastserver responds)
    (attribute     fastserver performs)
    (attribute     fastserver classifier)
    (attribute     fastserver rootclass)
    (attribute     fastserver rootrelation)
    (attribute     fastserver capability)
    (attribute     fastserver nocreate)
    (attribute     fastserver nochange)
    (attribute     fastserver noupdate)
    (attribute     fastserver nocommand)
    (attribute     fastserver frontpage)
    (attribute     fastserver header)
    (attribute     fastserver footer)
    (attribute     fastserver stylesheet)
    (attribute     fastserver logfile)
    (attribute     fastserver security)

    (isa           fullserver class)
    (superclass    fullserver agent)
    (attribute     fullserver interest)
    (attribute     fullserver specialty)
    (attribute     fullserver responds)
    (attribute     fullserver performs)
    (attribute     fullserver classifier)
    (attribute     fullserver rootclass)
    (attribute     fullserver rootrelation)
    (attribute     fullserver capability)
    (attribute     fullserver nocreate)
    (attribute     fullserver nochange)
    (attribute     fullserver noupdate)
    (attribute     fullserver nocommand)
    (attribute     fullserver frontpage)
    (attribute     fullserver header)
    (attribute     fullserver footer)
    (attribute     fullserver stylesheet)
    (attribute     fullserver logfile)
    (attribute     fullserver security)

    (isa           interface class)
    (superclass    interface agent)
    (attribute     interface rootclass)
    (attribute     interface rootrelation)
    (attribute     interface capability)
    (attribute     interface frontpage)
    (attribute     interface header)
    (attribute     interface footer)
    (attribute     interface stylesheet)
    (attribute     interface nocreate)
    (attribute     interface nochange)
    (attribute     interface noupdate)
    (attribute     interface nocommand)
    (attribute     interface logfile)
    (attribute     interface metadata)
    (attribute     interface security)
    (attribute     interface recipient)

    (isa           integrator class)
    (superclass    integrator agent)
    (attribute     integrator interest)
    (attribute     integrator specialty)
    (attribute     integrator responds)
    (attribute     integrator performs)
    (attribute     integrator classifier)
    (attribute     integrator rulebase)
    (attribute     integrator recipient)
    (attribute     integrator rootclass)
    (attribute     integrator rootrelation)
    (attribute     integrator capability)
    (attribute     integrator nocreate)
    (attribute     integrator nochange)
    (attribute     integrator noupdate)
    (attribute     integrator nocommand)
    (attribute     integrator frontpage)
    (attribute     integrator header)
    (attribute     integrator footer)
    (attribute     integrator stylesheet)
    (attribute     integrator logfile)
    (attribute     integrator security)

    (isa           language class)
    (superclass    language parameter)

    (isa           modality class)
    (superclass    modality parameter)

    (isa           naryrelation class)
    (superclass    naryrelation relation)
    (attribute     naryrelation superrelation)
    (attribute     naryrelation arity)
    (attribute     naryrelation column)
    (attribute     naryrelation key)
    (attribute     naryrelation documentation)

    (isa           number class)
    (superclass    number parameter)

    (isa           parameter class)
    (superclass    parameter thing)

    (isa           predicaterelation class)
    (superclass    predicaterelation relation)
    (attribute     predicaterelation superrelation)
    (attribute     predicaterelation domain)
    (attribute     predicaterelation documentation)

    (isa           relation class)
    (superclass    relation thing)
    (attribute     relation superrelation)
    (attribute     relation arity)
    (attribute     relation column)
    (attribute     relation key)
    (attribute     relation documentation)

    (isa           ruleserver class)
    (superclass    ruleserver agent)
    (attribute     ruleserver interest)
    (attribute     ruleserver specialty)
    (attribute     ruleserver responds)
    (attribute     ruleserver performs)
    (attribute     ruleserver classifier)
    (attribute     ruleserver rootclass)
    (attribute     ruleserver rootrelation)
    (attribute     ruleserver capability)
    (attribute     ruleserver nocreate)
    (attribute     ruleserver nochange)
    (attribute     ruleserver noupdate)
    (attribute     ruleserver nocommand)
    (attribute     ruleserver frontpage)
    (attribute     ruleserver header)
    (attribute     ruleserver footer)
    (attribute     ruleserver stylesheet)
    (attribute     ruleserver logfile)
    (attribute     ruleserver security)

    (isa           soapserver class)
    (superclass    soapserver agent)
    (attribute     soapserver host)
    (attribute     soapserver port)
    (attribute     soapserver interest)
    (attribute     soapserver specialty)
    (attribute     soapserver responds)
    (attribute     soapserver performs)
    (attribute     soapserver classifier)
    (attribute     soapserver rootclass)
    (attribute     soapserver rootrelation)
    (attribute     soapserver capability)
    (attribute     soapserver nocreate)
    (attribute     soapserver nochange)
    (attribute     soapserver noupdate)
    (attribute     soapserver nocommand)
    (attribute     soapserver frontpage)
    (attribute     soapserver header)
    (attribute     soapserver footer)
    (attribute     soapserver stylesheet)
    (attribute     soapserver logfile)
    (attribute     soapserver security)

    (isa           sqlserver class)
    (superclass    sqlserver agent)
    (attribute     sqlserver host)
    (attribute     sqlserver port)
    (attribute     sqlserver interest)
    (attribute     sqlserver specialty)
    (attribute     sqlserver responds)
    (attribute     sqlserver performs)
    (attribute     sqlserver classifier)
    (attribute     sqlserver rootclass)
    (attribute     sqlserver nocreate)
    (attribute     sqlserver nochange)
    (attribute     sqlserver rootrelation)
    (attribute     sqlserver noupdate)
    (attribute     sqlserver capability)
    (attribute     sqlserver frontpage)
    (attribute     sqlserver header)
    (attribute     sqlserver footer)
    (attribute     sqlserver stylesheet)
    (attribute     sqlserver nocommand)
    (attribute     sqlserver logfile)
    (attribute     sqlserver security)

    (isa           string class)
    (superclass    string parameter)

    (isa           thing class)

    (isa           transformer class)
    (superclass    transformer agent)
    (attribute     transformer rulebase)
    (attribute     transformer recipient)
    (attribute     transformer interest)
    (attribute     transformer specialty)
    (attribute     transformer responds)
    (attribute     transformer performs)
    (attribute     transformer classifier)
    (attribute     transformer rootclass)
    (attribute     transformer nocreate)
    (attribute     transformer nochange)
    (attribute     transformer rootrelation)
    (attribute     transformer noupdate)
    (attribute     transformer capability)
    (attribute     transformer frontpage)
    (attribute     transformer header)
    (attribute     transformer footer)
    (attribute     transformer stylesheet)
    (attribute     transformer nocommand)
    (attribute     transformer logfile)
    (attribute     transformer security)

    (isa           translator class)
    (superclass    translator agent)
    (attribute     translator rulebase)
    (attribute     translator recipient)
    (attribute     translator interest)
    (attribute     translator specialty)
    (attribute     translator responds)
    (attribute     translator performs)
    (attribute     translator classifier)
    (attribute     translator rootclass)
    (attribute     translator nocreate)
    (attribute     translator nochange)
    (attribute     translator rootrelation)
    (attribute     translator noupdate)
    (attribute     translator capability)
    (attribute     translator frontpage)
    (attribute     translator header)
    (attribute     translator footer)
    (attribute     translator stylesheet)
    (attribute     translator nocommand)
    (attribute     translator logfile)
    (attribute     translator security)

    (isa           viewserver class)
    (superclass    viewserver agent)
    (attribute     viewserver interest)
    (attribute     viewserver specialty)
    (attribute     viewserver responds)
    (attribute     viewserver performs)
    (attribute     viewserver classifier)
    (attribute     viewserver rootclass)
    (attribute     viewserver rootrelation)
    (attribute     viewserver capability)
    (attribute     viewserver nocreate)
    (attribute     viewserver nochange)
    (attribute     viewserver noupdate)
    (attribute     viewserver nocommand)
    (attribute     viewserver frontpage)
    (attribute     viewserver header)
    (attribute     viewserver footer)
    (attribute     viewserver stylesheet)
    (attribute     viewserver logfile)
    (attribute     viewserver security)

    (isa           weekday class)
    (superclass    weekday parameter)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (isa           accessible attributerelation)
    (superrelation accessible true)
    (domain        accessible agent)
    (range         accessible string)
    (unique        accessible no)
    (total         accessible no)
    (createstyle   accessible stringfield)
    (changestyle   accessible stringfield)
    (searchstyle   accessible stringfield)
    (comparestyle  accessible noshow)
    (inspectstyle  accessible glyph)
    (documentation accessible "IP address from which an interface can be accessed.")

    (isa           argument attributerelation)
    (superrelation argument true)
    (domain        argument action)
    (range         argument argumentrelation)
    (unique        argument no)
    (total         argument no)
    (createstyle   argument selector)
    (changestyle   argument selector)
    (searchstyle   argument selector)
    (comparestyle  argument glyph)
    (inspectstyle  argument glyph)

    (isa           argumenttype attributerelation)
    (superrelation argumenttype true)
    (domain        argumenttype argumentrelation)
    (range         argumenttype class)
    (unique        argumenttype yes)
    (total         argumenttype yes)
    (createstyle   argumenttype selector)
    (changestyle   argumenttype selector)
    (searchstyle   argumenttype selector)
    (comparestyle  argumenttype glyph)
    (inspectstyle  argumenttype glyph)

    (isa           argumentstyle attributerelation)
    (superrelation argumentstyle true)
    (domain        argumentstyle argumentrelation)
    (range         argumentstyle modality)
    (unique        argumentstyle yes)
    (total         argumentstyle yes)
    (createstyle   argumentstyle selector)
    (changestyle   argumentstyle selector)
    (searchstyle   argumentstyle selector)
    (comparestyle  argumentstyle glyph)
    (inspectstyle  argumentstyle glyph)

    (isa           arity attributerelation)
    (superrelation arity true)
    (domain        arity relation)
    (range         arity number)
    (unique        arity yes)
    (total         arity yes)
    (createstyle   arity typein)
    (changestyle   arity typein)
    (searchstyle   arity interval)
    (comparestyle  arity glyph)
    (inspectstyle  arity glyph)
    (documentation arity "Number of arguments to a relation.")

    (isa           attribute attributerelation)
    (superrelation attribute true)
    (domain        attribute class)
    (range         attribute attributerelation)
    (unique        attribute no)
    (total         attribute no)
    (createstyle   attribute selector)
    (changestyle   attribute selector)
    (searchstyle   attribute selector)
    (comparestyle  attribute glyph)
    (inspectstyle  attribute glyph)
    (documentation attribute "Attribute relation relevant to the specified class.")

    (isa           capability attributerelation)
    (superrelation capability true)
    (domain        capability agent)
    (range         capability action)
    (unique        capability no)
    (total         capability no)
    (createstyle   capability selector)
    (changestyle   capability selector)
    (searchstyle   capability selector)
    (comparestyle  capability noshow)
    (inspectstyle  capability glyph)
    (documentation capability "Action relevant to this interface.")

    (isa           casename attributerelation)
    (superrelation casename true)
    (domain        casename thing)
    (range         casename string)
    (unique        casename yes)
    (total         casename no)
    (createstyle   casename stringfield)
    (changestyle   casename stringfield)
    (searchstyle   casename stringfield)
    (comparestyle  casename glyph)
    (inspectstyle  casename glyph)
    (casename      casename "CaseName")
    (documentation casename "Mixed case string that is string-equal to the name of the term giving the ideal case convention for display.")

    (isa           changedefault attributerelation)
    (superrelation changedefault true)
    (domain        changedefault relation)
    (range         changedefault thing)
    (unique        changedefault yes)
    (total         changedefault no)
    (createstyle   changedefault typein)
    (changestyle   changedefault typein)
    (searchstyle   changedefault typein)
    (comparestyle  changedefault noshow)
    (inspectstyle  changedefault typein)
    (documentation changedefault "Default input value for an attribute.")

    (isa           changelabel attributerelation)
    (superrelation changelabel true)
    (domain        changelabel relation)
    (range         changelabel string)
    (unique        changelabel yes)
    (total         changelabel no)
    (createstyle   changelabel stringfield)
    (changestyle   changelabel stringfield)
    (searchstyle   changelabel stringfield)
    (comparestyle  changelabel noshow)
    (inspectstyle  changelabel glyph)
    (documentation changelabel "Label to print on change pages.")

    (isa           changestyle attributerelation)
    (superrelation changestyle true)
    (domain        changestyle attributerelation)
    (range         changestyle modality)
    (unique        changestyle yes)
    (total         changestyle yes)
    (createstyle   changestyle selector)
    (changestyle   changestyle selector)
    (searchstyle   changestyle selector)
    (comparestyle  changestyle noshow)
    (inspectstyle  changestyle glyph)
    (documentation changestyle "Format for entry and editing of an attribute.")

    (isa           choice attributerelation)
    (superrelation choice true)
    (domain        choice argumentrelation)
    (range         choice thing)
    (unique        choice no)
    (total         choice no)
    (createstyle   choice typein)
    (changestyle   choice typein)
    (searchstyle   choice typein)
    (comparestyle  choice noshow)
    (inspectstyle  choice glyph)
    (documentation choice "Possible value for an argument")

    (isa           classifier attributerelation)
    (superrelation classifier true)
    (domain        classifier agent)
    (range         classifier naryrelation)
    (unique        classifier yes)
    (total         classifier no)
    (createstyle   classifier selector)
    (changestyle   classifier selector)
    (searchstyle   classifier selector)
    (comparestyle  classifier noshow)
    (inspectstyle  classifier glyph)
    (documentation classifier "Isa relation for an agent.")

    (isa           column attributerelation)
    (superrelation column true)
    (domain        column relation)
    (range         column attributerelation)
    (unique        column no)
    (total         column no)
    (createstyle   column selector)
    (changestyle   column selector)
    (searchstyle   column selector)
    (comparestyle  column noshow)
    (inspectstyle  column glyph)
    (documentation column "Attribute of a relation.")

    (isa convertfromstring evaluablerelation)
    (arity         convertfromstring 2)

    (isa           createstyle attributerelation)
    (superrelation createstyle true)
    (domain        createstyle attributerelation)
    (range         createstyle modality)
    (unique        createstyle yes)
    (total         createstyle yes)
    (createstyle   createstyle selector)
    (changestyle   createstyle selector)
    (searchstyle   createstyle selector)
    (comparestyle  createstyle noshow)
    (inspectstyle  createstyle glyph)
    (documentation createstyle "Format for attribute in creating an object.")

    (isa           comparestyle attributerelation)
    (superrelation comparestyle true)
    (domain        comparestyle attributerelation)
    (range         comparestyle modality)
    (unique        comparestyle yes)
    (total         comparestyle yes)
    (createstyle   comparestyle selector)
    (changestyle   comparestyle selector)
    (searchstyle   comparestyle selector)
    (comparestyle  comparestyle noshow)
    (inspectstyle  comparestyle glyph)
    (option        comparestyle glyph)
    (option        comparestyle noshow)
    (documentation comparestyle "Format for entry and editing of an attribute.")

    (isa           database attributerelation)
    (superrelation database true)
    (domain        database agent)
    (range         database dataserver)
    (unique        database yes)
    (total         database no)
    (createstyle   database selector)
    (changestyle   database selector)
    (searchstyle   database selector)
    (comparestyle  database glyph)
    (inspectstyle  database glyph)
    (documentation database "Database associated with an agent.")

    (isa           derivative attributerelation)
    (superrelation derivative true)
    (domain        derivative agent)
    (range         derivative ruleserver)
    (unique        derivative yes)
    (total         derivative no)
    (createstyle   derivative selector)
    (changestyle   derivative selector)
    (searchstyle   derivative selector)
    (comparestyle  derivative glyph)
    (inspectstyle  derivative glyph)

    (isa           distinct evaluablerelation)
    (superrelation distinct true)
    (arity         distinct 2)

    (isa           documentation attributerelation)
    (superrelation documentation true)
    (domain        documentation thing)
    (range         documentation string)
    (unique        documentation yes)
    (total         documentation no)
    (createstyle   documentation text)
    (changestyle   documentation text)
    (searchstyle   documentation text)
    (comparestyle  documentation noshow)
    (inspectstyle  documentation glyph)
    (documentation documentation "Short description of a thing.")

    (isa           domain attributerelation)
    (superrelation domain true)
    (domain        domain relation)
    (range         domain class)
    (unique        domain yes)
    (total         domain no)
    (createstyle   domain selector)
    (changestyle   domain selector)
    (searchstyle   domain selector)
    (comparestyle  domain glyph)
    (inspectstyle  domain glyph)
    (documentation domain "Class containing the values for which the attribute is defined.")

    (isa           expander attributerelation)
    (superrelation expander true)
    (domain        expander relation)
    (range         expander relation)
    (unique        expander no)
    (total         expander no)
    (createstyle   expander selector)
    (changestyle   expander selector)
    (searchstyle   expander selector)
    (comparestyle  expander noshow)
    (inspectstyle  expander glyph)
    (documentation expander "A relation that relates values of the hierarchy for a hierarchicalselector.  Smaller to larger.")

    (isa           extension attributerelation)
    (superrelation extension true)
    (domain        extension agent)
    (range         extension relation)
    (unique        extension no)
    (total         extension no)
    (createstyle   extension selector)
    (changestyle   extension selector)
    (searchstyle   extension selector)
    (comparestyle  extension glyph)
    (inspectstyle  extension glyph)
    (documentation extension "Relation defined by an agent purely extensionally.")

    (isa           footer attributerelation)
    (superrelation footer true)
    (domain        footer agent)
    (range         footer string)
    (unique        footer yes)
    (total         footer no)
    (createstyle   footer stringfield)
    (changestyle   footer stringfield)
    (searchstyle   footer noshow)
    (comparestyle  footer noshow)
    (inspectstyle  footer glyph)
    (documentation footer "Name of file containing text to be used at the bottom of a page.")

    (isa           frontpage attributerelation)
    (superrelation frontpage true)
    (domain        frontpage agent)
    (range         frontpage string)
    (unique        frontpage yes)
    (total         frontpage no)
    (createstyle   frontpage stringfield)
    (changestyle   frontpage stringfield)
    (searchstyle   frontpage noshow)
    (comparestyle  frontpage noshow)
    (inspectstyle  frontpage glyph)
    (documentation frontpage "Name of file containing text to be returned by agent when no operation is specified.")

    (isa           header attributerelation)
    (superrelation header true)
    (domain        header agent)
    (range         header string)
    (unique        header yes)
    (total         header no)
    (createstyle   header stringfield)
    (changestyle   header stringfield)
    (searchstyle   header noshow)
    (comparestyle  header noshow)
    (inspectstyle  header glyph)
    (documentation header "Name of file containing text to be used at the top of a page.")

    (isa           host attributerelation)
    (superrelation host true)
    (domain        host agent)
    (range         host string)
    (unique        host yes)
    (total         host no)
    (createstyle   host stringfield)
    (changestyle   host stringfield)
    (searchstyle   host stringfield)
    (comparestyle  host glyph)
    (inspectstyle  host glyph)
    (documentation host "Name of machine on which the database resides.")

    (isa           includee attributerelation)
    (superrelation includee true)
    (domain        includee agent)
    (range         includee agent)
    (unique        includee no)
    (total         includee no)
    (createstyle   includee selector)
    (changestyle   includee selector)
    (searchstyle   includee selector)
    (comparestyle  includee glyph)
    (inspectstyle  includee glyph)
    (documentation includee "Agent whose data is included in agent.")

    (isa           inspectstyle attributerelation)
    (superrelation inspectstyle true)
    (domain        inspectstyle attributerelation)
    (range         inspectstyle modality)
    (unique        inspectstyle yes)
    (total         inspectstyle yes)
    (createstyle   inspectstyle selector)
    (changestyle   inspectstyle selector)
    (searchstyle   inspectstyle selector)
    (comparestyle  inspectstyle noshow)
    (inspectstyle  inspectstyle glyph)
    (option        inspectstyle glyph)
    (option        inspectstyle noshow)
    (documentation inspectstyle "Format for entry and editing of an attribute.")

    (isa           interest attributerelation)
    (superrelation interest true)
    (domain        interest agent)
    (range         interest relation)
    (unique        interest no)
    (total         interest no)
    (createstyle   interest selector)
    (changestyle   interest selector)
    (searchstyle   interest selector)
    (comparestyle  interest glyph)
    (inspectstyle  interest glyph)
    (documentation interest "Relation for which database wishes updates.")

    (isa           isa naryrelation)
    (superrelation isa true)
    (arity         isa 2)
    (documentation isa "Preferred class of which the specified object is a member.")

    (isa           isastring evaluablerelation)
    (arity         isastring 1)

    (isa           key attributerelation)
    (superrelation key true)
    (domain        key relation)
    (range         key relation)
    (unique        key no)
    (total         key no)
    (createstyle   key selector)
    (changestyle   key selector)
    (searchstyle   key selector)
    (comparestyle  key noshow)
    (inspectstyle  key glyph)
    (documentation key "Key attribute for a relation.")

    (isa           logfile attributerelation)
    (superrelation logfile true)
    (domain        logfile agent)
    (range         logfile string)
    (unique        logfile yes)
    (total         logfile no)
    (createstyle   logfile stringfield)
    (changestyle   logfile stringfield)
    (searchstyle   logfile noshow)
    (comparestyle  logfile noshow)
    (inspectstyle  logfile glyph)
    (documentation logfile "Name of file in which updates are logged.  No log file means no logging.")

    (isa           meta attributerelation)
    (superrelation meta true)
    (domain        meta agent)
    (range         meta boolean)
    (unique        meta yes)
    (total         meta no)
    (createstyle   meta selector)
    (changestyle   meta selector)
    (searchstyle   meta selector)
    (comparestyle  meta glyph)
    (inspectstyle  meta glyph)
    (option        meta static)
    (option        meta dynamic)
    (documentation meta "Yes means meta information updated automatically; no means no automatic update.")

    (isa           metadata attributerelation)
    (superrelation metadata true)
    (domain        metadata interface)
    (range         metadata agent)
    (unique        metadata yes)
    (total         metadata no)
    (createstyle   metadata selector)
    (changestyle   metadata selector)
    (searchstyle   metadata selector)
    (comparestyle  metadata glyph)
    (inspectstyle  metadata glyph)
    (documentation metadata "Database containing metadata for an interface.")

    (isa           nameisglobal attributerelation) 
    (superrelation nameisglobal true)
    (domain        nameisglobal agent) 
    (range         nameisglobal boolean)
    (unique        nameisglobal yes)
    (total         nameisglobal yes)
    (createstyle   nameisglobal selector)
    (changestyle   nameisglobal selector)
    (searchstyle   nameisglobal selector)
    (comparestyle  nameisglobal glyph)
    (inspectstyle  nameisglobal glyph)
    (documentation nameisglobal "Global name of an agent.")

    (isa nochange attributerelation)
    (superrelation nochange true)
    (domain        nochange agent)
    (range         nochange class)
    (unique        nochange no)
    (total         nochange no)
    (createstyle   nochange selector)
    (changestyle   nochange selector)
    (searchstyle   nochange selector)
    (comparestyle  nochange noshow)
    (inspectstyle  nochange glyph)
    (documentation nochange "Class for which the user may not modify instances.")

    (isa           nocommand attributerelation)
    (superrelation nocommand true)
    (domain        nocommand agent)
    (range         nocommand command)
    (unique        nocommand no)
    (total         nocommand no)
    (createstyle   nocommand selector)
    (changestyle   nocommand selector)
    (searchstyle   nocommand selector)
    (comparestyle  nocommand noshow)
    (inspectstyle  nocommand glyph)
    (documentation nocommand "Control button that is not to be displayed.")

    (isa           nocreate attributerelation)
    (superrelation nocreate true)
    (domain        nocreate agent)
    (range         nocreate class)
    (unique        nocreate no)
    (total         nocreate no)
    (createstyle   nocreate selector)
    (changestyle   nocreate selector)
    (searchstyle   nocreate selector)
    (comparestyle  nocreate noshow)
    (inspectstyle  nocreate glyph)
    (documentation nocreate "Class for which user may not create or delete instances.")

    (isa           noupdate attributerelation)
    (superrelation noupdate true)
    (domain        noupdate agent)
    (range         noupdate relation)
    (unique        noupdate no)
    (total         noupdate no)
    (createstyle   noupdate selector)
    (changestyle   noupdate selector)
    (searchstyle   noupdate selector)
    (comparestyle  noupdate noshow)
    (inspectstyle  noupdate glyph)
    (documentation noupdate "Relation for which user may not insert or delete tuples.")

    (isa           oneof evaluablerelation)
    (superrelation oneof true)
    (arity         oneof -1)

    (isa           option attributerelation)
    (superrelation option true)
    (domain        option relation)
    (range         option thing)
    (unique        option no)
    (total         option no)
    (createstyle   option typein)
    (changestyle   option typein)
    (searchstyle   option typein)
    (comparestyle  option noshow)
    (inspectstyle  option glyph)
    (documentation option "Possible value for an attribute.")

    (isa           performs attributerelation)
    (superrelation performs true)
    (domain        performs agent)
    (range         performs action)
    (unique        performs no)
    (total         performs no)
    (createstyle   performs selector)
    (changestyle   performs selector)
    (searchstyle   performs selector)
    (comparestyle  performs glyph)
    (inspectstyle  performs glyph)
    (documentation performs "Operation database can perform.")

    (isa           port attributerelation)
    (superrelation port true)
    (domain        port agent)
    (range         port number)
    (unique        port yes)
    (total         port no)
    (createstyle   port typein)
    (changestyle   port typein)
    (searchstyle   port interval)
    (comparestyle  port glyph)
    (inspectstyle  port glyph)
    (documentation port "Port to use for communication with a database.")

    (isa           predicate attributerelation)
    (superrelation predicate true)
    (domain        predicate class)
    (range         predicate predicaterelation)
    (unique        predicate yes)
    (total         predicate no)
    (createstyle   predicate selector)
    (changestyle   predicate selector)
    (searchstyle   predicate selector)
    (comparestyle  predicate glyph)
    (inspectstyle  predicate glyph)
    (documentation predicate "Predicate for a class.")

    (isa           prettyname attributerelation)
    (superrelation prettyname true)
    (domain        prettyname thing)
    (range         prettyname string)
    (unique        prettyname yes)
    (total         prettyname no)
    (createstyle   prettyname stringfield)
    (changestyle   prettyname stringfield)
    (searchstyle   prettyname noshow)
    (comparestyle  prettyname noshow)
    (inspectstyle  prettyname noshow)
    (documentation prettyname "HTML string to use for displaying an object.")

    (isa           pwd attributerelation)
    (superrelation pwd true)
    (domain        pwd thing)
    (range         pwd string)
    (unique        pwd yes)
    (total         pwd yes)
    (createstyle   pwd password)
    (changestyle   pwd password)
    (searchstyle   pwd noshow)
    (comparestyle  pwd glyph)
    (inspectstyle  pwd glyph)
    (documentation pwd "Password for a user.")

    (isa           range attributerelation)
    (superrelation range true)
    (domain        range relation)
    (range         range class)
    (unique        range yes)
    (total         range no)
    (createstyle   range selector)
    (changestyle   range selector)
    (searchstyle   range selector)
    (comparestyle  range glyph)
    (inspectstyle  range glyph)
    (documentation range "Class containing all values for an attribute.")

    (isa           recent attributerelation)
    (superrelation recent true)
    (domain        recent thing)
    (range         recent number)
    (unique        recent yes)
    (total         recent no)
    (createstyle   recent password)
    (changestyle   recent password)
    (searchstyle   recent noshow)
    (comparestyle  recent glyph)
    (inspectstyle  recent glyph)
    (documentation recent "Number of seconds sinnce last connection qualifying as recent.
All values must be included, either explicitly (e.g. 1, 2, 3) or implicitly (via rules).")

    (isa           recipient attributerelation)
    (superrelation recipient true)
    (domain        recipient agent)
    (range         recipient agent)
    (unique        recipient yes)
    (total         recipient no)
    (createstyle   recipient selector)
    (changestyle   recipient selector)
    (searchstyle   recipient selector)
    (comparestyle  recipient glyph)
    (inspectstyle  recipient glyph)
    (documentation recipient "Agent that receives messages from this agent.")

    (isa           responds attributerelation)
    (superrelation responds true)
    (domain        responds agent)
    (range         responds action)
    (unique        responds no)
    (total         responds no)
    (createstyle   responds selector)
    (changestyle   responds selector)
    (searchstyle   responds selector)
    (comparestyle  responds glyph)
    (inspectstyle  responds glyph)
    (documentation responds "Event about which database wishes to be notified.")

    (isa           rootclass attributerelation)
    (superrelation rootclass true)
    (domain        rootclass agent)
    (range         rootclass class)
    (unique        rootclass no)
    (total         rootclass no)
    (createstyle   rootclass selector)
    (changestyle   rootclass selector)
    (searchstyle   rootclass selector)
    (comparestyle  rootclass glyph)
    (inspectstyle  rootclass glyph)
    (documentation rootclass "Class meaningful to this interface.")

    (isa           rootrelation attributerelation)
    (superrelation rootrelation true)
    (domain        rootrelation agent)
    (range         rootrelation relation)
    (unique        rootrelation no)
    (total         rootrelation no)
    (createstyle   rootrelation selector)
    (changestyle   rootrelation selector)
    (searchstyle   rootrelation selector)
    (comparestyle  rootrelation glyph)
    (inspectstyle  rootrelation glyph)
    (documentation rootrelation "Relation relevant to this interface.")

    (isa           rulebase attributerelation)
    (superrelation rulebase true)
    (domain        rulebase agent)
    (range         rulebase ruleserver)
    (unique        rulebase yes)
    (total         rulebase no)
    (createstyle   rulebase selector)
    (changestyle   rulebase selector)
    (searchstyle   rulebase selector)
    (comparestyle  rulebase glyph)
    (inspectstyle  rulebase glyph)
    (documentation rulebase "Ruleserver associated with this agent.")

    (isa           same evaluablerelation)
    (arity         same 2)

    (isa           searchdefault attributerelation)
    (superrelation searchdefault true)
    (domain        searchdefault attributerelation)
    (range         searchdefault thing)
    (unique        searchdefault yes)
    (total         searchdefault no)
    (createstyle   searchdefault typein)
    (changestyle   searchdefault typein)
    (searchstyle   searchdefault typein)
    (comparestyle  searchdefault noshow)
    (inspectstyle  searchdefault glyph)
    (documentation searchdefault "Default search value for an attribute.")

    (isa           searchlabel attributerelation)
    (superrelation searchlabel true)
    (domain        searchlabel relation)
    (range         searchlabel string)
    (unique        searchlabel yes)
    (total         searchlabel no)
    (createstyle   searchlabel stringfield)
    (changestyle   searchlabel stringfield)
    (searchstyle   searchlabel stringfield)
    (comparestyle  searchlabel noshow)
    (inspectstyle  searchlabel glyph)
    (documentation searchlabel "Label to print on search pages.")

    (isa           searchstyle attributerelation)
    (superrelation searchstyle true)
    (domain        searchstyle attributerelation)
    (range         searchstyle modality)
    (unique        searchstyle yes)
    (total         searchstyle yes)
    (createstyle   searchstyle selector)
    (changestyle   searchstyle selector)
    (searchstyle   searchstyle selector)
    (comparestyle  searchstyle noshow)
    (inspectstyle  searchstyle glyph)
    (documentation searchstyle "Format for entry and editing of an attribute.")

    (isa           security attributerelation)
    (superrelation security true)
    (domain        security agent)
    (range         security agent)
    (unique        security yes)
    (total         security no)
    (createstyle   security selector)
    (changestyle   security selector)
    (searchstyle   security selector)
    (comparestyle  security noshow)
    (inspectstyle  security glyph)
    (documentation security "Security database associated with this agent.")

    (isa           sorter attributerelation)
    (superrelation sorter true)
    (domain        sorter class)
    (range         sorter attributerelation)
    (unique        sorter no)
    (total         sorter no)
    (createstyle   sorter selector)
    (changestyle   sorter selector)
    (searchstyle   sorter selector)
    (comparestyle  sorter noshow)
    (inspectstyle  sorter glyph)
    (documentation sorter "Attribute of a class used for sorting.")

    (isa           specialist attributerelation)
    (superrelation specialist true)
    (domain        specialist relation)
    (range         specialist agent)
    (unique        specialist no)
    (total         specialist no)
    (createstyle   specialist selector)
    (changestyle   specialist selector)
    (searchstyle   specialist selector)
    (comparestyle  specialist glyph)
    (inspectstyle  specialist glyph)
    (documentation specialist "Agent that has complete information about relation.")

    (isa           specialty attributerelation)
    (superrelation specialty true)
    (domain        specialty agent)
    (range         specialty relation)
    (unique        specialty no)
    (total         specialty no)
    (createstyle   specialty selector)
    (changestyle   specialty selector)
    (searchstyle   specialty selector)
    (comparestyle  specialty glyph)
    (inspectstyle  specialty glyph)
    (documentation specialty "Relation about which agent has complete information.")

    (isa           stringalphanumeric evaluablerelation)
    (arity         stringalphanumeric 2)

    (isa           stringappend evaluablerelation)
    (arity         stringappend 3)

    (isa           stringcapitalize evaluablerelation)
    (arity         stringcapitalize 2)

    (isa           stringcharpos evaluablerelation)
    (arity         stringcharpos 3)

    (isa           stringdowncase evaluablerelation)
    (arity         stringdowncase 2)

    (isa           stringelement evaluablerelation)
    (arity         stringelement 3)

    (isa           stringgreater evaluablerelation)
    (arity         stringgreater 2)

    (isa           stringify evaluablerelation)
    (arity         stringify 2)

    (isa           stringlength evaluablerelation)
    (arity         stringlength 2)

    (isa           stringless evaluablerelation)
    (arity         stringless 2)

    (isa           stringmatch evaluablerelation)
    (arity         stringmatch 4)

    (isa           stringmatch3 evaluablerelation)
    (arity         stringmatch3 3)

    (isa           stringmatch4 evaluablerelation)
    (arity         stringmatch4 4)

    (isa           stringmatch5 evaluablerelation)
    (arity         stringmatch5 5)

    (isa           stringmatch6 evaluablerelation)
    (arity         stringmatch6 6)

    (isa           stringmatchall evaluablerelation)
    (arity         stringmatchall 2)

    (isa           stringmatchany evaluablerelation)
    (arity         stringmatchany 2)

    (isa           stringmatchphrase evaluablerelation)
    (arity         stringmatchphrase 2)

    (isa           stringposition evaluablerelation)
    (arity         stringposition 3)

    (isa           stringsubseq evaluablerelation)
    (arity         stringsubseq 4)

    (isa           stringsubstitute evaluablerelation)
    (arity         stringsubstitute 4)

    (isa           stringupcase evaluablerelation)
    (arity         stringupcase 2)

    (isa           stylesheet attributerelation)
    (superrelation stylesheet true)
    (domain        stylesheet agent)
    (range         stylesheet string)
    (unique        stylesheet yes)
    (total         stylesheet no)
    (createstyle   stylesheet text)
    (changestyle   stylesheet text)
    (searchstyle   stylesheet noshow)
    (comparestyle  stylesheet noshow)
    (inspectstyle  stylesheet glyph)

    (isa           substring evaluablerelation)
    (arity         substring 2)

    (isa           superclass attributerelation)
    (superrelation superclass true)
    (domain        superclass class)
    (range         superclass class)
    (unique        superclass no)
    (total         superclass no)
    (createstyle   superclass selector)
    (changestyle   superclass selector)
    (searchstyle   superclass selector)
    (comparestyle  superclass glyph)
    (inspectstyle  superclass glyph)
    (documentation superclass "Superclass of the specified class.")

    (isa           superrelation attributerelation)
    (superrelation superrelation true)
    (domain        superrelation relation)
    (range         superrelation relation)
    (unique        superrelation no)
    (total         superrelation no)
    (createstyle   superrelation selector)
    (changestyle   superrelation selector)
    (searchstyle   superrelation selector)
    (comparestyle  superrelation glyph)
    (inspectstyle  superrelation glyph)
    (documentation superrelation "Relation of which specified relation is a refinement.")

    (isa           symbolize evaluablerelation)
    (arity         symbolize 2)

    (isa           total attributerelation)
    (superrelation total true)
    (domain        total attributerelation)
    (range         total boolean)
    (unique        total yes)
    (total         total yes)
    (createstyle   total selector)
    (changestyle   total selector)
    (searchstyle   total selector)
    (comparestyle  total glyph)
    (inspectstyle  total glyph)
    (option        total yes)
    (option        total no)
    (documentation total "A relation always has a value if and only if there is at least one value in its range for every argument in its domain.")

    (isa           true naryrelation)
    (arity         true -1)
    (documentation true "Relation true of all tuples.  Top of relational heterarchy.")

    (isa           unique attributerelation)
    (superrelation unique true)
    (domain        unique attributerelation)
    (range         unique boolean)
    (unique        unique yes)
    (total         unique yes)
    (createstyle   unique selector)
    (changestyle   unique selector)
    (searchstyle   unique selector)
    (comparestyle  unique glyph)
    (inspectstyle  unique glyph)
    (option        unique yes)
    (option        unique no)
    (documentation unique "A relation is unique if and only if there is at most one value in its range for every argument in its domain.")

    (isa           + evaluablerelation)
    (arity         + 3)

    (isa           - evaluablerelation)
    (arity         - 3)

    (isa           * evaluablerelation)
    (arity         * 3)

    (isa           / evaluablerelation)
    (arity         / 3)

    (isa           < evaluablerelation)
    (arity         < 2)

    (isa           > evaluablerelation)
    (arity         > 2)

    (isa           =< evaluablerelation)
    (arity         =< 2)

    (isa           >= evaluablerelation)
    (arity         >= 2)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (isa           applytransforms action)
    (argument      applytransforms applytransforms.source)
    (argument      applytransforms applytransforms.target)

    (isa           applytransforms.source argumentrelation)
    (argumenttype  applytransforms.source basket)
    (argumentstyle applytransforms.source selector)

    (isa           applytransforms.target argumentrelation)
    (argumenttype  applytransforms.target agent)
    (argumentstyle applytransforms.target selector)

    ;;;;

    (isa           backwardrules action)
    (argument      backwardrules backwardrules.relation)
    (argument      backwardrules backwardrules.source)
    (argument      backwardrules backwardrules.target)
    (documentation backwardrules
      "<B>backwardrules</B> takes as argument a relation and two ruleservers.  It writes
the definition of the specified relation as given in the first ruleserver as backward
rules and places the resulting rules in the second ruleserver.  If no target ruleserver is
specified, there are no side effects and a list of the backward rules is returned.")

    (isa           backwardrules.relation argumentrelation)
    (argumenttype  backwardrules.relation relation)
    (argumentstyle backwardrules.relation selector)

    (isa           backwardrules.source argumentrelation)
    (argumenttype  backwardrules.source basket)
    (argumentstyle backwardrules.source selector)

    (isa           backwardrules.target argumentrelation)
    (argumenttype  backwardrules.target basket)
    (argumentstyle backwardrules.target selector)

    ;;;;

    (isa           checkattribute action)
    (argument      checkattribute checkattribute.attribute)
    (argument      checkattribute checkattribute.agent)
    (documentation checkattribute
      "<B>checkattribute</B> is an action.  It takes an attribute and an agent as arguments and
       examines contents for arity, domain, range, uniqueness, and totality.  Checkattribute works
       only with local databases.")

    (isa           checkattribute.attribute argumentrelation)
    (domain        checkattribute.attribute actioninvocation)
    (argumenttype  checkattribute.attribute attributerelation)
    (argumentstyle checkattribute.attribute selector)

    (isa           checkattribute.agent argumentrelation)
    (domain        checkattribute.agent actioninvocation)
    (argumenttype  checkattribute.agent agent)
    (argumentstyle checkattribute.agent selector)

    ;;;;

    (isa           checkclass action)
    (argument      checkclass checkclass.class)
    (argument      checkclass checkclass.agent)
    (documentation checkclass
      "<B>checkclass</B> is an action.  It takes a class and an agent as arguments and
       checks the predicate and attribute of the class.  checkclass works
       only with local databases.")

    (isa           checkclass.class argumentrelation)
    (argumenttype  checkclass.class class)
    (argumentstyle checkclass.class selector)

    (isa           checkclass.agent argumentrelation)
    (argumenttype  checkclass.agent agent)
    (argumentstyle checkclass.agent selector)

    ;;;;

    (isa           checkdata action)
    (argument      checkdata checkdata.source)
    (documentation checkdata
      "<B>checkdata</B> is an action.  It takes an agent as argument and
       examines contents for arity and uniqueness.  Checkdata works
       only with local databases.")

    (isa           checkdata.source argumentrelation)
    (argumenttype  checkdata.source agent)
    (argumentstyle checkdata.source selector)

    ;;;;

    (isa           checkpredicate action)
    (argument      checkpredicate checkpredicate.predicate)
    (argument      checkpredicate checkpredicate.agent)
    (documentation checkpredicate
      "<B>checkpredicate</B> is an action.  It takes a predicate and an agent as arguments and
       examines the predicate for arity and domain.  checkpredicate works only with local
       databases.")

    (isa           checkpredicate.predicate argumentrelation)
    (argumenttype  checkpredicate.predicate predicaterelation)
    (argumentstyle checkpredicate.predicate selector)

    (isa           checkpredicate.agent argumentrelation)
    (argumenttype  checkpredicate.agent agent)
    (argumentstyle checkpredicate.agent selector)

    ;;;;

    (isa           checkrelation action)
    (argument      checkrelation checkrelation.relation)
    (argument      checkrelation checkrelation.agent)
    (documentation checkrelation
      "<B>checkrelation</B> is an action.  It takes a relation and an agent as arguments and
       examines contents for arity.  checkrelation works only with local databases.")

    (isa           checkrelation.relation argumentrelation)
    (argumenttype  checkrelation.relation relation)
    (argumentstyle checkrelation.relation selector)

    (isa           checkrelation.agent argumentrelation)
    (argumenttype  checkrelation.agent agent)
    (argumentstyle checkrelation.agent selector)

    ;;;;

    (isa           checkrules action)
    (argument      checkrules checkrules.source)
    (documentation checkrules
      "<B>checkrules</B> is an action.  It takes a ruleserver as argument and
       check for a variety of bad or inefficient conditions and prints out
       warnings for each condition found.")

    (isa           checkrules.source argumentrelation)
    (argumenttype  checkrules.source basket)
    (argumentstyle checkrules.source selector)
 
    ;;;;

    (isa           concordance action)
    (argument      concordance concordance.attribute)
    (argument      concordance concordance.source)
    (argument      concordance concordance.map)
    (argument      concordance concordance.target)
    
    (isa           concordance.attribute argumentrelation)
    (argumenttype  concordance.attribute attributerelation)
    (argumentstyle concordance.attribute selector)

    (isa           concordance.source argumentrelation)
    (argumenttype  concordance.source agent)
    (argumentstyle concordance.source selector)
    
    (isa           concordance.map argumentrelation)
    (argumenttype  concordance.map attributerelation)
    (argumentstyle concordance.map typein)

    (isa           concordance.target argumentrelation)
    (argumenttype  concordance.target agent)
    (argumentstyle concordance.target selector)

    ;;;;

    (isa           consolidatedata action)
    (argument      consolidatedata consolidatedata.relation)
    (argument      consolidatedata consolidatedata.source)
    (argument      consolidatedata consolidatedata.target)
    (documentation consolidatedata
      "<B>consolidatedata</B> takes a class, an agent, and a dataserver as arguments.
It mateializes the membership relation and attributes of the class from the source to the target.")

    (isa           consolidatedata.relation argumentrelation)
    (argumenttype  consolidatedata.relation relation)
    (argumentstyle consolidatedata.relation selector)

    (isa           consolidatedata.source argumentrelation)
    (argumenttype  consolidatedata.source dataserver)
    (argumentstyle consolidatedata.source selector)

    (isa           consolidatedata.target argumentrelation)
    (argumenttype  consolidatedata.target dataserver)
    (argumentstyle consolidatedata.target selector)

    ;;;;

    (isa           consolidaterules action)
    (argument consolidaterules consolidaterules.relation)
    (argument consolidaterules consolidaterules.target)
    (documentation consolidaterules
      "<B>consolidaterules</B> takes a relation and a ruleserver as arguments.  It
writes rules to define  the relation in terms of relations for all non-key
attributes written as functions of the key attributes and saves the rules in
the specified ruleserver.  If no ruleserver is specified, there is no side effect
and a list of the resulting rules is returned as value.")

    (isa           consolidaterules.relation argumentrelation)
    (argumenttype  consolidaterules.relation relation)
    (argumentstyle consolidaterules.relation selector)

    (isa           consolidaterules.target argumentrelation)
    (argumenttype  consolidaterules.target basket)
    (argumentstyle consolidaterules.target selector)

    ;;;;

    (isa           copyclassdata action)
    (argument copyclassdata copyclassdata.class)
    (argument copyclassdata copyclassdata.source)
    (argument copyclassdata copyclassdata.target)
    (documentation copyclassdata
      "<B>CopyClassData</B> is an action.  It takes as arguments a class, a
       source agent, and a target agent.  It copies the class membership relation
       and all attributes form the specified class from the source to
       the target.")

    (isa           copyclassdata.class argumentrelation)
    (argumenttype  copyclassdata.class class)
    (argumentstyle copyclassdata.class selector)

    (isa           copyclassdata.source argumentrelation)
    (argumenttype  copyclassdata.source agent)
    (argumentstyle copyclassdata.source selector)

    (isa           copyclassdata.target argumentrelation)
    (argumenttype  copyclassdata.target agent)
    (argumentstyle copyclassdata.target selector)

    ;;;;

    (isa           copyagentdata action)
    (argument      copyagentdata copyagentdata.source)
    (argument      copyagentdata copyagentdata.target)
    (documentation copyagentdata
      "<B>CopyAgentData</B> is an action.  It takes as arguments a
       source agent and a target agent.  It copies the contents
       from teh source agent to the target agent.")

    (isa           copyagentdata.source argumentrelation)
    (argumenttype  copyagentdata.source agent)
    (argumentstyle copyagentdata.source selector)

    (isa           copyagentdata.target argumentrelation)
    (argumenttype  copyagentdata.target agent)
    (argumentstyle copyagentdata.target selector)

    ;;;;

    (isa           copytabledata action)
    (argument copytabledata copytabledata.relation)
    (argument copytabledata copytabledata.source)
    (argument copytabledata copytabledata.target)
    (documentation copytabledata
      "<B>CopyTableData</B> is an action.  It takes as arguments a relation, a
       source agent, and a target agent.  It retrieves all rows of
       the specified table from the source agent and forwards the
       information to the target agent.")

    (isa           copytabledata.relation argumentrelation)
    (argumenttype  copytabledata.relation relation)
    (argumentstyle copytabledata.relation selector)

    (isa           copytabledata.source argumentrelation)
    (argumenttype  copytabledata.source agent)
    (argumentstyle copytabledata.source selector)

    (isa           copytabledata.target argumentrelation)
    (argumenttype  copytabledata.target agent)
    (argumentstyle copytabledata.target selector)

    ;;;;

    (isa           countbytes action)
    (argument countbytes countbytes.source)
    (documentation countbytes
      "<B>countbytes</B> takes an agent as argument and returns the total number
of bytes in all tables stored in the agent")

    (isa           countbytes.source argumentrelation)
    (argumenttype  countbytes.source basket)
    (argumentstyle countbytes.source selector)

    ;;;;

    (isa           countcells action)
    (argument      countcells countcells.source)
    (documentation countcells "<B>CountCells</B> takes an agent as argument and returns the total number of cells consumed by all tables stored in the agent.")

    (isa           countcells.source argumentrelation)
    (argumenttype  countcells.source basket)
    (argumentstyle countcells.source selector)

    ;;;;

    (isa           countrows action)
    (argument countrows countrows.source)
    (documentation countrows
      "<B>countrows</B> takes an agent as argument and returns the total number
of rows in all tables stored in the agent")

    (isa           countrows.source argumentrelation)
    (argumenttype  countrows.source basket)
    (argumentstyle countrows.source selector)

    ;;;;

    (isa           dematerializeclass action)
    (argument dematerializeclass dematerializeclass.class)
    (argument dematerializeclass dematerializeclass.target)
    (documentation dematerializeclass
      "<B>dematerializeclass</B> takes as arguments a class and a &quot;target&quot;
       agent.  It eliminates all instances of the class from the target agent.")

    (isa           dematerializeclass.class argumentrelation)
    (argumenttype  dematerializeclass.class class)
    (argumentstyle dematerializeclass.class selector)

    (isa           dematerializeclass.target argumentrelation)
    (argumenttype  dematerializeclass.target basket)
    (argumentstyle dematerializeclass.target selector)

    ;;;;

    (isa           dematerializeschema action)
    (argument      dematerializeschema dematerializeschema.class)
    (argument      dematerializeschema dematerializeschema.target)

    (isa           dematerializeschema.class argumentrelation)
    (argumenttype  dematerializeschema.class class)
    (argumentstyle dematerializeschema.class selector)

    (isa           dematerializeschema.target argumentrelation)
    (argumenttype  dematerializeschema.target basket)
    (argumentstyle dematerializeschema.target selector)

    ;;;;

    (isa           dematerializetable action)
    (argument      dematerializetable dematerializetable.relation)
    (argument      dematerializetable dematerializetable.target)

    (isa           dematerializetable.relation argumentrelation)
    (argumenttype  dematerializetable.relation relation)
    (argumentstyle dematerializetable.relation selector)

    (isa           dematerializetable.target argumentrelation)
    (argumenttype  dematerializetable.target basket)
    (argumentstyle dematerializetable.target selector)

    ;;;;

    (isa           dematerializetree action)
    (argument      dematerializetree dematerializetree.class)
    (argument      dematerializetree dematerializetree.target)

    (isa           dematerializetree.class argumentrelation)
    (argumenttype  dematerializetree.class class)
    (argumentstyle dematerializetree.class selector)

    (isa           dematerializetree.target argumentrelation)
    (argumenttype  dematerializetree.target basket)
    (argumentstyle dematerializetree.target selector)

    ;;;;

    (isa           dropclassdata action)
    (argument dropclassdata dropclassdata.class)
    (argument dropclassdata dropclassdata.target)
    (documentation dropclassdata
      "<B>DropClassData</B> is an action.  It takes as arguments a class
       and a target agent.  It drops the class membership relation and all
       attributes from the specified agent.")

    (isa           dropclassdata.class argumentrelation)
    (argumenttype  dropclassdata.class class)
    (argumentstyle dropclassdata.class selector)

    (isa           dropclassdata.target argumentrelation)
    (argumenttype  dropclassdata.target agent)
    (argumentstyle dropclassdata.target selector)

    ;;;;

    (isa           dropagentdata action)
    (argument dropagentdata dropagentdata.target)
    (documentation dropagentdata
      "<B>DropAgentData</B> is an action.  It takes an agent as argument
       and empties the specified agent.")

    (isa           dropagentdata.target argumentrelation)
    (argumenttype  dropagentdata.target agent)
    (argumentstyle dropagentdata.target selector)

    ;;;;

    (isa           droptabledata action)
    (argument droptabledata droptabledata.relation)
    (argument droptabledata droptabledata.target)
    (documentation droptabledata
      "<B>DropTableData</B> is an action.  It takes as arguments a relation
       and a target agent.  It eliminate all rows of
       the specified table from the target agent.")

    (isa           droptabledata.relation argumentrelation)
    (argumenttype  droptabledata.relation relation)
    (argumentstyle droptabledata.relation selector)

    (isa           droptabledata.target argumentrelation)
    (argumenttype  droptabledata.target agent)
    (argumentstyle droptabledata.target selector)

    ;;;;

    (isa           dump action)
    (argument dump dump.filename)
    (documentation dump
      "<B>dump</B> takes a filename as argument and dumps all information in thes
       system into the specified file in a way that allows the system to be
       recreated by using the load command.")

    (isa           dump.filename argumentrelation)
    (argumenttype  dump.filename string)
    (argumentstyle dump.filename stringfield)

    ;;;;

    (isa           dumpagent action)
    (argument      dumpagent dumpagent.source)
    (argument      dumpagent dumpagent.filename)
    (argument      dumpagent dumpagent.language)
    (documentation dumpagent
      "<B>dumpagent</B> takes as arguments an agent, a filename, and a language.
       It dumps the contents of the agent's database to the specified file in
       the specified language.")

    (isa           dumpagent.source argumentrelation)
    (argumenttype  dumpagent.source basket)
    (argumentstyle dumpagent.source selector)

    (isa           dumpagent.filename argumentrelation)
    (argumenttype  dumpagent.filename string)
    (argumentstyle dumpagent.filename stringfield)

    (isa           dumpagent.language argumentrelation)
    (argumenttype  dumpagent.language language)
    (argumentstyle dumpagent.language selector)
    (choice        dumpagent.language kif)
    (choice        dumpagent.language ookif)
    (choice        dumpagent.language rdf)
    (choice        dumpagent.language tdc)

    ;;;;

    (isa           dumpclass action)
    (argument      dumpclass dumpclass.class)
    (argument      dumpclass dumpclass.source)
    (argument      dumpclass dumpclass.filename)
    (argument      dumpclass dumpclass.language)
    (documentation dumpclass
      "<B>dumpclass</B> takes as arguments a class, a local agent, a filename,
and a language.  It dumps all sentences relevant to the specified class from
the agent's database to the specified file in the specified language.")

    (isa           dumpclass.class argumentrelation)
    (argumenttype  dumpclass.class class)
    (argumentstyle dumpclass.class selector)

    (isa           dumpclass.source argumentrelation)
    (argumenttype  dumpclass.source basket)
    (argumentstyle dumpclass.source selector)

    (isa           dumpclass.filename argumentrelation)
    (argumenttype  dumpclass.filename string)
    (argumentstyle dumpclass.filename stringfield)

    (isa           dumpclass.language argumentrelation)
    (argumenttype  dumpclass.language language)
    (argumentstyle dumpclass.language selector)
    (choice        dumpclass.language kif)
    (choice        dumpclass.language ookif)
    (choice        dumpclass.language sxml)
    (choice        dumpclass.language tdc)

    ;;;;

    (isa           dumprelation action)
    (argument      dumprelation dumprelation.relation)
    (argument      dumprelation dumprelation.source)
    (argument      dumprelation dumprelation.filename)
    (argument      dumprelation dumprelation.language)
    (documentation dumprelation
      "<B>dumprelation</B> takes as arguments a relation, a local agent, a filename,
and a character-delimited language (cdt, sdt, tdt, or vdt).  It dumps all sentences
containing the relation from the agent's database to the specified file as rows in
the specified language.")

    (isa           dumprelation.relation argumentrelation)
    (argumenttype  dumprelation.relation relation)
    (argumentstyle dumprelation.relation selector)

    (isa           dumprelation.source argumentrelation)
    (argumenttype  dumprelation.source basket)
    (argumentstyle dumprelation.source selector)

    (isa           dumprelation.filename argumentrelation)
    (argumenttype  dumprelation.filename string)
    (argumentstyle dumprelation.filename stringfield)

    (isa           dumprelation.language argumentrelation)
    (argumenttype  dumprelation.language language)
    (argumentstyle dumprelation.language selector)
    (choice        dumprelation.language kif)
    ;(choice        dumprelation.language ookif)
    ;(choice        dumprelation.language sxml)
    ;(choice        dumprelation.language tdc)

    ;;;;

    (isa           dumprules action)
    (argument      dumprules dumprules.source)
    (argument      dumprules dumprules.filename)
    (argument      dumprules dumprules.language)

    (isa           dumprules.source argumentrelation)
    (argumenttype  dumprules.source basket)
    (argumentstyle dumprules.source selector)

    (isa           dumprules.filename argumentrelation)
    (argumenttype  dumprules.filename string)
    (argumentstyle dumprules.filename stringfield)

    (isa           dumprules.language argumentrelation)
    (argumenttype  dumprules.language language)
    (argumentstyle dumprules.language selector)
    (choice        dumprules.language kif)
    ;(choice        dumprules.language ookif)
    ;(choice        dumprules.language sxml)
    ;(choice        dumprules.language tdc)

    ;;;;

    (isa           duplicate action)
    (argument      duplicate duplicate.old)
    (argument      duplicate duplicate.new)
    (argument      duplicate duplicate.target)
    (documentation duplicate
      "<B>duplicate</B> takes as argument two concepts and an agent.  It copies
all of the information about the first to the second and stores it in the
specified target.")

    (isa           duplicate.old argumentrelation)
    (argumenttype  duplicate.old thing)
    (argumentstyle duplicate.old typein)

    (isa           duplicate.new argumentrelation)
    (argumenttype  duplicate.new string)
    (argumentstyle duplicate.new stringfield)

    (isa           duplicate.target argumentrelation)
    (argumenttype  duplicate.target agent)
    (argumentstyle duplicate.target selector)

    ;;;;

    (isa           evertrules action)
    (argument evertrules evertrules.relation)
    (argument      evertrules evertrules.source)
    (argument      evertrules evertrules.target)
    (documentation evertrules
      "<B>evertrules</B> takes as argument a relation and two ruleservers.  It everts
the definition of the specified relation as given in the first ruleserver and
places the resulting rules in the second ruleserver.  If no relation is specified,
it everts all specialties of the source ruleserver  If no target ruleserver is
specified, there are no side effects and a list of the everted rules is returned.")

    (isa           evertrules.relation argumentrelation)
    (argumenttype  evertrules.relation relation)
    (argumentstyle evertrules.relation selector)

    (isa           evertrules.source argumentrelation)
    (argumenttype  evertrules.source basket)
    (argumentstyle evertrules.source selector)

    (isa           evertrules.target argumentrelation)
    (argumenttype  evertrules.target basket)
    (argumentstyle evertrules.target selector)

    ;;;;

    (isa           forwardrules action)
    (argument      forwardrules forwardrules.relation)
    (argument      forwardrules forwardrules.source)
    (argument      forwardrules forwardrules.target)
    (documentation forwardrules
      "<B>forwardrules</B> takes as argument a relation and two ruleservers.  It writes
the definition of the specified relation as given in the first ruleserver as forward rules
and places the resulting rules in the second ruleserver.  If no target ruleserver is
specified, there are no side effects and a list of the forward rules is returned.")

    (isa           forwardrules.relation argumentrelation)
    (argumenttype  forwardrules.relation relation)
    (argumentstyle forwardrules.relation selector)

    (isa           forwardrules.source argumentrelation)
    (argumenttype  forwardrules.source basket)
    (argumentstyle forwardrules.source selector)

    (isa           forwardrules.target argumentrelation)
    (argumenttype  forwardrules.target basket)
    (argumentstyle forwardrules.target selector)

    ;;;;

    (isa           incorporaterules action)
    (argument incorporaterules incorporaterules.class)
    (argument incorporaterules incorporaterules.target)

    (isa           incorporaterules.class argumentrelation)
    (argumenttype  incorporaterules.class class)
    (argumentstyle incorporaterules.class selector)

    (isa           incorporaterules.target argumentrelation)
    (argumenttype  incorporaterules.target basket)
    (argumentstyle incorporaterules.target selector)

    ;;;;

    (isa           initialize action)
    (argument      initialize initialize.target)
    (documentation initialize
      "<B>initialize</B> takes as argument a local agent and initializes its contents.")

    (isa           initialize.target argumentrelation)
    (argumenttype  initialize.target agent)
    (argumentstyle initialize.target selector)

    ;;;;

    (isa           integraterules action)
    (argument      integraterules integraterules.source)
    (argument      integraterules integraterules.target)

    (isa           integraterules.source argumentrelation)
    (argumenttype  integraterules.source agent)
    (argumentstyle integraterules.source selector)

    (isa           integraterules.target argumentrelation)
    (argumenttype  integraterules.target agent)
    (argumentstyle integraterules.target selector)

    ;;;;

    (isa           introspect action)
    (argument      introspect introspect.target)

    (isa           introspect.target argumentrelation)
    (argumenttype  introspect.target agent)
    (argumentstyle introspect.target selector)

    ;;;;

    (isa           interrelaterules action)
    (argument      interrelaterules interrelaterules.relation)
    (argument      interrelaterules interrelaterules.target)

    (isa           interrelaterules.relation argumentrelation)
    (argumenttype  interrelaterules.relation relation)
    (argumentstyle interrelaterules.relation selector)

    (isa           interrelaterules.target argumentrelation)
    (argumenttype  interrelaterules.target basket)
    (argumentstyle interrelaterules.target selector)

    ;;;;

    (isa           load action)
    (argument      load load.filename)
    (documentation load
      "<B>load</B> takes a filename as argument.  It executes the ACL
       commands contained in the specified file.")

    (isa           load.filename argumentrelation)
    (argumenttype  load.filename string)
    (argumentstyle load.filename stringfield)

    ;;;;

    (isa           loadfile action)
    (argument      loadfile loadfile.filename)
    (argument      loadfile loadfile.target)
    (argument      loadfile loadfile.language)
    (argument      loadfile loadfile.meta)
    (documentation loadfile
      "<B>loadfile</B> takes as arguments a filename, a local agent, a
language, and a meta argument.  It loads the
information from the specified file into the specified agent assuming it
is in the specified language.  If the meta argument is YES, it adds
appropriate specialties and interests, table and rootclass information for
the tables in the agent.")

    (isa           loadfile.filename argumentrelation)
    (argumenttype  loadfile.filename string)
    (argumentstyle loadfile.filename stringfield)

    (isa           loadfile.target argumentrelation)
    (argumenttype  loadfile.target basket)
    (argumentstyle loadfile.target selector)

    (isa           loadfile.language argumentrelation)
    (argumenttype  loadfile.language language)
    (argumentstyle loadfile.language selector)
    (choice        loadfile.language kif)
    (choice        loadfile.language rdf)
    (choice        loadfile.language tdc)

    (isa           loadfile.meta argumentrelation)
    (argumenttype  loadfile.meta boolean)
    (argumentstyle loadfile.meta selector)
    (choice        loadfile.meta no)
    (choice        loadfile.meta yes)

    ;;;;

    (isa           materializeclass action)
    (argument      materializeclass materializeclass.class)
    (argument      materializeclass materializeclass.source)
    (argument      materializeclass materializeclass.target)

    (isa           materializeclass.class argumentrelation)
    (argumenttype  materializeclass.class class)
    (argumentstyle materializeclass.class selector)

    (isa           materializeclass.source argumentrelation)
    (argumenttype  materializeclass.source agent)
    (argumentstyle materializeclass.source selector)

    (isa           materializeclass.target argumentrelation)
    (argumenttype  materializeclass.target basket)
    (argumentstyle materializeclass.target selector)

    ;;;;

    (isa           materializeschema action)
    (argument      materializeschema materializeschema.class)
    (argument      materializeschema materializeschema.source)
    (argument      materializeschema materializeschema.target)

    (isa           materializeschema.class argumentrelation)
    (argumenttype  materializeschema.class class)
    (argumentstyle materializeschema.class selector)

    (isa           materializeschema.source argumentrelation)
    (argumenttype  materializeschema.source agent)
    (argumentstyle materializeschema.source selector)

    (isa           materializeschema.target argumentrelation)
    (argumenttype  materializeschema.target basket)
    (argumentstyle materializeschema.target selector)

    ;;;;

    (isa           materializetable action)
    (argument      materializetable materializetable.relation)
    (argument      materializetable materializetable.source)
    (argument      materializetable materializetable.target)

    (isa           materializetable.relation argumentrelation)
    (argumenttype  materializetable.relation relation)
    (argumentstyle materializetable.relation selector)

    (isa           materializetable.source argumentrelation)
    (argumenttype  materializetable.source agent)
    (argumentstyle materializetable.source selector)

    (isa           materializetable.target argumentrelation)
    (argumenttype  materializetable.target basket)
    (argumentstyle materializetable.target selector)

    ;;;;

    (isa           materializetree action)
    (argument      materializetree materializetree.class)
    (argument      materializetree materializetree.source)
    (argument      materializetree materializetree.target)

    (isa           materializetree.class argumentrelation)
    (argumenttype  materializetree.class class)
    (argumentstyle materializetree.class selector)

    (isa           materializetree.source argumentrelation)
    (argumenttype  materializetree.source agent)
    (argumentstyle materializetree.source selector)

    (isa           materializetree.target argumentrelation)
    (argumenttype  materializetree.target basket)
    (argumentstyle materializetree.target selector)

    ;;;;

    (isa           move action)
    (argument      move move.object)
    (argument      move move.source)
    (argument      move move.target)
    (documentation move
      "<B>move</B> is an action.  It takes as arguments an object, a
       source agent, and a target agent.  It retrieves all facts about
       the specified object from the source agent and forwards the
       information to the target agent.")

    (isa           move.object argumentrelation)
    (argumenttype  move.object relation)
    (argumentstyle move.object selector)

    (isa           move.source argumentrelation)
    (argumenttype  move.source agent)
    (argumentstyle move.source selector)

    (isa           move.target argumentrelation)
    (argumenttype  move.target agent)
    (argumentstyle move.target selector)

    ;;;;

    (isa           moveclassdata action)
    (argument      moveclassdata moveclassdata.class)
    (argument      moveclassdata moveclassdata.source)
    (argument      moveclassdata moveclassdata.target)
    (documentation moveclassdata
      "<B>MoveClassData</B> is an action.  It takes as arguments a class, a
       source agent, and a target agent.  It moves the class membership relation
       and all attributes from the source agent to the target agent.")

    (isa           moveclassdata.class argumentrelation)
    (argumenttype  moveclassdata.class class)
    (argumentstyle moveclassdata.class selector)

    (isa           moveclassdata.source argumentrelation)
    (argumenttype  moveclassdata.source agent)
    (argumentstyle moveclassdata.source selector)

    (isa           moveclassdata.target argumentrelation)
    (argumenttype  moveclassdata.target agent)
    (argumentstyle moveclassdata.target selector)

    ;;;;

    (isa           moveagentdata action)
    (argument      moveagentdata moveagentdata.source)
    (argument      moveagentdata moveagentdata.target)
    (documentation moveagentdata
      "<B>MoveAgentData</B> is an action.  It takes as arguments a
       source agent and a target agent.  It moves the contents of
       the source agent to the target agent.")

    (isa           moveagentdata.source argumentrelation)
    (argumenttype  moveagentdata.source agent)
    (argumentstyle moveagentdata.source selector)

    (isa           moveagentdata.target argumentrelation)
    (argumenttype  moveagentdata.target agent)
    (argumentstyle moveagentdata.target selector)

    ;;;;

    (isa           movetabledata action)
    (argument      movetabledata movetabledata.relation)
    (argument      movetabledata movetabledata.source)
    (argument      movetabledata movetabledata.target)
    (documentation movetabledata
      "<B>MoveTableData</B> is an action.  It takes as arguments a relation, a
       source agent, and a target agent.  It retrieves all rows of
       the specified table from the source agent, erases them, and forwards
       the information to the target agent.")

    (isa           movetabledata.relation argumentrelation)
    (argumenttype  movetabledata.relation relation)
    (argumentstyle movetabledata.relation selector)

    (isa           movetabledata.source argumentrelation)
    (argumenttype  movetabledata.source agent)
    (argumentstyle movetabledata.source selector)

    (isa           movetabledata.target argumentrelation)
    (argumenttype  movetabledata.target agent)
    (argumentstyle movetabledata.target selector)

    ;;;;

    (isa           reifydata action)
    (argument      reifydata reifydata.relation)
    (argument      reifydata reifydata.class)
    (argument      reifydata reifydata.source)
    (argument      reifydata reifydata.target)

    (isa           reifydata.relation argumentrelation)
    (argumenttype  reifydata.relation relation)
    (argumentstyle reifydata.relation selector)

    (isa           reifydata.class argumentrelation)
    (argumenttype  reifydata.class class)
    (argumentstyle reifydata.class selector)

    (isa           reifydata.source argumentrelation)
    (argumenttype  reifydata.source agent)
    (argumentstyle reifydata.source selector)

    (isa           reifydata.target argumentrelation)
    (argumenttype  reifydata.target dataserver)
    (argumentstyle reifydata.target selector)

    ;;;;

    (isa           reifyrules action)
    (argument      reifyrules reifyrules.relation)
    (argument      reifyrules reifyrules.class)
    (argument      reifyrules reifyrules.target)

    (isa           reifyrules.relation argumentrelation)
    (argumenttype  reifyrules.relation relation)
    (argumentstyle reifyrules.relation selector)

    (isa           reifyrules.class argumentrelation)
    (argumenttype  reifyrules.class class)
    (argumentstyle reifyrules.class selector)

    (isa           reifyrules.target argumentrelation)
    (argumenttype  reifyrules.target basket)
    (argumentstyle reifyrules.target selector)

    ;;;;

    (isa           rematerializeclass action)
    (argument      rematerializeclass rematerializeclass.class)
    (argument      rematerializeclass rematerializeclass.source)
    (argument      rematerializeclass rematerializeclass.target)

    (isa           rematerializeclass.class argumentrelation)
    (argumenttype  rematerializeclass.class class)
    (argumentstyle rematerializeclass.class selector)

    (isa           rematerializeclass.source argumentrelation)
    (argumenttype  rematerializeclass.source agent)
    (argumentstyle rematerializeclass.source selector)

    (isa           rematerializeclass.target argumentrelation)
    (argumenttype  rematerializeclass.target basket)
    (argumentstyle rematerializeclass.target selector)

    ;;;;

    (isa           rematerializeschema action)
    (argument      rematerializeschema rematerializeschema.class)
    (argument      rematerializeschema rematerializeschema.source)
    (argument      rematerializeschema rematerializeschema.target)

    (isa           rematerializeschema.class argumentrelation)
    (argumenttype  rematerializeschema.class class)
    (argumentstyle rematerializeschema.class selector)

    (isa           rematerializeschema.source argumentrelation)
    (argumenttype  rematerializeschema.source agent)
    (argumentstyle rematerializeschema.source selector)

    (isa           rematerializeschema.target argumentrelation)
    (argumenttype  rematerializeschema.target basket)
    (argumentstyle rematerializeschema.target selector)

    ;;;;

    (isa           rematerializetable action)
    (argument      rematerializetable rematerializetable.relation)
    (argument      rematerializetable rematerializetable.source)
    (argument      rematerializetable rematerializetable.target)

    (isa           rematerializetable.relation argumentrelation)
    (argumenttype  rematerializetable.relation relation)
    (argumentstyle rematerializetable.relation selector)

    (isa           rematerializetable.source argumentrelation)
    (argumenttype  rematerializetable.source agent)
    (argumentstyle rematerializetable.source selector)

    (isa           rematerializetable.target argumentrelation)
    (argumenttype  rematerializetable.target basket)
    (argumentstyle rematerializetable.target selector)

    ;;;;

    (isa           rematerializetree action)
    (argument      rematerializetree rematerializetree.class)
    (argument      rematerializetree rematerializetree.source)
    (argument      rematerializetree rematerializetree.target)

    (isa           rematerializetree.class argumentrelation)
    (argumenttype  rematerializetree.class class)
    (argumentstyle rematerializetree.class selector)

    (isa           rematerializetree.source argumentrelation)
    (argumenttype  rematerializetree.source agent)
    (argumentstyle rematerializetree.source selector)

    (isa           rematerializetree.target argumentrelation)
    (argumenttype  rematerializetree.target basket)
    (argumentstyle rematerializetree.target selector)

    ;;;;

    (isa           removeduplicatedata action)
    (argument      removeduplicatedata removeduplicatedata.target)
    (documentation removeduplicatedata
      "<B>RemoveDuplicateData</B> takes as argument an agent and removes duplicates from
its contents.")

    (isa           removeduplicatedata.target argumentrelation)
    (argumenttype  removeduplicatedata.target agent)
    (argumentstyle removeduplicatedata.target selector)

    ;;;;

    (isa           rename action)
    (argument      rename rename.old)
    (argument      rename rename.new)
    (argument      rename rename.target)
    (documentation rename
      "<B>rename</B> takes as argument two concepts and an agent.  It replaces
all occurrences of the first argument with the second in the third.")

    (isa           rename.old argumentrelation)
    (argumenttype  rename.old thing)
    (argumentstyle rename.old typein)

    (isa           rename.new argumentrelation)
    (argumenttype  rename.new thing)
    (argumentstyle rename.new typein)

    (isa           rename.target argumentrelation)
    (argumenttype  rename.target agent)
    (argumentstyle rename.target selector)
    
    ;;;;

    (isa           resetagent action)
    (argument      resetagent resetagent.target)
    (argument      resetagent resetagent.meta)
    (documentation resetagent
      "<B>ResetAgent</B> takes as argument a local agent and a meta argument. 
It initializes the contents of the agent.
If the meta argument is <B>yes</B>, it updates appropriate specialties and
interests, table and rootclass information for the classes and relations in the
agent.")

    (isa           resetagent.target argumentrelation)
    (argumenttype  resetagent.target agent)
    (argumentstyle resetagent.target selector)

    (isa           resetagent.meta argumentrelation)
    (argumenttype  resetagent.meta boolean)
    (argumentstyle resetagent.meta selector)

    ;;;;

    (isa           resetsystem action)
    (documentation resetsystem
      "<B>resetsystem</B> is an action.  It puts the system back into its initial state.")

    ;;;;

    (isa           separatedata action)
    (argument      separatedata separatedata.relation)
    (argument      separatedata separatedata.source)
    (argument      separatedata separatedata.target)
    (documentation separatedata
      "<B>separatedata</B> takes a relation and two agents as arguments.  It
materializes the non-key-attributes from the source to the target.")

    (isa           separatedata.relation argumentrelation)
    (argumenttype  separatedata.relation relation)
    (argumentstyle separatedata.relation selector)

    (isa           separatedata.source argumentrelation)
    (argumenttype  separatedata.source agent)
    (argumentstyle separatedata.source selector)

    (isa           separatedata.target argumentrelation)
    (argumenttype  separatedata.target agent)
    (argumentstyle separatedata.target selector)

    ;;;;

    (isa           separaterules action)
    (argument      separaterules separaterules.relation)
    (argument      separaterules separaterules.target)
    (documentation separaterules
      "<B>separaterules</B> takes a relation and a ruleserver as arguments.  It writes
rules defining all of the non-key attributes of the relation as functions of the
key attributes of the relation and saves the rules in the specified ruleserver.
If no ruleserver is specified, there is no side effect and a list of the resulting
rules is returned as value.")

    (isa           separaterules.relation argumentrelation)
    (argumenttype  separaterules.relation relation)
    (argumentstyle separaterules.relation selector)

    (isa           separaterules.target argumentrelation)
    (argumenttype  separaterules.target basket)
    (argumentstyle separaterules.target selector)

    ;;;;

    (isa           tabulaterules action)
    (argument      tabulaterules tabulaterules.class)
    (argument      tabulaterules tabulaterules.relation)
    (argument      tabulaterules tabulaterules.target)
    
    (isa           tabulaterules.class argumentrelation)
    (argumenttype  tabulaterules.class class)
    (argumentstyle tabulaterules.class selector)
    
    (isa           tabulaterules.relation argumentrelation)
    (argumenttype  tabulaterules.relation relation)
    (argumentstyle tabulaterules.relation selector)
    
    (isa           tabulaterules.target argumentrelation)
    (argumenttype  tabulaterules.target basket)
    (argumentstyle tabulaterules.target selector)

    ;;;;

    (isa           triggerrules action)
    (argument      triggerrules triggerrules.relation)
    (argument      triggerrules triggerrules.source)
    (argument      triggerrules triggerrules.target)
    (documentation triggerrules
      "<B>triggerrules</B> takes as argument a relation and two ruleservers.  It writes
all consequences of the specified relation as given in the first ruleserver as forward
rules and places the resulting rules in the second ruleserver.  If no target ruleserver is
specified, there are no side effects and a list of the trigger rules is returned.")

    (isa           triggerrules.relation argumentrelation)
    (argumenttype  triggerrules.relation relation)
    (argumentstyle triggerrules.relation selector)

    (isa           triggerrules.source argumentrelation)
    (argumenttype  triggerrules.source basket)
    (argumentstyle triggerrules.source selector)

    (isa           triggerrules.target argumentrelation)
    (argumenttype  triggerrules.target basket)
    (argumentstyle triggerrules.target selector)

    ;;;;

    (isa           unloadfile action)
    (argument      unloadfile unloadfile.filename)
    (argument      unloadfile unloadfile.target)
    (argument      unloadfile unloadfile.language)
    (argument      unloadfile unloadfile.meta)
    (documentation unloadfile
      "<B>unloadfile</B> takes as arguments a filename, a local agent,
a language, and a meta argument.
It removes the information in the specified file from the specified agent
assuming it is in the specified language.  If the meta argument is
<B>yes</B>, it updates appropriate specialties and interests, table
and rootclass information for the classes and relations in the agent.")

    (isa           unloadfile.filename argumentrelation)
    (argumenttype  unloadfile.filename string)
    (argumentstyle unloadfile.filename stringfield)

    (isa           unloadfile.target argumentrelation)
    (argumenttype  unloadfile.target basket)
    (argumentstyle unloadfile.target selector)

    (isa           unloadfile.language argumentrelation)
    (argumenttype  unloadfile.language language)
    (argumentstyle unloadfile.language selector)
    (choice        unloadfile.language kif)
    (choice        unloadfile.language ookif)
    (choice        unloadfile.language sxml)
    (choice        unloadfile.language tdc)
    (choice        unloadfile.language xml)

    (isa           unloadfile.meta argumentrelation)
    (argumenttype  unloadfile.meta boolean)
    (argumentstyle unloadfile.meta selector)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (isa yes boolean)
    (isa no boolean)

    ;;;;

    (isa add command)
    (isa apropos command)
    (isa change command)
    (isa communication command)
    (isa convert command)
    (isa count command)
    (isa create command)
    (isa creator command)
    (isa display command)
    (isa update command)
    (isa updator command)
    (isa execution command)
    (isa inspect command)
    (isa inspector command)
    (isa memory command)
    (isa metalevel command)
    (isa reduction command)
    (isa rulify command)
    (isa save command)
    (isa showexpansion command)
    (isa showoptimization command)
    (isa showreduction command)
    (isa showsourcing command)
    (isa subframe command)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (isa acl language)
    (isa cdf language)
    (isa cdt language)
    (isa csv language)
    (isa html language)
    (isa kif language)
    (isa ookif language)
    (isa qbe language)
    (isa sdt language)
    (isa tdt language)
    (isa vdt language)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (isa checkbox modality)
    (isa combobox modality)
    (isa datestyle modality)
    (isa dollarstyle modality)
    (isa emailstyle modality)
    (isa fancyselector modality)
    (isa fastselector modality)
    (isa glyph modality)
    (isa htmlstyle modality)
    (isa imagestyle modality)
    (isa intermenu modality)
    (isa interval modality)
    (isa message modality)
    (isa menu modality)
    (isa noshow modality)
    (isa password modality)
    (isa prettystyle modality)
    (isa radiobutton modality)
    (isa selector modality)
    (isa stringfield modality)
    (isa subframe modality)
    (isa text modality)
    (isa textarea modality)
    (isa typein modality)
    (isa urlstyle modality)

    ;;;;

    (isa monday weekday)
    (isa tuesday weekday)
    (isa wednesday weekday)
    (isa thursday weekday)
    (isa friday weekday)
    (isa saturday weekday)
    (isa sunday weekday)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *metarules*
  '((<= (isa ?x thing) (isa ?x action))
    (<= (isa ?x thing) (isa ?x agent))
    (<= (isa ?x thing) (isa ?x class))
    (<= (isa ?x thing) (isa ?x parameter))
    (<= (isa ?x thing) (isa ?x relation))

    (<= (isa ?x agent) (isa ?x factserver))
    (<= (isa ?x agent) (isa ?x viewserver))
    (<= (isa ?x agent) (isa ?x fullserver))
    (<= (isa ?x agent) (isa ?x dualserver))
    (<= (isa ?x agent) (isa ?x dataserver))
    (<= (isa ?x agent) (isa ?x diffserver))
    (<= (isa ?x agent) (isa ?x ruleserver))
    (<= (isa ?x agent) (isa ?x fastserver))
    (<= (isa ?x agent) (isa ?x translator))
    (<= (isa ?x agent) (isa ?x transformer))
    (<= (isa ?x agent) (isa ?x integrator))
    (<= (isa ?x agent) (isa ?x facilitator))
    (<= (isa ?x agent) (isa ?x interface))

    (<= (isa ?x basket) (isa ?x factserver))
    (<= (isa ?x basket) (isa ?x dataserver))
    (<= (isa ?x basket) (isa ?x viewserver))
    (<= (isa ?x basket) (isa ?x fullserver))
    (<= (isa ?x basket) (isa ?x dualserver))
    (<= (isa ?x basket) (isa ?x ruleserver))
    (<= (isa ?x basket) (isa ?x fastserver))

    (<= (isa ?x parameter) (isa ?x boolean))
    (<= (isa ?x parameter) (isa ?x command))
    (<= (isa ?x parameter) (isa ?x language))
    (<= (isa ?x parameter) (isa ?x modality))
    (<= (isa ?x parameter) (isa ?x weekday))

    (<= (isa ?x relation) (isa ?x predicaterelation))
    (<= (isa ?x relation) (isa ?x attributerelation))
    (<= (isa ?x relation) (isa ?x naryrelation))
    (<= (isa ?x relation) (isa ?x evaluablerelation))
    (<= (isa ?x relation) (isa ?x argumentrelation))

    (<= (arity ?x ?y) (same ?y 1) (isa ?x predicaterelation))
    (<= (arity ?x ?y) (same ?y 2) (isa ?x attributerelation))
    (<= (arity ?x ?y) (same ?y 2) (isa ?x argumentrelation))

    (<= (domain ?predicate ?class) (predicate ?class ?predicate))

    (<= (specialty ?x ?y) (material ?x ?y))
    (<= (specialist ?x ?y) (specialty ?y ?x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize ((target (eql *manager*)))
  (define-theory target "" *sentences*)
  'done)

(defmethod revise (p sender (th (eql *manager*)))
  ;(reviser facts th)
  (call-next-method p sender th))

(defmethod reviser (facts (th (eql *manager*)))
  (dolist (p facts)
    (do ((l (indexps p th) (cdr l)))
        ((null l) (insert p th))
        (when (samep p (car l))
          (remcontent (car l) th)
          (addcontent p th)
          (unindex (car l) (car l) th)
          (index p p th)
          (return t))))
  'done)

(defmethod insert (p (receiver (eql *manager*)))
  (prog1 (call-next-method p receiver)
         (unless (atom p) (setvalue (car p) (cadr p) (caddr p)))))

(defmethod uninsert (p (receiver (eql *manager*)))
  (prog2 (call-next-method p receiver)
         (unless (atom p) (remvalue (car p) (cadr p) (caddr p)))))

(defmethod define-theory ((th (eql *manager*)) doc facts)
  (empty th)
  (setf (documentation th 'concept) doc)
  (dolist (p facts)
    (cond ((atom p) (insert p th))
          ((find (car p) '(not unprovable)) (drop (cadr p) th))
          (t (insert p th))))
  th)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create (obj type)
  (cond ((not (symbolp obj)) nil)
        ((eq type 'remote)
         (unless (and (boundp obj) (remotep (symbol-value obj)))
           (let ((host (or (find-host obj) "")) (port (or (find-port obj) 0)))
             (set obj (make-remote obj host port)))))
        ((and (boundp obj) (eq (type-of (symbol-value obj)) type)))
        ((find type '(aclserver soapserver sqlserver))
         (set obj (make-instance type :name obj)))))

(defmethod destroy (obj type)
  (cond ((not (symbolp obj)) nil)
        ((and (boundp obj) (eq (type-of (symbol-value obj)) type)
              (find type '(interface counter basket aclserver soapserver sqlserver
                           facilitator authorizer)))
         (makunbound obj))))

(defmethod create (obj (type (eql 'factserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'factserver))
    (set obj (make-instance 'factserver :name obj))))

(defmethod destroy (obj (type (eql 'factserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'factserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'dualserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'dualserver))
    (set obj (make-instance 'dualserver :name obj))))

(defmethod destroy (obj (type (eql 'dualserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'dualserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'dataserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'dataserver))
    (set obj (make-instance 'dataserver :name obj))))

(defmethod destroy (obj (type (eql 'dataserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'dataserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'viewserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'viewserver))
    (set obj (make-instance 'viewserver :name obj))))

(defmethod destroy (obj (type (eql 'viewserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'viewserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'fullserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'fullserver))
    (set obj (make-instance 'fullserver :name obj))))

(defmethod destroy (obj (type (eql 'fullserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'fullserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'fastserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'fastserver))
    (set obj (make-instance 'fastserver :name obj))))

(defmethod destroy (obj (type (eql 'fastserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'fastserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'ruleserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'ruleserver))
    (set obj (make-instance 'ruleserver :name obj))))

(defmethod destroy (obj (type (eql 'ruleserver)))
  (when (and (symbolp obj) (boundp obj)
             (eq (type-of (symbol-value obj)) 'ruleserver))
    (empty (symbol-value obj))
    (makunbound obj)))

(defmethod create (obj (type (eql 'diffserver)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'diffserver))
    (set obj (make-instance 'diffserver :name obj))))

(defmethod destroy (obj (type (eql 'diffserver)))
  (cond ((and (symbolp obj) (boundp obj)
              (eq (type-of (symbol-value obj)) 'diffserver))
         (makunbound obj))))

(defmethod create (obj (type (eql 'translator)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'translator))
    (set obj (make-instance 'translator :name obj))))

(defmethod destroy (obj (type (eql 'translator)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'translator))
    (makunbound obj)))

(defmethod create (obj (type (eql 'transformer)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'transformer))
    (set obj (make-instance 'transformer :name obj))))

(defmethod destroy (obj (type (eql 'transformer)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'transformer))
    (makunbound obj)))

(defmethod create (obj (type (eql 'integrator)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'integrator))
    (set obj (make-instance 'integrator :name obj))))

(defmethod destroy (obj (type (eql 'integrator)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'integrator))
    (makunbound obj)))

(defmethod create (obj (type (eql 'facilitator)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'facilitator))
    (set obj (make-instance 'facilitator :name obj))))

(defmethod destroy (obj (type (eql 'facilitator)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'facilitator))
    (makunbound obj)))

(defmethod create (obj (type (eql 'interface)))
  (unless (and (boundp obj) (typep (symbol-value obj) 'interface))
    (set obj (make-instance 'interface :name obj))))

(defmethod destroy (obj (type (eql 'interface)))
  (when (and (symbolp obj) (boundp obj) (typep (symbol-value obj) 'interface))
    (makunbound obj)))


(defmethod setvalue (slot obj val)
  (cond ((eq 'isa slot) (create obj val))
        ((eq 'includee slot) (setincludee obj val))
        ((eq 'host slot) (sethost obj val))
        ((eq 'port slot) (setport obj val))))

(defmethod remvalue (slot obj val)
  (cond ((eq 'isa slot) (destroy obj val))
        ((eq 'includee slot) (unsetincludee obj val))
        ((eq 'host slot) (sethost obj ""))
        ((eq 'port slot) (setport obj 0))))

(defun setincludee (t1 t2)
  (if (and (symbolp t1) (boundp t1) (setq t1 (symbol-value t1))
           (symbolp t2) (boundp t2) (setq t2 (symbol-value t2))
           (typep t2 'agent))
      (includes t1 t2)))

(defun unsetincludee (t1 t2)
  (if (and (symbolp t1) (boundp t1) (setq t1 (symbol-value t1))
           (symbolp t2) (boundp t2) (setq t2 (symbol-value t2))
           (typep t2 'agent))
      (unincludes t1 t2)))

(defun sethost (x h)
  (if (and (symbolp x) (boundp x) (remotep (setq x (symbol-value x)))
           (or (not (equal h *host*)) (not (equal (port x) *port*))))
    (setf (host x) h)))

(defun setport (x p)
  (if (and (symbolp x) (boundp x)  (remotep (setq x (symbol-value x)))
           (or (not (equal (host x) *host*)) (not (equal p *port*))))
    (setf (port x) p)))

(defmethod initialize ((target (eql *metalibrary*)))
  (define-theory target "" *metarules*)
  'done)

(define-theory *manager* "" *sentences*)

(define-theory *metalibrary* "" *metarules*)

(defmethod resetsystem ()
  (reset)
  (initialize *manager*)
  (initialize *metalibrary*)
  (setq *ancestry* t)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
