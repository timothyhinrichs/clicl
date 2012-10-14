; Signature for permanent storage of messages
(defsignature3 db.message (id :type integer) author (text :type (integer html)))
(defschema db.message :signature db.message :guards(message))

; Signature for showing and sending messages
(defsignature message id (text :type html)
(defschema message :signature message :guards(message))

; message id must be unique
(defguard message
  (=> (message.id ?x) (message.id ?y) (= ?x ?y))
)

(defupdate savemessage (:language posneg)
  (<= (pos (db.message.id ?x)) (message.id ?x))
  (<= (pos (db.message.author ?x ?y)) (and (message.id ?x)(session.username ?y)))
  (<= (pos (db.message.text ?x ?y)) (and (message.id ?x)(message.text ?y)))
  )


(defform new-message :schema message :target new-message)

(deftable messages :schema message)

(defservlet show-message-board :page message-board-page)
(defservlet new-message :updates (savemessage) :page show-message-board)


(defhtml message-board-page header message-board footer)
(defhtml message-board ("msgboard.html" :forms (("new-message" new-message)) :tables (("messages" messages))))