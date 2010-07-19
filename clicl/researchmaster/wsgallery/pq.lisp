(DEFVAR DATASTORE)
(DEFUN INIT_INDEX NIL (SETQ DATASTORE (MAKE-CLASS 'DICTIONARY)) (DICTIONARY-SET DATASTORE 'UNIV (MAKE-CLASS 'HASHBAG (MAKE-CLASS 'EXPR 0) (MAKE-CLASS 'EXPR 1) (MAKE-CLASS 'EXPR 2) (MAKE-CLASS 'EXPR 3) (MAKE-CLASS 'EXPR 4) (MAKE-CLASS 'EXPR 5))) (DICTIONARY-SET DATASTORE 'AB (MAKE-CLASS 'SET (MAKE-CLASS 'EXPR 4) (MAKE-CLASS 'EXPR 5))) (DICTIONARY-SET DATASTORE 'BOOLEAN (MAKE-CLASS 'SET (MAKE-CLASS 'EXPR 2) (MAKE-CLASS 'EXPR 3))))
(DEFUN CHECK_UNIV (?X0) (RETURN-FROM CHECK_UNIV (COLLECTION-MEMBER (DICTIONARY-GET DATASTORE 'UNIV) (MAKE-CLASS 'EXPR ?X0) #'COLLECTION-EQUAL)))
(DEFUN CHECK_BOOLEAN (?X0) (RETURN-FROM CHECK_BOOLEAN (COLLECTION-MEMBER (DICTIONARY-GET DATASTORE 'BOOLEAN) (MAKE-CLASS 'EXPR ?X0) #'COLLECTION-EQUAL)))
(DEFUN CHECK_AB (?X0) (RETURN-FROM CHECK_AB (COLLECTION-MEMBER (DICTIONARY-GET DATASTORE 'AB) (MAKE-CLASS 'EXPR ?X0) #'COLLECTION-EQUAL)))
(DEFUN ENUM_UNIV (?X0) (DECLARE (IGNORE ?X0)) (RETURN-FROM ENUM_UNIV (DICTIONARY-GET DATASTORE 'UNIV)))
(DEFUN ENUM_BOOLEAN (?X0) (DECLARE (IGNORE ?X0)) (RETURN-FROM ENUM_BOOLEAN (DICTIONARY-GET DATASTORE 'BOOLEAN)))
(DEFUN ENUM_AB (?X0) (DECLARE (IGNORE ?X0)) (RETURN-FROM ENUM_AB (DICTIONARY-GET DATASTORE 'AB)))
(DEFUN NEGX_Q (X0) (NORMRETURN NIL (FUNCALL #'NEG_Q #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE SOFAR SUPPORT)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'TRUE NEWVAL))) X0)))
(DEFUN NEGS_Q (X0) (NORMRETURN NIL (FUNCALL #'NEG_Q #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE SUPPORT)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'FALSE (COLLECTION-ADJOIN SOFAR NEWVAL #'COLLECTION-EQUAL)))) X0)))
(DEFUN NEGSUPPX_Q (X0) (NORMRETURN NIL (FUNCALL #'NEG_Q #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE SOFAR)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'TRUE (MAKE-CLASS 'PAIR NEWVAL SUPPORT)))) X0)))
(DEFUN NEGSUPPS_Q (X0) (NORMRETURN NIL (FUNCALL #'NEG_Q #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'FALSE (COLLECTION-ADJOIN SOFAR (MAKE-CLASS 'PAIR NEWVAL SUPPORT) #'COLLECTION-EQUAL)))) X0)))
(DEFUN NEG_Q (ONSUCCESS X0) (IF (FUNCALL #'VARP X0) (IF (FUNCALL #'FBOUNDP (FUNCTIONNAME NEG_Q_F)) (NORMRETURN NEG_Q (FUNCALL #'NEG_Q_F ONSUCCESS X0)) (NORMRETURN NEG_Q 'UNDEFINED)) (IF (FUNCALL #'FBOUNDP (FUNCTIONNAME NEG_Q_B)) (NORMRETURN NEG_Q (FUNCALL #'NEG_Q_B ONSUCCESS X0)) (NORMRETURN NEG_Q 'UNDEFINED))))
(DEFUN NEG_Q_B (ONSUCCESS NSH9) (LET ((SOFAR (MAKE-CLASS 'SET)) TMP (X0 NSH9)) (WHEN (FUNCALL #'HASCELLVALUE 'P) (LET ((TLH0 '?TLH0)) (LET ((ARH9 (CELLVALUES 'P))) (WITH-COLLECTION-ITERATOR (KEY ARH9) (SETQ TLH0 (COLLECTION-ELEMENT ARH9 KEY)) (WHEN (NOT (EQ TLH0 X0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'P) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_Q_B (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))))) (SETQ X0 NSH9)) (LET ((TLH0 '?TLH0)) (LET ((ARH10 (CELLVALUES 'P))) (WITH-COLLECTION-ITERATOR (KEY ARH10) (SETQ TLH0 (COLLECTION-ELEMENT ARH10 KEY)) (WHEN (NOT (EQ X0 TLH0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'P) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_Q_B (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))))) (SETQ X0 NSH9))) (WHEN (NOT (EQ X0 X0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_Q_B (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP))) (SETQ X0 NSH9)) (NORMRETURN NEG_Q_B SOFAR)))
(DEFUN NEG_Q_F (ONSUCCESS NSH10) (LET ((SOFAR (MAKE-CLASS 'SET)) TMP (X0 NSH10)) (WHEN (FUNCALL #'HASCELLVALUE 'P) (LET ((TLH0 '?TLH0)) (LET ((ARH13 (CELLVALUES 'P))) (WITH-COLLECTION-ITERATOR (KEY ARH13) (SETQ TLH0 (COLLECTION-ELEMENT ARH13 KEY)) (LET ((ARH12 (ENUM_AB X0))) (WITH-COLLECTION-ITERATOR (KEY ARH12) (SETQ X0 (COLLECTION-ELEMENT (COLLECTION-ELEMENT ARH12 KEY) 0)) (WHEN (NOT (EQ TLH0 X0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'P) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_Q_F (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))))))) (SETQ X0 NSH10)) (LET ((TLH0 '?TLH0)) (LET ((ARH15 (CELLVALUES 'P))) (WITH-COLLECTION-ITERATOR (KEY ARH15) (SETQ TLH0 (COLLECTION-ELEMENT ARH15 KEY)) (LET ((ARH14 (ENUM_AB X0))) (WITH-COLLECTION-ITERATOR (KEY ARH14) (SETQ X0 (COLLECTION-ELEMENT (COLLECTION-ELEMENT ARH14 KEY) 0)) (WHEN (NOT (EQ X0 TLH0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'P) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_Q_F (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))))))) (SETQ X0 NSH10))) (LET ((ARH16 (ENUM_AB X0))) (WITH-COLLECTION-ITERATOR (KEY ARH16) (SETQ X0 (COLLECTION-ELEMENT (COLLECTION-ELEMENT ARH16 KEY) 0)) (WHEN (NOT (EQ X0 X0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_Q_F (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP))))) (SETQ X0 NSH10)) (NORMRETURN NEG_Q_F SOFAR)))
(DEFUN POSX_Q (X0) (NORMRETURN NIL (FUNCALL #'POS_Q #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE SOFAR SUPPORT)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'TRUE NEWVAL))) X0)))
(DEFUN POSS_Q (X0) (NORMRETURN NIL (FUNCALL #'POS_Q #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE SUPPORT)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'FALSE (COLLECTION-ADJOIN SOFAR NEWVAL #'COLLECTION-EQUAL)))) X0)))
(DEFUN POSSUPPX_Q (X0) (NORMRETURN NIL (FUNCALL #'POS_Q #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE SOFAR)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'TRUE (MAKE-CLASS 'PAIR NEWVAL SUPPORT)))) X0)))
(DEFUN POSSUPPS_Q (X0) (NORMRETURN NIL (FUNCALL #'POS_Q #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'FALSE (COLLECTION-ADJOIN SOFAR (MAKE-CLASS 'PAIR NEWVAL SUPPORT) #'COLLECTION-EQUAL)))) X0)))
(DEFUN POS_Q (ONSUCCESS X0) (IF (FUNCALL #'VARP X0) (IF (FUNCALL #'FBOUNDP (FUNCTIONNAME POS_Q_F)) (NORMRETURN POS_Q (FUNCALL #'POS_Q_F ONSUCCESS X0)) (NORMRETURN POS_Q 'UNDEFINED)) (IF (FUNCALL #'FBOUNDP (FUNCTIONNAME POS_Q_B)) (NORMRETURN POS_Q (FUNCALL #'POS_Q_B ONSUCCESS X0)) (NORMRETURN POS_Q 'UNDEFINED))))
(DEFUN POS_Q_B (ONSUCCESS NSH16) (LET ((SOFAR (MAKE-CLASS 'SET)) TMP (X0 NSH16)) (WHEN (FUNCALL #'HASCELLVALUE 'P) (WHEN (COLLECTION-MEMBER (CELLVALUES 'P) X0) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'P) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM POS_Q_B (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP))) (SETQ X0 NSH16))) (NORMRETURN POS_Q_B SOFAR)))
(DEFUN POS_Q_F (ONSUCCESS NSH17) (LET ((SOFAR (MAKE-CLASS 'SET)) TMP (X0 NSH17)) (WHEN (FUNCALL #'HASCELLVALUE 'P) (LET ((ARH17 (CELLVALUES 'P))) (WITH-COLLECTION-ITERATOR (KEY ARH17) (SETQ X0 (COLLECTION-ELEMENT ARH17 KEY)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'P) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM POS_Q_F (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))) (SETQ X0 NSH17))) (NORMRETURN POS_Q_F SOFAR)))
(DEFUN NEGX_P (X0) (NORMRETURN NIL (FUNCALL #'NEG_P #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE SOFAR SUPPORT)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'TRUE NEWVAL))) X0)))
(DEFUN NEGS_P (X0) (NORMRETURN NIL (FUNCALL #'NEG_P #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE SUPPORT)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'FALSE (COLLECTION-ADJOIN SOFAR NEWVAL #'COLLECTION-EQUAL)))) X0)))
(DEFUN NEGSUPPX_P (X0) (NORMRETURN NIL (FUNCALL #'NEG_P #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE SOFAR)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'TRUE (MAKE-CLASS 'PAIR NEWVAL SUPPORT)))) X0)))
(DEFUN NEGSUPPS_P (X0) (NORMRETURN NIL (FUNCALL #'NEG_P #'(LAMBDA (NEWVAL SUPPORT SOFAR) (DECLARE (IGNORE)) (NORMRETURN NIL (MAKE-CLASS 'PAIR 'FALSE (COLLECTION-ADJOIN SOFAR (MAKE-CLASS 'PAIR NEWVAL SUPPORT) #'COLLECTION-EQUAL)))) X0)))
(DEFUN NEG_P (ONSUCCESS X0) (IF (FUNCALL #'VARP X0) (IF (FUNCALL #'FBOUNDP (FUNCTIONNAME NEG_P_F)) (NORMRETURN NEG_P (FUNCALL #'NEG_P_F ONSUCCESS X0)) (NORMRETURN NEG_P 'UNDEFINED)) (IF (FUNCALL #'FBOUNDP (FUNCTIONNAME NEG_P_B)) (NORMRETURN NEG_P (FUNCALL #'NEG_P_B ONSUCCESS X0)) (NORMRETURN NEG_P 'UNDEFINED))))
(DEFUN NEG_P_B (ONSUCCESS NSH18) (LET ((SOFAR (MAKE-CLASS 'SET)) TMP (X0 NSH18)) (WHEN (FUNCALL #'HASCELLVALUE 'P) (LET ((TLH1 '?TLH1)) (LET ((ARH18 (CELLVALUES 'P))) (WITH-COLLECTION-ITERATOR (KEY ARH18) (SETQ TLH1 (COLLECTION-ELEMENT ARH18 KEY)) (WHEN (NOT (EQ X0 TLH1)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'P) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_P_B (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))))) (SETQ X0 NSH18)) (LET ((TLH0 '?TLH0)) (LET ((ARH19 (CELLVALUES 'P))) (WITH-COLLECTION-ITERATOR (KEY ARH19) (SETQ TLH0 (COLLECTION-ELEMENT ARH19 KEY)) (WHEN (NOT (EQ TLH0 X0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'P) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_P_B (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))))) (SETQ X0 NSH18))) (WHEN (FUNCALL #'HASCELLVALUE 'Q) (LET ((TLH1 '?TLH1)) (SETQ TLH1 (FUNCALL #'CELLVALUE 'Q)) (WHEN (NOT (EQ X0 TLH1)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'Q) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_P_B (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))) (SETQ X0 NSH18)) (LET ((TLH1 '?TLH1)) (SETQ TLH1 (FUNCALL #'CELLVALUE 'Q)) (WHEN (NOT (EQ TLH1 X0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'Q) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_P_B (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))) (SETQ X0 NSH18)) (WHEN (NOT (EQ (CELLVALUE 'Q) X0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'Q) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_P_B (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP))) (SETQ X0 NSH18))) (NORMRETURN NEG_P_B SOFAR)))
(DEFUN NEG_P_F (ONSUCCESS NSH19) (LET ((SOFAR (MAKE-CLASS 'SET)) TMP (X0 NSH19)) (WHEN (FUNCALL #'HASCELLVALUE 'P) (LET ((TLH1 '?TLH1)) (LET ((ARH24 (CELLVALUES 'P))) (WITH-COLLECTION-ITERATOR (KEY ARH24) (SETQ TLH1 (COLLECTION-ELEMENT ARH24 KEY)) (LET ((ARH23 (ENUM_AB X0))) (WITH-COLLECTION-ITERATOR (KEY ARH23) (SETQ X0 (COLLECTION-ELEMENT (COLLECTION-ELEMENT ARH23 KEY) 0)) (WHEN (NOT (EQ X0 TLH1)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'P) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_P_F (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))))))) (SETQ X0 NSH19)) (LET ((TLH0 '?TLH0)) (LET ((ARH26 (CELLVALUES 'P))) (WITH-COLLECTION-ITERATOR (KEY ARH26) (SETQ TLH0 (COLLECTION-ELEMENT ARH26 KEY)) (LET ((ARH25 (ENUM_AB X0))) (WITH-COLLECTION-ITERATOR (KEY ARH25) (SETQ X0 (COLLECTION-ELEMENT (COLLECTION-ELEMENT ARH25 KEY) 0)) (WHEN (NOT (EQ TLH0 X0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'P) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_P_F (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))))))) (SETQ X0 NSH19))) (WHEN (FUNCALL #'HASCELLVALUE 'Q) (LET ((TLH1 '?TLH1)) (SETQ TLH1 (FUNCALL #'CELLVALUE 'Q)) (LET ((ARH27 (ENUM_AB X0))) (WITH-COLLECTION-ITERATOR (KEY ARH27) (SETQ X0 (COLLECTION-ELEMENT (COLLECTION-ELEMENT ARH27 KEY) 0)) (WHEN (NOT (EQ X0 TLH1)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'Q) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_P_F (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))))) (SETQ X0 NSH19)) (LET ((TLH1 '?TLH1)) (SETQ TLH1 (FUNCALL #'CELLVALUE 'Q)) (LET ((ARH28 (ENUM_AB X0))) (WITH-COLLECTION-ITERATOR (KEY ARH28) (SETQ X0 (COLLECTION-ELEMENT (COLLECTION-ELEMENT ARH28 KEY) 0)) (WHEN (NOT (EQ TLH1 X0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'Q) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_P_F (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP)))))) (SETQ X0 NSH19)) (LET ((ARH29 (ENUM_AB X0))) (WITH-COLLECTION-ITERATOR (KEY ARH29) (SETQ X0 (COLLECTION-ELEMENT (COLLECTION-ELEMENT ARH29 KEY) 0)) (WHEN (NOT (EQ (CELLVALUE 'Q) X0)) (SETQ TMP (FUNCALL ONSUCCESS (MAKE-CLASS 'EXPR X0) (MAKE-CLASS 'SET 'Q) SOFAR)) (IF (EQ (COLLECTION-FIRST TMP) 'TRUE) (RETURN-FROM NEG_P_F (COLLECTION-SECOND TMP)) (SETQ SOFAR (COLLECTION-SECOND TMP))))) (SETQ X0 NSH19))) (NORMRETURN NEG_P_F SOFAR)))
