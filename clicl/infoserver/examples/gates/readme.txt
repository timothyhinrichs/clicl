
Backing up in preparation for reload:
  (dumpclasses) ;;; This saves files in /infoserver/examples/gates/backup/
  Quit CCL
  Empty /infoserver/examples/gates/backupold/
  MOVE 12 class files from /gates/ to /infoserver/examples/gates/backupold/
  MOVE gates.log and webserver.log to /infoserver/examples/gates/backupold/
  Note that logs should be gone from /gates/ so new changes not appended to old
  COPY 12 class files from /infoserver/examples/gates/backup/ to .../gates/

Manually starting the system (see end of load.ccl for these instructions):
  Start CCL
  (load "/infoserver/source/loader.ccl") loads infoserver base system
  (load "/infoserver/examples/gates/load.ccl") loads gates code and data
  (load "/infoserver/examples/gates/gates.log") loads logged deltas
  (setq *log* "/infoserver/examples/gates/gates.log") starts logging updates
  (setq *logfile* "/infoserver/examples/gates/webserver.log") starts logging web
  (tcp-handlers #'http-handler 3333) listens for connections
  apple-comma followed by apple-period breaks out of listen loop
