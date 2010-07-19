#!/bin/sh

mv /gin/infomaster/examples/gates/pedit/pedit_person.kif /gin/infomaster/examples/gates/pedit/pedit_person_backup.kif
mv /gin/infomaster/examples/gates/pedit/pedit_office.kif /gin/infomaster/examples/gates/pedit/pedit_office_backup.kif
mv /gin/infomaster/examples/gates/pedit/pedit_phone.kif /gin/infomaster/examples/gates/pedit/pedit_phone_backup.kif
mv /gin/infomaster/examples/gates/pedit/pedit_email.kif /gin/infomaster/examples/gates/pedit/pedit_email_backup.kif

env SYBASE=/Applications/Sybase/System DSQUERY=pedit DSCONSOLE=pedit \
   /Applications/Sybase/System/OCS-12_5/bin/isql -S pedit -U infomaster -P 99info99 -D csd \
  -i /gin/infomaster/examples/gates/pedit/pedit1 \
  | sed '$!N;s/\n/ /' | sed '$!N;s/\n/ /' | tr -d "\t" | tr -s " " | sed -e '/--/d' -e '/relation_/d' -e 's/= /"/g' -e 's/ +/"/g' \
  | sed -e 's/+/"/g' > /gin/infomaster/examples/gates/pedit/pedit_person.kif

env SYBASE=/Applications/Sybase/System DSQUERY=pedit DSCONSOLE=pedit \
   /Applications/Sybase/System/OCS-12_5/bin/isql -S pedit -U infomaster -P 99info99 -D csd \
  -i /gin/infomaster/examples/gates/pedit/pedit2 \
  | tr -s " " | sed -e '/---/d' -e '/relation_/d' -e 's/= /"/g' -e 's/ +/"/g' \
  | sed -e 's/+/"/g' > /gin/infomaster/examples/gates/pedit/pedit_office.kif

env SYBASE=/Applications/Sybase/System DSQUERY=pedit DSCONSOLE=pedit \
   /Applications/Sybase/System/OCS-12_5/bin/isql -S pedit -U infomaster -P 99info99 -D csd \
  -i /gin/infomaster/examples/gates/pedit/pedit3 \
  | sed '$!N;s/\n/ /' | tr -d "\t" | tr -s " " | sed -e '/---/d' -e '/relation_/d' -e 's/= /"/g' -e 's/ +/"/g' \
  | sed -e 's/=/"/g' -e 's/+/"/g' > /gin/infomaster/examples/gates/pedit/pedit_phone.kif

env SYBASE=/Applications/Sybase/System DSQUERY=pedit DSCONSOLE=pedit \
   /Applications/Sybase/System/OCS-12_5/bin/isql -S pedit -U infomaster -P 99info99 -D csd \
  -i /gin/infomaster/examples/gates/pedit/pedit4 \
 | tr -s " " | sed -e '/---/d' -e 's/ )/)/g' -e '/relation_/d' > /gin/infomaster/examples/gates/pedit/pedit_email.kif
env SYBASE=/Applications/Sybase/System DSQUERY=pedit DSCONSOLE=pedit \
   /Applications/Sybase/System/OCS-12_5/bin/isql -S pedit -U infomaster -P 99info99 -D csd \
  -i /gin/infomaster/examples/gates/pedit/pedit5 > /gin/infomaster/examples/gates/pedit/pedit5.kif

echo done
