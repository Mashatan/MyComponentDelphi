{
   Author:   Ali Mashatan
   E-mail:  ali.mashatan@gmail.com
   website: https://github.com/Mashatan
   Date:     2008-02-28
   License:  GNU Public License ( GPL3 ) [http://www.gnu.org/licenses/gpl-3.0.txt]
}

unit Reg;

interface
uses Classes , MSStatistics, MSHints, MSPopupMenu;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Mashatan', [TMSStatistics]);
  RegisterComponents('Mashatan', [TMSBalloonHint]);
  RegisterComponents('Mashatan', [TMSPopupMenu]);

end;

end.
