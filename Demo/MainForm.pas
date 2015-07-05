{
   Author:   Ali Mashatan
   E-mail:  ali.mashatan@gmail.com
   website: https://github.com/Mashatan
   Date:     2008-02-28
   License:  GNU Public License ( GPL3 ) [http://www.gnu.org/licenses/gpl-3.0.txt]
}

unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MSStatistics;

type
  TForm1 = class(TForm)
    MSStatistics1: TMSStatistics;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  MSStatistics1.Series.Items[0].Add(125,1000,ntFrameRect,true);
  MSStatistics1.Series.Items[0].Add(20,170,ntElli);
  MSStatistics1.Series.Items[0].Add(30,150,ntRect,true);
  MSStatistics1.Series.Items[0].Add(140,20,ntElli);
  MSStatistics1.Series.Items[0].Add(50,150,ntFrameElli);

  MSStatistics1.Series.Items[1].Add(6,8,ntElli);
  MSStatistics1.Series.Items[1].Add(45,500,ntFrameRect);
  MSStatistics1.Series.Items[1].Add(200,700,ntElli);
  MSStatistics1.Flash:=true;
end;

end.
