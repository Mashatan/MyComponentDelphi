{
   Author:   Ali Mashatan
   E-mail:  ali.mashatan@gmail.com
   website: https://github.com/Mashatan
   Date:     2008-02-28
   License:  GNU Public License ( GPL3 ) [http://www.gnu.org/licenses/gpl-3.0.txt]
}

unit MSTimeSheet;

interface
uses Forms,Windows,Types,Controls,Classes,Grids,Graphics,Messages,
     MSCommon;
const
  DefaultSelectColor=$00A4A4A4;

  ColorAllow=$00AAEFD5;
  ColorSelectAllow=clGreen;

  ColorFair=$00EEE1D5;
  ColorSelectFair=clBlue;

  ColorDeny=$007575FF;
  ColorSelectDeny=clRed;

  ColorOverMouse=$00C2E0DA;
Type

  TDayKind = (dkSaturday=1,dkSunday=2,dkMonday=3,dkTuesday=4,dkWednesday=5,dkThursday=6,dkFriday=7);
  TTimeMode=(tmHour=24,tmHalf=48,tmQuarter=96);
  TTimePermission=(tpAllow=1,tpDeny=0,tpFair=2);
  TDayofWeekStr=array[TDayKind] of String;

  TMSTimeSheet=class(TCustomDrawGrid)
    private
      fTimeMode:TTimeMode;
      fTitleCorner:String;
      fTimePermission:TTimePermission;
      fWeek:TStringList;
      fSelectFix:TGridRect;
      fSelectOverColNo,fSelectOverRowNo:integer;
      fReadonly:boolean;
      fDayofWeekStr:TDayofWeekStr;
      function  GetTimeMode():TTimeMode;
      procedure SetTimeMode(pValue:TTimeMode);
      procedure Switch();
      function GetCells(pCol, pRow: Integer): char;
      procedure SetCells(pCol, pRow: Integer; const pValue: char);
      procedure BulidWeek;
      procedure FillWeek;
      function  GetTimeofWeek():String;
      procedure SetTimeofWeek(pValue:String);
      function GetDayofWeek(ADay: TDayKind): String;
      procedure SetDayofWeek(ADay: TDayKind; const pValue: String);
    protected
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                          X, Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy;override;

      procedure SetPermission(pTimePermission:TTimePermission);
      procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
                  AState: TGridDrawState);override;
      property DayofWeek[AIdx:TDayKind]:String read GetDayofWeek write SetDayofWeek;
      property Cells[ACol, ARow: Integer]: char read GetCells write SetCells;
      property TimeofWeek:String read GetTimeofWeek write SetTimeofWeek;
      property TitleCorner:String read fTitleCorner write fTitleCorner;
      property Readonly:boolean read fReadonly write fReadonly;
    published
      property TimeMode:TTimeMode read GetTimeMode write SetTimeMode default tmHour;
      property Color;
      property Font;
      property FixedColor;
      property FixedCols;
  end;

procedure Register;

implementation
uses SysUtils;

procedure Register;
begin
  RegisterComponents('Mashatan', [TMSTimeSheet]);
end;


{ MSTimePermission }
constructor TMSTimeSheet.Create(AOwner: TComponent);
begin
  inherited;
  fTitleCorner:='All';
  fDayofWeekStr[dkSaturday]:='Saturday';
  fDayofWeekStr[dkSunday]:='Sunday';
  fDayofWeekStr[dkMonday]:='Monday';
  fDayofWeekStr[dkTuesday]:='Tuesday';
  fDayofWeekStr[dkWednesday]:='Wednesday';
  fDayofWeekStr[dkThursday]:='Thursday';
  fDayofWeekStr[dkFriday]:='Friday';

  fReadonly:=false;
  fTimeMode:=tmHour;
  fWeek:=TStringList.Create;
  BulidWeek;
  fSelectOverColNo:=-1;
  fSelectOverRowNo:=-1;
  self.Ctl3D:=false;
  self.DefaultDrawing:=true;
  self.Height:=178;
  self.Width:=468;
  self.DefaultColWidth:=15;
  self.DefaultRowHeight:=21;
  self.RowHeights[0]:=20;
  self.ColWidths[0]:=80;
  self.FixedCols:=1;
  self.FixedRows:=1;
  fSelectFix.Left:=1;
  fSelectFix.Top:=1;
  fSelectFix.Right:=1;
  fSelectFix.Bottom:=1;
  self.Options:=[goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goRangeSelect,goDrawFocusSelected];
  Switch;
end;

destructor TMSTimeSheet.Destroy;
begin
  fWeek.Free;
  inherited;
end;

function TMSTimeSheet.GetTimeMode: TTimeMode;
begin
   result:=fTimeMode;
end;

procedure TMSTimeSheet.SetTimeMode(pValue: TTimeMode);
begin
  if fTimeMode <> pValue then
    begin
       fTimeMode:=pValue;
       Switch;
    end;
end;

procedure TMSTimeSheet.Switch;
begin
  self.ColCount:=byte(fTimeMode)+1;
  self.RowCount:=8;
end;

procedure TMSTimeSheet.SetPermission(
  pTimePermission: TTimePermission);
var
  vRowNo,vColNo:Integer;
  vChar:Char;
  vSelect:TGridRect;
begin
  if fReadonly then
    exit;
  vSelect:=self.Selection;
  for vRowNo:= vSelect.Top to vSelect.Bottom do
    for vColNo:= vSelect.Left to vSelect.Right do begin
      vChar:=Char(byte(pTimePermission)+48);
      Cells[vColNo,vRowNo]:=vChar;
    end;
   vSelect.Left:=vSelect.Right;
   vSelect.Top:=vSelect.Bottom;
   self.Selection :=vSelect;
  InvalidateGrid;
end;

procedure TMSTimeSheet.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  vStr:String;
  vTP:TTimePermission;
  vHeight,vWidth:Integer;
begin
  inherited;
  vStr:='';
  if (Acol=0) and (ARow=0) then
    vStr:=fTitleCorner;
  if (Acol>0) and (ARow>0) then
  begin
    if (gdSelected in AState) then
       Canvas.Brush.Color := DefaultSelectColor;
    vTP:=TTimePermission(byte(Cells[ACol, ARow])-48);
    case vTP of
     tpAllow:
     begin
         if not (gdSelected in AState) then
           Canvas.Brush.Color := ColorAllow
         else
           Canvas.Brush.Color := ColorSelectAllow;
     end;
     tpFair:
     begin
         if not (gdSelected in AState) then
           Canvas.Brush.Color := ColorFair
         else
           Canvas.Brush.Color := ColorSelectFair;
     end;
     tpDeny:
     begin
         if not (gdSelected in AState) then
           Canvas.Brush.Color := ColorDeny
         else
           Canvas.Brush.Color := ColorSelectDeny;
     end;
    end;
   // vStr:=Cells[ACol, ARow];
  end else begin
    if (Acol=0) and (ARow>0) then
      vStr:=fDayofWeekStr[TDayKind(aRow)];
    if (Acol>0) and (ARow=0) then
      vStr:=intToStr(Acol);
  end;
  if (fSelectOverColNo=Acol) and (ARow >0)  then
    Canvas.Brush.Color := ChangeBrightness(Canvas.Brush.Color,40);
  if (fSelectOverRowNo=ARow)and (ACol >0) then
    Canvas.Brush.Color := ChangeBrightness(Canvas.Brush.Color,40);
  vWidth:=Canvas.TextWidth(vStr);
  vHeight:=Canvas.TextHeight(vStr);
  Canvas.TextRect(ARect,  ARect.Left + (((ARect.Right-ARect.Left) div 2) - (vWidth div 2))  , ARect.Top+ (((ARect.Bottom-ARect.Top) div 2) - (vHeight div 2)), vStr);
end;


function TMSTimeSheet.GetCells(pCol, pRow: Integer): char;
var
  vLiner:Integer;
begin
  result:=#0;
  if (pRow > 0) and (pCol > 0) then
    if Length(fWeek[pRow-1])> byte(fTimeMode) then
      result:=fWeek[pRow-1][pCol];
end;

procedure TMSTimeSheet.SetCells(pCol, pRow: Integer;
  const pValue: char);
var
  vStr:String;
begin
  if (pRow > 0) and (pCol > 0) then begin
    vStr:=fWeek[pRow-1];
    vStr[pCol]:=pValue;
    fWeek[pRow-1]:=vStr;
  end;
end;

function TMSTimeSheet.GetDayofWeek(ADay : TDayKind): String;
begin
  result:=fDayofWeekStr[ADay];
end;

procedure TMSTimeSheet.SetDayofWeek(ADay : TDayKind; const pValue: String);
begin
  fDayofWeekStr[ADay]:=pValue;
end;

procedure TMSTimeSheet.BulidWeek;
var
  vRowNo,vColNo:Integer;
  vSum:String;
begin
  for vRowNo:=1 to 7 do begin
      vSum:='';
      for vColNo:=1 to 96 do
         vSum:=vSum+Char(byte(tpFair)+48);;
      fWeek.Add(vSum);
  end;
end;

procedure TMSTimeSheet.FillWeek;
var
  vRowNo,vColNo:Integer;
  vSum:String;
begin
  for vRowNo:=1 to 7 do begin
      vSum:='';
      for vColNo:=1 to 96 do
         vSum:=vSum+Char(byte(tpFair)+48);;
      fWeek[vRowNo-1]:=vSum;
  end;
end;


procedure TMSTimeSheet.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vRowNo,vColNo:Integer;
begin
  inherited;
  if Button = mbLeft then begin
    self.MouseToCell(X,Y,vColNo,vRowNo);
    if (vColNo=0) and (vRowNo=0) then
    begin
       fSelectFix.Left:=1;
       fSelectFix.Top:=1;
       fSelectFix.Right:=byte(fTimeMode);
       fSelectFix.Bottom:=7;
       self.Selection:=fSelectFix;
    end;
    if (vColNo=0) and (vRowNo>0) then
    begin
       fSelectFix.Left:=1;
       fSelectFix.Top:=vRowNo;
       fSelectFix.Right:=self.ColCount-1;
       fSelectFix.Bottom:=vRowNo;
       self.Selection:=fSelectFix;
    end;
    if (vColNo>0) and (vRowNo=0) then
    begin
       fSelectFix.Left:=vColNo;
       fSelectFix.Top:=1;
       fSelectFix.Right:=vColNo;
       fSelectFix.Bottom:=self.RowCount-1;
       self.Selection:=fSelectFix;
    end;
  end;
end;

procedure TMSTimeSheet.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vRowNo,vColNo,vTemp:Integer;
begin
  inherited;
  if (csDesigning in ComponentState) then
    exit;
  self.MouseToCell(X,Y,vColNo,vRowNo);
  if (ssLeft in Shift) then
  begin
    if (vColNo=0) and (vRowNo>0)  then
    begin
//       fSelectFix.Left:=1;
//       fSelectFix.Top:=vRowNo;
       fSelectFix.Right:=self.ColCount-1;
       fSelectFix.Bottom:=vRowNo;
       self.Selection:=fSelectFix;
    end;
    if (vColNo>0) and (vRowNo=0) then
    begin
//       fSelectFix.Left:=vColNo;
//       fSelectFix.Top:=1;
       fSelectFix.Right:=vColNo;
       fSelectFix.Bottom:=self.RowCount-1;
       self.Selection:=fSelectFix;
    end;
  end else begin
  {***************** Mouseover Colume ******************}
    if (vColNo>0)and (vRowNo=0) then begin
      if (fSelectOverColNo<>vColNo) then begin
        self.InvalidateCol(fSelectOverColNo);
        fSelectOverColNo:=vColNo;
        self.InvalidateCol(vColNo);
      end;
    end else
      if fSelectOverColNo<>-1 then
      begin
          vTemp:=fSelectOverColNo;
          fSelectOverColNo:=-1;
          self.InvalidateCol(vTemp);
      end;

    {***************** Mouseover Row ******************}
    if (vColNo=0)and (vRowNo>0) then begin
      if (fSelectOverRowNo<>vRowNo) then begin
        self.InvalidateRow(fSelectOverRowNo);
        fSelectOverRowNo:=vRowNo;
        self.InvalidateRow(vRowNo);
      end;
    end else
      if fSelectOverRowNo<>-1 then
      begin
          vTemp:=fSelectOverRowNo;
          fSelectOverRowNo:=-1;
          self.InvalidateRow(vTemp);
      end;
  end;
end;

function TMSTimeSheet.GetTimeofWeek: String;
var
  vCount:Integer;
  vSum:String;
begin
  vSum:='';
  for vCount:=0 to fWeek.Count-1 do
    vSum:=vSum+copy(fWeek.Strings[vCount],1,byte(fTimeMode));
  result:=vSum;
end;

procedure TMSTimeSheet.SetTimeofWeek(pValue:String);
var
  vWeek,vUnit,vLen,vPart,vMod,vLenUnit:Integer;
  vSum:String;
begin
  FillWeek;
  vLenUnit:=byte(fTimeMode);
  if Length(pValue) > (vLenUnit*7) then
    Delete(pValue,(vLenUnit*7)+1,Length(pValue));
  vLen:=Length(pValue);
  vPart:=vLen div (vLenUnit);
  vMod:=vLen mod (vLenUnit);
  for vWeek:=1 to vPart do begin
    vSum:=fWeek.Strings[vWeek-1];
    Delete(vSum,1,vLenUnit);
    vSum:=Copy(pValue,((vWeek-1)*vLenUnit)+1,vLenUnit)+vSum;
    fWeek.Strings[vWeek-1]:=vSum;
  end;
  if vMod >0 then
  begin
    vWeek:=vPart;
    vSum:=fWeek.Strings[vWeek];
    Delete(vSum,1,vMod);
    vSum:=Copy(pValue,(vWeek*vLenUnit)+1,vMod)+vSum;
    fWeek.Strings[vWeek]:=vSum;
  end;
  Self.InvalidateGrid;
end;


procedure TMSTimeSheet.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vRowNo,vColNo:Integer;
begin
  inherited MouseUp (Button, Shift, X, Y);
  if Button=mbRight then begin
    self.MouseToCell(X,Y,vColNo,vRowNo);

  end;
end;

procedure TMSTimeSheet.CMMouseLeave(var Message: TMessage);
var
  vTemp:Integer;
begin
  inherited;

  vTemp:=fSelectOverColNo;
  fSelectOverColNo:=-1;
  self.InvalidateCol(vTemp);

  vTemp:=fSelectOverRowNo;
  fSelectOverRowNo:=-1;
  self.InvalidateRow(vTemp);
end;

end.
