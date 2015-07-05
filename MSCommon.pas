{
   Author:   Ali Mashatan
   E-mail:  ali.mashatan@gmail.com
   website: https://github.com/Mashatan
   Date:     2008-02-28
   License:  GNU Public License ( GPL3 ) [http://www.gnu.org/licenses/gpl-3.0.txt]
}

unit MSCommon;

interface
uses Forms, Classes, Graphics;

  function ChangeBrightness(Color : TColor; Delta : integer) : TColor;
  function IsDesigning(pControl:TComponent): Boolean;
  function IsBusyComponent(pControl:TComponent): Boolean;

implementation
uses math;
type
  TsColor = record
    case integer of
      0  : (C : TColor);
      1  : (R, G, B, A : Byte);
      2  : (I : integer);
    end;

function IsDesigning(pControl:TComponent): Boolean;
begin
  Result := (csDesigning in pControl.ComponentState) or
            (csDesignInstance in pControl.ComponentState);
end;

function IsBusyComponent(pControl:TComponent): Boolean;
begin
  Result :=  ( csLoading in pControl.ComponentState ) or
        ( csReading in pControl.ComponentState );
end;

function ChangeBrightness(Color : TColor; Delta : integer) : TColor;
var
  C : TsColor;
  dR, dG, dB : real;
begin
  Result := Color;
  if Delta = 0 then Exit;
  C.C := Color;

  if Delta > 0 then begin
    dR := (255 - C.R) / 100;
    dG := (255 - C.G) / 100;
    dB := (255 - C.B) / 100;
  end
  else begin
    dR := C.R / 100;
    dG := C.G / 100;
    dB := C.B / 100;
  end;

  C.R := max(min(Round(C.R + Delta * dR), 255), 0);
  C.G := max(min(Round(C.G + Delta * dG), 255), 0);
  C.B := max(min(Round(C.B + Delta * dB), 255), 0);
  Result := C.C;
end;

end.
