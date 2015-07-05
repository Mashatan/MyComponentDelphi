{
   Author:   Ali Mashatan
   E-mail:  ali.mashatan@gmail.com
   website: https://github.com/Mashatan
   Date:     2008-02-28
   License:  GNU Public License ( GPL3 ) [http://www.gnu.org/licenses/gpl-3.0.txt]
}

unit MSStatistics;

interface
uses Controls, Windows, Messages, Graphics, SysUtils, ExtCtrls, Menus,
     Classes, ComCtrls, Contnrs, MSHints, MSPopupMenu;
const
  coMAX_MEMORY_CNT = 1500;
  coMAX_ROW=4;
  coMAX_COL=4;
  coNODE_SPACE_SIZE=2;
  coNODE_SIZE=6;
  coGRID_SIZE = 16;
type
  TNodeType=(ntRect,ntFrameRect,ntElli,ntFrameElli);
  TMSStatistics=class;

  TMSSeriesNode=class(TObject)
  private
    fValueX:Integer;
    fValueY:Integer;
    fDescription:string;
    fNodeType:TNodeType;
    fFlash:Boolean;
    fFlashColor:Integer;
    fFlashLevel:Integer;
    fFlashToggle:Integer;
    fTimeMousePause:Cardinal;
    fTimeMouseLock:boolean;
    fID:string;
  public
    constructor Create;
    function  ChangeLevel(pColor:TColor):TColor;
    function  ExistMouse:boolean;
    procedure ResetMouse;
    property  ValueY:Integer     read fValueY      write fValueY;
    property  ValueX:Integer     read fValueX      write fValueX;
    property  Decription:string  read fDescription write fDescription;
    property  NodeType:TNodeType read fNodeType    write fNodeType;
    property  Flash:Boolean      read fFlash       write fFlash;
    property  ID:string          read fID          write fID;
  end;

  TMSSeriesItem=class (TCollectionItem)
  private
    fColor:     TColor;
    fStyle:     TPenStyle;
    fCaption:   string;
    fTag:       Integer;
    fListSeries:TObjectList;
    function    GetCount:Integer;
    function    GetMaxX:Integer;
    function    GetMaxY:Integer;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection:TCollection); override;
    destructor  Destroy; override;
    procedure   Assign(Source: TPersistent); override;
    property    Count:Integer read GetCount;
    function    GetNode(pIndex:Integer):TMSSeriesNode;
    function    Add(pValueX,pValueY:Integer;pNodeType:TNodeType=ntRect;pFlash:Boolean=false):TMSSeriesNode;
    property    MaxX:Integer read GetMaxX;
    property    MaxY:Integer read GetMaxY;
  published
    property    Color:TColor read fColor write fColor;
    property    Style:TPenStyle read fStyle write fStyle;
    property    Caption:string read fCaption write fCaption;
    property    Tag:Integer read fTag write fTag;
  end;

  TMSSeries=class (TOwnedCollection)
  private
    function    GetItem(Index: Integer): TMSSeriesItem;
    procedure   SetItem(Index: Integer; Value: TMSSeriesItem);
  protected
  public
    function    Add:TMSSeriesItem;
    function    Insert(index: Integer): TMSSeriesItem;
    property    Items[Index: Integer]: TMSSeriesItem read GetItem write SetItem; default;
  end;

  TMSVertical=class(TPersistent)
  private
    fOwner:     TMSStatistics;
    fFont:      TFont;
    fColor:     TColor;
    fMargin:    Integer;
    fCaption:   TCaption;
    procedure   SetColor(pValue:TColor);
    procedure   SetFont(pValue:TFont);
    procedure   SetMargin(pValue:Integer);
    procedure   SetCaption(pValue:TCaption);
    procedure   Repaint;
  public
    constructor Create(AOwner: TMSStatistics);
    destructor  Destroy;override;
    procedure   Assign(Source: TPersistent); override;
   published
    property    Color:TColor read fColor write SetColor;
    property    Font:TFont read fFont write SetFont;
    property    Caption:TCaption read fCaption write SetCaption;
    property    Margin:Integer read fMargin write SetMargin;
  end;

  TMSHorizontal=class(TPersistent)
  private
    fOwner:     TMSStatistics;
    fFont:      TFont;
    fColor:     TColor;
    fMargin:    Integer;
    fCaption:   TCaption;
    procedure   SetColor(pValue:TColor);
    procedure   SetFont(pValue:TFont);
    procedure   SetMargin(pValue:Integer);
    procedure   SetCaption(pValue:TCaption);
    procedure   Repaint;
  public
    constructor Create(AOwner: TMSStatistics);
    destructor  Destroy;override;
    procedure   Assign(Source: TPersistent); override;
  published
    property    Color:TColor read fColor write SetColor;
    property    Font:TFont read fFont write SetFont;
    property    Caption:TCaption read fCaption write SetCaption;
    property    Margin:Integer read fMargin write SetMargin;
  end;

  TMSAppearance=class(TPersistent)
  private
    fOwner:     TMSStatistics;
    fGridColor: TColor;
    fBGGridColor:TColor;
    fFrameColor:TColor;
    fHintColor:TColor;
    procedure   SetGridColor(pValue:TColor);
    procedure   SetBGGridColor(pValue:TColor);
    procedure   SetFrameColor(pValue:TColor);
    procedure   SetHintColor(pValue:TColor);
    procedure   Repaint;
  public
    constructor Create(AOwner: TMSStatistics);
    destructor  Destroy;override;
    procedure   Assign(Source: TPersistent); override;
  published
    property    GridColor:TColor read fGridColor write SetGridColor;
    property    BGGridColor:TColor read fBGGridColor write SetBGGridColor;
    property    FrameColor:TColor read fFrameColor write SetFrameColor;
    property    HintColor:TColor read fHintColor write SetHintColor;
  end;

  TMSBeforePopupEvent = procedure ( Sender: TObject; NodeID:string; var Active:boolean ) of object;
  TMSStatistics=class(TGraphicControl)
  private
    fImage:TBitmap;
    fOwner:TComponent;
    fActive:Boolean;
    fGridOffset: Integer;
    fDefaultMaxX,fMaxX:Integer;
    fDefaultMaxY,fMaxY:Integer;
    fOnBeforDraw:TNotifyEvent;
    fOnAfterDraw:TNotifyEvent;
    fAppearance:TMSAppearance;
    fHorizontal:TMSHorizontal;
    fVertical:TMSVertical;
    fSeries:TMSSeries;
    fWidthGrid,fHeightGrid:Integer;
    fTimer:TTimer;
    fFlash:Boolean;
    fBalloonHint: TMSBalloonHint;
    fCS: TRTLCriticalSection;
    fPopupMenu: TMSPopupMenu;
    fBeforePopup:TMSBeforePopupEvent;

    procedure   Calculator(pNode: TMSSeriesNode;var pAxisX, pAxisY:Integer);
    procedure   DrawStatistItem(pIdxSeries,pIdxItem:Integer;pCanvas:TCanvas);
    procedure   DrawStatist;
    procedure   DrawGrid;
    procedure   DrawFrame;
    procedure   DrawLegend;
    procedure   TextStatist();
    function    IsDesigning: Boolean;
    procedure   doTimer(Sender:TObject);
    procedure   SetFlash(const pValue: Boolean);
//    procedure SetStep(pValue:Integer);
    function    CreateAngledFont(Font: HFont;
                               Angle: Longint;
                               Quality: byte = PROOF_QUALITY): HFont;
    procedure   TextOutA(Canvas: TCanvas; X, Y, Angle: Integer;
                                     Text: string);
  protected
    procedure   Loaded; override;
    procedure   Paint;override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;

    property    OnBeforDraw:TNotifyEvent read fOnBeforDraw write fOnBeforDraw;
    property    OnAfterDraw:TNotifyEvent read fOnAfterDraw write fOnAfterDraw;
    property    Flash:Boolean read fFlash write SetFlash;
  published
    property    Align;
    property    PopupMenu: TMSPopupMenu read fPopupMenu write fPopupMenu;
    property    Series:TMSSeries read fSeries write fSeries;
    property    Appearance:TMSAppearance read fAppearance write fAppearance;
    property    Vertical:TMSVertical read fVertical write fVertical;
    property    Horizontal:TMSHorizontal read fHorizontal write fHorizontal;
    property    DefaultMaxX:Integer read fDefaultMaxX write fDefaultMaxX stored true default coMAX_COL;
    property    DefaultMaxY:Integer read fDefaultMaxY write fDefaultMaxY stored true default coMAX_ROW;
    property    OnBeforPopup:TMSBeforePopupEvent read fBeforePopup write fBeforePopup;
  end;
implementation

uses commctrl, MSCommon, Math;
var
  gSeriesCS:TRTLCriticalSection;

{ TStatistics }

constructor TMSStatistics.Create(AOwner:TComponent);
begin
  inherited;
  Width:=400;
  Height:=200;
  InitializeCriticalSection(fCS);
  InitializeCriticalSection(gSeriesCS);
  fOwner:=AOwner;
  Parent:=TWinControl(AOwner);
  fAppearance:=TMSAppearance.Create(Self);
  fHorizontal:=TMSHorizontal.Create(Self);
  fVertical:=TMSVertical.Create(Self);
  fSeries:=TMSSeries.Create(Self,TMSSeriesItem);
  fDefaultMaxY:=coMAX_ROW;
  fDefaultMaxX:=coMAX_COL;
  fGridOffset:=0;
  fActive:=false;
  fImage:=TBitmap.Create;
  fTimer:=TTimer.Create(self);
  fTimer.Interval:=120;
  fTimer.Enabled:=false;
  fTimer.OnTimer:=doTimer;
  fBalloonHint:=TMSBalloonHint.Create(self);
  fBalloonHint.Delay:=0;
  fBalloonHint.Title:='';
  fBalloonHint.Description:='';
  fBalloonHint.Style:=bhsBalloon;
  Paint;
end;

destructor TMSStatistics.Destroy;
begin
  FreeAndNil(fHorizontal);
  FreeAndNil(fVertical);
  FreeAndNil(fAppearance);
  FreeAndNil(fSeries);
  FreeAndNil(fImage);
  DeleteCriticalSection(gSeriesCS);
  DeleteCriticalSection(fCS);
  inherited;
end;

procedure TMSStatistics.doTimer(Sender: TObject);
var
  vCountI,vCountJ: Integer;
  vSeriesNode:TMSSeriesNode;
begin
  if IsDesigning then
    exit;
  EnterCriticalSection(gSeriesCS);
  try
    for  vCountI := 0 to fSeries.Count - 1 do
    begin
      for  vCountJ := 0 to fSeries.Items[vCountI].Count - 1 do
      begin
        vSeriesNode:=TMSSeriesNode(fSeries.Items[vCountI].GetNode(vCountJ));
        if vSeriesNode.Flash then
          DrawStatistItem(vCountI,vCountJ,Self.Canvas);
      end;
    end;
  finally
    LeaveCriticalSection(gSeriesCS);
  end;
end;

function TMSStatistics.CreateAngledFont(Font: HFont; Angle: Longint;
  Quality: byte = PROOF_QUALITY): HFont;
var
  FontInfo: TLogFont;    // Font information structure
begin
  if GetObject(Font, SizeOf(FontInfo), @FontInfo) = 0 then begin
    Result := 0;
    exit;
  end;
  FontInfo.lfEscapement := Angle;
  FontInfo.lfOrientation := Angle;
  FontInfo.lfQuality := Quality;
  Result := CreateFontIndirect(FontInfo);
end;

procedure TMSStatistics.TextOutA(Canvas: TCanvas; X, Y, Angle: Integer;
  Text: string);
var
  OriginalFont, AngledFont: HFont;
begin
  AngledFont := CreateAngledFont(Canvas.Font.Handle, Angle);
  if AngledFont <> 0 then
  begin
    OriginalFont := SelectObject(Canvas.Handle, AngledFont);
    if OriginalFont <> 0 then
    begin
      Canvas.TextOut(X, Y, Text);
      if SelectObject(Canvas.Handle, OriginalFont) = 0 then begin
        Canvas.Font.Handle := AngledFont;
        exit;
      end;
    end;
    DeleteObject(AngledFont)
  end;
end;

procedure TMSStatistics.DrawFrame;
var
  vRect:TRect;
begin
  vRect := Rect(0, 0, self.Width, self.Height);
  if Vertical.Caption <> '' then
    TextOutA(fImage.Canvas, vRect.Left, vRect.Top+(self.Height div 2) + (fImage.Canvas.TextWidth(Vertical.Caption) div 2) , 90*10, Vertical.Caption);
  fImage.Canvas.Pen.Style:= psSolid;
  fImage.Canvas.Pen.Color :=   fAppearance.FrameColor;
  fImage.Canvas.Pen.Width:= 1;
  fImage.Canvas.MoveTo(vRect.Left,vRect.Top);
  fImage.Canvas.LineTo(vRect.Right-1,vRect.Top);
  fImage.Canvas.LineTo(vRect.Right-1,vRect.Bottom-1);
  fImage.Canvas.LineTo(vRect.Left,vRect.Bottom-1);
  fImage.Canvas.LineTo(vRect.Left,vRect.Top);
  fImage.Canvas.MoveTo(fVertical.Margin,0);
  fImage.Canvas.LineTo(fVertical.Margin,fHeightGrid);
//  fImage.Canvas.MoveTo(fVertical.Margin,fHeightGrid);
  fImage.Canvas.LineTo(fWidthGrid+fVertical.Margin,fHeightGrid);
  if Horizontal.Caption <> '' then
    fImage.Canvas.TextOut(vRect.Left+fVertical.Margin, vRect.Bottom - (fHorizontal.Margin div 2) - (fImage.Canvas.TextHeight(Vertical.Caption) div 2) - 2, Horizontal.Caption);
end;

procedure TMSStatistics.DrawGrid;
var
  vRect:TRect;
  AxisX,AxisY:Integer;
begin
    fImage.Canvas.Brush.Color :=   fAppearance.BGGridColor;
    vRect := Rect(0, 0, self.Width, self.Height);
    fImage.Canvas.FillRect(vRect);

    fImage.Canvas.Pen.Color := fAppearance.GridColor;
    fImage.Canvas.Pen.Style:= psSolid;
    fImage.Canvas.Pen.Width:= 1;
    AxisX := fVertical.Margin + fGridOffset;
    while AxisX < fWidthGrid+fVertical.Margin do
    begin
      if AxisX <> 0 then
      begin
        fImage.Canvas.MoveTo(AxisX, 0);
        fImage.Canvas.LineTo(AxisX, fHeightGrid);
      end;
      inc (AxisX, coGRID_SIZE);
    end;

    AxisY := coGRID_SIZE;
    while AxisY < fHeightGrid do
    begin
      fImage.Canvas.MoveTo(fVertical.Margin, AxisY);
      fImage.Canvas.LineTo(fWidthGrid+fVertical.Margin, AxisY);
      inc(AxisY, coGRID_SIZE);
    end;
end;


procedure TMSStatistics.DrawLegend;
var
  vCount:integer;
  vx:Integer;
begin
  vx:=fWidthGrid+fVertical.Margin;
  for  vCount := 0 to fSeries.Count - 1 do
  begin
    fImage.Canvas.Brush.Color:=fSeries[vCount].Color;
    fImage.Canvas.Pen.Color:=clBlack;
    fImage.Canvas.Rectangle(Rect(vx-15,fHeightGrid+5,vx-5,fHeightGrid+15));
    dec(vx,18);
    fImage.Canvas.Brush.Color:= fHorizontal.Color;
    vx:=vx-fImage.Canvas.TextWidth(fSeries[vCount].Caption);
    fImage.Canvas.Font.Color:= fHorizontal.Font.Color; //fSeries[vCount].Color;
    fImage.Canvas.TextOut(vx,fHeightGrid+2,fSeries[vCount].Caption);
  end;
end;

procedure TMSStatistics.Calculator(pNode: TMSSeriesNode;var pAxisX, pAxisY:Integer);
begin
  pAxisY := fHeightGrid  - ((fHeightGrid * pNode.ValueY) div fMaxY);
  pAxisX := fVertical.Margin + ((fWidthGrid * pNode.ValueX) div fMaxX);

  if pAxisY <= 0 then pAxisY := coNODE_SIZE;
  if pAxisY >= fHeightGrid-coNODE_SIZE-coNODE_SPACE_SIZE then pAxisY := fHeightGrid - coNODE_SIZE - coNODE_SPACE_SIZE;

  if pAxisX <= 0 then pAxisX := coNODE_SIZE;
  if pAxisX >= fWidthGrid then pAxisX := fWidthGrid - coNODE_SIZE;

end;

procedure TMSStatistics.DrawStatistItem(pIdxSeries,pIdxItem:Integer;pCanvas:TCanvas);
var
  vSeriesNode:TMSSeriesNode;
  vColor:TColor;
  pAxisX,pAxisY:Integer;
begin
  vSeriesNode:=TMSSeriesNode(fSeries.Items[pIdxSeries].GetNode(pIdxItem));
  if vSeriesNode=nil then
    exit;
  Calculator(vSeriesNode,pAxisX,pAxisY);
  vColor := vSeriesNode.ChangeLevel( fSeries.Items[pIdxSeries].Color );
//      vColor := fSeries.Items[vCountI].Color;
  pCanvas.Pen.Color :=   vColor;
  pCanvas.Brush.Color := vColor;

  pCanvas.Brush.Style:=bsSolid;
  if (vSeriesNode.NodeType = ntRect)  or (vSeriesNode.NodeType = ntFrameRect) then
    pCanvas.Rectangle(pAxisX,pAxisY,pAxisX+coNODE_SIZE,pAxisY+coNODE_SIZE);
  if (vSeriesNode.NodeType = ntElli)  or (vSeriesNode.NodeType = ntFrameElli) then
    pCanvas.Ellipse(pAxisX,pAxisY,pAxisX+coNODE_SIZE,pAxisY+coNODE_SIZE);

  pCanvas.Brush.Style:=bsClear;
  if (vSeriesNode.NodeType = ntFrameRect) then
    pCanvas.Rectangle(pAxisX-coNODE_SPACE_SIZE,
                            pAxisY-coNODE_SPACE_SIZE,
                            pAxisX+(coNODE_SIZE+coNODE_SPACE_SIZE),
                            pAxisY+(coNODE_SIZE+coNODE_SPACE_SIZE));
  if (vSeriesNode.NodeType = ntFrameElli) then
    pCanvas.Ellipse(pAxisX-coNODE_SPACE_SIZE,
                            pAxisY-coNODE_SPACE_SIZE,
                            pAxisX+(coNODE_SIZE+coNODE_SPACE_SIZE),
                            pAxisY+(coNODE_SIZE+coNODE_SPACE_SIZE));
end;

procedure TMSStatistics.TextStatist();
var
  vRect:TRect;
begin
  fImage.Canvas.Font.Assign(fVertical.Font);
  if (fVertical.Margin <>0) then
  begin

    vRect := Rect(0, 0,fVertical.Margin, fHeightGrid);
    fImage.Canvas.Brush.Color := fVertical.Color;
    fImage.Canvas.Brush.Style:= bsSolid;
    fImage.Canvas.FillRect(vRect);
    vRect := Rect(0, 0,fVertical.Margin, fHeightGrid);

  {  vFactorGra:=((vRect.Bottom-vRect.Top)  div fStep) -3;
    vFactorNum:= fMax div fStep;
    for vCountI := 0 to fStep do
    begin
      vTempStr:=IntToStr(fMax - (vFactorNum * vCountI));
      fImage.Canvas.TextOut(2,vRect.Top+(vFactorGra*vCountI) ,vTempStr);
    end;
  }
  end;
  if fHorizontal.Margin <>0  then
  begin
    vRect := Rect(0, fHeightGrid, fWidthGrid+fVertical.Margin, fHeightGrid+fHorizontal.Margin);
    fImage.Canvas.Brush.Style:= bsSolid;
    fImage.Canvas.Brush.Color := fHorizontal.Color;
    fImage.Canvas.FillRect(vRect);
//    fImage.Canvas.TextOut(5,  fHeightGrid+(fHorizontal.Margin div 2)-4,fCaptionUnit);
  end;
end;

procedure TMSStatistics.DrawStatist;
var
  vCountI,vCountJ: Integer;
begin
  fImage.Canvas.Pen.Style:= psSolid;
  fImage.Canvas.Pen.Width:= 1;
  for  vCountI := 0 to fSeries.Count - 1 do
  begin
    if self.fMaxX < fSeries.Items[vCountI].GetMaxX then
      self.fMaxX := fSeries.Items[vCountI].GetMaxX;
    if self.fMaxY < fSeries.Items[vCountI].GetMaxY then
    self.fMaxY := fSeries.Items[vCountI].GetMaxY;
  end;
  for  vCountI := 0 to fSeries.Count - 1 do
  begin
    for  vCountJ := 0 to fSeries.Items[vCountI].Count - 1 do
    begin
      DrawStatistItem(vCountI,vCountJ,fImage.Canvas);
    end;
  end;
end;

function TMSStatistics.IsDesigning: Boolean;
begin
  Result := (csDesigning in fOwner.ComponentState);
end;


procedure TMSStatistics.Loaded;
begin
  inherited;
  fMaxX:=DefaultMaxX;
  fMaxY:=DefaultMaxY;
end;

procedure TMSStatistics.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vCountI,vCountJ: Integer;
  pAxisX,pAxisY:Integer;
  vSeriesNode:TMSSeriesNode;
  vFlag:boolean;
  P: TPoint;
  vActive:Boolean;
begin
  inherited;
  if IsDesigning then
    exit;
  vFlag:=true;
  if (ssRight in Shift) then
    if Assigned(fPopupMenu) then
      fBalloonHint.HideHint;
  for  vCountI := 0 to fSeries.Count - 1 do
  begin
    for  vCountJ := 0 to fSeries.Items[vCountI].Count - 1 do
    begin
      vSeriesNode:=TMSSeriesNode(fSeries.Items[vCountI].GetNode(vCountJ));
      Calculator(vSeriesNode,pAxisX,pAxisY);
      if (pAxisX-5 < x) and (pAxisY-5 < y) and
         (pAxisY+coNODE_SIZE+5 > y) and (pAxisX+coNODE_SIZE+5 > X) then
      begin
        if (ssRight in Shift) then
          if Assigned(fPopupMenu) then
          begin
            P := ClientToScreen(Point(X , Y));
            fBalloonHint.HideHint;
            vActive:=true;
            if Assigned(fBeforePopup) then
              fBeforePopup(self, vSeriesNode.ID, vActive);
            if (vActive) then
              fPopupMenu.Popup(P.X, P.Y, vSeriesNode.ID);
            Exit;
          end;
         if vSeriesNode.ExistMouse and (vSeriesNode.Decription <> '') then
         begin
           fBalloonHint.Description:=vSeriesNode.Decription;
           fBalloonHint.Color:=fAppearance.HintColor;
           fBalloonHint.ShowHint();
         end;
         vFlag:=false;
      end else
        vSeriesNode.ResetMouse;
    end;
  end;
  if vFlag then
    fBalloonHint.HideHint;
end;

procedure TMSStatistics.Paint;
var
  vRect: TRect;
begin
  inherited;
  if Assigned(fOnBeforDraw) then
    fOnBeforDraw(self);
  fWidthGrid := self.Width-fVertical.Margin;
  fHeightGrid := self.Height-fHorizontal.Margin;
  fImage.Height:=self.Height;
  fImage.Width:=self.Width;
  fImage.Canvas.Lock;
  try
    DrawGrid();
    if not (IsDesigning()) then
      DrawStatist();
    TextStatist();
    DrawFrame();
    DrawLegend();
    vRect:=self.ClientRect;
    Canvas.CopyRect(vRect,fImage.Canvas,vRect);
  finally
    fImage.Canvas.Unlock;
  end;
  if Assigned(fOnAfterDraw) then
    fOnAfterDraw(self);
end;



procedure TMSStatistics.SetFlash(const pValue: Boolean);
begin
  if fFlash<>pValue then
  begin
    fFlash := pValue;
    fTimer.Enabled := pValue and not IsDesigning;
  end;
end;

{ TSeries }

function TMSSeries.Add: TMSSeriesItem;
begin
  EnterCriticalSection(gSeriesCS);
  Result := TMSSeriesItem(inherited Add);
  LeaveCriticalSection(gSeriesCS);
end;

function TMSSeries.Insert(index: Integer): TMSSeriesItem;
begin
  EnterCriticalSection(gSeriesCS);
  Result := TMSSeriesItem(inherited Insert(index));
  LeaveCriticalSection(gSeriesCS);
end;

procedure TMSSeries.SetItem(Index: Integer; Value: TMSSeriesItem);
begin
  EnterCriticalSection(gSeriesCS);
  inherited SetItem(Index, Value);
  LeaveCriticalSection(gSeriesCS);
end;

function TMSSeries.GetItem(Index: Integer): TMSSeriesItem;
begin
  EnterCriticalSection(gSeriesCS);
  Result := TMSSeriesItem(inherited GetItem(Index));
  LeaveCriticalSection(gSeriesCS);
end;

{ TSerieItem }

function TMSSeriesItem.Add(pValueX, pValueY:Integer; pNodeType:TNodeType=ntRect;pFlash:Boolean=false):TMSSeriesNode;
var
  vSeriesNode:TMSSeriesNode;
begin
  EnterCriticalSection(gSeriesCS);
  vSeriesNode:=TMSSeriesNode.Create;
  vSeriesNode.ValueY:=pValueY;
  vSeriesNode.ValueX:=pValueX;
  vSeriesNode.NodeType:=pNodeType;
  vSeriesNode.Flash:=pFlash;
  fListSeries.Add(vSeriesNode);
  LeaveCriticalSection(gSeriesCS);
  result:=vSeriesNode;
end;

procedure TMSSeriesItem.Assign(Source: TPersistent);
begin
  if Source is TMSSeriesItem then
  begin
    self.Color := TMSSeriesItem(Source).Color;
    self.Style := TMSSeriesItem(Source).Style;
    self.Caption := TMSSeriesItem(Source).Caption;
    self.Tag := TMSSeriesItem(Source).Tag;
  end else
    inherited Assign(Source);
end;

constructor TMSSeriesItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fListSeries:=TObjectList.Create;
  fListSeries.OwnsObjects:=true;
end;

destructor TMSSeriesItem.Destroy;
begin
  fListSeries.Free;
  inherited Destroy;
end;

function TMSSeriesItem.GetCount: Integer;
begin
  result:=fListSeries.Count;
end;

function TMSSeriesItem.GetDisplayName: string;
begin
  if fCaption='' then
    Result := 'Series'+inttostr(index)
  else
    Result := fCaption;
end;

function TMSSeriesItem.GetMaxX: Integer;
var
  vCount:Integer;
begin
  Result:=0;
  for vCount := 0 to fListSeries.Count - 1 do
    if Result < TMSSeriesNode(fListSeries.Items[vCount]).fValueX then
      Result:=TMSSeriesNode(fListSeries.Items[vCount]).fValueX;
end;

function TMSSeriesItem.GetMaxY: Integer;
var
  vCount:Integer;
begin
  Result:=0;
  for vCount := 0 to fListSeries.Count - 1 do
    if Result < TMSSeriesNode(fListSeries.Items[vCount]).fValueY then
      Result:=TMSSeriesNode(fListSeries.Items[vCount]).fValueY;
end;

function TMSSeriesItem.GetNode(pIndex: Integer): TMSSeriesNode;
begin
  EnterCriticalSection(gSeriesCS);
  Result:=nil;
  if fListSeries.Count > pIndex then
    Result:=TMSSeriesNode(fListSeries.Items[pIndex]);
  LeaveCriticalSection(gSeriesCS);
end;

{ TAppearance }

procedure TMSAppearance.Assign(Source : TPersistent);
begin
  if Source is TMSAppearance then
  begin
      Self.GridColor := TMSAppearance(Source).GridColor;
      Self.BGGridColor := TMSAppearance(Source).BGGridColor;
      Self.FrameColor := TMSAppearance(Source).FrameColor;
      Self.HintColor := TMSAppearance(Source).HintColor;
  end
  else
    inherited;
end;

constructor TMSAppearance.Create(AOwner: TMSStatistics);
begin
  inherited Create;
  fOwner:=AOwner;
  fBGGridColor:=$00E7F9FE;
  fFrameColor:=$002B5BAC;
  fGridColor:=$00A3D6FC;
  fHintColor:=$00CDE6FE;
end;

destructor TMSAppearance.Destroy;
begin
  inherited;
end;

procedure TMSAppearance.Repaint;
begin
  with FOwner do
   if not (csLoading in ComponentState) then
     Invalidate;
end;

procedure TMSAppearance.SetBGGridColor(pValue: TColor);
begin
  if (fBGGridColor<>pValue) then
  begin
    fBGGridColor:=pValue;
    Repaint;
  end;
end;

procedure TMSAppearance.SetFrameColor(pValue: TColor);
begin
  if (fFrameColor<>pValue) then
  begin
    fFrameColor:=pValue;
    Repaint;
  end;
end;

procedure TMSAppearance.SetGridColor(pValue: TColor);
begin
  if (fGridColor<>pValue) then
  begin
    fGridColor:=pValue;
    Repaint;
  end;
end;

procedure TMSAppearance.SetHintColor(pValue: TColor);
begin
  if (fHintColor<>pValue) then
  begin
    fHintColor:=pValue;
//    Repaint;
  end;
end;

{ TVertical }

constructor TMSVertical.Create(AOwner: TMSStatistics);
begin
  inherited Create;
  fOwner:=AOwner;
  fFont:=TFont.Create;
  fFont.Name:='Tahoma';
  fFont.Color:=clWhite;
  fMargin:=21;
  fColor:=$008F75FB;
end;

destructor TMSVertical.Destroy;
begin
  fFont.Free;
  inherited;
end;


procedure TMSVertical.Assign( Source: TPersistent );
begin
  if Source is TMSVertical then
  begin
    Self.Color:= TMSVertical(Source).Color;
    if Assigned(TMSVertical(Source).Font) then
      Self.Font.Assign(TMSVertical(Source).Font);
    Self.Margin:=TMSVertical(Source).Margin;
  end
  else
    inherited;
end;

procedure TMSVertical.Repaint;
begin
  with FOwner do
   if not (csLoading in ComponentState) then
     Invalidate;
end;

procedure TMSVertical.SetCaption(pValue: TCaption);
begin
  fCaption := pValue;
  Repaint;
end;

procedure TMSVertical.SetColor(pValue: TColor);
begin
  if (fColor<>pValue) then
  begin
    fColor:=pValue;
    Repaint;
  end;
end;

procedure TMSVertical.SetFont(pValue: TFont);
begin
  if (fFont<>pValue) then
  begin
    fFont:=pValue;
    Repaint;
  end;
end;

procedure TMSVertical.SetMargin(pValue: Integer);
begin
  if (fMargin<>pValue) then
  begin
    fMargin:=pValue;
    Repaint;
  end;
end;

{ THorizontal }

procedure TMSHorizontal.Assign( Source: TPersistent);
begin
  if Source is TMSHorizontal then
  begin
    Self.Color:=TMSHorizontal(Source).Color;
    if Assigned(TMSHorizontal(Source).Font) then
      Self.Font.Assign(TMSHorizontal(Source).Font);
    Self.Margin := TMSHorizontal(Source).Margin;
  end
  else
    inherited;
end;

constructor TMSHorizontal.Create(AOwner: TMSStatistics);
begin
  inherited Create;
  fOwner:=AOwner;
  fFont:=TFont.Create;
  fFont.Name:='Tahoma';
  fFont.Color:=clWhite;
  fMargin:=21;
  fColor:=$008F75FB;
end;

destructor TMSHorizontal.Destroy;
begin
  fFont.Free;
  inherited;
end;

procedure TMSHorizontal.Repaint;
begin
  with FOwner do
   if not (csLoading in ComponentState) then
     Invalidate;
end;

procedure TMSHorizontal.SetCaption(pValue: TCaption);
begin
   fCaption := pValue;
   Repaint;
end;

procedure TMSHorizontal.SetColor(pValue: TColor);
begin
  if (fColor<>pValue) then
  begin
    fColor:=pValue;
    Repaint;
  end;
end;

procedure TMSHorizontal.SetFont(pValue: TFont);
begin
  if (fFont<>pValue) then
  begin
    fFont:=pValue;
    Repaint;
  end;
end;

procedure TMSHorizontal.SetMargin(pValue: Integer);
begin
  if (fMargin<>pValue) then
  begin
    fMargin:=pValue;
    Repaint;
  end;
end;

{ TMSSeriesNode }

function  TMSSeriesNode.ChangeLevel(pColor:TColor):TColor;
begin
  Result:=pColor;
  if fFlash then
  begin
    if fFlashLevel<>0 then
      fFlashColor:=ChangeBrightness(pColor,fFlashLevel*7)
    else
      fFlashColor:=pColor;
    fFlashLevel:=fFlashLevel+fFlashToggle;
    if (fFlashLevel < 0) then
    begin
      fFlashToggle:=1;
      fFlashLevel:=1;
    end;
    if (fFlashLevel > 10) then
    begin
      fFlashToggle:=-1;
      fFlashLevel:=9;
    end;
    Result:=fFlashColor;
  end;
end;

constructor TMSSeriesNode.Create;
begin
  inherited;
  fFlashLevel:=0;
  fTimeMousePause:=0;
  fFlashToggle:=1;
  fTimeMouseLock:=false;
end;

function TMSSeriesNode.ExistMouse: boolean;
begin
  if fTimeMouseLock then
  begin
    result:=false;
  end else begin
    if fTimeMousePause=0 then
    begin
      fTimeMousePause:=GetTickCount()+100;
      result:=false;
    end else begin
      result:=fTimeMousePause < GetTickCount();
      if result then
        fTimeMouseLock:=true;
    end;
  end;
end;

procedure TMSSeriesNode.ResetMouse;
begin
  fTimeMousePause:=0;
  fTimeMouseLock:=false;
end;



end.
