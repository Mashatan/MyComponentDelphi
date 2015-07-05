{
   Author:   Ali Mashatan
   E-mail:  ali.mashatan@gmail.com
   website: https://github.com/Mashatan
   Date:     2008-02-28
   License:  GNU Public License ( GPL3 ) [http://www.gnu.org/licenses/gpl-3.0.txt]
}

unit MSHints;
{.$define OldVersion}
interface
uses Windows, classes, Graphics, Controls, ImgList, Messages,
     SyncObjs, SysUtils, Types;
type
  TMSCustomHint = class;
  TMSCustomHintWindow = class(TCustomControl)
  private
    FHintParent: TMSCustomHint;
    FPopAbove: Boolean;
    FTitle: string;
    FDescription: string;
    FImageIndex: TImageIndex;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure NCPaint(DC: HDC); virtual;
    procedure Paint; override;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    function IsThemed: Boolean;
    function NewStylePainting: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AutoSize;
    procedure PositionAt(Point: TPoint); overload;
    procedure PositionAt(Rect: TRect); overload;
    procedure PositionAtCursor;
    property  HintParent: TMSCustomHint read FHintParent write FHintParent;
    property  PopAbove: Boolean read FPopAbove;
    property  Title: string read FTitle;
    property  Description: string read FDescription;
    property  ImageIndex: TImageIndex read FImageIndex;
  end;

  TMSCustomHintShowHideThread = class(TThread)
  private
    FHintWindowQueue: TThreadList;
    FHintObject: TMSCustomHint;
    FHideHint: Boolean;
    FActive: Boolean;
    FDisplayTime: Cardinal;
    FWaitEvent: TEvent;
    procedure QueHintWindow(Value: TMSCustomHintWindow);
  public
    constructor Create(Hint: TMSCustomHintWindow; HintObject: TMSCustomHint); overload;
    destructor Destroy; override;
    procedure ResumeWork;
    procedure Execute; override;
    procedure HideHint;
  end;

  TMSCustomHint = class(TControl)
  private
    FTitle: string;
    FDescription: string;
    FImages: TImageList;
    FImageIndex: TImageIndex;
    FStyle: TBalloonHintStyle;
    FAnimateThread: TMSCustomHintShowHideThread;
    FShowDelay: Cardinal;
    FShow: Boolean;
    FHideAfter: Integer;
    FLatestHintControl: TControl;
    FWorkComplete: Boolean;
    FBiDiMode:TBiDiMode;
  protected
    property WorkComplete: Boolean read FWorkComplete;
    procedure ShowAnotherHint;
    procedure SetImages(Value: TImageList);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowHint; overload;
    procedure ShowHint(Point: TPoint); overload;
    procedure ShowHint(Rect: TRect); overload;
    procedure ShowHint(Control: TControl); overload;
    procedure HideHint; overload;
    procedure HideHint(HidingControl: TControl); overload;
    procedure PaintHint(HintWindow: TMSCustomHintWindow); virtual;
    procedure NCPaintHint(HintWindow: TMSCustomHintWindow; DC: HDC); virtual;
    procedure SetHintSize(HintWindow: TMSCustomHintWindow); virtual;
    property ShowingHint: Boolean read FShow;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex;
  published
    property BiDiMode:TBiDiMode read FBiDiMode write FBiDiMode;
    property Images: TImageList read FImages write SetImages;
    property Style: TBalloonHintStyle read FStyle write FStyle default bhsBalloon;
    property Delay: Cardinal read FShowDelay write FShowDelay default 500;
    property HideAfter: Integer read FHideAfter write FHideAfter default -1;
  end;

  TMSBalloonHint = class(TMSCustomHint)
  Private
    fColor:TColor;
    fRed,fGreen,fBlue:byte;
    procedure SetColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure PaintHint(HintWindow: TMSCustomHintWindow); override;
    procedure SetHintSize(HintWindow: TMSCustomHintWindow); override;
    property Color:TColor read fColor write SetColor;
  end;

implementation
uses Consts, Forms, ActiveX, Math, Themes, UxTheme, DwmApi;


{ TBalloonHintWindow }

procedure TMSCustomHintWindow.AutoSize;
begin
  HintParent.SetHintSize(Self);
end;

procedure TMSCustomHintWindow.CMTextChanged(var Message: TMessage);
begin
  inherited;
end;

constructor TMSCustomHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := False;
  Color := $80FFFF;
  Canvas.Font := Screen.HintFont;
  Canvas.Brush.Style := bsClear;
  Width := 0;
  Height := 0;
end;

procedure TMSCustomHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  end;
end;

procedure TMSCustomHintWindow.CreateWnd;
begin
  inherited;
{$ifdef OldVersion}
  if ThemeServices.ThemesEnabled then
{$endif OldVersion}
  begin
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
    SetLayeredWindowAttributes(Handle, $0000FF00, 0, LWA_COLORKEY or LWA_ALPHA);
  end;
end;

function TMSCustomHintWindow.IsThemed: Boolean;
begin
  Result := ThemeServices.ThemesEnabled and CheckWin32Version(6);
end;

function TMSCustomHintWindow.NewStylePainting: Boolean;
begin
  Result := CheckWin32Version(5, 1) and ThemeServices.ThemesEnabled;
end;

procedure TMSCustomHintWindow.NCPaint(DC: HDC);
begin
  HintParent.NCPaintHint(Self, DC);
end;

procedure TMSCustomHintWindow.Paint;
begin
  HintParent.PaintHint(Self);
end;

procedure TMSCustomHintWindow.PositionAtCursor;
var
  Pos: TPoint;
begin
  GetCursorPos(Pos);
  PositionAt(Pos);
end;

procedure TMSCustomHintWindow.PositionAt(Rect: TRect);
begin
  AutoSize;

  Top := Rect.Bottom;
  Left := Rect.Left + RectWidth(Rect) div 2 - (Width) div 2;

  if HintParent.Style = bhsBalloon then
    Left := Left + cBalloonStemHeight;

  FPopAbove := true;//Top > Screen.Height div 2;
  if FPopAbove then
    Top := Top - Height - RectHeight(Rect);
end;

procedure TMSCustomHintWindow.PositionAt(Point: TPoint);
begin
  PositionAt(Rect(Point.X, Point.Y, Point.X, Point.Y));
end;

procedure TMSCustomHintWindow.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTTRANSPARENT;
end;

procedure TMSCustomHintWindow.WMNCPaint(var Message: TWMNCPaint);
var
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    NCPaint(DC);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TMSCustomHintWindow.WMPrint(var Message: TMessage);
begin
  PaintTo(Message.WParam, 0, 0);
  NCPaint(Message.WParam);
end;

{ TBalloonHint }

procedure TMSCustomHint.SetHintSize(HintWindow: TMSCustomHintWindow);
var
  lWidth, lHeight: Integer;
  TextWidth, TextHeight: Integer;
  MeasureRect: TRect;
  WasBold: Boolean;
begin
  with HintWindow do
  begin
    lWidth := 0;
    lHeight := 0;
    TextWidth := 0;
    TextHeight := 0;

    with HintParent do
    begin
      if Title <> '' then
      begin
        WasBold := fsBold in Canvas.Font.Style;
        Canvas.Font.Style := Canvas.Font.Style + [fsBold];
//        Canvas.TextRect(MeasureRect, FTitle, [tfRight, tfCalcRect]);
        if not WasBold then
          Canvas.Font.Style := Canvas.Font.Style - [fsBold];
        TextWidth := RectWidth(MeasureRect) + cTextHorizontalMargin * 2;
        TextHeight := TextHeight + RectHeight(MeasureRect) + cTextVerticalMargin;
      end;

      if Description <> '' then
      begin
//        Canvas.TextRect(MeasureRect, FDescription, [tfRight, tfCalcRect]);
        TextWidth := Max(TextWidth, RectWidth(MeasureRect) + cTextHorizontalMargin * 2);
        TextHeight := TextHeight + RectHeight(MeasureRect) + cTextVerticalMargin;
      end;

      lHeight := lHeight + TextHeight;
      lWidth := lWidth + TextWidth;
    end;

    HintWindow.Height := lHeight;
    HintWindow.Width := lWidth;
  end;
end;

procedure TMSCustomHint.SetImages(Value: TImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    if Images <> nil then
      Images.FreeNotification(Self);
  end;
end;

constructor TMSCustomHint.Create(AOwner: TComponent);
begin
  inherited;

  FBiDiMode:=bdLeftToRight;
  FHideAfter := -1;
  FStyle := bhsBalloon;
  FAnimateThread := nil;
  FShowDelay := 500;
  FShow := True;
end;

destructor TMSCustomHint.Destroy;
begin
  HideHint;
  FWorkComplete := True;
  if FAnimateThread <> nil then
  begin
    FAnimateThread.ResumeWork;
    FAnimateThread.Free;
    FAnimateThread := nil;
  end;
  inherited;
end;

procedure TMSCustomHint.HideHint(HidingControl: TControl);
begin
  if FLatestHintControl = HidingControl then
    HideHint;
end;

procedure TMSCustomHint.HideHint;
begin
  FShow := False;
end;

procedure TMSCustomHint.NCPaintHint(HintWindow: TMSCustomHintWindow; DC: HDC);
begin

end;

procedure TMSCustomHint.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;
  end;
end;

procedure TMSCustomHint.PaintHint(HintWindow: TMSCustomHintWindow);
var
  CRect, TopText, BottomText: TRect;
  Region, OldRegion: HRGN;
  TextSize: TRect;
begin
  with HintWindow do
  begin
      CRect := SplitRect(ClientRect, srBottom, Height);

      Canvas.Brush.Color := $B0FFFF;
      Canvas.FillRect(ClientRect);

      Region := CreateRectRgn(CRect.Left, CRect.Top, CRect.Right, CRect.Bottom);

      OldRegion := SelectObject(Canvas.Handle, Region);

      Canvas.Brush.Color := clBlack;
      FrameRgn(Canvas.Handle, Region, Canvas.Brush.Handle, 1, 1);

      if FDescription <> '' then
        TopText := SplitRect(CRect, srTop, 0.50)
      else
        TopText := CRect;

      if FTitle <> '' then
        BottomText := SplitRect(CRect, srBottom, 0.50)
      else
        BottomText := CRect;

      Canvas.Brush.Style := bsClear;

//      Canvas.TextRect(TextSize, FTitle, [tfCalcRect]);
      TopText := CenteredRect(TopText, TextSize);
//      Canvas.TextRect(TopText, FTitle, [tfLeft, tfTop, tfWordBreak, tfNoClip]);

//      Canvas.TextRect(TextSize, FDescription, [tfCalcRect]);
      BottomText := CenteredRect(BottomText, TextSize);
//      Canvas.TextRect(BottomText, FDescription, [tfLeft,  tfTop, tfWordBreak, tfNoClip]);

      SelectObject(Canvas.Handle, OldRegion);

      DeleteObject(Region);
    end;
end;

procedure TMSCustomHint.ShowAnotherHint;
begin
  FShow := True;
end;

procedure TMSCustomHint.ShowHint(Control: TControl);
var
  Pos: TPoint;
  Index: Integer;
begin
  if Control.Hint = '' then
    Exit;

  Index := AnsiPos('|', Control.Hint); //Do Not Localize

  Title := GetShortHint(Control.Hint);
  if Index <> 0 then
    Description := GetLongHint(Control.Hint)
  else
    Description := '';

  Index := AnsiPos('|', Description); //Do Not Localize
  if Index <> 0 then
  begin
    ImageIndex := StrToInt(Copy(Description, Index + 1, MaxInt));
    Description := Copy(Description, 0, Index - 1);
  end
  else
    ImageIndex := -1;

  FLatestHintControl := Control;

  GetCursorPos(Pos);
  ShowHint(Pos);
end;

procedure TMSCustomHint.ShowHint(Rect: TRect);
var
  Hint: TMSCustomHintWindow;
begin
  FShow := True;
  Hint := TMSCustomHintWindow.Create(nil);
  Hint.BiDiMode:=fBiDiMode;
  Hint.HintParent := Self;
  Hint.HandleNeeded;
  Hint.FTitle := Title;
  Hint.FDescription := Description;
  Hint.FImageIndex := ImageIndex;
  Hint.PositionAt(Rect);
  FWorkComplete := False;
  if FAnimateThread = nil then
  begin
    FAnimateThread := TMSCustomHintShowHideThread.Create(Hint, Self);
  end
  else
  begin
    FAnimateThread.QueHintWindow(Hint);
    FAnimateThread.ResumeWork;
  end;
end;

procedure TMSCustomHint.ShowHint(Point: TPoint);
begin
  ShowHint(Rect(Point.X, Point.Y, Point.X, Point.Y));
end;

procedure TMSCustomHint.ShowHint;
begin
  ShowHint(Point(0, 0));
end;

{ TBalloonHintShowHideThread }

constructor TMSCustomHintShowHideThread.Create(Hint: TMSCustomHintWindow; HintObject: TMSCustomHint);
begin
  inherited Create(False);
  FWaitEvent := TEvent.Create;{(nil, False, False, 'BalloonHintWaitEvent');}
  FHintWindowQueue := TThreadList.Create;
  FHintWindowQueue.Duplicates := dupAccept;
  QueHintWindow(Hint);
  FHideHint := False;
  FActive := True;
  FHintObject := HintObject;
end;

destructor TMSCustomHintShowHideThread.Destroy;
var
  I: Integer;
begin
  FActive := False;
  ResumeWork;

  inherited;

  with FHintWindowQueue.LockList do
  try
    for I := 0 to Count - 1 do
      TObject(Items[I]).Free;
  finally
    FHintwindowQueue.UnlockList;
  end;
  FHintWindowQueue.Clear;
  FHintWindowQueue.Free;
  FWaitEvent.Free;
end;

procedure TMSCustomHintShowHideThread.Execute;
const
  cFadeFrames = 10;
  cFadeMSPF = 20;
var
  I: Integer;
  LHintParent: TMSCustomHint;
  LHintWindow: TMSCustomHintWindow;
  ShowAnotherWindow: Boolean;
  FirstShow: Boolean;
  listCount: Integer;

  function LatestHintWindow: TMSCustomHintWindow;
  var
    I: Integer;
    lCount: Integer;
    FreeList: TList;
  begin
    Result := nil;
    FreeList := TList.Create;
    try
      with FHintWindowQueue.LockList do
      try
        lCount := Count;
        for I := 0 to lCount - 1 do
        begin
          if I = lCount - 1 then
            Result := TMSCustomHintWindow(Items[0])
          else
          begin
            FreeList.Add(Items[0]);
            Delete(0);
          end;
        end;
      finally
        FHintWindowQueue.UnlockList;
      end;

      for I := 0 to FreeList.Count - 1 do
        Synchronize(TMSCustomHintWindow(FreeList[I]).Free);
    finally
      FreeList.Free;
    end;
  end;

begin
  while FActive do
  begin
    FirstShow := True;
    LHintWindow := LatestHintWindow;
    LHintParent := FHintObject;
    if LHintWindow <> nil then
    begin
      with LHintWindow do
      begin
        try
          if HandleAllocated then
            SetLayeredWindowAttributes(Handle, $0000FF00, 0, LWA_ALPHA or LWA_COLORKEY);
          Sleep(HintParent.Delay);

          if HintParent.ShowingHint then
          begin
            while True do
            begin
              ShowAnotherWindow := False;

              if HandleAllocated then
              begin
                Synchronize(PositionAtCursor);
                Synchronize(Paint);

                ShowWindow(Handle, SW_SHOWNOACTIVATE);
              end;

              if FirstShow then
              begin
               FirstShow := False;
               for I := 1 to cFadeFrames do
                begin
                  if HandleAllocated then
                    SetLayeredWindowAttributes(Handle, $0000FF00, Trunc((I / cFadeFrames) * 255), LWA_ALPHA or LWA_COLORKEY);

                  with FHintWindowQueue.Locklist do
                  try
                    listCount := Count;
                  finally
                    FHintWindowQueue.UnlockList;
                  end;

                  if listCount > 1 then
                  begin
                    LHintWindow := LatestHintWindow;
                    ShowAnotherWindow := True;
                    LHintParent.ShowAnotherHint;
                    Break;
                  end;

                  Sleep(cFadeMSPF);
                end;
              end;

              if ShowAnotherWindow then
                Continue;

              FDisplayTime := GetTickCount;

              if HandleAllocated then
                SetLayeredWindowAttributes(Handle, $0000FF00, 255, LWA_ALPHA or LWA_COLORKEY);

              while HintParent.ShowingHint do
              begin
                Sleep(cFadeMSPF);

                with FHintWindowQueue.Locklist do
                try
                  listCount := Count;
                finally
                  FHintWindowQueue.UnlockList;
                end;

                if listCount > 1 then
                begin
                  LHintWindow := LatestHintWindow;
                  ShowAnotherWindow := True;
                  LHintParent.ShowAnotherHint;
                  Break;
                end;

                if HintParent.HideAfter <> -1 then
                begin
                  if Cardinal(HintParent.HideAfter) < GetTickCount - FDisplayTime then
                    HintParent.HideHint;
                end
              end;

              if ShowAnotherWindow then
                Continue;

              for I := cFadeFrames downto 0 do
              begin
                if HandleAllocated then
                  SetLayeredWindowAttributes(Handle, $0000FF00, Trunc((I / cFadeFrames) * 255), LWA_ALPHA or LWA_COLORKEY);

                with FHintWindowQueue.Locklist do
                try
                  listCount := Count;
                finally
                  FHintWindowQueue.UnlockList;
                end;

                if listCount > 1 then
                begin
                  LHintWindow := LatestHintWindow;
                  ShowAnotherWindow := True;
                  LHintParent.ShowAnotherHint;
                  Break;
                end;

                Sleep(cFadeMSPF);
              end;

              if ShowAnotherWindow then
                Continue;

              with FHintWindowQueue.Locklist do
              try
                listCount := Count;
              finally
                FHintWindowQueue.UnlockList;
              end;

              if listCount = 1 then
                Break;
            end;
          end;

        finally
          Synchronize(Free);
          with FHintWindowQueue.Locklist do
          try
            Delete(0);
          finally
            FHintWindowQueue.UnlockList;
          end;
        end;
      end;
    end;

    if (not LHintParent.FWorkComplete) and (not Application.Terminated) then
    begin
      FWaitEvent.ResetEvent;
      FWaitEvent.WaitFor(INFINITE);
    end;
  end;
end;

procedure TMSCustomHintShowHideThread.HideHint;
begin
  FHideHint := True;
end;

procedure TMSCustomHintShowHideThread.QueHintWindow(Value: TMSCustomHintWindow);
begin
  FHintWindowQueue.Add(Value);
end;

procedure TMSCustomHintShowHideThread.ResumeWork;
begin
  FWaitEvent.SetEvent;
end;

{ TBalloonHint }

constructor TMSBalloonHint.Create(AOwner: TComponent);
begin
  inherited;
  fRed:=34;
  fGreen:=220;
  fBlue:=34;
  fColor:=TColor(RGB(fRed,fGreen,fBlue));
end;

procedure TMSBalloonHint.SetColor(const Value: TColor);
begin
  fColor := Value;
  fRed:=GetRValue(Value);
  fGreen:=GetGValue(Value);
  fBlue:=GetBValue(Value);
end;

procedure TMSBalloonHint.SetHintSize(
  HintWindow: TMSCustomHintWindow);
var
  lWidth, lHeight: Integer;
  ImageHeight: Integer;
  TextWidth, TextHeight: Integer;
  MeasureRect: TRect;
  Theme: HTHEME;
  WasBold: Boolean;
begin
  with HintWindow do
  begin
    if not NewStylePainting then
    begin
      inherited SetHintSize(HintWindow);
{$ifdef OldVersion}
      //Exit;
{$endif OldVersion}
    end;

    lWidth := 0;
    TextWidth := 0;
    TextHeight := 0;
    ImageHeight := 0;

    if HintParent.Style = bhsBalloon then
      lHeight := cBalloonStemHeight
    else
      lHeight := 0;

    Theme := ThemeServices.Theme[teToolTip];

    with HintParent do
    begin
      if (Images <> nil) and (ImageIndex <> -1) then
      begin
        lWidth := lWidth + Images.Width + cImageMargin;
        ImageHeight := Images.Height + cImageMargin;
      end;

      if Title <> '' then
      begin
        if IsThemed then
          GetThemeTextExtent(Theme, Canvas.Handle, TTP_STANDARDTITLE, TTSS_NORMAL, {$IFNDEF CLR}PWideChar{$ENDIF}(UnicodeString(Title)), -1, 0, {$IFNDEF CLR}nil{$ELSE}Rect(0, 0, 0, 0){$ENDIF}, MeasureRect)
        else
        begin
          WasBold := fsBold in Canvas.Font.Style;
          Canvas.Font.Style := Canvas.Font.Style + [fsBold];
          Canvas.TextRect(MeasureRect, FTitle, [tfLeft, tfCalcRect]);
          if not WasBold then
            Canvas.Font.Style := Canvas.Font.Style - [fsBold];
        end;
        TextWidth := RectWidth(MeasureRect) + cTextHorizontalMargin * 2;
        TextHeight := TextHeight + RectHeight(MeasureRect) + cTextVerticalMargin;
      end;

      if Description <> '' then
      begin
        if IsThemed then
          GetThemeTextExtent(Theme, Canvas.Handle, TTP_STANDARD, TTSS_NORMAL, {$IFNDEF CLR}PWideChar{$ENDIF}(UnicodeString(Description)), Length(Description), 0, {$IFNDEF CLR}nil{$ELSE}Rect(0, 0, 0, 0){$ENDIF}, MeasureRect)
        else
        begin
          Canvas.TextRect(MeasureRect, FDescription, [tfLeft, tfCalcRect]);
        end;
        TextWidth := Max(TextWidth, RectWidth(MeasureRect) + cTextHorizontalMargin * 2);
        TextHeight := TextHeight + RectHeight(MeasureRect) + cTextVerticalMargin;
      end;

      //Add some space for the non-themed painting since Canvas.TextRect returns
      //a slightly different result then the themed GetThemeTextExtent
      if not IsThemed then
      begin
        TextWidth := TextWidth + 4;
        TextHeight := TextHeight + 4;
      end;

      lHeight := lHeight + Max(ImageHeight, TextHeight);
      lWidth := lWidth + TextWidth;
    end;

    HintWindow.Height := lHeight;
    HintWindow.Width := lWidth;
  end;
end;

procedure TMSBalloonHint.PaintHint(HintWindow: TMSCustomHintWindow);
var
  Theme: HTHEME;
  CRect, TopText, BottomText, MeasureRect, ImageRect: TRect;
  Region, Bubble, Stem, OldRegion: HRGN;
  StemPts: array[0..2] of TPoint;
  FillPts: array[0..1] of TTriVertex;
  RectPts: array[0..0] of TGradientRect;
  Details: TThemedElementDetails;
  TextSize: TRect;
  FontWasBold: Boolean;
  OldFontColor: TColor;
  RTLTextFormat:TTextFormat;
begin
  Theme := ThemeServices.Theme[teToolTip];
  Stem := 0;

  with HintWindow do
  begin
    if not NewStylePainting then
    begin
      inherited PaintHint(HintWindow);
{$ifdef OldVersion}
      //Exit;
{$endif OldVersion}
    end;

    if HintParent.Style = bhsBalloon then
    begin
      if FPopAbove then
        CRect := SplitRect(ClientRect, srTop, Height - cBalloonStemHeight)
      else
        CRect := SplitRect(ClientRect, srBottom, Height - cBalloonStemHeight);
    end
    else
    begin
      CRect := SplitRect(ClientRect, srBottom, Height);
    end;

    Canvas.Brush.Color := clLime;
    Canvas.FillRect(ClientRect);

    if not IsThemed then
    begin
      Bubble := CreateRoundRectRgn(CRect.Left, CRect.Top, CRect.Right, CRect.Bottom, cEdgeRadius, cEdgeRadius);

      if HintParent.Style = bhsBalloon then
      begin
        if FPopAbove then
        begin
          MeasureRect := CenteredRect(SplitRect(ClientRect, srBottom, cBalloonStemHeight + 1), Rect(0, 0, cBalloonStemHeight, cBalloonStemHeight));
          StemPts[0] := MeasureRect.TopLeft;
          StemPts[1] := Point(MeasureRect.Right, MeasureRect.Top);
          StemPts[2] := Point(MeasureRect.Left, MeasureRect.Bottom);
          Stem := CreatePolygonRgn(StemPts, 3, WINDING);
        end
        else
        begin
          MeasureRect := CenteredRect(SplitRect(ClientRect, srTop, cBalloonStemHeight), Rect(0, 0, cBalloonStemHeight, cBalloonStemHeight));
          StemPts[0] := MeasureRect.TopLeft;
          StemPts[1] := MeasureRect.BottomRight;
          StemPts[2] := Point(MeasureRect.Left, MeasureRect.Bottom);
          Stem := CreatePolygonRgn(StemPts, 3, WINDING)
        end;

        Region := CreateRectRgn(0, 0, 1, 1);
        CombineRgn(Region, Bubble, Stem, RGN_OR);

        OldRegion := SelectObject(Canvas.Handle, Region);
      end
      else
      begin
        OldRegion := SelectObject(Canvas.Handle, Bubble);
        Region := Bubble;
      end;

      FillPts[0].X := 0;
      FillPts[0].Y := 0;
      FillPts[0].Red := MAXWORD;
      FillPts[0].Green := MAXWORD;
      FillPts[0].Blue := MAXWORD;
      FillPts[0].Alpha := MAXWORD;

      FillPts[1].X := ClientRect.Right;
      FillPts[1].Y := ClientRect.Bottom;
      FillPts[1].Red := fRed*255;
      FillPts[1].Green := fGreen*255;
      FillPts[1].Blue := fBlue*255;
      FillPts[1].Alpha := MAXWORD;

      RectPts[0].UpperLeft := 0;
      RectPts[0].LowerRight := 1;

      {$IFNDEF CLR}
      GradientFill(Canvas.Handle, @FillPts[0], 2, @RectPts[0], 1, GRADIENT_FILL_RECT_V);
      {$ELSE}
      GradientFill(Canvas.Handle, FillPts, 2, RectPts, 1, GRADIENT_FILL_RECT_V);
      {$ENDIF}

      Canvas.Brush.Color := $00767676;
      FrameRgn(Canvas.Handle, Region, Canvas.Brush.Handle, 1, 1);

      if (HintParent.Images <> nil) and (FImageIndex <> -1) then
      begin
        ImageRect := SplitRect(CRect, srLeft, HintParent.Images.Width + cImageMargin * 2);
        ImageRect := CenteredRect(ImageRect, Rect(0, 0, HintParent.Images.Width, HintParent.Images.Height));
        CRect := SplitRect(CRect, srRight, RectWidth(CRect) - (HintParent.Images.Width + cImageMargin));
      end;

      if (HintParent.Images <> nil) and (FImageIndex <> -1) then
        HintParent.Images.Draw(Canvas, ImageRect.Left, ImageRect.Top, HintParent.ImageIndex);

      if FDescription <> '' then
        TopText := SplitRect(CRect, srTop, 0.50)
      else
        TopText := CRect;

      if FTitle <> '' then
        BottomText := SplitRect(CRect, srBottom, 0.50)
      else
        BottomText := CRect;

      Canvas.Brush.Style := bsClear;

      FontWasBold := fsBold in Canvas.Font.Style;
      OldFontColor := Canvas.Font.Color;
      Canvas.Font.Color := $00575757;
      if (BiDiMode = bdLeftToRight) then
        RTLTextFormat := [tfLeft]
      else
        RTLTextFormat := [tfRight];
      Canvas.Font.Style := Canvas.Font.Style + [fsBold];
      Canvas.TextRect(TextSize, FTitle, [tfCalcRect]);
      TopText := CenteredRect(TopText, TextSize);
      Canvas.TextRect(TopText, FTitle, [tfTop]+RTLTextFormat);

      Canvas.Font.Style := Canvas.Font.Style - [fsBold];
      Canvas.TextRect(TextSize, FDescription, [tfCalcRect]);
      BottomText := CenteredRect(BottomText, TextSize);
      Canvas.TextRect(BottomText, FDescription, [tfWordBreak]+RTLTextFormat);

      if FontWasBold then
        Canvas.Font.Style := Canvas.Font.Style + [fsBold];
      Canvas.Font.Color := OldFontColor;

      SelectObject(Canvas.Handle, OldRegion);

      if HintParent.Style = bhsBalloon then
      begin
        DeleteObject(Stem);
        DeleteObject(Region);
      end;
      DeleteObject(Bubble);
    end
    else
    begin
      GetThemeBackgroundRegion(Theme, Canvas.Handle, TTP_STANDARD, TTSS_NORMAL, CRect, Region);
      Canvas.Brush.Color := clWhite;
      FillRgn(Canvas.Handle, Region, Canvas.Brush.Handle);
      DeleteObject(Region);

      Details := ThemeServices.GetElementDetails(tttStandardNormal);
      ThemeServices.DrawElement(Canvas.Handle, Details, CRect);

      if HintParent.Style = bhsBalloon then
      begin
        if FPopAbove then
        begin
          MeasureRect := SplitRect(ClientRect, srBottom, cBalloonStemHeight + 1);
          DrawThemeBackground(Theme, Canvas.Handle, TTP_BALLOONSTEM, TTBSS_POINTINGDOWNLEFTWALL, SplitRect(ClientRect, srBottom, cBalloonStemHeight + 11), {$IFNDEF CLR}@{$ENDIF}MeasureRect) //This +11 is a vista hack till I can find how to measure the Stem properly
        end
        else
        begin
          MeasureRect := SplitRect(ClientRect, srTop, cBalloonStemHeight + 1);
          DrawThemeBackground(Theme, Canvas.Handle, TTP_BALLOONSTEM, TTBSS_POINTINGUPLEFTWALL, SplitRect(ClientRect, srTop, cBalloonStemHeight + 1), {$IFNDEF CLR}@{$ENDIF}MeasureRect);
        end;
      end;

      if (HintParent.Images <> nil) and (FImageIndex <> -1) then
      begin
        ImageRect := SplitRect(CRect, srLeft, HintParent.Images.Width + cImageMargin);
        ImageRect := CenteredRect(ImageRect, Rect(0, 0, HintParent.Images.Width, HintParent.Images.Height));
        CRect := SplitRect(CRect, srRight, RectWidth(CRect) - (HintParent.Images.Width + cImageMargin));
      end;

      if FDescription <> '' then
        TopText := SplitRect(CRect, srTop, 0.50)
      else
        TopText := CRect;

      if FTitle <> '' then
        BottomText := SplitRect(CRect, srBottom, 0.50)
      else
        BottomText := CRect;

      GetThemeTextExtent(Theme, Canvas.Handle, TTP_STANDARDTITLE, TTSS_NORMAL, {$IFNDEF CLR}PWideChar{$ENDIF}(UnicodeString(FTitle)), -1, 0, {$IFNDEF CLR}nil{$ELSE}Rect(0, 0, 0, 0){$ENDIF}, MeasureRect);
      TopText := CenteredRect(TopText, MeasureRect);
      TopText.Left := CRect.Left + cTextHorizontalMargin;
      TopText.Right := CRect.Right - cTextHorizontalMargin;

      GetThemeTextExtent(Theme, Canvas.Handle, TTP_STANDARD, TTSS_NORMAL, {$IFNDEF CLR}PWideChar{$ENDIF}(UnicodeString(FDescription)), -1, 0, {$IFNDEF CLR}nil{$ELSE}Rect(0, 0, 0, 0){$ENDIF}, MeasureRect);
      BottomText := CenteredRect(BottomText, MeasureRect);
      BottomText.Left := CRect.Left + cTextHorizontalMargin;
      BottomText.Right := CRect.Right - cTextHorizontalMargin;

      Details := ThemeServices.GetElementDetails(tttStandardTitleNormal);
      ThemeServices.DrawText(Canvas.Handle, Details, FTitle, TopText, 0, 0);
      Details := ThemeServices.GetElementDetails(tttStandardNormal);
      ThemeServices.DrawText(Canvas.Handle, Details, FDescription, BottomText, 0, 0);

      if (HintParent.Images <> nil) and (FImageIndex <> -1) then
        HintParent.Images.Draw(Canvas, ImageRect.Left, ImageRect.Top, FImageIndex);
    end;
  end;
end;
end.
