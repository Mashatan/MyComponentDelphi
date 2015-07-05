object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MSStatistics1: TMSStatistics
    Left = 88
    Top = 40
    Width = 400
    Height = 200
    Series = <
      item
        Color = clGreen
        Style = psSolid
        Caption = 'Test1'
        Tag = 0
      end
      item
        Color = clRed
        Style = psSolid
        Caption = 'Test2'
        Tag = 0
      end>
    Appearance.GridColor = 10737404
    Appearance.BGGridColor = 15202814
    Appearance.FrameColor = 2841516
    Appearance.HintColor = 13494014
    Vertical.Color = 9401851
    Vertical.Font.Charset = DEFAULT_CHARSET
    Vertical.Font.Color = clWhite
    Vertical.Font.Height = -11
    Vertical.Font.Name = 'Tahoma'
    Vertical.Font.Style = []
    Vertical.Caption = 'Vertical'
    Vertical.Margin = 21
    Horizontal.Color = 9401851
    Horizontal.Font.Charset = DEFAULT_CHARSET
    Horizontal.Font.Color = clWhite
    Horizontal.Font.Height = -11
    Horizontal.Font.Name = 'Tahoma'
    Horizontal.Font.Style = []
    Horizontal.Caption = 'Horizontal'
    Horizontal.Margin = 21
  end
end
