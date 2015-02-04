unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, types, l4l_print;

type

  { TFormPreview }

  TFormPreview = class(TForm)
    ButtonPrint: TButton;
    ComboZoom: TComboBox;
    Image: TImage;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel1: TPanel;
    PanelPage: TPanel;
    PMenu: TPopupMenu;
    ScrollBar: TScrollBar;
    ScrollBox: TScrollBox;
    procedure ButtonPrintClick(Sender: TObject);
    procedure ComboZoomKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure ComboZoomSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; {%H-}Shift: TShiftState;
      {%H-}MousePos: TPoint; {%H-}var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; {%H-}Shift: TShiftState;
      {%H-}MousePos: TPoint; {%H-}var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure ScrollBoxPaint(Sender: TObject);
  private
    { private declarations }
    Zoom: integer;
    DragPoint: TPoint;
    DragImage, MovePage: boolean;
    procedure DoSetZoomText({%H-}Data: PtrInt);
    procedure SetPageInfo;
  public
    { public declarations }
    LP: TLuaPrint;
  end;

var
  FormPreview: TFormPreview;

implementation
uses
  Printers, LCLType, LCLIntf;

{$R *.lfm}

const
  FRAME_SIZE = 2;

{ TFormPreview }

procedure TFormPreview.FormCreate(Sender: TObject);
begin
  DragImage := False;
  MovePage := False;
  Image.OnMouseWheelDown:= @FormMouseWheelDown;
  Image.OnMouseWheelUp:= @FormMouseWheelUp;
  ScrollBox.OnMouseWheelDown:= @FormMouseWheelDown;
  ScrollBox.OnMouseWheelUp:= @FormMouseWheelUp;
  ScrollBox.DoubleBuffered := True;
  ScrollBox.Align:=alClient;
  ButtonPrint.Hint := 'Print with ' + Printer.Printers[Printer.PrinterIndex];
end;

procedure TFormPreview.FormShow(Sender: TObject);
var
  i: integer;
begin
  i:= LP.PageCount;
  if i > 0 then begin
    ScrollBar.SetParams(1, 1, i);
    ScrollBar.Enabled:= i > 1;
    SetPageInfo;
  end else begin
    Panel1.Visible:= False;
    Image.Visible:= False;
  end;

  ComboZoom.ItemIndex:= 11;
  ComboZoomSelect(nil);
  if Zoom < 80 then begin
    ComboZoom.ItemIndex:= 10;
    ComboZoomSelect(nil);
    if Zoom > 100 then begin
      ComboZoom.ItemIndex:= 5;
      ComboZoomSelect(nil);
    end;
  end;
end;

procedure TFormPreview.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ((Button = mbLeft) or (Button = mbMiddle)) and
   ((ScrollBox.ClientWidth < Image.Width) or
   (ScrollBox.ClientHeight < Image.Height)) then begin
    GetCursorPos(DragPoint);
    Mouse.Capture := Self.Handle;
    DragImage := True;
    Screen.Cursor:= crSize;
  end else if (Button = mbRight) then begin
    GetCursorPos(DragPoint);
    MovePage := True;
    Mouse.Capture := Self.Handle;
    Screen.Cursor:= crHandPoint;
  end;
end;

procedure TFormPreview.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  p: TPoint;
begin
  if DragImage then begin
    GetCursorPos(p{%H-});
    if p.X > DragPoint.X then begin
      ScrollBox.HorzScrollBar.Position:=
       ScrollBox.HorzScrollBar.Position - (p.X - Dragpoint.X);
    end;
    if p.X < DragPoint.X then begin
      ScrollBox.HorzScrollBar.Position:=
       ScrollBox.HorzScrollBar.Position + (Dragpoint.X - p.X);
    end;
    if p.Y > DragPoint.Y then begin
      ScrollBox.VertScrollBar.Position:=
       ScrollBox.VertScrollBar.Position - (p.Y - Dragpoint.Y);
    end;
    if p.Y < DragPoint.Y then begin
      ScrollBox.VertScrollBar.Position:=
       ScrollBox.VertScrollBar.Position + (Dragpoint.Y - p.Y);
    end;
    DragPoint:= p;
  end else if MovePage then begin
    GetCursorPos(p);
    if p.X > DragPoint.X+30 then begin
      ScrollBar.Position := ScrollBar.Position + 1;
      DragPoint:= p;
    end else if p.X < DragPoint.X-30 then begin
      ScrollBar.Position := ScrollBar.Position - 1;
      DragPoint:= p;
    end;
  end;
end;

procedure TFormPreview.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if DragImage or MovePage then begin
    ReleaseCapture;
    Screen.Cursor:= crDefault;
    DragImage:= False;
    MovePage:= False;
  end;
end;

procedure TFormPreview.ComboZoomSelect(Sender: TObject);
var
  i, w, h, l, t, dpi, pw, ph: integer;
  aspect: double;
  s: string;
begin
  ScrollBox.HorzScrollBar.Position:= 0;
  ScrollBox.VertScrollBar.Position:= 0;
  dpi:= Font.PixelsPerInch;
  pw:= LP.PaperSize.cx;
  ph:= LP.PaperSize.cy;
  with Image do begin
    case ComboZoom.ItemIndex of
      0: Zoom:= 500;
      1: Zoom:= 400;
      2: Zoom:= 300;
      3: Zoom:= 200;
      4: Zoom:= 150;
      5: Zoom:= 100;
      6: Zoom:= 75;
      7: Zoom:= 50;
      8: Zoom:= 25;
      9: Zoom:= 10;
      10: begin
        // Page width(ページ幅を基準に)
        Image.SetBounds(0, 0, 1, ScrollBox.ClientHeight+100);
        Zoom:= ScrollBox.ClientWidth * LP.dpi * 100 div (pw * dpi) - 1;
      end;
      11: begin
        // Whole page(ページ全体を表示)
        Image.SetBounds(0, 0, 0, 0);
        aspect:= LP.PageSize.cx / LP.PageSize.cy;
        w:= ScrollBox.ClientWidth;
        h:= Trunc(w / aspect);
        Zoom:= w * LP.dpi * 100 div (pw * dpi) - 1;
        if h > ScrollBox.ClientHeight then begin
          h:= ScrollBox.ClientHeight;
          Zoom:= h * LP.dpi * 100 div (ph * dpi) - 1;
        end;
      end;
      else begin
        s:= ComboZoom.Text;
        if (s <> '') and (s[Length(s)] = '%') then s:= Copy(s, 1, Length(s)-1);
        i:= StrToIntDef(s, 0);
        if (i >= 10) and (i <= 1000) then Zoom:= i;
      end;
    end;

    w:= pw * dpi * Zoom div (LP.dpi * 100);
    h:= ph * dpi * Zoom div (LP.dpi * 100);
    l:= 0;
    if w < ScrollBox.ClientWidth then l:= (ScrollBox.ClientWidth - w) div 2;
    t:= 0;
    if h < ScrollBox.ClientHeight then t:= (ScrollBox.ClientHeight - h) div 2;

    Image.SetBounds(l, t, w, h);
    Image.Picture.Bitmap.SetSize(w, h);

    Image.Canvas.Brush.Style:= bsSolid;
    Image.Canvas.Brush.Color:= clWhite;
    Image.Canvas.FillRect(0, 0, w, h);
    LP.Play(ScrollBar.Position, Image.Canvas, dpi, Zoom);
    ScrollBox.Invalidate;
  end;

  Application.QueueAsyncCall(@DoSetZoomText, 0);
end;

procedure TFormPreview.ButtonPrintClick(Sender: TObject);
var
  p: TPoint;
begin
  p:= Panel1.ClientToScreen(
   Point(ButtonPrint.Left, ButtonPrint.Top+ButtonPrint.Height));
  PMenu.Popup(p.X, p.Y);
end;

procedure TFormPreview.ComboZoomKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    ComboZoomSelect(Sender);
    Key:= 0;
  end;
end;

procedure TFormPreview.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Zoom >= 1000 then Exit;
  ComboZoom.Text:= IntToStr(Zoom + 1);
  ComboZoomSelect(Sender);
end;

procedure TFormPreview.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Zoom <= 10 then Exit;
  ComboZoom.Text:= IntToStr(Zoom - 1);
  ComboZoomSelect(Sender);
end;

procedure TFormPreview.MenuItem1Click(Sender: TObject);
begin
  // Print all
  LP.Print;
end;

procedure TFormPreview.MenuItem2Click(Sender: TObject);
begin
  // Print current page
  LP.Print(ScrollBar.Position, ScrollBar.Position);
end;

procedure TFormPreview.ScrollBarChange(Sender: TObject);
begin
  SetPageInfo;
  ComboZoomSelect(nil);
end;

procedure TFormPreview.ScrollBoxPaint(Sender: TObject);
var
  r: TRect;
begin
  inherited;
  ScrollBox.Canvas.Pen.Style:= psSolid;
  ScrollBox.Canvas.Pen.Width:= FRAME_SIZE;
  ScrollBox.Canvas.Pen.Color:= clGray;
  ScrollBox.Canvas.Pen.JoinStyle := pjsMiter;
  ScrollBox.Canvas.Brush.Style:= bsClear;
  ScrollBox.Canvas.Brush.Color:= clWhite;
  r:= Image.BoundsRect;
  InflateRect(r, FRAME_SIZE, FRAME_SIZE);
  ScrollBox.Canvas.Rectangle(r);
end;

procedure TFormPreview.DoSetZoomText(Data: PtrInt);
begin
  ComboZoom.Text:= IntToStr(Zoom) + '%';
  ComboZoom.SelectAll;
end;

procedure TFormPreview.SetPageInfo;
var
  s: string;
begin
  s:= Format('Page %2d/%2d', [ScrollBar.Position, LP.PageCount]);
  PanelPage.Caption:= s;
  ScrollBar.Hint:= s;
end;

end.

