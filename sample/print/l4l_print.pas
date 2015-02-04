unit l4l_print;
{
  Lua4Lazarus

    sample:

    License: New BSD
      Copyright(c)2010- Malcome@Japan All rights reserved.

  ToDo:
}

{$mode objfpc}{$H+}

{.$DEFINE USE_AGG}

interface

uses
  Classes, SysUtils, contnrs, graphics, printers, types,
  lua53, l4l_object;

type

  { TLuaPrint }

  TLuaPrint = class(TObject)
  private
    FCanvasStack: TObjectList;
    function GetPageCount: integer;
    function GetPageSize: TSize;
    function GetPaperSize: TSize;
    procedure CopyCanvas(src, dst: TCanvas);
  protected
    LS: Plua_State;
    FPageList: TObjectList;
    FBmpList: TObjectList;
    FResList: TObjectList;
    FUserMargin, FRealMargin: TRect;
    FPaperRect: TPaperRect;
    FDPI, FPlayDPI: integer;
    FCanvas: TCanvas;
    FOffset: TPoint;
    FZoom: integer;
    function DP2LP(dp: integer): integer;
    function LP2DP(lp: integer): integer;
    function z(i: integer): integer;
  public
    constructor Create(L : Plua_State);
    destructor Destroy; override;
    procedure BeginDoc;
    procedure BeginDoc(Margin: TRect);
    procedure Run(const SourceCode: string);
    procedure EndDoc;
    procedure NewPage;
    procedure Play(pageNumber: integer; Cv: TCanvas;
     dpi: integer = 0; Zoom: integer = 100);
    procedure Play(pageNumber: integer; Cv: TCanvas; Margin: TRect;
     dpi, Zoom: integer);
    procedure Print(beginPage: integer=0; endPage: integer=0);
    property PageCount: integer read GetPageCount;
    property PaperSize: TSize read GetPaperSize;
    property PageSize: TSize read GetPageSize;
    property DPI: integer read FDPI;
    procedure AddOrder(const s: string);
    property Canvas: TCanvas read FCanvas;
    procedure PushCanvas;
    procedure PopCanvas;
  end;

  { TLuaPrintObject }

  TLuaFontObject = class;
  TLuaPenObject = class;
  TLuaBrushObject = class;

  TLuaPrintObject  = class(TLuaObject)
  private
    FUnits: char;
    function GetBrushObject: TLuaBrushObject;
    function GetFontObject: TLuaFontObject;
    function GetPageHeight: integer;
    function GetPageLeft: integer;
    function GetPageNumber: integer;
    function GetPageTop: integer;
    function GetPageWidth: integer;
    function GetPenObject: TLuaPenObject;
  protected
  public
    LuaPrint: TLuaPrint;
    function DP2LP(dp: integer): integer;
    function LP2DP(lp: integer): integer;
    constructor Create(L : Plua_State; lp: TLuaPrint); overload;
    destructor Destroy; override;
    procedure GetFontName(var FontName: string);
  published
    function l4l_TextOut: integer;
    function l4l_Rectangle: integer;
    function l4l_Line: integer;
    function l4l_TextWidth: integer;
    function l4l_TextHeight: integer;
    function l4l_DrawImage: integer;
    function l4l_DrawPDF: integer;
    function l4l_NewPage: integer;
    function l4l_DP2LP: integer;
    function l4l_LP2DP: integer;
    property l4l_pageWidth: integer read GetPageWidth;
    property l4l_pageHeight: integer read GetPageHeight;
    property l4l_pageNumber: integer read GetPageNumber;
    property l4l_pageLeft: integer read GetPageLeft;
    property l4l_pageTop: integer read GetPageTop;
    property l4l_units: char read FUnits write FUnits;
    property l4l_Font: TLuaFontObject read GetFontObject;
    property l4l_Pen: TLuaPenObject read GetPenObject;
    property l4l_Brush: TLuaBrushObject read GetBrushObject;
  end;

  { TLuaFontObject }

  TLuaFontObject  = class(TLuaObject)
  private
    function GetColor: string;
    function GetHeight: integer;
    function GetName: string;
    function GetOrientation: integer;
    function GetSize: integer;
    function GetStyle: string;
    procedure SetColor(const AValue: string);
    procedure SetHeight(const AValue: integer);
    procedure SetName(const AValue: string);
    procedure SetOrientation(AValue: integer);
    procedure SetSize(const AValue: integer);
    procedure SetStyle(const AValue: string);
  protected
    LPO: TLuaPrintObject;
  public
    constructor Create(L : Plua_State;  aLPO: TLuaPrintObject); overload;
    destructor Destroy; override;
  published
    property l4l_color: string read GetColor write SetColor;
    property l4l_Name: string read GetName write SetName;
    property l4l_Size: integer read GetSize write SetSize;
    property l4l_Height: integer read GetHeight write SetHeight; // Only "DP".
    property l4l_Style: string read GetStyle write SetStyle;
    property l4l_Orientation: integer read GetOrientation write SetOrientation;
  end;

  { TLuaPenObject }

  TLuaPenObject  = class(TLuaObject)
  private
    function GetColor: string;
    function GetMode: string;
    function GetStyle: string;
    function GetWidth: integer;
    procedure SetColor(const AValue: string);
    procedure SetMode(const AValue: string);
    procedure SetStyle(const AValue: string);
    procedure SetWidth(const AValue: integer);
  protected
    LPO: TLuaPrintObject;
  public
    constructor Create(L : Plua_State; aLPO: TLuaPrintObject); overload;
    destructor Destroy; override;
  published
    property l4l_color: string read GetColor write SetColor;
    property l4l_style: string read GetStyle write SetStyle;
    property l4l_mode: string read GetMode write SetMode;
    property l4l_width: integer read GetWidth write SetWidth;
  end;

  { TLuaBrushObject }

  TLuaBrushObject  = class(TLuaObject)
  private
    function GetColor: string;
    function GetStyle: string;
    procedure SetColor(const AValue: string);
    procedure SetStyle(const AValue: string);
  protected
    LPO: TLuaPrintObject;
  public
    constructor Create(L : Plua_State; aLPO: TLuaPrintObject); overload;
    destructor Destroy; override;
  published
    property l4l_color: string read GetColor write SetColor;
    property l4l_style: string read GetStyle write SetStyle;
  end;

implementation
uses
{$IFDEF WINDOWS}
  windows,
{$ENDIF}
{$IFDEF USE_AGG}
  agg_lcl, agg_fpimage, fpcanvas, {agg_color,}
{$ENDIF}
  LCLType, LCLIntf, typinfo, l4l_pdf, graphmath;

const
  MM_P_INCH = 2540;
  PRUN_NAME = 'P_';

type

{$IFDEF USE_AGG}
  TMyAggCanvas = class(TAggLCLCanvas)
  end;
{$ENDIF}

  { TLuaPrintRunObject }

  TLuaPrintRunObject  = class(TLuaObject)
  private
{$IFDEF USE_AGG}
    AggLCLCanvas: TMyAggCanvas;
    rx1, ry1, rx2, ry2: integer;
    procedure poly_sub(flag: TAggDrawPathFlag; w: boolean);
{$ELSE}
    PPP: array of TPoint;
    PPC: array of integer;
{$ENDIF}
  protected
    LuaPrint: TLuaPrint;
    function w(i: integer): integer;
    function zx(i: integer): integer;
    function zy(i: integer): integer;
  public
    constructor Create(L : Plua_State; lp: TLuaPrint); overload;
    destructor Destroy; override;
  published
    function l4l_TextOut: integer;
    function l4l_Rectangle: integer;
    function l4l_fillrect: integer;
    function l4l_Line: integer;
    function l4l_AddPolyPoint: integer;
    function l4l_AddBezierPoint: integer;
    function l4l_Polygon: integer;
    function l4l_Polyfill: integer;
    function l4l_Polyline: integer;
    function l4l_DrawImage: integer;
    function l4l_font_color: integer;
    function l4l_font_name: integer;
    function l4l_font_size: integer;
    function l4l_font_height: integer;
    function l4l_font_style: integer;
    function l4l_font_orientation: integer;
    function l4l_pen_color: integer;
    function l4l_pen_style: integer;
    function l4l_pen_joinstyle: integer;
    function l4l_pen_endcap: integer;
    function l4l_pen_mode: integer;
    function l4l_pen_width: integer;
    function l4l_brush_color: integer;
    function l4l_brush_style: integer;
    function l4l_PushCanvas: integer;
    function l4l_PopCanvas: integer;
    function l4l_SetClipRect: integer;
  end;

{ TLuaPrint }

function TLuaPrint.GetPageCount: integer;
begin
  Result := FPageList.Count;
end;

function TLuaPrint.DP2LP(dp: integer): integer;
begin
  Result:= Trunc(MM_P_INCH * dp / FDPI + 0.5);
end;

function TLuaPrint.GetPageSize: TSize;
begin
  Result.cx := FPaperRect.PhysicalRect.Right-FRealMargin.Left-FRealMargin.Right;
  Result.cy:= FPaperRect.PhysicalRect.Bottom-FRealMargin.Top-FRealMargin.Bottom;
end;

function TLuaPrint.GetPaperSize: TSize;
begin
  Result.cx := FPaperRect.PhysicalRect.Right;
  Result.cy:= FPaperRect.PhysicalRect.Bottom;
end;

procedure TLuaPrint.CopyCanvas(src, dst: TCanvas);
begin
  dst.Font.Assign(src.Font);
  dst.Pen.Assign(src.Pen);
  dst.Brush.Assign(src.Brush);
end;

procedure TLuaPrint.PushCanvas;
var
  c: TCanvas;
begin
  c := TCanvas.Create;
  CopyCanvas(FCanvas, c);
  FCanvasStack.Add(c);
end;

procedure TLuaPrint.PopCanvas;
var
  c: TCanvas;
begin
  c := TCanvas(FCanvasStack[FCanvasStack.Count-1]);
  CopyCanvas(c, FCanvas);
  FCanvasStack.Delete(FCanvasStack.Count-1);
end;

function TLuaPrint.LP2DP(lp: integer): integer;
begin
  Result:= Trunc(lp * FDPI / MM_P_INCH + 0.5);
end;

function TLuaPrint.z(i: integer): integer;
begin
  Result := i*FPlayDpi*FZoom div (FDPI*100);
end;

procedure TLuaPrint.AddOrder(const s: string);
begin
  TStringList(FPageList[FPageList.Count-1]).Add(s);
end;

constructor TLuaPrint.Create(L : Plua_State);
begin
  LS := L;
  FPageList:= TObjectList.Create(True);
  FBmpList:= TObjectList.Create(True);
  FResList:= TObjectList.Create(True);
  FCanvasStack := TObjectList.Create(True);
end;

destructor TLuaPrint.Destroy;
begin
  FPageList.Free;
  FBmpList.Free;
  FResList.Free;
  FCanvasStack.Free;
  inherited Destroy;
end;

procedure TLuaPrint.BeginDoc;
begin
  BeginDoc(types.Rect(0,0,0,0));
end;

procedure TLuaPrint.BeginDoc(Margin: TRect);
var
  bmp: graphics.TBitmap;
begin
  FDPI := Printer.YDPI;
  FPageList.Clear;
  FPageList.Add(TStringList.Create);
  FPaperRect := Printer.PaperSize.PaperRect;
  FUserMargin :=
   types.Rect(LP2DP(Margin.Left), LP2DP(Margin.Top),
        LP2DP(Margin.Right), LP2DP(Margin.Bottom));
  FRealMargin := FUserMargin;
  if FPaperRect.WorkRect.Left > FRealMargin.Left then
    FRealMargin.Left := FPaperRect.WorkRect.Left;
  if FPaperRect.WorkRect.Top > FRealMargin.Top then
    FRealMargin.Top := FPaperRect.WorkRect.Top;
  if FPaperRect.PhysicalRect.Right-FPaperRect.WorkRect.Right > FRealMargin.Right then
    FRealMargin.Right := FPaperRect.PhysicalRect.Right-FPaperRect.WorkRect.Right;
  if FPaperRect.PhysicalRect.Bottom-FPaperRect.WorkRect.Bottom > FRealMargin.Bottom then
    FRealMargin.Bottom := FPaperRect.PhysicalRect.Bottom-FPaperRect.WorkRect.Bottom;
  FBmpList.Clear;
  bmp := graphics.TBitmap.Create;
  FBmpList.Add(bmp);
  FCanvas:= bmp.Canvas;
  FCanvas.Font.PixelsPerInch:= FDPI;
  FCanvas.Font.Size:= 10;
  FCanvasStack.Clear;
  PushCanvas;
  FResList.Clear;
end;

procedure TLuaPrint.EndDoc;
begin
  PopCanvas;
  FCanvasStack.Clear;
  if FPageList.Count > 0 then begin
    if TStringList(FPageList[FPageList.Count-1]).Count = 0 then begin
      FPageList.Delete(FPageList.Count-1);
      FBmpList.Delete(FBmpList.Count-1);
    end;
  end;
  FCanvas := nil;
end;

procedure TLuaPrint.NewPage;
var
  bmp: graphics.TBitmap;
begin
  FPageList.Add(TStringList.Create);
  bmp := graphics.TBitmap.Create;
  FBmpList.Add(bmp);
  CopyCanvas(FCanvas, bmp.Canvas);
  PopCanvas;
  FCanvas:= bmp.Canvas;
  FCanvas.Font.PixelsPerInch:= Printer.YDPI;
  FCanvas.Font.Size:= FCanvas.Font.Size;
  PushCanvas;
end;

procedure TLuaPrint.Run(const SourceCode: string);
begin
  if luaL_loadbuffer(LS, PChar(SourceCode), Length(SourceCode), 'print') <> 0 then
    Raise Exception.Create('');
  if lua_pcall(LS, 0, 0, 0) <> 0 then
    Raise Exception.Create('');
end;

procedure TLuaPrint.Play(pageNumber: integer; Cv: TCanvas; dpi: integer;
  Zoom: integer);
begin
  Play(pageNumber, Cv, types.Rect(0,0,0,0), dpi, Zoom);
end;

procedure TLuaPrint.Play(pageNumber: integer; Cv: TCanvas; Margin: TRect;
  dpi, Zoom: integer);
var
  i : integer;
  sl: TStringList;
  x, y: integer;
  bmp: graphics.TBitmap;
begin
  if (pageNumber > 0) and (pageNumber <= PageCount) then begin
    if dpi = 0 then dpi := Cv.Font.PixelsPerInch;
    FPlayDpi:= dpi;
    FZoom := Zoom;
    FOffset.x:= FRealMargin.Left;
    FOffset.y:= FRealMargin.Top;
    x := FPaperRect.PhysicalRect.Right-FRealMargin.Right;
    y := FPaperRect.PhysicalRect.Bottom-FRealMargin.Bottom;
    Dec(FOffset.x, Margin.Left);
    Dec(FOffset.y, Margin.Top);
    Dec(x, Margin.Left);
    Dec(y, Margin.Top);

    FCanvas := Cv;
    bmp := graphics.TBitmap(FBmpList[pageNumber-1]);
    CopyCanvas(bmp.Canvas, FCanvas);
    i := FCanvas.Font.Size;
    FCanvas.Font.PixelsPerInch:= FPlayDpi;
    FCanvas.Font.Size:= i + 1;
    FCanvas.Font.Size:= i;
    FCanvas.Font.Height:= FCanvas.Font.Height * FZoom div 100;
    FCanvas.Pen.Width:= FCanvas.Pen.Width * FPlayDpi * FZoom div (FDPI * 100);

    FCanvas.ClipRect := types.Rect(0, 0, z(x)+1, z(y)+1);
    FCanvas.Clipping:= True;
    try
      sl := TStringList(FPageList[pageNumber-1]);
      l4l_PushLuaObject(TLuaPrintRunObject.Create(LS, Self));
      lua_setglobal(LS, PRUN_NAME);
      try
        if luaL_loadbuffer(LS, PChar(sl.Text), Length(sl.Text), nil) <> 0 then
          Raise Exception.Create('');
        if lua_pcall(LS, 0, 0, 0) <> 0 then
          Raise Exception.Create('');
      finally
        lua_pushnil(LS);
        lua_setglobal(LS, PRUN_NAME);
      end;
    finally
      FCanvas.Clipping:= False;
    end;
  end;
end;

procedure TLuaPrint.Print(beginPage: integer; endPage: integer);
var
  i: integer;
  m: TRect;
begin
  if beginPage < 1 then beginPage := 1;
  if endPage < beginPage then endPage := FPageList.Count;
  Printer.BeginDoc;
  try
    m := types.Rect(
     Printer.PaperSize.PaperRect.WorkRect.Left,
     Printer.PaperSize.PaperRect.WorkRect.Top,
     Printer.PaperSize.PaperRect.PhysicalRect.Right
      -Printer.PaperSize.PaperRect.WorkRect.Right,
     Printer.PaperSize.PaperRect.PhysicalRect.Bottom
      -Printer.PaperSize.PaperRect.WorkRect.Bottom);
    for i := beginPage to endPage do begin
      Play(i, Printer.Canvas, m, Printer.YDPI, 100);
      if i < endPage then Printer.NewPage;
    end;
    Printer.EndDoc;
  except
    Printer.Abort;
    Raise;
  end;
end;

function str_param(const s: string): string;
var
  i: integer;
begin
  Result := '"';
  for i:= 1 to Length(s) do begin
    case s[i] of
      '"': Result:= Result + '\"';
      '\': Result:= Result + '\\';
      else Result := Result + s[i];
    end;
  end;
  Result := Result + '"';
end;

{ TLuaPrintObject }

function TLuaPrintObject.DP2LP(dp: integer): integer;
var
  i : integer;
begin
  case Upcase(FUnits) of
    'M': i:= MM_P_INCH;
    'I': i:= 1000;
    'T': i:= 1440;
    else begin
      Result := dp;
      Exit;
    end;
  end;
  Result:= Trunc(dp * i / LuaPrint.FDPI + 0.5);
end;

function TLuaPrintObject.GetBrushObject: TLuaBrushObject;
begin
  Result := TLuaBrushObject.Create(LS, Self);
end;

function TLuaPrintObject.GetFontObject: TLuaFontObject;
begin
  Result := TLuaFontObject.Create(LS, Self);
end;

function TLuaPrintObject.GetPenObject: TLuaPenObject;
begin
  Result := TLuaPenObject.Create(LS, Self);
end;

function TLuaPrintObject.GetPageHeight: integer;
begin
  Result := DP2LP(LuaPrint.PageSize.cy);
end;

function TLuaPrintObject.GetPageLeft: integer;
begin
  Result := DP2LP(LuaPrint.FRealMargin.Left);
end;

function TLuaPrintObject.GetPageTop: integer;
begin
  Result := DP2LP(LuaPrint.FRealMargin.Top);
end;

function TLuaPrintObject.GetPageNumber: integer;
begin
  Result := LuaPrint.PageCount;
end;

function TLuaPrintObject.GetPageWidth: integer;
begin
  Result := DP2LP(LuaPrint.PageSize.cx);
end;

function TLuaPrintObject.LP2DP(lp: integer): integer;
var
  i : integer;
begin
  case Upcase(FUnits) of
    'M': i:= MM_P_INCH;
    'I': i:= 1000;
    'T': i:= 1440;
    else begin
      Result := lp;
      Exit;
    end;
  end;
  Result:= Trunc(lp * LuaPrint.FDPI / i + 0.5);
end;

procedure TLuaPrintObject.GetFontName(var FontName: string);
begin
  // ToDo
end;

constructor TLuaPrintObject.Create(L: Plua_State; lp: TLuaPrint);
begin
  inherited Create(L);
  LuaPrint:= lp;
  FUnits := 'M';
end;

destructor TLuaPrintObject.Destroy;
begin
  inherited Destroy;
end;

function TLuaPrintObject.l4l_TextOut: integer;
begin
  LuaPrint.AddOrder(
   Format(PRUN_NAME + '.TextOut(%d,%d,%s)',
   [LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2)),
   str_param(lua_tostring(LS, 3))]));
  Result := 0;
end;

function TLuaPrintObject.l4l_Rectangle: integer;
begin
  LuaPrint.AddOrder(
   Format(PRUN_NAME + '.rectangle(%d,%d,%d,%d)',
   [LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2)),
    LP2DP(lua_tointeger(LS, 3)), LP2DP(lua_tointeger(LS, 4))]));
  Result := 0;
end;

function TLuaPrintObject.l4l_Line: integer;
var
  c: integer;
begin
  c := lua_gettop(LS);
  if c < 4 then begin
    LuaPrint.AddOrder(
     Format(PRUN_NAME + '.line(%d,%d)',
     [LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2))]));
  end else begin
    LuaPrint.AddOrder(
     Format(PRUN_NAME + '.line(%d,%d,%d,%d)',
     [LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2)),
      LP2DP(lua_tointeger(LS, 3)), LP2DP(lua_tointeger(LS, 4))]));
  end;
  Result := 0;
end;

function TLuaPrintObject.l4l_TextWidth: integer;
begin
  lua_pushinteger(LS, DP2LP(LuaPrint.FCanvas.TextWidth(lua_tostring(LS, 1))));
  Result := 1;
end;

function TLuaPrintObject.l4l_TextHeight: integer;
begin
  lua_pushinteger(LS, DP2LP(LuaPrint.FCanvas.TextHeight(lua_tostring(LS, 1))));
  Result := 1;
end;

function TLuaPrintObject.l4l_DrawImage: integer;
var
  fn: string;
  ms: TMemoryStream;
begin
  fn := lua_tostring(LS, -1);
  ms := TMemoryStream.Create;
  ms.LoadFromFile(fn);
  LuaPrint.FResList.Add(ms);
  case lua_gettop(LS) of
    5: begin
      LuaPrint.AddOrder(
       Format(PRUN_NAME + '.drawimage(%d,%d,%d,%d,%d,%s)',
       [LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2)),
        LP2DP(lua_tointeger(LS, 3)), LP2DP(lua_tointeger(LS, 4)),
        LuaPrint.FResList.Count-1, str_param(ExtractFileExt(fn))]));
    end;
  end;
  Result := 0;
end;

function TLuaPrintObject.l4l_DrawPDF: integer;
var
  fn: string;
  fs: TFileStream;
begin
  fn := lua_tostring(LS, -1);
  fs := TFileStream.Create(fn, fmOpenRead);
  try
    LuaPrint.PushCanvas;
    LuaPrint.AddOrder(PRUN_NAME + '.PushCanvas()');
    case lua_gettop(LS) of
      5: begin
        DrawPDF(fs, Self, 1,
         LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2)),
         LP2DP(lua_tointeger(LS, 3)), LP2DP(lua_tointeger(LS, 4)));
      end;
      6: begin
        DrawPDF(fs, Self, lua_tointeger(LS, 5),
         LP2DP(lua_tointeger(LS, 1)), LP2DP(lua_tointeger(LS, 2)),
         LP2DP(lua_tointeger(LS, 3)), LP2DP(lua_tointeger(LS, 4)));
      end;
    end;
    LuaPrint.AddOrder(PRUN_NAME + '.PopCanvas()');
    LuaPrint.PopCanvas;
  finally
    fs.Free;
  end;
  Result := 0;
end;

function TLuaPrintObject.l4l_NewPage: integer;
begin
  LuaPrint.NewPage;
  Result := 0;
end;

function TLuaPrintObject.l4l_DP2LP: integer;
begin
  lua_pushinteger(LS, DP2LP(lua_tointeger(LS, 1)));
  Result := 1;
end;

function TLuaPrintObject.l4l_LP2DP: integer;
begin
  lua_pushinteger(LS, LP2DP(lua_tointeger(LS, 1)));
  Result := 1;
end;

{ TLuaFontObject }

constructor TLuaFontObject.Create(L: Plua_State; aLPO: TLuaPrintObject);
begin
  inherited Create(L);
  LPO := aLPO;
end;

destructor TLuaFontObject.Destroy;
begin
  inherited Destroy;
end;

function TLuaFontObject.GetColor: string;
begin
  Result := ColorToString(LPO.LuaPrint.FCanvas.Font.Color);
end;

function TLuaFontObject.GetHeight: integer;
begin
  Result := LPO.LuaPrint.FCanvas.Font.Height;
end;

function TLuaFontObject.GetName: string;
begin
  Result := LPO.LuaPrint.FCanvas.Font.Name;
end;

function TLuaFontObject.GetOrientation: integer;
begin
  Result := LPO.LuaPrint.FCanvas.Font.Orientation;
end;

function TLuaFontObject.GetSize: integer;
begin
  Result := LPO.LuaPrint.FCanvas.Font.Size;
end;

function TLuaFontObject.GetStyle: string;
begin
  Result := SetToString(PTypeInfo(TypeInfo(TFontStyles)),
   Integer(LPO.LuaPrint.FCanvas.Font.Style), false);
end;

procedure TLuaFontObject.SetColor(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := StringToColor(AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Font.Color := TColor(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.font_color(%d)', [i]));
  end;
end;

procedure TLuaFontObject.SetHeight(const AValue: integer);
begin
  LPO.LuaPrint.FCanvas.Font.Height := AValue;
  LPO.LuaPrint.AddOrder(
   Format(PRUN_NAME + '.font_height(%d)', [AValue]));
end;

procedure TLuaFontObject.SetName(const AValue: string);
begin
  LPO.LuaPrint.FCanvas.Font.Name := AValue;
  LPO.LuaPrint.AddOrder(
   Format(PRUN_NAME + '.font_name(%s)', [str_param(AValue)]));
end;

procedure TLuaFontObject.SetOrientation(AValue: integer);
begin
  LPO.LuaPrint.FCanvas.Font.Orientation := AValue;
  LPO.LuaPrint.AddOrder(
   Format(PRUN_NAME + '.font_orientation(%d)', [AValue]));
end;

procedure TLuaFontObject.SetSize(const AValue: integer);
begin
  LPO.LuaPrint.FCanvas.Font.Size := AValue;
  LPO.LuaPrint.AddOrder(
   Format(PRUN_NAME + '.font_size(%d)', [AValue]));
end;

procedure TLuaFontObject.SetStyle(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := StringToSet(PTypeInfo(TypeInfo(TFontStyles)), AValue);
  end;
  LPO.LuaPrint.FCanvas.Font.Style := TFontStyles(i);
  LPO.LuaPrint.AddOrder(
   Format(PRUN_NAME + '.font_style(%d)', [i]));
end;

{ TLuaPenObject }

function TLuaPenObject.GetStyle: string;
begin
  Result := GetEnumName(TypeInfo(TPenStyle),
   Integer(LPO.LuaPrint.FCanvas.Pen.Style));
end;

function TLuaPenObject.GetWidth: integer;
begin
  Result := LPO.DP2LP(LPO.LuaPrint.FCanvas.Pen.Width);
  if Result < 1 then Result := 1;
end;

function TLuaPenObject.GetColor: string;
begin
  Result := ColorToString(LPO.LuaPrint.FCanvas.Pen.Color);
end;

function TLuaPenObject.GetMode: string;
begin
  Result := GetEnumName(TypeInfo(TPenMode),
   Integer(LPO.LuaPrint.FCanvas.Pen.Mode));
end;

procedure TLuaPenObject.SetColor(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := StringToColor(AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Pen.Color := TColor(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.pen_color(%d)', [i]));
  end;
end;

procedure TLuaPenObject.SetMode(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := GetEnumValue(TypeInfo(TPenMode), AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Pen.Mode := TPenMode(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.pen_mode(%d)', [i]));
  end;
end;

procedure TLuaPenObject.SetStyle(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := GetEnumValue(TypeInfo(TPenStyle), AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Pen.Style := TPenSTyle(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.pen_style(%d)', [i]));
  end;
end;

procedure TLuaPenObject.SetWidth(const AValue: integer);
var
  i: integer;
begin
  i:= LPO.LP2DP(AValue);
  if i < 1 then i := 1;
  LPO.LuaPrint.FCanvas.Pen.Width:= i;
  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_width(%d)', [i]));
end;

constructor TLuaPenObject.Create(L: Plua_State; aLPO: TLuaPrintObject);
begin
  inherited Create(L);
  LPO := aLPO;
end;

destructor TLuaPenObject.Destroy;
begin
  inherited Destroy;
end;

{ TLuaBrushObject }

function TLuaBrushObject.GetStyle: string;
begin
  Result := GetEnumName(TypeInfo(TBrushStyle),
   Integer(LPO.LuaPrint.FCanvas.Brush.Style));
end;

function TLuaBrushObject.GetColor: string;
begin
  Result := ColorToString(LPO.LuaPrint.FCanvas.Brush.Color);
end;

procedure TLuaBrushObject.SetColor(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := StringToColor(AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Brush.Color := TColor(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.brush_color(%d)', [i]));
  end;
end;

procedure TLuaBrushObject.SetStyle(const AValue: string);
var
  i: integer;
begin
  try
    i := StrToInt(AValue);
  except
    i := GetEnumValue(TypeInfo(TBrushStyle), AValue);
  end;
  if i >= 0 then begin
    LPO.LuaPrint.FCanvas.Brush.Style := TBrushSTyle(i);
    LPO.LuaPrint.AddOrder(
     Format(PRUN_NAME + '.brush_style(%d)', [i]));
  end;
end;

constructor TLuaBrushObject.Create(L: Plua_State; aLPO: TLuaPrintObject);
begin
  inherited Create(L);
  LPO := aLPO;
end;

destructor TLuaBrushObject.Destroy;
begin
  inherited Destroy;
end;

{ TLuaPrintRunObject }

function TLuaPrintRunObject.w(i: integer): integer;
begin
  Result := i * LuaPrint.FPlayDpi * LuaPrint.FZoom div (LuaPrint.FDPI * 100);
end;

function TLuaPrintRunObject.zx(i: integer): integer;
begin
  Result := (i + LuaPrint.FOffset.x) *
   LuaPrint.FPlayDpi * LuaPrint.FZoom div (LuaPrint.FDPI * 100);
end;

function TLuaPrintRunObject.zy(i: integer): integer;
begin
  Result := (i + LuaPrint.FOffset.y) *
   LuaPrint.FPlayDpi * LuaPrint.FZoom div (LuaPrint.FDPI * 100);
end;

constructor TLuaPrintRunObject.Create(L: Plua_State; lp: TLuaPrint);
begin
  inherited Create(L);
  LuaPrint:= lp;
{$IFDEF USE_AGG}
  AggLCLCanvas:= TMyAggCanvas.Create;
  AggLCLCanvas.Image.PixelFormat:= afpimRGBA32;
  AggLCLCanvas.AggAntiAliasGamma:= 1.0;
  rx1:= MaxInt; ry1 := MaxInt; rx2 := 0; ry2 := 0;
{$ENDIF}
end;

destructor TLuaPrintRunObject.Destroy;
begin
{$IFDEF USE_AGG}
  AggLCLCanvas.Free;
{$ENDIF}
  inherited Destroy;
end;

function TLuaPrintRunObject.l4l_TextOut: integer;
begin
  LuaPrint.FCanvas.TextOut(
   zx(lua_tointeger(LS, 1)), zy(lua_tointeger(LS, 2)), lua_tostring(LS, 3));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_Rectangle: integer;
var
  x1, y1, x2, y2: integer;
begin
  x1:= zx(lua_tointeger(LS, 1));
  y1:= zy(lua_tointeger(LS, 2));
  x2:= zx(lua_tointeger(LS, 3));
  y2:= zy(lua_tointeger(LS, 4));
  if x1 = x2 then begin
    LuaPrint.FCanvas.Line(x1, y1, x1, y2);
  end else if y1 = y2 then begin
    LuaPrint.FCanvas.Line(x1, y1, x2, y1);
  end else begin
    LuaPrint.FCanvas.Rectangle(x1, y1, x2, y2);
  end;
  Result := 0;
end;

function TLuaPrintRunObject.l4l_fillrect: integer;
begin
  LuaPrint.FCanvas.FillRect(
   zx(lua_tointeger(LS, 1)), zy(lua_tointeger(LS, 2)),
   zx(lua_tointeger(LS, 3)), zy(lua_tointeger(LS, 4)));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_Line: integer;
var
  c: integer;
begin
  c := lua_gettop(LS);
  if c < 4 then begin
    LuaPrint.FCanvas.LineTo(
     zx(lua_tointeger(LS, 1)), zy(lua_tointeger(LS, 2)));
  end else begin
    LuaPrint.FCanvas.Line(
     zx(lua_tointeger(LS, 1)), zy(lua_tointeger(LS, 2)),
     zx(lua_tointeger(LS, 3)), zy(lua_tointeger(LS, 4)));
  end;
  Result := 0;
end;

function TLuaPrintRunObject.l4l_AddPolyPoint: integer;
var
  i,
{$IFDEF USE_AGG}
  x, y,
{$ELSE}
  l,
{$ENDIF}
  c: integer;
begin
  c := lua_gettop(LS);
  if c > 0 then begin
{$IFDEF USE_AGG}
    x := zx(lua_tointeger(LS, 1));
    y := zy(lua_tointeger(LS, 2));
    AggLCLCanvas.Path.m_path.move_to(x, y);
    if x < rx1 then rx1 := x;
    if y < ry1 then ry1 := y;
    if x > rx2 then rx2 := x;
    if y > ry2 then ry2 := y;
    for i := 2 to c div 2 do begin
      x := zx(lua_tointeger(LS, i*2-1));
      y := zy(lua_tointeger(LS, i*2));
      AggLCLCanvas.Path.m_path.line_to(x, y);
      if x < rx1 then rx1 := x;
      if y < ry1 then ry1 := y;
      if x > rx2 then rx2 := x;
      if y > ry2 then ry2 := y;
    end;
{$ELSE}
    l := Length(PPP);
    SetLength(PPP, l + c div 2);
    for i := 1 to c div 2 do begin
      PPP[l+i-1] := types.Point(zx(lua_tointeger(LS, i*2-1)), zy(lua_tointeger(LS, i*2)));
    end;
    SetLength(PPC, Length(PPC)+1);
    PPC[Length(PPC)-1] := c div 2;
{$ENDIF}
  end else begin
{$IFDEF USE_AGG}
    AggLCLCanvas.Path.m_path.remove_all;
{$ELSE}
    SetLength(PPP, 0);
    SetLength(PPC, 0);
{$ENDIF}
  end;
  Result := 0;
end;

function TLuaPrintRunObject.l4l_AddBezierPoint: integer;
var
  i,
{$IFDEF USE_AGG}
{$ELSE}
  l,
{$ENDIF}
  c: integer;
  p: array of TPoint;
  pp: PPoint;
begin
  c := lua_gettop(LS);
  if c > 0 then begin
    SetLength(p, c div 2);
    for i := 1 to c div 2 do begin
      p[i-1] := types.Point(zx(lua_tointeger(LS, i*2-1)), zy(lua_tointeger(LS, i*2)));
    end;
    pp:= AllocMem(0);
    try
      PolyBezier2Polyline(p, pp, c, True);
{$IFDEF USE_AGG}
      AggLCLCanvas.Path.m_path.move_to(pp^.x, pp^.y);
      if pp^.x < rx1 then rx1 := pp^.x;
      if pp^.y < ry1 then ry1 := pp^.y;
      if pp^.x > rx2 then rx2 := pp^.x;
      if pp^.y > ry2 then ry2 := pp^.y;
      for i := 2 to c do begin
        AggLCLCanvas.Path.m_path.line_to((pp+i-1)^.x, (pp+i-1)^.y);
        if (pp+i-1)^.x < rx1 then rx1 := (pp+i-1)^.x;
        if (pp+i-1)^.y < ry1 then ry1 := (pp+i-1)^.y;
        if (pp+i-1)^.x > rx2 then rx2 := (pp+i-1)^.x;
        if (pp+i-1)^.y > ry2 then ry2 := (pp+i-1)^.y;
      end;
{$ELSE}
      l := Length(PPP);
      SetLength(PPP, l + c);
      for i := 1 to c do PPP[l+i-1] := (pp+i-1)^;
      SetLength(PPC, Length(PPC)+1);
      PPC[Length(PPC)-1] := c;
{$ENDIF}
    finally
      FreeMem(pp);
    end;
  end else begin
{$IFDEF USE_AGG}
    AggLCLCanvas.Path.m_path.remove_all;
{$ELSE}
    SetLength(PPP, 0);
    SetLength(PPC, 0);
{$ENDIF}
  end;
  Result := 0;
end;

{$IFDEF USE_AGG}
procedure TLuaPrintRunObject.poly_sub(flag: TAggDrawPathFlag; w: boolean);
var
  i: integer;
  bmp: graphics.TBitmap;
  lw : integer;
  x, y: double;
begin
  bmp := graphics.TBitmap.Create;
  try
    lw := LuaPrint.FCanvas.Pen.Width;
    AggLCLCanvas.Pen.AggLineWidth:= lw;
    AggLCLCanvas.Pen.Color:= LuaPrint.FCanvas.Pen.Color;
    case LuaPrint.FCanvas.Pen.EndCap of
      pecRound: AggLCLCanvas.Pen.AggLineCap:= AGG_CapRound;
      pecSquare: AggLCLCanvas.Pen.AggLineCap:= AGG_CapSquare;
      pecFlat: AggLCLCanvas.Pen.AggLineCap:= AGG_CapButt;
    end;
    case LuaPrint.FCanvas.Pen.JoinStyle of
      pjsRound: AggLCLCanvas.Pen.AggLineJoin:= AGG_JoinRound;
      pjsBevel: AggLCLCanvas.Pen.AggLineJoin:= AGG_JoinBevel;
      pjsMiter: AggLCLCanvas.Pen.AggLineJoin:= AGG_JoinMiter;
    end;
    AggLCLCanvas.Brush.Color:= LuaPrint.FCanvas.Brush.Color;
    AggLCLCanvas.Brush.AggFillEvenOdd:= not w;
    AggLCLCanvas.Image.SetSize(rx2-rx1+1+lw*2, ry2-ry1+1+lw*2);
    AggLCLCanvas.Erase;
    for i := 0 to AggLCLCanvas.Path.m_path.total_vertices-1 do begin
      AggLCLCanvas.Path.m_path.vertex_(i, @x ,@y);
      AggLCLCanvas.Path.m_path.modify_vertex(i, x-rx1+lw, y-ry1+lw);
    end;
    AggLCLCanvas.AggDrawPath(flag);
    bmp.LoadFromIntfImage(AggLCLCanvas.Image.IntfImg);
    LuaPrint.FCanvas.Draw(rx1-lw, ry1-lw, bmp);
    rx1:= MaxInt; ry1 := MaxInt; rx2 := 0; ry2 := 0;
  finally
    bmp.Free;
  end;
end;
{$ENDIF}

function TLuaPrintRunObject.l4l_Polygon: integer;
{$IFDEF USE_AGG}
{$ELSE}
var
  i: integer;
{$ENDIF}
begin
{$IFDEF USE_AGG}
  poly_sub(AGG_FillAndStroke, lua_toboolean(LS, 1));
{$ELSE}
  {$IFDEF WINDOWS}
  i := ALTERNATE;
  if lua_toboolean(LS, 1) then i := WINDING;
  SetPolyFillMode(LuaPrint.FCanvas.Handle, i);
  PolyPolygon(LuaPrint.FCanvas.Handle, PPP[0], PPC[0], Length(PPC));
  {$ELSE}
  // ToDo
  LuaPrint.FCanvas.Pen  .Polygon(PPP, lua_toboolean(LS, 1));
  {$ENDIF}
{$ENDIF}
  Result := 0;
end;

function TLuaPrintRunObject.l4l_Polyfill: integer;
{$IFDEF USE_AGG}
{$ELSE}
var
  i: integer;
  ps: TPenStyle;
{$ENDIF}
begin
{$IFDEF USE_AGG}
  poly_sub(AGG_FillOnly, lua_toboolean(LS, 1));
{$ELSE}
  {$IFDEF WINDOWS}
  i := ALTERNATE;
  if lua_toboolean(LS, 1) then i := WINDING;
  SetPolyFillMode(LuaPrint.FCanvas.Handle, i);
  ps := LuaPrint.FCanvas.Pen.Style;
  LuaPrint.FCanvas.Pen.Style:= psClear;
  PolyPolygon(LuaPrint.FCanvas.Handle, PPP[0], PPC[0], Length(PPC));
  LuaPrint.FCanvas.Pen.Style:= ps;
  {$ELSE}
  // ToDo
  LuaPrint.FCanvas.Polygon(PPP, lua_toboolean(LS, 1));
  {$ENDIF}
{$ENDIF}
  Result := 0;
end;

function TLuaPrintRunObject.l4l_Polyline: integer;
begin
{$IFDEF USE_AGG}
  poly_sub(AGG_StrokeOnly, False);
{$ELSE}
  {$IFDEF WINDOWS}
  PolyPolyline(LuaPrint.FCanvas.Handle, PPP[0], PPC[0], Length(PPC));
  {$ELSE}
  // ToDo
  LuaPrint.FCanvas.Polyline(PPP);
  {$ENDIF}
{$ENDIF}
  Result := 0;
end;

function TLuaPrintRunObject.l4l_DrawImage: integer;
var
  ms: TStream;
  g: TPicture;
  x1, y1, x2, y2, i: integer;
  n: lua_number;
begin
  g := TPicture.Create;
  try
    ms := TStream(LuaPrint.FResList[lua_tointeger(LS, -2)]);
    ms.Position:= 0;
    //g.LoadFromStreamWithFileExt(ms, lua_tostring(LS, -1));
    g.LoadFromStream(ms);
    x1 := zx(lua_tointeger(LS, 1));
    y1 := zy(lua_tointeger(LS, 2));
    x2 := zx(lua_tointeger(LS, 3));
    y2 := zy(lua_tointeger(LS, 4));
    n := Abs(y2 - y1) / g.Height;
    i := Trunc(g.Width * n);
    if i < Abs(x2 - x1) then begin
      x2 := x1 + i;
      y2 := y1 + Trunc(g.Height * n);
    end else begin
      n := Abs(x2 - x1) / g.Width;
      x2 := x1 + Trunc(g.Width  * n);
      y2 := y1 + Trunc(g.Height * n);
    end;
    LuaPrint.FCanvas.StretchDraw(types.Rect(x1, y1, x2, y2), g.Graphic);
  finally
    g.Free;
  end;
  Result := 0;
end;

function TLuaPrintRunObject.l4l_font_color: integer;
begin
  LuaPrint.FCanvas.Font.Color := TColor(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_font_name: integer;
begin
  LuaPrint.FCanvas.Font.Name := lua_tostring(LS, 1);
  Result := 0;
end;

function TLuaPrintRunObject.l4l_font_size: integer;
begin
  LuaPrint.FCanvas.Font.Size := lua_tointeger(LS, 1);
  LuaPrint.FCanvas.Font.Height:=
   LuaPrint.FCanvas.Font.Height * LuaPrint.FZoom div 100;
  Result := 0;
end;

function TLuaPrintRunObject.l4l_font_height: integer;
begin
  LuaPrint.FCanvas.Font.Height := w(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_font_style: integer;
begin
  LuaPrint.FCanvas.Font.Style := TFontStyles(Integer(lua_tointeger(LS, 1)));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_font_orientation: integer;
begin
  LuaPrint.FCanvas.Font.Orientation := lua_tointeger(LS, 1);
  Result := 0;
end;

function TLuaPrintRunObject.l4l_pen_color: integer;
begin
  LuaPrint.FCanvas.Pen.Color := TColor(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_pen_style: integer;
begin
  LuaPrint.FCanvas.Pen.Style := TPenStyle(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_pen_joinstyle: integer;
begin
  LuaPrint.FCanvas.Pen.JoinStyle := TPenJoinStyle(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_pen_endcap: integer;
begin
  LuaPrint.FCanvas.Pen.EndCap := TPenEndCap(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_pen_mode: integer;
begin
  LuaPrint.FCanvas.Pen.Mode := TPenMode(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_pen_width: integer;
begin
  LuaPrint.FCanvas.Pen.Width:= w(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_brush_color: integer;
begin
  LuaPrint.FCanvas.Brush.Color := TColor(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_brush_style: integer;
begin
  LuaPrint.FCanvas.Brush.Style := TBrushStyle(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaPrintRunObject.l4l_PushCanvas: integer;
begin
  LuaPrint.PushCanvas;
  Result := 0;
end;

function TLuaPrintRunObject.l4l_PopCanvas: integer;
begin
  LuaPrint.PopCanvas;
  Result := 0;
end;

function TLuaPrintRunObject.l4l_SetClipRect: integer;
begin
  LuaPrint.FCanvas.Region.ClipRect :=
   types.Rect(lua_tointeger(LS, 1), lua_tointeger(LS, 2),
        lua_tointeger(LS, 3), lua_tointeger(LS, 4));
  Result := 0;
end;

end.

