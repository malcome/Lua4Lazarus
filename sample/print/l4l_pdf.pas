unit l4l_pdf;
{
  Lua4Lazarus

    sample:

    License: New BSD
      Copyright(c)2010- Malcome@Japan All rights reserved.

    Note:
      Not support embedded font.
      Not support draw image.
      Not full support color function.

    ToDo:
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, l4l_print;

procedure DrawPDF(stream: TStream; LPO: TLuaPrintObject; page: integer;
 x1, y1, x2, y2: integer);

implementation
uses
  LCLProc, graphics, contnrs, paszlib;

const
  PRUN_NAME = 'P_';

type
  TStrokeType = (tstNone, tstM, tstRe, tstBez);

  { TDblXY }

  TDblXY = class
  public
    tst: TStrokeType;
    x, y: double;
    constructor Create;
  end;

  TMatrix = array[1..3, 1..3] of double;

  { TPDFObj }

  TPDFObj = class
  public
    no: integer;
    val: string;
    decoded: boolean;
    stream: string;
  end;

  { TPDFReader }

  TPDFReader = class
  public
    stream: TStream;
    objs: TStringList;
    buf: string;
    buf_p: PChar;
    buf_l: integer;
    constructor Create(Astream: TStream);
    destructor Destroy; override;
    function FindObj(const no: string): TPDFObj;
    function FindPageObj(no: integer): TPDFObj;
    procedure DecodeObj(obj: TPDFObj);
    procedure ReadBuf;
    function GetVal(const name, arr: string): string;
  end;

  { TFontObj }

  TFontObj = class
  public
    b: integer;
    l: TStringList;
    font_name, font_file_no: string;
    constructor Create;
    destructor Destroy; override;
    procedure make_l(const s: string);
    procedure cid2utf8(const cid: string; objs: TObjectList);
    procedure ascii_w(const ascii: string; objs: TObjectList);
  end;

  TCidObj = class
  public
    utf8: string;
    utf16: word;
    width: integer;
  end;

{ TDblXY }

constructor TDblXY.Create;
begin
  tst := tstNone;
end;

function mempos(str1, str2: PChar; l: integer) : PChar;
var
  i, j, l2: integer;
begin
  l2 := strlen(str2);
  for i := 0 to l-1 do begin
    j := 0;
    while ((j < l2) and (i+j < l) and ((str1+i+j)^ = (str2+j)^)) do Inc(j);
    if j = l2 then begin
      Result := str1 + i;
      Exit;
    end;
  end;
  Result := nil;
end;

function TokenStr(var p: PChar): string;
var
  c: char;
  i: integer;
begin
  Result := '';
  while p^ in [#$09, #$0a, #$0c, #$0d, ' '] do Inc(p);
  if p^ = #0 then Exit;
  if p^ in ['(', '<', '[', '{'] then begin
    case p^ of
      '(': c := ')';
      '<': c := '>';
      '[': c := ']';
      '{': c := '}';
    end;
    i:= 0;
    Result := p^;
    Inc(p);
    while (p^ <> #0) and ((i > 0) or (p^ <> c)) do begin
      if (c = ')') and (p^ = '\') and ((p+1)^ in ['(', ')']) then begin
        Result := Result + p^;
        Inc(p);
      end else begin
        if (p^ = Result[1]) then begin
          Inc(i);
        end else if (p^ = c) then begin
          Dec(i);
        end;
      end;
      Result := Result + p^;
      Inc(p);
    end;
    if p^ <> #0 then begin
      Result := Result + p^;
      Inc(p);
    end;
  end else begin
    case p^ of
      '/', '%': begin
        Result := p^;
        Inc(p);
      end
    end;
    while not (p^ in [#0, #$09, #$0a, #$0c, #$0d, ' ',
     '(', ')', '<', '>', '[', ']', '{', '}', '/', '%']) do begin
      Result := Result + p^;
      Inc(p);
    end;
  end;

  while p^ in [#$09, #$0a, #$0c, #$0d, ' '] do Inc(p);
end;

var
  FontNameTable: TStringList;

procedure CreateFontNameTable;
begin
  FontNameTable := TStringList.Create;
  FontNameTable.Sorted:= True;
{$IFDEF WINDOWS}
  FontNameTable.Text:=
   'ArialMT=Arial' + #$0d +
   'Arial-Black=Arial Black' + #$0d +
   'Arial-BoldMT=Arial Bold' + #$0d +
   'Arial-BoldItalicMT=Arial Bold Italic' + #$0d +
   'Arial-ItalicMT=Arial Italic' + #$0d +
   'ArialNarrow=Arial Narrow' + #$0d +
   'ArialNarrow-Bold=Arial Narrow Bold' + #$0d +
   'ArialNarrow-BoldItalic=Arial Narrow Bold Italic' + #$0d +
   'ArialNarrow-Italic=Arial Narrow Italic' + #$0d +
   'ArialUnicodeMS=Arial Unicode MS' + #$0d +

   'CenturyGothic=Century Gothic' + #$0d +
   'CenturyGothic-Bold=Century Gothic Bold' + #$0d +
   'CenturyGothic-BoldItalic=Century Gothic Bold Italic' + #$0d +
   'CenturyGothic-Italic=Century Gothic Italic' + #$0d +

   'ComicSansMS=Comic Sans MS' + #$0d +
   'ComicSansMS-Bold=Comic Sans MS Bold' + #$0d +

   'MS-Gothic=MS Gothic' + #$0d +
   'MS-Mincho=MS Mincho' + #$0d +
   'MS-PGothic=MS PGothic' + #$0d +
   'MS-PMincho=MS PMincho' + #$0d +

   'SymbolMT=Symbol' + #$0d +
   'Tahoma-Bold=Tahoma Bold' + #$0d +
   'TimesNewRomanPSMT=Times New Roman' + #$0d +
   'TimesNewRomanPS-BoldMT=Times New Roman Bold' + #$0d +
   'TimesNewRomanPS-BoldItalicMT=Times New Roman Bold Italic' + #$0d +
   'TimesNewRomanPS-ItalicMT=Times New Roman Italic' + #$0d +

   'Verdana-Bold=Verdana Bold' + #$0d +
   'Verdana-BoldItalic=Verdana Bold Italic' + #$0d +
   'Verdana-Italic=Verdana Italic' + #$0d +
   'Wingdings-Regular=Wingdings' + #$0d +
   'Wingdings2=Wingdings 2' + #$0d +
   'Wingdings3=Wingdings 3' + #$0d +

   '';
{$ENDIF}
end;

{ TPDF }

constructor TPDFReader.Create(Astream: TStream);
begin
  objs := TStringList.Create;
  objs.Sorted:= True;
  stream := Astream;
  stream.Position:= 0;
  buf_l := 0;
end;

destructor TPDFReader.Destroy;
var
  i : integer;
begin
  for i := 0 to objs.Count-1 do objs.Objects[i].Free;
  objs.Free;
  inherited Destroy;
end;

function TPDFReader.FindObj(const no: string): TPDFObj;
var
  i, j, c: integer;
  sp1, sp2: PChar;
  s, s1: string;
  o, o1: TPDFObj;
  ofs: array of integer;
begin
  Result := nil;
  i := objs.IndexOf(no);
  if i >= 0 then begin
    Result := TPDFObj(objs.Objects[i]);
    if Result.decoded then Exit;
  end else begin
    while True do begin
      if (buf_l = 0) or (buf_p >= PChar(buf)+buf_l) then ReadBuf;
      sp1 := mempos(buf_p, 'obj', buf_l-Integer(buf_p-PChar(buf)));
      if sp1 <> nil then begin
        sp2 := mempos(buf_p, 'endobj', buf_l-Integer(buf_p-PChar(buf)));
        if sp2 <> nil then begin
          o := TPDFObj.Create;
          o.decoded:= False;
          SetLength(o.val, sp2-sp1-3);
          move((sp1+3)^, o.val[1], sp2-sp1-3);
          o.val := Trim(o.val);
          s1 := '';
          Dec(sp1, 4);
          while sp1^ in ['0'..'9'] do begin
            s1 := sp1^ + s1;
            Dec(sp1);
          end;
          o.no:= StrToInt(s1);
          Objs.AddObject(s1, o);
          buf_p := sp2 + 6;
          if s1 = no then begin
            Result := o;
            Break;
          end;
        end else
          Exit; // broken pdf
      end else begin
        i:= 0;
        while  i < objs.Count do begin
          o := TPDFObj(objs.Objects[i]);
          if Pos('/Type/ObjStm', o.val) > 0 then begin
            c := StrToIntDef(GetVal('/N', o.val), 0);
            if c = 0 then Exit; // broken pdf
            DecodeObj(o);
            SetLength(s, Length(o.stream));
            move(o.stream[1], s[1], Length(o.stream));
            o.Free;
            objs.Delete(i);
            sp1 := PChar(s);
            SetLength(ofs, c);
            for j:= 1 to c do begin
              TokenStr(sp1);
              ofs[j-1] := StrToInt(TokenStr(sp1));
            end;
            sp2 := PChar(s);
            for j:= 1 to c do begin
              o1 := TPDFObj.Create;
              s1 := TokenStr(sp2);
              TokenStr(sp2);
              if s1 = no then Result := o1;
              objs.AddObject(s1, o1);
              o1.no:= StrToInt(s1);
              if j < c then begin
                SetLength(o1.val, ofs[j]-ofs[j-1]);
                move((sp1+ofs[j-1])^, o1.val[1], ofs[j]-ofs[j-1]);
              end else begin
                SetLength(o1.val, Length(s)-(sp1-PChar(s)));
                move((sp1+ofs[j-1])^, o1.val[1], Length(s)-(sp1-PChar(s)));
              end;
            end;
            if Result <> nil then Break;
          end else
            Inc(i);
        end;
        Break;
      end;
    end; // while
  end;

  if (Result <> nil) and (Result.decoded = False) then begin
    DecodeObj(Result);
  end;
end;

function TPDFReader.FindPageObj(no: integer): TPDFObj;
var
  i: integer;
  sp1, sp2: PChar;
  s1: string;
  o: TPDFObj;
begin
  Result := nil;
  for i := 0 to objs.Count-1 do begin
    o := TPDFObj(objs.Objects[i]);
    if (Pos('/Type/Page', o.val) > 0) and (Pos('/Contents', o.val) > 0) then begin
      Dec(no);
      if no = 0 then begin
        Result := o;
        Break;
      end;
    end;
  end;

  if Result = nil then begin
    while True do begin
      if (buf_l = 0) or (buf_p >= PChar(buf)+buf_l) then ReadBuf;
      sp1 := mempos(buf_p, 'obj', buf_l-Integer(buf_p-PChar(buf)));
      if sp1 <> nil then begin
        sp2 := mempos(buf_p, 'endobj', buf_l-Integer(buf_p-PChar(buf)));
        if sp2 <> nil then begin
          o := TPDFObj.Create;
          o.decoded:= False;
          SetLength(o.val, sp2-sp1-3);
          move((sp1+3)^, o.val[1], sp2-sp1-3);
          o.val := Trim(o.val);
          s1 := '';
          Dec(sp1, 4);
          while sp1^ in ['0'..'9'] do begin
            s1 := sp1^ + s1;
            Dec(sp1);
          end;
          o.no:= StrToInt(s1);
          Objs.AddObject(s1, o);
          buf_p := sp2 + 6;
          if (Pos('/Type/Page', o.val) > 0) and (Pos('/Contents', o.val) > 0) then begin
            Dec(no);
            if no = 0 then begin
              Result := o;
              Break;
            end;
          end;
        end else
          Exit; // broken pdf
      end else
        Exit; // finished
    end; // while
  end;

  if (Result <> nil) and (Result.decoded = False) then begin
    DecodeObj(Result);
  end;
end;

procedure TPDFReader.DecodeObj(obj: TPDFObj);
const
  ZBUF_LEN = 10000;
var
  sp, sp1: PChar;
  s1, s2: string;
  r, len, l: integer;
  z: TZStream;
begin
  obj.decoded:= True;
  sp := PChar(obj.val);
  sp1 := strpos(sp, 'stream');
  if sp1 = nil then Exit;
  s1:= Trim(Copy(sp, 1, sp1-sp));
  len := StrToIntDef(GetVal('/Length', s1), 0);
  if len > 0 then begin
    Inc(sp1, 6);
    while (sp1^ <> #0) and (sp1^ in [#$0d, #$0a]) do Inc(sp1);
    if Pos('/FlateDecode', s1) > 0 then begin
      z.next_in := PByte(sp1);
      z.avail_in := len;
      if inflateInit(z) = Z_OK then begin
        try
          obj.stream := '';
          SetLength(s2, ZBUF_LEN);
          r := Z_OK;
          while r = Z_OK do begin
            z.next_out := PByte(PChar(s2));
            z.avail_out := ZBUF_LEN;
            r := inflate(z, Z_SYNC_FLUSH);
            l := Length(obj.stream);
            SetLength(obj.stream, l+(ZBUF_LEN-z.avail_out));
            move(s2[1], obj.stream[l+1], ZBUF_LEN-z.avail_out);
          end;
        finally
          inflateEnd(z);
        end;
      end;
    end else begin
      SetLength(obj.stream, len);
      move(sp1^, obj.stream[1], len);
    end;
    obj.val:= s1;
  end;
end;

procedure TPDFReader.ReadBuf;
const
  BUF_LEN = 100000;
var
  i, l: integer;
  sp1, sp2: PChar;
begin
  l := BUF_LEN;
  while True do begin
    SetLength(buf, l);
    buf_l := stream.Read(buf[1], l);
    buf_p := PChar(buf);
    if buf_l = l then begin
      sp1 := mempos(buf_p, 'obj', buf_l-Integer(buf_p-PChar(buf)));
      sp2 := mempos(buf_p, 'endobj', buf_l-Integer(buf_p-PChar(buf)));
      if (sp1 = nil) or (sp2 = nil) or (sp1 > sp2) then begin
        stream.Position:= stream.Position - l;
        Inc(l, BUF_LEN);
        continue;
      end;
      sp1 := PChar(buf) + l;
      i := stream.Position;
      while Copy(sp1-6, 1, 6) <> 'endobj' do begin
        Dec(sp1);
        Dec(buf_l);
        Dec(i);
      end;
      stream.Position:= i;
    end;
    Break;
  end; // while
end;

function TPDFReader.GetVal(const name, arr: string): string;

  function GetValSub(sp: PChar): string;
  var
    c: integer;
    s: string;
    o: TPDFObj;
    sp1: PChar;
  begin
    Result := '';
    if (sp^ = '<') and ((sp+1)^ = '<') then begin
      c:= 0;
      sp1 := sp + 2;
      while True do begin
        if (sp1^ = '<') and ((sp1+1)^ = '<') then begin
          Inc(sp1);
          Inc(c);
        end;
        if (sp1^ = '>') and ((sp1+1)^ = '>') then begin
          if c = 0 then begin
            SetLength(Result, sp1-sp+2);
            move(sp^, Result[1], sp1-sp+2);
            Exit;
          end else
            Dec(c);
        end;
        Inc(sp1);
      end;
    end else begin
      s := TokenStr(sp);
      TokenStr(sp);
      if sp^ = 'R' then begin
        o := FindObj(s);
        if o <> nil then begin
          if o.stream <> '' then begin
            Result := o.stream;
          end else
            Result := GetValSub(PChar(o.val));
        end;
      end else
        Result := s;
    end;
  end;

var
  sp: PChar;
begin
  Result := '';
  sp := strpos(PChar(arr), PChar(name));
  if sp = nil then Exit;
  Inc(sp, Length(name));
  while sp^ in [#$09, #$0a, #$0c, #$0d, ' '] do Inc(sp);
  if sp^ = #0 then Exit;
  Result := GetValSub(sp);
end;

{ TFontObj }

constructor TFontObj.Create;
begin
  inherited Create;
  l := TStringList.Create;
  l.Sorted:= True;
end;

destructor TFontObj.Destroy;
var
  i: Integer;
begin
  for i := 0 to l.Count-1 do l.Objects[i].Free;
  l.Free;
  inherited Destroy;
end;

procedure TFontObj.make_l(const s: string);
var
  p, p1: PChar;
  s1, s2, s3, s4: string;
  i, c1, c2, c3: integer;
  cid: TCidObj;
begin
  p := strpos(PChar(s), 'beginbfchar');
  if p = nil then Exit;
  b := 0;
  l.Clear;
  Inc(p, 11);
  while True do begin
    s1 := TokenStr(p);
    if (s1 = '') or (s1 = 'endbfchar') then break;
    b := Length(s1) - 2;
    s2 := TokenStr(p);
    cid := TCidObj.Create;
    cid.utf16:= StrToInt('$' + Copy(s2, 2, Length(s2)-2));
    cid.utf8:= UTF8Encode(WideString(WideChar(cid.utf16)));
    cid.width:= 0;
    l.AddObject(Copy(s1, 2, b), cid);
  end;

  p := strpos(PChar(s), 'beginbfrange');
  if p = nil then Exit;
  Inc(p, 12);
  while True do begin
    s1 := TokenStr(p);
    if (s1 = '') or (s1 = 'endbfrange') then break;
    c1 := StrToInt('$'+Copy(s1, 2, Length(s1)-2));
    s2 := TokenStr(p);
    c2 := StrToInt('$'+Copy(s2, 2, Length(s2)-2));
    s3 := TokenStr(p);
    if (s3 <> '') and (s3[1] = '[') then begin
      i := 0;
      p1 := PChar(s3) + 1;
      while (p1^ <> ']') and (c1 + i <= c2) do begin
        s4 := TokenStr(p1);
        cid := TCidObj.Create;
        cid.utf16:= StrToInt('$' + Copy(s4, 2, Length(s4)-2));
        cid.utf8:= UTF8Encode(WideString(WideChar(cid.utf16)));
        cid.width:= 0;
        l.AddObject(IntToHex(c1+i, b), cid);
        Inc(i);
      end;
    end else begin
      c3 := StrToInt('$'+Copy(s3, 2, Length(s3)-2));
      i := 0;
      while c1 + i <= c2 do begin
        cid := TCidObj.Create;
        cid.utf16:= c3 + i;
        cid.utf8:= UTF8Encode(WideString(WideChar(cid.utf16)));
        cid.width:= 0;
        l.AddObject(IntToHex(c1+i, b), cid);
        Inc(i);
      end;
    end;
  end;
end;

procedure TFontObj.cid2utf8(const cid: string; objs: TObjectList);
var
  s: string;
  i, j: integer;
begin
  for i := 1 to Length(cid) div b do begin
    s := '';
    for j := 1 to b do s := s + cid[(i-1)*b+j];
    j := l.IndexOf(s);
    if j < 0 then break;
    objs.Add(l.Objects[j]);
    if TCidObj(l.Objects[j]).utf8 = '\' then objs.Add(l.Objects[j]);
  end;
end;

procedure TFontObj.ascii_w(const ascii: string; objs: TObjectList);
var
  s: string;
  i, j: integer;
begin
  for i := 1 to Length(ascii) do begin
    s := IntToHex(Byte(ascii[i]), b);
    j := l.IndexOf(s);
    if j < 0 then break;
    objs.Add(l.Objects[j]);
  end;
end;

procedure DrawPDF(stream: TStream; LPO: TLuaPrintObject; page: integer;
 x1, y1, x2, y2: integer);
var
  PageW, PageH, RateW, RateH, Rate: double;
  fonts: TStringList;

  function matrixmul(m1, m2: TMatrix): TMatrix;
  var
    i, j, k: integer;
  begin
    for i:=1 to 3 do begin
      for j:=1 to 3 do begin
        Result[i][j]:= 0;
        for k := 1 to 3 do begin
          Result[i][j] := Result[i][j] + m1[i][k] * m2[k][j];
        end;
      end;
    end;
  end;

  function LStrObj2Str(const str:string): string;
  var
    i, j: integer;
  begin
    Result := '';
    i := 1;
    while i <= Length(str) do begin
      if str[i] = '\' then begin
        if i < Length(str) then begin
           case str[i+1] of
             '(', ')': ;
             '\': begin
               Result := Result + '\\';
               Inc(i);
             end;
             '0'..'9': begin // ToDo
               if i+3 > Length(str) then Break;
               Result := Result +
                Char((Byte(str[i+1])-Byte('0')) shl 6 +
                 (Byte(str[i+2])-Byte('0')) shl 3 +
                 (Byte(str[i+3])-Byte('0')));
               Inc(i, 3);
             end;
             else Inc(i);
           end;
        end;
      end else
        Result := Result + str[i];

      Inc(i);
    end;
  end;

  procedure DrawPage(const cmd: string);
  var
    ss: TStringList;
    params, texts: TObjectList;
    i, j, l, Tf_index: integer;
    s, s1, s2, cm, Tf: string;
    sx, sy, x1, y1, x2, y2: double;
    Tl, Tc, Tw, Tfs, Th, Trise: double;
    sp, sp1: PChar;
    xy: TDblXY;
    Tlm, Tm, m1, m2: TMatrix;
    poly: boolean;
  begin
    ss:= TStringList.Create;
    try
      params := TObjectList.Create(True);
      texts := TObjectList.Create(False);
      try
        sp := PChar(cmd);
        Tl:=0; Tc := 0; Tw := 0; Trise:=0; Th:=1;
        Tf := ''; Tf_index := -1;
        LPO.LuaPrint.Canvas.Pen.JoinStyle:= pjsMiter;
        LPO.LuaPrint.Canvas.Pen.EndCap:= pecFlat;
        while sp^ <> #0 do begin
          cm := TokenStr(sp);
          if cm = 'BT' then begin
            Tlm[1][1] := 1; Tlm[1][2]:=0;Tlm[1][3]:=0;
            Tlm[2][1] := 0; Tlm[2][2]:=1;Tlm[2][3]:=0;
            Tlm[3][1] := 0; Tlm[3][2]:=0;Tlm[3][3]:=1;
            Tm := Tlm;
            ss.Clear;
          end else if cm = 'ET' then begin
            ss.Clear;
          end else if cm = 'q' then begin
            LPO.LuaPrint.PushCanvas;
            LPO.LuaPrint.AddOrder(PRUN_NAME + '.PushCanvas()');
            ss.Clear;
          end else if cm = 'Q' then begin
            LPO.LuaPrint.PopCanvas;
            LPO.LuaPrint.AddOrder(PRUN_NAME + '.PopCanvas()');
            ss.Clear;
          end else if cm = 'm' then begin
            xy := TDblXY.Create;
            xy.tst:= tstM;
            xy.x:= StrToFloat(ss[ss.Count-2]);
            xy.y := StrToFloat(ss[ss.Count-1]);
            sx := xy.x;
            sy := xy.y;
            params.Add(xy);
            ss.Clear;
          end else if cm = 'l' then begin
            if TDblXY(params[params.Count-1]).tst = tstBez then begin
              xy := TDblXY.Create;
              xy.tst:= tstM;
              xy.x:= TDblXY(params[params.Count-1]).x;
              xy.y := TDblXY(params[params.Count-1]).y;
              params.Add(xy);
            end;
            xy := TDblXY.Create;
            xy.x:= StrToFloat(ss[ss.Count-2]);
            xy.y := StrToFloat(ss[ss.Count-1]);
            params.Add(xy);
            ss.Clear;
          end else if cm = 'c' then begin
            if TDblXY(params[params.Count-1]).tst = tstNone then begin
              xy := TDblXY.Create;
              xy.tst:= tstM;
              xy.x:= TDblXY(params[params.Count-1]).x;
              xy.y := TDblXY(params[params.Count-1]).y;
              params.Add(xy);
            end;
            xy := TDblXY.Create;
            xy.tst:= tstBez;
            xy.x:= StrToFloat(ss[ss.Count-6]);
            xy.y := StrToFloat(ss[ss.Count-5]);
            params.Add(xy);
            xy := TDblXY.Create;
            xy.tst:= tstBez;
            xy.x:= StrToFloat(ss[ss.Count-4]);
            xy.y := StrToFloat(ss[ss.Count-3]);
            params.Add(xy);
            xy := TDblXY.Create;
            xy.tst:= tstBez;
            xy.x:= StrToFloat(ss[ss.Count-2]);
            xy.y := StrToFloat(ss[ss.Count-1]);
            params.Add(xy);
            ss.Clear;
          end else if cm = 'h' then begin
            if (params.Count > 0) and
             ((TDblXY(params[params.Count-1]).x <> sx) or
              (TDblXY(params[params.Count-1]).y <> sy)) then begin
              if TDblXY(params[params.Count-1]).tst = tstBez then begin
                xy := TDblXY.Create;
                xy.tst:= tstM;
                xy.x:= TDblXY(params[params.Count-1]).x;
                xy.y := TDblXY(params[params.Count-1]).y;
                params.Add(xy);
              end;
              xy := TDblXY.Create;
              xy.x:= sx;
              xy.y := sy;
              params.Add(xy);
            end;
            ss.Clear;
          end else if cm = 're' then begin
            xy := TDblXY.Create;
            xy.tst:= tstRe;
            xy.x:= StrToFloat(ss[ss.Count-4]);
            xy.y := StrToFloat(ss[ss.Count-3]);
            params.Add(xy);
            xy := TDblXY.Create;
            xy.x:= StrToFloat(ss[ss.Count-2]);
            xy.y := StrToFloat(ss[ss.Count-1]);
            params.Add(xy);
            ss.Clear;
            ////////////////////////////////
          end else if cm = 'J' then begin
            case Trunc(StrToFloat(ss[ss.Count-1])) of
              1: i:= Integer(pecRound);
              2: i:= Integer(pecSquare);
              else i:= Integer(pecFlat);
            end;
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_EndCap(%d)', [i]));
            LPO.LuaPrint.Canvas.Pen.EndCap:= TPenEndCap(i);
            ss.Clear;
          end else if cm = 'j' then begin
            case Trunc(StrToFloat(ss[ss.Count-1])) of
              1: i:= Integer(pjsRound);
              2: i:= Integer(pjsBevel);
              else i:= Integer(pjsMiter);
            end;
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_JoinStyle(%d)', [i]));
            LPO.LuaPrint.Canvas.Pen.JoinStyle:= TPenJoinStyle(i);
            ss.Clear;
            ////////////////////////////////
          end else if cm = 'G' then begin
            x1 := StrToFloat(ss[ss.Count-1]);
            i := RGBToColor(Trunc(255*x1), Trunc(255*x1), Trunc(255*x1));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)', [i]));
            LPO.LuaPrint.Canvas.Pen.Color:= i;
            ss.Clear;
          end else if cm = 'g' then begin
            x1 := StrToFloat(ss[ss.Count-1]);
            i := RGBToColor(Trunc(255*x1), Trunc(255*x1), Trunc(255*x1));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_color(%d)', [i]));
            LPO.LuaPrint.Canvas.Brush.Color:= i;
            ss.Clear;
          end else if cm = 'RG' then begin
            i := RGBToColor(
             Trunc(255*StrToFloat(ss[ss.Count-3])),
             Trunc(255*StrToFloat(ss[ss.Count-2])),
             Trunc(255*StrToFloat(ss[ss.Count-1])));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)', [i]));
            LPO.LuaPrint.Canvas.Pen.Color:= i;
            ss.Clear;
          end else if cm = 'rg' then begin
            i := RGBToColor(
             Trunc(255*StrToFloat(ss[ss.Count-3])),
             Trunc(255*StrToFloat(ss[ss.Count-2])),
             Trunc(255*StrToFloat(ss[ss.Count-1])));
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_color(%d)', [i]));
            LPO.LuaPrint.Canvas.Brush.Color:= i;
            ss.Clear;
          ////////////////////////////////
          end else if cm = 'w' then begin
            i := Trunc(StrToFloat(ss[ss.Count-1]) * Rate);
            LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_width(%d)', [i]));
            ss.Clear;
          ////////////////////////////////
          end else if cm = 'n' then begin
            // End the path object without filling or stroking it.
            params.Clear;
            ss.Clear;
          end else if (cm = 'S') or (cm = 's') or (cm = 'f') or (cm = 'b') or
           (cm = 'f*') or (cm = 'b*') or (cm = 'B') or (cm = 'B*') then begin
            if params.Count > 0 then begin
              if ((cm = 's') or (cm = 'b')) and
               ((TDblXY(params[params.Count-1]).x <> sx) or
                (TDblXY(params[params.Count-1]).y <> sy)) then begin
                if TDblXY(params[params.Count-1]).tst = tstBez then begin
                  xy := TDblXY.Create;
                  xy.tst:= tstM;
                  xy.x:= TDblXY(params[params.Count-1]).x;
                  xy.y := TDblXY(params[params.Count-1]).y;
                  params.Add(xy);
                end;
                xy := TDblXY.Create;
                xy.x:= sx;
                xy.y := sy;
                params.Add(xy);
              end;

              i := 0;
              poly:= False;
              while i < params.Count do begin
                case TDblXY(params[i]).tst of
                  tstM: begin
                    poly := True;
                    case TDblXY(params[i+1]).tst of
                      tstBez: begin
                        s := '';
                        repeat
                          sx := TDblXY(params[i]).x * Rate;
                          sy := (PageH - TDblXY(params[i]).y) * Rate;
                          s := s + Format('%d,%d,', [Trunc(sx), Trunc(sy)]);
                          Inc(i);
                        until (i >= params.Count) or
                         (TDblXY(params[i]).tst <> tstBez);
                        Delete(s, Length(s), 1);
                        LPO.LuaPrint.AddOrder(
                         Format(PRUN_NAME + '.AddBezierPoint(%s)', [s]));
                      end;
                      else begin
                        s := '';
                        repeat
                          sx := TDblXY(params[i]).x * Rate;
                          sy := (PageH - TDblXY(params[i]).y) * Rate;
                          s := s + Format('%d,%d,', [Trunc(sx), Trunc(sy)]);
                          Inc(i);
                        until (i >= params.Count) or
                         (TDblXY(params[i]).tst <> tstNone);
                        Delete(s, Length(s), 1);
                        LPO.LuaPrint.AddOrder(
                         Format(PRUN_NAME + '.AddPolyPoint(%s)', [s]));
                      end;
                    end;
                  end;
                  tstRe: begin
                    x1 := TDblXY(params[i]).x;
                    y1 := TDblXY(params[i]).y;
                    x2 := TDblXY(params[i+1]).x;
                    y2 := TDblXY(params[i+1]).y;
                    s:= Format('%d,%d,%d,%d',
                     [Trunc(x1 * Rate), Trunc((PageH-y1) * Rate),
                      Trunc((x1+x2) * Rate), Trunc((PageH-y1-y2) * Rate)]);

                    if (cm = 's') or (cm = 'S') then begin
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                       [Integer(bsClear)]));
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s]));
                    end else if (cm = 'b') or (cm = 'b*')
                     or (cm = 'B') or (cm = 'B*') then begin
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                       [Integer(bsSolid)]));
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s]));
                    end else begin
                      LPO.LuaPrint.AddOrder(PRUN_NAME + '.PushCanvas()');
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_style(%d)',
                       [Integer(psSolid)]));
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.pen_color(%d)',
                       [LPO.LuaPrint.Canvas.Brush.Color]));
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                       [Integer(bsSolid)]));
                      LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.rectangle(%s)', [s]));
                      LPO.LuaPrint.AddOrder(PRUN_NAME + '.PopCanvas()');
                    end;
                    Inc(i, 2);
                  end;
                end;
              end; // while

              if poly then begin
                if (cm = 's') or (cm = 'S') then begin
                  LPO.LuaPrint.AddOrder(PRUN_NAME + '.polyline()');
                end else if (cm = 'b') or (cm = 'b*')
                 or (cm = 'B') or (cm = 'B*') then begin
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                   [Integer(bsSolid)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polygon(%d)',
                   [Integer((cm = 'b') or (cm = 'B'))]));
                end else begin
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
                   [Integer(bsSolid)]));
                  LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.polyfill(%d)',
                   [Integer(cm = 'f')]));
                end;
                LPO.LuaPrint.AddOrder(PRUN_NAME + '.AddPolyPoint()');
              end;
              params.Clear;
            end;
            ss.Clear;
          ////////////////////////////////////////////////////////////////////////
          end else if cm = 'Tm' then begin
            Tm[1][1] := StrToFloat(ss[ss.Count-6]); // a
            Tm[1][2] := StrToFloat(ss[ss.Count-5]); // b
            Tm[1][3] := 0;
            Tm[2][1] := StrToFloat(ss[ss.Count-4]); // c
            Tm[2][2] := StrToFloat(ss[ss.Count-3]); // d
            Tm[2][3] := 0;
            Tm[3][1] := StrToFloat(ss[ss.Count-2]); // e = x
            Tm[3][2] := StrToFloat(ss[ss.Count-1]); // f = y
            Tm[3][3] := 1;
            Tlm := Tm;
            ss.Clear;
          end else if cm = 'Td' then begin
            m1[1][1] := 1; m1[1][2] := 0; m1[1][3] := 0;
            m1[2][1] := 0; m1[2][2] := 1; m1[2][3] := 0;
            m1[3][1] := StrToFloat(ss[ss.Count-2]);
            m1[3][2] := StrToFloat(ss[ss.Count-1]);
            m1[3][3] := 1;
            Tlm := matrixmul(m1, Tlm);
            Tm := Tlm;
            ss.Clear;
          end else if cm = 'TD' then begin
            m1[1][1] := 1; m1[1][2] := 0; m1[1][3] := 0;
            m1[2][1] := 0; m1[2][2] := 1; m1[2][3] := 0;
            m1[3][1] := StrToFloat(ss[ss.Count-2]);
            m1[3][2] := StrToFloat(ss[ss.Count-1]);
            m1[3][3] := 1;
            Tlm := matrixmul(m1, Tlm);
            Tm := Tlm;
            Tl := m1[3][2];
            ss.Clear;
          end else if cm = 'T*' then begin
            m1[1][1] := 1; m1[1][2] := 0; m1[1][3] := 0;
            m1[2][1] := 0; m1[2][2] := 1; m1[2][3] := 0;
            m1[3][1] := 0;
            m1[3][2] := Tl;
            m1[3][3] := 1;
            Tlm := matrixmul(m1, Tlm);
            Tm := Tlm;
            ss.Clear;
          end else if cm = 'TL' then begin
            Tl := StrToFloat(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Tc' then begin
            Tc := StrToFloat(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Tw' then begin
            Tw := StrToFloat(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Tz' then begin
            Th := StrToFloat(ss[ss.Count-1]) / 100;
            ss.Clear;
          end else if cm = 'Tr' then begin
          //  Tmode := StrToInt(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Ts' then begin
            Trise := StrToFloat(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Tf' then begin
            Tf := ss[ss.Count-2];
            Tf_index := fonts.IndexOf(Tf);
            if Tf_index >= 0 then begin
              s := TFontObj(fonts.Objects[Tf_index]).font_name;
              LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.font_name("%s")', [s]));
              LPO.LuaPrint.Canvas.Font.Name:= s;
            end;
            Tfs := StrToFloat(ss[ss.Count-1]);
            ss.Clear;
          end else if cm = 'Tj' then begin
            s := ss[ss.Count-1];
            texts.Clear;
            while True do begin
              if s[1] = '(' then begin
                Delete(s, 1, 1); Delete(s, Length(s), 1);
                s := LStrObj2Str(s);
                if s = '' then break;
                if (Tf_index >= 0) and (fonts.Objects[Tf_index] <> nil)
                 and (TFontObj(fonts.Objects[Tf_index]).l.Count > 0) then
                  TFontObj(fonts.Objects[Tf_index]).ascii_w(s, texts);
              end else if s[1] = '<' then begin
                Delete(s, 1, 1); Delete(s, Length(s), 1);
                if (Tf_index >= 0) and (fonts.Objects[Tf_index] <> nil)
                 and (TFontObj(fonts.Objects[Tf_index]).l.Count > 0) then
                  TFontObj(fonts.Objects[Tf_index]).cid2utf8(s, texts);
                if texts.Count = 0 then Break;
                s := '';
              end;

              m1[1][1] := Tfs * Th; m1[1][2]:=0;   m1[1][3]:=0;
              m1[2][1]:=0;          m1[2][2]:=Tfs; m1[2][3]:=0;
              m1[3][1]:= 0;         m1[3][2]:= Trise; m1[3][3]:= 1;
              m2 := matrixmul(m1, Tm);
              LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.font_height(%d)',
               [-Trunc(m2[2][2]*Rate)]));
              LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
               [Integer(bsClear)]));
              LPO.LuaPrint.Canvas.Font.Height:= -Trunc(m2[2][2]);

              if s = '' then begin
                l := texts.Count;
              end else begin
                l := UTF8Length(s);
              end;
              for j := 1 to l do begin
                if s = '' then begin
                  s2 := TCidObj(texts[j-1]).utf8;
                end else begin
                  s2 := UTF8Copy(s, j, 1);
                end;
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.textout(%d,%d,"%s")',
                 [Trunc(m2[3][1]*Rate), Trunc((PageH-m2[3][2]-m2[2][2])*Rate), s2]));
                m1[1][1]:= 1; m1[1][2]:=0; m1[1][3]:=0;
                m1[2][1]:= 0; m1[2][2]:=1; m1[2][3]:=0;
                if (texts.Count > 0) and (TCidObj(texts[j-1]).width <> 0) then begin
                  m1[3][1]:= (TCidObj(texts[j-1]).width*Tfs/1000+Tc)*Th;
                  if s2 = ' ' then m1[3][1]:= m1[3][1] + Tw*Th;
                  m1[3][2]:= 0; m1[3][3]:= 1;
                  Tm := matrixmul(m1, Tm);
                end else begin
                  m1[3][1]:= Tc*Th;
                  if s2 = ' ' then m1[3][1]:= m1[3][1] + Tw*Th;
                  m1[3][2]:= 0; m1[3][3]:= 1;
                  Tm := matrixmul(m1, Tm);
                  Tm[3][1]:= Tm[3][1] + LPO.LuaPrint.Canvas.TextWidth(s2)*Th;
                end;

                m1[1][1] := Tfs * Th; m1[1][2]:=0;   m1[1][3]:=0;
                m1[2][1]:=0;          m1[2][2]:=Tfs; m1[2][3]:=0;
                m1[3][1]:= 0;         m1[3][2]:= Trise; m1[3][3]:= 1;
                m2 := matrixmul(m1, Tm);
              end;
              break;
            end;
            ss.Clear;
          end else if cm = 'TJ' then begin
            s := ss[ss.Count-1];
            sp1 := PChar(s) + 1;
            while sp1^ <> ']' do begin
              s1 := TokenStr(sp1);
              texts.Clear;
              if s1[1] = '<' then begin
                Delete(s1, 1, 1); Delete(s1, Length(s1), 1);
                if (Tf_index >= 0) and (fonts.Objects[Tf_index] <> nil)
                 and (TFontObj(fonts.Objects[Tf_index]).l.Count > 0) then
                  TFontObj(fonts.Objects[Tf_index]).cid2utf8(s1, texts);
                if texts.Count = 0 then continue;
                s1 := '';
              end else if s1[1] = '(' then begin
                Delete(s1, 1, 1); Delete(s1, Length(s1), 1);
                s1 := LStrObj2Str(s1);
                if s1 = '' then continue;
                if (Tf_index >= 0) and (fonts.Objects[Tf_index] <> nil)
                 and (TFontObj(fonts.Objects[Tf_index]).l.Count > 0) then
                  TFontObj(fonts.Objects[Tf_index]).ascii_w(s1, texts);
              end else begin
                m1[1][1]:= 1; m1[1][2]:=0; m1[1][3]:=0;
                m1[2][1]:= 0; m1[2][2]:=1; m1[2][3]:=0;
                m1[3][1]:= -StrToFloat(s1) * Tfs * Th / 1000;
                m1[3][2]:= 0; m1[3][3]:= 1;
                Tm := matrixmul(m1, Tm);
                continue;
              end;

              m1[1][1] := Tfs * Th; m1[1][2]:=0;   m1[1][3]:=0;
              m1[2][1]:=0;          m1[2][2]:=Tfs; m1[2][3]:=0;
              m1[3][1]:= 0;         m1[3][2]:= Trise; m1[3][3]:= 1;
              m2 := matrixmul(m1, Tm);
              LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.font_height(%d)',
               [-Trunc(m2[2][2]*Rate)]));
              LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.brush_style(%d)',
               [Integer(bsClear)]));
              LPO.LuaPrint.Canvas.Font.Height:= -Trunc(m2[2][2]);
              if s1 = '' then begin
                l := texts.Count;
              end else begin
                l := UTF8Length(s1);
              end;
              for j := 1 to l do begin
                if s1 = '' then begin
                  s2 := TCidObj(texts[j-1]).utf8;
                end else begin
                  s2 := UTF8Copy(s1, j, 1);
                end;
                LPO.LuaPrint.AddOrder(Format(PRUN_NAME + '.textout(%d,%d,"%s")',
                 [Trunc(m2[3][1]*Rate), Trunc((PageH-m2[3][2]-m2[2][2])*Rate), s2]));
                m1[1][1]:= 1; m1[1][2]:=0; m1[1][3]:=0;
                m1[2][1]:= 0; m1[2][2]:=1; m1[2][3]:=0;
                if (texts.Count > 0) and (TCidObj(texts[j-1]).width <> 0) then begin
                  m1[3][1]:= (TCidObj(texts[j-1]).width*Tfs/1000+Tc)*Th;
                  if s2 = ' ' then m1[3][1]:= m1[3][1] + Tw*Th;
                  m1[3][2]:= 0; m1[3][3]:= 1;
                  Tm := matrixmul(m1, Tm);
                end else begin
                  m1[3][1]:= Tc*Th;
                  if s2 = ' ' then m1[3][1]:= m1[3][1] + Tw*Th;
                  m1[3][2]:= 0; m1[3][3]:= 1;
                  Tm := matrixmul(m1, Tm);
                  Tm[3][1]:= Tm[3][1] + LPO.LuaPrint.Canvas.TextWidth(s2)*Th;
                end;

                m1[1][1] := Tfs * Th; m1[1][2]:=0;   m1[1][3]:=0;
                m1[2][1]:=0;          m1[2][2]:=Tfs; m1[2][3]:=0;
                m1[3][1]:= 0;         m1[3][2]:= Trise; m1[3][3]:= 1;
                m2 := matrixmul(m1, Tm);
              end;
            end;
            ss.Clear;
          end else
            ss.Add(cm);
        end; // while
      finally
        params.Free;
        texts.Free;
      end;
    finally
      ss.Free;
    end;
  end;

var
  pdfr: TPDFReader;
  pageobj, pdfobj, pdfobj2: TPDFObj;
  i, j, k: integer;
  s, s1: string;
  sp1: PChar;
  sl: TStringList; // for debug
  fs: TFileStream; // for debug
begin
  sl:= TStringList.Create;
  pdfr := TPDFReader.Create(stream);
  fonts:= TStringList.Create;
  try
    pageobj := pdfr.FindPageObj(page);
    if pageobj = nil then Exit;

    PageW := 0;
    PageH := 0;
    s := pdfr.GetVal('/MediaBox', pageobj.val);
    if s <> '' then begin
      sp1 := PChar(s) + 1;
      TokenStr(sp1);
      TokenStr(sp1);
      PageW := StrToFloat(TokenStr(sp1));
      PageH := StrToFloat(TokenStr(sp1));

      if PageW <> 0 then begin
        RateW := (x2 - x1) / PageW;
      end else
        RateW := 1;

      if PageH <> 0 then begin
        RateH := (y2 - y1) / PageH;
      end else
        RateH := 1;

      Rate := RateW;
      if RateW > RateH then Rate := RateH;
    end;

    s := pdfr.GetVal('/Resources', pageobj.val);
    s := pdfr.GetVal('/Font', s);
    sp1 := PChar(s);
    while sp1^ <> #0 do begin
      if sp1^ = '/' then begin
        fonts.Add('');
        while not(sp1^ in [#0, ' ']) do begin
          fonts[fonts.Count-1] := fonts[fonts.Count-1] + sp1^;
          Inc(sp1);
        end;
        i := StrToInt(TokenStr(sp1));
        fonts.Objects[fonts.Count-1]:= TObject(i);
      end;
      Inc(sp1);
    end;

    for i := 0 to fonts.Count-1 do begin
      pdfobj := pdfr.FindObj(IntToStr(Integer(fonts.Objects[i])));
      fonts.Objects[i] := nil;
      if pdfobj <> nil then begin
        fonts.Objects[i] := TFontObj.Create;
        sp1 := strpos(PChar(pdfobj.val), '/BaseFont');
        if sp1 <> nil then begin
          Inc(sp1, 9);
          while sp1^ <> '/' do Inc(sp1);
          Inc(sp1);
          s := '';
          while not(sp1^ in ['/', #$0a, #$0d, ' ']) do begin
            s := s + sp1^;
            Inc(sp1);
          end;
          j := Pos('+', s);
          s := Copy(s, j+1, Length(s));
          if FontNameTable.Count > 0 then begin
            s1 := FontNameTable.Values[s];
            if s1 <> '' then s := s1;
          end;
          LPO.GetFontName(s);
          TFontObj(fonts.Objects[i]).font_name := s;
        end;

        s := pdfr.GetVal('/ToUnicode', pdfobj.val);
        if s <> ''then begin
          TFontObj(fonts.Objects[i]).make_l(s);
        end else begin
          TFontObj(fonts.Objects[i]).b:= 4;
        end;

        s := pdfr.GetVal('/FontDescriptor', pdfobj.val);
        if s = '' then begin
          s := pdfr.GetVal('/DescendantFonts', pdfobj.val);
          if s <> '' then begin
            sp1 := PChar(s) + 1;
            pdfobj2 := pdfr.FindObj(TokenStr(sp1));
            s := pdfr.GetVal('/FontDescriptor', pdfobj2.val);
          end;
        end;
        if s <> '' then begin
          sp1 := strpos(PChar(s), '/FontFile');
          if sp1 <> nil then begin
            Inc(sp1, 9);
            if sp1^ <> ' ' then Inc(sp1);
            Inc(sp1);
            TFontObj(fonts.Objects[i]).font_file_no := TokenStr(sp1);
          end;

          s1 := pdfr.GetVal('/Flags', s);
          if StrToIntDef(s1, 0) and 1 <> 0 then begin
            //FixedPitch
            for j := 0 to TFontObj(fonts.Objects[i]).l.Count-1 do begin
              if Length(TCidObj(TFontObj(fonts.Objects[i]).l.Objects[j]).utf8) = 1 then
                TCidObj(TFontObj(fonts.Objects[i]).l.Objects[j]).width := 500
              else
                TCidObj(TFontObj(fonts.Objects[i]).l.Objects[j]).width := 1000;
            end;
          end else begin
            for j := 0 to TFontObj(fonts.Objects[i]).l.Count-1 do
              TCidObj(TFontObj(fonts.Objects[i]).l.Objects[j]).width := 1000;
          end;
        end;

        s := pdfr.GetVal('/Widths', pdfobj.val);
        if s <> '' then begin
          sp1 := PChar(s) + 1;
          j := StrToIntDef(pdfr.GetVal('/FirstChar', pdfobj.val), 0);
          while sp1^ <> ']' do begin
            s1 := IntToHex(j, TFontObj(fonts.Objects[i]).b);
            k := TFontObj(fonts.Objects[i]).l.IndexOf(s1);
            if k < 0 then begin
              k := TFontObj(fonts.Objects[i]).l.AddObject(s1, TCidObj.Create);
            end;
            TCidObj(TFontObj(fonts.Objects[i]).l.Objects[k]).width := StrToInt(TokenStr(sp1));
            Inc(j);
          end;
        end;
      end;
    end;

    s := pdfr.GetVal('/Contents', pageobj.val);
    if s <> '' then begin
      if s[1] = '[' then begin
        s1 := s;
        sp1 := PChar(s1) + 1;
        s := '';
        while sp1^ <> ']' do begin
          pdfobj := pdfr.FindObj(TokenStr(sp1));
          TokenStr(sp1); TokenStr(sp1);
          s := s + pdfobj.stream;
        end;
      end;
      //sl.Text:=s; sl.SaveToFile('1.txt');
      //fs:= TFileStream.Create('3.txt', fmOpenWrite);
      //fs.WriteBuffer(s[1], Length(s));
      //fs.Free;
      DrawPage(s);
    end;
  finally
    for i := 0 to fonts.Count-1 do fonts.Objects[i].Free;
    fonts.Free;
    pdfr.Free;
    sl.Free;
  end;
end;

initialization
  CreateFontNameTable;
finalization
  FontNameTable.Free;
end.

