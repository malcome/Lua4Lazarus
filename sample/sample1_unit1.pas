{
  Lua4Lazarus Sample1: User Define Object.
}
unit sample1_Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, dbf, Forms, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Dbf1: TDbf;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation
uses
  Lua53, l4l_object, l4l_strings, l4l_myutils;

{$R *.lfm}

type

  { TLuaDbf }

  TLuaDbf = class(TLuaObject)
  private
    function GetActive: boolean;
    procedure SetActive(const AValue: boolean);
    function GetEof: boolean;
  protected
  public
    constructor Create(L : Plua_State); override;
    destructor Destroy; override;
  published
    function l4l_Next: integer;
    function l4l_FieldByName: integer;
    property l4l_Active: boolean read GetActive write SetActive;
    property l4l_eof: boolean read GetEof;
  end;

function CreateDbfObject(L : Plua_State) : Integer; cdecl;
begin
  l4l_PushLuaObject(TLuaDbf.Create(L));
  Result := 1;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  dbf1.FilePath := 'db/'; // \lazarus\components\lazreport\samples\editor\db
  dbf1.TableName := 'disco.dbf';
end;

function print_func(L : Plua_State) : Integer; cdecl;
var
  i, c: integer;
begin
  c:= lua_gettop(L);
  for i:= 1 to c do
    Form1.Memo2.Lines.Add(lua_tostring(L, i));
  Form1.Memo2.SelStart:= 0;
  Form1.Memo2.SelLength:= 0;
  Result := 0;
end;

function Alloc({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t) : Pointer; cdecl;
begin
  try
    Result:= ptr;
    ReallocMem(Result, nSize);
  except
    Result:= nil;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  L: Plua_State;
  s: string;
begin
  Memo2.Clear;
  L:= lua_newstate(@alloc, nil);
  try
    luaL_openlibs(L);
    lua_register(L, 'print', @print_func);
    lua_register(L, 'CreateStringsObject', @CreateStringsObject);
    lua_register(L, 'CreateDbfObject', @CreateDbfObject);
    //l4l_PushLuaObject(TLuaDbf.Create(L)); lua_setglobal(L, 'dbf'); // set global value.
    l4l_PushLuaObject(TLuaMyUtilsObject.Create(L)); lua_setglobal(L, 'MyUtils'); // set global value.
    try
      s:= Memo1.Text;
      if luaL_loadbuffer(L, PChar(s), Length(s), 'sample1') <> 0 then
        Raise Exception.Create('');
      if lua_pcall(L, 0, 0, 0) <> 0 then
        Raise Exception.Create('');
    except
      Form1.Memo2.Lines.Add(lua_tostring(L, -1));
      Form1.Memo2.SelStart:= 0;
      Form1.Memo2.SelLength:= 0;
    end;
  finally
    lua_close(L);
  end;
end;

{ TLuaDbf }

constructor TLuaDbf.Create(L: Plua_State);
begin
  inherited Create(L);
  Form1.Dbf1.Close;
end;

destructor TLuaDbf.Destroy;
begin
  Form1.Dbf1.Close;
  inherited Destroy;
end;

function TLuaDbf.l4l_Next: integer;
begin
  Form1.Dbf1.Next;
  Result := 0;
end;

function TLuaDbf.l4l_FieldByName: integer;
var
  s: string;
  f: TField;
begin
  s:= lua_tostring(LS, 1);
  f:= Form1.Dbf1.FieldByName(s);
  case f.DataType of
    ftSmallint, ftInteger, ftWord: lua_pushinteger(LS, f.AsInteger);
    ftFloat: lua_pushnumber(LS, f.AsFloat);
    ftBoolean: lua_pushboolean(LS, f.AsBoolean);
    else
      lua_pushstring(LS, f.AsString);
  end;
  Result := 1;
end;

function TLuaDbf.GetActive: boolean;
begin
  Result:=Form1.Dbf1.Active;
end;

procedure TLuaDbf.SetActive(const AValue: boolean);
begin
  Form1.Dbf1.Active:=AValue;
end;

function TLuaDbf.GetEof: boolean;
begin
  Result:=Form1.Dbf1.EOF;
end;

end.

