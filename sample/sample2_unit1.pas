unit sample2_Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation
uses
  Lua53, l4l_activex;

{$R *.lfm}

{ TForm1 }

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
    lua_register(L, 'CreateActiveXObject', @CreateActiveXObject);
    try
      s:= Memo1.Text;
      if luaL_loadbuffer(L, PChar(s), Length(s), 'sample') <> 0 then
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

end.

