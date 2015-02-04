{
  Lua4Lazarus

    sample:TLuaMyUtilsObject

    License: New BSD
    Copyright(c)2010- Malcome@Japan All rights reserved.

}
unit l4l_myutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, lua53, l4l_object;

type

  { TLuaMyUtilsObject }

  TLuaMyUtilsObject  = class(TLuaObject)
  private
    function GetNow: string;
  protected
  public
    constructor Create(L : Plua_State); override;
    destructor Destroy; override;
  published
    function l4l_Sleep: integer;
    function l4l_ShowMessage: integer;
    function l4l_Utf8ToWide: integer;
    function l4l_WideToUtf8: integer;
    function l4l_Utf8ToAnsi: integer;
    function l4l_AnsiToUtf8: integer;
    property l4l_Now: string read GetNow;
  end;

function CreateMyUtilsObject(L : Plua_State) : Integer; cdecl;

implementation

{ TLuaMyUtilsObject }

constructor TLuaMyUtilsObject.Create(L: Plua_State);
begin
  inherited Create(L);
end;

destructor TLuaMyUtilsObject.Destroy;
begin
  inherited Destroy;
end;

function TLuaMyUtilsObject.l4l_Sleep: integer;
begin
  SysUtils.sleep(lua_tointeger(LS, 1));
  Result := 0;
end;

function TLuaMyUtilsObject.l4l_ShowMessage: integer;
begin
  Dialogs.ShowMessage(lua_tostring(LS, 1));
  Result := 0;
end;

function TLuaMyUtilsObject.l4l_Utf8ToWide: integer;
var
  ws: WideString;
begin
  ws := UTF8Decode(lua_tostring(LS, 1));
  lua_pushlstring(LS, PChar(ws), Length(ws) shl 1);
  Result := 1;
end;

function TLuaMyUtilsObject.l4l_WideToUtf8: integer;
var
  l: integer;
  ws: WideString;
  p: PChar;
begin
  p := lua_tolstring(LS, 1, @l);
  SetLength(ws, l shr 1);
  Strlcopy(PChar(ws), p, l);
  lua_pushstring(LS, PChar(UTF8Encode(ws)));
  Result := 1;
end;

function TLuaMyUtilsObject.l4l_Utf8ToAnsi: integer;
begin
  lua_pushstring(LS, PChar(System.Utf8ToAnsi(lua_tostring(LS, 1))));
  Result := 1;
end;

function TLuaMyUtilsObject.l4l_AnsiToUtf8: integer;
begin
  lua_pushstring(LS, PChar(System.AnsiToUtf8(lua_tostring(LS, 1))));
  Result := 1;
end;

function TLuaMyUtilsObject.GetNow: string;
begin
  Result:= DatetimeToStr(SysUtils.Now, DefaultFormatSettings);
end;

function CreateMyUtilsObject(L : Plua_State) : Integer; cdecl;
begin
  l4l_PushLuaObject(TLuaMyUtilsObject.Create(L));
  Result := 1;
end;

end.

