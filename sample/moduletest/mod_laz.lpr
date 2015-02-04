library mod_laz;

{$mode objfpc}{$H+}

uses
  Classes
  { you can add units after this }
  , lua53, l4l_object;

type

  { TTestObject }

  TTestObject = class(TLuaObject)
  private
    function GetTest1: string;
    function GetTest2: string;
  protected
  public
  published
    property l4l_test1: string read GetTest1;
    property l4l_test2: string read GetTest2;
  end;

{ TTestObject }

function TTestObject.GetTest1: string;
begin
  Result:= 'TEST';
end;

function TTestObject.GetTest2: string;
begin
  Result:= 'テスト';
end;

function WhoAreYou(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, 'I am Lazarus.');
  Result:= 1;
end;

function DareDesuka(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, '私は Lazarus です。');
  Result:= 1;
end;

function luaopen_mod_laz(L: Plua_State): integer; cdecl;
const
  arr: array[1..3] of luaL_Reg = (
   (name:'WhoAreYou'; func:@WhoAreYou), (name:'DareDesuka'; func:@DareDesuka),
   (name:nil; func:nil)
  );
begin
  luaL_newlib(L, arr);
  l4l_SetLuaObject(TTestObject.Create(L));
  Result:= 1;
end;

exports
  luaopen_mod_laz;

begin
end.

