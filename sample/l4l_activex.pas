{
  Lua4Lazarus

    ActiveXObject

    License: New BSD
      Copyright(c)2010- Malcome@Japan All rights reserved.

    Note:

    ToDo:
      Event handling.

}
unit l4l_activex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

function CreateActiveXObject(L : Plua_State) : Integer; cdecl;
function CreateRefType(L : Plua_State) : Integer; cdecl;

implementation
uses
  Windows, ComObj, ActiveX, variants, varutils, l4l_object;

const
  FIELD_ID = '___IDispatch___';
  FIELD_FN = '___FuncName___';
  FIELD_ID_PARENT = '___IDispatchParent___';

function call(L : Plua_State) : Integer; cdecl; forward;
procedure DoCreateActiveXObject(L : Plua_State; id: IDispatch); forward;

procedure ChkErr(L : Plua_State; Val: HResult; const prop: string='');
var
  s: string;
begin
  if not(Succeeded(Val)) then begin
    case val of
      DISP_E_MEMBERNOTFOUND: s:= 'Member Not Found';
      DISP_E_TYPEMISMATCH: s:= 'Type Mismatch';
      DISP_E_UNKNOWNNAME: s:= 'Unknown Name';
      DISP_E_BADPARAMCOUNT: s:= 'Bad Param Count';
      E_INVALIDARG: s:= 'Invaid Arg'
      else s:= 'ActiveX Error';
    end;
    s:= s + Format('(%x)', [Val]);
    if prop <> '' then s := s + ' in "' + prop + '".';
    luaL_error(L, PChar(s));
  end;
end;

procedure Lua2Var(L : Plua_State; va: POleVariant; index: integer; toWideStr: boolean = True);
var
  i, j: integer;
  p: POleVariant;
  obj: TLuaObject;
  hp: TVarArrayBoundArray;
begin
  VariantInit(TVarData(va^));
  case lua_type(L, index) of
    LUA_TNIL: va^:= VarAsType(va^, varNull);
    LUA_TBOOLEAN: va^:= lua_toboolean(L, index);
    LUA_TNUMBER: begin
      if lua_isinteger(L, index) then
        va^:= lua_tointeger(L, index)
      else
        va^:= lua_tonumber(L, index);
    end;
    LUA_TSTRING: begin
      if toWideStr then
        va^:= UTF8Decode(lua_tostring(L, index))
      else
        va^:= lua_tostring(L, index);
    end;
    LUA_TTABLE: begin
      lua_getmetatable(L, index);
      lua_getfield(L, -1, FIELD_ID);
      if lua_isnil(L, -1) then begin
        obj:= l4l_toobject(L, index);
        if not Assigned(obj) then begin
          j:= lua_rawlen(L, index);
          if j > 0 then begin
            //va^:= VarArrayCreate([0, j-1], varVariant);
            hp[0].LowBound:= 0;
            hp[0].ElementCount:= j;
            TVarData(va^).vType:= varVariant or varArray;
            TVarData(va^).vArray:= SafeArrayCreate(varVariant, 1, hp);
            SafeArrayAccessData(TVarData(va^).vArray, p{%H-});
            for i:= 1 to j do begin
              lua_rawgeti(L, index, i);
              Lua2Var(L, p, lua_gettop(L));
              lua_pop(L, 1);
              Inc(p);
            end;
            SafeArrayUnaccessData(TVarData(va^).vArray);
          end else begin
            TVarData(va^).vType:= varVariant or varArray;
            TVarData(va^).vArray:= nil;
          end;
        end else begin
          // TLuaObject
          //if obj is TLuaInteger then begin
          //  va^:= TLuaInteger(obj).Value;
          //end else begin
            va^:= VarAsType(va^, varNull);
          //end;
        end;
      end else begin
        p:= lua_touserdata(L, -1);
        TVarData(va^).vtype:= varVariant or varByRef;
        TVarData(va^).vpointer:= p;
      end;
      lua_pop(L, 2);
    end;
  end{case};
end;

function Index(L : Plua_State) : Integer; cdecl;
var
  p: POleVariant;
  key: string;
  s: string;
  ws: WideString;
  id: IDispatch;
  di: TDispID;
  param: TDispParams;
  ret: OleVariant;
  hr: HResult;
begin
  Result:= 0;

  key := LowerCase(lua_tostring(L, 2));
  lua_pushstring(L, key);
  lua_rawget(L, 1);
  if not lua_isnil(L, -1) then begin
    // for method name is case insesitive
    Result:= 1;
    Exit;
  end;
  lua_pop(L, 1);

  lua_getmetatable(L, 1);
  lua_getfield(L, -1, FIELD_ID);
  p:= lua_touserdata(L, -1);
  lua_pop(L, 2);

  if TVarData(p^).vtype <> varDispatch then begin
    //key:= LowerCase(key);
    if key = 'value' then begin
      case VarType(p^) of
        varNull: lua_pushnil(L);
        varSmallint,varInteger,varshortint,varByte,
        varword,varlongword,varint64,varqword: lua_pushinteger(L, p^);
        varSingle,varDouble,vardecimal: lua_pushnumber(L, p^);
        varBoolean: lua_pushboolean(L, p^);
        // TODO: varArray
        else begin
          ws := p^;
          s := UTF8Encode(ws);
          lua_pushstring(L, PChar(s));
        end;
      end;
    end else
      Exit;
  end else begin
    id:= IDispatch(TVarData(p^).vDispatch);
    ws:= UTF8Decode(key);
    ChkErr(L, id.GetIDsOfNames(GUID_NULL, @ws, 1, GetUserDefaultLCID, @di), key);
    param.rgvarg := nil;
    param.rgdispidNamedArgs := nil;
    param.cArgs := 0;
    param.cNamedArgs := 0;
    VariantInit(TVarData({%H-}ret));

    hr:= id.Invoke(di, GUID_NULL, GetUserDefaultLCID,
                   DISPATCH_PROPERTYGET, param, @ret, nil, nil);
    if hr = 0 then begin
      // Return property value
      case VarType(ret) of
        varNull: lua_pushnil(L);
        varSmallint,varInteger,varshortint,varByte,
        varword,varlongword,varint64,varqword: lua_pushinteger(L, ret);
        varSingle,varDouble,vardecimal: lua_pushnumber(L, ret);
        varBoolean: lua_pushboolean(L, ret);
        varDispatch: DoCreateActiveXObject(L, ret);
        else begin
          ws := ret;
          s := UTF8Encode(ws);
          lua_pushstring(L, PChar(s));
        end;
      end;
    end else begin
      // Return Table for function(method) call
      lua_newtable(L);
    end;

    // Change Metatable for function call
    if lua_getmetatable(L, -1) = 0 then lua_newtable(L);
    lua_pushstring(L, FIELD_FN);
    lua_pushstring(L, PChar(key));
    lua_rawset(L, -3);
    lua_pushstring(L, FIELD_ID_PARENT);
    p:= lua_newuserdata(L, SizeOf(OleVariant));
    VariantInit(TVarData(p^));
    p^:= id;
    lua_rawset(L, -3);
    lua_pushstring(L, '__call');
    lua_pushcfunction(L, @call);
    lua_rawset(L, -3);
    lua_setmetatable(L, -2);

    if hr = DISP_E_MEMBERNOTFOUND then begin
      // Key is method, not property
      lua_pushstring(L, PChar(key)); // key is Lower Case
      lua_pushvalue(L, -2);
      lua_rawset(L, 1);
    end;
  end;
  Result := 1;
end;

function NewIndex(L : Plua_State) : Integer; cdecl;
var
  p: POleVariant;
  key: string;
  ws: WideString;
  id: IDispatch;
  di, diput: TDispID;
  param: TDispParams;
  v: OleVariant;
begin
  Result:=0;
  lua_getmetatable(L, 1);
  lua_getfield(L, -1, FIELD_ID);
  p:= lua_touserdata(L, -1);
  lua_pop(L, 2);
  key := lua_tostring(L, 2);

  if TVarData(p^).vtype <> varDispatch then begin
    key:= LowerCase(key);
    if key = 'value' then begin
      Lua2Var(L, p, 3, False);
    end else
      Exit;
  end else begin
    id:= IDispatch(TVarData(p^).vDispatch);
    ws:= UTF8Decode(key);
    ChkErr(L, id.GetIDsOfNames(GUID_NULL, @ws, 1, GetUserDefaultLCID, @di), key);
    Lua2Var(L, @v, 3);
    diput := DISPID_PROPERTYPUT;
    param.rgvarg := @v;
    param.cArgs := 1;
    param.rgdispidNamedArgs := @diput;
    param.cNamedArgs := 1;
    ChkErr(L, id.Invoke(di, GUID_NULL, GetUserDefaultLCID,
     DISPATCH_PROPERTYPUT, param, nil, nil, nil), key);
  end;
end;

function call(L : Plua_State) : Integer; cdecl;
var
  i, c, t: integer;
  p: POleVariant;
  id: IDispatch;
  di: TDispID;
  s, func: string;
  ws: WideString;
  arglist, pp: {$IFDEF VER2_4}lPVariantArg{$ELSE}PVariantArg{$ENDIF};
  param: TDispParams;
  ret: OleVariant;
begin
  Result:= 0;
  c:= lua_gettop(L);
  t:= 1; // ToDo: const?
  lua_getmetatable(L, 1);
  lua_getfield(L, -1, FIELD_ID_PARENT);
  p:= lua_touserdata(L, -1);
  lua_pop(L, 2);
  if TVarData(p^).vtype <> varDispatch then Exit;
  id:= IDispatch(TVarData(p^).vDispatch);

  lua_getmetatable(L, 1);
  lua_getfield(L, -1, FIELD_FN);
  func:= lua_tostring(L, -1);
  lua_pop(L, 2);
  ws:= UTF8Decode(func);
  ChkErr(L, id.GetIDsOfNames(GUID_NULL, @ws, 1, GetUserDefaultLCID, @di), func);
  GetMem(arglist, SizeOf({$IFDEF VER2_4}VariantArg{$ELSE}TVariantArg{$ENDIF}) * (c-t));
  try
    // 逆順で
    pp := arglist;
    for i := c downto t+1 do begin
      Lua2Var(L, POleVariant(pp), i);
      Inc(pp);
    end;
    param.cArgs := c - t;
{$IFDEF VER2_4}
    param.rgvarg := arglist;
{$ELSE}
    param.rgvarg := PVariantArgList(arglist);
{$ENDIF}
    param.rgdispidNamedArgs := nil;
    param.cNamedArgs := 0;
    VariantInit(TVarData({%H-}ret));

    ChkErr(L, id.Invoke(
     di,
     GUID_NULL,
     GetUserDefaultLCID,
     DISPATCH_PROPERTYGET or DISPATCH_METHOD,
     param, @ret, nil, nil), func);
  finally
    FreeMem(arglist);
  end;

  case VarType(ret) of
    varNull: lua_pushnil(L);
    varSmallint,varInteger,varshortint,varByte,
    varword,varlongword,varint64,varqword: lua_pushinteger(L, ret);
    varSingle,varDouble,vardecimal: lua_pushnumber(L, ret);
    varBoolean: lua_pushboolean(L, ret);
    varDispatch: DoCreateActiveXObject(L, ret);
    else{case} begin
      ws := ret;
      s := UTF8Encode(ws);
      lua_pushstring(L, PChar(s));
    end;
  end;

  Result := 1;
end;

function iterator(L : Plua_State) : Integer; cdecl;
var
  i: integer;
  p: POleVariant;
  id: IDispatch;
  s: string;
  ws: WideString;
  param: TDispParams;
  v, ret: OleVariant;
begin
  Result:= 0;
  lua_getmetatable(L, 1);
  lua_getfield(L, -1, FIELD_ID);
  p:= lua_touserdata(L, -1);
  lua_pop(L, 2);

  if TVarData(p^).vtype <> varDispatch then Exit;

  id:= IDispatch(TVarData(p^).vDispatch);
  i:= lua_tointeger(L, lua_upvalueindex(1));
  lua_pushinteger(L, i+1);
  lua_replace(L, lua_upvalueindex(1));

  VariantInit(TVarData({%H-}v));
  v := i;
  param.cArgs := 1;
  param.rgvarg := @v;
  param.rgdispidNamedArgs := nil;
  param.cNamedArgs := 0;
  VariantInit(TVarData({%H-}ret));

  if id.Invoke(
   DISPID_VALUE,
   GUID_NULL,
   GetUserDefaultLCID,
   DISPATCH_PROPERTYGET or DISPATCH_METHOD,
   param, @ret, nil, nil) = 0 then begin
    case VarType(ret) of
      varNull: lua_pushnil(L);
      varSmallint,varInteger,varshortint,varByte,
      varword,varlongword,varint64,varqword: lua_pushinteger(L, ret);
      varSingle,varDouble,vardecimal: lua_pushnumber(L, ret);
      varBoolean: lua_pushboolean(L, ret);
      varDispatch: begin
        if TVarData(ret).vdispatch <> nil then begin
          DoCreateActiveXObject(L, ret);
        end else begin
          lua_pushnil(L);
        end;
      end;
      else begin
        ws := ret;
        s := UTF8Encode(ws);
        lua_pushstring(L, PChar(s));
      end;
    end;
  end else begin
    lua_pushnil(L);
  end;
  Result := 1;
end;

function pairs(L : Plua_State) : Integer; cdecl;
begin
  lua_pushinteger(L, 0); // upvalue for loop counter
  lua_pushcclosure(L, @iterator, 1); // f
  lua_pushvalue(L, 1); // t
  lua_pushnil(L); // s
  Result:= 3;
end;

function gc(L : Plua_State) : Integer; cdecl;
var
  p: POleVariant;
  id: IDispatch;
begin
  Result:= 0;
  p:= lua_touserdata(L, 1);
  if TVarData(p^).vtype <> varDispatch then Exit;
  id:= IDispatch(TVarData(p^).vDispatch);
  id._Release;
end;

procedure DoCreateActiveXObject(L : Plua_State; id: IDispatch);
var
  p: POleVariant;
begin
  id._AddRef;
  lua_newtable(L); //t:= lua_gettop(L);

  lua_newtable(L);
  lua_pushstring(L, FIELD_ID);
  p:= lua_newuserdata(L, SizeOf(OleVariant));
  VariantInit(TVarData(p^));
  p^:= id;
  if lua_getmetatable(L, -1) = 0 then lua_newtable(L);
  lua_pushstring(L, '__gc');
  lua_pushcfunction(L, @gc);
  lua_settable(L, -3);
  lua_setmetatable(L, -2);
  lua_settable(L, -3);
  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, @NewIndex);
  lua_settable(L, -3);
  lua_pushstring(L, '__index');
  lua_pushcfunction(L, @Index);
  lua_settable(L, -3);
  lua_pushstring(L, '__pairs');
  lua_pushcfunction(L, @pairs);
  lua_settable(L, -3);
  lua_pushstring(L, '__ipairs');
  lua_pushcfunction(L, @pairs);
  lua_settable(L, -3);
  lua_setmetatable(L, -2);
end;

function CreateActiveXObject(L : Plua_State) : Integer; cdecl;
begin
  DoCreateActiveXObject(L, CreateOleObject(lua_tostring(L, 1)));
  Result := 1;
end;

// Make ref(ByRef) type
function CreateRefType(L : Plua_State) : Integer; cdecl;
var
  p: POleVariant;
  ParamCount: integer;
begin
  ParamCount:= lua_gettop(L);
  lua_newtable(L);

  lua_newtable(L);
  lua_pushstring(L, FIELD_ID);
  p:= lua_newuserdata(L, SizeOf(OleVariant));
  VariantInit(TVarData(p^));
  if ParamCount > 0 then begin
    Lua2Var(L, p, 1, False);
  end;
  lua_settable(L, -3);

  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, @NewIndex);
  lua_settable(L, -3);
  lua_pushstring(L, '__index');
  lua_pushcfunction(L, @Index);
  lua_settable(L, -3);
  lua_pushstring(L, '__pairs');
  lua_pushcfunction(L, @pairs);
  lua_settable(L, -3);
  lua_pushstring(L, '__ipairs');
  lua_pushcfunction(L, @pairs);
  lua_settable(L, -3);
  lua_setmetatable(L, -2);

  Result := 1;
end;

end.

