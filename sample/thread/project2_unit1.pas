unit project2_unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Contnrs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    ThreadList: TObjectList;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation
uses
  Lua53, l4l_object;

{$R *.lfm}

const
  THREAD_VAR_NAME = 'THREAD_VAR';

type

  { TLuaThread }

  PLuaThread = ^TLuaThread;
  TLuaThread = class(TThread)
  private
    L: Plua_State;
    msg: string;
    {$IF FPC_FULLVERSION < 30000}
    finished: boolean;
    {$ENDIF}
    procedure ShowMsg;
    procedure Last;
  protected
    procedure Execute; override;
  public
    property Terminated;
    constructor Create(aL: plua_state); overload;
    destructor Destroy; override;
  end;

  { TLuaMyObject }

  TLuaMyObject = class(TLuaObject)
  private
    function GetID: integer;
    procedure DoPrint;
  protected
  public
  published
    property l4l_ID: integer read GetID;
    function l4l_print: integer;
    function l4l_sleep: integer;
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

procedure hook({%H-}L: plua_State; {%H-}ar: plua_Debug); cdecl;
var
  p: PLuaThread;
begin
  lua_getfield(L, LUA_REGISTRYINDEX, THREAD_VAR_NAME);
  p:= lua_touserdata(L, -1);
  Lua_pop(L, 1);
  if p^.Terminated then SysUtils.Abort;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  L: Plua_State;
  s: string;
  i: integer;
  t: TLuaThread;
  p: PLuaThread;
begin
  // garbage collection
  i:= 0;
  while i < ThreadList.Count do begin
    t:= TLuaThread(ThreadList[i]);
    if t.finished then begin
      t.Free;
      ThreadList.Delete(i);
    end else
      Inc(i);
  end;

  // Start New thread.
  L:= lua_newstate(@alloc, nil);
  lua_sethook(L, @hook, LUA_MASKLINE, 0);
  l4l_PushLuaObject(TLuaMyObject.Create(L)); lua_setglobal(L, 'my'); // set global value.
  s:= Memo1.Text;
  if luaL_loadbuffer(L, PChar(s), Length(s), 'sample') <> 0 then begin
    Form1.Memo2.Lines.Add(lua_tostring(L, -1));
    Form1.Memo2.SelStart:= 0;
    Form1.Memo2.SelLength:= 0;
    lua_close(L);
    Exit;
  end;

  t := TLuaThread.Create(L);
  ThreadList.Add(t);
  p:= lua_newuserdata(L, SizeOf(Pointer));
  p^:= t;
  lua_setfield(L, LUA_REGISTRYINDEX, THREAD_VAR_NAME);

  Label1.Caption:= IntToStr(ThreadList.Count);
  Memo2.SetFocus;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ThreadList:= TObjectList.Create(False);
  Timer1.Enabled:= True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  while ThreadList.Count > 0 do begin
    TLuaThread(ThreadList[0]).Free;
    ThreadList.Delete(0);
  end;
  ThreadList.Free;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  while ThreadList.Count > 0 do begin
    TLuaThread(ThreadList[0]).Free;
    ThreadList.Delete(0);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: Integer;
  t: TLuaThread;
begin
  // garbage collection
  i:= 0;
  while i < ThreadList.Count do begin
    t:= TLuaThread(ThreadList[i]);
    if t.finished then begin
      t.Free;
      ThreadList.Delete(i);
    end else
      Inc(i);
  end;

  Label1.Caption:= IntToStr(ThreadList.Count);
end;

{ TLuaThread }

procedure TLuaThread.Execute;
begin
  {$IF FPC_FULLVERSION < 30000}
  finished:= False;
  {$ENDIF}
  try
    //msg := 'Start: ' + IntToStr(Self.FThreadID);
    //Synchronize(@ShowMsg);
    try
      if lua_pcall(L, 0, 0, 0) <> 0 then Exception.Create('');
    except
      on E: EAbort do begin
        msg := 'Abort: ' + IntToStr(Self.FThreadID);
        Synchronize(@ShowMsg);
      end;
      else begin
        msg := 'Error: ' + IntToStr(Self.FThreadID) + ' : ' + lua_tostring(L, -1);
        Synchronize(@ShowMsg);
      end;
    end;
    //msg := 'Finish: ' + IntToStr(Self.FThreadID);
    //Synchronize(@ShowMsg);
  finally
    {$IF FPC_FULLVERSION < 30000}
    finished:= True;
    {$ENDIF}
    msg := ''; // For economy.
    lua_close(L);
    //Synchronize(@Last);
  end;
end;

constructor TLuaThread.Create(aL: plua_state);
begin
  L := aL;
  FreeOnTerminate:= False;
  inherited Create(False);
end;

procedure TLuaThread.ShowMsg;
begin
  Form1.Memo2.Lines.Insert(0, msg);
  Form1.Memo2.SelStart:= 0;
  Form1.Memo2.SelLength:= 0;
end;

procedure TLuaThread.Last;
begin
end;

destructor TLuaThread.Destroy;
begin
  inherited Destroy;
end;

{ TLuaMyObject }

procedure TLuaMyObject.DoPrint;
var
  i, c: integer;
  s: string;
begin
  c:= lua_gettop(LS);
  s:= '';
  for i:= 1 to c do s:= s + lua_tostring(LS, i);
  Form1.Memo2.Lines.Insert(0, s);
  Form1.Memo2.SelStart:= 0;
  Form1.Memo2.SelLength:= 0;
end;

function TLuaMyObject.GetID: integer;
var
  p: PLuaThread;
begin
  lua_getfield(LS, LUA_REGISTRYINDEX, THREAD_VAR_NAME);
  p:= lua_touserdata(LS, -1);
  Lua_pop(LS, 1);
  Result:= p^.ThreadID;
end;

function TLuaMyObject.l4l_print: integer;
begin
  TThread.Synchronize(nil, @DoPrint);
  Result := 0;
end;

function TLuaMyObject.l4l_sleep: integer;
begin
  Sleep(lua_tointeger(LS, 1));
  Result := 0;
end;

end.

