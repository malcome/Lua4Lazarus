unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  PrintersDlgs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    Memo1: TMemo;
    Memo2: TMemo;
    PrinterSetupDialog1: TPrinterSetupDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
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
  lua53, Printers, Unit2, l4l_object, l4l_print;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  if Printer.Printers.Count <= 0 then begin
    Button1.Enabled := False;
    Button2.Enabled := False;
    Button3.Enabled := False;
    ComboBox1.Enabled := False;
    Exit;
  end;

  for i := 0 to Printer.Printers.Count-1 do begin
    ComboBox1.Items.Add(Printer.Printers[i]);
  end;
  ComboBox1.ItemIndex:= Printer.PrinterIndex;
  ComboBox1Select(nil);
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  Memo1.Clear;
  try
    Screen.Cursor:= crHourglass;
    try
      Printer.PrinterIndex:= ComboBox1.ItemIndex;
    finally
      Screen.Cursor:= crDefault;
    end;
  except
    on e: Exception do begin
      ShowMessage(e.Message);
      Printer.PrinterIndex:= -1;
      ComboBox1.ItemIndex:= Printer.PrinterIndex;
    end;
  end;
  if Printer.CanPrint then begin
    Memo1.Lines.Add(Printer.PaperSize.PaperName);
    Memo1.Lines.Add(Format('%d * %d DPI', [Printer.XDPI, Printer.YDPI]));
    Memo1.Lines.Add(Format('(%d, %d), (%d, %d)',
     [Printer.PaperSize.PaperRect.WorkRect.Left,
      Printer.PaperSize.PaperRect.WorkRect.Top,
      Printer.PaperSize.PaperRect.PhysicalRect.Right
       -Printer.PaperSize.PaperRect.WorkRect.Right,
      Printer.PaperSize.PaperRect.PhysicalRect.Bottom
       -Printer.PaperSize.PaperRect.WorkRect.Bottom]));
  end;
  Button1.Enabled:= Printer.CanPrint;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if PrinterSetupDialog1.Execute then begin
    ComboBox1.ItemIndex:= Printer.PrinterIndex;
    ComboBox1Select(nil);
  end;
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
  lp: TLuaPrint;
begin
  L:= lua_newstate(@alloc, nil);
  try
    lp := TLuaPrint.Create(L);
    try
      luaopen_base(L);
      //luaopen_string(L);
      //lua_register(L, 'CreateStringsObject', @CreateStringsObject);
      l4l_PushLuaObject(TLuaPrintObject.Create(L, lp)); lua_setglobal(L, 'P');
      try
        lp.BeginDoc(Rect(2000, 3000, 2000, 3000));
        try
          lp.Run(Memo2.Text);
        finally
          lp.EndDoc;
        end;

        if Sender = Button1 then begin
          lp.Print;
        end else begin
          FormPreview:= TFormPreview.Create(Self);
          try
            FormPreview.LP:= lp;
            FormPreView.ShowModal;
          finally
            FormPreview.Free;
          end;
        end;

      except
        ShowMessage(lua_tostring(L, -1));
      end;
    finally
      lp.Free;
    end;
  finally
    lua_close(L);
  end;
end;

end.

