unit UMake_Options;


interface


uses
  UMake_Configuration,
  Forms, FileCtrl, RegOpts;


(*****************************************************************************)
(*  TOptions
(*****************************************************************************)

type
  TOptionsPerformIndex = (perfSuccess, perfFailure);
  TOptionsPerformWindow = (perfWindowNone, perfWindowFront, perfWindowClose);

  TOptionsPerform = record
    RegOptWindow: TRegOptInteger;
    RegOptLaunch: TRegOptString;
    RegOptSound:  TRegOptString;
  end;

  TOptions = class
  public
    RegOptDetails: TRegOptBoolean;
    RegOptEditor: TRegOptString;
    RegOptProjects: TRegOptString;
    Perform: array [TOptionsPerformIndex] of TOptionsPerform;

    constructor Create;
    destructor Destroy; override;

    procedure PerformAction(IndexPerform: TOptionsPerformIndex; Form: TCustomForm; Configuration: TConfiguration);
    procedure PerformEdit(Configuration: TConfiguration; TextFileError: string; IndexLineError: Integer);
  end;


implementation


uses
  Registry, Windows, MMSystem, Shellapi, SysUtils, SysTools, RegExpr;


(*****************************************************************************)
(*  TOptions
(*****************************************************************************)

constructor TOptions.Create;
const
  TextNameProgram = 'Phase\UMake';
  TextNameSettingPerform: array [TOptionsPerformIndex] of string = ('Success', 'Failure');
var
  IndexPerform: TOptionsPerformIndex;
  IndexProject: Integer;
  Registry: TRegistry;
  TextFileSound: string;
  TextNameSetting: string;
  TextNameSound: string;
begin
  RegOptDetails  := TRegOptBoolean.Create(TextNameProgram, 'Details', False);
  RegOptEditor   := TRegOptString.Create(TextNameProgram, 'Editor', '');
  RegOptProjects := TRegOptString.CreateList(TextNameProgram, 'Projects');

  for IndexProject := RegOptProjects.ItemCount - 1 downto 0 do
    if not DirectoryExists(RegOptProjects[IndexProject].Value) then
      RegOptProjects.ItemDelete(IndexProject);

  if not DirectoryExists(RegOptProjects.Value) then
    RegOptProjects.Value := '';

  Registry := TRegistry.Create;

  for IndexPerform := Low(TOptionsPerformIndex) to High(TOptionsPerformIndex) do
  begin
    case IndexPerform of
      perfSuccess:  TextNameSound := 'SystemExclamation';
      perfFailure:  TextNameSound := 'SystemHand';
    end;

    if Registry.OpenKeyReadOnly('\AppEvents\Schemes\Apps\.Default\' + TextNameSound + '\.Current')
      then TextFileSound := Registry.ReadString('')
      else TextFileSound := '';

    TextNameSetting := 'Perform' + TextNameSettingPerform[IndexPerform];

    with Perform[IndexPerform] do
    begin
      RegOptWindow := TRegOptInteger.Create(TextNameProgram, TextNameSetting + 'Window', Integer(perfWindowFront));
      RegOptLaunch := TRegOptString .Create(TextNameProgram, TextNameSetting + 'Launch', '');
      RegOptSound  := TRegOptString .Create(TextNameProgram, TextNameSetting + 'Sound', TextFileSound);

      if (RegOptWindow.Value < Integer(Low (TOptionsPerformWindow)))
      or (RegOptWindow.Value > Integer(High(TOptionsPerformWindow))) then
        RegOptWindow.Value := Integer(perfWindowFront);
    end;
  end;

  Registry.Free;
end;


destructor TOptions.Destroy;
var
  IndexPerform: TOptionsPerformIndex;
begin
  RegOptDetails.Free;
  RegOptEditor.Free;
  RegOptProjects.Free;

  for IndexPerform := Low(TOptionsPerformIndex) to High(TOptionsPerformIndex) do
  begin
    with Perform[IndexPerform] do
    begin
      RegOptWindow.Destroy;
      RegOptLaunch.Destroy;
      RegOptSound .Destroy;
    end;
  end;
end;


procedure TOptions.PerformAction(IndexPerform: TOptionsPerformIndex; Form: TCustomForm; Configuration: TConfiguration);
var
  TextCommand: string;
  OptionsSound: Integer;
begin
  with Perform[IndexPerform] do
  begin
    if Length(Trim(RegOptSound.Value)) > 0 then
    begin
      OptionsSound := SND_FILENAME;
      if TOptionsPerformWindow(RegOptWindow.Value) <> perfWindowClose then
        OptionsSound := OptionsSound + SND_ASYNC;

      PlaySound(PChar(RegOptSound.Value), 0, OptionsSound);
    end;

    TextCommand := RegOptLaunch.Value;
    if Length(Trim(TextCommand)) > 0 then
    begin
      TextCommand := ReplaceRegExpr('%package%',    TextCommand, Configuration.Package);
      TextCommand := ReplaceRegExpr('%packagedir%', TextCommand, Configuration.DirPackage);
      TextCommand := ReplaceRegExpr('%gamedir%',    TextCommand, Configuration.DirGame);
      LaunchProgram(TextCommand);
    end;

    case TOptionsPerformWindow(RegOptWindow.Value) of
      perfWindowClose:  begin Form.Hide;  Form.Close;  end;
      perfWindowFront:  begin Form.Show;  Form.Update; end;
    end;
  end;
end;


procedure TOptions.PerformEdit(Configuration: TConfiguration; TextFileError: string; IndexLineError: Integer);
var
  TextCommand: string;
begin
  if Length(RegOptEditor.Value) > 0 then
  begin
    TextCommand := RegOptEditor.Value;
    TextCommand := ReplaceRegExpr('%package%',    TextCommand, Configuration.Package);
    TextCommand := ReplaceRegExpr('%packagedir%', TextCommand, Configuration.DirPackage);
    TextCommand := ReplaceRegExpr('%gamedir%',    TextCommand, Configuration.DirGame);
    TextCommand := ReplaceRegExpr('%errfile%',    TextCommand, GetQuotedParam(TextFileError));

    if IndexLineError = 0
      then TextCommand := ReplaceRegExpr('%errline%', TextCommand, '1')
      else TextCommand := ReplaceRegExpr('%errline%', TextCommand, IntToStr(IndexLineError));

    if not LaunchProgram(TextCommand) then
      Application.MessageBox('Unable to start specified source code editor.', PChar(Application.Title), MB_ICONERROR);
  end
  else begin
    Application.MessageBox('No source code editor specified.', PChar(Application.Title), MB_ICONERROR)
  end;
end;


end.
