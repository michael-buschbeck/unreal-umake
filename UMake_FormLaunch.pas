unit UMake_FormLaunch;


interface


uses
  UMake_Configuration, UMake_Options,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, SysTools, Shellapi, ShlObj, FileCtrl;


(*****************************************************************************)
(*  TFormLaunch
(*****************************************************************************)

type
  TFormLaunch = class(TForm)
    BevelHints: TBevel;
    ButtonBrowseProject: TBitBtn;
    ButtonClose: TButton;
    ButtonCompile: TButton;
    ButtonOptions: TButton;
    LabelHints: TLabel;
    LabelHintsParagraph1: TLabel;
    LabelHintsParagraph2: TLabel;
    LabelSource: TLabel;
    ComboBoxProject: TComboBox;

    procedure ButtonBrowseProjectClick(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure ComboBoxProjectChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonCompileClick(Sender: TObject);

  protected
    procedure MessageDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;

  public
    Configuration: TConfiguration;
    Options: TOptions;
  end;

var
  FormLaunch: TFormLaunch;

implementation

uses
  UMake_FormOptions;


{$R *.DFM}


(*****************************************************************************)
(*  TFormLaunch
(*****************************************************************************)

procedure TFormLaunch.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, True);
end;


procedure TFormLaunch.FormShow(Sender: TObject);
var
  IndexProject: Integer;
begin
  for IndexProject := 0 to Options.RegOptProjects.ItemCount - 1 do
    ComboBoxProject.Items.Add(Options.RegOptProjects[IndexProject].Value);

  ComboBoxProject.Text := Options.RegOptProjects.Value;
  ComboBoxProjectChange(ComboBoxProject);
end;


procedure TFormLaunch.MessageDropFiles(var Msg: TWMDropFiles);
var
  LengthTextFileDropped: Integer;
  TextFileDropped: string;
begin
  LengthTextFileDropped := DragQueryFile(Msg.Drop, 0, nil, 0) + 1;
  SetLength(TextFileDropped, LengthTextFileDropped);

  DragQueryFile(Msg.Drop, 0, PChar(TextFileDropped), LengthTextFileDropped);
  DragFinish(Msg.Drop);

  if FileExists(TextFileDropped) then
    TextFileDropped := ExcludeTrailingBackslash(ExtractFilePath(TextFileDropped));
  if AnsiSameText(ExtractFileName(TextFileDropped), 'Classes') then
    TextFileDropped := ExcludeTrailingBackslash(ExtractFilePath(TextFileDropped));

  ComboBoxProject.Text := TextFileDropped;
  ComboBoxProject.SelectAll;
end;


procedure TFormLaunch.ButtonBrowseProjectClick(Sender: TObject);
var
  BrowseInfo: TBrowseInfo;
  PointerIdListPath: Pointer;
  TextPath: string;
  TextDirPath: string;
begin
  SetLength(TextPath,    MAX_PATH);
  SetLength(TextDirPath, MAX_PATH);

  BrowseInfo.hwndOwner      := Handle;
  BrowseInfo.pidlRoot       := nil;
  BrowseInfo.pszDisplayName := PChar(TextPath);
  BrowseInfo.lpszTitle      := 'Select the UnrealScript project directory you wish to compile:';
  BrowseInfo.ulFlags        := BIF_RETURNONLYFSDIRS;
  BrowseInfo.lpfn           := nil;

  PointerIdListPath := SHBrowseForFolder(BrowseInfo);

  if Assigned(PointerIdListPath) and SHGetPathFromIDList(PointerIdListPath, PChar(TextDirPath)) then
  begin
    SetLength(TextDirPath, Pos(#0, TextDirPath) - 1);
    if AnsiSameText(ExtractFileName(TextDirPath), 'Classes') then
      TextDirPath := ExcludeTrailingBackslash(ExtractFilePath(TextDirPath));

    ComboBoxProject.Text := TextDirPath;
    ComboBoxProjectChange(ComboBoxProject);
  end;

  ComboBoxProject.SetFocus;
end;


procedure TFormLaunch.ComboBoxProjectChange(Sender: TObject);
var
  TextDirPackage: string;
begin
  FreeAndNil(Configuration);

  TextDirPackage := Trim(ComboBoxProject.Text);
  if DirectoryExists(TextDirPackage) then
  begin
    TextDirPackage := GetLongPath(TextDirPackage);
    TextDirPackage := ExcludeTrailingBackslash(TextDirPackage);

    try
      Configuration := TConfiguration.Create(ExtractFileName(TextDirPackage), ExtractFilePath(TextDirPackage));
      Configuration.Read;
    except
      on EConfiguration do FreeAndNil(Configuration);
    end;
  end;

  ButtonCompile.Enabled := Assigned(Configuration);
  ButtonCompile.Default := Assigned(Configuration);    
end;


procedure TFormLaunch.ButtonOptionsClick(Sender: TObject);
begin
  FormOptions.Configuration := Configuration;
  FormOptions.Options := Options;
  FormOptions.ShowModal;
end;


procedure TFormLaunch.ButtonCompileClick(Sender: TObject);
var
  IndexProject: Integer;
  TextDirProject: string;
begin
  TextDirProject := Trim(ComboBoxProject.Text);
  Options.RegOptProjects.Value := TextDirProject;

  IndexProject := 0;

  while IndexProject < Options.RegOptProjects.ItemCount do
  begin
    if AnsiCompareText(TextDirProject, Options.RegOptProjects[IndexProject].Value) <= 0 then
      Break;
    Inc(IndexProject);
  end;

  if (IndexProject >= Options.RegOptProjects.ItemCount) or not AnsiSameText(TextDirProject, Options.RegOptProjects[IndexProject].Value) then
  begin
    Options.RegOptProjects.ItemInsert(IndexProject);
    Options.RegOptProjects[IndexProject].Value := TextDirProject;
  end;
end;

end.
